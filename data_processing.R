# Poverty competition at DrivenData: https://www.drivendata.org/competitions/50/worldbank-poverty-prediction/data/
library(tidyverse)
library(data.table)
library(caret)
# run processes in parallel
library(doMC)
registerDoMC(cores = 2)

### download individual data -----------------------

# training
#download.file("https://s3.amazonaws.com/drivendata/data/50/public/A_indiv_train.csv", "A_indiv_train.csv")
a_indiv_train <- fread("A_indiv_train.csv", stringsAsFactors = TRUE)
# testing
#download.file("https://s3.amazonaws.com/drivendata/data/50/public/A_indiv_test.csv", "A_indiv_test.csv")
a_indiv_test <- fread("A_indiv_test.csv", stringsAsFactors = TRUE)

### Pre-processing ------------------------
# append train and test
a_indiv_all <- bind_rows(a_indiv_train,a_indiv_test)
# keep train ids and classes for later
a_indiv_train_ids_class <- select(a_indiv_train, id, poor) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(id = as.character(id), poor = ifelse(poor,1,0))
# dummify variables
a_indiv_all_ids <- a_indiv_all[,c(1)]
a_indiv_all_class <- ifelse(a_indiv_all$poor == TRUE,1,0)
dummies <- dummyVars(id ~ ., data = select(a_indiv_all, -poor,-country))
dummies_pred <- predict(dummies, newdata = a_indiv_all) %>% as.data.frame()
a_indiv_all_dummy <- cbind(a_indiv_all_ids,dummies_pred) %>%
  mutate(id = as.character(id))
# zero & near-zero variance
nzv <- nearZeroVar(a_indiv_all_dummy)
a_indiv_all_nzv <- a_indiv_all_dummy[, -nzv]
a_indiv_all_nzv <- select(a_indiv_all_nzv, -iid)
# highly correlated predictors
descrCor <- cor(select_if(a_indiv_all_nzv, is.numeric))
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
a_indiv_all_corr <- a_indiv_all_nzv[,-highlyCorDescr]
# linear dependencies
comboInfo <- findLinearCombos(select_if(a_indiv_all_corr, is.numeric))
if (!is.null(comboInfo$remove)) {
  a_indiv_all_ldep <- a_indiv_all_corr[, -comboInfo$remove]
} else {
  a_indiv_all_ldep <- a_indiv_all_corr
}
# aggregate at household level
a_indiv_all_aggr <- a_indiv_all_ldep %>%
  mutate(id = as.character(id)) %>%
  group_by(id) %>%
  summarise_all(mean)
# split it back into train and test
training <- merge(a_indiv_train_ids_class, a_indiv_all_aggr, by = "id")
testing <- merge(distinct(a_indiv_test, id) %>% mutate(id = as.character(id)) , a_indiv_all_aggr, by = "id")

### Explore the data ------------------------
# library(tsne)
# set.seed(123)
# tsne_points <- tsne(sample_frac(training[,-c(1,2)], size = .2),
#                     max_iter=400,
#                     perplexity=20,
#                     epoch=100)
# plot(tsne_points)

### plot the variables
# library(AppliedPredictiveModeling)
# transparentTheme(trans = .4)
# featurePlot(x = training[, 3:10],
#              y = as.factor(training$poor),
#              plot = "pairs",
#              ## Add a key at the top
#              auto.key = list(columns = 2))

### Models --------------------------

# Check out class balance
mean(training$poor) # for country A it is 55-45%

# establish the resampling method
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# train the model
library(randomForest)
rfGrid <-  expand.grid(mtry = 10)

set.seed(825)
model <- train(poor ~ ., data = training, 
               method = "rf", 
               trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
               verbose = FALSE,
               tuneGrid = rfGrid)




