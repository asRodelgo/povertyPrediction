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

### download household data -----------------------
# training
#download.file("https://s3.amazonaws.com/drivendata/data/50/public/A_hhold_train.csv", "A_hhold_train.csv")
a_hhold_train <- fread("A_hhold_train.csv", stringsAsFactors = TRUE)
# testing
#download.file("https://s3.amazonaws.com/drivendata/data/50/public/A_hhold_test.csv", "A_hhold_test.csv")
a_hhold_test <- fread("A_hhold_test.csv", stringsAsFactors = TRUE)

### Pre-processing individual data ------------------------
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
train_indiv <- merge(a_indiv_train_ids_class, a_indiv_all_aggr, by = "id")
test_indiv <- merge(distinct(a_indiv_test, id) %>% mutate(id = as.character(id)) , a_indiv_all_aggr, by = "id")


### Pre-processing hhold data ------------------------
# append train and test
a_hhold_all <- bind_rows(a_hhold_train,a_hhold_test)
# keep train ids and classes for later
a_hhold_train_ids_class <- select(a_hhold_train, id, poor) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(id = as.character(id), poor = ifelse(poor,1,0))
# dummify variables
a_hhold_all_ids <- a_hhold_all[,c(1)]
a_hhold_all_class <- ifelse(a_hhold_all$poor == TRUE,1,0)
dummies <- dummyVars(id ~ ., data = select(a_hhold_all, -poor,-country))
dummies_pred <- predict(dummies, newdata = a_hhold_all) %>% as.data.frame()
a_hhold_all_dummy <- cbind(a_hhold_all_ids,dummies_pred) %>%
  mutate(id = as.character(id))
# zero & near-zero variance
nzv <- nearZeroVar(a_hhold_all_dummy)
a_hhold_all_nzv <- a_hhold_all_dummy[, -nzv]
# highly correlated predictors
descrCor <- cor(select_if(a_hhold_all_nzv, is.numeric))
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
a_hhold_all_corr <- a_hhold_all_nzv[,-highlyCorDescr[-which(highlyCorDescr == 1)]]
# linear dependencies
comboInfo <- findLinearCombos(select_if(a_hhold_all_corr, is.numeric))
if (!is.null(comboInfo$remove)) {
  a_hhold_all_ldep <- a_hhold_all_corr[, -comboInfo$remove]
} else {
  a_hhold_all_ldep <- a_hhold_all_corr
}
# split it back into train and test
train_hhold <- merge(a_hhold_train_ids_class, a_hhold_all_ldep, by = "id")
test_hhold <- merge(distinct(a_hhold_test, id) %>% mutate(id = as.character(id)) , a_hhold_all_ldep, by = "id")

# put indiv and hhold together
train_all <- merge(train_hhold,train_indiv, by = c("id","poor"))
test_all <- merge(test_hhold,test_indiv, by = c("id"))


