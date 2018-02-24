# Poverty competition at DrivenData: https://www.drivendata.org/competitions/50/worldbank-poverty-prediction/data/
library(tidyverse)
library(data.table)
library(caret)
# run processes in parallel
library(doMC)
registerDoMC(cores = 2)

# ### download train data -------------
# split <- "train" 
# type <- "indiv"
# type_train <- data.frame()
# for (cou in c("A","B","C")){
#     download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_",type,"_",split,".csv"), paste0(cou,"_",type,"_",split,".csv"))
#     this_train <- fread(paste0(cou,"_",type,"_",split,".csv"), stringsAsFactors = TRUE) %>%
#       mutate(country = cou)
#     if (nrow(type_train)>0) type_train <- bind_rows(type_train,this_train) else type_train <- this_train
# }
# indiv_train <- type_train
# 
# type <- "hhold"
# type_train <- data.frame()
# for (cou in c("A","B","C")){
#   download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_",type,"_",split,".csv"), paste0(cou,"_",type,"_",split,".csv"))
#   this_train <- fread(paste0(cou,"_",type,"_",split,".csv"), stringsAsFactors = TRUE) %>%
#     mutate(country = cou)
#   if (nrow(type_train)>0) type_train <- bind_rows(type_train,this_train) else type_train <- this_train
# }
# hhold_train <- type_train
# 
# ### download test data -------------
# split <- "test" 
# type <- "indiv"
# type_test <- data.frame()
# for (cou in c("A","B","C")){
#   download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_",type,"_",split,".csv"), paste0(cou,"_",type,"_",split,".csv"))
#   this_test <- fread(paste0(cou,"_",type,"_",split,".csv"), stringsAsFactors = TRUE) %>%
#     mutate(country = cou)
#   if (nrow(type_test)>0) type_test <- bind_rows(type_test,this_test) else type_test <- this_test
# }
# indiv_test <- type_test
# 
# type <- "hhold"
# type_test <- data.frame()
# for (cou in c("A","B","C")){
#   download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_",type,"_",split,".csv"), paste0(cou,"_",type,"_",split,".csv"))
#   this_test <- fread(paste0(cou,"_",type,"_",split,".csv"), stringsAsFactors = TRUE) %>%
#     mutate(country = cou)
#   if (nrow(type_test)>0) type_test <- bind_rows(type_test,this_test) else type_test <- this_test
# }
# hhold_test <- type_test
# 
# ### Pre-processing individual data ------------------------
# # append train and test
# indiv_all <- bind_rows(indiv_train,indiv_test) %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_if(is.factor, addNA)
# # keep train ids and classes for later
# indiv_train_ids_class <- select(indiv_train, id, poor) %>%
#   distinct(id, .keep_all = TRUE) %>%
#   mutate(id = as.character(id), poor = ifelse(poor,1,0))
# # dummify variables
# indiv_all_ids <- indiv_all[,c(1)]
# indiv_all_class <- ifelse(indiv_all$poor == TRUE,1,0)
# dummies <- dummyVars(id ~ ., data = select(indiv_all, -poor,-country))
# dummies_pred <- predict(dummies, newdata = indiv_all) %>% as.data.frame()
# indiv_all_dummy <- cbind(id = indiv_all_ids,dummies_pred) %>%
#   mutate(id = as.character(id))
# # zero & near-zero variance
# nzv <- nearZeroVar(indiv_all_dummy)
# indiv_all_nzv <- indiv_all_dummy[, -nzv]
# indiv_all_nzv <- select(indiv_all_nzv, -iid)
# # impute NAs in numeric variables
# indiv_all_nzv2 <- mutate_all(indiv_all_nzv, funs(ifelse(is.na(.),mean(.,na.rm=TRUE),.)))
# # highly correlated predictors
# descrCor <- cor(select_if(indiv_all_nzv2, is.numeric))
# summary(descrCor[upper.tri(descrCor)])
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
# indiv_all_corr <- select_if(indiv_all_nzv2, is.numeric)[,-highlyCorDescr] %>%
#   cbind(select_if(indiv_all_nzv2, is.character))
# # linear dependencies
# comboInfo <- findLinearCombos(select_if(indiv_all_corr, is.numeric))
# if (!is.null(comboInfo$remove)) {
#   indiv_all_ldep <- indiv_all_corr[, -comboInfo$remove]
# } else {
#   indiv_all_ldep <- indiv_all_corr
# }
# # aggregate at household level
# indiv_all_aggr <- indiv_all_ldep %>%
#   mutate(id = as.character(id)) %>%
#   group_by(id) %>%
#   summarise_all(mean)
# # split it back into train and test
# train_indiv <- merge(indiv_train_ids_class, indiv_all_aggr, by = "id")
# test_indiv <- merge(distinct(indiv_test, id) %>% mutate(id = as.character(id)) , indiv_all_aggr, by = "id")
# 
# ### Pre-processing hhold data ------------------------
# # append train and test
# hhold_all <- bind_rows(hhold_train,hhold_test) %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_if(is.factor, addNA)
# # keep train ids and classes for later
# hhold_train_ids_class <- select(hhold_train, id, poor) %>%
#   distinct(id, .keep_all = TRUE) %>%
#   mutate(id = as.character(id), poor = ifelse(poor,1,0))
# # dummify variables
# hhold_all_ids <- hhold_all[,c(1)]
# hhold_all_class <- ifelse(hhold_all$poor == TRUE,1,0)
# dummies <- dummyVars(id ~ ., data = select(hhold_all, -poor,-country))
# dummies_pred <- predict(dummies, newdata = hhold_all) %>% as.data.frame()
# hhold_all_dummy <- cbind(id = hhold_all_ids,dummies_pred) %>%
#   mutate(id = as.character(id))
# # zero & near-zero variance
# nzv <- nearZeroVar(hhold_all_dummy)
# hhold_all_nzv <- hhold_all_dummy[, -nzv]
# # impute NAs in numeric variables
# hhold_all_nzv2 <- mutate_all(hhold_all_nzv, funs(ifelse(is.na(.),mean(.,na.rm=TRUE),.)))
# # highly correlated predictors
# descrCor <- cor(select_if(hhold_all_nzv2, is.numeric))
# summary(descrCor[upper.tri(descrCor)])
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
# hhold_all_corr <- select_if(hhold_all_nzv2, is.numeric)[,-highlyCorDescr] %>%
#   cbind(select_if(hhold_all_nzv2, is.character))
# # linear dependencies
# comboInfo <- findLinearCombos(select_if(hhold_all_corr, is.numeric))
# if (!is.null(comboInfo$remove)) {
#   hhold_all_ldep <- hhold_all_corr[, -comboInfo$remove]
# } else {
#   hhold_all_ldep <- hhold_all_corr
# }
# # split it back into train and test
# train_hhold <- merge(hhold_train_ids_class, hhold_all_ldep, by = "id")
# test_hhold <- merge(distinct(hhold_test, id) %>% mutate(id = as.character(id)) , hhold_all_ldep, by = "id")
# 
# # put indiv and hhold together
# train_all <- merge(train_hhold,train_indiv, by = c("id","poor"))
# test_all <- merge(test_hhold,test_indiv, by = c("id"))
# #write.csv(train_all, "train_all.csv", row.names = FALSE)
# #write.csv(test_all, "test_all.csv", row.names = FALSE)

################################################################################################
################################################################################################
## Same pre-processing country by country

### download individual data -----------------------
cou <- "A"
# training
download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_indiv_train.csv"), "indiv_train.csv")
indiv_train <- fread("indiv_train.csv", stringsAsFactors = TRUE)
# testing
download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_indiv_test.csv"), "indiv_test.csv")
indiv_test <- fread("indiv_test.csv", stringsAsFactors = TRUE)
# 
### download household data -----------------------
# training
download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_hhold_train.csv"), "hhold_train.csv")
hhold_train <- fread("hhold_train.csv", stringsAsFactors = TRUE)
# testing
download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/",cou,"_hhold_test.csv"), "hhold_test.csv")
hhold_test <- fread("hhold_test.csv", stringsAsFactors = TRUE)

### Pre-processing individual data ------------------------
# append train and test
indiv_all <- bind_rows(indiv_train,indiv_test)
# keep train ids and classes for later
indiv_train_ids_class <- select(indiv_train, id, poor) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(id = as.character(id), poor = ifelse(poor,1,0))
# dummify variables
indiv_all_ids <- indiv_all[,c(1)]
indiv_all_class <- ifelse(indiv_all$poor == TRUE,1,0)
dummies <- dummyVars(id ~ ., data = select(indiv_all, -poor,-country))
dummies_pred <- predict(dummies, newdata = indiv_all) %>% as.data.frame()
indiv_all_dummy <- cbind(indiv_all_ids,dummies_pred) %>%
  mutate(id = as.character(id))
# zero & near-zero variance
nzv <- nearZeroVar(indiv_all_dummy)
indiv_all_nzv <- indiv_all_dummy[, -nzv]
indiv_all_nzv <- select(indiv_all_nzv, -iid)
# highly correlated predictors
descrCor <- cor(select_if(indiv_all_nzv, is.numeric))
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
#indiv_all_corr <- indiv_all_nzv[,-highlyCorDescr]
indiv_all_corr <- select_if(indiv_all_nzv, is.numeric)[,-highlyCorDescr] %>%
   cbind(select_if(indiv_all_nzv, is.character))

# linear dependencies
comboInfo <- findLinearCombos(select_if(indiv_all_corr, is.numeric))
if (!is.null(comboInfo$remove)) {
  indiv_all_ldep <- indiv_all_corr[, -comboInfo$remove]
} else {
  indiv_all_ldep <- indiv_all_corr
}
# aggregate at household level
indiv_all_aggr <- indiv_all_ldep %>%
  mutate(id = as.character(id)) %>%
  group_by(id) %>%
  summarise_all(mean)
# split it back into train and test
train_indiv <- merge(indiv_train_ids_class, indiv_all_aggr, by = "id")
test_indiv <- merge(distinct(indiv_test, id) %>% mutate(id = as.character(id)) , indiv_all_aggr, by = "id")


### Pre-processing hhold data ------------------------
# append train and test
hhold_all <- bind_rows(hhold_train,hhold_test) %>%
   mutate_if(is.character, as.factor) %>%
   mutate_if(is.factor, addNA)

# keep train ids and classes for later
hhold_train_ids_class <- select(hhold_train, id, poor) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(id = as.character(id), poor = ifelse(poor,1,0))
# dummify variables
hhold_all_ids <- hhold_all[,c(1)]
hhold_all_class <- ifelse(hhold_all$poor == TRUE,1,0)
dummies <- dummyVars(id ~ ., data = select(hhold_all, -poor,-country))
dummies_pred <- predict(dummies, newdata = hhold_all) %>% as.data.frame()
hhold_all_dummy <- cbind(id = hhold_all_ids,dummies_pred) %>%
  mutate(id = as.character(id))
# zero & near-zero variance
nzv <- nearZeroVar(hhold_all_dummy)
hhold_all_nzv <- hhold_all_dummy[, -nzv]
# highly correlated predictors
descrCor <- cor(select_if(hhold_all_nzv, is.numeric))
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
hhold_all_corr <- select_if(hhold_all_nzv, is.numeric)[,-highlyCorDescr] %>%
  cbind(select_if(hhold_all_nzv, is.character))

# linear dependencies
comboInfo <- findLinearCombos(select_if(hhold_all_corr, is.numeric))
if (!is.null(comboInfo$remove)) {
  hhold_all_ldep <- hhold_all_corr[, -comboInfo$remove]
} else {
  hhold_all_ldep <- hhold_all_corr
}
# split it back into train and test
train_hhold <- merge(hhold_train_ids_class, hhold_all_ldep, by = "id")
test_hhold <- merge(distinct(hhold_test, id) %>% mutate(id = as.character(id)) , hhold_all_ldep, by = "id")

# put indiv and hhold together
train_all <- merge(train_hhold,train_indiv, by = c("id","poor"))
test_all <- merge(test_hhold,test_indiv, by = c("id"))


