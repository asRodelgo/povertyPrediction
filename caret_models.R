# caret models -----------
#
library(caret)
#### GBM model ####
model <- "gbm"
require(pROC)
require(e1071)

grid <- expand.grid(.interaction.depth = c(7),
                    .n.trees=seq(500),
                    .shrinkage=c(0.005),
                    .n.minobsinnode=5)

ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     repeats=10,
                     classProbs=TRUE,
                     summaryFunction=twoClassSummary)

# without parallelization this takes about an hour to run
set.seed(123)
poor_train <- mutate(poor_train, poor = ifelse(poor == 1,"poor","no_poor"))
gbmTune <- caret::train(poor ~., data=poor_train,
                 method="gbm",
                 metric="ROC",
                 tuneGrid=grid,
                 verbose=FALSE,
                 trControl=ctrl)
#######
# The results on train
ggplot(gbmTune) + theme(legend.position="top")
gbmPred <- predict(gbmTune, poor_train)
gbmProbs <- predict(gbmTune, poor_train, type="prob")
head(gbmProbs)
cm <- confusionMatrix(gbmPred, poor_train$poor)
cm
rocCurve <- roc(response=poor_train$poor,
                predictor=gbmProbs[,"poor"],
                levels=rev(levels(factor(poor_train$poor))))
rocCurve
plot(rocCurve,
     print.thres=c(.5,.2),
     print.thres.pch=16,
     print.thres.cex=1.2)

# predicted classes on test
gbmPred <- predict(gbmTune, poor_test)
# predicted probabilities on test
gbmProbs <- predict(gbmTune, poor_test, type="prob")
head(gbmProbs)
# put all predictions together
preds_df <- tibble(pred_prob = as.numeric(gbmProbs$poor), pred_class = as.character(gbmPred),
                   test_class = poor_test$poor)
# compute the mean log loss on the test data
.logLoss(preds_df[,c(1,3)])

# prepare submission file: id,country,poor
# add poor column to test_all just to fit the features() function defined above
test_all_poor <- mutate(test_all, poor = "unknown")
final_predictions <- predict(gbmTune, test_all_poor, type="prob")
submission_file <- tibble(id = test_all_poor$id,
                          country = cou,
                          poor = as.numeric(final_predictions$poor))
pov_gbm1_A <- submission_file

##############################################################################
### Random Forest ###
model <- "rf"
library(randomForest)
require(pROC)
require(e1071)

grid <- expand.grid(.mtry=5)

ctrl <- trainControl(method="repeatedcv",
                     number=5,
                     repeats=5,
                     classProbs=TRUE,
                     summaryFunction=twoClassSummary)

set.seed(123)
rfTune <- caret::train(poor ~., data=poor_train,
                       method="rf",
                       metric="ROC",
                       tuneGrid=grid,
                       verbose=FALSE,
                       trControl=ctrl)
#######
# The results
rfPred <- predict(rfTune, poor_train)
rfProbs <- predict(rfTune, poor_train, type="prob")

# predicted classes on test
rfPred <- predict(rfTune, poor_test)
# predicted probabilities on test
rfProbs <- predict(rfTune, poor_test, type="prob")
head(rfProbs)
# put all predictions together
preds_df <- tibble(pred_prob = as.numeric(rfProbs$poor), pred_class = as.character(rfPred),
                   test_class = poor_test$poor)
# compute the mean log loss on the test data
.logLoss(preds_df[,c(1,3)])

# prepare submission file: id,country,poor
# add poor column to test_all just to fit the features() function defined above
test_all_poor <- mutate(test_all, poor = "unknown")
final_predictions <- predict(rfTune, test_all_poor, type="prob")
submission_file <- tibble(id = test_all_poor$id,
                          country = cou,
                          poor = as.numeric(final_predictions$poor))
pov_rf1_A <- submission_file

########################################################################################
### KNN ###
# The model
model <- "knn"
# restrict to top x variables according to importance
#topVar <- as.character(rfImp$variable[1:15])
####
require(kknn)

grid <- expand.grid(.k = 20
)

ctrl <- trainControl(method="repeatedcv",
                     number=10,
                     repeats=10,
                     classProbs=TRUE,
                     summaryFunction=twoClassSummary)

#
set.seed(123)
knnTune <- caret::train(poor ~., data=poor_train,
                 method="knn",
                 metric="ROC",
                 tuneGrid=grid,
                 #preProcess = c("center","scale"),
                 #tuneLength = 50,
                 trControl=ctrl)

#######
# The results
knnPred <- predict(knnTune, poor_train)
knnProbs <- predict(knnTune, poor_train, type="prob")

# predicted classes on test
knnPred <- predict(knnTune, poor_test)
# predicted probabilities on test
knnProbs <- predict(knnTune, poor_test, type="prob")
head(knnProbs)
# put all predictions together
preds_df <- tibble(pred_prob = as.numeric(knnProbs$poor), pred_class = as.character(knnPred),
                   test_class = poor_test$poor)
# compute the mean log loss on the test data
.logLoss(preds_df[,c(1,3)])

# prepare submission file: id,country,poor
# add poor column to test_all just to fit the features() function defined above
test_all_poor <- mutate(test_all, poor = "unknown")
final_predictions <- predict(knnTune, test_all_poor, type="prob")
submission_file <- tibble(id = test_all_poor$id,
                          country = cou,
                          poor = as.numeric(final_predictions$poor))
pov_knn1_A <- submission_file






