## Classification model using Tensorflow via Estimators --------------
#
# Alberto Sanchez 2/10/2017
##################################################################

# Use pre-processed data from data_processing.R
# train_all and test_all

#install.packages("tfestimators")
library(tfestimators)

response <- function() "poor"
features <- function() setdiff(names(train_all[,-1]), response())
# 
# split into train, test datasets
set.seed(123)
partitions <- modelr::resample_partition(train_all, c(test = 0.2, train = 0.8))
poor_train <- as.data.frame(partitions$train)[,-1]
poor_test  <- as.data.frame(partitions$test)[,-1]

# construct feature columns
feature_columns <- feature_columns(
  column_numeric(features())
)

# construct classifier
classifier <- dnn_classifier(
  feature_columns = feature_columns,
  hidden_units = c(10,20,10),
  n_classes = 2
)

# construct input function
poor_input_fn <- function(data) {
  input_fn(data, features = features(), response = response())
}

# train classifier with training dataset
set.seed(123)
train(classifier, input_fn = poor_input_fn(poor_train))

# valuate with test dataset
predictions <- predict(classifier, input_fn = poor_input_fn(poor_test))
evaluation <- evaluate(classifier, input_fn = poor_input_fn(poor_test))
# predicted classes vs test classes
preds_df <- tibble(pred_prob = as.numeric(predictions$logistic), pred_class = as.numeric(predictions$classes),
                   test_class = poor_test$poor)

table(preds_df$test_class,preds_df$pred_class)

# compute the mean log loss on the test data
.logLoss(preds_df[,c(1,3)])

# prepare submission file: id,country,poor
# add poor column to test_all just to fit the features() function defined above
test_all_poor <- mutate(test_all, poor = 0)
final_predictions <- predict(classifier, input_fn = poor_input_fn(test_all_poor[,-1]))
submission_file <- tibble(id = test_all_poor$id, 
                          country = cou,
                          poor = as.numeric(final_predictions$logistic))
pov_keras1_A <- submission_file



