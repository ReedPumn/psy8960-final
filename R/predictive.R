# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fastDummies)
library(haven)
library(caret)
library(parallel)
library(doParallel)
set.seed(123)

# Data Import and Cleaning
# Start by creating a tibble from the previously created csv file.
no_text_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble() %>%
  select(Age:Employee_ID) %>%
  # No need to include these variables since they showed no variance.
  select(-EmployeeCount, -StandardHours, -Over18) %>% 
  # These variables wer nonnormally distributed, so I log transformed them. There were other nonnormally distributing variables, but log transforming them introduced problems with a lot of data points reaching negative infinity, so I only log transformed variables where that did not occur.
  mutate(DistanceFromHome = log(DistanceFromHome),
         MonthlyIncome = log(MonthlyIncome),
         PercentSalaryHike = log(PercentSalaryHike)) %>%
  # I recoded binary categorical variables from characters to 0s and 1s. 
  mutate(Attrition = recode(Attrition, "No" = "0", "Yes" = "1"),
         Gender = recode(Gender, "Female" = "0", "Male" = "1"),
         OverTime = recode(OverTime, "No" = "0", "Yes" = "1")) %>%
  # Now dummy code the other categorical variables that were not binary. I used the fastDummies package to quickly dummy code these data.
  dummy_cols(select_columns = c("BusinessTravel", "Department", "EducationField", "JobRole", "MaritalStatus")) %>%
  # Now recode all categorical variables as factors with these two sets of pipes. 
  mutate(Attrition = as_factor(Attrition),
         Gender = as_factor(Gender),
         OverTime = as_factor(OverTime)) %>%
  mutate(across(`BusinessTravel_Non-Travel`:MaritalStatus_Single, factor)) %>%
  # Now that these variables are dummy coded, we remove the original columns.
  select(-BusinessTravel, -Department, -EducationField, -JobRole, -MaritalStatus) %>%
  # Also remove the Employee_ID column since its inclusion would only add random noise to the model
  select(-Employee_ID)

# Analysis
# This line randomizes the rows in our tibble to be later divided into training and test sets.
no_text_random_tbl <- no_text_tbl[sample(nrow(no_text_tbl)), ]
# This line lets us know where to "slice" our data in half. We use this slice point to create our training data and test data.
no_text_random_75 <- round(nrow(no_text_random_tbl) * 0.75, 0)
# This line creates our training set with 75% of our data. We do this to give our models enough data to form stable predictions to new data.
no_text_train_tbl <- no_text_random_tbl[1:no_text_random_75, ]
# This line splits our training data into 10 folds. We do this to more consistently evaluate how well our models predict.
kfolds <- createFolds(no_text_train_tbl$Attrition, 10)
# This line creates our test set with 25% of our data. We do this to later test the predictive accuracy of our models.
no_text_test_tbl <- no_text_random_tbl[(no_text_random_75 + 1):nrow(no_text_random_tbl), ]

# I established parallel processing to speed up these analyses.
num_clusters <- makeCluster(7)
registerDoParallel(num_clusters)

# I started with a logistic regression since our outcome variable of Attrition is binomial. I set a logistic regression using the method = "glm" and family = "binomial" arguments. I also chose to run an elastic net, random forest, and an eXtreme gradient boosting model to triangulate results across multiple possible models. 
LOG <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "glm",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE)
)
# Evaluate the model to check for NA values or errors. I do the same for all other models.
LOG

ENET <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "glmnet",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE)
)
ENET

RFORREST <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "ranger",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE))
RFORREST

EGB <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "xgbLinear",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE))
EGB

# Turn off parallel processing now that the hard computations are done.
stopCluster(num_clusters)
registerDoSEQ()

# These four pipes calculate the accuracy of our models. Accuracy for our models refers to correctly identifying whether or not the given employee has experienced job attrition. Our accuracy for each model was very high.
FirstR2 <- round(LOG$results$Accuracy, 2)
SecondR2 <- round(max(ENET$results$Accuracy), 2)
ThirdR2 <- round(max(RFORREST$results$Accuracy), 2)
FourthR2 <- round(max(EGB$results$Accuracy), 2)

# These four lines of code apply our models we trained on our test set data to evaluate how well they generalize to new data. As we will see, the accuracy of these models was quite poor, suggesting that we overfitted our training data with our original models. There were a lot of conversions needed to turn our factor data into integer data. These conversions were needed to calculate the mean number of accurate predictions. Numbers here range from 0 to 1, representing the percent of correct catagorizations in our test data.
holdout1 <- mean(as.integer(as.character(predict(LOG, no_text_test_tbl, na.action = na.pass)))) %>%
  round(2)
holdout2 <- mean(as.integer(as.character(predict(ENET, no_text_test_tbl, na.action = na.pass)))) %>%
  round(2)
holdout3 <- mean(as.integer(as.character(predict(RFORREST, no_text_test_tbl, na.action = na.pass)))) %>%
  round(2)
holdout4 <- mean(as.integer(as.character(predict(EGB, no_text_test_tbl, na.action = na.pass)))) %>%
  round(2)

# Publication
# Create a tibble to summarize and display the results of our four models across both training and test data.
table1_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  cv_rsq = c(FirstR2, SecondR2, ThirdR2, FourthR2),
  ho_rsq = c(holdout1, holdout2, holdout3, holdout4)
)
# What characteristics of how you created the final model likely made the biggest impact in maximizing its performance? How do you know? Be sure to interpret specific numbers in the table you just created.
# The single decision that most likely impact the final model's performance was my inclusion of all available data as predictors. The instructions for this assignment were to "develop the model you believe likely to best predict turnover in new samples using all available cases and all available variables, in some way." Because I used all variables to predict turnover, I likely overfitted my training set data, thereby making my models poorly generalize to other data. I beleive I overfitted my data because the training set data had incredibly impressive accuracy across ALL models (they all ranged from .90 to .99). Because they were so well fitted to the training data, accuracy was incredibly low across ALL models when applied to test data (accuracy ranged from .08 to .11). If I were to continue refining these models, I would consider using less predictors to avoid overfitting my training set data. Doing so may reduce the model's initial accuracy, but it would likely improve my models' accuracy when applied to other data.