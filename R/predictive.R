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
# This line splits our training data into 10 folds. We do this to safely evaluate the central tendency and variability of our metrics of interest.
kfolds <- createFolds(no_text_train_tbl$Attrition, 10)
# This line creates our test set with 25% of our data. We do this to later test the predictive accuracy of our models.
no_text_test_tbl <- no_text_random_tbl[(no_text_random_75 + 1):nrow(no_text_random_tbl), ]

# I established parallel processing to speed up these analyses.
num_clusters <- makeCluster(7)
registerDoParallel(num_clusters)

# I started with a logistic regression since our outcome variable of Attrition is binomial. I set a logistic regression using the method = "glm" and family = "binomial" arguments. I 
LOG <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "glm",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE)
)
LOG

ENET <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "glmnet",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE)
)
ENET

RFORREST <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "ranger",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE))
RFORREST

EGB <- train(
  Attrition ~ .,
  no_text_train_tbl,
  method = "xgbLinear", 
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE))
EGB

holdout1 <- mean(as.integer(as.character(predict(LOG, no_text_test_tbl, na.action = na.pass))))
holdout2 <- mean(as.integer(as.character(predict(ENET, no_text_test_tbl, na.action = na.pass))))
holdout3 <- mean(as.integer(as.character(predict(RFORREST, no_text_test_tbl, na.action = na.pass))))
holdout4 <- mean(as.integer(as.character(predict(EGB, no_text_test_tbl, na.action = na.pass))))

stopCluster(num_clusters)
registerDoSEQ()

FirstR2 <- LOG$results$Accuracy %>%
  round(2) %>%
  str_remove(pattern = "^(?-)0")
SecondR2 <- max(ENET$results$Accuracy) %>%
  round(2) %>%
  str_remove(pattern = "^(?-)0")
ThirdR2 <- max(RFORREST$results$Accuracy) %>%
  round(2) %>%
  str_remove(pattern = "^(?-)0")
FourthR2 <- max(EGB$results$Accuracy) %>%
  round(2) %>%
  str_remove(pattern = "^(?-)0")
