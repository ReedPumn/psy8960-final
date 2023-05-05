# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fastDummies)
library(haven)
library(caret)
library(parallel)
library(doParallel)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(ldatuning)
library(topicmodels)
library(tidytext)
set.seed(123)

# Data Import and Cleaning
# Start by creating a tibble from the previously created csv file.
no_text_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble() %>%
  select(Age:Employee_ID) %>%
  # No need to include these variables since they showed no variance.
  select(-EmployeeCount, -StandardHours, -Over18) %>% 
  # These variables were nonnormally distributed, so I log transformed them. There were other nonnormally distributing variables, but log transforming them introduced problems with a lot of data points reaching negative infinity, so I only log transformed variables where that did not occur.
  mutate(DistanceFromHome = log(DistanceFromHome),
         MonthlyIncome = log(MonthlyIncome),
         PercentSalaryHike = log(PercentSalaryHike)) %>%
  # I recoded binary categorical variables from characters to factors. 
  mutate(Attrition = as_factor(recode(Attrition, "No" = "0", "Yes" = "1")),
         Gender = as_factor(recode(Gender, "Female" = "0", "Male" = "1")),
         OverTime = as_factor(recode(OverTime, "No" = "0", "Yes" = "1"))) %>%
  # Now recode all other categorical variables as factors. To keep the factored data as numbers, rather than text, I converted them to numeric values and then back to factors.
  mutate(Attrition = as_factor(as.numeric(as_factor(Attrition))),
         Gender = as_factor(as.numeric(as_factor(Gender))),
         OverTime = as_factor(as.numeric(as_factor(OverTime))),
         BusinessTravel = as_factor(as.numeric(as_factor(BusinessTravel))),
         Department = as_factor(as.numeric(as_factor(Department))),
         EducationField = as_factor(as.numeric(as_factor(EducationField))),
         JobRole = as_factor(as.numeric(as_factor(JobRole))),
         MaritalStatus = as_factor(as.numeric(as_factor(MaritalStatus))))

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

# These four lines of code apply our models we trained on our test set data to evaluate how well they generalize to new data.
predicted1 <- predict(LOG, no_text_test_tbl, na.action = na.pass)
predicted2 <- predict(ENET, no_text_test_tbl, na.action = na.pass)
predicted3 <- predict(RFORREST, no_text_test_tbl, na.action = na.pass)
predicted4 <- predict(EGB, no_text_test_tbl, na.action = na.pass)

# The confusionMatrix function calculates the accuracy of our model based upon its predicted values using the predict function.
holdout1 <- confusionMatrix(predicted1, no_text_test_tbl$Attrition)
holdout2 <- confusionMatrix(predicted2, no_text_test_tbl$Attrition)
holdout3 <- confusionMatrix(predicted3, no_text_test_tbl$Attrition)
holdout4 <- confusionMatrix(predicted4, no_text_test_tbl$Attrition)

# Adding text data to the model:
# First bring in our original tbl, but this time add the two free-response text items. I used the same code as before with one slight alteration that is commented out.
text_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble() %>%
  # select(Age:Employee_ID) %>% I commented out this line out so that the two free response items would be included
  select(-EmployeeCount, -StandardHours, -Over18) %>%
  mutate(DistanceFromHome = log(DistanceFromHome),
         MonthlyIncome = log(MonthlyIncome),
         PercentSalaryHike = log(PercentSalaryHike)) %>%
  mutate(Attrition = recode(Attrition, "No" = "0", "Yes" = "1"),
         Gender = recode(Gender, "Female" = "0", "Male" = "1"),
         OverTime = recode(OverTime, "No" = "0", "Yes" = "1")) %>%
  dummy_cols(select_columns = c("BusinessTravel", "Department", "EducationField", "JobRole", "MaritalStatus")) %>%
  mutate(Attrition = as_factor(Attrition),
         Gender = as_factor(Gender),
         OverTime = as_factor(OverTime)) %>%
  mutate(across(`BusinessTravel_Non-Travel`:MaritalStatus_Single, factor)) %>%
  select(-BusinessTravel, -Department, -EducationField, -JobRole, -MaritalStatus) %>%
  select(-Employee_ID) %>%
  # Remove any and all rows with NAs there are part of the two free-response text questions. We do this to not introduce problems when making our corpus.
  drop_na()

# Create a corpus for the two free-response questions.
corpus <- VCorpus(VectorSource(c(text_tbl$The_Good, text_tbl$The_Bad)))

# This series of pipes creates that trimmed corpus.Removing punctuation is particularly relevant for these data.
corpus_trimmed <- corpus %>%
  tm_map(content_transformer(qdap::replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c(stopwords("en"), "can")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(lemmatize_words))

# Create the DTM. We first tokenize our data to include 2-gram tokens. Given the relatively small amount of data at my disposal, I was hesitant to consider 3-gram tokens.
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
}
DTM <- DocumentTermMatrix(corpus_trimmed, control = list(tokenize = tokenizer))
slim_DTM <- removeSparseTerms(DTM, .997)
# Now transform the DTM to a tibble.
slim_DTM_tbl <- as_tibble(as.matrix(slim_DTM))
# Now that we have a tibble of our two free-response questions' content, we can merge it with the original tibble. Except I need to go back and ensure these two tibbles have the Employee_ID columns so that they can be merged.
full_tbl <- full_join(no_text_tbl, slim_DTM_tbl, by = "Employee_ID")

# This series of lines creates our week12_tbl. We first create two tibbles with ids to enable joining.
week12_tbl_with_ids <- week12_tbl %>%
  mutate(doc_id = as.character(1:nrow(week12_tbl)))
topics_tbl_with_ids <- tibble(doc_id = Docs(io_dtm))

# Publication
# Create a tibble to summarize and display the results of our four models across both training and test data.
table1_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  Train_Accuracy = c(FirstR2, SecondR2, ThirdR2, FourthR2),
  Test_Accuracy = c(round(holdout1$overall[[1]], 2), round(holdout2$overall[[1]], 2), round(holdout3$overall[[1]], 2), round(holdout4$overall[[1]], 2)))
# What characteristics of how you created the final model likely made the biggest impact in maximizing its performance? How do you know? Be sure to interpret specific numbers in the table you just created.
# The single decision that most likely impact the final model's performance was my inclusion of all available data as predictors. The instructions for this assignment were to "develop the model you believe likely to best predict turnover in new samples using all available cases and all available variables, in some way." Because I used all variables to predict turnover, I likely overfitted my training set data, thereby making my models poorly generalize to other data. I believe I overfitted my data because the training set data had incredibly impressive accuracy across ALL models (they all ranged from .90 to .99). Because they were so well fitted to the training data, accuracy was incredibly low across ALL models when applied to test data (accuracy ranged from .08 to .11). Other characteristics of my models do not likely have a large chance to meaningfully influence the results because of this overfitting. If I were to continue refining these models, I would consider using less predictors to avoid overfitting my training set data. Doing so may reduce the model's initial accuracy, but it would likely improve my models' accuracy when applied to other data.