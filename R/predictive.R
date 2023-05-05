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

# These four lines of code calculate the accuracy of our models. Accuracy for our models refers to correctly identifying whether or not the given employee has experienced job attrition. Our accuracy for each model was very high.
FirstR2 <- round(LOG$results$Accuracy, 2)
SecondR2 <- round(max(ENET$results$Accuracy), 2)
ThirdR2 <- round(max(RFORREST$results$Accuracy), 2)
FourthR2 <- round(max(EGB$results$Accuracy), 2)

# These four lines of code apply our models we trained on our test set data to predict attrition values in the test data.
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
  mutate(Attrition = as_factor(recode(Attrition, "No" = "0", "Yes" = "1")),
         Gender = as_factor(recode(Gender, "Female" = "0", "Male" = "1")),
         OverTime = as_factor(recode(OverTime, "No" = "0", "Yes" = "1"))) %>%
  mutate(Attrition = as_factor(as.numeric(as_factor(Attrition))),
         Gender = as_factor(as.numeric(as_factor(Gender))),
         OverTime = as_factor(as.numeric(as_factor(OverTime))),
         BusinessTravel = as_factor(as.numeric(as_factor(BusinessTravel))),
         Department = as_factor(as.numeric(as_factor(Department))),
         EducationField = as_factor(as.numeric(as_factor(EducationField))),
         JobRole = as_factor(as.numeric(as_factor(JobRole))),
         MaritalStatus = as_factor(as.numeric(as_factor(MaritalStatus))))  %>%
  # Remove any and all rows with NAs there are part of the two free-response text questions. We do this to not introduce problems when making our corpus.
  drop_na()

# Create a corpus for each of the two free-response questions.
corpus_good <- VCorpus(VectorSource(text_tbl$The_Good))
corpus_bad <- VCorpus(VectorSource(text_tbl$The_Bad))

# This series of pipes creates trimmed corpuses. Removing punctuation is particularly relevant for these data.
corpus_good_trimmed <- corpus_good %>%
  tm_map(content_transformer(qdap::replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c(stopwords("en"), "can")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(lemmatize_words))

corpus_bad_trimmed <- corpus_bad %>%
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
DTM_good <- DocumentTermMatrix(corpus_good_trimmed, control = list(tokenize = tokenizer))
DTM_bad <- DocumentTermMatrix(corpus_bad_trimmed, control = list(tokenize = tokenizer))

# Determine how many topics exist in the tibbles. For good responses, 5 topics seem appropriate.
topics_good <- FindTopicsNumber(DTM_good, 
                                topics = seq(2, 10, by = 1),
                                metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), 
                                verbose = TRUE)
FindTopicsNumber_plot(topics_good)
# Determine how many topics exist. For bad responses, 4 topics seem appropriate.
topics_bad <- FindTopicsNumber(DTM_bad, 
                                topics = seq(2, 10, by = 1),
                                metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), 
                                verbose = TRUE)
FindTopicsNumber_plot(topics_bad)

# I chose to analyze our good data with 5 topics and our bad data with 4 topics based on findings from the previous plots.
lda_good_results <- LDA(DTM_good, 5)
lda_bad_results <- LDA(DTM_bad, 4)
# This line documents our beta matrix, noting the likelihood of each word appearing in the data.
lda_good_betas <- tidy(lda_good_results, matrix = "beta")
lda_bad_betas <- tidy(lda_bad_results, matrix = "beta")
# Similarly, this line documents our gamma matrix, noting the likelihood fo each document appearing in the data.
 
# Turn our two DTMs into tibbles. I focused on the gamma values since they correspond directly to topics that each employee discussed. I grouped the data by each document to get the desired number of rows corresponding to the number of employees. Arranging data by employee ids is much cleaner.
lda_good_gammas <- tidy(lda_good_results, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(n = 1, gamma, with_ties = FALSE) %>%
  mutate(Employee_ID = as.integer(document),
         topic = topic_good) %>%
  arrange(Employee_ID)
lda_bad_gammas <- tidy(lda_bad_results, matrix = "gamma") %>%
  group_by(document) %>%
  slice_max(n = 1, gamma, with_ties = FALSE) %>%
  mutate(Employee_ID = as.integer(document),
         topic = topic_bad) %>%
  arrange(Employee_ID)

# Create one tibble with both quantitative and qualitative information.
full_tbl <- no_text_tbl %>%
  full_join(lda_good_gammas) %>%
  select(-document, -gamma, -topic) %>%
  full_join(lda_bad_gammas) %>%
  select(-document, -gamma, -topic)

# Just as before, these lines of code create the test and training sets in our data.
full_random_tbl <- full_tbl[sample(nrow(full_tbl)), ]
full_random_75 <- round(nrow(full_random_tbl) * 0.75, 0)
full_train_tbl <- full_random_tbl[1:full_random_75, ]
kfolds <- createFolds(full_train_tbl$Attrition, 10)
full_test_tbl <- full_random_tbl[(full_random_75 + 1):nrow(full_random_tbl), ]

# I established parallel processing to speed up these analyses.
num_clusters <- makeCluster(7)
registerDoParallel(num_clusters)

# I am running the same models as before to enable fair comparisons.
LOG_full <- train(
  Attrition ~ .,
  full_train_tbl,
  method = "glm",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE)
)
# Evaluate the model to check for NA values or errors. I do the same for all other models.
LOG_full

ENET_full <- train(
  Attrition ~ .,
  full_train_tbl,
  method = "glmnet",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE)
)
ENET_full

RFORREST_full <- train(
  Attrition ~ .,
  full_train_tbl,
  method = "ranger",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE))
RFORREST_full

EGB_full <- train(
  Attrition ~ .,
  full_train_tbl,
  method = "xgbLinear",
  tuneLength = 3,
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method = "cv", indexOut = kfolds, number = 10, search = "grid", verboseIter = TRUE))
EGB_full

# Turn off parallel processing now that the hard computations are done.
stopCluster(num_clusters)
registerDoSEQ()

# Just as before, these four lines of code calculate the accuracy of our models.
FirstR2_full <- round(LOG_full$results$Accuracy, 2)
SecondR2_full <- round(max(ENET_full$results$Accuracy), 2)
ThirdR2_full <- round(max(RFORREST_full$results$Accuracy), 2)
FourthR2_full <- round(max(EGB_full$results$Accuracy), 2)

# These four lines of code apply our models we trained on our test set data to predict attrition values in the test data.
predicted1_full <- predict(LOG_full, full_test_tbl, na.action = na.pass)
predicted2_full <- predict(ENET_full, full_test_tbl, na.action = na.pass)
predicted3_full <- predict(RFORREST_full, full_test_tbl, na.action = na.pass)
predicted4_full <- predict(EGB_full, full_test_tbl, na.action = na.pass)

# The confusionMatrix function calculates the accuracy of our model based upon its predicted values using the predict function.
holdout1_full <- confusionMatrix(predicted1_full, full_test_tbl$Attrition)
holdout2_full <- confusionMatrix(predicted2_full, full_test_tbl$Attrition)
holdout3_full <- confusionMatrix(predicted3_full, full_test_tbl$Attrition)
holdout4_full <- confusionMatrix(predicted4_full, full_test_tbl$Attrition)

# Publication
# Create a tibble to summarize and display the results of our four models across both training and test data.
table1_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  Train_Accuracy = c(FirstR2, SecondR2, ThirdR2, FourthR2),
  Test_Accuracy = c(round(holdout1$overall[[1]], 2), round(holdout2$overall[[1]], 2), round(holdout3$overall[[1]], 2), round(holdout4$overall[[1]], 2)))
# What characteristics of how you created the final model likely made the biggest impact in maximizing its performance? How do you know? Be sure to interpret specific numbers in the table you just created.
# The single decision that most likely impact the final model's performance was my inclusion of all available data as predictors. The instructions for this assignment were to "develop the model you believe likely to best predict turnover in new samples using all available cases and all available variables, in some way." Because I used all variables to predict turnover, I likely overfitted my training set data, thereby making my models poorly generalize to other data. I believe I overfitted my data because the training set data had incredibly impressive accuracy across ALL models (they all ranged from .90 to .99). Because they were so well fitted to the training data, accuracy was incredibly low across ALL models when applied to test data (accuracy ranged from .08 to .11). Other characteristics of my models do not likely have a large chance to meaningfully influence the results because of this overfitting. If I were to continue refining these models, I would consider using less predictors to avoid overfitting my training set data. Doing so may reduce the model's initial accuracy, but it would likely improve my models' accuracy when applied to other data.

# What is the incremental predictive accuracy gained by including text data in your model versus not including text data? In the Publication section, include a summary table comparing predictive accuracy of your final model with and without text-derived predictors, provide an answer in a comment, and explain your reasoning.
# Model fit did not meaningfully improve because ...
table2_tbl <- tibble(
  algo = c("OLS Regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
  No_Text_Train_Accuracy = c(FirstR2, SecondR2, ThirdR2, FourthR2),
  No_Text_Test_Accuracy = c(round(holdout1$overall[[1]], 2), round(holdout2$overall[[1]], 2), round(holdout3$overall[[1]], 2), round(holdout4$overall[[1]], 2)),
  With_Text_Train_Accuracy = c(FirstR2_full, SecondR2_full, ThirdR2_full, FourthR2_full),
  With_Text_Test_Accuracy = c(round(holdout1_full$overall[[1]], 2), round(holdout2_full$overall[[1]], 2), round(holdout3_full$overall[[1]], 2), round(holdout4_full$overall[[1]], 2)))
