# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(GGally)

# Data Import and Cleaning
no_text_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble() %>%
  select(Age:Employee_ID) %>%
  # No need to include these variables since they showed no variance.
  select(-EmployeeCount, -StandardHours, -Over18) %>% 
  # These variables wer nonnormally distributed, so I log transformed them. There were other nonnormally distributing variables, but log transforming them introduced problems with a lot of data points reaching negative infinity, so I only log transformed variables where that did not occur.
  mutate(DistanceFromHome = log(DistanceFromHome),
         MonthlyIncome = log(MonthlyIncome),
         PercentSalaryHike = log(PercentSalaryHike),
         TotalWorkingYears = log(TotalWorkingYears)) %>%
  # I recoded binary categorical variables from characters to 0s and 1s. 
  mutate(Attrition = recode(Attrition, "No" = "0", "Yes" = "1"),
         Gender = recode(Gender, "Female" = "0", "Male" = "1"),
         OverTime = recode(OverTime, "No" = "0", "Yes" = "1")) %>%
  # Make these binary categorical variables characters, rather than integers.
  mutate(Attrition = as.character(Attrition),
         Gender = as.character(Gender),
         OverTime = as.character(OverTime)) %>%
  # Some categorical variables, such as MaritalStatus were not binary. They were excluded from the dataset to prevent too many dummy variables from obfuscating the model's interpretability.
  select(-BusinessTravel, -Department, -EducationField, -JobRole, -MaritalStatus)
