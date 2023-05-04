# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fastDummies)

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
  # Now dummy code the other categorical variables that were not binary. I used the fastDummies package to quickly dummy code these data.
  dummy_cols(select_columns = c("BusinessTravel", "Department", "EducationField", "JobRole", "MaritalStatus")) %>%
  # Now that these variables are dummy coded, we remove the original columns.
  select(-BusinessTravel, -Department, -EducationField, -JobRole, -MaritalStatus)
