# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning
# Start by importing the dataset as a tibble. It uses a plus sign to separate data, so we use that as the delimiter. I added an Employee_ID column indicating the row number to enable merging this tibble with the next tibble.
import_tbl_1 <- read_delim("../data/dataset.csv", delim = "+") %>%
  mutate(Employee_ID = row_number())
# There are zero missing values in this tibble.
sum(is.na(import_tbl_1))

# Now import the free-response csv. There is a non-fixed number of cells in each row, so I used the col_types = cols() argument to let tidyverse organize the data by grouping it appropriately. Each response to the three questions are separated by a period, so I used a period as the delimiter.
import_tbl_2 <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = (FALSE), col_types = cols()) %>%
  rename(c("The_Good" = X1, "The_Bad" = X2, "Employee_ID" = X3))
# There are 106 missing values in this tibble.
sum(is.na(import_tbl_2))
# We are missing 32 employees' responses due to missing 32 Employee_ID numbers.
1470 - nrow(import_tbl_2)

# Join the two tibbles.
full_tbl <- full_join(import_tbl_1, import_tbl_2, by = "Employee_ID") %>%
  write_csv(file = "../data/full_dataset.csv")
# There are 170 missing values in the merged tibble. This adds up. 170 - 106 = 64 total missing values across tibbles. 32 of these values are from missing ID numbers in both "The_Good" and 32 from "The_Bad" columns, because no data could be imported into these cells for the 32 employees that were missing from import_tbl_2. 
sum(is.na(full_tbl))

# I considered changing all categorical variables to factors at this point to make them appropriately typed, but stylistically I like keeping each alteration in the respective R file. It helps prevent a user from having to navigate multiple R files to understand the data. So I made further alterations to the full_tbl in the respective R files, but itialized it in this R file.
