# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning
# Start by importing the dataset tibble. It uses a + to separate data, so we use that as the delimiter.
import_tbl_1 <- read_delim("../data/dataset.csv", delim = "+")
# There are zero missing values in this tibble.
sum(is.na(import_tbl_1))

# Now import the free-response csv. There is a non-fixed number of cells in each row, so I used the col_types = cols() argument to let tidyverse organize the data by grouping it appropriately. Each response to the three questions are separated by a period, so I used a period as the delimiter.
import_tbl_2 <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = (FALSE), col_types = cols()) %>%
  rename(c("The_Good" = X1, "The_Bad" = X2, "Employee_ID" = X3))
# There are 106 missing values in this tibble.
sum(is.na(import_tbl_2))
