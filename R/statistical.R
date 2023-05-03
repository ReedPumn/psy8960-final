# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
stat_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble()

# Analyses
# H1 Test: There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
stat_tbl %>%
  cor_test(MonthlyIncome, PerformanceRating)

# H2 Test: Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).
stat_tbl %>%
  anova_test(MonthlyIncome ~ Department)
# Post-hoc analyses since the ANOVA was signicicant.
stat_tbl %>%
  tukey_hsd(MonthlyIncome ~ Department)
