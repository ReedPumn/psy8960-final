# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
stat_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble()

# Analyses
# H1 Test: There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
(cor_test(stat_tbl, MonthlyIncome, PerformanceRating)) %>%
  write_csv(file = "../out/H1.csv")

# H2 Test: Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).
(anova_test(stat_tbl, MonthlyIncome ~ Department, detailed = TRUE)) %>%
  write_csv(file = "../out/H1a.csv")
# Post-hoc analyses since the ANOVA was signicicant.
(tukey_hsd(stat_tbl, MonthlyIncome ~ Department)) %>%
  write_csv(file = "../out/H1b.csv")

# H3 Test: Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
int <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = stat_tbl)
(anova_test(int, detailed = TRUE)) %>%
  write_csv(file = "../out/H3.csv")

# Visualizations

# Publication
# Publication results for H1
# Monthly pay and performance rating produced the following correlation: `r str_remove(round(cor_test(stat_tbl, MonthlyIncome, PerformanceRating)[[3]], 2), pattern = "^(-)?0")`. This was not a significant relationship at the alpha level of 0.05.
  