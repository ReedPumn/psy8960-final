# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
stat_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble()

# Analyses
# H1 Test: There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
H1cor <- cor_test(stat_tbl, MonthlyIncome, PerformanceRating)
H1cor

# H2 Test: Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).
H2anova <- anova_test(stat_tbl, MonthlyIncome ~ Department, detailed = TRUE)
H2anova
# Post-hoc analyses since the ANOVA was signicicant.
H2tukey <- tukey_hsd(stat_tbl, MonthlyIncome ~ Department)
H2tukey

# H3 Test: Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
H3lm <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = stat_tbl)
H3anova <- anova_test(H3lm, detailed = TRUE)
H3anova

# Visualizations
# H1 scatterplot. There was a perfectly bimodal distribution in performance rating scores, which obfuscated interpretations So I jittered the data. I considered adjusting the y-axis units to dispaly the full range of possible ranking values, but without context on that variable's full scale, I chose to leave the plotted scale as is. For this and all other graphs, I chose to avoid going too far with color to keep it simple and avoid possible challenges to inclusiveness with colorblindness.
(ggplot(data = stat_tbl, aes(x = MonthlyIncome, y = PerformanceRating)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Mothly Income", y = "Performance Rating")) %>%
  ggsave(filename = "../figs/H1.png", units = "px", width = 1920, height = 1080)

# H2 boxplot. 
(ggplot(data = stat_tbl, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot() +
  labs(x = "Departments", y = "Monthly Income")) %>%
  ggsave(filename = "../figs/H2.png", units = "px", width = 1920, height = 1080)

# H3 moderated linear regression. To graph the marginal effects, rather than raw data, I put the fitted (i.e., predicted) values on the y-axis
(ggplot(data = stat_tbl, aes(x = RelationshipSatisfaction, y = H3lm$fitted.values, group = Gender, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Relationship Satisfaction", y = "Years at Company")) %>%
  ggsave(filename = "../figs/H3.png", units = "px", width = 1920, height = 1080)

# Publication
# Publication results for H1
# Monthly pay and performance ratings produced the following correlation: `r str_remove(round(H1cor$cor, 2), pattern = "^(-)?0")`. This was not a significant relationship at the alpha level of 0.05.

# Publication results for H2
# Monthly pay was found to differ by department: F(`r H2anova$DFn`, `r H2anovaDFd`) = `r format(round(H2anova$F, 2), nsmall = 2)`, p = `r str_remove(round(H2anova$p, 2), pattern = "^(-)?0")`. Tukey's HSD tests indicated that the Research and Development department was paid significantly more than the Sales department.

# Publication results for H3
# Years at the company were not significantly predicted by relationship satisfaction: F(`r H3anova$DFn`, `r H3anova$DFd`) = `r H3anova$F[[3]]`, p = `r str_remove(round(H3anova$p[[3]], 2), pattern = "^(-)?0")`. As a result, post-hoc analyses with Tukey's HSD were not conducted.

# Here are the three publication tables saved according to their hypothesis numbers.
(cor_test(stat_tbl, MonthlyIncome, PerformanceRating)) %>%
  write_csv(file = "../out/H1.csv")
(tukey_hsd(stat_tbl, MonthlyIncome ~ Department)) %>%
  write_csv(file = "../out/H1b.csv")
(anova_test(H3lm, detailed = TRUE)) %>%
  write_csv(file = "../out/H3.csv")