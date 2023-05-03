# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# Data Import and Cleaning
stat_tbl <- read.csv("../data/full_dataset.csv") %>%
  as_tibble()

# Analyses
# H1 Test: There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
cor_test(stat_tbl, MonthlyIncome, PerformanceRating)

# H2 Test: Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).
anova_test(stat_tbl, MonthlyIncome ~ Department, detailed = TRUE)
# Post-hoc analyses since the ANOVA was signicicant.
tukey_hsd(stat_tbl, MonthlyIncome ~ Department)

# H3 Test: Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
int <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = stat_tbl)
anova_test(int, detailed = TRUE)

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

# H3 moderated linear regression. I added a jitter function to display more variance in the relationship satisfaction variable.
ggplot(data = stat_tbl, aes(x = RelationshipSatisfaction, y = int$residuals, group = Gender, color = Gender)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Relationship Satisfaction", y = "Years at Compan")

# Publication
# Publication results for H1
# Monthly pay and performance rating produced the following correlation: `r str_remove(round(cor_test(stat_tbl, MonthlyIncome, PerformanceRating)[[3]], 2), pattern = "^(-)?0")`. This was not a significant relationship at the alpha level of 0.05.


# Here are the three publication tables saved according to their hypothesis numbers.
(cor_test(stat_tbl, MonthlyIncome, PerformanceRating)) %>%
  write_csv(file = "../out/H1.csv")
(tukey_hsd(stat_tbl, MonthlyIncome ~ Department)) %>%
  write_csv(file = "../out/H1b.csv")
(anova_test(int, detailed = TRUE)) %>%
  write_csv(file = "../out/H3.csv")