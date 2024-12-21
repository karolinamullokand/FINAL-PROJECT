# title: "FINAL PROJECT"
# author: Karolina Mullokand
# output: html_document
# date: "2024-12-20"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r start_point}
# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(haven)
library(plotly)

# Set working directory and load data
setwd("/Users/karolinamullokand/Desktop/ECO_B2000")
menu <- read.csv("SCFP2022.csv")
```

# Creating age groups, binary indicator for student loans, and income midpoint

```{r Menu_setup}
menu <- menu %>%
  mutate(
    age_group = case_when(
      AGE >= 18 & AGE <= 30 ~ "18-30",
      AGE >= 31 & AGE <= 65 ~ "31-65",
      TRUE ~ NA_character_
    ),
    has_student_loans = ifelse(HEDN_INST > 0, "Yes", "No"),
    income_midpoint = case_when(
      INCOME < 25000 ~ 12500,        # less than $25k
      INCOME >= 25000 & INCOME < 35000 ~ 30000, # $25k to $34.9k
      INCOME >= 35000 & INCOME < 50000 ~ 40000, # $35k to $49.9k
      INCOME >= 50000 & INCOME < 75000 ~ 62500, # $50k to $74.9k
      INCOME >= 75000 & INCOME < 100000 ~ 82500, # $75k to $99.9k
      INCOME >= 100000 & INCOME < 150000 ~ 125000, # $100k to $149k
      INCOME >= 150000 & INCOME < 200000 ~ 175000, # $150k to $199k
      INCOME >= 200000 ~ 225000,      # $200k and above
      TRUE ~ NA_real_                # handling missing or unexpected values
    )
  ) %>%
  filter(!is.na(age_group))
```

# Visualizing DEBT2INC distribution

```{r DEBT2INC}
quantiles <- quantile(menu$DEBT2INC, probs = c(0.01, 0.99), na.rm = TRUE)
print(quantiles)
```

# Winsorizing DEBT2INC to remove outliers

```{r method_outliers}
menu <- menu %>%
  mutate(
    DEBT2INC_cleaned = case_when(
      DEBT2INC > quantiles[2] ~ quantiles[2], 
      TRUE ~ DEBT2INC
    ),
    high_debt_to_income = ifelse(DEBT2INC_cleaned > 0.5, 1, 0)  # Create binary outcome
  )
```

```{r summaries}
summary(menu$DEBT2INC_cleaned)
summary(menu$EDN_INST)
```

# Hypothesis Testing: Debt-to-Income Ratio by Student Loan Status

```{r hypothesis_studentloan_status}
menu_student_loans <- menu %>% filter(has_student_loans == "Yes")
menu_no_student_loans <- menu %>% filter(has_student_loans == "No")

t_test_debt2inc_cleaned <- t.test(
  menu_student_loans$DEBT2INC_cleaned,
  menu_no_student_loans$DEBT2INC_cleaned,
  alternative = "greater",  # Test if DEBT2INC is greater for student loans
  var.equal = FALSE
)
print("T-Test Results for Debt-to-Income Ratio (Cleaned)")
print(t_test_debt2inc_cleaned)

# Subgroup Analysis: Age Groups
group_18_30 <- menu %>% filter(age_group == "18-30")
group_31_65 <- menu %>% filter(age_group == "31-65")
```

# Hypothesis Testing: Age Group 18-30

```{r hypothesis_18_30}
t_test_18_30_cleaned <- t.test(
  group_18_30 %>% filter(has_student_loans == "Yes") %>% pull(DEBT2INC_cleaned),
  group_18_30 %>% filter(has_student_loans == "No") %>% pull(DEBT2INC_cleaned),
  alternative = "greater",
  var.equal = FALSE
)
print("T-Test Results for Age Group 18-30 (Cleaned)")
print(t_test_18_30_cleaned)
```

# Hypothesis Testing: Age Group 31-65

```{r hypothesis_31_65}
t_test_31_65_cleaned <- t.test(
  group_31_65 %>% filter(has_student_loans == "Yes") %>% pull(DEBT2INC_cleaned),
  group_31_65 %>% filter(has_student_loans == "No") %>% pull(DEBT2INC_cleaned),
  alternative = "greater",
  var.equal = FALSE
)
print("T-Test Results for Age Group 31-65 (Cleaned)")
print(t_test_31_65_cleaned)
```

# Visualizations: Debt-to-Income Ratio by Student Loan Status

```{r plot18_30, echo=FALSE}
plot1_18_30 <- ggplot(group_18_30, aes(x = has_student_loans, y = DEBT2INC_cleaned, fill = has_student_loans)) +
  geom_boxplot() +
  labs(
    title = "Debt-to-Income Ratio for Age Group 18–30 by Student Loan Status",
    x = "Student Loan Status",
    y = "Debt-to-Income Ratio",
    fill = "Student Loan Status"
  ) +
  theme_minimal()
ggplotly(plot1_18_30)
```

```{r plot31_65, echo=FALSE}
plot1_31_65 <- ggplot(group_31_65, aes(x = has_student_loans, y = DEBT2INC_cleaned, fill = has_student_loans)) +
  geom_boxplot() +
  labs(
    title = "Debt-to-Income Ratio for Age Group 31–65 by Student Loan Status",
    x = "Student Loan Status",
    y = "Debt-to-Income Ratio",
    fill = "Student Loan Status"
  ) +
  theme_minimal()
ggplotly(plot1_31_65)
```

# OLS Regression Models: Cleaned DEBT2INC

```{r ols}
ols_18_30 <- lm(DEBT2INC_cleaned ~ HEDN_INST + income_midpoint + NETWORTH, data = group_18_30)
ols_31_65 <- lm(DEBT2INC_cleaned ~ HEDN_INST + income_midpoint + NETWORTH, data = group_31_65)

print("OLS Results for Age Group 18-30")
summary(ols_18_30)

print("OLS Results for Age Group 31-65")
summary(ols_31_65)
```

# Logit regression

```{r logit regression}

quantiles <- quantile(menu$DEBT2INC, probs = c(0.01, 0.99), na.rm = TRUE)

menu <- menu %>%
  mutate(
    DEBT2INC_cleaned = ifelse(DEBT2INC > quantiles[2], quantiles[2], DEBT2INC)
  )


menu <- menu %>%
  mutate(menu$high_debt_to_income, ifelse(menu$DEBT2INC_cleaned > 0.5, 1, 0))

# Re-create the subsets after the new variable
group_18_30 <- menu %>% filter(age_group == "18-30")
group_31_65 <- menu %>% filter(age_group == "31-65")

# Logistic Regression: High Debt-to-Income Ratio for each age group
menu <- menu %>%
  mutate(high_debt_to_income = ifelse(DEBT2INC_cleaned > 0.5, 1, 0))

logit_18_30 <- glm(
  high_debt_to_income ~ HEDN_INST + income_midpoint + NETWORTH,
  data = group_18_30, family = binomial
)

logit_31_65 <- glm(
  high_debt_to_income ~ HEDN_INST + income_midpoint + NETWORTH,
  data = group_31_65, family = binomial
)

# Summarize results
print("Logit Results for Age Group 18-30")
summary(logit_18_30)

print("Logit Results for Age Group 31-65")
summary(logit_31_65)
```

# Type I and Type II errors

```{r erorrs}
pred_vals1 <- predict(logit_18_30, group_18_30, type = "response")
pred_model_logit_18 <- (pred_vals1 > mean(pred_vals1))
print("Type I and Type II errors results 18-30")
table(pred = pred_model_logit_18, true = group_18_30$high_debt_to_income)


pred_vals2 <- predict(logit_31_65, group_31_65, type = "response")
pred_model_logit_31 <- (pred_vals2 > mean(pred_vals2))
print("Type I and Type II errors results 31-65")
table(pred = pred_model_logit_31, true = group_31_65$high_debt_to_income)

```

# Predicted Probabilities Visualization

```{r probab_vis}
menu$predicted_prob <- predict(logit_18_30, newdata = menu, type = "response")

plot2 <- ggplot(menu, aes(x = age_group, y = predicted_prob, fill = has_student_loans)) +
  geom_boxplot() +
  labs(
    title = "Predicted Probability of High Debt-to-Income Ratio by Age Group and Student Loan Status",
    x = "Age Group",
    y = "Predicted Probability",
    fill = "Student Loan Status"
  ) +
  theme_minimal()
ggplotly(plot2)
```

# Short and Long-term burden

```{r}
menu <- menu %>%
  mutate(
    short_term_burden = rowSums(select(., starts_with("PAYEDU")), na.rm = TRUE) / INCOME,
    long_term_burden = EDN_INST / INCOME)
menu <- menu %>%
  filter(!is.na(short_term_burden), !is.na(long_term_burden), INCOME > 0)

summary(menu$short_term_burden)
summary(menu$long_term_burden)
```

# Compare short-term burden between age groups

```{r comparing burden length}
menu <- menu %>%
  mutate(
    short_term_burden = rowSums(select(., starts_with("PAYEDU")), na.rm = TRUE) / INCOME
  ) %>%
  filter(!is.na(short_term_burden), INCOME > 0)
summary(menu$short_term_burden)
group_18_30 <- menu %>%
  filter(age_group == "18-30")

group_31_65 <- menu %>%
  filter(age_group == "31-65")

t_test_short_term_18_30 <- t.test(
  group_18_30$short_term_burden,
  group_31_65$short_term_burden,
  alternative = "two.sided",
  var.equal = FALSE
)
print("T-Test Results for Short-term Educational Loan Burden (18-30 vs 31-65)")
print(t_test_short_term_18_30)

# Compare long-term burden between age groups
t_test_long_term_18_30 <- t.test(
  group_18_30$long_term_burden,
  group_31_65$long_term_burden,
  alternative = "two.sided",
  var.equal = FALSE
)
print("T-Test Results for Long-term Educational Loan Burden (18-30 vs 31-65)")
print(t_test_long_term_18_30)
```

# Visualizations: Short-term vs Long-term Strain

```{r}
burden_summary <- menu %>%
  group_by(age_group) %>%
  summarise(
    mean_short_term = mean(short_term_burden, na.rm = TRUE),
    se_short_term = sd(short_term_burden, na.rm = TRUE) / sqrt(n()),
    mean_long_term = mean(long_term_burden, na.rm = TRUE),
    se_long_term = sd(long_term_burden, na.rm = TRUE) / sqrt(n())
  )

burden_summary_long <- burden_summary %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "burden_type",
    values_to = "mean"
  ) %>%
  mutate(
    se = if_else(str_detect(burden_type, "short_term"), se_short_term, se_long_term),
    burden_type = str_replace(burden_type, "mean_", "")
  ) %>%
  select(-se_short_term, -se_long_term)

plot_bar <- ggplot(burden_summary_long, aes(x = age_group, y = mean, fill = burden_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Average Educational Loan Burden by Age Group",
    x = "Age Group",
    y = "Average Burden",
    fill = "Burden Type"
  ) +
  theme_minimal()

ggplotly(plot_bar)

summary(burden_summary_long)
summary(burden_summary)
```

# Regression Analysis: Predictors of Short-term and Long-term Burden

```{r}
# Short-term burden regression for age group 18-30
ols_short_term_18_30 <- lm(menu$short_term_burden ~ menu$HEDN_INST + menu$income_midpoint + menu$NETWORTH, data = menu$group_18_30)
print("OLS Regression for Short-term Burden (Age Group 18-30)")
summary(ols_short_term_18_30)

ols_long_term_18_30 <- lm(menu$long_term_burden ~ menu$HEDN_INST + menu$income_midpoint + menu$NETWORTH, data = menu$group_18_30)
print("OLS Regression for Long-term Burden (Age Group 18-30)")
summary(ols_long_term_18_30)

ols_short_term_31_65 <- lm(menu$short_term_burden ~ menu$HEDN_INST + menu$income_midpoint + menu$NETWORTH, data = menu$group_31_65)
print("OLS Regression for Short-term Burden (Age Group 31-65)")
summary(ols_short_term_31_65)

# Long-term burden regression for age group 31-65
ols_long_term_31_65 <- lm(menu$long_term_burden ~ menu$HEDN_INST + menu$income_midpoint + menu$NETWORTH, data = menu$group_31_65)
print("OLS Regression for Long-term Burden (Age Group 31-65)")
summary(ols_long_term_31_65)
```

# Visualization regression for group 18-30

```{r}
# Extract coefficients for Age Group 18–30: Short-term Burden
coef_short_term_18_30 <- as.data.frame(coef(summary(ols_short_term_18_30)))
coef_short_term_18_30$Predictor <- rownames(coef_short_term_18_30)

plot_short_term_18_30 <- ggplot(coef_short_term_18_30, aes(x = Predictor, y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  labs(
    title = "Predictors of Short-term Educational Loan Burden (18–30)",
    x = "Predictors",
    y = "Effect on Short-term Burden"
  ) +
  theme_minimal()

ggplotly(plot_short_term_18_30)

# Extract coefficients for Age Group 18–30: Long-term Burden
coef_long_term_18_30 <- as.data.frame(coef(summary(ols_long_term_18_30)))
coef_long_term_18_30$Predictor <- rownames(coef_long_term_18_30)

plot_long_term_18_30 <- ggplot(coef_long_term_18_30, aes(x = Predictor, y = Estimate)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  labs(
    title = "Predictors of Long-term Educational Loan Burden (18–30)",
    x = "Predictors",
    y = "Effect on Long-term Burden"
  ) +
  theme_minimal()

ggplotly(plot_long_term_18_30)
```

# Visualization regression for group 31-65

```{r}
# Extract coefficients for Age Group 31–65: Short-term Burden
coef_short_term_31_65 <- as.data.frame(coef(summary(ols_short_term_31_65)))
coef_short_term_31_65$Predictor <- rownames(coef_short_term_31_65)

plot_short_term_31_65 <- ggplot(coef_short_term_31_65, aes(x = Predictor, y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  labs(
    title = "Predictors of Short-term Educational Loan Burden (31–65)",
    x = "Predictors",
    y = "Effect on Short-term Burden"
  ) +
  theme_minimal()

ggplotly(plot_short_term_31_65)

# Extract coefficients for Age Group 31–65: Long-term Burden
coef_long_term_31_65 <- as.data.frame(coef(summary(ols_long_term_31_65)))
coef_long_term_31_65$Predictor <- rownames(coef_long_term_31_65)

plot_long_term_31_65 <- ggplot(coef_long_term_31_65, aes(x = Predictor, y = Estimate)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  labs(
    title = "Predictors of Long-term Educational Loan Burden (31–65)",
    x = "Predictors",
    y = "Effect on Long-term Burden"
  ) +
  theme_minimal()

ggplotly(plot_long_term_31_65)
```

# 1.2 Do people who have student loans invest in stocks or bonds?

# Add investment indicators

```{r investment indicatos}
menu <- menu %>%
  mutate(
    invests_in_stocks = ifelse(STOCKS > 0, 1, 0),
    invests_in_bonds = ifelse(BOND > 0, 1, 0)
  )
```

# Summary of stock and bond investments by student loan status

```{r}
summary_investments <- menu %>%
  group_by(has_student_loans) %>%
  summarise(
    stock_investment_rate = mean(invests_in_stocks, na.rm = TRUE),
    bond_investment_rate = mean(invests_in_bonds, na.rm = TRUE),
    count = n()
  )

print(summary_investments)
```

# Hypothesis Testing: Stock and Bond Investment by Student Loan Status

```{r}
stocks_t_test <- t.test(
  menu %>% filter(has_student_loans == "Yes") %>% pull(STOCKS),
  menu %>% filter(has_student_loans == "No") %>% pull(STOCKS),
  alternative = "greater",
  var.equal = FALSE
)

bonds_t_test <- t.test(
  menu %>% filter(has_student_loans == "Yes") %>% pull(BOND),
  menu %>% filter(has_student_loans == "No") %>% pull(BOND),
  alternative = "greater",
  var.equal = FALSE
)

print("T-Test Results for Stock Investments")
print(stocks_t_test)

print("T-Test Results for Bond Investments")
print(bonds_t_test)
```

# Subgroup Analysis: Age Groups

```{r}
stock_age_group_analysis <- menu %>%
  group_by(age_group, has_student_loans) %>%
  summarise(
    stock_investment_rate = mean(invests_in_stocks, na.rm = TRUE),
    bond_investment_rate = mean(invests_in_bonds, na.rm = TRUE),
    count = n()
  )

print(stock_age_group_analysis)
```

# Visualizations: Count of Stock Investments by Student Loan Status

```{r, echo=FALSE}
plot_stocks <- ggplot(menu, aes(x = has_student_loans, fill = factor(invests_in_stocks))) +
  geom_bar() +
  labs(
    title = "Count of Stock Investments by Student Loan Status",
    x = "Student Loan Status",
    y = "Count of Stock Investments",
    fill = "Invests in Stocks"
  ) +
  theme_minimal()

ggplotly(plot_stocks)
```

# Visualizations: Count of Bond Investments by Student Loan Status

```{r, echo=FALSE}
plot_bonds <- ggplot(menu, aes(x = has_student_loans, fill = factor(invests_in_bonds))) +
  geom_bar() +
  labs(
    title = "Count of Bond Investments by Student Loan Status",
    x = "Student Loan Status",
    y = "Count of Bond Investments",
    fill = "Invests in Bonds"
  ) +
  theme_minimal()

ggplotly(plot_bonds)
```

# Logistic Regression: Predictors of Stock and Bond Investments

```{r}
logit_stocks <- glm(
  invests_in_stocks ~ HEDN_INST + income_midpoint + NETWORTH,
  data = menu, family = binomial
)

logit_bonds <- glm(
  invests_in_bonds ~ HEDN_INST + income_midpoint + NETWORTH,
  data = menu, family = binomial
)

print("Logit Results for Stock Investments")
summary(logit_stocks)

print("Logit Results for Bond Investments")
summary(logit_bonds)
```

# Predicted Probabilities Visualization

```{r, echo=FALSE}
menu$predicted_prob_stocks <- predict(logit_stocks, newdata = menu, type = "response")
menu$predicted_prob_bonds <- predict(logit_bonds, newdata = menu, type = "response")

plot_predicted_probs <- ggplot(menu, aes(x = age_group, y = predicted_prob_stocks, fill = has_student_loans)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Predicted Probability of Stock Investment by Age Group and Student Loan Status",
    x = "Age Group",
    y = "Predicted Probability",
    fill = "Student Loan Status"
  ) +
  theme_minimal()

ggplotly(plot_predicted_probs)

plot_predicted_probs_bonds <- ggplot(menu, aes(x = age_group, y = predicted_prob_bonds, fill = has_student_loans)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Predicted Probability of Bond Investment by Age Group and Student Loan Status",
    x = "Age Group",
    y = "Predicted Probability",
    fill = "Student Loan Status"
  ) +
  theme_minimal()

ggplotly(plot_predicted_probs_bonds)
```
