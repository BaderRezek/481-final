##################################################
#                DATA EXPLORATION
##################################################

# Check for missing values
colSums(is.na(data))
# AGE SEX BMI BP S1 S2 S3 S4 S5 S6 Target
# 0   0   0   0  0  0  0  0  0  0   0

# Summary statistics
summary(data)
# AGE       || SEX       || BMI       || BP         || S1
# Min. :19  || Min.:1    || Min.:18   || Min.: 62   || Min.: 97.0
# 1Q  :38.2 || 1Q :1     || 1Q :23.2  || 1Q : 84    || 1Q :164.2
# Median:50 || Median:1  || Median:25.7|| Median: 93 || Median:186.0
# Mean :48.5|| Mean :1.47|| Mean :26.38|| Mean : 94.6|| Mean :189.1
# 3Q  :59   || 3Q :2     || 3Q :29.27 || 3Q :105    || 3Q :209.8
# Max. :79  || Max.:2    || Max.:42.2 || Max.:133   || Max.:301.0

# S2        || S3       || S4       || S5       || S6
# Min.: 41.6|| Min.:22  || Min.:2   || Min.:3.26|| Min.: 58.0
# 1Q : 96.0 || 1Q :40.25|| 1Q :3    || 1Q :4.28|| 1Q : 83.25
# Median:113|| Median:48|| Median:4 || Median:4.62|| Median: 91
# Mean :115.4|| Mean :49.8|| Mean :4.07|| Mean :4.64|| Mean : 91.3
# 3Q :134.5 || 3Q :57.75|| 3Q :5    || 3Q :5.00|| 3Q : 98
# Max.:242.4|| Max.:99  || Max.:9.09|| Max.:6.11|| Max.:124

# Target
# Min.: 25.0
# 1Q : 87.0
# Median:140.5
# Mean :152.1
# 3Q :211.5
# Max.:346.0


##################################################
#       STANDARD DEVIATION ANALYSIS
##################################################

Std_Dev <- sapply(data[, sapply(data, is.numeric)], sd)
Std_Dev

barplot(
  Std_Dev,
  main = "Standard Deviation of Numeric Variables",
  ylab = "Standard Deviation",
  col = "#99EDB8FF",
  ylim = c(0, 80),
  las = 2
)

# Observations:
# - Lowest variability: BMI, S4, S5
# - Highest variability: S1 (cholesterol), S2 (LDL)


##################################################
#       FACTOR CONVERSION
##################################################

data$SEX <- factor(data$SEX, levels = c(1, 2), labels = c("Male", "Female"))


##################################################
#       EXPLORING PREDICTORS
##################################################

## Age
hist(
  data$AGE,
  main = "Histogram of Age",
  xlab = "Ages",
  ylim = c(0, 80),
  col = "#99EDB8FF"
)
# Looks relatively normal

## Sex
barplot(
  table(data$SEX),
  main = "Counts of Sex",
  ylim = c(0, 250),
  col = "#99EDB8FF"
)
# More males than females

## BMI
hist(
  data$BMI,
  main = "Histogram of BMI",
  xlab = "BMI",
  ylim = c(0, 100),
  col = "#99EDB8FF"
)
# Right-skewed → possible transformation needed

## BP
hist(
  data$BP,
  main = "Histogram of BP",
  xlab = "BP",
  ylim = c(0, 140),
  col = "#99EDB8FF"
)
# Right-skewed → possible transformation needed

## S1 - Serum total cholesterol
hist(
  data$S1,
  main = "Histogram of Serum total cholesterol (S1)",
  xlab = "S1",
  ylim = c(0, 120),
  col = "#99EDB8FF"
)
# Normal distribution

## S2 - LDL
hist(
  data$S2,
  main = "Histogram of Low-density lipoproteins (LDL) (S2)",
  xlab = "S2",
  ylim = c(0, 140),
  col = "#99EDB8FF"
)
# Right-skewed → possible transformation needed

## S3 - HDL
hist(
  data$S3,
  main = "Histogram of High-density lipoproteins (HDL) (S3)",
  xlab = "S3",
  ylim = c(0, 150),
  col = "#99EDB8FF"
)
# Right-skewed

## S4 - Total cholesterol / HDL ratio
hist(
  data$S4,
  main = "Histogram of Total cholesterol / HDL ratio (S4)",
  xlab = "S4",
  ylim = c(0, 160),
  col = "#99EDB8FF"
)
# Extremely right-skewed

## S5 - Serum triglyceride
hist(
  data$S5,
  main = "Histogram of Serum triglyceride level (S5)",
  xlab = "S5",
  ylim = c(0, 150),
  col = "#99EDB8FF"
)
# Relatively normal

## S6 - Blood sugar
hist(
  data$S6,
  main = "Histogram of Blood sugar level (S6)",
  xlab = "S6",
  ylim = c(0, 100),
  col = "#99EDB8FF"
)
# Relatively normal


##################################################
#       MODEL SELECTION & FITTING
##################################################

## Preliminary Hypothesis Testing
main_model <- lm(Target ~ ., data = data)
residuals <- main_model$residuals

# Normality
qqnorm(residuals)
qqline(residuals, col = "lightpink", lwd = 2)
shapiro.test(residuals)
# W = 0.99706, p-value = 0.6162

# Equal Variance
plot(
  main_model$fitted.values, residuals,
  main = "Residuals vs Fitted Values",
  xlab = "Fitted Values", ylab = "Residuals",
  pch = 19, col = "#99EDB8FF"
)
abline(h = 0, col = "lightpink", lwd = 2)

# Independence
plot(
  residuals[-length(residuals)], residuals[-1],
  main = "Residuals vs Lagged Residuals",
  xlab = expression(e[i-1]), ylab = expression(e[i]),
  pch = 19, col = "#99EDB8FF"
)


##################################################
#       MULTICOLLINEARITY
##################################################

library(car)
vif(main_model)
# VIF > 10 → Possible multicollinearity in S1, S2, S3, S5

##################################################
#       VARIABLE SELECTION METHODS
##################################################

## Backward Selection (AIC)
full_model <- lm(Target ~ ., data = data)
backward_model <- step(full_model, direction = "backward", trace = TRUE)
summary(backward_model)

## Backward Selection (P-value)
pval_backward_model <- lm(Target ~ ., data = data)

repeat {
    model_summary <- summary(pval_backward_model)
    pvals <- coef(model_summary)[, "Pr(>|t|)"][-1]  # Exclude intercept
    max_pval <- max(pvals, na.rm = TRUE)

    if (max_pval > 0.10) {
        var_to_remove <- names(which.max(pvals))
        current_vars <- all.vars(formula(pval_backward_model))[-1]
        updated_vars <- setdiff(current_vars, var_to_remove)
        updated_formula <- as.formula(paste("Target ~", paste(updated_vars, collapse = " + ")))
        pval_backward_model <- lm(updated_formula, data = data)
    } else {
        break
    }
}
summary(pval_backward_model)


## Forward Selection (AIC)
null_model <- lm(Target ~ 1, data = data)
forward_model <- step(
    null_model,
    scope = list(lower = null_model, upper = full_model),
    direction = "forward",
    trace = TRUE
)
summary(forward_model)


## Forward Selection (P-value)
null_model <- lm(Target ~ 1, data = data)
scope <- setdiff(names(data), "Target")
selected <- c()

repeat {
    pvals <- c()
    for (var in setdiff(scope, selected)) {
        temp_formula <- as.formula(paste("Target ~", paste(c(selected, var), collapse = " + ")))
        temp_model <- lm(temp_formula, data = data)
        p <- summary(temp_model)$coefficients[var, "Pr(>|t|)"]
        pvals[var] <- p
    }
    if (length(pvals) == 0 || min(pvals, na.rm = TRUE) > 0.10) break
    selected <- c(selected, names(which.min(pvals)))
}

final_formula <- as.formula(paste("Target ~", paste(selected, collapse = " + ")))
pval_forward_model <- lm(final_formula, data = data)
summary(pval_forward_model)


## Stepwise Selection
stepwise_model <- step(full_model, direction = "both", trace = TRUE)
summary(stepwise_model)


##################################################
#       MODEL COMPARISON (AIC)
##################################################

AIC(full_model, backward_model, forward_model, stepwise_model)
# Lower AIC → better model


##################################################
#       FINAL MODEL & ASSUMPTION CHECKS
##################################################

final_model <- lm(Target ~ SEX + BMI + BP + S1 + S2 + S5, data = data)
summary(final_model)$coefficients

# 1. Linearity
par(mfrow = c(2, 3))
for (var in c("SEX", "BMI", "BP", "S1", "S2", "S5")) {
    plot(
        data[[var]], residuals(final_model),
        main = paste("Residuals vs", var),
        xlab = var, ylab = "Residuals",
        pch = 19, col = "#99EDB8FF"
    )
    abline(h = 0, col = "lightpink", lty = 2)
}
par(mfrow = c(1, 1))

# 2. Independence
plot(
    residuals(final_model)[-length(residuals(final_model))],
    residuals(final_model)[-1],
    main = "Residuals vs Lagged Residuals",
    xlab = expression(e[i-1]), ylab = expression(e[i]),
    pch = 19, col = "#99EDB8FF"
)
abline(h = 0, v = 0, col = "lightpink", lty = 2)

# 3. Normality
qqnorm(
    residuals(final_model),
    main = "Q-Q Plot of Residuals",
    col = "#99EDB8FF", pch = 19
)
qqline(residuals(final_model), col = "lightpink", lwd = 2)
shapiro.test(residuals(final_model))

# 4. Homoscedasticity
plot(
    final_model$fitted.values, residuals(final_model),
    main = "Residuals vs Fitted Values",
    xlab = "Fitted Values", ylab = "Residuals",
    pch = 19, col = "#99EDB8FF"
)
abline(h = 0, col = "lightpink", lwd = 2)
title(sub = "Checking for constant variance")