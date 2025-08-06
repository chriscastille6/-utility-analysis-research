# STURMAN'S TRUE USEFULNESS ANALYSIS: MULTIPLE REGRESSION APPROACH
# ================================================================
# Based on Darlington (1968): "Multiple Regression in Psychological Research and Practice"
# 
# Darlington defines "usefulness" of predictor Xj as:
# "the amount R² would drop if Xj were removed from the regression equation 
# and the weights of the remaining predictor variables were then recalculated"

library(dplyr)

set.seed(42)
n_sims <- 10000

cat("STURMAN'S TRUE USEFULNESS ANALYSIS\n")
cat("==================================\n")
cat("Using Darlington (1968) Multiple Regression Approach\n\n")

cat("DARLINGTON'S DEFINITION OF USEFULNESS:\n")
cat("The 'usefulness' of predictor variable Xj is the amount R² would drop\n")
cat("if Xj were removed from the regression equation and the remaining\n")
cat("predictor variables were then recalculated.\n\n")

cat("STURMAN'S APPLICATION:\n")
cat("Apply this to the 10,000 Monte Carlo scenarios to determine the\n")
cat("relative importance of the 5 utility adjustments.\n\n")

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Generate parameters using Sturman's Table 1 ranges
params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.70),
  sdy = runif(n_sims, 5000, 50000),
  cost = exp(runif(n_sims, log(10), log(1000))),
  
  # Economic parameters
  discount = runif(n_sims, 0.05, 0.12),
  tax = runif(n_sims, 0.25, 0.35),
  vc = runif(n_sims, 0.15, 0.25),
  
  # Multiple devices
  r_old = runif(n_sims, 0.10, 0.30),
  
  # Other adjustments
  reject_rate = runif(n_sims, 0.15, 0.35),
  corr_perf_accept = runif(n_sims, -0.25, -0.15),
  prob_cutoff = runif(n_sims, -1.2, -0.8),
  turnover_rate = runif(n_sims, 0.08, 0.20),
  perf_turn_corr = runif(n_sims, -0.25, -0.15)
)

cat("STEP 1: CALCULATE ALL UTILITY VALUES\n")
cat("====================================\n")

# Basic utility (no adjustments)
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Calculate percentage reductions for each adjustment
cat("Calculating percentage reductions for each adjustment...\n")

# Economic adjustments
params$utility_economic <- with(params, {
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
})

# Multiple devices
params$utility_multiple <- with(params, {
  incremental_r <- r - r_old
  n * t * ux(sr) * incremental_r * sdy - (n/sr) * cost
})

# Top-down hiring
params$utility_topdown <- with(params, {
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  n * t * z_adj * r * sdy - (n/sr) * cost
})

# Probationary period
params$utility_probation <- with(params, {
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  effective_n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Employee flows
params$utility_flows <- with(params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  (avg_workforce * t * ux(sr) * r * sdy * performance_effect) - 
  (n/sr) * cost - (n * turnover_rate * t * cost * 0.3)
})

# All adjustments combined
params$utility_all <- with(params, {
  incremental_r <- r - r_old
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  effective_n <- n * prob_retained
  avg_workforce <- effective_n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  annual_benefit <- avg_workforce * incremental_r * z_adj * sdy * performance_effect
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  initial_costs <- (n/sr) * cost
  replacement_costs <- n * turnover_rate * t * cost * 0.3
  
  after_tax_pv - initial_costs - replacement_costs
})

# Calculate percentage changes
params$pct_economic <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
params$pct_multiple <- 100 * (params$utility_basic - params$utility_multiple) / abs(params$utility_basic)
params$pct_topdown <- 100 * (params$utility_basic - params$utility_topdown) / abs(params$utility_basic)
params$pct_probation <- 100 * (params$utility_basic - params$utility_probation) / abs(params$utility_basic)
params$pct_flows <- 100 * (params$utility_basic - params$utility_flows) / abs(params$utility_basic)
params$pct_all <- 100 * (params$utility_basic - params$utility_all) / abs(params$utility_basic)

cat("STEP 2: MULTIPLE REGRESSION USEFULNESS ANALYSIS\n")
cat("===============================================\n")
cat("Using the percentage reductions as predictors of the final combined effect\n")
cat("to determine the 'usefulness' (unique contribution) of each adjustment.\n\n")

# Create the regression dataset
# Y = final percentage reduction (all adjustments combined)
# X1 = economic percentage reduction
# X2 = multiple devices percentage reduction  
# X3 = top-down percentage reduction
# X4 = probationary percentage reduction
# X5 = employee flows percentage reduction

regression_data <- data.frame(
  y = params$pct_all,
  x1 = params$pct_economic,
  x2 = params$pct_multiple,
  x3 = params$pct_topdown,
  x4 = params$pct_probation,
  x5 = params$pct_flows
)

# Remove any rows with missing values
regression_data <- regression_data[complete.cases(regression_data), ]

cat("Regression data prepared with", nrow(regression_data), "complete cases\n")
cat("Dependent variable: Final percentage reduction (all adjustments)\n")
cat("Predictors: Individual adjustment percentage reductions\n\n")

# Fit the full multiple regression model
full_model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = regression_data)

cat("FULL MODEL RESULTS:\n")
cat("==================\n")
print(summary(full_model))
cat("\n")

# Calculate usefulness (unique contribution) of each predictor
# This is the drop in R² when each predictor is removed

r_squared_full <- summary(full_model)$r.squared
cat("Full model R² =", round(r_squared_full, 4), "\n\n")

# Calculate R² for models with each predictor removed
cat("CALCULATING USEFULNESS (R² DROP) FOR EACH ADJUSTMENT:\n")
cat("====================================================\n")

# Remove economic (x1)
model_no_x1 <- lm(y ~ x2 + x3 + x4 + x5, data = regression_data)
r2_no_x1 <- summary(model_no_x1)$r.squared
usefulness_economic <- r_squared_full - r2_no_x1

# Remove multiple (x2)
model_no_x2 <- lm(y ~ x1 + x3 + x4 + x5, data = regression_data)
r2_no_x2 <- summary(model_no_x2)$r.squared
usefulness_multiple <- r_squared_full - r2_no_x2

# Remove top-down (x3)
model_no_x3 <- lm(y ~ x1 + x2 + x4 + x5, data = regression_data)
r2_no_x3 <- summary(model_no_x3)$r.squared
usefulness_topdown <- r_squared_full - r2_no_x3

# Remove probationary (x4)
model_no_x4 <- lm(y ~ x1 + x2 + x3 + x5, data = regression_data)
r2_no_x4 <- summary(model_no_x4)$r.squared
usefulness_probation <- r_squared_full - r2_no_x4

# Remove flows (x5)
model_no_x5 <- lm(y ~ x1 + x2 + x3 + x4, data = regression_data)
r2_no_x5 <- summary(model_no_x5)$r.squared
usefulness_flows <- r_squared_full - r2_no_x5

# Create usefulness summary
usefulness_results <- data.frame(
  Adjustment = c("Economic", "Multiple", "TopDown", "Probation", "Flows"),
  Usefulness = c(usefulness_economic, usefulness_multiple, usefulness_topdown, 
                usefulness_probation, usefulness_flows),
  Percent_of_Total = c(usefulness_economic, usefulness_multiple, usefulness_topdown, 
                      usefulness_probation, usefulness_flows) / r_squared_full * 100
) %>%
  arrange(desc(Usefulness))

cat("USEFULNESS ANALYSIS RESULTS:\n")
cat("============================\n")
print(usefulness_results)
cat("\n")

cat("INTERPRETATION:\n")
cat("==============\n")
cat("Each 'Usefulness' value represents the unique contribution of that adjustment\n")
cat("to explaining the variance in the final combined utility reduction.\n")
cat("This is Darlington's definition: the drop in R² when that predictor is removed.\n\n")

cat("COMPARISON WITH STURMAN'S SEQUENTIAL APPROACH:\n")
cat("=============================================\n")

# For comparison, show the median individual effects
individual_effects <- data.frame(
  Adjustment = c("Economic", "Multiple", "TopDown", "Probation", "Flows"),
  Median_Individual_Effect = c(
    median(params$pct_economic, na.rm = TRUE),
    median(params$pct_multiple, na.rm = TRUE),
    median(params$pct_topdown, na.rm = TRUE),
    median(params$pct_probation, na.rm = TRUE),
    median(params$pct_flows, na.rm = TRUE)
  )
) %>%
  arrange(desc(Median_Individual_Effect))

cat("INDIVIDUAL EFFECTS (Median % Reduction):\n")
print(individual_effects)
cat("\n")

cat("FINAL COMBINED EFFECT:\n")
final_median <- median(params$pct_all, na.rm = TRUE)
cat("Median reduction across all scenarios:", round(final_median, 1), "%\n")
cat("Sturman's target: 291%\n")
cat("Our gap:", round(abs(final_median - 291), 1), "percentage points\n\n")

cat("KEY INSIGHT:\n")
cat("===========\n")
cat("The multiple regression usefulness analysis shows the UNIQUE contribution\n")
cat("of each adjustment, accounting for correlations between adjustments.\n")
cat("This is different from the sequential selection approach we tried earlier.\n")
cat("It directly answers: 'How much does each adjustment uniquely contribute\n")
cat("to the final combined effect?'\n") 