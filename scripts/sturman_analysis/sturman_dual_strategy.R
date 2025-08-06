# Sturman (2000) Dual Strategy Implementation
# Strategy A: Incremental effects (compare each step to previous step)
# Strategy B: Cumulative effects (compare each step to basic utility)

library(dplyr)
library(pdftools)

set.seed(42)
n_sims <- 10000

cat("STURMAN (2000) DUAL STRATEGY USEFULNESS ANALYSIS\n")
cat("================================================\n")
cat("Testing both interpretations of the methodology\n\n")

# Try to extract more parameter information from the PDF
cat("Attempting to extract parameter ranges from text...\n")

# Based on text analysis, here are the ranges I can extract:
# - n: 1 to 1100 (exponential distribution)
# - cost: exponential distribution (range not fully specified)
# - r_old (L&W analysis): 0.05 to 0.38
# - All others: uniform distributions

# Generate parameters based on available text information
params <- data.frame(
  # Basic parameters - using best guesses from text
  n = round(exp(runif(n_sims, log(1), log(1100)))),  # Confirmed: 1-1100, exponential
  t = runif(n_sims, 1, 10),                          # Estimated from typical ranges
  sr = runif(n_sims, 0.05, 1.0),                     # Estimated from typical ranges
  r = runif(n_sims, 0.10, 0.70),                     # Estimated from typical ranges
  sdy = runif(n_sims, 5000, 50000),                  # Estimated from typical ranges
  cost = exp(runif(n_sims, log(10), log(1000))),     # Exponential, estimated range
  
  # Economic adjustment parameters - estimated ranges
  discount = runif(n_sims, 0.05, 0.15),
  tax = runif(n_sims, 0.20, 0.40),
  vc = runif(n_sims, 0.10, 0.30),
  
  # Multiple devices - using L&W range as guide
  r_old = runif(n_sims, 0.05, 0.38),  # Confirmed from L&W analysis
  
  # Other adjustments - estimated ranges
  reject_rate = runif(n_sims, 0.10, 0.40),
  corr_perf_accept = runif(n_sims, -0.3, -0.1),
  prob_cutoff = runif(n_sims, -1.5, -0.5),
  turnover_rate = runif(n_sims, 0.05, 0.25),
  perf_turn_corr = runif(n_sims, -0.3, -0.1)
)

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

cat("Calculating utility estimates for", n_sims, "scenarios...\n")

# Basic utility (baseline for all comparisons)
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Individual adjustments
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

params$utility_multiple <- with(params, {
  incremental_r <- r - r_old
  n * t * ux(sr) * incremental_r * sdy - (n/sr) * cost
})

params$utility_topdown <- with(params, {
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  n * t * z_adj * r * sdy - (n/sr) * cost
})

params$utility_probation <- with(params, {
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  effective_n * t * ux(sr) * r * sdy - (n/sr) * cost
})

params$utility_flows <- with(params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  (avg_workforce * t * ux(sr) * r * sdy * performance_effect) - 
  (n/sr) * cost - (n * turnover_rate * t * cost * 0.3)
})

# Sequential combinations (following Sturman's ranking)
# Step 1: Economic (winner from individual comparison)
params$step1_economic <- params$utility_economic

# Step 2: Economic + Multiple devices
params$step2_econ_mult <- with(params, {
  incremental_r <- r - r_old
  annual_benefit <- n * incremental_r * ux(sr) * sdy
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

# Step 3: Economic + Multiple + Top-down
params$step3_plus_topdown <- with(params, {
  incremental_r <- r - r_old
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  
  annual_benefit <- n * incremental_r * z_adj * sdy
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

# Step 4: + Probationary period
params$step4_plus_probation <- with(params, {
  incremental_r <- r - r_old
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  effective_n <- n * prob_retained
  annual_benefit <- effective_n * incremental_r * z_adj * sdy
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

# Step 5: All adjustments
params$step5_all <- with(params, {
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
calc_pct_change <- function(basic, adjusted) {
  pct_change <- 100 * (basic - adjusted) / abs(basic)
  pct_change[is.finite(pct_change)]
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("STRATEGY A: INCREMENTAL EFFECTS (Each step vs. previous step)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Strategy A: Incremental effects
individual_effects_A <- data.frame(
  Adjustment = c("Economic", "Multiple Devices", "Top-Down Hiring", "Probationary Period", "Employee Flows"),
  Median_Reduction = c(
    median(calc_pct_change(params$utility_basic, params$utility_economic), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$utility_multiple), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$utility_topdown), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$utility_probation), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$utility_flows), na.rm = TRUE)
  )
) %>%
  arrange(desc(Median_Reduction)) %>%
  mutate(Median_Reduction = round(Median_Reduction, 1))

cat("INDIVIDUAL EFFECTS (Column 1 of Table 2):\n")
print(individual_effects_A)

# Incremental effects beyond economic
incremental_A <- data.frame(
  Addition = c("Multiple Devices", "Top-Down Hiring", "Probationary Period", "Employee Flows"),
  Beyond_Economic = c(
    median(calc_pct_change(params$step1_economic, params$step2_econ_mult), na.rm = TRUE),
    median(calc_pct_change(params$step2_econ_mult, params$step3_plus_topdown), na.rm = TRUE),
    median(calc_pct_change(params$step3_plus_topdown, params$step4_plus_probation), na.rm = TRUE),
    median(calc_pct_change(params$step4_plus_probation, params$step5_all), na.rm = TRUE)
  )
) %>%
  arrange(desc(Beyond_Economic)) %>%
  mutate(Beyond_Economic = round(Beyond_Economic, 1))

cat("\nINCREMENTAL EFFECTS (Column 2+ of Table 2):\n")
print(incremental_A)

final_A <- median(calc_pct_change(params$utility_basic, params$step5_all), na.rm = TRUE)
cat("\nFINAL CUMULATIVE EFFECT (Strategy A):", round(final_A, 1), "%\n")

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("STRATEGY B: CUMULATIVE EFFECTS (Each step vs. basic utility)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Strategy B: All comparisons to basic utility
cumulative_B <- data.frame(
  Step = c("1. Economic Only", "2. + Multiple Devices", "3. + Top-Down Hiring", 
           "4. + Probationary Period", "5. + Employee Flows (ALL)"),
  Median_Reduction = c(
    median(calc_pct_change(params$utility_basic, params$step1_economic), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$step2_econ_mult), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$step3_plus_topdown), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$step4_plus_probation), na.rm = TRUE),
    median(calc_pct_change(params$utility_basic, params$step5_all), na.rm = TRUE)
  )
) %>%
  mutate(Median_Reduction = round(Median_Reduction, 1))

print(cumulative_B)

final_B <- median(calc_pct_change(params$utility_basic, params$step5_all), na.rm = TRUE)
cat("\nFINAL CUMULATIVE EFFECT (Strategy B):", round(final_B, 1), "%\n")

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("COMPARISON TO STURMAN'S 291% TARGET\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("Strategy A Final Result:", round(final_A, 1), "%\n")
cat("Strategy B Final Result:", round(final_B, 1), "%\n")
cat("Sturman's Target: 291%\n")
cat("Difference A:", round(abs(final_A - 291), 1), "percentage points\n")
cat("Difference B:", round(abs(final_B - 291), 1), "percentage points\n")

# Additional diagnostics
negative_pct <- sum(params$step5_all < 0, na.rm = TRUE) / nrow(params) * 100
cat("\nAdditional Diagnostics:\n")
cat("Negative cases:", round(negative_pct, 1), "% (Target: 16%)\n")
cat("Min reduction:", round(min(calc_pct_change(params$utility_basic, params$step5_all), na.rm = TRUE), 1), "% (Target: 71%)\n")

if(abs(final_A - 291) < abs(final_B - 291)) {
  cat("\n✅ Strategy A appears closer to Sturman's methodology\n")
} else {
  cat("\n✅ Strategy B appears closer to Sturman's methodology\n")
}

# Try to identify what might be missing
cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARAMETER DIAGNOSTICS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("Parameter summary statistics:\n")
param_summary <- params %>%
  select(n, t, sr, r, sdy, cost, discount, tax, vc, r_old) %>%
  summarise_all(list(min = min, max = max, median = median)) %>%
  tidyr::pivot_longer(everything()) %>%
  tidyr::separate(name, into = c("param", "stat"), sep = "_") %>%
  tidyr::pivot_wider(names_from = stat, values_from = value)

print(param_summary)

cat("\nNOTE: Some parameter ranges may need adjustment to match Sturman's Table 1\n")
cat("Consider testing different ranges if results don't match targets\n") 