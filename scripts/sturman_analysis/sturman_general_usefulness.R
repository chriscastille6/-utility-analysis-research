# Sturman (2000) General Usefulness Analysis
# Attempting to replicate the 291% median reduction finding

# Load standardized utility functions
source("../utilities/sturman_utility_functions.R")

cat("=== STURMAN (2000) GENERAL USEFULNESS ANALYSIS ===\n\n")

library(dplyr)
library(scales)

set.seed(42)  # For reproducibility
n_sims <- 10000

cat("STURMAN (2000) GENERAL USEFULNESS ANALYSIS\n")
cat("==========================================\n")
cat("Reproducing the 291% median reduction finding\n")
cat("Using completely random parameter combinations\n\n")

# Generate completely random parameters (matching Sturman's Table 1)
# Note: n and cost use exponential distribution (skewed)
params <- data.frame(
  # Basic parameters (exponential for n and cost, uniform for others)
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.70),
  sdy = runif(n_sims, 5000, 50000),
  cost = exp(runif(n_sims, log(10), log(1000))),
  
  # Economic adjustment parameters
  discount = runif(n_sims, 0.05, 0.15),
  tax = runif(n_sims, 0.20, 0.40),
  vc = runif(n_sims, 0.10, 0.30),
  
  # Multiple devices
  r_old = runif(n_sims, 0.05, 0.25),
  
  # Top-down hiring deviations
  reject_rate = runif(n_sims, 0.10, 0.40),
  corr_perf_accept = runif(n_sims, -0.3, -0.1),
  
  # Probationary period
  prob_cutoff = runif(n_sims, -1.5, -0.5),
  
  # Employee flows
  turnover_rate = runif(n_sims, 0.05, 0.25),
  perf_turn_corr = runif(n_sims, -0.3, -0.1)
)

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

cat("Calculating utility estimates for", n_sims, "scenarios...\n")

# Basic utility (no adjustments) - Using standardized function
params$utility_basic <- with(params, {
  calculate_basic_utility(n, t, sr, r, sdy, cost)
})

# Individual adjustments (for usefulness ranking)
# 1. Economic adjustments
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

# 2. Multiple devices
params$utility_multiple <- with(params, {
  incremental_r <- r - r_old
  n * t * ux(sr) * incremental_r * sdy - (n/sr) * cost
})

# 3. Top-down hiring deviations
params$utility_topdown <- with(params, {
  # Approximate effect of hiring deviations
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  n * t * z_adj * r * sdy - (n/sr) * cost
})

# 4. Probationary period
params$utility_probation <- with(params, {
  # Proportion retained after probation
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  effective_n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# 5. Employee flows
params$utility_flows <- with(params, {
  # Simplified employee flow adjustment
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  (avg_workforce * t * ux(sr) * r * sdy * performance_effect) - 
  (n/sr) * cost - (n * turnover_rate * t * cost * 0.3)
})

# CUMULATIVE ADJUSTMENTS (Sturman's sequential approach)
# Step 1: Economic (largest individual effect)
params$utility_step1 <- params$utility_economic

# Step 2: Economic + Multiple devices
params$utility_step2 <- with(params, {
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

# Step 3: + Top-down hiring
params$utility_step3 <- with(params, {
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
params$utility_step4 <- with(params, {
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

# Step 5: + Employee flows (all adjustments)
params$utility_step5 <- with(params, {
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

# Individual adjustment effects
pct_economic <- calc_pct_change(params$utility_basic, params$utility_economic)
pct_multiple <- calc_pct_change(params$utility_basic, params$utility_multiple)
pct_topdown <- calc_pct_change(params$utility_basic, params$utility_topdown)
pct_probation <- calc_pct_change(params$utility_basic, params$utility_probation)
pct_flows <- calc_pct_change(params$utility_basic, params$utility_flows)

# Cumulative effects
pct_step1 <- calc_pct_change(params$utility_basic, params$utility_step1)
pct_step2 <- calc_pct_change(params$utility_basic, params$utility_step2)
pct_step3 <- calc_pct_change(params$utility_basic, params$utility_step3)
pct_step4 <- calc_pct_change(params$utility_basic, params$utility_step4)
pct_step5 <- calc_pct_change(params$utility_basic, params$utility_step5)

# Results
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("INDIVIDUAL ADJUSTMENT EFFECTS (for usefulness ranking)\n")
cat(paste(rep("=", 60), collapse=""), "\n")

individual_results <- data.frame(
  Adjustment = c("Economic", "Multiple Devices", "Top-Down Hiring", "Probationary Period", "Employee Flows"),
  Median_Reduction = c(
    median(pct_economic, na.rm = TRUE),
    median(pct_multiple, na.rm = TRUE),
    median(pct_topdown, na.rm = TRUE),
    median(pct_probation, na.rm = TRUE),
    median(pct_flows, na.rm = TRUE)
  ),
  Mean_Reduction = c(
    mean(pct_economic, na.rm = TRUE),
    mean(pct_multiple, na.rm = TRUE),
    mean(pct_topdown, na.rm = TRUE),
    mean(pct_probation, na.rm = TRUE),
    mean(pct_flows, na.rm = TRUE)
  )
) %>%
  arrange(desc(Median_Reduction)) %>%
  mutate(
    Median_Reduction = round(Median_Reduction, 1),
    Mean_Reduction = round(Mean_Reduction, 1)
  )

print(individual_results)

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("CUMULATIVE SEQUENTIAL EFFECTS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cumulative_results <- data.frame(
  Step = c("1. Economic Only", "2. + Multiple Devices", "3. + Top-Down Hiring", 
           "4. + Probationary Period", "5. + Employee Flows (ALL)"),
  Median_Reduction = c(
    median(pct_step1, na.rm = TRUE),
    median(pct_step2, na.rm = TRUE),
    median(pct_step3, na.rm = TRUE),
    median(pct_step4, na.rm = TRUE),
    median(pct_step5, na.rm = TRUE)
  ),
  Mean_Reduction = c(
    mean(pct_step1, na.rm = TRUE),
    mean(pct_step2, na.rm = TRUE),
    mean(pct_step3, na.rm = TRUE),
    mean(pct_step4, na.rm = TRUE),
    mean(pct_step5, na.rm = TRUE)
  )
) %>%
  mutate(
    Median_Reduction = round(Median_Reduction, 1),
    Mean_Reduction = round(Mean_Reduction, 1)
  )

print(cumulative_results)

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("FINAL RESULTS: ALL FIVE ADJUSTMENTS COMBINED\n")
cat(paste(rep("=", 60), collapse=""), "\n")

final_median <- median(pct_step5, na.rm = TRUE)
final_mean <- mean(pct_step5, na.rm = TRUE)
negative_pct <- sum(params$utility_step5 < 0, na.rm = TRUE) / nrow(params) * 100

cat("Median reduction:", round(final_median, 1), "%\n")
cat("Mean reduction:", round(final_mean, 1), "%\n")
cat("Negative cases:", round(negative_pct, 1), "%\n")
cat("Minimum reduction:", round(min(pct_step5, na.rm = TRUE), 1), "%\n")
cat("Maximum reduction:", round(max(pct_step5, na.rm = TRUE), 1), "%\n")

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("COMPARISON TO STURMAN'S TARGETS\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("STURMAN'S GENERAL USEFULNESS ANALYSIS:\n")
cat("- Median reduction: 291%\n")
cat("- Mean reduction: 298%\n")
cat("- Negative cases: 16%\n")
cat("- Minimum reduction: 71%\n\n")

cat("OUR RESULTS:\n")
cat("- Median reduction:", round(final_median, 1), "%\n")
cat("- Mean reduction:", round(final_mean, 1), "%\n")
cat("- Negative cases:", round(negative_pct, 1), "%\n")
cat("- Minimum reduction:", round(min(pct_step5, na.rm = TRUE), 1), "%\n\n")

# Assessment
diff_median <- abs(final_median - 291)
diff_mean <- abs(final_mean - 298)
diff_negative <- abs(negative_pct - 16)

cat("ASSESSMENT:\n")
if(diff_median < 50 && diff_mean < 50 && diff_negative < 5) {
  cat("✅ EXCELLENT MATCH to Sturman's general usefulness analysis!\n")
} else if(diff_median < 100 && diff_mean < 100 && diff_negative < 10) {
  cat("✅ GOOD MATCH to Sturman's general usefulness analysis!\n")
} else {
  cat("⚠️  Partial match. May need parameter adjustment.\n")
  cat("   Median difference:", round(diff_median, 1), "percentage points\n")
  cat("   Mean difference:", round(diff_mean, 1), "percentage points\n")
  cat("   Negative cases difference:", round(diff_negative, 1), "percentage points\n")
} 