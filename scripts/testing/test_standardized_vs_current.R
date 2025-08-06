# ============================================================================
# TEST: STANDARDIZED VS CURRENT INCONSISTENT IMPLEMENTATIONS
# ============================================================================
# This script compares the new standardized utility functions with the current
# inconsistent implementations to demonstrate the impact of standardization.

source("../utilities/sturman_utility_functions.R")

cat("=== TESTING STANDARDIZED VS CURRENT IMPLEMENTATIONS ===\n\n")

# Test parameters
set.seed(42)
n_test <- 1000

test_params <- data.frame(
  n = round(exp(runif(n_test, log(1), log(1100)))),
  t = runif(n_test, 1, 10),
  sr = runif(n_test, 0.05, 1.0),
  r = runif(n_test, 0.10, 0.77),
  sdy = runif(n_test, 4000, 40000),
  cost = exp(runif(n_test, log(1), log(1100))),
  discount = runif(n_test, 0.01, 0.11),
  tax = runif(n_test, 0.30, 0.63),
  vc = runif(n_test, -0.35, -0.02),
  r_old = runif(n_test, 0.05, 0.38),
  reject_rate = runif(n_test, 0.20, 0.70),
  corr_perf_accept = runif(n_test, -0.50, 0.00),
  prob_cutoff = runif(n_test, -2.0, 0.0),
  turnover_rate = runif(n_test, 0.00, 0.33),
  perf_turn_corr = runif(n_test, -0.50, 0.00)
)

cat("Generated", n_test, "test parameter combinations\n\n")

# STANDARDIZED APPROACH
cat("=== STANDARDIZED APPROACH ===\n")
standardized_basic <- with(test_params, calculate_basic_utility(n, t, sr, r, sdy, cost))
standardized_economic <- with(test_params, calculate_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc))

std_reduction <- median(100 * (standardized_basic - standardized_economic) / abs(standardized_basic), na.rm = TRUE)
cat("Standardized Economic Reduction:", round(std_reduction, 1), "%\n")

# CURRENT FORMULA-BASED APPROACH
cat("\n=== CURRENT FORMULA-BASED APPROACH ===\n")
current_economic <- with(test_params, {
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * abs(vc)  # Note: abs() difference
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Formula-based present value
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
})

curr_reduction <- median(100 * (standardized_basic - current_economic) / abs(standardized_basic), na.rm = TRUE)
cat("Current Formula-Based Reduction:", round(curr_reduction, 1), "%\n")

# COMPARISON
cat("\n=== COMPARISON ===\n")
cat("Difference between approaches:", round(abs(std_reduction - curr_reduction), 1), "pp\n")

if (abs(std_reduction - curr_reduction) > 5) {
  cat("🚨 SIGNIFICANT DIFFERENCE: Standardization will change results substantially!\n")
} else {
  cat("✅ MINIMAL DIFFERENCE: Standardization will have limited impact.\n")
}

cat("\n=== RECOMMENDATIONS ===\n")
cat("1. Use standardized library for consistency\n")
cat("2. Update all scripts to use source('sturman_utility_functions.R')\n")
cat("3. Re-run analyses with standardized approach\n")
