# Test Parameter Correlations Impact on Utility Analysis
# Investigating if realistic organizational parameter correlations explain the 291% discrepancy

# Load standardized functions
source("../utilities/sturman_utility_functions.R")

cat("=== TESTING REALISTIC PARAMETER CORRELATIONS ===\n\n")

library(mvtnorm)

set.seed(42)
n_sims <- 10000

cat("Sturman (2000) assumed ZERO correlation between parameters.\n")
cat("But he noted: 'If such intercorrelations significantly deviate from 0,\n")
cat("then utility estimates may be notably different.'\n\n")

# Realistic organizational parameter correlations based on organizational theory
create_correlated_parameters <- function(n_sims) {
  
  # Define realistic correlations between organizational parameters
  # Based on organizational behavior and HR research
  
  # Parameters that should be correlated in real organizations:
  # 1. Larger organizations (higher n) often have:
  #    - Higher costs per applicant (more bureaucracy)
  #    - Higher tax rates (corporate vs small business)
  #    - Lower selection ratios (more selective)
  #    - Higher discount rates (more conservative)
  
  # 2. Organizations with higher validity tests often have:
  #    - Higher costs per applicant (better tests cost more)
  #    - Lower selection ratios (can afford to be selective)
  #    - Higher SDy (more sophisticated jobs)
  
  # 3. Economic factors often correlate:
  #    - Higher tax rates with higher variable costs (regulated industries)
  #    - Higher discount rates with higher tax rates (risk-averse organizations)
  
  # Create correlation matrix for key parameters
  # Order: log(n), log(cost), sr, r, log(sdy), discount, tax, vc
  corr_matrix <- matrix(c(
    1.00,  0.40, -0.30,  0.20,  0.35,  0.25,  0.30, -0.20,  # log(n)
    0.40,  1.00, -0.25,  0.35,  0.30,  0.20,  0.25, -0.15,  # log(cost)
   -0.30, -0.25,  1.00, -0.40, -0.25, -0.15, -0.20,  0.10,  # sr
    0.20,  0.35, -0.40,  1.00,  0.30,  0.15,  0.20, -0.10,  # r
    0.35,  0.30, -0.25,  0.30,  1.00,  0.20,  0.25, -0.15,  # log(sdy)
    0.25,  0.20, -0.15,  0.15,  0.20,  1.00,  0.45, -0.25,  # discount
    0.30,  0.25, -0.20,  0.20,  0.25,  0.45,  1.00, -0.30,  # tax
   -0.20, -0.15,  0.10, -0.10, -0.15, -0.25, -0.30,  1.00   # vc
  ), nrow = 8, ncol = 8)
  
  # Generate correlated random variables
  corr_data <- rmvnorm(n_sims, mean = rep(0, 8), sigma = corr_matrix)
  
  # Transform to uniform [0,1] using normal CDF
  uniform_data <- pnorm(corr_data)
  
  # Transform to parameter ranges
  params <- data.frame(
    # Basic parameters
    n = round(exp(uniform_data[,1] * (log(1100) - log(1)) + log(1))),
    t = runif(n_sims, 1, 10),  # Keep tenure independent
    sr = uniform_data[,3] * (1.0 - 0.05) + 0.05,
    r = uniform_data[,4] * (0.77 - 0.10) + 0.10,
    sdy = exp(uniform_data[,5] * (log(40000) - log(4000)) + log(4000)),
    cost = exp(uniform_data[,2] * (log(1100) - log(1)) + log(1)),
    
    # Economic parameters (correlated)
    discount = uniform_data[,6] * (0.11 - 0.01) + 0.01,
    tax = uniform_data[,7] * (0.63 - 0.30) + 0.30,
    vc = uniform_data[,8] * (-0.02 - (-0.35)) + (-0.35),
    
    # Multiple devices (correlated with validity)
    r_old = runif(n_sims, 0.05, 0.38),  # Keep independent for now
    
    # Other parameters (keep independent)
    reject_rate = runif(n_sims, 0.20, 0.70),
    corr_perf_accept = runif(n_sims, -0.50, 0.00),
    prob_cutoff = runif(n_sims, -2.0, 0.0),
    turnover_rate = runif(n_sims, 0.00, 0.33),
    perf_turn_corr = runif(n_sims, -0.50, 0.00)
  )
  
  return(params)
}

cat("Generating", n_sims, "scenarios with realistic parameter correlations...\n")

# Generate correlated parameters
corr_params <- create_correlated_parameters(n_sims)

# Also generate independent parameters for comparison
indep_params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.77),
  sdy = runif(n_sims, 4000, 40000),
  cost = exp(runif(n_sims, log(1), log(1100))),
  discount = runif(n_sims, 0.01, 0.11),
  tax = runif(n_sims, 0.30, 0.63),
  vc = runif(n_sims, -0.35, -0.02),
  r_old = runif(n_sims, 0.05, 0.38),
  reject_rate = runif(n_sims, 0.20, 0.70),
  corr_perf_accept = runif(n_sims, -0.50, 0.00),
  prob_cutoff = runif(n_sims, -2.0, 0.0),
  turnover_rate = runif(n_sims, 0.00, 0.33),
  perf_turn_corr = runif(n_sims, -0.50, 0.00)
)

cat("Calculating utilities with correlated vs independent parameters...\n")

# Calculate utilities for both scenarios
for(dataset_name in c("corr", "indep")) {
  params <- get(paste0(dataset_name, "_params"))
  
  # Basic utility
  params$utility_basic <- with(params, calculate_basic_utility(n, t, sr, r, sdy, cost))
  
  # Economic utility
  params$utility_economic <- with(params, calculate_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc))
  
  # Multiple devices utility
  params$utility_multiple <- with(params, calculate_multiple_devices_utility(n, t, sr, r, r_old, sdy, cost))
  
  # Calculate percentage changes
  params$pct_change_economic <- with(params, 100 * (utility_basic - utility_economic) / abs(utility_basic))
  params$pct_change_multiple <- with(params, 100 * (utility_basic - utility_multiple) / abs(utility_basic))
  
  # Store back
  assign(paste0(dataset_name, "_params"), params)
}

# Compare results
cat("\n=== RESULTS COMPARISON ===\n")

indep_econ_median <- median(indep_params$pct_change_economic, na.rm = TRUE)
corr_econ_median <- median(corr_params$pct_change_economic, na.rm = TRUE)

indep_mult_median <- median(indep_params$pct_change_multiple, na.rm = TRUE)
corr_mult_median <- median(corr_params$pct_change_multiple, na.rm = TRUE)

indep_negative_pct <- sum(indep_params$utility_economic < 0, na.rm = TRUE) / nrow(indep_params) * 100
corr_negative_pct <- sum(corr_params$utility_economic < 0, na.rm = TRUE) / nrow(corr_params) * 100

cat("INDEPENDENT PARAMETERS (Sturman's assumption):\n")
cat("  Economic adjustments median reduction:", round(indep_econ_median, 1), "%\n")
cat("  Multiple devices median reduction:", round(indep_mult_median, 1), "%\n")
cat("  Negative cases:", round(indep_negative_pct, 1), "%\n\n")

cat("CORRELATED PARAMETERS (realistic organizations):\n")
cat("  Economic adjustments median reduction:", round(corr_econ_median, 1), "%\n")
cat("  Multiple devices median reduction:", round(corr_mult_median, 1), "%\n")
cat("  Negative cases:", round(corr_negative_pct, 1), "%\n\n")

cat("IMPACT OF CORRELATIONS:\n")
cat("  Economic adjustment difference:", round(corr_econ_median - indep_econ_median, 1), "pp\n")
cat("  Multiple devices difference:", round(corr_mult_median - indep_mult_median, 1), "pp\n")
cat("  Negative cases difference:", round(corr_negative_pct - indep_negative_pct, 1), "pp\n\n")

# Test extreme correlation scenario
cat("=== TESTING EXTREME CORRELATION SCENARIO ===\n")

# Create even stronger correlations for organizations in crisis/extreme situations
extreme_corr_matrix <- matrix(c(
  1.00,  0.60, -0.50,  0.40,  0.50,  0.40,  0.50, -0.40,  # log(n)
  0.60,  1.00, -0.40,  0.50,  0.45,  0.35,  0.40, -0.30,  # log(cost)
 -0.50, -0.40,  1.00, -0.60, -0.40, -0.30, -0.35,  0.20,  # sr
  0.40,  0.50, -0.60,  1.00,  0.45,  0.30,  0.35, -0.20,  # r
  0.50,  0.45, -0.40,  0.45,  1.00,  0.35,  0.40, -0.25,  # log(sdy)
  0.40,  0.35, -0.30,  0.30,  0.35,  1.00,  0.65, -0.40,  # discount
  0.50,  0.40, -0.35,  0.35,  0.40,  0.65,  1.00, -0.45,  # tax
 -0.40, -0.30,  0.20, -0.20, -0.25, -0.40, -0.45,  1.00   # vc
), nrow = 8, ncol = 8)

extreme_corr_data <- rmvnorm(n_sims, mean = rep(0, 8), sigma = extreme_corr_matrix)
extreme_uniform_data <- pnorm(extreme_corr_data)

extreme_params <- data.frame(
  n = round(exp(extreme_uniform_data[,1] * (log(1100) - log(1)) + log(1))),
  t = runif(n_sims, 1, 10),
  sr = extreme_uniform_data[,3] * (1.0 - 0.05) + 0.05,
  r = extreme_uniform_data[,4] * (0.77 - 0.10) + 0.10,
  sdy = exp(extreme_uniform_data[,5] * (log(40000) - log(4000)) + log(4000)),
  cost = exp(extreme_uniform_data[,2] * (log(1100) - log(1)) + log(1)),
  discount = extreme_uniform_data[,6] * (0.11 - 0.01) + 0.01,
  tax = extreme_uniform_data[,7] * (0.63 - 0.30) + 0.30,
  vc = extreme_uniform_data[,8] * (-0.02 - (-0.35)) + (-0.35),
  r_old = runif(n_sims, 0.05, 0.38)
)

extreme_params$utility_basic <- with(extreme_params, calculate_basic_utility(n, t, sr, r, sdy, cost))
extreme_params$utility_economic <- with(extreme_params, calculate_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc))
extreme_params$pct_change_extreme <- with(extreme_params, 100 * (utility_basic - utility_economic) / abs(utility_basic))

extreme_median <- median(extreme_params$pct_change_extreme, na.rm = TRUE)
extreme_negative_pct <- sum(extreme_params$utility_economic < 0, na.rm = TRUE) / nrow(extreme_params) * 100

cat("EXTREME CORRELATIONS:\n")
cat("  Economic adjustments median reduction:", round(extreme_median, 1), "%\n")
cat("  Negative cases:", round(extreme_negative_pct, 1), "%\n")
cat("  Gap from 291% target:", round(abs(extreme_median - 291), 1), "pp\n\n")

# Final assessment
cat("=== CORRELATION IMPACT ASSESSMENT ===\n")

if (extreme_median > 200) {
  cat("🚀 BREAKTHROUGH: Extreme correlations approach 291% target!\n")
  cat("This could explain Sturman's finding!\n")
} else if (extreme_median > 150) {
  cat("📈 SIGNIFICANT PROGRESS: Strong correlations show major improvement!\n")
  cat("Correlations are a key factor in the 291% discrepancy.\n")
} else if (corr_econ_median > indep_econ_median + 20) {
  cat("✅ IMPORTANT FACTOR: Correlations have meaningful impact!\n")
  cat("This partially explains the discrepancy.\n")
} else {
  cat("📊 MODEST IMPACT: Correlations alone don't explain 291%.\n")
  cat("Other factors still needed.\n")
}

cat("\nParameter correlations investigation complete.\n") 