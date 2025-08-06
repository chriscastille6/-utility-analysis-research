# Test All Decimal Parameters with 2-Decimal Rounding
# Comprehensive test applying 2-decimal precision to all relevant parameters

# Load standardized utility functions
source("../utilities/sturman_utility_functions.R")

cat("=== RUNNING 10,000 MONTE CARLO WITH ALL 2-DECIMAL ROUNDING ===\n\n")

# Set parameters matching Sturman's Table 1 ranges
set.seed(42)  # For reproducibility
n_sims <- 10000

cat("Generating", n_sims, "parameter combinations with 2-decimal rounding...\n")

# Generate parameters using Sturman's exact ranges with 2-decimal rounding applied
params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = round(runif(n_sims, 0.10, 0.77), 2),  # 2-decimal rounding
  sdy = runif(n_sims, 4000, 40000),
  cost = exp(runif(n_sims, log(1), log(1100))),
  
  # Economic parameters - ALL with 2-decimal rounding
  discount = round(runif(n_sims, 0.01, 0.11), 2),  # 2-decimal rounding
  tax = round(runif(n_sims, 0.30, 0.63), 2),       # 2-decimal rounding
  vc = round(runif(n_sims, -0.35, -0.02), 2),      # 2-decimal rounding (variable costs)
  
  # Multiple devices - 2-decimal rounding
  r_old = round(runif(n_sims, 0.05, 0.38), 2),     # 2-decimal rounding
  
  # Other adjustment parameters - 2-decimal rounding where applicable
  reject_rate = round(runif(n_sims, 0.20, 0.70), 2),           # 2-decimal rounding
  corr_perf_accept = round(runif(n_sims, -0.50, 0.00), 2),     # 2-decimal rounding
  prob_cutoff = round(runif(n_sims, -2.0, 0.0), 2),            # 2-decimal rounding
  turnover_rate = round(runif(n_sims, 0.00, 0.33), 2),         # 2-decimal rounding
  perf_turn_corr = round(runif(n_sims, -0.50, 0.00), 2),       # 2-decimal rounding
  stability = round(runif(n_sims, 0.50, 1.00), 2)              # 2-decimal rounding
)

cat("Calculating utility estimates with 2-decimal precision...\n")

# Calculate basic utility (ux already uses 2-decimal rounding) - Using standardized function
params$utility_basic <- with(params, {
  calculate_basic_utility(n, t, sr, r, sdy, cost)
})

# Calculate economic adjustments with 2-decimal rounded parameters - Using standardized function
params$utility_economic <- with(params, {
  calculate_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc)
})

# Calculate multiple devices adjustment with 2-decimal rounded validity - Using standardized function
params$r_incremental <- params$r - params$r_old
params$utility_multiple <- with(params, {
  calculate_multiple_devices_utility(n, t, sr, r, r_old, sdy, cost)
})

# Calculate all adjustments combined with 2-decimal precision
params$utility_all <- with(params, {
  # Apply top-down hiring adjustment
  z_adj <- ux(sr) * (1 - reject_rate) + corr_perf_accept * dnorm(qnorm(reject_rate))
  
  # Apply employee flows
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  # Economic adjustments with all other factors
  annual_benefit <- avg_workforce * r_incremental * z_adj * sdy * performance_effect
  annual_variable_costs <- annual_benefit * abs(vc)
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  initial_costs <- (n/sr) * cost
  replacement_costs <- n * turnover_rate * t * cost * 0.3
  
  after_tax_pv - initial_costs - replacement_costs
})

# Calculate percentage changes
params$pct_change_economic <- with(params, {
  100 * (utility_basic - utility_economic) / abs(utility_basic)
})

params$pct_change_multiple <- with(params, {
  100 * (utility_basic - utility_multiple) / abs(utility_basic)
})

params$pct_change_all <- with(params, {
  100 * (utility_basic - utility_all) / abs(utility_basic)
})

cat("\n=== RESULTS WITH ALL 2-DECIMAL ROUNDING ===\n")

# Calculate key statistics
economic_median <- median(params$pct_change_economic, na.rm = TRUE)
multiple_median <- median(params$pct_change_multiple, na.rm = TRUE)
all_median <- median(params$pct_change_all, na.rm = TRUE)
negative_pct <- sum(params$utility_all < 0, na.rm = TRUE) / nrow(params) * 100

cat("Economic adjustments median reduction:", round(economic_median, 1), "%\n")
cat("Multiple devices median reduction:", round(multiple_median, 1), "%\n")
cat("All adjustments median reduction:", round(all_median, 1), "%\n")
cat("Percentage of negative cases:", round(negative_pct, 1), "%\n\n")

# Compare to Sturman's targets
cat("=== COMPARISON TO STURMAN'S TARGETS ===\n")
cat("Economic adjustments:\n")
cat("  Our result:", round(economic_median, 1), "%\n")
cat("  Sturman target: ~85% (largest impact)\n")
cat("  Gap:", round(abs(economic_median - 85), 1), "pp\n\n")

cat("Multiple devices:\n")
cat("  Our result:", round(multiple_median, 1), "%\n")
cat("  Sturman target: ~70% (second largest)\n")
cat("  Gap:", round(abs(multiple_median - 70), 1), "pp\n\n")

cat("General usefulness analysis:\n")
cat("  Our result:", round(all_median, 1), "%\n")
cat("  Sturman target: 291%\n")
cat("  Gap:", round(abs(all_median - 291), 1), "pp\n\n")

cat("Negative cases:\n")
cat("  Our result:", round(negative_pct, 1), "%\n")
cat("  Sturman target: 16%\n")
cat("  Gap:", round(abs(negative_pct - 16), 1), "pp\n\n")

# Compare to previous results (with only ux rounding)
cat("=== COMPARISON TO PREVIOUS RESULTS ===\n")
cat("Previous results (only ux 2-decimal rounding):\n")
cat("  Economic: 59.6%, Multiple: 38.0%, All: 76.5%, Negative: 13.9%\n\n")

cat("Current results (all parameters 2-decimal rounding):\n")
cat("  Economic:", round(economic_median, 1), "%, Multiple:", round(multiple_median, 1), 
    "%, All:", round(all_median, 1), "%, Negative:", round(negative_pct, 1), "%\n\n")

# Calculate improvement
econ_improvement <- economic_median - 59.6
mult_improvement <- multiple_median - 38.0
all_improvement <- all_median - 76.5
neg_improvement <- negative_pct - 13.9

cat("Improvements from comprehensive 2-decimal rounding:\n")
cat("  Economic:", sprintf("%+.1f", econ_improvement), "pp\n")
cat("  Multiple:", sprintf("%+.1f", mult_improvement), "pp\n") 
cat("  All adjustments:", sprintf("%+.1f", all_improvement), "pp\n")
cat("  Negative cases:", sprintf("%+.1f", neg_improvement), "pp\n\n")

# Test L&W case study with all 2-decimal rounding
cat("=== LATHAM & WHYTE CASE STUDY TEST ===\n")
lw_utility <- 470 * 18 * ux(470/1410) * round(0.4, 2) * 16290 - 429110
cat("L&W with all 2-decimal rounding:", paste0("$", format(lw_utility, big.mark = ",")), "\n")
cat("Sturman's L&W result: $59,657,532\n")
cat("Difference:", paste0("$", format(lw_utility - 59657532, big.mark = ",")), "\n")

if (abs(lw_utility - 59657532) < 1) {
  cat("✅ SUCCESS: Perfect L&W replication maintained!\n")
} else {
  cat("⚠️  L&W replication slightly affected by additional rounding\n")
}

cat("\n=== FINAL ASSESSMENT ===\n")
if (all_median > 150) {
  cat("🎯 SIGNIFICANT PROGRESS: All-parameter 2-decimal rounding brought us much closer to 291%!\n")
} else if (all_median > 100) {
  cat("📈 GOOD PROGRESS: All-parameter 2-decimal rounding improved results meaningfully.\n")
} else {
  cat("📊 MODEST PROGRESS: All-parameter 2-decimal rounding had limited additional impact.\n")
}

remaining_gap <- abs(all_median - 291)
cat("Remaining gap from 291% target:", round(remaining_gap, 1), "pp\n")

if (remaining_gap < 50) {
  cat("🎉 EXCELLENT: Very close to Sturman's target!\n")
} else if (remaining_gap < 100) {
  cat("✅ GOOD: Substantial progress toward Sturman's target.\n")
} else {
  cat("🔍 INVESTIGATION NEEDED: Still significant gap suggests other methodological differences.\n")
} 