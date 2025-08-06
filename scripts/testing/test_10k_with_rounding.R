# Test 10,000 Monte Carlo simulation with 2-decimal ux rounding
# Compare results to Sturman's targets

source("scripts/sturman_2000_monte_carlo.R")

cat("=== RUNNING 10,000 MONTE CARLO SIMULATION WITH 2-DECIMAL UX ROUNDING ===\n\n")

# Set parameters matching Sturman's Table 1 ranges
set.seed(42)  # For reproducibility
n_sims <- 10000

cat("Generating", n_sims, "parameter combinations...\n")

# Generate parameters using Sturman's exact ranges
params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.70),
  sdy = runif(n_sims, 4000, 40000),
  cost = exp(runif(n_sims, log(1), log(1100))),
  
  # Economic parameters
  discount = runif(n_sims, 0.05, 0.15),
  tax = runif(n_sims, 0.20, 0.40),
  vc = runif(n_sims, 0.10, 0.30),
  
  # Multiple devices
  r_old = runif(n_sims, 0.05, 0.25),
  
  # Other adjustment parameters (simplified)
  reject_rate = runif(n_sims, 0.10, 0.40),
  corr_perf_accept = runif(n_sims, -0.3, -0.1),
  prob_cutoff = runif(n_sims, -1.5, -0.5),
  turnover_rate = runif(n_sims, 0.05, 0.25),
  perf_turn_corr = runif(n_sims, -0.3, -0.1)
)

cat("Calculating utility estimates...\n")

# Calculate basic utility
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Calculate economic adjustments (simplified version)
params$utility_economic <- with(params, {
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation (simplified)
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
})

# Calculate multiple devices adjustment
params$r_incremental <- params$r - params$r_old
params$utility_multiple <- with(params, {
  n * t * ux(sr) * r_incremental * sdy - (n/sr) * cost
})

# Calculate all adjustments combined (simplified)
params$utility_all <- with(params, {
  annual_benefit <- n * r_incremental * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
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

cat("\n=== RESULTS COMPARISON ===\n")

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

# Test L&W case study with 2-decimal rounding
cat("=== LATHAM & WHYTE CASE STUDY TEST ===\n")
lw_utility <- 470 * 18 * ux(470/1410) * 0.4 * 16290 - 429110
cat("L&W with 2-decimal ux:", paste0("$", format(lw_utility, big.mark = ",")), "\n")
cat("Sturman's L&W result: $59,657,532\n")
cat("Difference:", paste0("$", format(lw_utility - 59657532, big.mark = ",")), "\n")

if (abs(lw_utility - 59657532) < 1) {
  cat("✅ SUCCESS: Perfect L&W replication with 2-decimal rounding!\n")
} else {
  cat("❌ L&W replication still not perfect\n")
}

cat("\n=== IMPACT OF 2-DECIMAL ROUNDING ===\n")
cat("The 2-decimal rounding in ux() function:\n")
cat("- Matches Sturman's Zx values exactly\n")
cat("- Should make our L&W replication perfect\n")
cat("- May bring our general usefulness analysis closer to 291%\n")
cat("- Represents Sturman's actual computational methodology\n") 