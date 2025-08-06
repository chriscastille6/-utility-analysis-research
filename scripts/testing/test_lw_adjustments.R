# Test Latham & Whyte Case Study Adjustments Against Sturman's Exact Values
# Verifying our modeling matches the specific dollar amounts reported

source("scripts/sturman_2000_monte_carlo.R")

cat("=== TESTING L&W CASE STUDY ADJUSTMENTS ===\n\n")

# Sturman's reported L&W adjustment effects:
cat("Sturman's reported L&W adjustment effects:\n")
cat("- Basic utility: $59,657,532\n")
cat("- Multiple devices reduction: $9.3 million\n") 
cat("- Top-down hiring reduction: $4.4 million\n")
cat("- Probationary period reduction: $0.5 million\n")
cat("- Employee flows reduction: $0.5 million\n")
cat("- Final mean utility: $2,228,170 (96% reduction)\n")
cat("- Final median utility: $1,738,861\n\n")

# L&W fixed parameters
n_hired <- 470
t_years <- 18
validity <- 0.4
sdy <- 16290
total_cost <- 429110
total_applicants <- 1410
selection_ratio <- n_hired / total_applicants

# Basic utility (we know this matches exactly)
basic_utility <- n_hired * t_years * ux(selection_ratio) * validity * sdy - total_cost
cat("=== OUR L&W REPLICATION ===\n")
cat("Basic utility:", paste0("$", format(basic_utility, big.mark = ",")), "\n")
cat("Sturman basic:", "$59,657,532\n")
cat("Match:", abs(basic_utility - 59657532) < 1, "\n\n")

# Now test individual adjustments using L&W parameters + random additional parameters
# We need to simulate the 10,000 L&W scenarios with varying adjustment parameters

set.seed(42)
n_sims <- 10000

cat("Generating", n_sims, "L&W scenarios with varying adjustment parameters...\n")

# Fixed L&W parameters for all scenarios
lw_params <- data.frame(
  n = rep(n_hired, n_sims),
  t = rep(t_years, n_sims),
  sr = rep(selection_ratio, n_sims),
  r = rep(validity, n_sims),
  sdy = rep(sdy, n_sims),
  cost_total = rep(total_cost, n_sims),
  
  # Variable adjustment parameters (from Table 1 ranges, 2-decimal rounded)
  discount = round(runif(n_sims, 0.01, 0.11), 2),
  tax = round(runif(n_sims, 0.30, 0.63), 2),
  vc = round(runif(n_sims, -0.35, -0.02), 2),
  r_old = round(runif(n_sims, 0.05, 0.38), 2),
  reject_rate = round(runif(n_sims, 0.20, 0.70), 2),
  corr_perf_accept = round(runif(n_sims, -0.50, 0.00), 2),
  prob_cutoff = round(runif(n_sims, -2.0, 0.0), 2),
  turnover_rate = round(runif(n_sims, 0.00, 0.33), 2),
  perf_turn_corr = round(runif(n_sims, -0.50, 0.00), 2)
)

# Calculate basic utility (should be same for all)
lw_params$utility_basic <- with(lw_params, {
  n * t * ux(sr) * r * sdy - cost_total
})

# Calculate economic adjustments
lw_params$utility_economic <- with(lw_params, {
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * abs(vc)
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  after_tax_pv - cost_total
})

# Calculate multiple devices adjustment
lw_params$r_incremental <- lw_params$r - lw_params$r_old
lw_params$utility_multiple <- with(lw_params, {
  n * t * ux(sr) * r_incremental * sdy - cost_total
})

# Calculate top-down hiring adjustment
lw_params$utility_topdown <- with(lw_params, {
  z_adj <- ux(sr) * (1 - reject_rate) + corr_perf_accept * dnorm(qnorm(reject_rate))
  n * t * z_adj * r * sdy - cost_total
})

# Calculate probationary period adjustment
lw_params$utility_probation <- with(lw_params, {
  prob_survive <- pnorm(prob_cutoff, lower.tail = FALSE)
  performance_gain <- dnorm(prob_cutoff) / prob_survive
  
  (n * t * ux(sr) * r * sdy * (1 + 0.1 * performance_gain)) - 
  cost_total - 
  (n * (1 - prob_survive) * (cost_total/n) * 0.5)
})

# Calculate employee flows adjustment
lw_params$utility_flows <- with(lw_params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  avg_workforce * t * ux(sr) * r * sdy * performance_effect - 
  cost_total - 
  (n * turnover_rate * t * (cost_total/n) * 0.3)
})

# Calculate cumulative adjustments (step by step as Sturman did)
lw_params$utility_econ_mult <- with(lw_params, {
  annual_benefit <- n * r_incremental * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * abs(vc)
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  after_tax_pv - cost_total
})

# Final combined utility (all adjustments)
lw_params$utility_all <- with(lw_params, {
  z_adj <- ux(sr) * (1 - reject_rate) + corr_perf_accept * dnorm(qnorm(reject_rate))
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  annual_benefit <- avg_workforce * r_incremental * z_adj * sdy * performance_effect
  annual_variable_costs <- annual_benefit * abs(vc)
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- (1 - (1 + discount)^(-t)) / discount
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  initial_costs <- cost_total
  replacement_costs <- n * turnover_rate * t * (cost_total/n) * 0.3
  
  after_tax_pv - initial_costs - replacement_costs
})

# Calculate step-by-step reductions
cat("\n=== STEP-BY-STEP ADJUSTMENT ANALYSIS ===\n")

# Step 1: Economic adjustments
econ_reduction_mean <- mean(lw_params$utility_basic - lw_params$utility_economic, na.rm = TRUE)
cat("Step 1 - Economic adjustments:\n")
cat("  Our mean reduction: $", format(econ_reduction_mean/1000000, digits = 2), " million\n")
cat("  Sturman reported: ~$42.8 million (from $59.7M to ~$17M)\n\n")

# Step 2: Multiple devices (from economic base)
mult_reduction_mean <- mean(lw_params$utility_economic - lw_params$utility_econ_mult, na.rm = TRUE)
cat("Step 2 - Multiple devices (incremental):\n")
cat("  Our mean reduction: $", format(mult_reduction_mean/1000000, digits = 2), " million\n")
cat("  Sturman reported: $9.3 million\n\n")

# Compare final results
final_mean <- mean(lw_params$utility_all, na.rm = TRUE)
final_median <- median(lw_params$utility_all, na.rm = TRUE)
final_min <- min(lw_params$utility_all, na.rm = TRUE)
final_max <- max(lw_params$utility_all, na.rm = TRUE)

cat("=== FINAL L&W RESULTS COMPARISON ===\n")
cat("Final mean utility:\n")
cat("  Our result: $", format(final_mean, big.mark = ","), "\n")
cat("  Sturman target: $2,228,170\n")
cat("  Difference: $", format(final_mean - 2228170, big.mark = ","), "\n\n")

cat("Final median utility:\n")
cat("  Our result: $", format(final_median, big.mark = ","), "\n")
cat("  Sturman target: $1,738,861\n")
cat("  Difference: $", format(final_median - 1738861, big.mark = ","), "\n\n")

cat("Range of outcomes:\n")
cat("  Our min: $", format(final_min, big.mark = ","), "\n")
cat("  Our max: $", format(final_max, big.mark = ","), "\n")
cat("  Sturman min: -$2,577,238\n")
cat("  Sturman max: $17,162,452\n\n")

# Calculate overall reduction
overall_reduction <- (basic_utility - final_mean) / basic_utility * 100
sturman_reduction <- (59657532 - 2228170) / 59657532 * 100

cat("Overall reduction:\n")
cat("  Our reduction: ", round(overall_reduction, 1), "%\n")
cat("  Sturman reduction: ", round(sturman_reduction, 1), "%\n")
cat("  Difference: ", round(abs(overall_reduction - sturman_reduction), 1), "pp\n\n")

# Check if we're in the right ballpark
cat("=== ASSESSMENT ===\n")
if (abs(final_mean - 2228170) < 1000000) {
  cat("✅ EXCELLENT: Final mean within $1M of Sturman's target!\n")
} else if (abs(final_mean - 2228170) < 5000000) {
  cat("✅ GOOD: Final mean within $5M of Sturman's target.\n")
} else {
  cat("❌ SIGNIFICANT GAP: Final mean differs substantially from Sturman.\n")
}

if (abs(final_median - 1738861) < 1000000) {
  cat("✅ EXCELLENT: Final median within $1M of Sturman's target!\n")
} else if (abs(final_median - 1738861) < 5000000) {
  cat("✅ GOOD: Final median within $5M of Sturman's target.\n")
} else {
  cat("❌ SIGNIFICANT GAP: Final median differs substantially from Sturman.\n")
} 