# AMPLIFICATION FACTOR ANALYSIS
# =============================
# We discovered that Sturman's 291% can be approximated by:
# Sum of Individual Effects (163%) × 1.8 = 293.4%
#
# This suggests interaction effects or a different calculation method.
# Let's investigate what could cause this amplification.

library(dplyr)

set.seed(42)
n_sims <- 10000

cat("AMPLIFICATION FACTOR ANALYSIS\n")
cat("=============================\n")
cat("Investigating why Sturman's combined effect (291%) is 1.8x larger\n")
cat("than the simple sum of individual effects (163%).\n\n")

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Generate parameters
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

# Calculate basic utility
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Calculate all adjustments combined
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

# Calculate individual adjustments (for comparison)
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

# Calculate percentage reductions
params$pct_all <- 100 * (params$utility_basic - params$utility_all) / abs(params$utility_basic)
params$pct_economic <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)

# Remove infinite values
params_clean <- params[is.finite(params$pct_all) & is.finite(params$pct_economic), ]

cat("Working with", nrow(params_clean), "clean scenarios\n\n")

# ANALYSIS 1: INTERACTION EFFECTS
cat("ANALYSIS 1: DETECTING INTERACTION EFFECTS\n")
cat("=========================================\n")

# Calculate the amplification factor in our data
our_combined_median <- median(params_clean$pct_all, na.rm = TRUE)
our_economic_median <- median(params_clean$pct_economic, na.rm = TRUE)

# If we had all 5 adjustments with Sturman's values
sturman_sum <- 64 + 53 + 23 + 22 + 1  # 163%
sturman_combined <- 291

# Calculate implied amplification factors
our_amplification <- our_combined_median / our_economic_median  # Just economic vs. all
sturman_amplification <- sturman_combined / sturman_sum

cat("Our data:\n")
cat("  Economic only:", round(our_economic_median, 1), "%\n")
cat("  All combined:", round(our_combined_median, 1), "%\n")
cat("  Amplification factor:", round(our_amplification, 2), "x\n\n")

cat("Sturman's data:\n")
cat("  Sum of individual:", sturman_sum, "%\n")
cat("  Combined reported:", sturman_combined, "%\n")
cat("  Amplification factor:", round(sturman_amplification, 2), "x\n\n")

# ANALYSIS 2: COMPOUND INTERACTION EFFECTS
cat("ANALYSIS 2: COMPOUND INTERACTION EFFECTS\n")
cat("========================================\n")

# Test if the amplification could come from multiplicative interactions
# between adjustments in the utility formula

# Let's examine the mathematical structure
cat("Mathematical Analysis:\n")
cat("In the combined utility formula, adjustments interact through:\n")
cat("1. Multiplicative terms (r_incremental × z_adj × performance_effect)\n")
cat("2. Compounding present value calculations\n")  
cat("3. Tax effects applied to the entire benefit stream\n")
cat("4. Cost interactions (replacement costs depend on turnover)\n\n")

# ANALYSIS 3: PARAMETER CORRELATION EFFECTS
cat("ANALYSIS 3: PARAMETER CORRELATION EFFECTS\n")
cat("=========================================\n")

# Check if certain parameter combinations create amplification
high_impact_scenarios <- params_clean[params_clean$pct_all > quantile(params_clean$pct_all, 0.9, na.rm = TRUE), ]
low_impact_scenarios <- params_clean[params_clean$pct_all < quantile(params_clean$pct_all, 0.1, na.rm = TRUE), ]

cat("High impact scenarios (top 10%):\n")
cat("  Mean discount rate:", round(mean(high_impact_scenarios$discount, na.rm = TRUE), 3), "\n")
cat("  Mean tax rate:", round(mean(high_impact_scenarios$tax, na.rm = TRUE), 3), "\n")
cat("  Mean variable cost:", round(mean(high_impact_scenarios$vc, na.rm = TRUE), 3), "\n")
cat("  Mean time horizon:", round(mean(high_impact_scenarios$t, na.rm = TRUE), 1), "\n\n")

cat("Low impact scenarios (bottom 10%):\n")
cat("  Mean discount rate:", round(mean(low_impact_scenarios$discount, na.rm = TRUE), 3), "\n")
cat("  Mean tax rate:", round(mean(low_impact_scenarios$tax, na.rm = TRUE), 3), "\n")
cat("  Mean variable cost:", round(mean(low_impact_scenarios$vc, na.rm = TRUE), 3), "\n")
cat("  Mean time horizon:", round(mean(low_impact_scenarios$t, na.rm = TRUE), 1), "\n\n")

# ANALYSIS 4: TESTING THE AMPLIFICATION HYPOTHESIS
cat("ANALYSIS 4: TESTING THE AMPLIFICATION HYPOTHESIS\n")
cat("================================================\n")

# If the amplification factor is real, we should be able to predict
# Sturman's result by applying it to our individual effects

our_individual_sum <- 58.0 + 51.1 + 36.3 + 16.5 + 10.2  # From our Table 2 replication
predicted_with_amplification <- our_individual_sum * 1.78

cat("Testing amplification hypothesis:\n")
cat("  Our sum of individual effects:", round(our_individual_sum, 1), "%\n")
cat("  Predicted with 1.78x amplification:", round(predicted_with_amplification, 1), "%\n")
cat("  Our actual combined effect:", round(our_combined_median, 1), "%\n")
cat("  Difference:", round(abs(predicted_with_amplification - our_combined_median), 1), "pp\n\n")

# ANALYSIS 5: ALTERNATIVE CALCULATION METHODS
cat("ANALYSIS 5: ALTERNATIVE CALCULATION METHODS\n")
cat("===========================================\n")

# Test if different ways of calculating "effect size" could explain amplification
cat("Testing alternative effect size calculations:\n\n")

# Method 1: Percentage change in absolute utility values
params_clean$abs_change <- abs(params_clean$utility_basic - params_clean$utility_all)
params_clean$abs_basic <- abs(params_clean$utility_basic)
params_clean$abs_effect_size <- 100 * params_clean$abs_change / params_clean$abs_basic

abs_median <- median(params_clean$abs_effect_size, na.rm = TRUE)
cat("1. Absolute change method:", round(abs_median, 1), "%\n")

# Method 2: Logarithmic effect size
params_clean$log_effect_size <- 100 * log(abs(params_clean$utility_basic) / abs(params_clean$utility_all))
log_median <- median(params_clean$log_effect_size, na.rm = TRUE)
cat("2. Logarithmic method:", round(log_median, 1), "%\n")

# Method 3: Ratio-based effect size (utility_basic / utility_all)
params_clean$ratio_effect_size <- 100 * (abs(params_clean$utility_basic) / abs(params_clean$utility_all) - 1)
ratio_median <- median(params_clean$ratio_effect_size, na.rm = TRUE)
cat("3. Ratio-based method:", round(ratio_median, 1), "%\n")

# Method 4: Squared effect size (emphasizes large changes)
params_clean$squared_effect_size <- sqrt(params_clean$pct_all^2) * sign(params_clean$pct_all)
squared_median <- median(params_clean$squared_effect_size, na.rm = TRUE)
cat("4. Squared method:", round(squared_median, 1), "%\n\n")

# SUMMARY
cat("SUMMARY OF FINDINGS\n")
cat("==================\n")
cat("The 1.8x amplification factor suggests that Sturman's 'median effect size\n")
cat("of the total set of adjustments' is NOT simply the sum of individual effects.\n\n")

cat("Possible explanations:\n")
cat("1. INTERACTION EFFECTS: Adjustments amplify each other's impact\n")
cat("2. MATHEMATICAL TRANSFORMATION: Different calculation method\n")
cat("3. PARAMETER DEPENDENCIES: Certain parameter combinations create amplification\n")
cat("4. COMPOUND BENEFITS: Present value and tax effects compound the benefits\n\n")

cat("The fact that 163% × 1.78 ≈ 291% is unlikely to be coincidental.\n")
cat("This suggests a systematic methodological difference in how Sturman\n")
cat("calculated the 'total effect size' compared to individual effects.\n")

# Test which alternative method gets closest to 291%
methods_test <- data.frame(
  Method = c("Standard", "Absolute", "Logarithmic", "Ratio-based", "Squared"),
  Result = c(our_combined_median, abs_median, log_median, ratio_median, squared_median),
  Gap_from_291 = abs(c(our_combined_median, abs_median, log_median, ratio_median, squared_median) - 291)
) %>%
  arrange(Gap_from_291)

cat("\nTesting alternative calculation methods:\n")
print(methods_test)

closest_method <- methods_test$Method[1]
closest_gap <- methods_test$Gap_from_291[1]

cat("\nClosest method:", closest_method, "with gap of", round(closest_gap, 1), "pp\n")

if (closest_gap < 50) {
  cat("✅ POTENTIAL EXPLANATION: This calculation method may explain the amplification\n")
} else {
  cat("⚠️  The amplification remains unexplained by standard calculation methods\n")
} 