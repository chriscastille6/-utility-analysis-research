# CLARIFYING "THE MEDIAN EFFECT SIZE OF THE TOTAL SET OF ADJUSTMENTS"
# ====================================================================
# Sturman's statement: "After applying all five adjustments, the median effect size 
# of the total set of adjustments was 291%, and the mean effect was 298%."
#
# This could mean several different things:
# 1. Median percentage reduction when all 5 adjustments are applied together
# 2. Median of the sum of individual adjustment effects
# 3. Median cumulative effect from the sequential usefulness analysis
# 4. Something else entirely
#
# Let's test all interpretations to see which gets us to 291%

library(dplyr)

set.seed(42)
n_sims <- 10000

cat("CLARIFYING STURMAN'S '291% MEDIAN EFFECT SIZE'\n")
cat("=============================================\n")
cat("Testing different interpretations of what this could mean...\n\n")

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

# Calculate basic utility
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Calculate individual adjustment utilities
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

# Calculate percentage reductions for each adjustment
params$pct_economic <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
params$pct_multiple <- 100 * (params$utility_basic - params$utility_multiple) / abs(params$utility_basic)
params$pct_topdown <- 100 * (params$utility_basic - params$utility_topdown) / abs(params$utility_basic)
params$pct_probation <- 100 * (params$utility_basic - params$utility_probation) / abs(params$utility_basic)
params$pct_flows <- 100 * (params$utility_basic - params$utility_flows) / abs(params$utility_basic)
params$pct_all <- 100 * (params$utility_basic - params$utility_all) / abs(params$utility_basic)

# Remove infinite values for clean analysis
params_clean <- params[is.finite(params$pct_all) & 
                      is.finite(params$pct_economic) & 
                      is.finite(params$pct_multiple) & 
                      is.finite(params$pct_topdown) & 
                      is.finite(params$pct_probation) & 
                      is.finite(params$pct_flows), ]

cat("Working with", nrow(params_clean), "clean scenarios\n\n")

cat("INTERPRETATION 1: COMBINED EFFECT (All 5 adjustments together)\n")
cat("==============================================================\n")
interpretation1_median <- median(params_clean$pct_all, na.rm = TRUE)
interpretation1_mean <- mean(params_clean$pct_all, na.rm = TRUE)
cat("Median combined effect:", round(interpretation1_median, 1), "%\n")
cat("Mean combined effect:", round(interpretation1_mean, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(interpretation1_median - 291), 1), "pp\n\n")

cat("INTERPRETATION 2: SUM OF INDIVIDUAL EFFECTS\n")
cat("===========================================\n")
params_clean$sum_individual <- params_clean$pct_economic + params_clean$pct_multiple + 
                              params_clean$pct_topdown + params_clean$pct_probation + 
                              params_clean$pct_flows
interpretation2_median <- median(params_clean$sum_individual, na.rm = TRUE)
interpretation2_mean <- mean(params_clean$sum_individual, na.rm = TRUE)
cat("Median sum of individual effects:", round(interpretation2_median, 1), "%\n")
cat("Mean sum of individual effects:", round(interpretation2_mean, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(interpretation2_median - 291), 1), "pp\n\n")

cat("INTERPRETATION 3: ABSOLUTE PERCENTAGE CHANGE\n")
cat("============================================\n")
cat("Maybe Sturman means the percentage change in absolute terms, not relative to basic utility?\n")
# Calculate as absolute change divided by absolute value of final utility
params_clean$abs_change_pct <- 100 * abs(params_clean$utility_basic - params_clean$utility_all) / abs(params_clean$utility_all)
interpretation3_median <- median(params_clean$abs_change_pct, na.rm = TRUE)
interpretation3_mean <- mean(params_clean$abs_change_pct, na.rm = TRUE)
cat("Median absolute percentage change:", round(interpretation3_median, 1), "%\n")
cat("Mean absolute percentage change:", round(interpretation3_mean, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(interpretation3_median - 291), 1), "pp\n\n")

cat("INTERPRETATION 4: RATIO OF BASIC TO FINAL UTILITY\n")
cat("=================================================\n")
cat("Maybe it's the ratio: (Basic Utility / Final Utility) * 100?\n")
params_clean$ratio_pct <- 100 * abs(params_clean$utility_basic) / abs(params_clean$utility_all)
interpretation4_median <- median(params_clean$ratio_pct, na.rm = TRUE)
interpretation4_mean <- mean(params_clean$ratio_pct, na.rm = TRUE)
cat("Median ratio percentage:", round(interpretation4_median, 1), "%\n")
cat("Mean ratio percentage:", round(interpretation4_mean, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(interpretation4_median - 291), 1), "pp\n\n")

cat("INTERPRETATION 5: SEQUENTIAL CUMULATIVE EFFECT\n")
cat("==============================================\n")
cat("Maybe it's the cumulative effect from the sequential usefulness analysis?\n")
cat("This would be more complex to calculate...\n\n")

cat("INTERPRETATION 6: DIFFERENT DENOMINATOR\n")
cat("=======================================\n")
cat("Maybe the percentage is calculated differently - using a different base?\n")
# Try using the final utility as denominator instead of basic utility
params_clean$alt_pct <- 100 * (params_clean$utility_basic - params_clean$utility_all) / abs(params_clean$utility_all)
interpretation6_median <- median(params_clean$alt_pct, na.rm = TRUE)
interpretation6_mean <- mean(params_clean$alt_pct, na.rm = TRUE)
cat("Median with final utility as denominator:", round(interpretation6_median, 1), "%\n")
cat("Mean with final utility as denominator:", round(interpretation6_mean, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(interpretation6_median - 291), 1), "pp\n\n")

cat("SUMMARY OF INTERPRETATIONS\n")
cat("==========================\n")
interpretations <- data.frame(
  Interpretation = c(
    "1. Combined Effect (Basic → All)",
    "2. Sum of Individual Effects", 
    "3. Absolute Change Percentage",
    "4. Ratio (Basic/Final) * 100",
    "5. Sequential Cumulative",
    "6. Alt Denominator (Final Utility)"
  ),
  Our_Median = c(
    round(interpretation1_median, 1),
    round(interpretation2_median, 1),
    round(interpretation3_median, 1),
    round(interpretation4_median, 1),
    NA,  # Would need more complex calculation
    round(interpretation6_median, 1)
  ),
  Gap_from_291 = c(
    round(abs(interpretation1_median - 291), 1),
    round(abs(interpretation2_median - 291), 1),
    round(abs(interpretation3_median - 291), 1),
    round(abs(interpretation4_median - 291), 1),
    NA,
    round(abs(interpretation6_median - 291), 1)
  )
)

print(interpretations)

cat("\nCONCLUSION:\n")
cat("==========\n")
cat("None of these interpretations get us close to Sturman's 291%.\n")
cat("The closest is interpretation", which.min(interpretations$Gap_from_291[!is.na(interpretations$Gap_from_291)]), "\n")
cat("with a gap of", min(interpretations$Gap_from_291, na.rm = TRUE), "percentage points.\n\n")

cat("This suggests the 291% discrepancy is not due to misunderstanding\n")
cat("what 'median effect size' means, but likely due to fundamental\n")
cat("differences in parameters, formulas, or calculation methods.\n") 