# TEST AGAINST STURMAN'S ACTUAL TABLE 2 VALUES
# ============================================
# From our codebase, Sturman's Table 2 individual adjustment values are:
# Economic Variables: 64%
# Multiple Devices: 53%
# Deviations from Top-Down Hiring: 23%
# Effect of a Probationary Period: 22%
# The Offset of Employee Flows: 1%
#
# And his final combined result: 291% median, 298% mean
#
# Let's test which interpretation of "median effect size" makes sense

library(dplyr)

set.seed(42)
n_sims <- 10000

cat("TESTING AGAINST STURMAN'S ACTUAL TABLE 2 VALUES\n")
cat("===============================================\n")
cat("Using Sturman's reported individual adjustment effects to test\n")
cat("what 'median effect size of the total set of adjustments' means.\n\n")

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

# Calculate individual adjustments
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

# Calculate percentage reductions
params$pct_economic <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
params$pct_multiple <- 100 * (params$utility_basic - params$utility_multiple) / abs(params$utility_basic)
params$pct_topdown <- 100 * (params$utility_basic - params$utility_topdown) / abs(params$utility_basic)
params$pct_probation <- 100 * (params$utility_basic - params$utility_probation) / abs(params$utility_basic)
params$pct_flows <- 100 * (params$utility_basic - params$utility_flows) / abs(params$utility_basic)
params$pct_all <- 100 * (params$utility_basic - params$utility_all) / abs(params$utility_basic)

# Remove infinite values
params_clean <- params[is.finite(params$pct_all) & 
                      is.finite(params$pct_economic) & 
                      is.finite(params$pct_multiple) & 
                      is.finite(params$pct_topdown) & 
                      is.finite(params$pct_probation) & 
                      is.finite(params$pct_flows), ]

cat("Working with", nrow(params_clean), "clean scenarios\n\n")

cat("STEP 1: CHECK OUR INDIVIDUAL ADJUSTMENT RESULTS\n")
cat("===============================================\n")

individual_results <- data.frame(
  Adjustment = c("Economic", "Multiple", "TopDown", "Probation", "Flows"),
  Our_Median = c(
    median(params_clean$pct_economic, na.rm = TRUE),
    median(params_clean$pct_multiple, na.rm = TRUE),
    median(params_clean$pct_topdown, na.rm = TRUE),
    median(params_clean$pct_probation, na.rm = TRUE),
    median(params_clean$pct_flows, na.rm = TRUE)
  ),
  Sturman_Target = c(64, 53, 23, 22, 1),
  Difference = abs(c(
    median(params_clean$pct_economic, na.rm = TRUE),
    median(params_clean$pct_multiple, na.rm = TRUE),
    median(params_clean$pct_topdown, na.rm = TRUE),
    median(params_clean$pct_probation, na.rm = TRUE),
    median(params_clean$pct_flows, na.rm = TRUE)
  ) - c(64, 53, 23, 22, 1))
) %>%
  arrange(desc(Our_Median))

print(individual_results)

avg_individual_diff <- mean(individual_results$Difference)
cat("\nAverage difference from Sturman's individual values:", round(avg_individual_diff, 1), "pp\n\n")

cat("STEP 2: TEST INTERPRETATIONS OF '291% MEDIAN EFFECT SIZE'\n")
cat("=========================================================\n")

# If we ASSUME Sturman's individual values are correct, what would the combined effect be?

cat("SCENARIO: IF OUR INDIVIDUAL VALUES MATCHED STURMAN'S EXACTLY\n")
cat("============================================================\n")

# Create hypothetical data that matches Sturman's individual values exactly
sturman_individual <- c(64, 53, 23, 22, 1)  # Economic, Multiple, TopDown, Probation, Flows

cat("Sturman's individual adjustment values:\n")
cat("  Economic:", sturman_individual[1], "%\n")
cat("  Multiple:", sturman_individual[2], "%\n")
cat("  TopDown:", sturman_individual[3], "%\n")
cat("  Probation:", sturman_individual[4], "%\n")
cat("  Flows:", sturman_individual[5], "%\n\n")

cat("INTERPRETATION 1: Simple Sum of Individual Effects\n")
cat("==================================================\n")
sum_individual <- sum(sturman_individual)
cat("Sum of Sturman's individual values:", sum_individual, "%\n")
cat("Gap from Sturman's 291%:", abs(sum_individual - 291), "pp\n\n")

cat("INTERPRETATION 2: Weighted Average\n")
cat("==================================\n")
# Maybe it's a weighted average based on some factor?
weights <- c(0.4, 0.3, 0.15, 0.1, 0.05)  # Hypothetical weights
weighted_avg <- sum(sturman_individual * weights)
cat("Weighted average (40%, 30%, 15%, 10%, 5%):", round(weighted_avg, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(weighted_avg - 291), 1), "pp\n\n")

cat("INTERPRETATION 3: Multiplicative Effect\n")
cat("=======================================\n")
# Maybe adjustments compound multiplicatively?
# Starting with 100%, each adjustment reduces by its percentage
remaining_after_econ <- 100 - sturman_individual[1]  # 36%
remaining_after_mult <- remaining_after_econ * (100 - sturman_individual[2]) / 100  # 36% * 47% = 16.92%
remaining_after_topd <- remaining_after_mult * (100 - sturman_individual[3]) / 100  # 16.92% * 77% = 13.03%
remaining_after_prob <- remaining_after_topd * (100 - sturman_individual[4]) / 100  # 13.03% * 78% = 10.16%
remaining_after_flow <- remaining_after_prob * (100 - sturman_individual[5]) / 100  # 10.16% * 99% = 10.06%

total_multiplicative_reduction <- 100 - remaining_after_flow
cat("Multiplicative compounding reduction:", round(total_multiplicative_reduction, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(total_multiplicative_reduction - 291), 1), "pp\n\n")

cat("INTERPRETATION 4: Quadratic/Exponential Effects\n")
cat("===============================================\n")
# Maybe there are interaction effects that amplify the total?
interaction_factor <- 1.8  # Hypothetical amplification
amplified_sum <- sum_individual * interaction_factor
cat("Sum with", interaction_factor, "x amplification:", round(amplified_sum, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(amplified_sum - 291), 1), "pp\n\n")

cat("INTERPRETATION 5: Different Base Calculation\n")
cat("============================================\n")
# Maybe the 291% is calculated differently (e.g., relative to final utility instead of basic)
# If basic utility = 100 and final utility = 25, then:
# Reduction relative to basic = 75%
# But reduction relative to final = 300% (75/25 * 100)
ratio_based <- sum_individual / (100 - sum_individual) * 100
cat("Ratio-based calculation:", round(ratio_based, 1), "%\n")
cat("Gap from Sturman's 291%:", round(abs(ratio_based - 291), 1), "pp\n\n")

cat("STEP 3: TEST WITH OUR ACTUAL DATA\n")
cat("=================================\n")

our_sum <- sum(individual_results$Our_Median)
our_ratio <- our_sum / (100 - our_sum) * 100

cat("Using our actual individual values:\n")
cat("  Simple sum:", round(our_sum, 1), "%\n")
cat("  Ratio-based:", round(our_ratio, 1), "%\n")
cat("  Combined effect (direct):", round(median(params_clean$pct_all, na.rm = TRUE), 1), "%\n\n")

cat("SUMMARY OF INTERPRETATIONS\n")
cat("==========================\n")
interpretations <- data.frame(
  Method = c("1. Simple Sum (Sturman values)", "2. Weighted Average", "3. Multiplicative", 
             "4. Amplified Sum", "5. Ratio-based (Sturman)", "6. Our Simple Sum", 
             "7. Our Ratio-based", "8. Our Direct Combined"),
  Result = c(sum_individual, weighted_avg, total_multiplicative_reduction, 
             amplified_sum, ratio_based, our_sum, our_ratio, 
             median(params_clean$pct_all, na.rm = TRUE)),
  Gap_from_291 = abs(c(sum_individual, weighted_avg, total_multiplicative_reduction, 
                      amplified_sum, ratio_based, our_sum, our_ratio, 
                      median(params_clean$pct_all, na.rm = TRUE)) - 291)
) %>%
  arrange(Gap_from_291)

print(interpretations)

closest_method <- interpretations$Method[1]
closest_gap <- interpretations$Gap_from_291[1]

cat("\nCLOSEST INTERPRETATION:", closest_method, "\n")
cat("Gap:", round(closest_gap, 1), "percentage points\n\n")

if (closest_gap < 20) {
  cat("✅ POTENTIAL MATCH: This interpretation gets reasonably close to 291%\n")
} else {
  cat("⚠️  NO CLEAR MATCH: None of these interpretations explain the 291% figure\n")
}

cat("\nCONCLUSION:\n")
cat("==========\n")
cat("Even testing multiple interpretations of 'median effect size of the total\n")
cat("set of adjustments' using Sturman's own individual values, we cannot\n")
cat("reproduce his 291% figure. This suggests either:\n")
cat("1. A fundamentally different calculation method\n")
cat("2. Different parameter assumptions\n")
cat("3. A different definition of 'effect size'\n")
cat("4. Possible calculation error in the original study\n") 