# STURMAN'S USEFULNESS ANALYSIS: STEP-BY-STEP WALKTHROUGH
# ========================================================
# This script demonstrates exactly what Sturman did with concrete examples

library(dplyr)
library(scales)

set.seed(42)
n_sims <- 1000  # Using smaller sample for clearer demonstration

# Simple dollar formatting function
dollar_format <- function(x) {
  paste0("$", format(round(x, 0), big.mark = ","))
}

cat("STURMAN'S USEFULNESS ANALYSIS: COMPLETE WALKTHROUGH\n")
cat("===================================================\n\n")

cat("WHAT IS USEFULNESS ANALYSIS?\n")
cat("============================\n")
cat("It's a sequential selection procedure that determines:\n")
cat("1. Which adjustment has the biggest impact when applied alone?\n")
cat("2. Which adjustment adds the most when combined with the winner?\n")
cat("3. Continue until all adjustments are ranked by their incremental contribution.\n\n")

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Generate a smaller dataset for demonstration
params <- data.frame(
  n = round(exp(runif(n_sims, log(10), log(100)))),  # Smaller range for clarity
  t = runif(n_sims, 2, 8),
  sr = runif(n_sims, 0.1, 0.8),
  r = runif(n_sims, 0.20, 0.60),
  sdy = runif(n_sims, 10000, 30000),
  cost = exp(runif(n_sims, log(50), log(500))),
  
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

cat("EXAMPLE SCENARIO (First organization in our simulation):\n")
cat("=======================================================\n")
cat("Number hired (n):", params$n[1], "\n")
cat("Tenure (t):", round(params$t[1], 1), "years\n")
cat("Selection ratio (sr):", round(params$sr[1], 3), "\n")
cat("Validity (r):", round(params$r[1], 2), "\n")
cat("SDy:", dollar_format(params$sdy[1]), "\n")
cat("Cost per applicant:", dollar_format(params$cost[1]), "\n")
cat("Discount rate:", round(params$discount[1] * 100, 1), "%\n")
cat("Tax rate:", round(params$tax[1] * 100, 1), "%\n")
cat("Variable costs:", round(params$vc[1] * 100, 1), "%\n\n")

# BASELINE: Basic utility (no adjustments)
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

cat("BASELINE CALCULATION:\n")
cat("====================\n")
basic_example <- with(params[1,], {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})
cat("Basic Utility = n × t × ux(sr) × r × SDy - (n/sr) × cost\n")
cat("Basic Utility =", params$n[1], "×", round(params$t[1], 1), "×", 
    round(ux(params$sr[1]), 2), "×", round(params$r[1], 2), "×", 
    dollar_format(params$sdy[1]), "- (", params$n[1], "/", round(params$sr[1], 3), ") ×", 
    dollar_format(params$cost[1]), "\n")
cat("Basic Utility =", dollar_format(basic_example), "\n\n")

cat("STEP 1: APPLY EACH ADJUSTMENT INDEPENDENTLY\n")
cat("===========================================\n")
cat("Now we apply each of the 5 adjustments by itself to see which has the biggest impact.\n\n")

# 1. Economic adjustments only
cat("1. ECONOMIC ADJUSTMENTS ONLY:\n")
cat("   - Apply taxes, variable costs, and discounting\n")
cat("   - Keep everything else the same as basic utility\n")
params$utility_economic_only <- with(params, {
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

econ_example <- params$utility_economic_only[1]
econ_reduction <- 100 * (basic_example - econ_example) / abs(basic_example)
cat("   Economic-adjusted utility:", dollar_format(econ_example), "\n")
cat("   Reduction:", round(econ_reduction, 1), "%\n\n")

# 2. Multiple devices only
cat("2. MULTIPLE DEVICES ONLY:\n")
cat("   - Use incremental validity (r - r_old) instead of full validity\n")
cat("   - Keep everything else the same as basic utility\n")
params$utility_multiple_only <- with(params, {
  incremental_r <- r - r_old
  n * t * ux(sr) * incremental_r * sdy - (n/sr) * cost
})

mult_example <- params$utility_multiple_only[1]
mult_reduction <- 100 * (basic_example - mult_example) / abs(basic_example)
cat("   Multiple devices utility:", dollar_format(mult_example), "\n")
cat("   Reduction:", round(mult_reduction, 1), "%\n\n")

# 3. Top-down hiring only
cat("3. TOP-DOWN HIRING DEVIATIONS ONLY:\n")
cat("   - Adjust for job offer rejections and performance-acceptance correlation\n")
cat("   - Keep everything else the same as basic utility\n")
params$utility_topdown_only <- with(params, {
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  n * t * z_adj * r * sdy - (n/sr) * cost
})

topdown_example <- params$utility_topdown_only[1]
topdown_reduction <- 100 * (basic_example - topdown_example) / abs(basic_example)
cat("   Top-down adjusted utility:", dollar_format(topdown_example), "\n")
cat("   Reduction:", round(topdown_reduction, 1), "%\n\n")

# 4. Probationary period only
cat("4. PROBATIONARY PERIOD ONLY:\n")
cat("   - Reduce effective workforce by probability of retention\n")
cat("   - Keep everything else the same as basic utility\n")
params$utility_probation_only <- with(params, {
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  effective_n * t * ux(sr) * r * sdy - (n/sr) * cost
})

prob_example <- params$utility_probation_only[1]
prob_reduction <- 100 * (basic_example - prob_example) / abs(basic_example)
cat("   Probation-adjusted utility:", dollar_format(prob_example), "\n")
cat("   Reduction:", round(prob_reduction, 1), "%\n\n")

# 5. Employee flows only
cat("5. EMPLOYEE FLOWS ONLY:\n")
cat("   - Adjust for turnover and performance-turnover correlation\n")
cat("   - Keep everything else the same as basic utility\n")
params$utility_flows_only <- with(params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  (avg_workforce * t * ux(sr) * r * sdy * performance_effect) - 
  (n/sr) * cost - (n * turnover_rate * t * cost * 0.3)
})

flows_example <- params$utility_flows_only[1]
flows_reduction <- 100 * (basic_example - flows_example) / abs(basic_example)
cat("   Employee flows utility:", dollar_format(flows_example), "\n")
cat("   Reduction:", round(flows_reduction, 1), "%\n\n")

# Calculate median effects across all scenarios
cat("STEP 1 RESULTS: MEDIAN EFFECTS ACROSS ALL", n_sims, "SCENARIOS\n")
cat("==========================================================\n")

round1_effects <- data.frame(
  Adjustment = c("Economic", "Multiple", "TopDown", "Probation", "Flows"),
  Example_Reduction = c(econ_reduction, mult_reduction, topdown_reduction, prob_reduction, flows_reduction),
  Median_Reduction = c(
    median(100 * (params$utility_basic - params$utility_economic_only) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_multiple_only) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_topdown_only) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_probation_only) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_flows_only) / abs(params$utility_basic), na.rm = TRUE)
  )
) %>%
  arrange(desc(Median_Reduction))

print(round1_effects)

round1_winner <- round1_effects$Adjustment[1]
cat("\nSTEP 1 WINNER:", round1_winner, "with", round(round1_effects$Median_Reduction[1], 1), "% median reduction\n\n")

cat("STEP 2: COMBINE THE WINNER WITH EACH REMAINING ADJUSTMENT\n")
cat("=========================================================\n")
cat("Now we take", round1_winner, "(the winner) and combine it with each of the other 4 adjustments.\n")
cat("We want to see which combination gives the largest median effect.\n\n")

# Assuming Economic wins (as expected), combine with others
cat("2A. ECONOMIC + MULTIPLE DEVICES:\n")
cat("   - Use incremental validity (r - r_old)\n")
cat("   - PLUS apply economic adjustments (taxes, variable costs, discounting)\n")
params$utility_econ_multiple <- with(params, {
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

econ_mult_example <- params$utility_econ_multiple[1]
econ_mult_reduction <- 100 * (basic_example - econ_mult_example) / abs(basic_example)
cat("   Example result:", dollar_format(econ_mult_example), "(", round(econ_mult_reduction, 1), "% reduction)\n\n")

cat("2B. ECONOMIC + TOP-DOWN HIRING:\n")
cat("   - Apply top-down hiring adjustments\n")
cat("   - PLUS apply economic adjustments\n")
params$utility_econ_topdown <- with(params, {
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  
  annual_benefit <- n * r * z_adj * sdy
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

econ_topdown_example <- params$utility_econ_topdown[1]
econ_topdown_reduction <- 100 * (basic_example - econ_topdown_example) / abs(basic_example)
cat("   Example result:", dollar_format(econ_topdown_example), "(", round(econ_topdown_reduction, 1), "% reduction)\n\n")

# Continue with other combinations...
cat("2C. ECONOMIC + PROBATIONARY PERIOD:\n")
params$utility_econ_probation <- with(params, {
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  
  annual_benefit <- effective_n * r * ux(sr) * sdy
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

cat("2D. ECONOMIC + EMPLOYEE FLOWS:\n")
params$utility_econ_flows <- with(params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  annual_benefit <- avg_workforce * r * ux(sr) * sdy * performance_effect
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

# Calculate Step 2 median effects
cat("\nSTEP 2 RESULTS: MEDIAN EFFECTS OF COMBINATIONS\n")
cat("==============================================\n")

round2_effects <- data.frame(
  Combination = c("Econ+Multiple", "Econ+TopDown", "Econ+Probation", "Econ+Flows"),
  Median_Reduction = c(
    median(100 * (params$utility_basic - params$utility_econ_multiple) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_econ_topdown) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_econ_probation) / abs(params$utility_basic), na.rm = TRUE),
    median(100 * (params$utility_basic - params$utility_econ_flows) / abs(params$utility_basic), na.rm = TRUE)
  )
) %>%
  arrange(desc(Median_Reduction))

print(round2_effects)

round2_winner <- round2_effects$Combination[1]
cat("\nSTEP 2 WINNER:", round2_winner, "with", round(round2_effects$Median_Reduction[1], 1), "% median reduction\n\n")

cat("STEP 3: CONTINUE THE PROCESS\n")
cat("============================\n")
cat("Now we take the Step 2 winner (", round2_winner, ") and add each remaining adjustment.\n")
cat("This continues until all 5 adjustments are ranked by their incremental contribution.\n\n")

cat("THE KEY INSIGHT:\n")
cat("================\n")
cat("This procedure shows the ORDER OF IMPORTANCE:\n")
cat("1. Which adjustment matters most when applied alone?\n")
cat("2. Which adjustment adds the most value when combined with #1?\n")
cat("3. Which adjustment adds the most when combined with #1 and #2?\n")
cat("4. And so on...\n\n")

cat("This is different from just applying all adjustments at once!\n")
cat("It shows the INCREMENTAL value of each adjustment.\n\n")

# Final result with all adjustments
params$utility_all_combined <- with(params, {
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

final_median <- median(100 * (params$utility_basic - params$utility_all_combined) / abs(params$utility_basic), na.rm = TRUE)
final_example <- params$utility_all_combined[1]
final_example_reduction <- 100 * (basic_example - final_example) / abs(basic_example)

cat("FINAL RESULT: ALL 5 ADJUSTMENTS COMBINED\n")
cat("========================================\n")
cat("Example scenario final utility:", dollar_format(final_example), "\n")
cat("Example scenario reduction:", round(final_example_reduction, 1), "%\n")
cat("Median reduction across all scenarios:", round(final_median, 1), "%\n")
cat("Sturman's target: 291%\n")
cat("Our gap:", round(abs(final_median - 291), 1), "percentage points\n\n")

cat("SUMMARY OF STURMAN'S USEFULNESS ANALYSIS:\n")
cat("=========================================\n")
cat("1. Apply each adjustment independently → Find the winner\n")
cat("2. Combine winner with each remaining adjustment → Find best combination\n")
cat("3. Continue until all adjustments are ranked\n")
cat("4. The final result shows the cumulative effect of all adjustments\n")
cat("5. But the process reveals the ORDER OF IMPORTANCE for practitioners\n\n")

cat("This tells practitioners: 'If you can only make one adjustment, do Economic.\n")
cat("If you can make two, do Economic + Multiple Devices. And so on.'\n") 