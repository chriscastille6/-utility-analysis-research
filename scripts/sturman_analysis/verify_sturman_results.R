# Verify Sturman (2000) Results
# Test specific scenarios to match published results

# Load standardized utility functions
source("../utilities/sturman_utility_functions.R")

cat("=== VERIFYING STURMAN (2000) RESULTS ===\n\n")

# Verify Sturman (2000) Monte Carlo Results
# This script runs the simulation programmatically and outputs key findings

library(dplyr)
library(scales)

# Source the Monte Carlo functions
source("scripts/sturman_2000_monte_carlo.R")

# Set parameters (using default ranges from Sturman 2000)
set.seed(42)  # For reproducibility
n_sims <- 10000

cat("Running Sturman (2000) Monte Carlo Verification...\n")
cat("Number of simulations:", n_sims, "\n\n")

# Generate parameter ranges (matching Sturman's Table 1)
params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.70),
  sdy = runif(n_sims, 5000, 50000),
  cost = exp(runif(n_sims, log(10), log(1000))),
  
  # Economic parameters
  discount = runif(n_sims, 0.05, 0.15),
  tax = runif(n_sims, 0.20, 0.40),
  vc = runif(n_sims, 0.10, 0.30),
  
  # Multiple devices
  r_old = runif(n_sims, 0.05, 0.25),
  
  # Top-down hiring
  reject_rate = runif(n_sims, 0.10, 0.40),
  corr_perf_accept = runif(n_sims, -0.3, -0.1),
  
  # Probationary period
  prob_cutoff = runif(n_sims, -1.5, -0.5),
  
  # Employee flows
  turnover_rate = runif(n_sims, 0.05, 0.25),
  perf_turn_corr = runif(n_sims, -0.3, -0.1)
)

cat("Calculating utility estimates...\n")

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Calculate basic utility (no adjustments) - Using standardized function
params$utility_basic <- with(params, {
  calculate_basic_utility(n, t, sr, r, sdy, cost)
})

# Adjustment 1: Economic factors (Boudreau 1983a)
params$utility_economic <- with(params, {
  # Present value calculation: sum of discounted annual benefits
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc  # Variable costs increase with performance
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Simplified present value calculation (geometric series formula)
  # PV = Annual_Benefit * [(1 - (1+r)^-t) / r]
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)  # If discount rate is 0, just multiply by years
  
  # After-tax present value minus initial costs
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
})

# Adjustment 2: Multiple devices (incremental validity)
params$utility_multiple <- with(params, {
  incremental_r <- r - r_old
  n * t * ux(sr) * incremental_r * sdy - (n/sr) * cost
})

# Adjustment 3: Top-down hiring deviations
params$utility_topdown <- with(params, {
  # Simplified model for top-down hiring effects
  hiring_efficiency <- 1 - (reject_rate * 0.5)  # Simplified assumption
  n * t * ux(sr) * r * sdy * hiring_efficiency - (n/sr) * cost
})

# Adjustment 4: Probationary period
params$utility_probation <- with(params, {
  # Proportion retained after probation
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  effective_n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Adjustment 5: Employee flows
params$utility_flows <- with(params, {
  # Simplified employee flow adjustment
  flow_factor <- 1 - (turnover_rate * 0.5)  # Simplified assumption
  n * t * ux(sr) * r * sdy * flow_factor - (n/sr) * cost
})

# Combined: Economic + Multiple devices
params$utility_econ_mult <- with(params, {
  # Use incremental validity and economic adjustments
  incremental_r <- r - r_old
  annual_benefit <- n * incremental_r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  # After-tax present value minus initial costs
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
})

# All adjustments combined (using corrected economic approach)
params$utility_all <- with(params, {
  # Apply all major adjustments
  incremental_r <- r - r_old
  hiring_efficiency <- 1 - (reject_rate * 0.5)
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  flow_factor <- 1 - (turnover_rate * 0.5)
  
  # Economic adjustments with all other factors
  effective_n <- n * prob_retained * flow_factor
  annual_benefit <- effective_n * incremental_r * ux(sr) * sdy * hiring_efficiency
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  # After-tax present value minus all costs
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

params$pct_change_topdown <- with(params, {
  100 * (utility_basic - utility_topdown) / abs(utility_basic)
})

params$pct_change_probation <- with(params, {
  100 * (utility_basic - utility_probation) / abs(utility_basic)
})

params$pct_change_flows <- with(params, {
  100 * (utility_basic - utility_flows) / abs(utility_basic)
})

params$pct_change_all <- with(params, {
  100 * (utility_basic - utility_all) / abs(utility_basic)
})

cat("Analysis complete. Generating results...\n\n")

# Summary statistics
utility_cols <- c("utility_basic", "utility_economic", "utility_multiple", 
                 "utility_topdown", "utility_probation", "utility_flows", 
                 "utility_econ_mult", "utility_all")

summary_stats <- params[utility_cols] %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE),
    Negative = ~sum(. < 0, na.rm = TRUE) / length(.) * 100
  )) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  tidyr::separate(Variable, into = c("Adjustment", "Statistic"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(
    Adjustment = case_when(
      Adjustment == "utility_basic" ~ "Basic (No Adjustments)",
      Adjustment == "utility_economic" ~ "Economic Adjustments",
      Adjustment == "utility_multiple" ~ "Multiple Devices",
      Adjustment == "utility_topdown" ~ "Top-Down Hiring",
      Adjustment == "utility_probation" ~ "Probationary Period",
      Adjustment == "utility_flows" ~ "Employee Flows",
      Adjustment == "utility_econ_mult" ~ "Economic + Multiple",
      Adjustment == "utility_all" ~ "All Adjustments",
      TRUE ~ Adjustment
    )
  )

# Usefulness analysis
pct_cols <- c("pct_change_economic", "pct_change_multiple", "pct_change_topdown", 
             "pct_change_probation", "pct_change_flows")

usefulness_stats <- params[pct_cols] %>%
  summarise_all(list(
    Median = ~median(., na.rm = TRUE),
    Mean = ~mean(., na.rm = TRUE),
    Q25 = ~quantile(., 0.25, na.rm = TRUE),
    Q75 = ~quantile(., 0.75, na.rm = TRUE)
  )) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  tidyr::separate(Variable, into = c("Adjustment", "Statistic"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(
    Adjustment = case_when(
      Adjustment == "pct_change_economic" ~ "Economic Adjustments",
      Adjustment == "pct_change_multiple" ~ "Multiple Devices",
      Adjustment == "pct_change_topdown" ~ "Top-Down Hiring",
      Adjustment == "pct_change_probation" ~ "Probationary Period",
      Adjustment == "pct_change_flows" ~ "Employee Flows",
      TRUE ~ Adjustment
    )
  ) %>%
  arrange(desc(Median))

# Print results
cat(paste(rep("=", 60), collapse=""), "\n")
cat("STURMAN (2000) MONTE CARLO VERIFICATION RESULTS\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

cat("SUMMARY STATISTICS:\n")
cat(paste(rep("-", 30), collapse=""), "\n")
print(summary_stats %>% 
  mutate(
    Mean = dollar(Mean),
    Median = dollar(Median),
    `% Negative` = paste0(round(Negative, 1), "%")
  ) %>%
  select(Adjustment, Mean, Median, `% Negative`))

cat("\n\nUSEFULNESS ANALYSIS (Ranking by Median Impact):\n")
cat(paste(rep("-", 50), collapse=""), "\n")
print(usefulness_stats %>%
  mutate(
    `Median %` = paste0(round(Median, 1), "%"),
    `Mean %` = paste0(round(Mean, 1), "%"),
    `25th %` = paste0(round(Q25, 1), "%"),
    `75th %` = paste0(round(Q75, 1), "%")
  ) %>%
  select(Adjustment, `Median %`, `Mean %`, `25th %`, `75th %`))

# Key findings comparison
basic_mean <- summary_stats$Mean[summary_stats$Adjustment == "Basic (No Adjustments)"]
all_mean <- summary_stats$Mean[summary_stats$Adjustment == "All Adjustments"]
total_reduction <- 100 * (basic_mean - all_mean) / basic_mean

econ_median <- usefulness_stats$Median[usefulness_stats$Adjustment == "Economic Adjustments"]
mult_median <- usefulness_stats$Median[usefulness_stats$Adjustment == "Multiple Devices"]

negative_basic <- summary_stats$Negative[summary_stats$Adjustment == "Basic (No Adjustments)"]
negative_all <- summary_stats$Negative[summary_stats$Adjustment == "All Adjustments"]

cat("\n\nKEY FINDINGS COMPARISON TO STURMAN (2000):\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("1. TOTAL REDUCTION FROM ALL ADJUSTMENTS:\n")
cat("   Our result:", round(total_reduction, 1), "%\n")
cat("   Sturman's finding: >90% reduction\n")
cat("   Match:", ifelse(total_reduction > 90, "✓ YES", "✗ NO"), "\n\n")

cat("2. ECONOMIC ADJUSTMENTS (Largest Impact):\n")
cat("   Our median reduction:", round(econ_median, 1), "%\n")
cat("   Sturman's finding: ~85% median reduction\n")
cat("   Match:", ifelse(abs(econ_median - 85) < 20, "✓ CLOSE", "✗ DIFFERENT"), "\n\n")

cat("3. MULTIPLE DEVICES (Second Largest):\n")
cat("   Our median reduction:", round(mult_median, 1), "%\n")
cat("   Sturman's finding: ~70% median reduction\n")
cat("   Match:", ifelse(abs(mult_median - 70) < 20, "✓ CLOSE", "✗ DIFFERENT"), "\n\n")

cat("4. NEGATIVE UTILITY CASES:\n")
cat("   Basic model negative:", round(negative_basic, 1), "%\n")
cat("   All adjustments negative:", round(negative_all, 1), "%\n")
cat("   Sturman's finding: Many scenarios become negative\n")
cat("   Match:", ifelse(negative_all > negative_basic, "✓ YES", "✗ NO"), "\n\n")

cat("5. RANKING ORDER:\n")
cat("   Our ranking:\n")
for(i in 1:nrow(usefulness_stats)) {
  cat("   ", i, ". ", usefulness_stats$Adjustment[i], " (", 
      round(usefulness_stats$Median[i], 1), "%)\n", sep="")
}
cat("   Sturman's expected: Economic > Multiple Devices > Others\n")
cat("   Match:", ifelse(usefulness_stats$Adjustment[1] == "Economic Adjustments" && 
                       usefulness_stats$Adjustment[2] == "Multiple Devices", 
                       "✓ YES", "✗ NO"), "\n\n")

cat("CONCLUSION:\n")
cat(paste(rep("-", 20), collapse=""), "\n")
if(total_reduction > 90 && 
   usefulness_stats$Adjustment[1] == "Economic Adjustments" && 
   usefulness_stats$Adjustment[2] == "Multiple Devices" &&
   negative_all > negative_basic) {
  cat("✓ VERIFICATION SUCCESSFUL: Results match Sturman's key findings!\n")
} else {
  cat("⚠ PARTIAL MATCH: Some results differ from Sturman's findings.\n")
}

cat("\nSimulation completed successfully.\n") 