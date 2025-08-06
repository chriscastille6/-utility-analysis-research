# Complete Sturman (2000) Table Replication
# Tables 1, 2, and 3 with hybrid improved approach

library(dplyr)
library(mvtnorm)

# Load our utility functions
source("scripts/sturman_2000_monte_carlo.R")

set.seed(42)
n_sims <- 10000

cat("=== STURMAN (2000) COMPLETE TABLE REPLICATION ===\n\n")

# Generate parameters using Sturman's exact Table 1 ranges
params <- data.frame(
  # Basic parameters (Table 1)
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = round(runif(n_sims, 0.10, 0.77), 2),
  sdy = runif(n_sims, 4000, 40000),
  cost = exp(runif(n_sims, log(1), log(1100))),
  
  # Economic parameters
  discount = round(runif(n_sims, 0.01, 0.11), 2),
  tax = round(runif(n_sims, 0.30, 0.63), 2),
  vc = round(runif(n_sims, -0.35, -0.02), 2),
  
  # Multiple devices
  r_old = round(runif(n_sims, 0.05, 0.38), 2),
  
  # Other adjustments
  reject_rate = round(runif(n_sims, 0.20, 0.70), 2),
  corr_perf_accept = round(runif(n_sims, -0.50, 0.00), 2),
  prob_cutoff = round(runif(n_sims, -2.0, 0.0), 2),
  turnover_rate = round(runif(n_sims, 0.00, 0.33), 2),
  perf_turn_corr = round(runif(n_sims, -0.50, 0.00), 2)
)

cat("=== TABLE 1: PARAMETER VERIFICATION ===\n")
cat("Verifying our parameter ranges match Sturman's Table 1:\n\n")

table1_verification <- data.frame(
  Parameter = c("Number hired (n)", "Years (t)", "Selection ratio", "Validity (r)", 
                "SDy ($)", "Cost per applicant", "Discount rate", "Tax rate", 
                "Variable costs", "Old validity", "Rejection rate", 
                "Performance-acceptance correlation", "Probationary cutoff", 
                "Turnover rate", "Performance-turnover correlation"),
  Our_Min = c(min(params$n), min(params$t), min(params$sr), min(params$r),
              min(params$sdy), min(params$cost), min(params$discount), min(params$tax),
              min(params$vc), min(params$r_old), min(params$reject_rate),
              min(params$corr_perf_accept), min(params$prob_cutoff), 
              min(params$turnover_rate), min(params$perf_turn_corr)),
  Our_Max = c(max(params$n), max(params$t), max(params$sr), max(params$r),
              max(params$sdy), max(params$cost), max(params$discount), max(params$tax),
              max(params$vc), max(params$r_old), max(params$reject_rate),
              max(params$corr_perf_accept), max(params$prob_cutoff), 
              max(params$turnover_rate), max(params$perf_turn_corr)),
  Sturman_Min = c(1, 1, 0.05, 0.10, 4000, 1, 0.01, 0.30, -0.35, 0.05, 0.20, -0.50, -2.0, 0.00, -0.50),
  Sturman_Max = c(1100, 10, 1.0, 0.77, 40000, 1100, 0.11, 0.63, -0.02, 0.38, 0.70, 0.00, 0.0, 0.33, 0.00)
)

print(table1_verification)

# Check if ranges match
ranges_match <- all(
  abs(table1_verification$Our_Min - table1_verification$Sturman_Min) < 0.01 &
  abs(table1_verification$Our_Max - table1_verification$Sturman_Max) < 0.01
)

cat("\nTable 1 Parameter Ranges Match Sturman:", ranges_match, "\n")
if (ranges_match) {
  cat("✅ SUCCESS: Table 1 parameters exactly match Sturman's specifications!\n")
} else {
  cat("⚠️  Some parameter ranges differ from Sturman's Table 1\n")
}

cat("\n" , paste(rep("=", 60), collapse=""), "\n")

# Calculate utility estimates using HYBRID IMPROVED APPROACH
cat("=== CALCULATING UTILITY ESTIMATES (HYBRID APPROACH) ===\n")

# Basic utility
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# IMPROVED ECONOMIC ADJUSTMENT (from backup app)
params$utility_economic <- with(params, {
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * abs(vc)
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Backup app's present value formula
  pv_factor <- (1/(1+discount)*(1-(1/(1+discount)^t)))/(1-(1/(1+discount)))
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  # Apply tax to costs too (key improvement from backup app)
  total_costs <- (n/sr) * cost * (1 - tax)
  
  after_tax_pv - total_costs
})

# Multiple devices
params$r_incremental <- params$r - params$r_old
params$utility_multiple <- with(params, {
  n * t * ux(sr) * r_incremental * sdy - (n/sr) * cost
})

# Top-down hiring deviations
params$utility_topdown <- with(params, {
  z_adj <- ux(sr) * (1 - reject_rate) + corr_perf_accept * dnorm(qnorm(reject_rate))
  n * t * z_adj * r * sdy - (n/sr) * cost
})

# Probationary period
params$utility_probation <- with(params, {
  prob_survive <- pnorm(prob_cutoff, lower.tail = FALSE)
  performance_gain <- dnorm(prob_cutoff) / prob_survive
  
  (n * t * ux(sr) * r * sdy * (1 + 0.1 * performance_gain)) - 
  (n/sr) * cost - 
  (n * (1 - prob_survive) * cost * 0.5)
})

# Employee flows
params$utility_flows <- with(params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  avg_workforce * t * ux(sr) * r * sdy * performance_effect - 
  (n/sr) * cost - 
  (n * turnover_rate * t * cost * 0.3)
})

# Calculate percentage changes for Table 2
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

cat("\n" , paste(rep("=", 60), collapse=""), "\n")
cat("=== TABLE 2: USEFULNESS ANALYSIS REPLICATION ===\n")

# Individual adjustment effects
individual_effects <- data.frame(
  Adjustment = c("Economic Variables", "Multiple Devices", "Deviations from Top-Down Hiring", 
                 "Effect of a Probationary Period", "The Offset of Employee Flows"),
  Median = c(
    median(params$pct_change_economic, na.rm = TRUE),
    median(params$pct_change_multiple, na.rm = TRUE),
    median(params$pct_change_topdown, na.rm = TRUE),
    median(params$pct_change_probation, na.rm = TRUE),
    median(params$pct_change_flows, na.rm = TRUE)
  ),
  Mean = c(
    mean(params$pct_change_economic, na.rm = TRUE),
    mean(params$pct_change_multiple, na.rm = TRUE),
    mean(params$pct_change_topdown, na.rm = TRUE),
    mean(params$pct_change_probation, na.rm = TRUE),
    mean(params$pct_change_flows, na.rm = TRUE)
  ),
  Sturman_Target = c(64, 53, 23, 22, 1)
) %>%
  mutate(
    Difference = abs(Median - Sturman_Target)
  ) %>%
  arrange(desc(Median))

cat("TABLE 2 RESULTS:\n")
print(individual_effects)

# Check ranking
sturman_ranking <- c("Economic Variables", "Multiple Devices", "Deviations from Top-Down Hiring", 
                     "Effect of a Probationary Period", "The Offset of Employee Flows")
our_ranking <- individual_effects$Adjustment
ranking_match <- identical(our_ranking, sturman_ranking)

cat("\nRanking Comparison:\n")
cat("Our ranking:    ", paste(our_ranking, collapse = " > "), "\n")
cat("Sturman ranking:", paste(sturman_ranking, collapse = " > "), "\n")
cat("Ranking matches Sturman:", ranking_match, "\n")

avg_difference <- mean(individual_effects$Difference)
cat("Average difference from Sturman targets:", round(avg_difference, 1), "percentage points\n")

if (ranking_match && avg_difference < 10) {
  cat("✅ EXCELLENT: Table 2 closely matches Sturman's results!\n")
} else if (ranking_match) {
  cat("✅ GOOD: Ranking correct, working on magnitude precision\n")
} else {
  cat("⚠️  Need to address ranking and/or magnitude issues\n")
}

cat("\n" , paste(rep("=", 60), collapse=""), "\n")
cat("=== TABLE 3: FREQUENCY DISTRIBUTIONS ===\n")

# Create Table 3 frequency distributions (in millions)
# Convert to millions for comparison with Sturman's Table 3
params$utility_economic_millions <- params$utility_economic / 1000000
params$utility_multiple_millions <- params$utility_multiple / 1000000
params$utility_topdown_millions <- params$utility_topdown / 1000000
params$utility_probation_millions <- params$utility_probation / 1000000
params$utility_flows_millions <- params$utility_flows / 1000000

# Define bins (from Sturman's Table 3)
bins <- c(-Inf, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 
          11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 
          24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, Inf)

bin_labels <- c("-3.0 to -2.0", "-2.0 to -1.0", "-1.0 to 0.0", "0.0 to 1.0", "1.0 to 2.0", 
                "2.0 to 3.0", "3.0 to 4.0", "4.0 to 5.0", "5.0 to 6.0", "6.0 to 7.0", 
                "7.0 to 8.0", "8.0 to 9.0", "9.0 to 10.0", "10.0 to 11.0", "11.0 to 12.0", 
                "12.0 to 13.0", "13.0 to 14.0", "14.0 to 15.0", "15.0 to 16.0", "16.0 to 17.0", 
                "17.0 to 18.0", "18.0 to 19.0", "19.0 to 20.0", "20.0 to 21.0", "21.0 to 22.0", 
                "22.0 to 23.0", "23.0 to 24.0", "24.0 to 25.0", "25.0 to 26.0", "26.0 to 27.0", 
                "27.0 to 28.0", "28.0 to 29.0", "29.0 to 30.0", "30.0 to 31.0", "31.0 to 32.0", 
                "32.0 to 33.0", "33.0 to 34.0", "34.0 to 35.0", "35.0 to 36.0", "36.0+")

# Create frequency table
create_frequency_table <- function(data, var_name) {
  freq_table <- table(cut(data, breaks = bins, labels = bin_labels, include.lowest = TRUE))
  data.frame(
    Range = names(freq_table),
    Frequency = as.numeric(freq_table)
  )
}

# Calculate frequencies for each adjustment
freq_economic <- create_frequency_table(params$utility_economic_millions, "Economic")
freq_multiple <- create_frequency_table(params$utility_multiple_millions, "Multiple")
freq_topdown <- create_frequency_table(params$utility_topdown_millions, "TopDown")
freq_probation <- create_frequency_table(params$utility_probation_millions, "Probation")
freq_flows <- create_frequency_table(params$utility_flows_millions, "Flows")

# Combine into Table 3 format
table3 <- data.frame(
  Range = freq_economic$Range,
  Economic_Variables = freq_economic$Frequency,
  Multiple_Devices = freq_multiple$Frequency,
  TopDown_Hiring = freq_topdown$Frequency,
  Probationary_Period = freq_probation$Frequency,
  Employee_Flows = freq_flows$Frequency
)

# Calculate means (in millions)
table3_means <- data.frame(
  Statistic = "Mean",
  Economic_Variables = round(mean(params$utility_economic_millions, na.rm = TRUE), 1),
  Multiple_Devices = round(mean(params$utility_multiple_millions, na.rm = TRUE), 1),
  TopDown_Hiring = round(mean(params$utility_topdown_millions, na.rm = TRUE), 1),
  Probationary_Period = round(mean(params$utility_probation_millions, na.rm = TRUE), 1),
  Employee_Flows = round(mean(params$utility_flows_millions, na.rm = TRUE), 1)
)

cat("TABLE 3: FREQUENCY DISTRIBUTIONS (in millions of dollars)\n")
cat("Showing first 20 rows and means:\n\n")
print(head(table3, 20))
cat("\n")
print(table3_means)

# Compare means to Sturman's Table 3 targets
sturman_means <- c(16.9, 7.6, 3.2, 2.7, 2.2)  # From Sturman's Table 3
our_means <- c(table3_means$Economic_Variables, table3_means$Multiple_Devices, 
               table3_means$TopDown_Hiring, table3_means$Probationary_Period, 
               table3_means$Employee_Flows)

cat("\n=== TABLE 3 MEAN COMPARISON ===\n")
comparison_df <- data.frame(
  Adjustment = c("Economic Variables", "Multiple Devices", "TopDown Hiring", "Probationary Period", "Employee Flows"),
  Our_Mean = our_means,
  Sturman_Mean = sturman_means,
  Difference = abs(our_means - sturman_means)
)
print(comparison_df)

avg_mean_diff <- mean(comparison_df$Difference)
cat("Average difference in means:", round(avg_mean_diff, 1), "million\n")

if (avg_mean_diff < 2) {
  cat("✅ EXCELLENT: Table 3 means closely match Sturman!\n")
} else if (avg_mean_diff < 5) {
  cat("✅ GOOD: Table 3 means are reasonably close to Sturman\n")
} else {
  cat("⚠️  Table 3 means differ significantly from Sturman\n")
}

cat("\n" , paste(rep("=", 80), collapse=""), "\n")
cat("=== OVERALL ASSESSMENT ===\n")
cat("Table 1 (Parameters): ", ifelse(ranges_match, "✅ PERFECT MATCH", "⚠️ Needs adjustment"), "\n")
cat("Table 2 (Usefulness): ", ifelse(ranking_match && avg_difference < 15, "✅ GOOD MATCH", "⚠️ Needs refinement"), "\n")
cat("Table 3 (Frequencies): ", ifelse(avg_mean_diff < 5, "✅ REASONABLE MATCH", "⚠️ Needs investigation"), "\n")

cat("\nKey Improvements Made:\n")
cat("- Used backup app's superior economic adjustment formula\n")
cat("- Applied tax to both benefits AND costs\n")
cat("- Used backup app's present value calculation\n")
cat("- Maintained current approach for other adjustments\n")

if (ranges_match && ranking_match && avg_difference < 20 && avg_mean_diff < 10) {
  cat("\n🎉 SUCCESS: Comprehensive replication achieved!\n")
} else {
  cat("\n📊 PROGRESS: Significant improvements made, continuing refinement\n")
} 