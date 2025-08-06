# ============================================================================
# STURMAN ET AL. (2003) COMPREHENSIVE REPLICATION ANALYSIS
# "Is it worth it to win the Talent War? Evaluating the Utility of Performance-Based Pay"
# ============================================================================

library(dplyr)
library(ggplot2)
library(knitr)

cat("STURMAN ET AL. (2003) COMPREHENSIVE REPLICATION ANALYSIS\n")
cat("========================================================\n")

# Study parameters
perf_rating <- seq(1, 5, by = 0.5)
n_per_perf_rating <- c(60, 97, 1171, 1090, 1667, 672, 317, 46, 23)
base_salary_2003 <- 47983

# Pay strategies
strat1_increases <- rep(0.04, length(perf_rating))
strat2_increases <- rep(0.04, length(perf_rating))
strat2_increases[perf_rating >= 3] <- 0.04 + 0.01 * (perf_rating[perf_rating >= 3] - 3) * 2
strat3_increases <- seq(0, 0.08, by = 0.01)

# Turnover probabilities
strat1_turnover <- c(0.96, 0.65, 0.38, 0.25, 0.21, 0.22, 0.27, 0.41, 0.66)
strat2_turnover <- c(0.96, 0.65, 0.38, 0.25, 0.21, 0.14, 0.11, 0.11, 0.14)
strat3_turnover <- c(0.99, 0.88, 0.60, 0.35, 0.21, 0.14, 0.11, 0.11, 0.14)

# Calculate retained employees
strat1_retained <- round((1 - strat1_turnover) * n_per_perf_rating)
strat2_retained <- round((1 - strat2_turnover) * n_per_perf_rating)
strat3_retained <- round((1 - strat3_turnover) * n_per_perf_rating)

# Calculate separations
total_separations <- c(
  sum(round(strat1_turnover * n_per_perf_rating)),
  sum(round(strat2_turnover * n_per_perf_rating)),
  sum(round(strat3_turnover * n_per_perf_rating))
)

# Calculate 2007 salaries
salary_2007_s1 <- base_salary_2003 * (1 + strat1_increases)^4
salary_2007_s2 <- base_salary_2003 * (1 + strat2_increases)^4
salary_2007_s3 <- base_salary_2003 * (1 + strat3_increases)^4

# Movement costs
movement_cost_multiplier <- 2.0
avg_salary_2007 <- c(
  weighted.mean(salary_2007_s1, strat1_retained),
  weighted.mean(salary_2007_s2, strat2_retained),
  weighted.mean(salary_2007_s3, strat3_retained)
)

movement_cost_2003 <- base_salary_2003 * movement_cost_multiplier
movement_costs_2007 <- avg_salary_2007 * movement_cost_multiplier
yearly_increase <- (movement_costs_2007 - movement_cost_2003) / 4
movement_cost_2004 <- movement_cost_2003 + yearly_increase
avg_movement_cost <- (movement_cost_2004 + movement_costs_2007) / 2
total_movement_costs <- avg_movement_cost * total_separations

# Service costs
service_cost_multiplier <- 1.37
service_cost_2003 <- base_salary_2003 * service_cost_multiplier
service_costs_2007 <- avg_salary_2007 * service_cost_multiplier
yearly_service_increase <- (service_costs_2007 - service_cost_2003) / 4
service_cost_2004 <- service_cost_2003 + yearly_service_increase
avg_service_cost <- (service_cost_2004 + service_costs_2007) / 2
total_service_costs <- 4 * sum(n_per_perf_rating) * avg_service_cost

# Service values
service_value_multiplier <- 1.754
avg_performance <- weighted.mean(perf_rating, n_per_perf_rating)
performance_sd <- sqrt(sum(n_per_perf_rating * (perf_rating - avg_performance)^2) / sum(n_per_perf_rating))
z_scores <- (perf_rating - avg_performance) / performance_sd

avg_service_value_2003 <- service_value_multiplier * base_salary_2003
avg_service_value_2007 <- service_value_multiplier * avg_salary_2007[1]

# Calculate net utilities for different SDy scenarios
sdy_scenarios <- c(0.30, 0.60, 0.90)
net_utilities <- array(0, dim = c(3, 3))

for(strategy in 1:3) {
  retained_employees <- list(strat1_retained, strat2_retained, strat3_retained)[[strategy]]
  
  for(sdy_idx in 1:3) {
    sdy_2007 <- sdy_scenarios[sdy_idx] * avg_salary_2007[1]
    
    # Service value from retained employees
    individual_values_2007 <- avg_service_value_2007 + (sdy_2007 * z_scores)
    retained_value <- sum(retained_employees * individual_values_2007)
    
    # Service value from replacements (assumed average)
    replacements <- sum(n_per_perf_rating - retained_employees)
    replacement_value <- replacements * avg_service_value_2007
    
    total_service_value <- retained_value + replacement_value
    
    # Net utility
    net_utilities[strategy, sdy_idx] <- total_service_value - 
                                       total_service_costs[strategy] - 
                                       total_movement_costs[strategy]
  }
}

# Results summary
cat("\nKEY RESULTS:\n")
cat("============\n")

cat("\nMovement Costs:\n")
for(i in 1:3) {
  cat(sprintf("Strategy %d: $%.0f\n", i, total_movement_costs[i]))
}

cat("\nService Costs:\n")
for(i in 1:3) {
  cat(sprintf("Strategy %d: $%.0f\n", i, total_service_costs[i]))
}

cat("\nNet Utilities by SDy Scenario:\n")
sdy_names <- c("30%", "60%", "90%")
for(sdy in 1:3) {
  cat(sprintf("\n%s SDy:\n", sdy_names[sdy]))
  for(strategy in 1:3) {
    cat(sprintf("  Strategy %d: $%.0f\n", strategy, net_utilities[strategy, sdy]))
  }
}

cat("\nUtility Differences from Strategy 1:\n")
for(sdy in 1:3) {
  cat(sprintf("\n%s SDy:\n", sdy_names[sdy]))
  for(strategy in 2:3) {
    diff <- net_utilities[strategy, sdy] - net_utilities[1, sdy]
    cat(sprintf("  Strategy %d vs 1: $%.0f\n", strategy, diff))
  }
}

# Save results
results <- list(
  net_utilities = net_utilities,
  movement_costs = total_movement_costs,
  service_costs = total_service_costs,
  separations = total_separations,
  avg_salaries = avg_salary_2007
)

save(results, file = "sturman_2003_results.RData")
cat("\nResults saved to sturman_2003_results.RData\n")
cat("\nREPLICATION COMPLETE ✓\n")
