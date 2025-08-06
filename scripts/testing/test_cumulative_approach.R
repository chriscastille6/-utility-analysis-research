# Test Cumulative Approach - Sequential Application of Adjustments
# This matches Sturman's usefulness analysis methodology

library(dplyr)

set.seed(42)
n_sims <- 10000

cat("Testing Cumulative Sequential Adjustment Approach...\n")
cat("This matches Sturman's usefulness analysis methodology\n\n")

# Generate parameters
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
  
  # Other adjustments
  reject_rate = runif(n_sims, 0.10, 0.40),
  corr_perf_accept = runif(n_sims, -0.3, -0.1),
  prob_cutoff = runif(n_sims, -1.5, -0.5),
  turnover_rate = runif(n_sims, 0.05, 0.25),
  perf_turn_corr = runif(n_sims, -0.3, -0.1)
)

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Step 1: Basic utility
params$utility_step0 <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Step 2: Add Economic adjustments (largest impact first)
params$utility_step1 <- with(params, {
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

# Step 3: Add Multiple devices (second largest impact)
params$utility_step2 <- with(params, {
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

# Step 4: Add Top-down hiring deviations
params$utility_step3 <- with(params, {
  incremental_r <- r - r_old
  hiring_efficiency <- 1 - (reject_rate * 0.5)
  
  annual_benefit <- n * incremental_r * ux(sr) * sdy * hiring_efficiency
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

# Step 5: Add Probationary period
params$utility_step4 <- with(params, {
  incremental_r <- r - r_old
  hiring_efficiency <- 1 - (reject_rate * 0.5)
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  effective_n <- n * prob_retained
  annual_benefit <- effective_n * incremental_r * ux(sr) * sdy * hiring_efficiency
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

# Step 6: Add Employee flows (final adjustment)
params$utility_step5 <- with(params, {
  incremental_r <- r - r_old
  hiring_efficiency <- 1 - (reject_rate * 0.5)
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  flow_factor <- 1 - (turnover_rate * 0.5)
  
  effective_n <- n * prob_retained * flow_factor
  annual_benefit <- effective_n * incremental_r * ux(sr) * sdy * hiring_efficiency
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

# Calculate cumulative percentage changes
calc_pct_change <- function(basic, adjusted) {
  change <- 100 * (basic - adjusted) / abs(basic)
  change[is.finite(change)]
}

pct_change_step1 <- calc_pct_change(params$utility_step0, params$utility_step1)
pct_change_step2 <- calc_pct_change(params$utility_step0, params$utility_step2)
pct_change_step3 <- calc_pct_change(params$utility_step0, params$utility_step3)
pct_change_step4 <- calc_pct_change(params$utility_step0, params$utility_step4)
pct_change_step5 <- calc_pct_change(params$utility_step0, params$utility_step5)

# Results
cat("CUMULATIVE SEQUENTIAL RESULTS:\n")
cat(paste(rep("=", 50), collapse=""), "\n")

results <- data.frame(
  Step = c("1. Economic Only", "2. + Multiple Devices", "3. + Top-Down Hiring", 
           "4. + Probationary Period", "5. + Employee Flows"),
  Median_Reduction = c(
    median(pct_change_step1, na.rm = TRUE),
    median(pct_change_step2, na.rm = TRUE),
    median(pct_change_step3, na.rm = TRUE),
    median(pct_change_step4, na.rm = TRUE),
    median(pct_change_step5, na.rm = TRUE)
  ),
  Mean_Reduction = c(
    mean(pct_change_step1, na.rm = TRUE),
    mean(pct_change_step2, na.rm = TRUE),
    mean(pct_change_step3, na.rm = TRUE),
    mean(pct_change_step4, na.rm = TRUE),
    mean(pct_change_step5, na.rm = TRUE)
  )
)

print(results %>% 
  mutate(
    Median_Reduction = round(Median_Reduction, 1),
    Mean_Reduction = round(Mean_Reduction, 1)
  ))

cat("\nFINAL CUMULATIVE RESULT (All 5 adjustments):\n")
cat("Median reduction:", round(median(pct_change_step5, na.rm = TRUE), 1), "%\n")
cat("Mean reduction:", round(mean(pct_change_step5, na.rm = TRUE), 1), "%\n")
cat("Negative cases:", round(sum(params$utility_step5 < 0, na.rm = TRUE) / nrow(params) * 100, 1), "%\n")

cat("\nSTURMAN'S TARGETS:\n")
cat("Median reduction: 291%\n")
cat("Mean reduction: 298%\n")
cat("Negative cases: 16%\n")

# Check if we're close
final_median <- median(pct_change_step5, na.rm = TRUE)
if(abs(final_median - 291) < 50) {
  cat("\n✓ REASONABLY CLOSE to Sturman's target!\n")
} else {
  cat("\n⚠ Still not matching Sturman's target. Difference:", round(abs(final_median - 291), 1), "percentage points\n")
} 