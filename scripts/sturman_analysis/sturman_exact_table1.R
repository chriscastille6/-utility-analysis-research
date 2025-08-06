# STURMAN (2000) EXACT IMPLEMENTATION - Using Table 1 Parameter Ranges
# This should match Sturman's 291% and 96% targets exactly

library(dplyr)
set.seed(42)

cat("STURMAN (2000) EXACT IMPLEMENTATION - TABLE 1 RANGES\n")
cat("===================================================\n\n")

# EXACT PARAMETER RANGES FROM TABLE 1
cat("Using Sturman's exact Table 1 parameter ranges:\n")
cat("1. Number of years: 1 to 10\n")
cat("2. Number of people selected: 1 to 1100 (exponential)\n") 
cat("3. Applicants per hire: 1 to 50\n")
cat("4. SDy: $4,000 to $40,000\n")
cat("5. Cost per applicant: $0 to $1100 (exponential)\n")
cat("6. Validity of new battery: 0.05 to 0.38 above old (0.10 to 0.77)\n")
cat("7. Discount rate: 0.01 to 0.11\n")
cat("8. Tax rate: 0.30 to 0.63\n")
cat("9. Variable cost rate: -0.02 to -0.35\n")
cat("10. Probability of turnover: 0.00 to 0.33\n")
cat("11. Correlation performance-turnover: 0.00 to -0.50\n")
cat("12. Stability (correlation) performance over time: 0.50 to 1.00\n")
cat("13. Cutoff probationary period: 0 to -2\n")
cat("14. Validity of old battery: 0.05 to 0.38\n")
cat("15. Percent accepting initial offer: 20% to 70%\n")
cat("16. Correlation probability accepting and performance: 0.00 to -0.50\n\n")

n_sims <- 10000

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Generate parameters using EXACT Table 1 ranges
params <- data.frame(
  # Basic parameters
  t = runif(n_sims, 1, 10),                                    # Years
  n = round(exp(runif(n_sims, log(1), log(1100)))),          # People selected (exponential)
  applicants_per_hire = runif(n_sims, 1, 50),                # Applicants per hire
  sdy = runif(n_sims, 4000, 40000),                          # SDy
  cost = exp(runif(n_sims, log(0.01), log(1100))),           # Cost per applicant (exponential, avoid 0)
  
  # Validity - "0.05 to 0.38 above the old (from 0.10 to 0.77)"
  r_old = runif(n_sims, 0.05, 0.38),                         # Old battery validity
  r_increment = runif(n_sims, 0.05, 0.38),                   # Increment above old
  
  # Economic adjustments
  discount = runif(n_sims, 0.01, 0.11),                      # Discount rate
  tax = runif(n_sims, 0.30, 0.63),                           # Tax rate
  vc = runif(n_sims, -0.35, -0.02),                          # Variable cost rate (negative!)
  
  # Employee flows
  prob_turnover = runif(n_sims, 0.00, 0.33),                 # Probability of turnover
  corr_perf_turnover = runif(n_sims, -0.50, 0.00),          # Correlation perf-turnover
  
  # Temporal validity
  stability_corr = runif(n_sims, 0.50, 1.00),               # Performance stability over time
  
  # Probationary period
  prob_cutoff = runif(n_sims, -2, 0),                       # Cutoff (0 to -2, but as z-score)
  
  # Top-down hiring deviations
  pct_accepting = runif(n_sims, 0.20, 0.70),                # Percent accepting offers
  corr_accept_perf = runif(n_sims, -0.50, 0.00)             # Correlation accept-performance
)

# Calculate derived parameters
params$r_new <- params$r_old + params$r_increment              # New battery validity
params$sr <- 1 / params$applicants_per_hire                   # Selection ratio
params$reject_rate <- 1 - params$pct_accepting                # Rejection rate
params$turnover_rate <- params$prob_turnover                  # Annual turnover rate
params$perf_turn_corr <- params$corr_perf_turnover           # Performance-turnover correlation
params$corr_perf_accept <- params$corr_accept_perf            # Performance-acceptance correlation

cat("Calculating utility estimates for", n_sims, "scenarios...\n")

# ANALYSIS 1: GENERAL USEFULNESS ANALYSIS
cat("\nANALYSIS 1: GENERAL USEFULNESS ANALYSIS\n")
cat("=======================================\n")

# Basic utility
params$utility_basic <- with(params, {
  n * t * ux(sr) * r_new * sdy - (n/sr) * cost
})

# All adjustments combined
params$utility_all_adj <- with(params, {
  # Multiple devices: use incremental validity
  incremental_r <- r_new - r_old
  
  # Top-down hiring adjustment
  p_accept <- pct_accepting
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  
  # Probationary period adjustment
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  # Employee flows adjustment
  effective_n <- n * prob_retained
  avg_workforce <- effective_n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  # Economic adjustments
  annual_benefit <- avg_workforce * incremental_r * z_adj * sdy * performance_effect
  
  # Variable costs (NOTE: vc is negative, so this INCREASES costs)
  annual_variable_costs <- annual_benefit * abs(vc)  # Use absolute value since vc is negative
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  # Costs
  initial_costs <- (n/sr) * cost
  replacement_costs <- n * turnover_rate * t * cost * 0.3
  
  after_tax_pv - initial_costs - replacement_costs
})

# Calculate percentage changes
calc_pct_change <- function(basic, adjusted) {
  pct_change <- 100 * (basic - adjusted) / abs(basic)
  pct_change[is.finite(pct_change)]
}

general_reductions <- calc_pct_change(params$utility_basic, params$utility_all_adj)
general_median <- median(general_reductions, na.rm = TRUE)
general_mean <- mean(general_reductions, na.rm = TRUE)
general_negative <- sum(params$utility_all_adj < 0, na.rm = TRUE) / nrow(params) * 100

cat("Results:\n")
cat("- Median reduction:", round(general_median, 1), "% (Target: 291%)\n")
cat("- Mean reduction:", round(general_mean, 1), "% (Target: 298%)\n")
cat("- Negative cases:", round(general_negative, 1), "% (Target: 16%)\n")
cat("- Difference from target:", round(abs(general_median - 291), 1), "percentage points\n")

# ANALYSIS 2: LATHAM & WHYTE CASE STUDY
cat("\nANALYSIS 2: LATHAM & WHYTE CASE STUDY\n")
cat("=====================================\n")

# L&W parameters (using same adjustment parameters as general analysis)
lw_params <- params
lw_params$n <- 18
lw_params$t <- 5  # Years
lw_params$sr <- 18/470
lw_params$r_new <- 0.40
lw_params$sdy <- 16290
lw_params$cost <- 429110/470

cat("L&W fixed parameters:\n")
cat("- Hired (n):", unique(lw_params$n), "\n")
cat("- Years (t):", unique(lw_params$t), "\n")
cat("- Selection ratio (sr):", round(unique(lw_params$sr), 3), "\n")
cat("- Validity (r_new):", unique(lw_params$r_new), "\n")
cat("- SDy:", unique(lw_params$sdy), "\n")
cat("- Cost per applicant:", round(unique(lw_params$cost), 0), "\n")

# Recalculate with L&W base parameters
lw_params$utility_basic <- with(lw_params, {
  n * t * ux(sr) * r_new * sdy - (n/sr) * cost
})

lw_params$utility_all_adj <- with(lw_params, {
  # Multiple devices: use incremental validity
  incremental_r <- r_new - r_old
  
  # Top-down hiring adjustment
  p_accept <- pct_accepting
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  
  # Probationary period adjustment
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  # Employee flows adjustment
  effective_n <- n * prob_retained
  avg_workforce <- effective_n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  # Economic adjustments
  annual_benefit <- avg_workforce * incremental_r * z_adj * sdy * performance_effect
  
  # Variable costs (NOTE: vc is negative, so this INCREASES costs)
  annual_variable_costs <- annual_benefit * abs(vc)  # Use absolute value since vc is negative
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  
  # Costs
  initial_costs <- (n/sr) * cost
  replacement_costs <- n * turnover_rate * t * cost * 0.3
  
  after_tax_pv - initial_costs - replacement_costs
})

lw_reductions <- calc_pct_change(lw_params$utility_basic, lw_params$utility_all_adj)
lw_median <- median(lw_reductions, na.rm = TRUE)
lw_mean <- mean(lw_reductions, na.rm = TRUE)

cat("\nResults:\n")
cat("- Median reduction:", round(lw_median, 1), "% (Target: 96%)\n")
cat("- Mean reduction:", round(lw_mean, 1), "%\n")
cat("- Difference from target:", round(abs(lw_median - 96), 1), "percentage points\n")

# FINAL SUMMARY
cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("FINAL RESULTS WITH EXACT TABLE 1 RANGES\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("GENERAL USEFULNESS ANALYSIS:\n")
cat("  Our result:", round(general_median, 1), "% vs Target: 291%\n")
cat("  Difference:", round(abs(general_median - 291), 1), "percentage points\n")
cat("  Mean result:", round(general_mean, 1), "% vs Target: 298%\n")
cat("  Negative cases:", round(general_negative, 1), "% vs Target: 16%\n")

cat("\nLATHAM & WHYTE CASE STUDY:\n")
cat("  Our result:", round(lw_median, 1), "% vs Target: 96%\n")
cat("  Difference:", round(abs(lw_median - 96), 1), "percentage points\n")

if(abs(general_median - 291) < 20) {
  cat("\n🎯 GENERAL USEFULNESS ANALYSIS: EXCELLENT MATCH!\n")
} else if(abs(general_median - 291) < 50) {
  cat("\n✅ GENERAL USEFULNESS ANALYSIS: GOOD MATCH!\n")
} else {
  cat("\n⚠️  GENERAL USEFULNESS ANALYSIS: Still needs refinement\n")
}

if(abs(lw_median - 96) < 10) {
  cat("🎯 L&W CASE STUDY: EXCELLENT MATCH!\n")
} else if(abs(lw_median - 96) < 20) {
  cat("✅ L&W CASE STUDY: GOOD MATCH!\n")
} else {
  cat("⚠️  L&W CASE STUDY: Still needs refinement\n")
}

cat("\n📊 PARAMETER DIAGNOSTICS:\n")
cat("Basic parameter ranges (sample):\n")
sample_params <- params[1:5, c("t", "n", "sr", "r_new", "sdy", "cost")]
print(sample_params)

cat("\nEconomic parameter ranges (sample):\n")
sample_econ <- params[1:5, c("discount", "tax", "vc")]
print(sample_econ)

cat("\nNOTE: Using exact Table 1 ranges should significantly improve accuracy!\n") 