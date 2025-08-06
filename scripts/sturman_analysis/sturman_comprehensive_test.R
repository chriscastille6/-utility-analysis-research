# COMPREHENSIVE STURMAN (2000) VERIFICATION
# Based on extracted PDF information and your insights

library(dplyr)
set.seed(42)  # For reproducibility

cat("COMPREHENSIVE STURMAN (2000) VERIFICATION\n")
cat("=========================================\n\n")

# KEY INSIGHTS FROM YOUR ANALYSIS:
# 1. Sturman did TWO separate analyses using the SAME 10,000 scenarios
# 2. General Usefulness Analysis: 10,000 random scenarios → 291% median reduction  
# 3. L&W Case Study: Same scenarios + L&W fixed parameters → 96% reduction
# 4. Parameter ranges from Table 1 (need to be extracted more precisely)
# 5. All parameters uniform except n and cost (exponential)
# 6. Strategy B (cumulative effects) appears correct based on text

n_sims <- 10000

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

cat("STEP 1: GENERAL USEFULNESS ANALYSIS (10,000 random scenarios)\n")
cat("=============================================================\n")

# Generate parameters for general usefulness analysis
# Based on extracted information and typical UA ranges
general_params <- data.frame(
  # Basic parameters - exponential for n and cost, uniform for others
  n = round(exp(runif(n_sims, log(1), log(1100)))),     # Confirmed: 1-1100, exponential
  t = runif(n_sims, 1, 10),                             # Years (typical range)
  sr = runif(n_sims, 0.05, 0.95),                       # Selection ratio
  r = runif(n_sims, 0.10, 0.70),                        # Validity (wide range)
  sdy = runif(n_sims, 5000, 50000),                     # SDy in dollars
  cost = exp(runif(n_sims, log(10), log(2000))),        # Cost per applicant, exponential
  
  # Economic adjustment parameters
  discount = runif(n_sims, 0.05, 0.20),                 # Discount rate
  tax = runif(n_sims, 0.15, 0.45),                      # Tax rate  
  vc = runif(n_sims, 0.05, 0.35),                       # Variable costs
  
  # Multiple devices parameters
  r_old = runif(n_sims, 0.05, 0.38),                    # Confirmed from L&W analysis
  
  # Other adjustment parameters (estimated ranges)
  reject_rate = runif(n_sims, 0.05, 0.50),              # Job offer rejection rate
  corr_perf_accept = runif(n_sims, -0.40, -0.05),       # Correlation performance-acceptance
  prob_cutoff = runif(n_sims, -2.0, -0.25),             # Probationary cutoff (z-score)
  turnover_rate = runif(n_sims, 0.02, 0.30),            # Annual turnover rate
  perf_turn_corr = runif(n_sims, -0.40, -0.05)          # Performance-turnover correlation
)

cat("Calculating utility estimates for general usefulness analysis...\n")

# Basic utility (baseline)
general_params$utility_basic <- with(general_params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Individual adjustments
general_params$utility_economic <- with(general_params, {
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

general_params$utility_multiple <- with(general_params, {
  incremental_r <- r - r_old
  n * t * ux(sr) * incremental_r * sdy - (n/sr) * cost
})

general_params$utility_topdown <- with(general_params, {
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  n * t * z_adj * r * sdy - (n/sr) * cost
})

general_params$utility_probation <- with(general_params, {
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  effective_n <- n * prob_retained
  effective_n * t * ux(sr) * r * sdy - (n/sr) * cost
})

general_params$utility_flows <- with(general_params, {
  avg_workforce <- n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  (avg_workforce * t * ux(sr) * r * sdy * performance_effect) - 
  (n/sr) * cost - (n * turnover_rate * t * cost * 0.3)
})

# Sequential combinations (Strategy B: Cumulative effects)
general_params$step1_economic <- general_params$utility_economic

general_params$step2_econ_mult <- with(general_params, {
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

general_params$step3_plus_topdown <- with(general_params, {
  incremental_r <- r - r_old
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  
  annual_benefit <- n * incremental_r * z_adj * sdy
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

general_params$step4_plus_probation <- with(general_params, {
  incremental_r <- r - r_old
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  effective_n <- n * prob_retained
  annual_benefit <- effective_n * incremental_r * z_adj * sdy
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

general_params$step5_all <- with(general_params, {
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

# Calculate percentage changes
calc_pct_change <- function(basic, adjusted) {
  pct_change <- 100 * (basic - adjusted) / abs(basic)
  pct_change[is.finite(pct_change)]
}

# General usefulness results
general_final <- median(calc_pct_change(general_params$utility_basic, general_params$step5_all), na.rm = TRUE)
general_mean <- mean(calc_pct_change(general_params$utility_basic, general_params$step5_all), na.rm = TRUE)
general_negative <- sum(general_params$step5_all < 0, na.rm = TRUE) / nrow(general_params) * 100

cat("GENERAL USEFULNESS ANALYSIS RESULTS:\n")
cat("Median reduction:", round(general_final, 1), "% (Target: 291%)\n")
cat("Mean reduction:", round(general_mean, 1), "% (Target: 298%)\n")
cat("Negative cases:", round(general_negative, 1), "% (Target: 16%)\n")

cat("\nSTEP 2: LATHAM & WHYTE CASE STUDY (Same scenarios + L&W parameters)\n")
cat("==================================================================\n")

# L&W fixed parameters (from Sturman's description)
lw_n <- 18
lw_t <- 470  # This seems wrong - should be years, not applicants
lw_sr <- 18/470  # 18 hired from 470 applicants
lw_r <- 0.40
lw_sdy <- 16290
lw_cost <- 429110/470  # Total cost divided by applicants

# Wait - let me check this. The formula suggests t should be years, not applicants
# Let me use reasonable L&W values
lw_t_years <- 5  # More reasonable for years
lw_applicants <- 470
lw_hired <- 18
lw_sr_calc <- lw_hired / lw_applicants

cat("Using L&W parameters:\n")
cat("- Hired (n):", lw_hired, "\n")
cat("- Years (t):", lw_t_years, "\n") 
cat("- Selection ratio (sr):", round(lw_sr_calc, 3), "\n")
cat("- Validity (r):", lw_r, "\n")
cat("- SDy:", lw_sdy, "\n")
cat("- Cost per applicant:", round(lw_cost, 0), "\n\n")

# Apply L&W case study using the SAME random adjustment parameters
lw_params <- general_params  # Use same adjustment parameters

# Override basic parameters with L&W values
lw_params$n <- lw_hired
lw_params$t <- lw_t_years
lw_params$sr <- lw_sr_calc
lw_params$r <- lw_r
lw_params$sdy <- lw_sdy
lw_params$cost <- lw_cost

# Recalculate with L&W parameters
lw_params$utility_basic <- with(lw_params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Recalculate all adjustments with L&W base parameters
lw_params$step5_all <- with(lw_params, {
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

# L&W case study results
lw_final <- median(calc_pct_change(lw_params$utility_basic, lw_params$step5_all), na.rm = TRUE)
lw_mean <- mean(calc_pct_change(lw_params$utility_basic, lw_params$step5_all), na.rm = TRUE)

cat("LATHAM & WHYTE CASE STUDY RESULTS:\n")
cat("Median reduction:", round(lw_final, 1), "% (Target: 96%)\n")
cat("Mean reduction:", round(lw_mean, 1), "%\n")

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("FINAL COMPARISON TO STURMAN'S TARGETS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("GENERAL USEFULNESS ANALYSIS:\n")
cat("  Our result:", round(general_final, 1), "% vs Target: 291%\n")
cat("  Difference:", round(abs(general_final - 291), 1), "percentage points\n")
cat("  Mean result:", round(general_mean, 1), "% vs Target: 298%\n")
cat("  Negative cases:", round(general_negative, 1), "% vs Target: 16%\n")

cat("\nLATHAM & WHYTE CASE STUDY:\n")
cat("  Our result:", round(lw_final, 1), "% vs Target: 96%\n")
cat("  Difference:", round(abs(lw_final - 96), 1), "percentage points\n")

if(abs(lw_final - 96) < 10) {
  cat("\n✅ L&W case study VERIFIED - within 10 percentage points\n")
} else {
  cat("\n❌ L&W case study needs adjustment\n")
}

if(abs(general_final - 291) < 50) {
  cat("✅ General usefulness analysis CLOSE - within 50 percentage points\n")
} else {
  cat("❌ General usefulness analysis needs significant adjustment\n")
}

cat("\nNEXT STEPS TO IMPROVE ACCURACY:\n")
cat("1. Extract exact parameter ranges from Table 1\n")
cat("2. Verify L&W parameter values (especially time period)\n")
cat("3. Test different parameter range combinations\n")
cat("4. Check for missing interaction effects\n")
cat("5. Verify economic adjustment formulas\n") 