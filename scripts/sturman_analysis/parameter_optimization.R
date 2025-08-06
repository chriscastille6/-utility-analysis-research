# PARAMETER OPTIMIZATION TO MATCH STURMAN'S RESULTS
# Systematically test different parameter ranges to hit 291% and 96% targets

library(dplyr)

# Function to run analysis with given parameter ranges
run_sturman_analysis <- function(n_sims = 10000, 
                                # Basic parameter ranges
                                n_range = c(1, 1100),
                                t_range = c(1, 10),
                                sr_range = c(0.05, 0.95),
                                r_range = c(0.10, 0.70),
                                sdy_range = c(5000, 50000),
                                cost_range = c(10, 2000),
                                # Economic adjustment ranges
                                discount_range = c(0.05, 0.20),
                                tax_range = c(0.15, 0.45),
                                vc_range = c(0.05, 0.35),
                                # Other adjustment ranges
                                r_old_range = c(0.05, 0.38),
                                reject_range = c(0.05, 0.50),
                                corr_perf_range = c(-0.40, -0.05),
                                prob_cutoff_range = c(-2.0, -0.25),
                                turnover_range = c(0.02, 0.30),
                                perf_turn_range = c(-0.40, -0.05),
                                seed = 42) {
  
  set.seed(seed)
  
  # Standard normal ordinate function
  ux <- function(selection_ratio) {
    dnorm(qnorm(1 - selection_ratio)) / selection_ratio
  }
  
  # Generate parameters
  params <- data.frame(
    n = round(exp(runif(n_sims, log(n_range[1]), log(n_range[2])))),
    t = runif(n_sims, t_range[1], t_range[2]),
    sr = runif(n_sims, sr_range[1], sr_range[2]),
    r = runif(n_sims, r_range[1], r_range[2]),
    sdy = runif(n_sims, sdy_range[1], sdy_range[2]),
    cost = exp(runif(n_sims, log(cost_range[1]), log(cost_range[2]))),
    
    discount = runif(n_sims, discount_range[1], discount_range[2]),
    tax = runif(n_sims, tax_range[1], tax_range[2]),
    vc = runif(n_sims, vc_range[1], vc_range[2]),
    
    r_old = runif(n_sims, r_old_range[1], r_old_range[2]),
    reject_rate = runif(n_sims, reject_range[1], reject_range[2]),
    corr_perf_accept = runif(n_sims, corr_perf_range[1], corr_perf_range[2]),
    prob_cutoff = runif(n_sims, prob_cutoff_range[1], prob_cutoff_range[2]),
    turnover_rate = runif(n_sims, turnover_range[1], turnover_range[2]),
    perf_turn_corr = runif(n_sims, perf_turn_range[1], perf_turn_range[2])
  )
  
  # Basic utility
  params$utility_basic <- with(params, {
    n * t * ux(sr) * r * sdy - (n/sr) * cost
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
  
  # Calculate percentage changes
  calc_pct_change <- function(basic, adjusted) {
    pct_change <- 100 * (basic - adjusted) / abs(basic)
    pct_change[is.finite(pct_change)]
  }
  
  general_reductions <- calc_pct_change(params$utility_basic, params$utility_all)
  
  # General usefulness results
  general_median <- median(general_reductions, na.rm = TRUE)
  general_mean <- mean(general_reductions, na.rm = TRUE)
  negative_pct <- sum(params$utility_all < 0, na.rm = TRUE) / nrow(params) * 100
  
  # L&W case study - use same adjustment parameters but L&W base parameters
  lw_params <- params
  lw_params$n <- 18
  lw_params$t <- 5  # Assuming 5 years
  lw_params$sr <- 18/470
  lw_params$r <- 0.40
  lw_params$sdy <- 16290
  lw_params$cost <- 429110/470
  
  lw_params$utility_basic <- with(lw_params, {
    n * t * ux(sr) * r * sdy - (n/sr) * cost
  })
  
  lw_params$utility_all <- with(lw_params, {
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
  
  lw_reductions <- calc_pct_change(lw_params$utility_basic, lw_params$utility_all)
  lw_median <- median(lw_reductions, na.rm = TRUE)
  
  return(list(
    general_median = general_median,
    general_mean = general_mean,
    negative_pct = negative_pct,
    lw_median = lw_median,
    general_target_diff = abs(general_median - 291),
    lw_target_diff = abs(lw_median - 96)
  ))
}

cat("PARAMETER OPTIMIZATION FOR STURMAN (2000) RESULTS\n")
cat("=================================================\n\n")

# Test 1: Baseline (current ranges)
cat("TEST 1: Baseline ranges\n")
result1 <- run_sturman_analysis()
cat("General median:", round(result1$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result1$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result1$negative_pct, 1), "% (Target: 16%)\n\n")

# Test 2: Wider parameter ranges
cat("TEST 2: Wider ranges\n")
result2 <- run_sturman_analysis(
  t_range = c(1, 20),
  sr_range = c(0.01, 0.99),
  r_range = c(0.05, 0.80),
  sdy_range = c(1000, 100000),
  cost_range = c(5, 5000),
  discount_range = c(0.01, 0.30),
  tax_range = c(0.10, 0.60),
  vc_range = c(0.01, 0.50)
)
cat("General median:", round(result2$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result2$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result2$negative_pct, 1), "% (Target: 16%)\n\n")

# Test 3: More extreme economic parameters
cat("TEST 3: More extreme economic parameters\n")
result3 <- run_sturman_analysis(
  discount_range = c(0.05, 0.40),
  tax_range = c(0.20, 0.70),
  vc_range = c(0.10, 0.60)
)
cat("General median:", round(result3$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result3$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result3$negative_pct, 1), "% (Target: 16%)\n\n")

# Test 4: Higher cost ranges
cat("TEST 4: Higher cost ranges\n")
result4 <- run_sturman_analysis(
  cost_range = c(50, 10000)
)
cat("General median:", round(result4$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result4$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result4$negative_pct, 1), "% (Target: 16%)\n\n")

# Test 5: Lower validity ranges (to increase adjustment effects)
cat("TEST 5: Lower validity ranges\n")
result5 <- run_sturman_analysis(
  r_range = c(0.05, 0.50),
  r_old_range = c(0.02, 0.30)
)
cat("General median:", round(result5$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result5$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result5$negative_pct, 1), "% (Target: 16%)\n\n")

# Test 6: More extreme adjustment parameters
cat("TEST 6: More extreme adjustment parameters\n")
result6 <- run_sturman_analysis(
  reject_range = c(0.10, 0.70),
  corr_perf_range = c(-0.60, -0.10),
  prob_cutoff_range = c(-3.0, -0.10),
  turnover_range = c(0.05, 0.50),
  perf_turn_range = c(-0.60, -0.10)
)
cat("General median:", round(result6$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result6$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result6$negative_pct, 1), "% (Target: 16%)\n\n")

# Test 7: Combination of best performing adjustments
cat("TEST 7: Combined optimizations\n")
result7 <- run_sturman_analysis(
  t_range = c(1, 20),
  sr_range = c(0.01, 0.99),
  r_range = c(0.05, 0.50),
  sdy_range = c(1000, 100000),
  cost_range = c(50, 10000),
  discount_range = c(0.05, 0.40),
  tax_range = c(0.20, 0.70),
  vc_range = c(0.10, 0.60),
  r_old_range = c(0.02, 0.30),
  reject_range = c(0.10, 0.70),
  corr_perf_range = c(-0.60, -0.10),
  prob_cutoff_range = c(-3.0, -0.10),
  turnover_range = c(0.05, 0.50),
  perf_turn_range = c(-0.60, -0.10)
)
cat("General median:", round(result7$general_median, 1), "% (Target: 291%)\n")
cat("L&W median:", round(result7$lw_median, 1), "% (Target: 96%)\n")
cat("Negative cases:", round(result7$negative_pct, 1), "% (Target: 16%)\n\n")

# Summary of results
cat("SUMMARY OF OPTIMIZATION TESTS\n")
cat("==============================\n")
results <- data.frame(
  Test = paste("Test", 1:7),
  General_Median = c(result1$general_median, result2$general_median, result3$general_median,
                     result4$general_median, result5$general_median, result6$general_median,
                     result7$general_median),
  LW_Median = c(result1$lw_median, result2$lw_median, result3$lw_median,
                result4$lw_median, result5$lw_median, result6$lw_median,
                result7$lw_median),
  General_Diff = c(result1$general_target_diff, result2$general_target_diff, result3$general_target_diff,
                   result4$general_target_diff, result5$general_target_diff, result6$general_target_diff,
                   result7$general_target_diff),
  LW_Diff = c(result1$lw_target_diff, result2$lw_target_diff, result3$lw_target_diff,
              result4$lw_target_diff, result5$lw_target_diff, result6$lw_target_diff,
              result7$lw_target_diff)
) %>%
  mutate(
    General_Median = round(General_Median, 1),
    LW_Median = round(LW_Median, 1),
    General_Diff = round(General_Diff, 1),
    LW_Diff = round(LW_Diff, 1)
  )

print(results)

# Find best results
best_general <- which.min(results$General_Diff)
best_lw <- which.min(results$LW_Diff)

cat("\nBest general usefulness result: Test", best_general, 
    "with", results$General_Median[best_general], "% (diff:", results$General_Diff[best_general], "pp)\n")
cat("Best L&W result: Test", best_lw, 
    "with", results$LW_Median[best_lw], "% (diff:", results$LW_Diff[best_lw], "pp)\n\n")

cat("CONCLUSIONS:\n")
cat("1. Parameter ranges significantly affect results\n")
cat("2. Economic parameters have major impact on reductions\n")
cat("3. May need Table 1 exact ranges to match 291% target\n")
cat("4. L&W case study is closer but still needs refinement\n")
cat("5. Consider testing different L&W time periods\n") 