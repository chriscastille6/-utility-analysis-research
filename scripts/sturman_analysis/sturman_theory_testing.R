# STURMAN (2000) THEORY TESTING FRAMEWORK
# Systematic investigation of why we can't replicate the 291% median reduction

library(dplyr)
library(scales)

# =============================================================================
# BASELINE IMPLEMENTATION (Current Best Attempt)
# =============================================================================

run_baseline_test <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("BASELINE TEST (Current Implementation)\n")
  cat("=====================================\n")
  
  # Current parameter ranges (best estimates)
  params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(1100)))),
    t = runif(n_sims, 1, 10),
    sr = runif(n_sims, 0.05, 1.0),
    r = runif(n_sims, 0.10, 0.70),
    sdy = runif(n_sims, 5000, 50000),
    cost = exp(runif(n_sims, log(10), log(1000))),
    discount = runif(n_sims, 0.05, 0.15),
    tax = runif(n_sims, 0.20, 0.40),
    vc = runif(n_sims, 0.10, 0.30),
    r_old = runif(n_sims, 0.05, 0.38)
  )
  
  # Standard normal ordinate
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  
  # Basic utility
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  
  # Economic adjustments
  params$utility_economic <- with(params, {
    annual_benefit <- n * r * ux(sr) * sdy
    annual_variable_costs <- annual_benefit * vc
    annual_net_benefit <- annual_benefit - annual_variable_costs
    pv_factor <- ifelse(discount > 0, (1 - (1 + discount)^(-t)) / discount, t)
    pv_benefits <- annual_net_benefit * pv_factor
    after_tax_pv <- pv_benefits * (1 - tax)
    total_costs <- (n/sr) * cost
    after_tax_pv - total_costs
  })
  
  # Calculate percentage change
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Target: 291%\n")
  cat("Gap:", round(291 - median_reduction, 1), "percentage points\n\n")
  
  return(list(
    median_reduction = median_reduction,
    params = params,
    gap = 291 - median_reduction
  ))
}

# =============================================================================
# THEORY 1: ALTERNATIVE PARAMETER RANGES
# =============================================================================

test_theory1_wider_ranges <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("THEORY 1A: WIDER PARAMETER RANGES\n")
  cat("==================================\n")
  
  # Test with wider ranges that might create more extreme values
  params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(2000)))),  # Wider n range
    t = runif(n_sims, 1, 20),                          # Wider tenure range  
    sr = runif(n_sims, 0.01, 1.0),                     # Lower minimum SR
    r = runif(n_sims, 0.05, 0.80),                     # Wider validity range
    sdy = runif(n_sims, 1000, 100000),                 # Much wider SDy range
    cost = exp(runif(n_sims, log(1), log(5000))),      # Wider cost range
    discount = runif(n_sims, 0.01, 0.25),              # Wider discount range
    tax = runif(n_sims, 0.10, 0.60),                   # Wider tax range
    vc = runif(n_sims, 0.05, 0.50),                    # Wider variable cost range
    r_old = runif(n_sims, 0.01, 0.50)                  # Wider old validity range
  )
  
  # Same calculations as baseline
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  params$utility_economic <- with(params, {
    annual_benefit <- n * r * ux(sr) * sdy
    annual_variable_costs <- annual_benefit * vc
    annual_net_benefit <- annual_benefit - annual_variable_costs
    pv_factor <- ifelse(discount > 0, (1 - (1 + discount)^(-t)) / discount, t)
    pv_benefits <- annual_net_benefit * pv_factor
    after_tax_pv <- pv_benefits * (1 - tax)
    total_costs <- (n/sr) * cost
    after_tax_pv - total_costs
  })
  
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Improvement from baseline:", round(median_reduction - 96.6, 1), "pp\n\n")
  
  return(median_reduction)
}

test_theory1_extreme_ranges <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("THEORY 1B: EXTREME PARAMETER RANGES\n")
  cat("====================================\n")
  
  # Test with extreme ranges that could produce the 291% target
  params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(10000)))),  # Very wide n range
    t = runif(n_sims, 1, 30),                           # Very wide tenure
    sr = runif(n_sims, 0.001, 1.0),                     # Very low minimum SR
    r = runif(n_sims, 0.01, 0.90),                      # Very wide validity
    sdy = runif(n_sims, 500, 200000),                   # Extreme SDy range
    cost = exp(runif(n_sims, log(0.1), log(10000))),    # Extreme cost range
    discount = runif(n_sims, 0.001, 0.50),              # Extreme discount range
    tax = runif(n_sims, 0.05, 0.80),                    # Extreme tax range
    vc = runif(n_sims, 0.01, 0.70),                     # Extreme variable cost
    r_old = runif(n_sims, 0.001, 0.70)                  # Extreme old validity
  )
  
  # Same calculations
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  params$utility_economic <- with(params, {
    annual_benefit <- n * r * ux(sr) * sdy
    annual_variable_costs <- annual_benefit * vc
    annual_net_benefit <- annual_benefit - annual_variable_costs
    pv_factor <- ifelse(discount > 0, (1 - (1 + discount)^(-t)) / discount, t)
    pv_benefits <- annual_net_benefit * pv_factor
    after_tax_pv <- pv_benefits * (1 - tax)
    total_costs <- (n/sr) * cost
    after_tax_pv - total_costs
  })
  
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Improvement from baseline:", round(median_reduction - 96.6, 1), "pp\n\n")
  
  return(median_reduction)
}

# =============================================================================
# THEORY 2: ALTERNATIVE ECONOMIC ADJUSTMENT FORMULAS
# =============================================================================

test_theory2_simple_economic <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("THEORY 2A: SIMPLIFIED ECONOMIC ADJUSTMENT\n")
  cat("==========================================\n")
  
  # Use baseline parameters
  params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(1100)))),
    t = runif(n_sims, 1, 10),
    sr = runif(n_sims, 0.05, 1.0),
    r = runif(n_sims, 0.10, 0.70),
    sdy = runif(n_sims, 5000, 50000),
    cost = exp(runif(n_sims, log(10), log(1000))),
    discount = runif(n_sims, 0.05, 0.15),
    tax = runif(n_sims, 0.20, 0.40),
    vc = runif(n_sims, 0.10, 0.30)
  )
  
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  
  # Alternative economic formula - simpler approach
  params$utility_economic <- with(params, {
    basic_benefit <- n * t * ux(sr) * r * sdy
    # Apply adjustments sequentially and more aggressively
    after_vc <- basic_benefit * (1 - vc)
    after_discount <- after_vc / ((1 + discount)^(t/2))  # Different discounting
    after_tax <- after_discount * (1 - tax)
    total_costs <- (n/sr) * cost
    after_tax - total_costs
  })
  
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Improvement from baseline:", round(median_reduction - 96.6, 1), "pp\n\n")
  
  return(median_reduction)
}

test_theory2_multiplicative_economic <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("THEORY 2B: MULTIPLICATIVE ECONOMIC ADJUSTMENT\n")
  cat("==============================================\n")
  
  # Use baseline parameters
  params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(1100)))),
    t = runif(n_sims, 1, 10),
    sr = runif(n_sims, 0.05, 1.0),
    r = runif(n_sims, 0.10, 0.70),
    sdy = runif(n_sims, 5000, 50000),
    cost = exp(runif(n_sims, log(10), log(1000))),
    discount = runif(n_sims, 0.05, 0.15),
    tax = runif(n_sims, 0.20, 0.40),
    vc = runif(n_sims, 0.10, 0.30)
  )
  
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  
  # Multiplicative economic adjustment (all factors multiply)
  params$utility_economic <- with(params, {
    economic_factor <- (1 - vc) * (1 - tax) * (1 / (1 + discount)^t)
    adjusted_benefit <- n * t * ux(sr) * r * sdy * economic_factor
    total_costs <- (n/sr) * cost
    adjusted_benefit - total_costs
  })
  
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Improvement from baseline:", round(median_reduction - 96.6, 1), "pp\n\n")
  
  return(median_reduction)
}

# =============================================================================
# THEORY 3: PARAMETER CORRELATIONS
# =============================================================================

test_theory3_realistic_correlations <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("THEORY 3: REALISTIC PARAMETER CORRELATIONS\n")
  cat("===========================================\n")
  
  # Generate correlated parameters
  base_params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(1100)))),
    t = runif(n_sims, 1, 10),
    sr = runif(n_sims, 0.05, 1.0),
    r = runif(n_sims, 0.10, 0.70),
    sdy = runif(n_sims, 5000, 50000),
    cost = exp(runif(n_sims, log(10), log(1000))),
    discount = runif(n_sims, 0.05, 0.15),
    tax = runif(n_sims, 0.20, 0.40),
    vc = runif(n_sims, 0.10, 0.30)
  )
  
  # Add realistic correlations
  params <- base_params %>%
    mutate(
      # Larger organizations tend to have higher SDy
      sdy = ifelse(n > 500, sdy * 1.2, sdy),
      # Lower selection ratios correlate with higher costs per applicant
      cost = ifelse(sr < 0.2, cost * 1.5, cost),
      # Higher validity correlates with higher costs
      cost = ifelse(r > 0.5, cost * 1.3, cost),
      # Larger organizations have lower discount rates (more stable)
      discount = ifelse(n > 500, discount * 0.8, discount)
    )
  
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  params$utility_economic <- with(params, {
    annual_benefit <- n * r * ux(sr) * sdy
    annual_variable_costs <- annual_benefit * vc
    annual_net_benefit <- annual_benefit - annual_variable_costs
    pv_factor <- ifelse(discount > 0, (1 - (1 + discount)^(-t)) / discount, t)
    pv_benefits <- annual_net_benefit * pv_factor
    after_tax_pv <- pv_benefits * (1 - tax)
    total_costs <- (n/sr) * cost
    after_tax_pv - total_costs
  })
  
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Improvement from baseline:", round(median_reduction - 96.6, 1), "pp\n\n")
  
  return(median_reduction)
}

# =============================================================================
# THEORY 4: DIFFERENT PROBABILITY DISTRIBUTIONS
# =============================================================================

test_theory4_alternative_distributions <- function(seed = 42, n_sims = 10000) {
  set.seed(seed)
  
  cat("THEORY 4: ALTERNATIVE PROBABILITY DISTRIBUTIONS\n")
  cat("================================================\n")
  
  # Use different distributions based on what might be realistic
  params <- data.frame(
    n = round(exp(runif(n_sims, log(1), log(1100)))),  # Keep exponential
    t = round(rgamma(n_sims, shape = 2, rate = 0.3)),  # Gamma for tenure
    sr = rbeta(n_sims, 2, 3),                          # Beta for selection ratio
    r = rbeta(n_sims, 2, 2),                           # Beta for validity
    sdy = round(rlnorm(n_sims, log(20000), 0.5)),      # Log-normal for SDy
    cost = exp(runif(n_sims, log(10), log(1000))),     # Keep exponential
    discount = rbeta(n_sims, 2, 8) * 0.2,              # Beta for discount rate
    tax = rbeta(n_sims, 3, 3) * 0.4 + 0.15,            # Beta for tax rate
    vc = rbeta(n_sims, 2, 5) * 0.4                     # Beta for variable costs
  )
  
  # Ensure parameters are within reasonable bounds
  params <- params %>%
    mutate(
      t = pmin(pmax(t, 1), 15),
      sr = pmin(pmax(sr, 0.05), 1.0),
      r = pmin(pmax(r, 0.10), 0.70),
      sdy = pmin(pmax(sdy, 5000), 50000),
      discount = pmin(pmax(discount, 0.01), 0.20),
      tax = pmin(pmax(tax, 0.15), 0.45),
      vc = pmin(pmax(vc, 0.05), 0.35)
    )
  
  ux <- function(sr) dnorm(qnorm(1 - sr)) / sr
  params$utility_basic <- with(params, n * t * ux(sr) * r * sdy - (n/sr) * cost)
  params$utility_economic <- with(params, {
    annual_benefit <- n * r * ux(sr) * sdy
    annual_variable_costs <- annual_benefit * vc
    annual_net_benefit <- annual_benefit - annual_variable_costs
    pv_factor <- ifelse(discount > 0, (1 - (1 + discount)^(-t)) / discount, t)
    pv_benefits <- annual_net_benefit * pv_factor
    after_tax_pv <- pv_benefits * (1 - tax)
    total_costs <- (n/sr) * cost
    after_tax_pv - total_costs
  })
  
  params$pct_change <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
  median_reduction <- median(params$pct_change, na.rm = TRUE)
  
  cat("Median reduction:", round(median_reduction, 1), "%\n")
  cat("Improvement from baseline:", round(median_reduction - 96.6, 1), "pp\n\n")
  
  return(median_reduction)
}

# =============================================================================
# COMPREHENSIVE TESTING FUNCTION
# =============================================================================

run_all_theory_tests <- function(seed = 42) {
  cat("COMPREHENSIVE STURMAN (2000) THEORY TESTING\n")
  cat("===========================================\n\n")
  
  # Run baseline
  baseline <- run_baseline_test(seed)
  
  # Test all theories
  results <- list(
    baseline = baseline$median_reduction,
    theory1a_wider = test_theory1_wider_ranges(seed),
    theory1b_extreme = test_theory1_extreme_ranges(seed),
    theory2a_simple = test_theory2_simple_economic(seed),
    theory2b_multiplicative = test_theory2_multiplicative_economic(seed),
    theory3_correlations = test_theory3_realistic_correlations(seed),
    theory4_distributions = test_theory4_alternative_distributions(seed)
  )
  
  # Summary
  cat("SUMMARY OF RESULTS\n")
  cat("==================\n")
  cat("Target: 291% median reduction\n\n")
  
  for(name in names(results)) {
    gap <- 291 - results[[name]]
    cat(sprintf("%-20s: %6.1f%% (gap: %6.1f pp)\n", name, results[[name]], gap))
  }
  
  # Find best performer
  best_theory <- names(results)[which.max(unlist(results))]
  best_result <- results[[best_theory]]
  
  cat("\nBEST PERFORMING THEORY:", best_theory, "\n")
  cat("Result:", round(best_result, 1), "%\n")
  cat("Remaining gap:", round(291 - best_result, 1), "percentage points\n\n")
  
  if(best_result >= 250) {
    cat("*** BREAKTHROUGH: Getting close to Sturman's target! ***\n")
  } else if(best_result >= 200) {
    cat("*** PROGRESS: Significant improvement over baseline ***\n")
  } else {
    cat("*** CONCLUSION: Systematic gap suggests missing information ***\n")
  }
  
  return(results)
}

# =============================================================================
# EXECUTION
# =============================================================================

# Run the comprehensive test
if(interactive()) {
  results <- run_all_theory_tests()
  
  cat("\nNEXT STEPS:\n")
  cat("===========\n")
  cat("1. Get Boudreau (1983a) for exact economic adjustment formula\n")
  cat("2. Get Murphy (1986) for top-down hiring parameter ranges\n") 
  cat("3. Get Darlington (1968) for usefulness analysis methodology\n")
  cat("4. Test combinations of the most promising theories\n")
  cat("5. Consider that Sturman may have used different assumptions\n")
} 