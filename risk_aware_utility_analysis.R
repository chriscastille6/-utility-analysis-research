# Risk-Aware Utility Analysis Framework
# Integrating Gregory (1981) Risk Analysis with Current Utility Analysis Methods
# 
# This framework combines Gregory's expected utility theory and risk modeling
# with the current BCG-based utility analysis approaches

library(mvtnorm)
library(ggplot2)
library(dplyr)

# =============================================================================
# GREGORY (1981) RISK ANALYSIS FUNCTIONS
# =============================================================================

# Exponential utility function with risk aversion parameter
exponential_utility <- function(wealth, risk_aversion) {
  -exp(-risk_aversion * wealth)
}

# Expected utility calculation for a distribution
expected_utility <- function(utility_values, probabilities) {
  sum(utility_values * probabilities)
}

# Risk aversion coefficient calculation (Pratt-Arrow measure)
pratt_arrow_risk_aversion <- function(utility_function, wealth) {
  # For exponential utility: r(w) = -u''(w)/u'(w) = v (constant)
  # This is the risk aversion parameter itself
  return(utility_function$risk_aversion)
}

# Stochastic dominance check (first-order)
first_order_stochastic_dominance <- function(dist1, dist2) {
  # Check if distribution 1 first-order stochastically dominates distribution 2
  # F1(x) <= F2(x) for all x, with strict inequality for some x
  all(dist1 <= dist2) && any(dist1 < dist2)
}

# =============================================================================
# ENHANCED UTILITY ANALYSIS WITH RISK MODELING
# =============================================================================

# Risk-aware utility calculation combining BCG model with risk analysis
calculate_risk_aware_utility <- function(params, risk_aversion = 0.001) {
  
  # Standard BCG utility calculation
  bcg_utility <- params$n * params$period * ux(params$sr) * params$rxy * params$sdy - 
                 (params$cost * (params$n / params$sr))
  
  # Create utility distribution through Monte Carlo simulation
  set.seed(123)
  n_sims <- 1000
  
  # Parameter uncertainty modeling (similar to current Monte Carlo)
  mc_results <- replicate(n_sims, {
    # Add uncertainty to key parameters
    mc_rxy <- pmax(0.01, pmin(0.99, rnorm(1, params$rxy, params$rxy * 0.1)))
    mc_sdy <- pmax(1000, rnorm(1, params$sdy, params$sdy * 0.15))
    mc_sr <- pmax(0.01, pmin(0.99, rnorm(1, params$sr, params$sr * 0.05)))
    
    # Calculate utility with varied parameters
    mc_utility <- params$n * params$period * ux(mc_sr) * mc_rxy * mc_sdy - 
                  (params$cost * (params$n / mc_sr))
    
    return(mc_utility)
  })
  
  # Calculate utility distribution statistics
  utility_mean <- mean(mc_results)
  utility_sd <- sd(mc_results)
  utility_var <- var(mc_results)
  
  # Risk-adjusted utility using exponential utility function
  risk_adjusted_utilities <- exponential_utility(mc_results, risk_aversion)
  expected_risk_adjusted_utility <- mean(risk_adjusted_utilities)
  
  # Convert back to certainty equivalent
  certainty_equivalent <- -log(-expected_risk_adjusted_utility) / risk_aversion
  
  # Risk premium (difference between expected value and certainty equivalent)
  risk_premium <- utility_mean - certainty_equivalent
  
  return(list(
    bcg_utility = bcg_utility,
    utility_distribution = mc_results,
    utility_mean = utility_mean,
    utility_sd = utility_sd,
    utility_var = utility_var,
    risk_adjusted_utility = expected_risk_adjusted_utility,
    certainty_equivalent = certainty_equivalent,
    risk_premium = risk_premium,
    risk_aversion = risk_aversion
  ))
}

# Compare two interventions using risk analysis
compare_interventions_risk_aware <- function(intervention1, intervention2, risk_aversion = 0.001) {
  
  # Calculate risk-aware utilities for both interventions
  risk1 <- calculate_risk_aware_utility(intervention1, risk_aversion)
  risk2 <- calculate_risk_aware_utility(intervention2, risk_aversion)
  
  # Stochastic dominance analysis
  # Create empirical distributions
  dist1 <- sort(risk1$utility_distribution)
  dist2 <- sort(risk2$utility_distribution)
  
  # Check first-order stochastic dominance
  fosd_1_over_2 <- first_order_stochastic_dominance(dist1, dist2)
  fosd_2_over_1 <- first_order_stochastic_dominance(dist2, dist1)
  
  # Expected utility comparison
  eu1 <- risk1$risk_adjusted_utility
  eu2 <- risk2$risk_adjusted_utility
  
  # Decision based on expected utility
  preferred_intervention <- ifelse(eu1 > eu2, "Intervention 1", "Intervention 2")
  utility_difference <- abs(eu1 - eu2)
  
  return(list(
    intervention1 = risk1,
    intervention2 = risk2,
    expected_utility_1 = eu1,
    expected_utility_2 = eu2,
    preferred_intervention = preferred_intervention,
    utility_difference = utility_difference,
    fosd_1_over_2 = fosd_1_over_2,
    fosd_2_over_1 = fosd_2_over_1,
    risk_aversion = risk_aversion
  ))
}

# =============================================================================
# RISK SENSITIVITY ANALYSIS
# =============================================================================

# Analyze how risk aversion affects intervention choice
risk_sensitivity_analysis <- function(intervention1, intervention2, 
                                    risk_aversion_range = seq(0, 0.01, 0.001)) {
  
  results <- data.frame(
    risk_aversion = risk_aversion_range,
    eu1 = numeric(length(risk_aversion_range)),
    eu2 = numeric(length(risk_aversion_range)),
    preferred = character(length(risk_aversion_range)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(risk_aversion_range)) {
    comparison <- compare_interventions_risk_aware(intervention1, intervention2, 
                                                 risk_aversion_range[i])
    
    results$eu1[i] <- comparison$expected_utility_1
    results$eu2[i] <- comparison$expected_utility_2
    results$preferred[i] <- comparison$preferred_intervention
  }
  
  return(results)
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

# Plot utility distributions
plot_utility_distributions <- function(risk_aware_result, title = "Utility Distribution Analysis") {
  
  df <- data.frame(
    utility = risk_aware_result$utility_distribution,
    intervention = "Intervention"
  )
  
  p <- ggplot(df, aes(x = utility)) +
    geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "steelblue") +
    geom_vline(xintercept = risk_aware_result$utility_mean, 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = risk_aware_result$certainty_equivalent, 
               color = "green", linetype = "dashed", size = 1) +
    labs(
      title = title,
      x = "Utility Value",
      y = "Density",
      caption = paste("Red line: Expected utility (", round(risk_aware_result$utility_mean, 0), 
                     ")\nGreen line: Certainty equivalent (", round(risk_aware_result$certainty_equivalent, 0), ")")
    ) +
    theme_minimal()
  
  return(p)
}

# Plot risk sensitivity analysis
plot_risk_sensitivity <- function(sensitivity_results, title = "Risk Sensitivity Analysis") {
  
  p <- ggplot(sensitivity_results, aes(x = risk_aversion)) +
    geom_line(aes(y = eu1, color = "Intervention 1"), size = 1) +
    geom_line(aes(y = eu2, color = "Intervention 2"), size = 1) +
    labs(
      title = title,
      x = "Risk Aversion Parameter",
      y = "Expected Utility",
      color = "Intervention"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# =============================================================================
# INTEGRATION WITH EXISTING UTILITY ANALYSIS
# =============================================================================

# Enhanced comprehensive utility calculation with risk analysis
calculate_comprehensive_risk_aware_utility <- function(params, risk_aversion = 0.001) {
  
  # Get standard comprehensive utility results
  comprehensive_results <- calculate_comprehensive_utility(params)
  
  # Add risk analysis
  risk_analysis <- calculate_risk_aware_utility(params, risk_aversion)
  
  # Combine results
  return(list(
    traditional = comprehensive_results$traditional,
    naylor_shine = comprehensive_results$naylor_shine,
    financially_adjusted = comprehensive_results$financially_adjusted,
    monte_carlo_mean = comprehensive_results$monte_carlo_mean,
    monte_carlo_sd = comprehensive_results$monte_carlo_sd,
    risk_aware = risk_analysis,
    risk_aversion = risk_aversion
  ))
}

# =============================================================================
# EXAMPLE APPLICATION
# =============================================================================

# Example parameters for demonstration
example_params <- list(
  n = 100,           # Number of employees
  period = 5,        # Time period
  sr = 0.2,          # Selection ratio
  rxy = 0.4,         # Validity coefficient
  sdy = 50000,       # Standard deviation of performance in dollars
  cost = 1000        # Cost per applicant
)

# Standard normal ordinate function (from existing code)
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Example usage
if (FALSE) {  # Set to TRUE to run examples
  
  # Calculate risk-aware utility
  risk_result <- calculate_risk_aware_utility(example_params, risk_aversion = 0.001)
  
  # Print results
  cat("Risk-Aware Utility Analysis Results:\n")
  cat("=====================================\n")
  cat("BCG Utility:", round(risk_result$bcg_utility, 0), "\n")
  cat("Expected Utility:", round(risk_result$utility_mean, 0), "\n")
  cat("Utility Standard Deviation:", round(risk_result$utility_sd, 0), "\n")
  cat("Certainty Equivalent:", round(risk_result$certainty_equivalent, 0), "\n")
  cat("Risk Premium:", round(risk_result$risk_premium, 0), "\n")
  cat("Risk Aversion:", risk_result$risk_aversion, "\n")
  
  # Create visualization
  plot_utility_distributions(risk_result, "Example Risk-Aware Utility Analysis")
  
}
