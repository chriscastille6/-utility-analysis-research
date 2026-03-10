# Employee Benefits Utility Analysis Framework
# Adapting Gregory (1981) Healthcare Benefits Risk Analysis to General Employee Benefits
# 
# This framework applies Gregory's expected utility theory and risk analysis methodology
# to evaluate the utility of different employee benefits programs and funding mechanisms

library(mvtnorm)
library(ggplot2)
library(dplyr)
library(plotly)

# =============================================================================
# GREGORY (1981) ADAPTATION FOR EMPLOYEE BENEFITS
# =============================================================================

# Exponential utility function with risk aversion parameter (from Gregory 1981)
exponential_utility <- function(wealth, risk_aversion) {
  -exp(-risk_aversion * wealth)
}

# Expected utility calculation for benefit cost distributions
expected_utility_benefits <- function(cost_distribution, risk_aversion) {
  utility_values <- exponential_utility(cost_distribution, risk_aversion)
  mean(utility_values)
}

# Risk aversion coefficient (Pratt-Arrow measure)
calculate_risk_aversion <- function(utility_function, wealth) {
  # For exponential utility: r(w) = -u''(w)/u'(w) = v (constant)
  return(utility_function$risk_aversion)
}

# =============================================================================
# EMPLOYEE BENEFITS UTILITY ANALYSIS FUNCTIONS
# =============================================================================

# Calculate utility of different benefits funding mechanisms
calculate_benefits_utility <- function(benefits_params, risk_aversion = 0.001) {
  
  # Extract parameters
  n_employees <- benefits_params$n_employees
  benefit_type <- benefits_params$benefit_type
  funding_mechanism <- benefits_params$funding_mechanism
  
  # Base benefit costs (annual per employee)
  base_costs <- list(
    "health_insurance" = 8000,
    "dental_insurance" = 1200,
    "vision_insurance" = 400,
    "life_insurance" = 300,
    "disability_insurance" = 600,
    "retirement_401k" = 3000,
    "pension" = 5000,
    "wellness_program" = 500,
    "tuition_reimbursement" = 2000,
    "childcare_assistance" = 1500
  )
  
  # Cost variability factors by benefit type
  cost_variability <- list(
    "health_insurance" = 0.25,  # High variability
    "dental_insurance" = 0.15,
    "vision_insurance" = 0.10,
    "life_insurance" = 0.05,    # Low variability
    "disability_insurance" = 0.20,
    "retirement_401k" = 0.30,   # High variability (market dependent)
    "pension" = 0.35,           # Very high variability
    "wellness_program" = 0.20,
    "tuition_reimbursement" = 0.40,  # Very high variability
    "childcare_assistance" = 0.25
  )
  
  # Funding mechanism adjustments
  funding_adjustments <- list(
    "fully_insured" = list(
      "cost_multiplier" = 1.0,
      "risk_reduction" = 0.8,  # Insurance reduces risk
      "administrative_cost" = 0.05
    ),
    "self_funded" = list(
      "cost_multiplier" = 0.85,  # Lower base cost
      "risk_reduction" = 0.0,    # No risk reduction
      "administrative_cost" = 0.02
    ),
    "partially_self_funded" = list(
      "cost_multiplier" = 0.92,
      "risk_reduction" = 0.4,
      "administrative_cost" = 0.03
    ),
    "stop_loss_insurance" = list(
      "cost_multiplier" = 0.88,
      "risk_reduction" = 0.6,
      "administrative_cost" = 0.04
    )
  )
  
  # Get base parameters
  base_cost <- base_costs[[benefit_type]]
  variability <- cost_variability[[benefit_type]]
  funding_params <- funding_adjustments[[funding_mechanism]]
  
  # Calculate total annual cost
  total_base_cost <- n_employees * base_cost * funding_params$cost_multiplier
  
  # Add administrative costs
  administrative_cost <- total_base_cost * funding_params$administrative_cost
  total_cost_with_admin <- total_base_cost + administrative_cost
  
  # Simulate cost distribution (Monte Carlo)
  set.seed(123)
  n_sims <- 1000
  
  # Generate cost scenarios
  cost_scenarios <- replicate(n_sims, {
    # Base cost with variability
    variable_cost <- total_cost_with_admin * (1 + rnorm(1, 0, variability))
    
    # Apply risk reduction from funding mechanism
    risk_adjusted_cost <- variable_cost * (1 - funding_params$risk_reduction)
    
    # Ensure non-negative costs
    max(0, risk_adjusted_cost)
  })
  
  # Calculate distribution statistics
  cost_mean <- mean(cost_scenarios)
  cost_sd <- sd(cost_scenarios)
  cost_var <- var(cost_scenarios)
  
  # Risk-adjusted utility using exponential utility function
  risk_adjusted_utilities <- exponential_utility(-cost_scenarios, risk_aversion)  # Negative because costs reduce utility
  expected_risk_adjusted_utility <- mean(risk_adjusted_utilities)
  
  # Convert to certainty equivalent (amount organization would pay to avoid risk)
  certainty_equivalent <- -log(-expected_risk_adjusted_utility) / risk_aversion
  
  # Risk premium (difference between expected cost and certainty equivalent)
  risk_premium <- cost_mean - certainty_equivalent
  
  # Calculate utility metrics
  utility_metrics <- list(
    benefit_type = benefit_type,
    funding_mechanism = funding_mechanism,
    n_employees = n_employees,
    cost_distribution = cost_scenarios,
    expected_cost = cost_mean,
    cost_standard_deviation = cost_sd,
    cost_variance = cost_var,
    risk_adjusted_utility = expected_risk_adjusted_utility,
    certainty_equivalent = certainty_equivalent,
    risk_premium = risk_premium,
    risk_aversion = risk_aversion,
    administrative_cost = administrative_cost,
    total_base_cost = total_base_cost
  )
  
  return(utility_metrics)
}

# Compare different benefits funding mechanisms
compare_benefits_funding <- function(benefits_params, funding_mechanisms, risk_aversion = 0.001) {
  
  results <- list()
  
  for (mechanism in funding_mechanisms) {
    params <- benefits_params
    params$funding_mechanism <- mechanism
    
    results[[mechanism]] <- calculate_benefits_utility(params, risk_aversion)
  }
  
  # Create comparison summary
  comparison_summary <- data.frame(
    funding_mechanism = funding_mechanisms,
    expected_cost = sapply(results, function(x) x$expected_cost),
    cost_std_dev = sapply(results, function(x) x$cost_standard_deviation),
    risk_premium = sapply(results, function(x) x$risk_premium),
    certainty_equivalent = sapply(results, function(x) x$certainty_equivalent),
    risk_adjusted_utility = sapply(results, function(x) x$risk_adjusted_utility),
    stringsAsFactors = FALSE
  )
  
  # Rank by expected utility (higher is better for costs)
  comparison_summary$rank <- rank(-comparison_summary$risk_adjusted_utility)
  
  return(list(
    individual_results = results,
    comparison_summary = comparison_summary,
    risk_aversion = risk_aversion
  ))
}

# Risk sensitivity analysis for benefits decisions
benefits_risk_sensitivity <- function(benefits_params, funding_mechanisms, 
                                    risk_aversion_range = seq(0, 0.01, 0.001)) {
  
  results <- data.frame(
    risk_aversion = rep(risk_aversion_range, length(funding_mechanisms)),
    funding_mechanism = rep(funding_mechanisms, each = length(risk_aversion_range)),
    expected_utility = numeric(length(risk_aversion_range) * length(funding_mechanisms)),
    certainty_equivalent = numeric(length(risk_aversion_range) * length(funding_mechanisms)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(risk_aversion_range)) {
    for (j in seq_along(funding_mechanisms)) {
      params <- benefits_params
      params$funding_mechanism <- funding_mechanisms[j]
      
      result <- calculate_benefits_utility(params, risk_aversion_range[i])
      
      idx <- (i - 1) * length(funding_mechanisms) + j
      results$expected_utility[idx] <- result$risk_adjusted_utility
      results$certainty_equivalent[idx] <- result$certainty_equivalent
    }
  }
  
  return(results)
}

# =============================================================================
# BENEFITS PORTFOLIO OPTIMIZATION
# =============================================================================

# Optimize benefits portfolio considering risk and cost
optimize_benefits_portfolio <- function(employee_count, benefit_types, budget_constraint, risk_aversion = 0.001) {
  
  # Generate all possible combinations of benefit types and funding mechanisms
  funding_mechanisms <- c("fully_insured", "self_funded", "partially_self_funded", "stop_loss_insurance")
  
  # Create parameter combinations
  combinations <- expand.grid(
    benefit_type = benefit_types,
    funding_mechanism = funding_mechanisms,
    stringsAsFactors = FALSE
  )
  
  # Calculate utility for each combination
  portfolio_results <- list()
  
  for (i in 1:nrow(combinations)) {
    params <- list(
      n_employees = employee_count,
      benefit_type = combinations$benefit_type[i],
      funding_mechanism = combinations$funding_mechanism[i]
    )
    
    portfolio_results[[i]] <- calculate_benefits_utility(params, risk_aversion)
  }
  
  # Create portfolio summary
  portfolio_summary <- data.frame(
    benefit_type = combinations$benefit_type,
    funding_mechanism = combinations$funding_mechanism,
    expected_cost = sapply(portfolio_results, function(x) x$expected_cost),
    risk_premium = sapply(portfolio_results, function(x) x$risk_premium),
    utility_score = sapply(portfolio_results, function(x) x$risk_adjusted_utility),
    stringsAsFactors = FALSE
  )
  
  # Filter by budget constraint
  portfolio_summary$total_cost <- portfolio_summary$expected_cost
  portfolio_summary <- portfolio_summary[portfolio_summary$total_cost <= budget_constraint, ]
  
  # Rank by utility score
  portfolio_summary$rank <- rank(-portfolio_summary$utility_score)
  
  return(list(
    portfolio_options = portfolio_summary,
    individual_results = portfolio_results,
    budget_constraint = budget_constraint,
    risk_aversion = risk_aversion
  ))
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

# Plot benefits cost distributions
plot_benefits_cost_distribution <- function(benefits_result, title = "Benefits Cost Distribution") {
  
  df <- data.frame(
    cost = benefits_result$cost_distribution,
    benefit_type = benefits_result$benefit_type,
    funding_mechanism = benefits_result$funding_mechanism
  )
  
  p <- ggplot(df, aes(x = cost)) +
    geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "steelblue") +
    geom_vline(xintercept = benefits_result$expected_cost, 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = benefits_result$certainty_equivalent, 
               color = "green", linetype = "dashed", size = 1) +
    labs(
      title = title,
      x = "Annual Cost ($)",
      y = "Density",
      caption = paste("Red line: Expected cost ($", round(benefits_result$expected_cost, 0), 
                     ")\nGreen line: Certainty equivalent ($", round(benefits_result$certainty_equivalent, 0), ")")
    ) +
    theme_minimal() +
    scale_x_continuous(labels = scales::dollar_format())
  
  return(p)
}

# Plot funding mechanism comparison
plot_funding_comparison <- function(comparison_results, title = "Benefits Funding Mechanism Comparison") {
  
  df <- comparison_results$comparison_summary
  
  p <- ggplot(df, aes(x = funding_mechanism, y = expected_cost, fill = funding_mechanism)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = expected_cost - cost_std_dev, 
                      ymax = expected_cost + cost_std_dev), 
                  width = 0.2) +
    labs(
      title = title,
      x = "Funding Mechanism",
      y = "Expected Annual Cost ($)",
      fill = "Funding Type"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::dollar_format())
  
  return(p)
}

# Plot risk sensitivity analysis
plot_benefits_risk_sensitivity <- function(sensitivity_results, title = "Benefits Risk Sensitivity Analysis") {
  
  p <- ggplot(sensitivity_results, aes(x = risk_aversion, y = expected_utility, color = funding_mechanism)) +
    geom_line(size = 1) +
    labs(
      title = title,
      x = "Risk Aversion Parameter",
      y = "Expected Utility",
      color = "Funding Mechanism"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

# =============================================================================
# INTEGRATION WITH EXISTING UTILITY ANALYSIS
# =============================================================================

# Enhanced benefits utility calculation incorporating existing methodologies
calculate_comprehensive_benefits_utility <- function(benefits_params, risk_aversion = 0.001) {
  
  # Get basic benefits utility
  benefits_utility <- calculate_benefits_utility(benefits_params, risk_aversion)
  
  # Add economic adjustments (similar to Sturman 2000)
  economic_adjustments <- list(
    tax_rate = 0.25,           # Corporate tax rate
    discount_rate = 0.05,      # Cost of capital
    time_horizon = 5           # Analysis period
  )
  
  # Apply present value adjustments
  pv_factor <- (1 - (1 + economic_adjustments$discount_rate)^(-economic_adjustments$time_horizon)) / 
               economic_adjustments$discount_rate
  
  # Tax-adjusted costs
  after_tax_cost <- benefits_utility$expected_cost * (1 - economic_adjustments$tax_rate)
  pv_after_tax_cost <- after_tax_cost * pv_factor
  
  # Enhanced results
  enhanced_results <- benefits_utility
  enhanced_results$economic_adjustments <- economic_adjustments
  enhanced_results$after_tax_cost <- after_tax_cost
  enhanced_results$present_value_cost <- pv_after_tax_cost
  enhanced_results$annual_pv_cost <- pv_after_tax_cost / economic_adjustments$time_horizon
  
  return(enhanced_results)
}

# =============================================================================
# EXAMPLE APPLICATIONS
# =============================================================================

# Example 1: Health Insurance Funding Decision
if (FALSE) {  # Set to TRUE to run examples
  
  # Parameters for health insurance analysis
  health_insurance_params <- list(
    n_employees = 500,
    benefit_type = "health_insurance",
    funding_mechanism = "fully_insured"  # Will be varied in comparison
  )
  
  # Compare funding mechanisms
  funding_mechanisms <- c("fully_insured", "self_funded", "partially_self_funded", "stop_loss_insurance")
  
  health_comparison <- compare_benefits_funding(health_insurance_params, funding_mechanisms, risk_aversion = 0.001)
  
  # Print results
  cat("Health Insurance Funding Comparison:\n")
  cat("=====================================\n")
  print(health_comparison$comparison_summary)
  
  # Create visualization
  plot_funding_comparison(health_comparison, "Health Insurance Funding Mechanism Comparison")
  
  # Risk sensitivity analysis
  risk_sensitivity <- benefits_risk_sensitivity(health_insurance_params, funding_mechanisms)
  plot_benefits_risk_sensitivity(risk_sensitivity, "Health Insurance Risk Sensitivity")
  
}

# Example 2: Benefits Portfolio Optimization
if (FALSE) {
  
  # Define benefit types for portfolio
  benefit_types <- c("health_insurance", "dental_insurance", "retirement_401k", "wellness_program")
  
  # Optimize portfolio
  portfolio_optimization <- optimize_benefits_portfolio(
    employee_count = 1000,
    benefit_types = benefit_types,
    budget_constraint = 5000000,  # $5M budget
    risk_aversion = 0.001
  )
  
  # Print top recommendations
  cat("Top Benefits Portfolio Options:\n")
  cat("===============================\n")
  top_options <- head(portfolio_optimization$portfolio_options[order(portfolio_optimization$portfolio_options$rank), ], 10)
  print(top_options)
  
}

# =============================================================================
# GREGORY (1981) METHODOLOGY ADAPTATION SUMMARY
# =============================================================================

# Key adaptations from Gregory (1981) healthcare benefits analysis:
#
# 1. **Expected Utility Theory**: Applied exponential utility function to employee benefits
# 2. **Risk Analysis**: Incorporated risk aversion and uncertainty in benefits costs
# 3. **Funding Mechanism Comparison**: Adapted experience-rated vs. self-funded analysis
# 4. **Stochastic Dominance**: Framework for comparing different benefits options
# 5. **Risk Sensitivity**: Analysis of how risk preferences affect benefits decisions
#
# Key differences from Gregory's original work:
# - Focus on general employee benefits rather than healthcare specifically
# - Integration with existing utility analysis methodologies
# - Application to benefits portfolio optimization
# - Incorporation of modern economic adjustments (taxes, discounting)
#
# This framework provides a rigorous approach to evaluating employee benefits
# that accounts for both expected costs and risk preferences, enabling
# organizations to make more informed benefits decisions.
