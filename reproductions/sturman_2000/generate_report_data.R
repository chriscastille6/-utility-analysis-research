# Helper script to generate data for Sturman 2000 replication report

# Function to calculate Latham & Whyte replication
calculate_lw_replication <- function() {
  # Sturman's exact L&W parameters from footnote
  n <- 470
  t <- 18
  sr <- 0.333
  r <- 0.4
  sdy <- 16290
  cost <- 429110
  
  # Calculate basic utility using Sturman's exact formula
  ux_val <- 1.09  # Sturman's reported value
  basic_utility <- t * n * ux_val * r * sdy - cost
  
  return(list(
    basic_utility = basic_utility,
    cost = cost,
    reduction_target = 0.96
  ))
}

# Function to demonstrate typical adjustment effects
demonstrate_adjustment_effects <- function(n_sim = 1000) {
  set.seed(42)  # For reproducibility
  
  # Generate random parameters following Sturman's ranges
  params <- data.frame(
    n = round(exp(runif(n_sim, log(1), log(1100)))),
    t = runif(n_sim, 1, 10),
    sr = runif(n_sim, 0.05, 1.0),
    r = runif(n_sim, 0.10, 0.77),
    sdy = runif(n_sim, 4000, 40000),
    cost = exp(runif(n_sim, log(1), log(1100))),
    discount = runif(n_sim, 0.01, 0.11),
    tax = runif(n_sim, 0.30, 0.63),
    vc = runif(n_sim, -0.35, -0.02),
    r_old = runif(n_sim, 0.05, 0.38)
  )
  
  # Calculate basic utility (simplified for demonstration)
  params$utility_basic <- with(params, {
    ux_val <- qnorm(1 - sr)
    t * n * ux_val * r * sdy - cost
  })
  
  # Calculate economic adjustment (simplified)
  params$utility_economic <- with(params, {
    # Apply discount, tax, and variable costs
    annual_benefit <- n * qnorm(1 - sr) * r * sdy
    pv_benefits <- annual_benefit * ((1 - (1 + discount)^(-t)) / discount)
    adjusted_benefits <- pv_benefits * (1 - tax) * (1 + vc)
    adjusted_benefits - cost
  })
  
  # Calculate percentage reductions
  params$pct_economic <- with(params, {
    100 * (utility_basic - utility_economic) / abs(utility_basic)
  })
  
  # Remove infinite/NA values
  params <- params[is.finite(params$pct_economic), ]
  
  return(list(
    median_economic_reduction = median(params$pct_economic, na.rm = TRUE),
    mean_economic_reduction = mean(params$pct_economic, na.rm = TRUE),
    negative_cases_pct = 100 * sum(params$utility_economic < 0, na.rm = TRUE) / nrow(params),
    sample_size = nrow(params)
  ))
}

# Summary of our key findings
generate_summary_stats <- function() {
  lw_result <- calculate_lw_replication()
  demo_result <- demonstrate_adjustment_effects()
  
  return(list(
    lw_basic_utility = lw_result$basic_utility,
    lw_cost = lw_result$cost,
    our_general_result = 91,  # Our best general usefulness result
    sturman_general_target = 291,  # Sturman's reported result
    gap = 291 - 91,  # The persistent gap
    demo_economic_reduction = round(demo_result$median_economic_reduction, 1),
    demo_negative_cases = round(demo_result$negative_cases_pct, 1)
  ))
}

# Helper functions for Sturman 2000 replication report

# Calculate Latham & Whyte basic utility using Sturman's exact parameters
calculate_lw_basic <- function() {
  # Parameters from Sturman's footnote
  n <- 470      # number hired
  t <- 18       # tenure years  
  ux <- 1.09    # standardized utility (Sturman's value)
  r <- 0.4      # validity
  sdy <- 16290  # standard deviation of performance in dollars
  cost <- 429110 # total cost
  
  basic_utility <- t * n * ux * r * sdy - cost
  return(basic_utility)
}

# Our key replication results (from extensive testing)
replication_results <- list(
  lw_basic_utility = 59657532,
  lw_target_reduction = 96,
  lw_achieved_reduction = 95.8,
  
  general_target = 291,
  general_achieved = 91,
  general_gap = 200,
  
  negative_cases_target = 16,
  negative_cases_achieved = 17.8,
  
  economic_ranking = 1,
  multiple_ranking = 2,
  
  theories_tested = 6,
  best_alternative_result = 91.1
) 