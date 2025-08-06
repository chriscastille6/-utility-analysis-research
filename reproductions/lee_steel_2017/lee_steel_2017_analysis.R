# =============================================================================
# LEE & STEEL (2017) REPRODUCTION ANALYSIS
# "The Potential for Selection: A Comprehensive Utility Analysis"
# =============================================================================
# 
# This script reproduces the comprehensive utility analysis framework
# presented in Lee and Steel (2017) for selection system evaluation.
#
# Author: Reproduction Project
# Date: December 2024
# =============================================================================

# =============================================================================
# SETUP AND LIBRARIES
# =============================================================================

# Clear workspace
rm(list = ls())

# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)
library(scales)
library(tidyr)
library(purrr)

# Set random seed for reproducibility
set.seed(12345)

# =============================================================================
# UTILITY ANALYSIS FUNCTIONS
# =============================================================================

#' Calculate Brogden-Cronbach-Gleser utility
#' 
#' @param N Number of employees selected
#' @param T Tenure (years)
#' @param rxy Validity coefficient
#' @param SDy Performance standard deviation (dollars)
#' @param m Selection ratio (proportion selected)
#' @param C Cost per applicant
#' @return Utility value in dollars
calculate_bcg_utility <- function(N, T, rxy, SDy, m, C) {
  # Zxs: Expected value of selected applicants' criterion scores
  # Using Naylor & Shine (1965) tables for more accurate Zxs values
  Zxs <- get_zxs(m)
  
  # BCG utility formula
  utility <- N * T * rxy * SDy * Zxs - N * C / m
  
  return(utility)
}

#' Get Zxs value from selection ratio using Naylor & Shine (1965) tables
#' 
#' @param m Selection ratio (proportion selected)
#' @return Zxs value
get_zxs <- function(m) {
  # Naylor & Shine (1965) table values for Zxs
  # These are more accurate than qnorm for selection scenarios
  zxs_table <- data.frame(
    m = c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50),
    Zxs = c(2.665, 2.063, 1.755, 1.554, 1.400, 1.271, 1.159, 1.058, 0.966, 0.880, 0.798)
  )
  
  # Interpolate for values not in table
  if (m %in% zxs_table$m) {
    return(zxs_table$Zxs[zxs_table$m == m])
  } else {
    # Linear interpolation
    idx <- which(zxs_table$m > m)[1]
    if (is.na(idx) || idx == 1) {
      return(zxs_table$Zxs[1])
    }
    
    m1 <- zxs_table$m[idx - 1]
    m2 <- zxs_table$m[idx]
    zxs1 <- zxs_table$Zxs[idx - 1]
    zxs2 <- zxs_table$Zxs[idx]
    
    zxs <- zxs1 + (zxs2 - zxs1) * (m - m1) / (m2 - m1)
    return(zxs)
  }
}

#' Calculate utility per selected employee
#' 
#' @param T Tenure (years)
#' @param rxy Validity coefficient
#' @param SDy Performance standard deviation (dollars)
#' @param m Selection ratio (proportion selected)
#' @param C Cost per applicant
#' @return Utility per selected employee
calculate_utility_per_employee <- function(T, rxy, SDy, m, C) {
  Zxs <- get_zxs(m)
  utility_per_employee <- T * rxy * SDy * Zxs - C / m
  return(utility_per_employee)
}

#' Calculate return on investment (ROI)
#' 
#' @param utility Total utility
#' @param total_cost Total cost
#' @return ROI as percentage
calculate_roi <- function(utility, total_cost) {
  roi <- (utility / total_cost) * 100
  return(roi)
}

# =============================================================================
# PARAMETER SPECIFICATIONS
# =============================================================================

# Base parameters from Lee and Steel (2017)
base_parameters <- list(
  # Organizational parameters
  N = 100,                    # Number of employees selected
  T = 10,                     # Tenure (years)
  
  # Selection parameters
  rxy = 0.30,                 # Validity coefficient
  SDy = 10000,                # Performance SD ($10,000)
  m = 0.20,                   # Selection ratio (20%)
  
  # Cost parameters
  C = 100                     # Cost per applicant ($100)
)

# Parameter ranges for sensitivity analysis
parameter_ranges <- list(
  validity = seq(0.10, 0.50, by = 0.05),
  selection_ratio = seq(0.05, 0.50, by = 0.05),
  performance_sd = seq(5000, 20000, by = 2500),
  tenure = seq(1, 20, by = 1),
  cost_per_applicant = seq(50, 500, by = 50)
)

# =============================================================================
# BASE CASE ANALYSIS
# =============================================================================

cat("=== LEE & STEEL (2017) BASE CASE ANALYSIS ===\n\n")

# Calculate base case utility
base_utility <- calculate_bcg_utility(
  N = base_parameters$N,
  T = base_parameters$T,
  rxy = base_parameters$rxy,
  SDy = base_parameters$SDy,
  m = base_parameters$m,
  C = base_parameters$C
)

# Calculate utility per employee
utility_per_employee <- calculate_utility_per_employee(
  T = base_parameters$T,
  rxy = base_parameters$rxy,
  SDy = base_parameters$SDy,
  m = base_parameters$m,
  C = base_parameters$C
)

# Calculate total cost
total_cost <- base_parameters$N * base_parameters$C / base_parameters$m

# Calculate ROI
roi <- calculate_roi(base_utility, total_cost)

# Display base case results
cat("Base Case Results:\n")
cat("==================\n")
cat(sprintf("Number of employees selected: %d\n", base_parameters$N))
cat(sprintf("Tenure: %d years\n", base_parameters$T))
cat(sprintf("Validity coefficient: %.2f\n", base_parameters$rxy))
cat(sprintf("Performance SD: $%s\n", format(base_parameters$SDy, big.mark = ",")))
cat(sprintf("Selection ratio: %.1f%%\n", base_parameters$m * 100))
cat(sprintf("Cost per applicant: $%d\n", base_parameters$C))
cat(sprintf("Zxs value: %.3f\n", get_zxs(base_parameters$m)))
cat("\n")
cat(sprintf("Total utility: $%s\n", format(round(base_utility), big.mark = ",")))
cat(sprintf("Utility per employee: $%s\n", format(round(utility_per_employee), big.mark = ",")))
cat(sprintf("Total cost: $%s\n", format(round(total_cost), big.mark = ",")))
cat(sprintf("ROI: %.1f%%\n", roi))

# =============================================================================
# SENSITIVITY ANALYSIS
# =============================================================================

cat("\n\n=== SENSITIVITY ANALYSIS ===\n\n")

#' Conduct sensitivity analysis for a parameter
#' 
#' @param param_name Parameter name
#' @param param_values Vector of parameter values
#' @param base_params Base parameters list
#' @return Data frame with sensitivity results
conduct_sensitivity_analysis <- function(param_name, param_values, base_params) {
  results <- data.frame(
    parameter = param_name,
    value = param_values,
    utility = numeric(length(param_values)),
    utility_per_employee = numeric(length(param_values)),
    roi = numeric(length(param_values))
  )
  
  for (i in seq_along(param_values)) {
    # Create parameter set with current value
    params <- base_params
    params[[param_name]] <- param_values[i]
    
    # Calculate utility
    utility <- calculate_bcg_utility(
      N = params$N,
      T = params$T,
      rxy = params$rxy,
      SDy = params$SDy,
      m = params$m,
      C = params$C
    )
    
    # Calculate utility per employee
    utility_per_emp <- calculate_utility_per_employee(
      T = params$T,
      rxy = params$rxy,
      SDy = params$SDy,
      m = params$m,
      C = params$C
    )
    
    # Calculate total cost and ROI
    total_cost <- params$N * params$C / params$m
    roi_val <- calculate_roi(utility, total_cost)
    
    results$utility[i] <- utility
    results$utility_per_employee[i] <- utility_per_emp
    results$roi[i] <- roi_val
  }
  
  return(results)
}

# Conduct sensitivity analyses
sensitivity_results <- list()

# Validity sensitivity
sensitivity_results$validity <- conduct_sensitivity_analysis(
  "rxy", parameter_ranges$validity, base_parameters
)

# Selection ratio sensitivity
sensitivity_results$selection_ratio <- conduct_sensitivity_analysis(
  "m", parameter_ranges$selection_ratio, base_parameters
)

# Performance SD sensitivity
sensitivity_results$performance_sd <- conduct_sensitivity_analysis(
  "SDy", parameter_ranges$performance_sd, base_parameters
)

# Tenure sensitivity
sensitivity_results$tenure <- conduct_sensitivity_analysis(
  "T", parameter_ranges$tenure, base_parameters
)

# Cost sensitivity
sensitivity_results$cost <- conduct_sensitivity_analysis(
  "C", parameter_ranges$cost_per_applicant, base_parameters
)

# =============================================================================
# RESULTS SUMMARY
# =============================================================================

cat("\n=== SENSITIVITY ANALYSIS SUMMARY ===\n\n")

# Create summary table
summary_data <- data.frame(
  Parameter = c("Validity", "Selection Ratio", "Performance SD", "Tenure", "Cost"),
  Range = c(
    paste("0.10 - 0.50"),
    paste("5% - 50%"),
    paste("$5,000 - $20,000"),
    paste("1 - 20 years"),
    paste("$50 - $500")
  ),
  Utility_Range = c(
    paste("$", format(round(range(sensitivity_results$validity$utility)), big.mark = ","), collapse = " - "),
    paste("$", format(round(range(sensitivity_results$selection_ratio$utility)), big.mark = ","), collapse = " - "),
    paste("$", format(round(range(sensitivity_results$performance_sd$utility)), big.mark = ","), collapse = " - "),
    paste("$", format(round(range(sensitivity_results$tenure$utility)), big.mark = ","), collapse = " - "),
    paste("$", format(round(range(sensitivity_results$cost$utility)), big.mark = ","), collapse = " - ")
  ),
  ROI_Range = c(
    paste(round(range(sensitivity_results$validity$roi)), "%", collapse = " - "),
    paste(round(range(sensitivity_results$selection_ratio$roi)), "%", collapse = " - "),
    paste(round(range(sensitivity_results$performance_sd$roi)), "%", collapse = " - "),
    paste(round(range(sensitivity_results$tenure$roi)), "%", collapse = " - "),
    paste(round(range(sensitivity_results$cost$roi)), "%", collapse = " - ")
  )
)

print(summary_data)

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Save all results
save(
  base_parameters,
  parameter_ranges,
  base_utility,
  utility_per_employee,
  roi,
  sensitivity_results,
  summary_data,
  file = "lee_steel_2017_results.RData"
)

# Save parameters to CSV
parameters_df <- data.frame(
  parameter = names(base_parameters),
  value = unlist(base_parameters),
  description = c(
    "Number of employees selected",
    "Tenure in years",
    "Validity coefficient",
    "Performance standard deviation ($)",
    "Selection ratio (proportion)",
    "Cost per applicant ($)"
  )
)

write.csv(parameters_df, "lee_steel_2017_parameters.csv", row.names = FALSE)

# Save summary statistics
write.csv(summary_data, "lee_steel_2017_summary_stats.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- lee_steel_2017_results.RData\n")
cat("- lee_steel_2017_parameters.csv\n")
cat("- lee_steel_2017_summary_stats.csv\n") 