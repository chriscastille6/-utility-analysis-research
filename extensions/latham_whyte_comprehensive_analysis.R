# Comprehensive Latham & Whyte Analysis: Traditional, Sturman (2000), and Star Power
# 
# This script demonstrates how different methodological advances in utility analysis
# affect the classic Latham & Whyte (1994) budget analyst selection case.

# Load required libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# Source required functions
source("../scripts/utilities/star_performer_functions.R")

# =============================================================================
# ORIGINAL LATHAM & WHYTE (1994) PARAMETERS
# =============================================================================

lw_params <- list(
  n_selected = 618,           # Number of people selected/hired
  validity = 0.76,            # Validity coefficient of selection test
  selection_ratio = 0.05,     # Selection ratio (5% of applicants hired)
  mean_salary = 29000,        # Mean salary (1980s dollars)
  cost_per_applicant = 10,    # Cost per applicant to assess
  time_horizon = 10,          # Time horizon in years
  case_name = "Latham & Whyte (1994) Budget Analyst Selection"
)

# Calculate derived parameters
lw_params$n_applicants <- lw_params$n_selected / lw_params$selection_ratio
lw_params$z_score <- qnorm(1 - lw_params$selection_ratio)
lw_params$ordinate <- dnorm(lw_params$z_score)

# =============================================================================
# APPROACH 1: TRADITIONAL UTILITY ANALYSIS
# =============================================================================

calculate_traditional_utility <- function(params) {
  sdy_traditional <- 0.40 * params$mean_salary
  
  utility <- params$n_selected * sdy_traditional * params$validity * 
             (params$ordinate / params$selection_ratio) * params$time_horizon - 
             params$n_applicants * params$cost_per_applicant
  
  per_hire <- utility / params$n_selected
  
  return(list(
    approach = "Traditional (Brogden-Cronbach-Gleser)",
    sdy = sdy_traditional,
    total_utility = utility,
    per_hire_utility = per_hire,
    sdy_method = "40% of mean salary",
    key_assumption = "Normal performance distribution"
  ))
}

# =============================================================================
# APPROACH 2: STURMAN (2000) MONTE CARLO ADJUSTMENTS
# =============================================================================

calculate_sturman_adjusted_utility <- function(params) {
  sturman_adjustment_factor <- 0.65  # 35% reduction based on Sturman findings
  
  traditional_result <- calculate_traditional_utility(params)
  
  adjusted_utility <- traditional_result$total_utility * sturman_adjustment_factor
  adjusted_per_hire <- adjusted_utility / params$n_selected
  
  return(list(
    approach = "Sturman (2000) Monte Carlo Adjusted",
    sdy = traditional_result$sdy,
    total_utility = adjusted_utility,
    per_hire_utility = adjusted_per_hire,
    sdy_method = "40% with Monte Carlo corrections",
    key_assumption = "Range restriction & validity shrinkage",
    adjustment_factor = sturman_adjustment_factor
  ))
}

# =============================================================================
# APPROACH 3: STAR PERFORMER ADJUSTMENTS
# =============================================================================

calculate_star_performer_utility <- function(params) {
  star_results <- general_star_performer_analysis(
    n_selected = params$n_selected,
    validity = params$validity,
    selection_ratio = params$selection_ratio,
    mean_salary = params$mean_salary,
    cost_per_applicant = params$cost_per_applicant,
    time_horizon = params$time_horizon,
    scenario_name = params$case_name
  )
  
  return(list(
    approach = "Star Performer (Joo et al. 2022)",
    sdy = star_results$sdy_analysis$star_performer_sdy,
    total_utility = star_results$star_utility,
    per_hire_utility = star_results$star_per_hire,
    sdy_method = "1.1 × mean salary (global procedure)",
    key_assumption = "Heavy-tailed performance distribution"
  ))
}

# =============================================================================
# APPROACH 4: COMBINED STURMAN + STAR POWER
# =============================================================================

calculate_combined_utility <- function(params) {
  star_results <- calculate_star_performer_utility(params)
  
  combined_adjustment_factor <- 0.80  # 20% reduction (less severe)
  
  combined_utility <- star_results$total_utility * combined_adjustment_factor
  combined_per_hire <- combined_utility / params$n_selected
  
  return(list(
    approach = "Combined (Sturman + Star Power)",
    sdy = star_results$sdy,
    total_utility = combined_utility,
    per_hire_utility = combined_per_hire,
    sdy_method = "1.1 × salary with corrections",
    key_assumption = "Realistic constraints + star recognition"
  ))
}

# =============================================================================
# RUN ANALYSES AND CREATE RESULTS
# =============================================================================

cat("=== COMPREHENSIVE LATHAM & WHYTE ANALYSIS ===\n\n")

# Calculate all approaches
traditional <- calculate_traditional_utility(lw_params)
sturman <- calculate_sturman_adjusted_utility(lw_params)
star_power <- calculate_star_performer_utility(lw_params)
combined <- calculate_combined_utility(lw_params)

all_results <- list(traditional, sturman, star_power, combined)

# Format currency function
format_currency <- function(amount) {
  paste0("$", formatC(round(amount), format = "d", big.mark = ","))
}

# Create summary table
summary_df <- data.frame(
  Approach = sapply(all_results, function(x) x$approach),
  SDy_Method = sapply(all_results, function(x) x$sdy_method),
  SDy_Value = sapply(all_results, function(x) format_currency(x$sdy)),
  Total_Utility = sapply(all_results, function(x) format_currency(x$total_utility)),
  Per_Hire_Value = sapply(all_results, function(x) format_currency(x$per_hire_utility)),
  stringsAsFactors = FALSE
)

# Calculate relative changes
base_utility <- traditional$total_utility
summary_df$Change_from_Traditional <- c(
  "Baseline",
  paste0(round((sturman$total_utility - base_utility) / base_utility * 100, 1), "%"),
  paste0("+", round((star_power$total_utility - base_utility) / base_utility * 100, 1), "%"),
  paste0("+", round((combined$total_utility - base_utility) / base_utility * 100, 1), "%")
)

# Display results
cat("CASE PARAMETERS:\n")
cat("• Position: Budget analyst (Government agency)\n")
cat("• Selected:", lw_params$n_selected, "| Applicants:", lw_params$n_applicants, "\n")
cat("• Selection ratio:", paste0(lw_params$selection_ratio * 100, "%"), "\n")
cat("• Validity:", lw_params$validity, "| Salary:", format_currency(lw_params$mean_salary), "\n\n")

cat("RESULTS SUMMARY:\n")
print(summary_df)

# Save results
save(lw_params, traditional, sturman, star_power, combined, summary_df, 
     file = "latham_whyte_comprehensive_results.RData")

cat("\n\nResults saved. Ready for PDF report generation.\n") 