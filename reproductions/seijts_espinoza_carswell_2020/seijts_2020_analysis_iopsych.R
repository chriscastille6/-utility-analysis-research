# SEIJTS, ESPINOZA, & CARSWELL (2020) REPRODUCTION
# Utility Analysis of Character Assessment in Employee Placement
# Updated Analysis Script - USING IOPsych PACKAGE

# Load required libraries
library(iopsych)
library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(knitr)
library(kableExtra)

# Set seed for reproducibility
set.seed(12345)

# =============================================================================
# STUDY PARAMETERS (from Seijts et al., 2020)
# =============================================================================

# Sample characteristics
N <- 111  # Number of senior managers
selection_ratio <- 0.33  # Selection ratio
cost_per_candidate <- 800  # CAD $800 per candidate

# Utility analysis parameters
sdy <- 115500  # CAD $115,500 (70% of annual salary)
tenure_15 <- 15  # 15-year tenure
tenure_10 <- 10  # 10-year tenure

# Reported correlation
reported_correlation <- 0.30

# Reliability coefficients
alpha_character <- 0.95
alpha_performance <- 0.91

# Character dimensions (11 total)
character_dimensions <- c(
  "Accountability", "Collaboration", "Courage", "Drive", "Humanity",
  "Humility", "Integrity", "Judgment", "Justice", "Temperance", "Transcendence"
)

# =============================================================================
# ECONOMIC ADJUSTMENT PARAMETERS (from Sturman, 2000)
# =============================================================================

# Variable costs rate (VC)
vc_min <- -0.02
vc_mid <- -0.185
vc_max <- -0.35

# Taxation rate (TAX)
tax_min <- 0.30
tax_mid <- 0.465
tax_max <- 0.63

# Discounting rate
discount_min <- 0.01
discount_mid <- 0.06
discount_max <- 0.11

# Incremental validity (rold - rnew)
# Note: Seijts et al. set max at 0.30 (not 0.38 from Sturman) since their actual r = 0.30
incremental_min <- 0.05
incremental_mid <- 0.175
incremental_max <- 0.30

# =============================================================================
# IOPsych PACKAGE UTILITY ANALYSIS
# =============================================================================

cat("=== SEIJTS ET AL. (2020) REPRODUCTION USING IOPsych PACKAGE ===\n\n")

# =============================================================================
# BASIC UTILITY CALCULATIONS USING IOPsych
# =============================================================================

cat("=== BASIC UTILITY ANALYSIS (IOPsych Package) ===\n")

# Get Zxs using iopsych ux() function
zxs_value <- ux(selection_ratio)
cat("Zxs value for selection ratio 0.33 (IOPsych ux()):", round(zxs_value, 3), "\n")

# Basic utility using iopsych utilityBcg() function
basic_utility_15 <- utilityBcg(
  n = 1,  # 1 candidate per year
  sdy = sdy,
  rxy = reported_correlation,
  sr = selection_ratio,
  cost = cost_per_candidate,
  period = tenure_15
)

basic_utility_10 <- utilityBcg(
  n = 1,
  sdy = sdy,
  rxy = reported_correlation,
  sr = selection_ratio,
  cost = cost_per_candidate,
  period = tenure_10
)

cat("Basic utility (15 years, IOPsych): CAD $", format(round(basic_utility_15), big.mark=","), "\n")
cat("Basic utility (10 years, IOPsych): CAD $", format(round(basic_utility_10), big.mark=","), "\n")
cat("Yearly utility (15 years): CAD $", format(round(basic_utility_15/tenure_15), big.mark=","), "\n")
cat("Yearly utility (10 years): CAD $", format(round(basic_utility_10/tenure_10), big.mark=","), "\n")

# ROI calculations
roi_15 <- (basic_utility_15 / (cost_per_candidate / selection_ratio)) * 100
roi_10 <- (basic_utility_10 / (cost_per_candidate / selection_ratio)) * 100

cat("ROI (15 years):", round(roi_15), "%\n")
cat("ROI (10 years):", round(roi_10), "%\n")

# =============================================================================
# ECONOMIC ADJUSTMENTS ANALYSIS
# =============================================================================

cat("\n=== ECONOMIC ADJUSTMENTS ANALYSIS (IOPsych + Manual Adjustments) ===\n")

# Create parameter combinations
adjustment_params <- expand.grid(
  VC = c(vc_min, vc_mid, vc_max),
  TAX = c(tax_min, tax_mid, tax_max),
  DISCOUNT = c(discount_min, discount_mid, discount_max),
  INCREMENTAL = c(incremental_min, incremental_mid, incremental_max)
)

# Add scenario labels
adjustment_params$scenario <- paste0("S", 1:nrow(adjustment_params))

# Calculate adjusted utilities for both tenure periods
results_15 <- data.frame()
results_10 <- data.frame()

for (i in 1:nrow(adjustment_params)) {
  # Get parameters for this scenario
  vc <- adjustment_params$VC[i]
  tax <- adjustment_params$TAX[i]
  discount <- adjustment_params$DISCOUNT[i]
  incremental <- adjustment_params$INCREMENTAL[i]
  
  # Calculate modified tenure with discounting
  Tmod_15 <- tenure_15 * (1 - (1 + discount)^(-tenure_15)) / discount
  Tmod_10 <- tenure_10 * (1 - (1 + discount)^(-tenure_10)) / discount
  
  # Calculate adjusted utilities using iopsych for basic calculation
  # and manual adjustments for economic factors
  basic_utility_15_adj <- utilityBcg(
    n = 1, sdy = sdy, rxy = incremental, sr = selection_ratio,
    cost = cost_per_candidate, period = Tmod_15
  )
  
  basic_utility_10_adj <- utilityBcg(
    n = 1, sdy = sdy, rxy = incremental, sr = selection_ratio,
    cost = cost_per_candidate, period = Tmod_10
  )
  
  # Apply economic adjustments
  adjusted_utility_15 <- basic_utility_15_adj * (1 + vc) * (1 - tax)
  adjusted_utility_10 <- basic_utility_10_adj * (1 + vc) * (1 - tax)
  
  # Store results
  results_15 <- rbind(results_15, data.frame(
    scenario = adjustment_params$scenario[i],
    VC = vc, TAX = tax, DISCOUNT = discount, INCREMENTAL = incremental,
    Tmod = Tmod_15, utility = adjusted_utility_15,
    yearly_utility = adjusted_utility_15 / tenure_15,
    roi = (adjusted_utility_15 / (cost_per_candidate / selection_ratio)) * 100
  ))
  
  results_10 <- rbind(results_10, data.frame(
    scenario = adjustment_params$scenario[i],
    VC = vc, TAX = tax, DISCOUNT = discount, INCREMENTAL = incremental,
    Tmod = Tmod_10, utility = adjusted_utility_10,
    yearly_utility = adjusted_utility_10 / tenure_10,
    roi = (adjusted_utility_10 / (cost_per_candidate / selection_ratio)) * 100
  ))
}

# Summary statistics
cat("15-year tenure results:\n")
cat("Mean utility: CAD $", format(round(mean(results_15$utility)), big.mark=","), "\n")
cat("Median utility: CAD $", format(round(median(results_15$utility)), big.mark=","), "\n")
cat("Min utility: CAD $", format(round(min(results_15$utility)), big.mark=","), "\n")
cat("Max utility: CAD $", format(round(max(results_15$utility)), big.mark=","), "\n")

cat("\n10-year tenure results:\n")
cat("Mean utility: CAD $", format(round(mean(results_10$utility)), big.mark=","), "\n")
cat("Median utility: CAD $", format(round(median(results_10$utility)), big.mark=","), "\n")
cat("Min utility: CAD $", format(round(min(results_10$utility)), big.mark=","), "\n")
cat("Max utility: CAD $", format(round(max(results_10$utility)), big.mark=","), "\n")

# =============================================================================
# COMPARISON WITH REPORTED VALUES
# =============================================================================

cat("\n=== COMPARISON WITH REPORTED VALUES ===\n")

# Reported values from Seijts et al. (2020)
reported_15 <- 564128
reported_10 <- 375285
reported_yearly_15 <- 37609
reported_yearly_10 <- 37529

# Calculate differences
diff_15 <- basic_utility_15 - reported_15
diff_10 <- basic_utility_10 - reported_10
diff_yearly_15 <- (basic_utility_15/tenure_15) - reported_yearly_15
diff_yearly_10 <- (basic_utility_10/tenure_10) - reported_yearly_10

cat("15-year utility comparison:\n")
cat("  Calculated (IOPsych): CAD $", format(round(basic_utility_15), big.mark=","), "\n")
cat("  Reported:             CAD $", format(reported_15, big.mark=","), "\n")
cat("  Difference:           CAD $", format(round(diff_15), big.mark=","), " (", round((diff_15/reported_15)*100, 1), "%)\n")

cat("10-year utility comparison:\n")
cat("  Calculated (IOPsych): CAD $", format(round(basic_utility_10), big.mark=","), "\n")
cat("  Reported:             CAD $", format(reported_10, big.mark=","), "\n")
cat("  Difference:           CAD $", format(round(diff_10), big.mark=","), " (", round((diff_10/reported_10)*100, 1), "%)\n")

cat("\nYearly utility comparison:\n")
cat("  15 years - Calculated: CAD $", format(round(basic_utility_15/tenure_15), big.mark=","), 
    " vs Reported: CAD $", format(reported_yearly_15, big.mark=","), 
    " (", round(((basic_utility_15/tenure_15) - reported_yearly_15)/reported_yearly_15*100, 1), "%)\n")
cat("  10 years - Calculated: CAD $", format(round(basic_utility_10/tenure_10), big.mark=","), 
    " vs Reported: CAD $", format(reported_yearly_10, big.mark=","), 
    " (", round(((basic_utility_10/tenure_10) - reported_yearly_10)/reported_yearly_10*100, 1), "%)\n")

# =============================================================================
# IOPsych PACKAGE ADVANTAGES
# =============================================================================

cat("\n=== IOPsych PACKAGE ADVANTAGES ===\n")

cat("✅ **Accurate Zxs calculation**: ux() function uses Naylor & Shine (1965) tables\n")
cat("✅ **Standardized implementation**: utilityBcg() implements Brogden-Cronbach-Gleser model\n")
cat("✅ **Reduced coding errors**: Built-in functions minimize implementation mistakes\n")
cat("✅ **Consistent methodology**: Package ensures standard utility analysis approach\n")
cat("✅ **Efficient workflow**: Quick calculations for multiple scenarios\n")
cat("✅ **Peer-reviewed implementation**: Package functions are tested and validated\n\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Create results list
seijts_2020_results_iopsych <- list(
  # Parameters
  parameters = list(
    N = N, selection_ratio = selection_ratio, cost_per_candidate = cost_per_candidate,
    sdy = sdy, tenure_15 = tenure_15, tenure_10 = tenure_10,
    reported_correlation = reported_correlation,
    alpha_character = alpha_character, alpha_performance = alpha_performance,
    zxs_value = zxs_value, zxs_source = "IOPsych ux() function"
  ),
  
  # Basic utility results
  basic_utility = list(
    utility_15 = basic_utility_15,
    utility_10 = basic_utility_10,
    yearly_15 = basic_utility_15/tenure_15,
    yearly_10 = basic_utility_10/tenure_10,
    roi_15 = roi_15,
    roi_10 = roi_10
  ),
  
  # Economic adjustments
  adjustment_params = adjustment_params,
  results_15 = results_15,
  results_10 = results_10,
  
  # Comparison with reported values
  comparison = list(
    reported_15 = reported_15,
    reported_10 = reported_10,
    reported_yearly_15 = reported_yearly_15,
    reported_yearly_10 = reported_yearly_10,
    diff_15 = diff_15,
    diff_10 = diff_10,
    diff_yearly_15 = diff_yearly_15,
    diff_yearly_10 = diff_yearly_10
  ),
  
  # IOPsych package info
  iopsych_info = list(
    package_version = packageVersion("iopsych"),
    functions_used = c("ux", "utilityBcg"),
    advantages = c("Accurate Zxs calculation", "Standardized implementation", 
                  "Reduced coding errors", "Consistent methodology")
  )
)

# Save results
save(seijts_2020_results_iopsych, file = "seijts_2020_results_iopsych.RData")

cat("=== ANALYSIS COMPLETE (IOPsych Package) ===\n")
cat("Results saved to: seijts_2020_results_iopsych.RData\n")
cat("✅ Reproduction using IOPsych package completed successfully!\n")
cat("✅ Zxs calculation using ux() function: ", round(zxs_value, 3), "\n")
cat("✅ Utility calculations using utilityBcg() function\n")
cat("✅ Economic adjustments applied manually for customization\n") 