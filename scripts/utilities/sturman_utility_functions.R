# ============================================================================
# STURMAN (2000) STANDARDIZED UTILITY FUNCTIONS LIBRARY
# ============================================================================
# This library provides standardized implementations of all utility calculations
# to ensure consistency across all scripts and analyses.

# Standard normal ordinate function with 2-decimal precision (matches Sturman)
ux <- function(selection_ratio) {
  round(dnorm(qnorm(1 - selection_ratio)) / selection_ratio, 2)
}

# Basic utility calculation (Brogden-Cronbach-Gleser formula)
calculate_basic_utility <- function(n, t, sr, r, sdy, cost) {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
}

# Economic adjustments (Boudreau 1983a approach)
# Uses loop-based present value calculation and benefits-taxable approach
calculate_economic_utility <- function(n, t, sr, r, sdy, cost, discount, tax, vc) {
  # Annual benefit and variable costs
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation using loop method (most accurate)
  pv_benefits <- sapply(1:length(n), function(i) {
    pv <- 0
    years <- pmax(1, round(t[i]))
    for(year in 1:years) {
      pv <- pv + (annual_net_benefit[i] / (1 + discount[i])^year)
    }
    return(pv)
  })
  
  # After-tax present value minus initial costs (benefits are taxable)
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  
  after_tax_pv - total_costs
}

# Multiple selection devices adjustment (incremental validity)
calculate_multiple_devices_utility <- function(n, t, sr, r, r_old, sdy, cost) {
  r_incremental <- r - r_old
  n * t * ux(sr) * r_incremental * sdy - (n/sr) * cost
}

# Latham & Whyte replication function
calculate_lw_replication <- function() {
  # Sturman's exact L&W parameters
  n_hired <- 470
  t_years <- 18
  selection_ratio <- 470/1410  # 0.333
  validity <- 0.4
  sdy <- 16290
  total_cost <- 429110
  
  # Calculate basic utility
  basic_utility <- n_hired * t_years * ux(selection_ratio) * validity * sdy - total_cost
  
  return(list(
    basic_utility = basic_utility,
    sturman_target = 59657532,
    difference = basic_utility - 59657532,
    match = abs(basic_utility - 59657532) < 1
  ))
}

# Validation function
validate_implementation <- function() {
  cat("=== VALIDATING STANDARDIZED IMPLEMENTATION ===\n\n")
  
  # Test L&W replication
  lw_result <- calculate_lw_replication()
  cat("Latham & Whyte Replication Test:\n")
  cat("Our result:", paste0("$", format(lw_result$basic_utility, big.mark = ",")), "\n")
  cat("Sturman target:", paste0("$", format(lw_result$sturman_target, big.mark = ",")), "\n")
  cat("Difference:", paste0("$", format(lw_result$difference, big.mark = ",")), "\n")
  cat("Perfect match:", lw_result$match, "\n\n")
  
  if (lw_result$match) {
    cat("✅ SUCCESS: L&W replication validated!\n\n")
  } else {
    cat("❌ FAILURE: L&W replication needs adjustment!\n\n")
  }
  
  cat("=== VALIDATION COMPLETE ===\n")
}

# Print library information
cat("Sturman (2000) Standardized Utility Functions Library Loaded\n")
cat("Use validate_implementation() to test the functions\n")
