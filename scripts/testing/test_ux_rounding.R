# Test the impact of ux() rounding precision
# Investigating whether Sturman's Zx = 1.09 vs our ux = 1.091 affects results

# Load Monte Carlo functions
source("scripts/sturman_2000_monte_carlo.R")

cat("=== TESTING UX() ROUNDING PRECISION IMPACT ===\n\n")

# Sturman's L&W parameters
n_hired <- 470
t_years <- 18
validity <- 0.4
sdy <- 16290
total_cost <- 429110
total_applicants <- 1410
selection_ratio <- n_hired / total_applicants

cat("Selection ratio:", round(selection_ratio, 3), "\n")

# Our precise ux calculation
ux_precise <- ux(selection_ratio)
cat("Our ux() precise:", ux_precise, "\n")

# Round to 2 decimal places (like Sturman)
ux_2_decimal <- round(ux_precise, 2)
cat("Our ux() rounded to 2 decimals:", ux_2_decimal, "\n")

# Round to 3 decimal places
ux_3_decimal <- round(ux_precise, 3)
cat("Our ux() rounded to 3 decimals:", ux_3_decimal, "\n")

# Sturman's reported value
sturman_zx <- 1.09
cat("Sturman's Zx:", sturman_zx, "\n\n")

# Calculate utility with different rounding levels
cat("=== UTILITY CALCULATIONS WITH DIFFERENT ROUNDING ===\n")

# With precise ux
utility_precise <- n_hired * t_years * ux_precise * validity * sdy - total_cost
cat("Utility with precise ux:", paste0("$", format(utility_precise, big.mark = ",")), "\n")

# With 2-decimal rounding
utility_2_decimal <- n_hired * t_years * ux_2_decimal * validity * sdy - total_cost
cat("Utility with 2-decimal ux:", paste0("$", format(utility_2_decimal, big.mark = ",")), "\n")

# With 3-decimal rounding  
utility_3_decimal <- n_hired * t_years * ux_3_decimal * validity * sdy - total_cost
cat("Utility with 3-decimal ux:", paste0("$", format(utility_3_decimal, big.mark = ",")), "\n")

# With Sturman's exact Zx
utility_sturman <- n_hired * t_years * sturman_zx * validity * sdy - total_cost
cat("Utility with Sturman's Zx:", paste0("$", format(utility_sturman, big.mark = ",")), "\n")

# Sturman's target
sturman_target <- 59657532
cat("Sturman's reported result:", paste0("$", format(sturman_target, big.mark = ",")), "\n\n")

# Calculate differences
cat("=== DIFFERENCES FROM STURMAN'S TARGET ===\n")
cat("Precise ux difference:", paste0("$", format(utility_precise - sturman_target, big.mark = ",")), 
    " (", round(100 * (utility_precise - sturman_target) / sturman_target, 4), "%)\n")
cat("2-decimal ux difference:", paste0("$", format(utility_2_decimal - sturman_target, big.mark = ",")), 
    " (", round(100 * (utility_2_decimal - sturman_target) / sturman_target, 4), "%)\n")
cat("3-decimal ux difference:", paste0("$", format(utility_3_decimal - sturman_target, big.mark = ",")), 
    " (", round(100 * (utility_3_decimal - sturman_target) / sturman_target, 4), "%)\n")
cat("Sturman Zx difference:", paste0("$", format(utility_sturman - sturman_target, big.mark = ",")), 
    " (", round(100 * (utility_sturman - sturman_target) / sturman_target, 4), "%)\n\n")

# Check if 2-decimal rounding matches Sturman exactly
cat("=== ROUNDING ANALYSIS ===\n")
cat("Does our 2-decimal ux match Sturman's Zx?", ux_2_decimal == sturman_zx, "\n")
cat("Does our 3-decimal ux match Sturman's Zx?", ux_3_decimal == sturman_zx, "\n")

if (ux_2_decimal == sturman_zx) {
  cat("✅ SUCCESS: 2-decimal rounding matches Sturman's Zx exactly!\n")
  cat("This suggests Sturman rounded ux to 2 decimal places.\n")
} else {
  cat("❌ 2-decimal rounding does not match Sturman's Zx\n")
  cat("Difference:", ux_2_decimal - sturman_zx, "\n")
}

# Test the impact on our Monte Carlo simulation
cat("\n=== IMPACT ON MONTE CARLO SIMULATION ===\n")
cat("If we modify our ux() function to round to 2 decimals,\n")
cat("it could affect all 10,000 simulations and potentially\n")
cat("bring our results closer to Sturman's 291% target.\n")

# Show the magnitude of the effect
effect_per_simulation <- utility_2_decimal - utility_precise
cat("Effect per simulation:", paste0("$", format(effect_per_simulation, big.mark = ",")), "\n")
cat("This could compound across 10,000 simulations.\n") 