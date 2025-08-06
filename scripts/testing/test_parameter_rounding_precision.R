# Test Parameter Rounding Precision Based on Sturman's Table 1
# Examining which parameters might use 2-decimal vs 3+ decimal precision

source("scripts/sturman_2000_monte_carlo.R")

cat("=== ANALYZING STURMAN'S TABLE 1 PARAMETER PRECISION ===\n\n")

# Based on Table 1, identify parameters that might be subject to rounding precision
cat("Parameter Analysis from Sturman's Table 1:\n\n")

cat("BASIC PARAMETERS (Adjustment 0):\n")
cat("1. Number of years: 1 to 10 (integers - no rounding issue)\n")
cat("2. Number of people selected: 1 to 1100 (integers - no rounding issue)\n") 
cat("3. Applicants per hire: 1 to 50 (integers - no rounding issue)\n")
cat("4. SDy: $4,000 to $40,000 (thousands - no rounding issue)\n")
cat("5. Cost per applicant: $0 to $1100 (dollars - no rounding issue)\n")
cat("6. Validity of new battery: 0.05 to 0.38 above old (0.10 to 0.77) ⚠️ DECIMAL PRECISION\n\n")

cat("ECONOMIC ADJUSTMENTS (Adjustment 1):\n")
cat("7. Discount rate: 0.01 to 0.11 ⚠️ DECIMAL PRECISION\n")
cat("8. Tax rate: 0.30 to 0.63 ⚠️ DECIMAL PRECISION\n")
cat("9. Variable cost rate: -0.02 to -0.35 ⚠️ DECIMAL PRECISION\n\n")

cat("EMPLOYEE FLOWS (Adjustment 2):\n")
cat("10. Probability of turnover: 0.00 to 0.33 ⚠️ DECIMAL PRECISION\n")
cat("11. Correlation between performance and turnover: 0.00 to -0.50 ⚠️ DECIMAL PRECISION\n\n")

cat("OTHER ADJUSTMENTS:\n")
cat("12. Stability of performance over time: 0.50 to 1.00 ⚠️ DECIMAL PRECISION\n")
cat("13. Cutoff for probationary period: 0 to -2 (Z-scores - may have decimal precision)\n")
cat("14. Validity of old battery: 0.05 to 0.38 ⚠️ DECIMAL PRECISION\n")
cat("15. Percent accepting initial offer: 20% to 70% (may be stored as 0.20 to 0.70) ⚠️ DECIMAL PRECISION\n")
cat("16. Correlation between probability of accepting and performance: 0.00 to -0.50 ⚠️ DECIMAL PRECISION\n\n")

# Test specific parameter rounding scenarios
cat("=== TESTING PARAMETER ROUNDING SCENARIOS ===\n\n")

# Test validity coefficients (most critical for utility calculations)
cat("1. VALIDITY COEFFICIENTS:\n")
test_validities <- c(0.105, 0.234, 0.387, 0.456, 0.678)
for(v in test_validities) {
  v_2dec <- round(v, 2)
  v_3dec <- round(v, 3)
  cat("  Original:", v, "→ 2-decimal:", v_2dec, "→ 3-decimal:", v_3dec, "\n")
}

# Test discount rates
cat("\n2. DISCOUNT RATES:\n")
test_discounts <- c(0.056, 0.087, 0.094, 0.103)
for(d in test_discounts) {
  d_2dec <- round(d, 2)
  d_3dec <- round(d, 3)
  cat("  Original:", d, "→ 2-decimal:", d_2dec, "→ 3-decimal:", d_3dec, "\n")
}

# Test tax rates
cat("\n3. TAX RATES:\n")
test_taxes <- c(0.305, 0.387, 0.456, 0.623)
for(t in test_taxes) {
  t_2dec <- round(t, 2)
  t_3dec <- round(t, 3)
  cat("  Original:", t, "→ 2-decimal:", t_2dec, "→ 3-decimal:", t_3dec, "\n")
}

# Test correlations
cat("\n4. CORRELATION COEFFICIENTS:\n")
test_corrs <- c(-0.105, -0.234, -0.387, -0.456)
for(c in test_corrs) {
  c_2dec <- round(c, 2)
  c_3dec <- round(c, 3)
  cat("  Original:", c, "→ 2-decimal:", c_2dec, "→ 3-decimal:", c_3dec, "\n")
}

cat("\n=== IMPACT ANALYSIS ===\n")

# Simulate the impact of rounding different parameters
set.seed(42)
n_test <- 1000

# Generate test parameters
test_params <- data.frame(
  r = runif(n_test, 0.10, 0.77),
  r_old = runif(n_test, 0.05, 0.38),
  discount = runif(n_test, 0.01, 0.11),
  tax = runif(n_test, 0.30, 0.63),
  vc = runif(n_test, -0.35, -0.02),  # Variable cost (negative means cost reduction)
  corr_perf_turn = runif(n_test, -0.50, 0.00)
)

# Apply different rounding strategies
test_params$r_2dec <- round(test_params$r, 2)
test_params$r_3dec <- round(test_params$r, 3)
test_params$discount_2dec <- round(test_params$discount, 2)
test_params$tax_2dec <- round(test_params$tax, 2)

# Calculate impact on a simple utility calculation
test_params$utility_precise <- with(test_params, {
  1000 * 5 * ux(0.2) * r * 20000 * (1 - tax) / (1 + discount)
})

test_params$utility_2dec <- with(test_params, {
  1000 * 5 * ux(0.2) * r_2dec * 20000 * (1 - tax_2dec) / (1 + discount_2dec)
})

# Calculate differences
diff_mean <- mean(abs(test_params$utility_precise - test_params$utility_2dec))
diff_max <- max(abs(test_params$utility_precise - test_params$utility_2dec))
diff_pct <- mean(abs(test_params$utility_precise - test_params$utility_2dec) / abs(test_params$utility_precise)) * 100

cat("Impact of 2-decimal rounding on", n_test, "test calculations:\n")
cat("  Mean absolute difference: $", format(diff_mean, big.mark = ","), "\n")
cat("  Maximum absolute difference: $", format(diff_max, big.mark = ","), "\n")
cat("  Mean percentage difference:", round(diff_pct, 3), "%\n\n")

cat("=== RECOMMENDATIONS ===\n")
cat("Parameters that likely used 2-decimal precision in Sturman's simulation:\n")
cat("✓ ux() values (already implemented)\n")
cat("? Validity coefficients (r, r_old)\n")
cat("? Discount rates\n") 
cat("? Tax rates\n")
cat("? Variable cost rates\n")
cat("? Correlation coefficients\n\n")

cat("Next steps:\n")
cat("1. Test implementing 2-decimal rounding for validity coefficients\n")
cat("2. Test implementing 2-decimal rounding for economic parameters\n")
cat("3. Run full 10K simulation with additional rounding\n")
cat("4. Compare results to see if we get closer to 291% target\n") 