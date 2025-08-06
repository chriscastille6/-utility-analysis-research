# IOPsych PACKAGE COMPARISON
# Comparing our manual implementation with iopsych package functions

# Load required libraries
library(iopsych)
library(tidyverse)

cat("=== IOPsych PACKAGE COMPARISON ===\n")
cat("Comparing our manual implementation with iopsych package functions\n\n")

# =============================================================================
# PARAMETERS FROM SEIJTS ET AL. (2020)
# =============================================================================

# Basic parameters
N <- 1  # Number of candidates
sdy <- 115500  # CAD $115,500
rxy <- 0.30  # Correlation between character and performance
sr <- 0.33  # Selection ratio
cost <- 800  # Cost per candidate
period_15 <- 15  # 15-year tenure
period_10 <- 10  # 10-year tenure

# Reported values from Seijts et al. (2020)
reported_15 <- 564128
reported_10 <- 375285

cat("Parameters:\n")
cat("N =", N, "\n")
cat("SDy = $", format(sdy, big.mark=","), "\n")
cat("rxy =", rxy, "\n")
cat("Selection ratio =", sr, "\n")
cat("Cost = $", cost, "\n")
cat("Period (15 years) =", period_15, "\n")
cat("Period (10 years) =", period_10, "\n\n")

# =============================================================================
# IOPsych PACKAGE FUNCTIONS
# =============================================================================

cat("=== IOPsych PACKAGE RESULTS ===\n")

# Test ux() function
ux_value <- ux(sr)
cat("ux(0.33) =", round(ux_value, 3), "\n")

# Test utilityBcg() function
utility_bcg_15 <- utilityBcg(n=N, sdy=sdy, rxy=rxy, sr=sr, cost=cost, period=period_15)
utility_bcg_10 <- utilityBcg(n=N, sdy=sdy, rxy=rxy, sr=sr, cost=cost, period=period_10)

cat("utilityBcg(15 years) = $", format(round(utility_bcg_15), big.mark=","), "\n")
cat("utilityBcg(10 years) = $", format(round(utility_bcg_10), big.mark=","), "\n\n")

# =============================================================================
# OUR MANUAL IMPLEMENTATION
# =============================================================================

cat("=== OUR MANUAL IMPLEMENTATION ===\n")

# Our Zxs calculation (using Naylor & Shine tables)
zxs_manual <- 1.09

# Basic utility formula: ΔU = N × T × r × Zxs × SDy - C
basic_utility_15_manual <- N * period_15 * rxy * zxs_manual * sdy - (N * cost / sr)
basic_utility_10_manual <- N * period_10 * rxy * zxs_manual * sdy - (N * cost / sr)

cat("Zxs (manual, Naylor & Shine) =", zxs_manual, "\n")
cat("Basic utility (15 years, manual) = $", format(round(basic_utility_15_manual), big.mark=","), "\n")
cat("Basic utility (10 years, manual) = $", format(round(basic_utility_10_manual), big.mark=","), "\n\n")

# =============================================================================
# COMPARISON WITH REPORTED VALUES
# =============================================================================

cat("=== COMPARISON WITH REPORTED VALUES ===\n")

# IOPsych results
diff_bcg_15 <- utility_bcg_15 - reported_15
diff_bcg_10 <- utility_bcg_10 - reported_10

# Manual results
diff_manual_15 <- basic_utility_15_manual - reported_15
diff_manual_10 <- basic_utility_10_manual - reported_10

cat("15-year utility comparison:\n")
cat("  Reported:     $", format(reported_15, big.mark=","), "\n")
cat("  IOPsych:      $", format(round(utility_bcg_15), big.mark=","), 
    " (", round((diff_bcg_15/reported_15)*100, 1), "%)\n")
cat("  Manual:       $", format(round(basic_utility_15_manual), big.mark=","), 
    " (", round((diff_manual_15/reported_15)*100, 1), "%)\n")

cat("\n10-year utility comparison:\n")
cat("  Reported:     $", format(reported_10, big.mark=","), "\n")
cat("  IOPsych:      $", format(round(utility_bcg_10), big.mark=","), 
    " (", round((diff_bcg_10/reported_10)*100, 1), "%)\n")
cat("  Manual:       $", format(round(basic_utility_10_manual), big.mark=","), 
    " (", round((diff_manual_10/reported_10)*100, 1), "%)\n")

# =============================================================================
# INVESTIGATING THE DIFFERENCE
# =============================================================================

cat("\n=== INVESTIGATING THE DIFFERENCE ===\n")

# Let's see what Zxs value iopsych is using internally
cat("Zxs values:\n")
cat("  IOPsych ux() function:", round(ux_value, 3), "\n")
cat("  Our manual (Naylor & Shine):", zxs_manual, "\n")
cat("  Difference:", round(ux_value - zxs_manual, 3), "\n")

# Let's calculate what Zxs would be needed to get the iopsych result
# utilityBcg = N × T × r × Zxs × SDy - (N × cost / sr)
# Solving for Zxs: Zxs = (utilityBcg + N × cost / sr) / (N × T × r × SDy)

zxs_from_bcg_15 <- (utility_bcg_15 + N * cost / sr) / (N * period_15 * rxy * sdy)
zxs_from_bcg_10 <- (utility_bcg_10 + N * cost / sr) / (N * period_10 * rxy * sdy)

cat("\nZxs values derived from utilityBcg results:\n")
cat("  From 15-year result:", round(zxs_from_bcg_15, 3), "\n")
cat("  From 10-year result:", round(zxs_from_bcg_10, 3), "\n")
cat("  IOPsych ux() function:", round(ux_value, 3), "\n")

# =============================================================================
# WHY WE DIDN'T USE IOPsych INITIALLY
# =============================================================================

cat("\n=== WHY WE DIDN'T USE IOPsych INITIALLY ===\n")

cat("1. **Learning Objective**: We wanted to understand the utility analysis methodology\n")
cat("   from first principles rather than relying on a black-box function.\n\n")

cat("2. **Educational Value**: Manual implementation helps students and researchers\n")
cat("   understand exactly what calculations are being performed.\n\n")

cat("3. **Transparency**: Our reproduction documents every step of the calculation,\n")
cat("   making it easier to verify and understand the methodology.\n\n")

cat("4. **Customization**: Manual implementation allows for easy modification of\n")
cat("   parameters and formulas for specific research needs.\n\n")

cat("5. **Reproduction Standards**: Following the exact methodology described in\n")
cat("   the paper, including the specific Zxs calculation method.\n\n")

# =============================================================================
# LESSONS LEARNED
# =============================================================================

cat("=== LESSONS LEARNED ===\n")

cat("✅ **IOPsych package is excellent** for quick utility calculations\n")
cat("✅ **ux() function** provides the correct Zxs values from Naylor & Shine tables\n")
cat("✅ **utilityBcg() function** implements the Brogden-Cronbach-Gleser model\n")
cat("✅ **Manual implementation** provides educational value and transparency\n")
cat("✅ **Both approaches** are valid and complementary\n\n")

cat("**Recommendation**: Use IOPsych for quick calculations, manual implementation\n")
cat("for learning, teaching, and when you need to customize the methodology.\n\n")

# =============================================================================
# SAVE COMPARISON RESULTS
# =============================================================================

comparison_results <- list(
  parameters = list(N=N, sdy=sdy, rxy=rxy, sr=sr, cost=cost, 
                   period_15=period_15, period_10=period_10),
  iopsych_results = list(
    ux_value = ux_value,
    utility_bcg_15 = utility_bcg_15,
    utility_bcg_10 = utility_bcg_10
  ),
  manual_results = list(
    zxs_manual = zxs_manual,
    basic_utility_15 = basic_utility_15_manual,
    basic_utility_10 = basic_utility_10_manual
  ),
  reported_values = list(
    reported_15 = reported_15,
    reported_10 = reported_10
  ),
  differences = list(
    iopsych_vs_reported_15 = diff_bcg_15,
    iopsych_vs_reported_10 = diff_bcg_10,
    manual_vs_reported_15 = diff_manual_15,
    manual_vs_reported_10 = diff_manual_10
  )
)

save(comparison_results, file = "iopsych_comparison_results.RData")

cat("Results saved to: iopsych_comparison_results.RData\n")
cat("\n=== COMPARISON COMPLETE ===\n") 