# Test Boudreau (1983) Exact Economic Adjustment Formulas
# Implementing the precise formulas from the original 1983 paper

# Load standardized functions for comparison
source("../utilities/sturman_utility_functions.R")

cat("=== TESTING BOUDREAU (1983) EXACT FORMULAS ===\n\n")

# Boudreau's exact economic adjustment formula (Equation 12)
calculate_boudreau_economic_utility <- function(n, t, sr, r, sdy, cost, discount, tax, vc) {
  # Basic annual benefit
  annual_benefit <- n * r * ux(sr) * sdy
  
  # Apply variable costs: SD_y * (1 + V) where V is negative for costs that increase with performance
  # Note: Boudreau uses V as proportion, where negative V means costs increase with performance
  # Our 'vc' parameter is already negative, so we use (1 + vc) directly
  adjusted_annual_benefit <- annual_benefit * (1 + vc)
  
  # Apply tax to benefits: (1 - TAX)
  after_tax_annual_benefit <- adjusted_annual_benefit * (1 - tax)
  
  # Present value calculation - Boudreau's exact year-by-year discounting
  pv_benefits <- 0
  for(year in 1:max(1, round(t))) {
    pv_benefits <- pv_benefits + (after_tax_annual_benefit / (1 + discount)^year)
  }
  
  # Apply tax to costs: C * (1 - TAX)
  after_tax_costs <- (n/sr) * cost * (1 - tax)
  
  # Final utility
  pv_benefits - after_tax_costs
}

# Test with Boudreau's suggested parameter ranges
set.seed(42)
n_sims <- 10000

cat("Generating", n_sims, "scenarios with Boudreau's parameter ranges...\n")

# Boudreau's suggested ranges from his paper
params <- data.frame(
  # Basic parameters (using Sturman's ranges)
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.77),
  sdy = runif(n_sims, 4000, 40000),
  cost = exp(runif(n_sims, log(1), log(1100))),
  
  # Boudreau's economic parameter ranges
  discount = runif(n_sims, 0.05, 0.15),  # Boudreau: 5% to 15%
  tax = runif(n_sims, 0.20, 0.55),       # Boudreau: 20% to 55%
  vc = runif(n_sims, -0.50, -0.15)       # Boudreau: V from -0.50 to -0.15 (costs increase with performance)
)

cat("Calculating utilities with Boudreau's exact formulas...\n")

# Basic utility (standardized)
params$utility_basic <- with(params, calculate_basic_utility(n, t, sr, r, sdy, cost))

# Our current economic utility (standardized)
params$utility_our_economic <- with(params, calculate_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc))

# Boudreau's exact economic utility
params$utility_boudreau_economic <- with(params, calculate_boudreau_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc))

# Calculate percentage changes
params$pct_change_our <- with(params, 100 * (utility_basic - utility_our_economic) / abs(utility_basic))
params$pct_change_boudreau <- with(params, 100 * (utility_basic - utility_boudreau_economic) / abs(utility_basic))

# Results
our_median <- median(params$pct_change_our, na.rm = TRUE)
boudreau_median <- median(params$pct_change_boudreau, na.rm = TRUE)
boudreau_negative_pct <- sum(params$utility_boudreau_economic < 0, na.rm = TRUE) / nrow(params) * 100

cat("\n=== RESULTS COMPARISON ===\n")
cat("Our current economic formula median reduction:", round(our_median, 1), "%\n")
cat("Boudreau's exact formula median reduction:", round(boudreau_median, 1), "%\n")
cat("Difference:", round(boudreau_median - our_median, 1), "pp\n")
cat("Boudreau formula negative cases:", round(boudreau_negative_pct, 1), "%\n")

cat("\n=== COMPARISON TO STURMAN'S TARGETS ===\n")
cat("Boudreau exact formula:\n")
cat("  Median reduction:", round(boudreau_median, 1), "%\n")
cat("  Sturman target: 291%\n")
cat("  Gap:", round(abs(boudreau_median - 291), 1), "pp\n")

if (boudreau_median > 200) {
  cat("🎉 BREAKTHROUGH: Boudreau's exact formulas approach the 291% target!\n")
} else if (boudreau_median > 150) {
  cat("📈 SIGNIFICANT PROGRESS: Much closer to 291% target!\n")
} else if (boudreau_median > our_median + 20) {
  cat("✅ IMPROVEMENT: Boudreau's formulas show meaningful difference\n")
} else {
  cat("📊 MODEST DIFFERENCE: Similar to our current approach\n")
}

# Test extreme parameter combinations
cat("\n=== TESTING EXTREME BOUDREAU SCENARIOS ===\n")

extreme_params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 5, 10),  # Longer tenure
  sr = runif(n_sims, 0.05, 0.3),  # Lower selection ratios
  r = runif(n_sims, 0.3, 0.77),  # Higher validity
  sdy = runif(n_sims, 20000, 40000),  # Higher SDy
  cost = exp(runif(n_sims, log(100), log(1100))),  # Higher costs
  
  # Extreme economic parameters
  discount = runif(n_sims, 0.10, 0.15),  # Higher discount rates
  tax = runif(n_sims, 0.45, 0.55),       # Higher tax rates  
  vc = runif(n_sims, -0.50, -0.30)       # Higher variable costs
)

extreme_params$utility_basic <- with(extreme_params, calculate_basic_utility(n, t, sr, r, sdy, cost))
extreme_params$utility_boudreau <- with(extreme_params, calculate_boudreau_economic_utility(n, t, sr, r, sdy, cost, discount, tax, vc))
extreme_params$pct_change_extreme <- with(extreme_params, 100 * (utility_basic - utility_boudreau) / abs(utility_basic))

extreme_median <- median(extreme_params$pct_change_extreme, na.rm = TRUE)
extreme_negative_pct <- sum(extreme_params$utility_boudreau < 0, na.rm = TRUE) / nrow(extreme_params) * 100

cat("Extreme scenario median reduction:", round(extreme_median, 1), "%\n")
cat("Extreme scenario negative cases:", round(extreme_negative_pct, 1), "%\n")
cat("Gap from 291% target:", round(abs(extreme_median - 291), 1), "pp\n")

if (extreme_median > 250) {
  cat("🚀 JACKPOT: Extreme Boudreau scenarios very close to 291%!\n")
} else if (extreme_median > 200) {
  cat("🎯 EXCELLENT: Extreme scenarios show major progress toward 291%!\n")
} else {
  cat("📊 INVESTIGATION: Even extreme scenarios don't reach 291%\n")
}

cat("\n=== BOUDREAU FORMULA ANALYSIS COMPLETE ===\n") 