#### Utility Analysis Test Script ####

#### Required Libraries ####
library(iopsych)

#### Input Parameters ####
#' Basic utility parameters
n      <- 470     # number of hires
sdy    <- 16290   # SD of performance in dollars
rxy1   <- 0.10    # validity of old procedure
rxy2   <- 0.50    # validity of new procedure
sr     <- 0.33    # selection ratio (as decimal)
cost1  <- 200     # cost per applicant of old procedure
cost2  <- 304.33  # cost per applicant of new procedure
period <- 18      # years of tenure

#### UNADJUSTED UTILITY ANALYSIS ####

#### Unadjusted Utility Calculation ####
#' 1. Critical z for top 33%
z_crit   <- qnorm(1 - sr)

#' 2. Mean of top 33% of the standard normal (rounded down to 2 decimal places)
mean_top <- floor(dnorm(z_crit) / sr * 100) / 100

#' 3. Effect size in SD‐units (difference between procedures)
delta_rxy <- rxy2 - rxy1

# Calculate unadjusted utility
total_applicants <- ceiling(n / sr)  # Use selection ratio as decimal
cost_diff <- cost2 - cost1
cost_term <- floor(total_applicants * cost_diff)  # Round down the total cost difference

# Debug printing
cat(sprintf("\nDebug values:\n"))
cat(sprintf("Number of hires (n): %d\n", n))
cat(sprintf("Selection ratio (sr): %.2f\n", sr))
cat(sprintf("Total applicants: %d\n", total_applicants))
cat(sprintf("Cost difference per applicant: $%.2f\n", cost_diff))
cat(sprintf("Cost term: $%.0f\n", cost_term))

# Calculate utility using same method as main app
uxs <- floor(ux(sr) * 100) / 100  # Round down to 2 decimal places
unadjusted_utility <- n * sdy * delta_rxy * uxs * period - cost_term

# Print unadjusted utility
cat(sprintf("\nUnadjusted Utility: $%s\n", format(unadjusted_utility, big.mark=",", scientific=FALSE)))

#### Unadjusted Break-Even SDy Analysis ####
# Function to calculate unadjusted utility for a given SDy
calculate_unadjusted_utility <- function(sdy_val) {
  period * n * mean_top * delta_rxy * sdy_val - cost_term
}

# Function to solve for unadjusted break-even SDy
solve_unadjusted_break_even_sdy <- function(lower = 0, upper = sdy * 10) {
  # Check bounds and expand if needed
  if (calculate_unadjusted_utility(lower) > 0) {
    stop("Utility at SDy=0 is already positive—no root below.")
  }
  while (calculate_unadjusted_utility(upper) < 0) {
    upper <- upper * 2
  }
  
  # Find break-even SDy
  be_sdy <- uniroot(calculate_unadjusted_utility, c(lower, upper))$root
  return(be_sdy)
}

# Calculate unadjusted break-even SDy
unadjusted_break_even_sdy <- solve_unadjusted_break_even_sdy()
cat(sprintf("Unadjusted Break-even SDy: $%.2f\n", unadjusted_break_even_sdy))

#### Unadjusted Break-Even Validity Analysis ####
#' Compute the unadjusted break-even incremental validity (Δr)
#' For unadjusted: Utility = n * sdy * ux(sr) * period * Δr - cost_term
#' At break-even: n * sdy * ux(sr) * period * Δr = cost_term
ux_sr <- 1.09  # ux(sr) multiplier
delta_r_unadjusted <- cost_term / (n * sdy * ux_sr * period)

cat(sprintf("Unadjusted Break-even Δr: %.6f (i.e. %.3f%%)\n", 
            delta_r_unadjusted, delta_r_unadjusted * 100))

#### Unadjusted Star Power Analysis ####
#' Baseline per-hire benefit & NNH (unadjusted)
uxs_unadjusted <- floor(ux(sr) * 100) / 100  # Round down to 2 decimal places
B_hire_unadjusted <- sdy * delta_rxy * uxs_unadjusted * period
NNH_unadjusted <- ceiling(cost_term / B_hire_unadjusted)

#' Power-law uplift scenario (from Joo et al., 1.74×)
tail_multiplier <- 1.74
B_hire_pl_unadjusted <- B_hire_unadjusted * tail_multiplier
NNH_pl_unadjusted <- ceiling(NNH_unadjusted / tail_multiplier)  # Correct calculation

#' Print unadjusted results
cat(sprintf("\nUnadjusted Star Power Analysis:\n"))
cat(sprintf("Baseline per-hire benefit (unadjusted): $%.0f\n", B_hire_unadjusted))
cat(sprintf("NNH (Normal-tail, unadjusted): %d hires\n", NNH_unadjusted))
cat(sprintf("Power-law per-hire benefit (unadjusted): $%.0f  (×%.2f)\n",
            B_hire_pl_unadjusted, tail_multiplier))
cat(sprintf("NNH (Power-law tail, unadjusted): %d hires\n\n", NNH_pl_unadjusted))

#### ADJUSTED UTILITY ANALYSIS ####

#### Financial Adjustment Parameters ####
v      <- 0.35    # variable costs proportion
tax    <- 0.63    # corporate tax rate
i      <- 0.11    # discount rate

#### Break-Even Analysis Functions ####
#' Function to calculate adjusted utility for a given set of inputs
calculate_adjusted_utility <- function(inputs) {
  # Calculate basic components using the same approach as the app
  uxs <- floor(ux(inputs$sr) * 100) / 100  # Round down to 2 decimal places like the app
  ES <- (inputs$rxy2 - inputs$rxy1) * uxs  # Use delta_rxy * uxs like the app
  B0_raw <- inputs$sdy * ES
  
  # Apply financial adjustments to benefits only
  B0_after_vc <- B0_raw * (1 - inputs$v)
  B0_after_tax <- B0_after_vc * (1 - inputs$tax)
  pv_factor <- (1 - (1 + inputs$i)^(-inputs$period)) / inputs$i
  pv_benefit_per <- B0_after_tax * pv_factor
  total_pv_benefit <- inputs$n * pv_benefit_per
  
  # For break-even analysis, use original cost term (not after-tax)
  # This is because we want to know what SDy is needed to overcome the original cost
  original_cost <- inputs$cost
  
  # Return adjusted utility
  return(list(
    adjusted_utility = total_pv_benefit - original_cost,
    benefit_term = total_pv_benefit,
    cost_term = original_cost
  ))
}

#### Financially Adjusted Utility Calculation ####
#' Calculate basic components first
ES <- delta_rxy * mean_top  # Effect size
B0_raw <- sdy * ES          # Raw benefit per hire

#' 1. Adjust for variable costs (reduces benefit)
B0_after_vc <- B0_raw * (1 - v)

#' 2. Adjust for taxes (reduces benefit)
B0_after_tax <- B0_after_vc * (1 - tax)

#' 3. Calculate present value factor (reduces future benefits)
pv_factor <- (1 - (1 + i)^(-period)) / i

#' 4. Calculate present value of benefits per hire
pv_benefit_per <- B0_after_tax * pv_factor

#' 5. Total present value benefit for all hires
total_pv_benefit <- n * pv_benefit_per

#' 6. After‐tax cost (total cost difference)
net_cost <- cost_term * (1 - tax)

#' 7. Net present value (final adjusted utility)
NPV <- total_pv_benefit - net_cost

# Calculate percentage reduction
pct_reduction <- (1 - (NPV / unadjusted_utility)) * 100

# Print financially adjusted utility and reduction
cat(sprintf("Financially Adjusted Utility: $%s\n", format(NPV, big.mark=",", scientific=FALSE)))
cat(sprintf("Financial Adjustments Reduce Utility by: %.1f%%\n", pct_reduction))

#### Adjusted Break-Even SDy Analysis ####
# Use the direct formula approach like the app (which is correct)
financial_adjustment <- (1 - v) * (1 - tax) * ((1 - (1 + i)^(-period)) / i)
uxs <- floor(ux(sr) * 100) / 100  # Round down to 2 decimal places

# Direct calculation: SDy = Cost Term / (n × Δr × ux(sr) × period × Financial Adjustment)
adjusted_break_even_sdy <- cost_term / (n * delta_rxy * uxs * period * financial_adjustment)
cat(sprintf("Adjusted Break-even SDy: $%.2f\n", adjusted_break_even_sdy))

#### Internal Rate of Return (IRR) ####
# Function to calculate utility at a given interest rate with financial adjustments
utility_at_r_adjusted <- function(r) {
  pv_factor <- if(r == 0) period else (1 - (1 + r)^(-period)) / r
  B0_after_vc <- B0_raw * (1 - v)  # Adjust for variable costs
  B0_after_tax <- B0_after_vc * (1 - tax)  # Adjust for taxes
  pv_benefits <- n * B0_after_tax * pv_factor
  pv_benefits - net_cost
}

# Find IRR where NPV = 0
low_irr  <- 0.0001  # Start slightly above 0 to avoid division by zero
high_irr <- 0.10
while (utility_at_r_adjusted(high_irr) > 0) {
  high_irr <- high_irr * 2
  if (high_irr > 1e6) stop("IRR exceeds 1,000,000%—check your inputs")
}

# Calculate Internal Rate of Return (IRR)
IRR <- uniroot(utility_at_r_adjusted, c(low_irr, high_irr))$root
cat(sprintf("Internal Rate of Return (IRR): %.1f%%\n", IRR * 100))

#### Adjusted Break-Even Validity Analysis ####
#' Calculate financial adjustment factor
financial_adjustment <- (1 - v) * (1 - tax) * ((1 - (1 + i)^(-period)) / i)

#' Compute the adjusted break-even incremental validity (Δr)
delta_r_adjusted <- cost_term / (n * sdy * ux_sr * period * financial_adjustment)

cat(sprintf("Adjusted Break-even Δr: %.6f (i.e. %.3f%%)\n", 
            delta_r_adjusted, delta_r_adjusted * 100))

#### Number Needed to Hire (NNH) Calculation ####
# Calculate NNH using the same approach as appv03.R
uxs <- floor(ux(sr) * 100) / 100  # Round down to 2 decimal places, same as appv03.R
financial_adjustment <- (1 - v) * (1 - tax) * ((1 - (1 + i)^(-period)) / i)

# Calculate adjusted NNH directly
NNH <- ceiling(cost_term / (sdy * delta_rxy * uxs * period * financial_adjustment))
cat(sprintf("Adjusted Number Needed to Hire (NNH): %d hires\n", NNH))

#### Adjusted Star Power Analysis ####
#' Baseline per-hire benefit & NNH (adjusted)
B_hire_norm <- sdy * delta_rxy * uxs * period * financial_adjustment
NNH_norm <- ceiling(cost_term / B_hire_norm)

#' Power-law uplift scenario (from Joo et al., 1.74×)
B_hire_pl <- B_hire_norm * tail_multiplier
NNH_pl <- ceiling(NNH_norm / tail_multiplier)  # Correct calculation

#' Print adjusted results
cat(sprintf("\nAdjusted Star Power Analysis:\n"))
cat(sprintf("Baseline per-hire benefit (adjusted): $%.0f\n", B_hire_norm))
cat(sprintf("NNH (Normal-tail, adjusted): %d hires\n", NNH_norm))
cat(sprintf("Power-law per-hire benefit (adjusted): $%.0f  (×%.2f)\n",
            B_hire_pl, tail_multiplier))
cat(sprintf("NNH (Power-law tail, adjusted): %d hires\n\n", NNH_pl))

#### Results Summary ####
cat(sprintf("\n=== COMPARISON SUMMARY ===\n"))
cat(sprintf("Unadjusted Utility: $%s\n", format(unadjusted_utility, big.mark=",", scientific=FALSE)))
cat(sprintf("Adjusted Utility: $%s\n", format(NPV, big.mark=",", scientific=FALSE)))
cat(sprintf("Reduction due to adjustments: %.1f%%\n\n", pct_reduction))

cat(sprintf("Break-even SDy:\n"))
cat(sprintf("  Unadjusted: $%.2f\n", unadjusted_break_even_sdy))
cat(sprintf("  Adjusted: $%.2f\n", adjusted_break_even_sdy))

cat(sprintf("\nBreak-even Validity (Δr):\n"))
cat(sprintf("  Unadjusted: %.6f (%.3f%%)\n", delta_r_unadjusted, delta_r_unadjusted * 100))
cat(sprintf("  Adjusted: %.6f (%.3f%%)\n", delta_r_adjusted, delta_r_adjusted * 100))

cat(sprintf("\nStar Power NNH (Normal-tail):\n"))
cat(sprintf("  Unadjusted: %d hires\n", NNH_unadjusted))
cat(sprintf("  Adjusted: %d hires\n", NNH_norm))

cat(sprintf("\nStar Power NNH (Power-law tail):\n"))
cat(sprintf("  Unadjusted: %d hires\n", NNH_pl_unadjusted))
cat(sprintf("  Adjusted: %d hires\n", NNH_pl))

#### MULTIPLE COHORTS ####

# ─── Parameters ───────────────────────────────────────────────────────────────
program_length           <- 15    # years of the hiring program
period                   <- 18    # employee tenure (years)
horizon                  <- program_length + period

employees_added_per_year <- 470   # hires each year

# B–C–G & financial inputs
sdy      <- 16290    # $ per SD of performance
r_old    <- 0.20     # old validity
r_new    <- 0.40     # new validity
sr       <- 0.33     # selection ratio
cost_old <- 200      # $ per applicant, old
cost_new <- 304.33   # $ per applicant, new

vc       <- 0.35     # variable cost rate
tax      <- 0.63     # tax rate
dr       <- 0.11     # discount rate

# ─── Helper functions ───────────────────────────────────────────────────────
pvf <- function(Y) (1 - (1 + dr)^(-Y)) / dr

z_cutoff <- qnorm(1 - sr)
Zx       <- dnorm(z_cutoff) / sr

npv_cohort <- function(n_hires, years) {
  n_app      <- ceiling(n_hires / sr)
  delta_r    <- r_new - r_old
  cost_term  <- n_app * (cost_new - cost_old)
  raw_ben    <- n_hires * sdy * delta_r * Zx * years
  ben_vc     <- raw_ben * (1 - vc)
  ben_taxed  <- ben_vc  * (1 - tax)
  pv_ben     <- ben_taxed * pvf(years)
  cost_after <- cost_term * (1 - tax)
  pv_ben - cost_after
}

# ─── Calculate lifetime NPV ─────────────────────────────────────────────────
cohort_npvs <- sapply(
  1:program_length,
  function(t) {
    yrs_left <- horizon - t + 1
    npv_cohort(employees_added_per_year, yrs_left)
  }
)

total_lifetime_npv <- sum(cohort_npvs)

# ─── Output ──────────────────────────────────────────────────────────────────
cat("Lifetime incremental NPV (", horizon, "-year horizon): $",
    format(round(total_lifetime_npv, 0), big.mark = ","), "\n", sep = "")

