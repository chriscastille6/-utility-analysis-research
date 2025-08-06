# Load required libraries
library(iopsych)  # for ux() function

# Example inputs - using default values from the app
inputs <- list(
  # Selection parameters
  n = 470,          # number of hires
  sr = 0.33,        # selection ratio (33% of applicants hired)
  rxy1 = 0.1,       # validity of old procedure
  rxy2 = 0.5,       # validity of new procedure
  sdy = 16290,      # standard deviation of job performance in dollars
  period = 18,      # tenure in years
  cost1 = 200,      # cost per applicant for old procedure
  cost2 = 304.33,   # cost per applicant for new procedure
  
  # Financial adjustments
  vcost = 35,       # variable costs (%)
  tax = 63,         # tax rate (%)
  drate = 11,       # discount rate (%)
  
  # Employee flows
  pyears = 15,      # program length in years
  nadd = 470,       # employees added per year
  nsub = 470        # employees lost per year after tenure
)

# Function to calculate unadjusted utility
calculate_unadjusted_utility <- function(inputs) {
  unadjusted_utility <- inputs$n * inputs$sdy * 
    (inputs$rxy2 - inputs$rxy1) * 
    ux(inputs$sr) * 
    inputs$period - 
    (inputs$cost2 - inputs$cost1) * 
    (inputs$n / inputs$sr)
  
  return(unadjusted_utility)
}

# Function to calculate adjusted utility
calculate_adjusted_utility <- function(inputs) {
  # Convert percentages to proportions
  varCosts <- -inputs$vcost/100
  taxProp <- inputs$tax/100
  discProp <- inputs$drate/100
  discRat <- 1/(1 + discProp)
  
  # Calculate other parameters
  valid <- inputs$rxy2 - inputs$rxy1
  ord <- ux(inputs$sr)
  SDjp <- inputs$sdy
  sr1 <- inputs$sr
  tenure1 <- inputs$period
  last <- inputs$pyears
  add <- inputs$nadd
  subt <- inputs$nsub
  costOrd <- inputs$cost1
  costAc <- inputs$cost2
  ordsr1 <- dnorm(qnorm(1-sr1),0,1)
  ck <- add * ((costAc - costOrd) / sr1)
  numyr <- tenure1 + last

  # Initialize variables
  nk <- 0
  totDelta <- 0
  totDelta1 <- 0

  # Calculate adjusted utility
  for (i in 1:numyr) {
    if (i > tenure1) {nk <- nk - subt}
    if (i <= last) {nk <- nk + add}
    if (i > last) {ck <- 0}
    if (nk >= 0) {
      delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
      delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
    }
    delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
    totDelta1 <- totDelta1 + delta2 + delta3
    delta <- delta1 - delta2
    totDelta <- totDelta + delta
  }
  
  return(list(
    adjusted_utility = totDelta,
    cost_adjustment = totDelta1/last
  ))
}

# Calculate and display results
unadjusted <- calculate_unadjusted_utility(inputs)
adjusted <- calculate_adjusted_utility(inputs)

# Print results
cat("\nUtility Analysis Results:\n")
cat("------------------------\n")
cat(sprintf("Unadjusted Utility: $%.2f\n", unadjusted))
cat(sprintf("Adjusted Utility: $%.2f\n", adjusted$adjusted_utility))
cat(sprintf("Cost Adjustment: $%.2f\n", adjusted$cost_adjustment))

# Calculate per-hire and per-year values
cat("\nPer-Hire and Per-Year Values:\n")
cat("----------------------------\n")
cat(sprintf("Adjusted Utility per Hire: $%.2f\n", 
    adjusted$adjusted_utility / (inputs$pyears * inputs$nadd)))
cat(sprintf("Adjusted Utility per Hire per Year: $%.2f\n", 
    adjusted$adjusted_utility / (inputs$pyears * inputs$nadd * inputs$period))) 


# -----------------------------------------------------------------------------
# Solve for break-even SDy (economic value of performance)
# -----------------------------------------------------------------------------
solve_break_even_sdy <- function(inputs, lower = 0, upper = inputs$sdy * 10) {
  # 1) helper: returns adjusted utility for a given SDy
  utility_at_sdy <- function(sdy_val) {
    inp       <- inputs
    inp$sdy   <- sdy_val
    calculate_adjusted_utility(inp)$adjusted_utility
  }
  
  # 2) check that utility(lower) < 0 < utility(upper), otherwise expand bracket
  if (utility_at_sdy(lower) > 0) {
    stop("Utility at SDy=0 is already positive—no root below.")
  }
  while (utility_at_sdy(upper) < 0) {
    upper <- upper * 2
  }
  
  # 3) root-find on [lower, upper]
  be_sdy <- uniroot(utility_at_sdy, c(lower, upper))$root
  return(be_sdy)
}

# -----------------------------------------------------------------------------
# Example usage:
# -----------------------------------------------------------------------------
be_sdy <- solve_break_even_sdy(inputs)
cat(sprintf("Break-even SDy: $%.2f\n", be_sdy))


