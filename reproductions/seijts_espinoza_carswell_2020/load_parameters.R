# LOAD SEIJTS ET AL. (2020) PARAMETERS
# Simple script to load parameters for use in UA+ app

# Load required libraries
library(iopsych)
library(tidyverse)

# Load parameters from CSV file
params <- read.csv("seijts_2020_parameters.csv")

# Convert to named list for easy access
param_list <- setNames(params$value, params$parameter)

# Extract key parameters
N <- param_list["N"]
selection_ratio <- param_list["selection_ratio"]
cost_per_candidate <- param_list["cost_per_candidate"]
sdy <- param_list["sdy"]
tenure_15 <- param_list["tenure_15"]
tenure_10 <- param_list["tenure_10"]
reported_correlation <- param_list["reported_correlation"]

# Get Zxs value using IOPsych
zxs_value <- ux(selection_ratio)

# Calculate utility using IOPsych
utility_15 <- utilityBcg(
  n = N,
  sdy = sdy,
  rxy = reported_correlation,
  sr = selection_ratio,
  cost = cost_per_candidate,
  period = tenure_15
)

utility_10 <- utilityBcg(
  n = N,
  sdy = sdy,
  rxy = reported_correlation,
  sr = selection_ratio,
  cost = cost_per_candidate,
  period = tenure_10
)

# Display results
cat("=== SEIJTS ET AL. (2020) PARAMETERS LOADED ===\n")
cat("Parameters loaded from: seijts_2020_parameters.csv\n\n")

cat("Key Parameters:\n")
cat("N =", N, "\n")
cat("Selection Ratio =", selection_ratio, "\n")
cat("Cost per Candidate = $", format(cost_per_candidate, big.mark=","), "\n")
cat("SDy = $", format(sdy, big.mark=","), "\n")
cat("Correlation =", reported_correlation, "\n")
cat("Zxs (IOPsych) =", round(zxs_value, 3), "\n\n")

cat("Utility Results:\n")
cat("15-year utility = $", format(round(utility_15), big.mark=","), "\n")
cat("10-year utility = $", format(round(utility_10), big.mark=","), "\n")
cat("Yearly (15 years) = $", format(round(utility_15/tenure_15), big.mark=","), "\n")
cat("Yearly (10 years) = $", format(round(utility_10/tenure_10), big.mark=","), "\n\n")

cat("Reported Values (for comparison):\n")
cat("15-year reported = $", format(param_list["reported_utility_15"], big.mark=","), "\n")
cat("10-year reported = $", format(param_list["reported_utility_10"], big.mark=","), "\n")

# Save parameters as R object for easy loading in app
seijts_2020_params <- list(
  basic_params = param_list,
  zxs_value = zxs_value,
  utility_15 = utility_15,
  utility_10 = utility_10,
  yearly_15 = utility_15/tenure_15,
  yearly_10 = utility_10/tenure_10
)

save(seijts_2020_params, file = "seijts_2020_params.RData")

cat("\nParameters saved to: seijts_2020_params.RData\n")
cat("Ready for use in UA+ app!\n") 