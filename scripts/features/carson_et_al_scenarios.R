# Carson et al. (1998) - All Scenarios Master Script
# Source: Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. Journal of Applied Psychology, 83(1), 84–96.

# Load all individual scenario scripts
source("scripts/features/carson_latham_whyte_validity_only.R")
source("scripts/features/carson_latham_whyte_validity_utility.R")
source("scripts/features/carson_revised_validity_only.R")
source("scripts/features/carson_revised_validity_utility.R")
source("scripts/features/carson_sdy_enhanced_validity_utility.R")

# Create a list of all scenarios for easy access
carson_scenarios <- list(
  latham_whyte_validity_only = latham_whyte_validity_only,
  latham_whyte_validity_utility = latham_whyte_validity_utility,
  revised_validity_only = revised_validity_only,
  revised_validity_utility = revised_validity_utility,
  sdy_enhanced_validity_utility = sdy_enhanced_validity_utility
)

# Study Design Overview
study_design <- data.frame(
  Condition = c("Latham & Whyte Validity-Only", 
                "Latham & Whyte Validity + Utility",
                "Revised Validity-Only", 
                "Revised Validity + Utility",
                "SDy-Enhanced Revised Validity + Utility"),
  Study_1 = c("X", "X", "X", "X", ""),
  Study_2 = c("X", "X", "", "X", "X"),
  Word_Count = c(365, 1550, 485, 772, 800),
  Type = c("Replication", "Replication", "New", "New", "New"),
  Preferred = c("", "", "", "YES", "")
)

# Key Results Summary
results_summary <- data.frame(
  Condition = c("Latham & Whyte Validity-Only", 
                "Latham & Whyte Validity + Utility",
                "Revised Validity + Utility",
                "SDy-Enhanced Revised Validity + Utility"),
  Acceptability_Study2 = c(22.60, 22.12, 24.71, 24.60),
  Understandability_Study2 = c(6.04, 5.54, 6.42, 6.11),
  N_Study2 = c(47, 41, 45, 53)
)

# Statistical Significance Summary
significance_summary <- data.frame(
  Comparison = c("Revised vs. Original Utility (Acceptability)",
                 "Revised vs. Original Utility (Understandability)",
                 "Revised Utility vs. Original Validity-Only (Acceptability)",
                 "Latham & Whyte Replication (Acceptability)"),
  Study_1_p = c("p = .058 (marginal)", "p = .037*", "N/A", "p = .68 (ns)"),
  Study_2_p = c("p = .02*", "p = .019*", "p = .047*", "p = .703 (ns)")
)

# Function to display scenario information
display_scenario_info <- function(scenario_name) {
  cat("\n=== Scenario:", scenario_name, "===\n")
  cat("Word count:", length(strsplit(carson_scenarios[[scenario_name]], "\\s+")[[1]]), "\n")
  cat("First 200 characters:\n")
  cat(substr(carson_scenarios[[scenario_name]], 1, 200), "...\n")
}

# Function to compare scenarios
compare_scenarios <- function(scenario1, scenario2) {
  cat("\n=== Comparison:", scenario1, "vs", scenario2, "===\n")
  cat("Word count difference:", 
      length(strsplit(carson_scenarios[[scenario1]], "\\s+")[[1]]) - 
      length(strsplit(carson_scenarios[[scenario2]], "\\s+")[[1]]), "words\n")
  
  # Check for key differences
  has_utility1 <- grepl("utility", tolower(carson_scenarios[[scenario1]]))
  has_utility2 <- grepl("utility", tolower(carson_scenarios[[scenario2]]))
  has_validity_coef1 <- grepl("\\.40", carson_scenarios[[scenario1]])
  has_validity_coef2 <- grepl("\\.40", carson_scenarios[[scenario2]])
  
  cat("Contains utility analysis:", has_utility1, "vs", has_utility2, "\n")
  cat("Contains validity coefficients:", has_validity_coef1, "vs", has_validity_coef2, "\n")
}

# Print study overview
cat("=== CARSON ET AL. (1998) SCENARIOS LOADED ===\n")
cat("Study: Is utility really futile? A failure to replicate and an extension\n")
cat("Journal: Journal of Applied Psychology, 83(1), 84–96\n\n")

cat("Study Design:\n")
print(study_design)

cat("\nKey Results (Study 2):\n")
print(results_summary)

cat("\nStatistical Significance:\n")
print(significance_summary)

cat("\nAvailable scenarios:\n")
cat("- latham_whyte_validity_only\n")
cat("- latham_whyte_validity_utility\n") 
cat("- revised_validity_only\n")
cat("- revised_validity_utility (PREFERRED)\n")
cat("- sdy_enhanced_validity_utility\n")

cat("\nUse display_scenario_info('scenario_name') to view details\n")
cat("Use compare_scenarios('scenario1', 'scenario2') to compare\n") 