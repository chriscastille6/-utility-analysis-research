# Generate Reference Dataset for Sturman (2000) Monte Carlo Analysis
# This creates a reproducible dataset of 10,000 samples using our verified implementation

library(dplyr)

cat("Generating Sturman (2000) Reference Dataset...\n")
cat("==============================================\n")

# Set seed for reproducibility
set.seed(42)
n_sims <- 10000

cat("Number of simulations:", n_sims, "\n")
cat("Random seed: 42\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Generate parameters using our verified ranges
cat("Generating parameters...\n")
params <- data.frame(
  # Basic utility parameters
  n = round(exp(runif(n_sims, log(1), log(1100)))),     # Number of people (exponential)
  t = runif(n_sims, 1, 10),                             # Tenure in years
  sr = runif(n_sims, 0.05, 1.0),                        # Selection ratio
  r = runif(n_sims, 0.10, 0.70),                        # Validity of new device
  sdy = runif(n_sims, 5000, 50000),                     # Standard deviation of performance
  cost = exp(runif(n_sims, log(10), log(1000))),        # Cost per applicant (exponential)
  
  # Economic adjustment parameters
  discount = runif(n_sims, 0.05, 0.15),                 # Discount rate
  tax = runif(n_sims, 0.20, 0.40),                      # Tax rate
  vc = runif(n_sims, 0.10, 0.30),                       # Variable cost rate
  
  # Multiple devices adjustment
  r_old = runif(n_sims, 0.05, 0.38),                    # Validity of old device
  
  # Top-down hiring deviations
  reject_rate = runif(n_sims, 0.10, 0.40),              # Job offer rejection rate
  corr_perf_accept = runif(n_sims, -0.3, -0.1),         # Correlation performance-acceptance
  
  # Probationary period
  prob_cutoff = runif(n_sims, -1.5, -0.5),              # Probationary cutoff (z-score)
  
  # Employee flows
  turnover_rate = runif(n_sims, 0.05, 0.25),            # Annual turnover rate
  perf_turn_corr = runif(n_sims, -0.3, -0.1)            # Performance-turnover correlation
)

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

cat("Calculating utility estimates...\n")

# Basic utility (no adjustments)
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# Adjustment 1: Economic factors
params$utility_economic <- with(params, {
  annual_benefit <- n * r * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  after_tax_pv - total_costs
})

# Adjustment 2: Multiple selection devices
params$utility_multiple <- with(params, {
  r_net <- sqrt(r^2 + r_old^2 - 2*r*r_old*0) # Assuming zero correlation
  annual_benefit <- n * r_net * ux(sr) * sdy
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  total_costs <- (n/sr) * cost
  after_tax_pv - total_costs
})

# Adjustment 3: Top-down hiring deviations
params$utility_topdown <- with(params, {
  # Start with multiple devices result
  base_utility <- utility_multiple
  
  # Adjust for rejection and suboptimal hiring
  rejection_loss <- base_utility * reject_rate * 0.5
  performance_loss <- base_utility * abs(corr_perf_accept) * 0.3
  
  base_utility - rejection_loss - performance_loss
})

# Adjustment 4: Probationary period
params$utility_probation <- with(params, {
  # Start with top-down result
  base_utility <- utility_topdown
  
  # Calculate probationary dismissal rate
  dismissal_rate <- pnorm(prob_cutoff)
  
  # Benefit from dismissing poor performers
  prob_benefit <- base_utility * dismissal_rate * 0.4
  
  base_utility + prob_benefit
})

# Adjustment 5: Employee flows
params$utility_flows <- with(params, {
  # Start with probationary result
  base_utility <- utility_probation
  
  # Adjust for turnover effects
  turnover_adjustment <- 1 - (turnover_rate * (1 + abs(perf_turn_corr)) / 2)
  
  base_utility * turnover_adjustment
})

# Combined adjustments
params$utility_econ_mult <- params$utility_multiple  # Economic + Multiple
params$utility_all <- params$utility_flows           # All adjustments

# Calculate percentage changes for usefulness analysis
params$pct_change_economic <- 100 * (params$utility_basic - params$utility_economic) / abs(params$utility_basic)
params$pct_change_multiple <- 100 * (params$utility_economic - params$utility_multiple) / abs(params$utility_economic)
params$pct_change_topdown <- 100 * (params$utility_multiple - params$utility_topdown) / abs(params$utility_multiple)
params$pct_change_probation <- 100 * (params$utility_topdown - params$utility_probation) / abs(params$utility_topdown)
params$pct_change_flows <- 100 * (params$utility_probation - params$utility_flows) / abs(params$utility_probation)

cat("Calculating summary statistics...\n")

# Summary statistics
utility_cols <- c("utility_basic", "utility_economic", "utility_multiple", 
                 "utility_topdown", "utility_probation", "utility_flows",
                 "utility_econ_mult", "utility_all")

summary_stats <- params[utility_cols] %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE),
    Negative = ~sum(. < 0, na.rm = TRUE) / length(.) * 100
  )) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  tidyr::separate(Variable, into = c("Adjustment", "Statistic"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Statistic, values_from = Value)

# Usefulness analysis
pct_cols <- c("pct_change_economic", "pct_change_multiple", "pct_change_topdown", 
             "pct_change_probation", "pct_change_flows")

usefulness_stats <- params[pct_cols] %>%
  summarise_all(list(
    Median = ~median(., na.rm = TRUE),
    Mean = ~mean(., na.rm = TRUE),
    Q25 = ~quantile(., 0.25, na.rm = TRUE),
    Q75 = ~quantile(., 0.75, na.rm = TRUE)
  )) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  tidyr::separate(Variable, into = c("Adjustment", "Statistic"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Statistic, values_from = Value) %>%
  arrange(desc(Median))

cat("Saving datasets...\n")

# Save the complete dataset
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save as CSV
csv_filename <- paste0("sturman_reference_dataset_", timestamp, ".csv")
write.csv(params, csv_filename, row.names = FALSE)

# Save as RDS (more efficient for R)
rds_filename <- paste0("sturman_reference_dataset_", timestamp, ".rds")
saveRDS(params, rds_filename)

# Save summary statistics
summary_filename <- paste0("sturman_summary_stats_", timestamp, ".csv")
write.csv(summary_stats, summary_filename, row.names = FALSE)

usefulness_filename <- paste0("sturman_usefulness_stats_", timestamp, ".csv")
write.csv(usefulness_stats, usefulness_filename, row.names = FALSE)

# Create metadata file
metadata <- list(
  dataset_info = list(
    title = "Sturman (2000) Monte Carlo Reference Dataset",
    description = "10,000 Monte Carlo simulations replicating Sturman's utility analysis adjustments",
    n_simulations = n_sims,
    random_seed = 42,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    r_version = R.version.string
  ),
  
  parameter_ranges = list(
    n = "1 to 1,100 (exponential distribution)",
    t = "1 to 10 years (uniform)",
    sr = "0.05 to 1.0 (uniform)",
    r = "0.10 to 0.70 (uniform)",
    sdy = "$5,000 to $50,000 (uniform)",
    cost = "$10 to $1,000 (exponential distribution)",
    discount = "0.05 to 0.15 (uniform)",
    tax = "0.20 to 0.40 (uniform)",
    vc = "0.10 to 0.30 (uniform)",
    r_old = "0.05 to 0.38 (uniform)"
  ),
  
  key_results = list(
    general_usefulness_median_reduction = round(median(params$pct_change_economic, na.rm = TRUE), 1),
    economic_adjustment_impact = round(median(params$pct_change_economic, na.rm = TRUE), 1),
    negative_cases_percent = round(sum(params$utility_all < 0, na.rm = TRUE) / nrow(params) * 100, 1),
    sturman_target_general = 291.0,
    sturman_target_lw = 96.0,
    replication_gap_general = round(291.0 - median(params$pct_change_economic, na.rm = TRUE), 1)
  ),
  
  files_created = list(
    main_dataset_csv = csv_filename,
    main_dataset_rds = rds_filename,
    summary_stats = summary_filename,
    usefulness_analysis = usefulness_filename
  )
)

# Save metadata as JSON
metadata_filename <- paste0("sturman_dataset_metadata_", timestamp, ".json")
jsonlite::write_json(metadata, metadata_filename, pretty = TRUE)

cat("\nDataset Generation Complete!\n")
cat("============================\n")
cat("Files created:\n")
cat("- Main dataset (CSV):", csv_filename, "\n")
cat("- Main dataset (RDS):", rds_filename, "\n") 
cat("- Summary statistics:", summary_filename, "\n")
cat("- Usefulness analysis:", usefulness_filename, "\n")
cat("- Metadata:", metadata_filename, "\n")

cat("\nKey Results:\n")
cat("- Economic adjustment median reduction:", round(median(params$pct_change_economic, na.rm = TRUE), 1), "%\n")
cat("- Negative utility cases:", round(sum(params$utility_all < 0, na.rm = TRUE) / nrow(params) * 100, 1), "%\n")
cat("- Gap from Sturman's 291% target:", round(291.0 - median(params$pct_change_economic, na.rm = TRUE), 1), "pp\n")

cat("\nDataset ready for analysis and sharing!\n") 