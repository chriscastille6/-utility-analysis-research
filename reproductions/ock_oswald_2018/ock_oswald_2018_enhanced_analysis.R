# Enhanced Ock & Oswald (2018) Analysis
# Incorporating Berry et al. (2024) Correlation Matrix and Sturman Economic Adjustments
#
# This script tests the hypothesis that multiple hurdle selection models become
# more useful when using realistic correlation matrices and economic adjustments.

# =============================================================================
# SETUP AND LIBRARIES
# =============================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(psych)
library(MASS)
library(knitr)
library(kableExtra)
library(gridExtra)
library(corrplot)

# Explicitly use dplyr select to avoid conflict with MASS
select <- dplyr::select

# Set random seed for reproducibility
set.seed(12345)

# Create figures directory if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# =============================================================================
# BERRY ET AL. (2024) CORRELATION MATRIX
# =============================================================================

# Berry et al. (2024) correlation matrix (6 predictors)
berry_cor_matrix <- matrix(c(
  1.00, 0.13, 0.54, 0.21, 0.25, 0.42,
  0.13, 1.00, 0.03, 0.18, 0.01, 0.29,
  0.54, 0.03, 1.00, 0.08, 0.28, 0.23,
  0.21, 0.18, 0.08, 1.00, -0.02, 0.45,
  0.25, 0.01, 0.28, -0.02, 1.00, 0.16,
  0.42, 0.29, 0.23, 0.45, 0.16, 1.00
), nrow=6, byrow=TRUE)

# Variable names
var_names <- c("Biodata", "GMA", "Conscientiousness", "Structured_Interview", "Integrity", "SJT")
colnames(berry_cor_matrix) <- rownames(berry_cor_matrix) <- var_names

# Berry et al. (2024) validities
berry_validities <- c(0.38, 0.31, 0.19, 0.42, 0.31, 0.26)
names(berry_validities) <- var_names

# Berry et al. (2024) Black-White d-values
berry_d_values <- c(0.32, 0.79, -0.07, 0.24, 0.10, 0.37)
names(berry_d_values) <- var_names

# =============================================================================
# STURMAN ECONOMIC ADJUSTMENT PARAMETERS
# =============================================================================

# Economic adjustment parameters (based on Sturman 2000)
sturman_params <- list(
  discount_rate = 0.10,    # 10% discount rate
  tax_rate = 0.35,         # 35% tax rate
  variable_cost_ratio = 0.20,  # 20% variable costs
  time_horizon = 5         # 5-year time horizon
)

# =============================================================================
# ENHANCED UTILITY ANALYSIS FUNCTIONS
# =============================================================================

# Standard normal ordinate function (from Sturman)
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Traditional utility analysis (Brogden-Cronbach-Gleser)
calculate_traditional_utility <- function(validity, selection_ratio, n_selected, mean_salary, sdy_ratio = 0.40) {
  sdy <- sdy_ratio * mean_salary
  utility <- n_selected * sdy * validity * ux(selection_ratio) * sturman_params$time_horizon
  return(utility)
}

# Sturman economic adjustment utility analysis
calculate_sturman_utility <- function(validity, selection_ratio, n_selected, mean_salary, sdy_ratio = 0.40) {
  sdy <- sdy_ratio * mean_salary
  
  # Annual benefit
  annual_benefit <- n_selected * validity * ux(selection_ratio) * sdy
  
  # Variable costs
  annual_variable_costs <- annual_benefit * sturman_params$variable_cost_ratio
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  # Present value calculation
  pv_factor <- (1 - (1 + sturman_params$discount_rate)^(-sturman_params$time_horizon)) / sturman_params$discount_rate
  pv_benefits <- annual_net_benefit * pv_factor
  
  # After-tax present value
  after_tax_pv <- pv_benefits * (1 - sturman_params$tax_rate)
  
  return(after_tax_pv)
}

# Calculate composite validity for multiple predictors using Berry matrix
calculate_composite_validity_berry <- function(predictors, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1/length(predictors), length(predictors))
  }
  
  # Get indices of selected predictors
  pred_indices <- which(var_names %in% predictors)
  
  # Extract submatrix and validities
  cor_matrix <- berry_cor_matrix[pred_indices, pred_indices]
  validities <- berry_validities[pred_indices]
  
  # Calculate composite validity using Schmidt & Hunter (1977) formula
  numerator <- sum(weights * validities)
  denominator <- sqrt(sum(weights^2) + 2 * sum(weights[1:(length(weights)-1)] * 
                                               weights[2:length(weights)] * 
                                               cor_matrix[upper.tri(cor_matrix)]))
  
  composite_validity <- numerator / denominator
  return(composite_validity)
}

# =============================================================================
# ENHANCED SELECTION MODEL FUNCTIONS
# =============================================================================

# Generate synthetic data using Berry correlation matrix
generate_berry_data <- function(n_applicants, predictors) {
  # Get indices of selected predictors
  pred_indices <- which(var_names %in% predictors)
  
  # Extract submatrix for selected predictors
  cor_matrix <- berry_cor_matrix[pred_indices, pred_indices]
  
  # Generate predictor scores (multivariate normal)
  predictor_scores <- mvrnorm(n_applicants, 
                             mu = rep(0, length(predictors)), 
                             Sigma = cor_matrix)
  
  # Generate criterion scores (job performance)
  criterion_scores <- rnorm(n_applicants, 0, 1)
  
  # Create predictor-criterion relationships based on validities
  validities <- berry_validities[pred_indices]
  for (i in 1:length(predictors)) {
    criterion_scores <- criterion_scores + validities[i] * predictor_scores[, i]
  }
  
  # Standardize criterion scores
  criterion_scores <- scale(criterion_scores)[, 1]
  
  return(list(
    predictors = predictor_scores,
    criterion = criterion_scores,
    predictor_names = predictors
  ))
}

# Compensatory selection model
compensatory_selection <- function(scores, selection_ratio) {
  # Calculate composite score (simple average)
  composite_score <- rowMeans(scores)
  
  # Determine cutoff score
  cutoff <- quantile(composite_score, 1 - selection_ratio)
  
  # Select candidates above cutoff
  selected <- composite_score >= cutoff
  
  return(list(
    selected = selected,
    composite_scores = composite_score,
    cutoff = cutoff
  ))
}

# Multiple hurdle selection model
multiple_hurdle_selection <- function(scores, selection_ratio) {
  n_candidates <- nrow(scores)
  n_predictors <- ncol(scores)
  
  # Calculate individual cutoffs for each predictor
  # Use equal selection ratios for each hurdle
  hurdle_ratio <- 1 - (1 - selection_ratio)^(1/n_predictors)
  
  cutoffs <- apply(scores, 2, function(x) quantile(x, 1 - hurdle_ratio))
  
  # Apply hurdles sequentially
  selected <- rep(TRUE, n_candidates)
  
  for (i in 1:n_predictors) {
    selected <- selected & (scores[, i] >= cutoffs[i])
  }
  
  return(list(
    selected = selected,
    cutoffs = cutoffs,
    hurdle_ratio = hurdle_ratio
  ))
}

# =============================================================================
# ENHANCED SIMULATION FUNCTION
# =============================================================================

# Run enhanced simulation
run_enhanced_simulation <- function(predictors, selection_ratio, n_applicants = 1000) {
  # Generate data using Berry matrix
  data <- generate_berry_data(n_applicants, predictors)
  
  # Apply compensatory selection
  comp_result <- compensatory_selection(data$predictors, selection_ratio)
  
  # Apply multiple hurdle selection
  hurdle_result <- multiple_hurdle_selection(data$predictors, selection_ratio)
  
  # Calculate performance of selected candidates
  comp_performance <- mean(data$criterion[comp_result$selected])
  hurdle_performance <- mean(data$criterion[hurdle_result$selected])
  
  # Calculate validities
  comp_validity <- cor(data$criterion, comp_result$composite_scores)
  
  hurdle_composite <- rowMeans(data$predictors[hurdle_result$selected, , drop = FALSE])
  hurdle_validity <- cor(data$criterion[hurdle_result$selected], hurdle_composite)
  
  # Calculate utilities (assuming mean salary of $50,000)
  mean_salary <- 50000
  
  # Traditional utility
  comp_traditional <- calculate_traditional_utility(
    validity = comp_validity,
    selection_ratio = selection_ratio,
    n_selected = sum(comp_result$selected),
    mean_salary = mean_salary
  )
  
  hurdle_traditional <- calculate_traditional_utility(
    validity = hurdle_validity,
    selection_ratio = selection_ratio,
    n_selected = sum(hurdle_result$selected),
    mean_salary = mean_salary
  )
  
  # Sturman economic utility
  comp_sturman <- calculate_sturman_utility(
    validity = comp_validity,
    selection_ratio = selection_ratio,
    n_selected = sum(comp_result$selected),
    mean_salary = mean_salary
  )
  
  hurdle_sturman <- calculate_sturman_utility(
    validity = hurdle_validity,
    selection_ratio = selection_ratio,
    n_selected = sum(hurdle_result$selected),
    mean_salary = mean_salary
  )
  
  return(list(
    compensatory_performance = comp_performance,
    hurdle_performance = hurdle_performance,
    compensatory_traditional_utility = comp_traditional,
    hurdle_traditional_utility = hurdle_traditional,
    compensatory_sturman_utility = comp_sturman,
    hurdle_sturman_utility = hurdle_sturman,
    compensatory_selected = sum(comp_result$selected),
    hurdle_selected = sum(hurdle_result$selected),
    compensatory_validity = comp_validity,
    hurdle_validity = hurdle_validity
  ))
}

# =============================================================================
# MAIN ENHANCED ANALYSIS
# =============================================================================

cat("=== ENHANCED OCK & OSWALD (2018) ANALYSIS ===\n")
cat("Incorporating Berry et al. (2024) Matrix and Sturman Economic Adjustments\n\n")

# Define analysis parameters
analysis_params <- list(
  n_applicants = 1000,
  selection_ratios = c(0.05, 0.10, 0.20),
  predictor_combinations = list(
    c("GMA", "Structured_Interview"),
    c("GMA", "Biodata", "Structured_Interview"),
    c("Biodata", "Structured_Interview", "Integrity", "SJT"),
    c("Biodata", "Conscientiousness", "Structured_Interview", "Integrity", "SJT")
  ),
  n_iterations = 500  # Reduced for faster execution
)

# Initialize results storage
enhanced_results <- list()

# Run simulations
cat("Running enhanced simulations...\n")

for (i in seq_along(analysis_params$predictor_combinations)) {
  predictors <- analysis_params$predictor_combinations[[i]]
  
  for (selection_ratio in analysis_params$selection_ratios) {
    
    cat(sprintf("Simulating: %s predictors, selection_ratio=%.2f\n",
               paste(predictors, collapse = "+"), selection_ratio))
    
    # Run multiple iterations
    sim_results <- replicate(analysis_params$n_iterations, 
                           run_enhanced_simulation(predictors, selection_ratio, analysis_params$n_applicants), 
                           simplify = FALSE)
    
    # Aggregate results
    comp_perf <- sapply(sim_results, function(x) x$compensatory_performance)
    hurdle_perf <- sapply(sim_results, function(x) x$hurdle_performance)
    comp_trad <- sapply(sim_results, function(x) x$compensatory_traditional_utility)
    hurdle_trad <- sapply(sim_results, function(x) x$hurdle_traditional_utility)
    comp_stur <- sapply(sim_results, function(x) x$compensatory_sturman_utility)
    hurdle_stur <- sapply(sim_results, function(x) x$hurdle_sturman_utility)
    
    # Store results
    result_key <- sprintf("combo%d_s%.2f", i, selection_ratio)
    enhanced_results[[result_key]] <- list(
      predictors = predictors,
      selection_ratio = selection_ratio,
      compensatory_performance = comp_perf,
      hurdle_performance = hurdle_perf,
      compensatory_traditional_utility = comp_trad,
      hurdle_traditional_utility = hurdle_trad,
      compensatory_sturman_utility = comp_stur,
      hurdle_sturman_utility = hurdle_stur,
      performance_difference = comp_perf - hurdle_perf,
      traditional_utility_difference = comp_trad - hurdle_trad,
      sturman_utility_difference = comp_stur - hurdle_stur
    )
  }
}

# =============================================================================
# RESULTS ANALYSIS
# =============================================================================

cat("\nAnalyzing enhanced results...\n")

# Create summary statistics
enhanced_summary <- data.frame()

for (result_name in names(enhanced_results)) {
  result <- enhanced_results[[result_name]]
  
  # Extract parameters from result name
  params <- strsplit(result_name, "_")[[1]]
  combo_num <- as.numeric(substr(params[1], 5, 5))
  selection_ratio <- as.numeric(substr(params[2], 2, 5))
  
  # Calculate summary statistics
  enhanced_summary <- rbind(enhanced_summary, data.frame(
    predictor_combination = paste(result$predictors, collapse = "+"),
    n_predictors = length(result$predictors),
    selection_ratio = selection_ratio,
    comp_perf_mean = mean(result$compensatory_performance),
    comp_perf_sd = sd(result$compensatory_performance),
    hurdle_perf_mean = mean(result$hurdle_performance),
    hurdle_perf_sd = sd(result$hurdle_performance),
    comp_trad_mean = mean(result$compensatory_traditional_utility),
    comp_trad_sd = sd(result$compensatory_traditional_utility),
    hurdle_trad_mean = mean(result$hurdle_traditional_utility),
    hurdle_trad_sd = sd(result$hurdle_traditional_utility),
    comp_stur_mean = mean(result$compensatory_sturman_utility),
    comp_stur_sd = sd(result$compensatory_sturman_utility),
    hurdle_stur_mean = mean(result$hurdle_sturman_utility),
    hurdle_stur_sd = sd(result$hurdle_sturman_utility),
    perf_diff_mean = mean(result$performance_difference),
    perf_diff_sd = sd(result$performance_difference),
    trad_util_diff_mean = mean(result$traditional_utility_difference),
    trad_util_diff_sd = sd(result$traditional_utility_difference),
    stur_util_diff_mean = mean(result$sturman_utility_difference),
    stur_util_diff_sd = sd(result$sturman_utility_difference)
  ))
}

# =============================================================================
# HYPOTHESIS TESTING
# =============================================================================

cat("\n=== HYPOTHESIS TESTING ===\n")

# Test 1: Does Sturman adjustment reduce the utility gap?
cat("Test 1: Utility Gap Reduction with Sturman Adjustments\n")
cat("======================================================\n")

gap_reduction <- enhanced_summary %>%
  mutate(
    traditional_gap = abs(trad_util_diff_mean),
    sturman_gap = abs(stur_util_diff_mean),
    gap_reduction = (traditional_gap - sturman_gap) / traditional_gap * 100
  )

print(gap_reduction[, c("predictor_combination", "selection_ratio", 
                       "traditional_gap", "sturman_gap", "gap_reduction")])

# Test 2: Does multiple hurdle become more favorable with Sturman adjustments?
cat("\nTest 2: Multiple Hurdle Favorability with Sturman Adjustments\n")
cat("==============================================================\n")

favorability_analysis <- enhanced_summary %>%
  mutate(
    traditional_favorable = trad_util_diff_mean < 0,  # Negative means hurdle better
    sturman_favorable = stur_util_diff_mean < 0,
    became_more_favorable = !traditional_favorable & sturman_favorable
  )

print(favorability_analysis[, c("predictor_combination", "selection_ratio", 
                               "traditional_favorable", "sturman_favorable", 
                               "became_more_favorable")])

# Test 3: Effect of selection ratio on utility gap
cat("\nTest 3: Selection Ratio Effects\n")
cat("================================\n")

selection_effects <- enhanced_summary %>%
  group_by(selection_ratio) %>%
  summarise(
    mean_traditional_gap = mean(abs(trad_util_diff_mean)),
    mean_sturman_gap = mean(abs(stur_util_diff_mean)),
    mean_gap_reduction = mean((abs(trad_util_diff_mean) - abs(stur_util_diff_mean)) / abs(trad_util_diff_mean) * 100)
  )

print(selection_effects)

# =============================================================================
# VISUALIZATION
# =============================================================================

cat("\nCreating enhanced visualizations...\n")

# Plot 1: Utility gap comparison
p1 <- ggplot(enhanced_summary, aes(x = selection_ratio, y = abs(trad_util_diff_mean), 
                                  color = "Traditional", shape = factor(n_predictors))) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(predictor_combination, "Traditional"))) +
  geom_point(aes(y = abs(stur_util_diff_mean), color = "Sturman"), size = 3) +
  geom_line(aes(y = abs(stur_util_diff_mean), group = interaction(predictor_combination, "Sturman"), color = "Sturman")) +
  facet_wrap(~predictor_combination, labeller = label_wrap_gen(width = 20)) +
  labs(title = "Utility Gap: Traditional vs. Sturman Economic Adjustments",
       x = "Selection Ratio",
       y = "Absolute Utility Difference ($)",
       color = "Utility Method",
       shape = "Number of Predictors") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("figures/enhanced_utility_gap_comparison.png", p1, width = 12, height = 8)

# Plot 2: Performance difference by selection ratio
p2 <- ggplot(enhanced_summary, aes(x = selection_ratio, y = perf_diff_mean, 
                                  color = predictor_combination, 
                                  shape = factor(n_predictors))) +
  geom_point(size = 3) +
  geom_line(aes(group = predictor_combination)) +
  labs(title = "Performance Difference: Compensatory vs. Multiple Hurdle",
       subtitle = "Using Berry et al. (2024) Correlation Matrix",
       x = "Selection Ratio",
       y = "Performance Difference (Compensatory - Hurdle)",
       color = "Predictor Combination",
       shape = "Number of Predictors") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("figures/enhanced_performance_comparison.png", p2, width = 10, height = 8)

# =============================================================================
# SAVE ENHANCED RESULTS
# =============================================================================

cat("Saving enhanced results...\n")

# Save all results
save(enhanced_results, enhanced_summary, berry_cor_matrix, berry_validities, 
     berry_d_values, sturman_params, analysis_params,
     file = "ock_oswald_2018_enhanced_results.RData")

# Create summary table
enhanced_summary_table <- enhanced_summary %>%
  select(predictor_combination, n_predictors, selection_ratio,
         comp_perf_mean, hurdle_perf_mean, perf_diff_mean,
         comp_trad_mean, hurdle_trad_mean, trad_util_diff_mean,
         comp_stur_mean, hurdle_stur_mean, stur_util_diff_mean) %>%
  mutate(
    comp_perf_mean = round(comp_perf_mean, 3),
    hurdle_perf_mean = round(hurdle_perf_mean, 3),
    perf_diff_mean = round(perf_diff_mean, 3),
    comp_trad_mean = round(comp_trad_mean, 0),
    hurdle_trad_mean = round(hurdle_trad_mean, 0),
    trad_util_diff_mean = round(trad_util_diff_mean, 0),
    comp_stur_mean = round(comp_stur_mean, 0),
    hurdle_stur_mean = round(hurdle_stur_mean, 0),
    stur_util_diff_mean = round(stur_util_diff_mean, 0)
  )

# Display summary
cat("\n=== ENHANCED SUMMARY RESULTS ===\n")
print(enhanced_summary_table)

# Key findings
cat("\n=== KEY FINDINGS ===\n")
cat("1. Average utility gap reduction with Sturman adjustments:", 
    round(mean(gap_reduction$gap_reduction, na.rm = TRUE), 1), "%\n")
cat("2. Cases where multiple hurdle became more favorable:", 
    sum(favorability_analysis$became_more_favorable), "out of", nrow(favorability_analysis), "\n")
cat("3. Average performance difference:", 
    round(mean(enhanced_summary$perf_diff_mean, na.rm = TRUE), 3), "\n")

cat("\nEnhanced analysis complete! Results saved to ock_oswald_2018_enhanced_results.RData\n")
cat("Figures saved to figures/ directory\n") 