# Ock & Oswald (2018) Reproducibility Analysis
# "Comparing Compensatory and Multiple Hurdle Selection Models"
#
# This script replicates the comparison of compensatory and multiple hurdle
# selection models as described in Ock & Oswald (2018).

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
# STUDY PARAMETERS (Based on Ock & Oswald, 2018)
# =============================================================================

# Define study parameters
study_params <- list(
  # Sample sizes
  n_applicants = 1000,
  n_selected = 100,  # 10% selection ratio
  
  # Validity coefficients for different predictors
  validities = c(0.30, 0.40, 0.50),  # Range of validities
  
  # Intercorrelations between predictors
  predictor_correlations = c(0.20, 0.40, 0.60),  # Range of correlations
  
  # Selection ratios
  selection_ratios = c(0.05, 0.10, 0.20),  # 5%, 10%, 20%
  
  # Number of predictors
  n_predictors = c(2, 3, 4),  # Different numbers of predictors
  
  # Monte Carlo iterations
  n_iterations = 1000
)

# =============================================================================
# UTILITY ANALYSIS FUNCTIONS
# =============================================================================

# Traditional utility analysis function (Brogden-Cronbach-Gleser)
calculate_utility <- function(validity, selection_ratio, n_selected, mean_salary, sdy_ratio = 0.40) {
  sdy <- sdy_ratio * mean_salary
  z_score <- qnorm(1 - selection_ratio)
  ordinate <- dnorm(z_score)
  
  utility <- n_selected * sdy * validity * (ordinate / selection_ratio)
  return(utility)
}

# Calculate composite validity for multiple predictors
calculate_composite_validity <- function(validities, correlations, weights = NULL) {
  n_pred <- length(validities)
  
  # If weights not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1/n_pred, n_pred)
  }
  
  # Create correlation matrix
  if (length(correlations) == 1) {
    # If single correlation provided, create matrix with that correlation
    cor_matrix <- matrix(correlations, n_pred, n_pred)
    diag(cor_matrix) <- 1
  } else {
    # Use provided correlation matrix
    cor_matrix <- correlations
  }
  
  # Calculate composite validity using formula from Schmidt & Hunter (1977)
  numerator <- sum(weights * validities)
  denominator <- sqrt(sum(weights^2) + 2 * sum(weights[1:(n_pred-1)] * 
                                               weights[2:n_pred] * 
                                               cor_matrix[upper.tri(cor_matrix)]))
  
  composite_validity <- numerator / denominator
  return(composite_validity)
}

# =============================================================================
# SELECTION MODEL FUNCTIONS
# =============================================================================

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
# MONTE CARLO SIMULATION FUNCTIONS
# =============================================================================

# Generate synthetic data
generate_synthetic_data <- function(n_applicants, n_predictors, validities, correlation) {
  # Create correlation matrix
  cor_matrix <- matrix(correlation, n_predictors, n_predictors)
  diag(cor_matrix) <- 1
  
  # Generate predictor scores (multivariate normal)
  predictor_scores <- mvrnorm(n_applicants, 
                             mu = rep(0, n_predictors), 
                             Sigma = cor_matrix)
  
  # Generate criterion scores (job performance)
  # Add some error to make validities realistic
  criterion_scores <- rnorm(n_applicants, 0, 1)
  
  # Create predictor-criterion relationships based on validities
  for (i in 1:n_predictors) {
    criterion_scores <- criterion_scores + validities[i] * predictor_scores[, i]
  }
  
  # Standardize criterion scores
  criterion_scores <- scale(criterion_scores)[, 1]
  
  return(list(
    predictors = predictor_scores,
    criterion = criterion_scores
  ))
}

# Run single simulation
run_single_simulation <- function(params) {
  # Generate data
  data <- generate_synthetic_data(
    n_applicants = params$n_applicants,
    n_predictors = params$n_predictors,
    validities = params$validities,
    correlation = params$correlation
  )
  
  # Apply compensatory selection
  comp_result <- compensatory_selection(data$predictors, params$selection_ratio)
  
  # Apply multiple hurdle selection
  hurdle_result <- multiple_hurdle_selection(data$predictors, params$selection_ratio)
  
  # Calculate performance of selected candidates
  comp_performance <- mean(data$criterion[comp_result$selected])
  hurdle_performance <- mean(data$criterion[hurdle_result$selected])
  
  # Calculate utility (assuming mean salary of $50,000)
  mean_salary <- 50000
  
  # For compensatory model
  comp_utility <- calculate_utility(
    validity = cor(data$criterion, comp_result$composite_scores),
    selection_ratio = params$selection_ratio,
    n_selected = sum(comp_result$selected),
    mean_salary = mean_salary
  )
  
  # For hurdle model - calculate composite score for selected candidates
  hurdle_composite <- rowMeans(data$predictors[hurdle_result$selected, , drop = FALSE])
  hurdle_utility <- calculate_utility(
    validity = cor(data$criterion[hurdle_result$selected], hurdle_composite),
    selection_ratio = params$selection_ratio,
    n_selected = sum(hurdle_result$selected),
    mean_salary = mean_salary
  )
  
  return(list(
    compensatory_performance = comp_performance,
    hurdle_performance = hurdle_performance,
    compensatory_utility = comp_utility,
    hurdle_utility = hurdle_utility,
    compensatory_selected = sum(comp_result$selected),
    hurdle_selected = sum(hurdle_result$selected)
  ))
}

# =============================================================================
# MAIN ANALYSIS
# =============================================================================

cat("=== OCK & OSWALD (2018) REPRODUCTION ANALYSIS ===\n\n")

# Initialize results storage
results <- list()

# Run simulations across different parameter combinations
cat("Running Monte Carlo simulations...\n")

for (n_pred in study_params$n_predictors) {
  for (validity in study_params$validities) {
    for (correlation in study_params$predictor_correlations) {
      for (selection_ratio in study_params$selection_ratios) {
        
        cat(sprintf("Simulating: %d predictors, validity=%.2f, correlation=%.2f, selection_ratio=%.2f\n",
                   n_pred, validity, correlation, selection_ratio))
        
        # Set parameters for this simulation
        sim_params <- list(
          n_applicants = study_params$n_applicants,
          n_predictors = n_pred,
          validities = rep(validity, n_pred),
          correlation = correlation,
          selection_ratio = selection_ratio
        )
        
        # Run multiple iterations
        sim_results <- replicate(study_params$n_iterations, 
                               run_single_simulation(sim_params), 
                               simplify = FALSE)
        
        # Aggregate results
        comp_perf <- sapply(sim_results, function(x) x$compensatory_performance)
        hurdle_perf <- sapply(sim_results, function(x) x$hurdle_performance)
        comp_util <- sapply(sim_results, function(x) x$compensatory_utility)
        hurdle_util <- sapply(sim_results, function(x) x$hurdle_utility)
        
        # Store results
        result_key <- sprintf("n%d_v%.2f_c%.2f_s%.2f", n_pred, validity, correlation, selection_ratio)
        results[[result_key]] <- list(
          parameters = sim_params,
          compensatory_performance = comp_perf,
          hurdle_performance = hurdle_perf,
          compensatory_utility = comp_util,
          hurdle_utility = hurdle_util,
          performance_difference = comp_perf - hurdle_perf,
          utility_difference = comp_util - hurdle_util
        )
      }
    }
  }
}

# =============================================================================
# RESULTS ANALYSIS
# =============================================================================

cat("\nAnalyzing results...\n")

# Create summary statistics
summary_stats <- data.frame()

for (result_name in names(results)) {
  result <- results[[result_name]]
  
  # Extract parameters from result name
  params <- strsplit(result_name, "_")[[1]]
  n_pred <- as.numeric(substr(params[1], 2, 2))
  validity <- as.numeric(substr(params[2], 2, 5))
  correlation <- as.numeric(substr(params[3], 2, 5))
  selection_ratio <- as.numeric(substr(params[4], 2, 5))
  
  # Calculate summary statistics
  summary_stats <- rbind(summary_stats, data.frame(
    n_predictors = n_pred,
    validity = validity,
    correlation = correlation,
    selection_ratio = selection_ratio,
    comp_perf_mean = mean(result$compensatory_performance),
    comp_perf_sd = sd(result$compensatory_performance),
    hurdle_perf_mean = mean(result$hurdle_performance),
    hurdle_perf_sd = sd(result$hurdle_performance),
    comp_util_mean = mean(result$compensatory_utility),
    comp_util_sd = sd(result$compensatory_utility),
    hurdle_util_mean = mean(result$hurdle_utility),
    hurdle_util_sd = sd(result$hurdle_utility),
    perf_diff_mean = mean(result$performance_difference),
    perf_diff_sd = sd(result$performance_difference),
    util_diff_mean = mean(result$utility_difference),
    util_diff_sd = sd(result$utility_difference)
  ))
}

# =============================================================================
# VISUALIZATION
# =============================================================================

cat("Creating visualizations...\n")

# Performance comparison plot
p1 <- ggplot(summary_stats, aes(x = validity, y = perf_diff_mean, 
                               color = factor(correlation), 
                               shape = factor(n_predictors))) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(correlation, n_predictors))) +
  facet_wrap(~selection_ratio, labeller = label_both) +
  labs(title = "Performance Difference: Compensatory vs. Multiple Hurdle",
       x = "Validity Coefficient",
       y = "Performance Difference (Compensatory - Hurdle)",
       color = "Predictor Correlation",
       shape = "Number of Predictors") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(NA, NA))  # Remove automatic limits

ggsave("figures/performance_comparison.png", p1, width = 10, height = 8)

# Utility comparison plot
p2 <- ggplot(summary_stats, aes(x = validity, y = util_diff_mean, 
                               color = factor(correlation), 
                               shape = factor(n_predictors))) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(correlation, n_predictors))) +
  facet_wrap(~selection_ratio, labeller = label_both) +
  labs(title = "Utility Difference: Compensatory vs. Multiple Hurdle",
       x = "Validity Coefficient",
       y = "Utility Difference ($)",
       color = "Predictor Correlation",
       shape = "Number of Predictors") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_y_continuous(limits = c(NA, NA))  # Remove automatic limits

ggsave("figures/utility_comparison.png", p2, width = 10, height = 8)

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("Saving results...\n")

# Save all results
save(results, summary_stats, study_params, 
     file = "ock_oswald_2018_results.RData")

# Create summary table
summary_table <- summary_stats %>%
  select(n_predictors, validity, correlation, selection_ratio,
         comp_perf_mean, hurdle_perf_mean, perf_diff_mean,
         comp_util_mean, hurdle_util_mean, util_diff_mean) %>%
  mutate(
    comp_perf_mean = round(comp_perf_mean, 3),
    hurdle_perf_mean = round(hurdle_perf_mean, 3),
    perf_diff_mean = round(perf_diff_mean, 3),
    comp_util_mean = round(comp_util_mean, 0),
    hurdle_util_mean = round(hurdle_util_mean, 0),
    util_diff_mean = round(util_diff_mean, 0)
  )

# Display summary
cat("\n=== SUMMARY RESULTS ===\n")
print(summary_table)

cat("\nAnalysis complete! Results saved to ock_oswald_2018_results.RData\n")
cat("Figures saved to figures/ directory\n") 