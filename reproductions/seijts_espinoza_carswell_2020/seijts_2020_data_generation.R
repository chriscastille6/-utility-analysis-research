# SEIJTS, ESPINOZA, & CARSWELL (2020) REPRODUCTION
# Synthetic Data Generation Script
# Creates dataset matching reported correlation matrix and descriptive statistics

# Load required libraries
library(MASS)
library(psych)
library(tidyverse)

# Set seed for reproducibility
set.seed(12345)

# =============================================================================
# REPORTED STATISTICS (from Seijts et al., 2020, Table 1)
# =============================================================================

# Sample size
N <- 111

# Character dimensions
character_dimensions <- c(
  "Accountability", "Collaboration", "Courage", "Drive", "Humanity",
  "Humility", "Integrity", "Judgment", "Justice", "Temperance", "Transcendence"
)

# Reported means and standard deviations
reported_means <- c(
  4.56, 4.32, 4.57, 4.38, 4.20, 4.29, 4.56, 4.42, 4.31, 4.29, 4.28, 5.40
)
names(reported_means) <- c(character_dimensions, "Performance")

reported_sds <- c(
  0.35, 0.38, 0.28, 0.32, 0.43, 0.37, 0.34, 0.32, 0.36, 0.43, 0.30, 0.83
)
names(reported_sds) <- c(character_dimensions, "Performance")

# Reported correlation matrix (from Table 1)
# Note: This is the correlation matrix as reported in the paper
correlation_matrix <- matrix(
  c(
    # Accountability
    1.00, 0.58, 0.79, 0.76, 0.48, 0.68, 0.75, 0.79, 0.65, 0.36, 0.65, 0.28,
    # Collaboration  
    0.58, 1.00, 0.42, 0.53, 0.87, 0.86, 0.71, 0.50, 0.81, 0.64, 0.55, 0.23,
    # Courage
    0.79, 0.42, 1.00, 0.77, 0.32, 0.51, 0.62, 0.75, 0.50, 0.24, 0.65, 0.17,
    # Drive
    0.76, 0.53, 0.77, 1.00, 0.40, 0.59, 0.64, 0.68, 0.59, 0.40, 0.66, 0.35,
    # Humanity
    0.48, 0.87, 0.32, 0.40, 1.00, 0.89, 0.69, 0.45, 0.82, 0.67, 0.49, 0.23,
    # Humility
    0.68, 0.86, 0.51, 0.59, 0.89, 1.00, 0.80, 0.62, 0.86, 0.69, 0.66, 0.31,
    # Integrity
    0.75, 0.71, 0.62, 0.64, 0.69, 0.80, 1.00, 0.66, 0.83, 0.54, 0.67, 0.24,
    # Judgment
    0.79, 0.50, 0.75, 0.68, 0.45, 0.62, 0.66, 1.00, 0.63, 0.44, 0.71, 0.28,
    # Justice
    0.65, 0.81, 0.50, 0.59, 0.82, 0.86, 0.83, 0.63, 1.00, 0.65, 0.64, 0.23,
    # Temperance
    0.36, 0.64, 0.24, 0.40, 0.67, 0.69, 0.54, 0.44, 0.65, 1.00, 0.46, 0.21,
    # Transcendence
    0.65, 0.55, 0.65, 0.66, 0.49, 0.66, 0.67, 0.71, 0.64, 0.46, 1.00, 0.18,
    # Performance
    0.28, 0.23, 0.17, 0.35, 0.23, 0.31, 0.24, 0.28, 0.23, 0.21, 0.18, 1.00
  ),
  nrow = 12, ncol = 12,
  byrow = TRUE
)

# Set row and column names
rownames(correlation_matrix) <- c(character_dimensions, "Performance")
colnames(correlation_matrix) <- c(character_dimensions, "Performance")

# Ensure the matrix is symmetric and has 1s on diagonal
correlation_matrix[lower.tri(correlation_matrix)] <- t(correlation_matrix)[lower.tri(correlation_matrix)]
diag(correlation_matrix) <- 1

# =============================================================================
# DATA GENERATION FUNCTION
# =============================================================================

generate_synthetic_data <- function(N, means, sds, cor_matrix, seed = 12345) {
  set.seed(seed)
  
  # Generate multivariate normal data
  # Note: We need to ensure the correlation matrix is positive definite
  eigen_vals <- eigen(cor_matrix)$values
  if (any(eigen_vals < 0)) {
    cat("Warning: Correlation matrix has negative eigenvalues. Adjusting...\n")
    # Add small constant to diagonal to make it positive definite
    cor_matrix <- cor_matrix + diag(0.01, nrow(cor_matrix))
  }
  
  # Generate data using MASS::mvrnorm
  synthetic_data <- MASS::mvrnorm(
    n = N,
    mu = means,
    Sigma = cor_matrix,
    empirical = TRUE
  )
  
  # Convert to data frame
  synthetic_df <- as.data.frame(synthetic_data)
  
  # Scale to match reported means and standard deviations
  for (i in 1:ncol(synthetic_df)) {
    var_name <- colnames(synthetic_df)[i]
    synthetic_df[[var_name]] <- (synthetic_df[[var_name]] - mean(synthetic_df[[var_name]])) / 
                                sd(synthetic_df[[var_name]]) * sds[var_name] + means[var_name]
  }
  
  return(synthetic_df)
}

# =============================================================================
# GENERATE SYNTHETIC DATA
# =============================================================================

cat("Generating synthetic data matching Seijts et al. (2020) statistics...\n")

# Generate the synthetic dataset
synthetic_data <- generate_synthetic_data(
  N = N,
  means = reported_means,
  sds = reported_sds,
  cor_matrix = correlation_matrix
)

# Add composite character score (average of all character dimensions)
synthetic_data$Character <- rowMeans(synthetic_data[, character_dimensions])

# =============================================================================
# VALIDATE GENERATED DATA
# =============================================================================

cat("\n=== VALIDATION OF GENERATED DATA ===\n")

# Check means for original variables only
cat("Means comparison:\n")
generated_means <- colMeans(synthetic_data[, names(reported_means)])
for (i in 1:length(reported_means)) {
  var_name <- names(reported_means)[i]
  cat(sprintf("%-12s: Reported = %.2f, Generated = %.2f, Diff = %.3f\n",
              var_name, reported_means[var_name], generated_means[var_name],
              abs(reported_means[var_name] - generated_means[var_name])))
}

# Check standard deviations for original variables only
cat("\nStandard deviations comparison:\n")
generated_sds <- apply(synthetic_data[, names(reported_sds)], 2, sd)
for (i in 1:length(reported_sds)) {
  var_name <- names(reported_sds)[i]
  cat(sprintf("%-12s: Reported = %.2f, Generated = %.2f, Diff = %.3f\n",
              var_name, reported_sds[var_name], generated_sds[var_name],
              abs(reported_sds[var_name] - generated_sds[var_name])))
}

# Check key correlations
cat("\nKey correlations comparison:\n")
generated_cor <- cor(synthetic_data)
key_correlations <- c("Character-Performance", "Accountability-Performance", 
                     "Integrity-Performance", "Judgment-Performance")

for (cor_name in key_correlations) {
  if (cor_name == "Character-Performance") {
    reported_cor <- 0.30  # From the paper
    generated_cor_val <- cor(synthetic_data$Character, synthetic_data$Performance)
  } else {
    vars <- strsplit(cor_name, "-")[[1]]
    reported_cor <- correlation_matrix[vars[1], vars[2]]
    generated_cor_val <- cor(synthetic_data[[vars[1]]], synthetic_data[[vars[2]]])
  }
  cat(sprintf("%-20s: Reported = %.2f, Generated = %.2f, Diff = %.3f\n",
              cor_name, reported_cor, generated_cor_val,
              abs(reported_cor - generated_cor_val)))
}

# Check reliability coefficients
cat("\nReliability coefficients:\n")
# Character composite (all 11 dimensions)
character_alpha <- psych::alpha(synthetic_data[, character_dimensions])$total$raw_alpha
cat(sprintf("Character composite alpha: %.3f (reported: %.2f)\n", character_alpha, 0.95))

# Performance (8 items simulated as 8 columns)
# Since we don't have the actual 8 items, we'll simulate them
performance_items <- matrix(NA, nrow = N, ncol = 8)
for (i in 1:8) {
  # Add some random variation to the performance score
  performance_items[, i] <- synthetic_data$Performance + rnorm(N, 0, 0.2)
}
performance_alpha <- psych::alpha(performance_items)$total$raw_alpha
cat(sprintf("Performance alpha: %.3f (reported: %.2f)\n", performance_alpha, 0.91))

# =============================================================================
# SAVE GENERATED DATA
# =============================================================================

# Save the synthetic dataset
write.csv(synthetic_data, "seijts_2020_synthetic_data.csv", row.names = FALSE)

# Create summary statistics for original variables only
summary_stats <- data.frame(
  Variable = names(reported_means),
  Reported_Mean = reported_means,
  Generated_Mean = generated_means,
  Mean_Diff = abs(reported_means - generated_means),
  Reported_SD = reported_sds,
  Generated_SD = generated_sds,
  SD_Diff = abs(reported_sds - generated_sds)
)

# Save summary statistics
write.csv(summary_stats, "seijts_2020_summary_stats.csv", row.names = FALSE)

# Save correlation matrix
write.csv(correlation_matrix, "seijts_2020_correlation_matrix.csv")

cat("\n=== DATA GENERATION COMPLETE ===\n")
cat("Files saved:\n")
cat("- seijts_2020_synthetic_data.csv\n")
cat("- seijts_2020_summary_stats.csv\n")
cat("- seijts_2020_correlation_matrix.csv\n")

# =============================================================================
# CREATE VALIDATION PLOTS
# =============================================================================

# Create correlation heatmap
png("figures/correlation_matrix.png", width = 800, height = 600)
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         title = "Correlation Matrix - Seijts et al. (2020)")
dev.off()

# Create scatter plot of character vs performance
png("figures/character_performance_scatter.png", width = 600, height = 400)
ggplot(synthetic_data, aes(x = Character, y = Performance)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Character vs Performance Relationship",
       subtitle = paste("r =", round(cor(synthetic_data$Character, synthetic_data$Performance), 3)),
       x = "Character (Composite Score)",
       y = "Performance Rating") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
dev.off()

cat("Validation plots saved to figures/ directory\n") 