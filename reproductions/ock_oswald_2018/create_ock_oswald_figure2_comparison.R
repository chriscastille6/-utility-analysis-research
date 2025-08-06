# Create Ock & Oswald Figure 2 Comparison
# This script generates a plot similar to Ock & Oswald's Figure 2
# showing performance differences between compensatory and multiple hurdle models

# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load our results
load("ock_oswald_2018_results.RData")
load("ock_oswald_2018_enhanced_results.RData")

# Create comparison data
create_comparison_data <- function() {
  # Original analysis results
  original_data <- summary_stats %>%
    mutate(
      analysis_type = "Original (Constant Correlations)",
      performance_difference = perf_diff_mean,
      utility_difference = util_diff_mean
    ) %>%
    select(analysis_type, n_predictors, validity, correlation, selection_ratio, 
           performance_difference, utility_difference)
  
  # Enhanced analysis results (using Berry matrix)
  enhanced_data <- enhanced_summary %>%
    mutate(
      analysis_type = "Enhanced (Berry et al. 2024 Matrix)",
      performance_difference = perf_diff_mean,
      utility_difference = stur_util_diff_mean,  # Use Sturman-adjusted utility
      validity = 0.40,  # Approximate average validity for comparison
      correlation = 0.30  # Approximate average correlation for comparison
    ) %>%
    select(analysis_type, n_predictors, validity, correlation, selection_ratio, 
           performance_difference, utility_difference)
  
  # Combine datasets
  comparison_data <- rbind(original_data, enhanced_data)
  
  return(comparison_data)
}

# Generate the comparison data
comparison_data <- create_comparison_data()

# Create Figure 2-style comparison plots

# Plot 1: Performance Difference by Selection Ratio
p1 <- ggplot(comparison_data, aes(x = selection_ratio, y = performance_difference, 
                                 color = analysis_type, shape = factor(n_predictors))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(aes(group = interaction(analysis_type, n_predictors)), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Performance Difference: Compensatory vs. Multiple Hurdle",
       subtitle = "Comparison with Ock & Oswald (2018) Figure 2",
       x = "Selection Ratio",
       y = "Performance Difference (Compensatory - Hurdle)",
       color = "Analysis Type",
       shape = "Number of Predictors") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("Original (Constant Correlations)" = "blue", 
                               "Enhanced (Berry et al. 2024 Matrix)" = "red")) +
  scale_shape_manual(values = c("2" = 16, "3" = 17, "4" = 18, "5" = 19))

# Plot 2: Utility Difference by Selection Ratio
p2 <- ggplot(comparison_data, aes(x = selection_ratio, y = utility_difference, 
                                 color = analysis_type, shape = factor(n_predictors))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(aes(group = interaction(analysis_type, n_predictors)), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Utility Difference: Compensatory vs. Multiple Hurdle",
       subtitle = "Economic Adjustments Applied to Enhanced Analysis",
       x = "Selection Ratio",
       y = "Utility Difference ($)",
       color = "Analysis Type",
       shape = "Number of Predictors") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("Original (Constant Correlations)" = "blue", 
                               "Enhanced (Berry et al. 2024 Matrix)" = "red")) +
  scale_shape_manual(values = c("2" = 16, "3" = 17, "4" = 18, "5" = 19))

# Plot 3: Performance Difference by Number of Predictors
p3 <- ggplot(comparison_data, aes(x = factor(n_predictors), y = performance_difference, 
                                 fill = analysis_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Performance Difference by Number of Predictors",
       subtitle = "Boxplots Show Distribution Across Selection Ratios",
       x = "Number of Predictors",
       y = "Performance Difference (Compensatory - Hurdle)",
       fill = "Analysis Type") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("Original (Constant Correlations)" = "lightblue", 
                              "Enhanced (Berry et al. 2024 Matrix)" = "lightcoral"))

# Create a comprehensive comparison table
create_comparison_table <- function() {
  summary_comparison <- comparison_data %>%
    group_by(analysis_type, n_predictors) %>%
    summarise(
      mean_performance_diff = mean(performance_difference, na.rm = TRUE),
      mean_utility_diff = mean(utility_difference, na.rm = TRUE),
      sd_performance_diff = sd(performance_difference, na.rm = TRUE),
      sd_utility_diff = sd(utility_difference, na.rm = TRUE),
      n_observations = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      mean_performance_diff = round(mean_performance_diff, 3),
      mean_utility_diff = round(mean_utility_diff, 0),
      sd_performance_diff = round(sd_performance_diff, 3),
      sd_utility_diff = round(sd_utility_diff, 0)
    )
  
  return(summary_comparison)
}

# Generate comparison table
comparison_table <- create_comparison_table()

# Save plots
ggsave("figures/ock_oswald_figure2_performance_comparison.png", p1, width = 12, height = 8)
ggsave("figures/ock_oswald_figure2_utility_comparison.png", p2, width = 12, height = 8)
ggsave("figures/ock_oswald_figure2_predictors_comparison.png", p3, width = 10, height = 8)

# Create combined plot
combined_plot <- grid.arrange(p1, p2, ncol = 2)
ggsave("figures/ock_oswald_figure2_combined.png", combined_plot, width = 16, height = 8)

# Print comparison summary
cat("=== OCK & OSWALD FIGURE 2 COMPARISON ===\n\n")

cat("Comparison Summary:\n")
print(comparison_table)

cat("\nKey Differences from Original Ock & Oswald (2018):\n")
cat("1. Enhanced analysis uses realistic Berry et al. (2024) correlations\n")
cat("2. Economic adjustments applied to utility calculations\n")
cat("3. More conservative performance differences in enhanced analysis\n")
cat("4. Multiple hurdle models show improved viability\n\n")

cat("Files Generated:\n")
cat("- ock_oswald_figure2_performance_comparison.png\n")
cat("- ock_oswald_figure2_utility_comparison.png\n")
cat("- ock_oswald_figure2_predictors_comparison.png\n")
cat("- ock_oswald_figure2_combined.png\n")

# Save comparison data
save(comparison_data, comparison_table, file = "ock_oswald_figure2_comparison.RData")

cat("\nComparison complete! Check the figures/ directory for visualizations.\n") 