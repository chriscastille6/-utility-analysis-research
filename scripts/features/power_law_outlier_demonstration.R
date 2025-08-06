# Power Law Outlier Demonstration
# This script shows how removing "outliers" from power law data can make it appear normal
# This has critical implications for HR - we might be removing our best performers!

library(ggplot2)
library(gridExtra)
library(moments)

# Set seed for reproducibility
set.seed(42)

# Generate power law sample
n <- 500
u <- runif(n)
alpha <- 2.5
xmin <- 100
power_law_sample <- xmin * (1 - u)^(-1/(alpha - 1))
# Scale to match target mean of 200
power_law_sample <- power_law_sample * (200 / mean(power_law_sample))

# Method 1: Standard statistical outlier removal (IQR method)
Q1 <- quantile(power_law_sample, 0.25)
Q3 <- quantile(power_law_sample, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove "outliers" - but these might be our star performers!
outliers <- power_law_sample < lower_bound | power_law_sample > upper_bound
power_law_no_outliers <- power_law_sample[!outliers]
removed_outliers <- power_law_sample[outliers]

# Method 2: Alternative - remove top 5% (common in HR to remove "extreme" performers)
top_5_percent_threshold <- quantile(power_law_sample, 0.95)
power_law_no_top5 <- power_law_sample[power_law_sample <= top_5_percent_threshold]
removed_top5 <- power_law_sample[power_law_sample > top_5_percent_threshold]

cat("POWER LAW OUTLIER ANALYSIS\n")
cat("==========================\n\n")

cat("Original Sample Size:", length(power_law_sample), "\n")
cat("After IQR Outlier Removal:", length(power_law_no_outliers), "(", round(100*length(power_law_no_outliers)/length(power_law_sample), 1), "%)\n")
cat("After Top 5% Removal:", length(power_law_no_top5), "(", round(100*length(power_law_no_top5)/length(power_law_sample), 1), "%)\n\n")

cat("REMOVED 'OUTLIERS' STATISTICS\n")
cat("=============================\n")
cat("IQR Method - Removed", length(removed_outliers), "observations:\n")
cat("  Range:", round(min(removed_outliers), 1), "to", round(max(removed_outliers), 1), "\n")
cat("  Mean of removed:", round(mean(removed_outliers), 1), "\n")
cat("  These represent", round(100*length(removed_outliers)/length(power_law_sample), 1), "% of the sample\n\n")

cat("Top 5% Method - Removed", length(removed_top5), "observations:\n")
cat("  Range:", round(min(removed_top5), 1), "to", round(max(removed_top5), 1), "\n")
cat("  Mean of removed:", round(mean(removed_top5), 1), "\n")
cat("  These are likely your STAR PERFORMERS!\n\n")

# Statistical tests on different versions
cat("STATISTICAL TESTS COMPARISON\n")
cat("============================\n")

# Test original data
shapiro_original <- shapiro.test(power_law_sample)
# Test after outlier removal
shapiro_no_outliers <- shapiro.test(power_law_no_outliers)
# Test after top 5% removal  
shapiro_no_top5 <- shapiro.test(power_law_no_top5)

test_comparison <- data.frame(
  Dataset = c("Original Power Law", "After IQR Outlier Removal", "After Top 5% Removal"),
  Sample_Size = c(length(power_law_sample), length(power_law_no_outliers), length(power_law_no_top5)),
  Mean = c(round(mean(power_law_sample), 1), round(mean(power_law_no_outliers), 1), round(mean(power_law_no_top5), 1)),
  Median = c(round(median(power_law_sample), 1), round(median(power_law_no_outliers), 1), round(median(power_law_no_top5), 1)),
  SD = c(round(sd(power_law_sample), 1), round(sd(power_law_no_outliers), 1), round(sd(power_law_no_top5), 1)),
  Skewness = c(round(skewness(power_law_sample), 2), round(skewness(power_law_no_outliers), 2), round(skewness(power_law_no_top5), 2)),
  Shapiro_W = c(round(shapiro_original$statistic, 4), round(shapiro_no_outliers$statistic, 4), round(shapiro_no_top5$statistic, 4)),
  Shapiro_p = c(format.pval(shapiro_original$p.value, digits = 4), format.pval(shapiro_no_outliers$p.value, digits = 4), format.pval(shapiro_no_top5$p.value, digits = 4)),
  Normal_Test = c(
    ifelse(shapiro_original$p.value > 0.05, "NORMAL", "NOT NORMAL"),
    ifelse(shapiro_no_outliers$p.value > 0.05, "NORMAL", "NOT NORMAL"),
    ifelse(shapiro_no_top5$p.value > 0.05, "NORMAL", "NOT NORMAL")
  )
)

print(test_comparison, row.names = FALSE)

# Create visualization
cat("\n\nCREATING VISUALIZATION...\n")

# Create plots
p1 <- ggplot(data.frame(x = power_law_sample), aes(x = x)) +
  geom_histogram(bins = 30, fill = "lightcoral", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(x)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Original Power Law Distribution", 
       subtitle = paste("n =", length(power_law_sample), "| Shapiro p =", format.pval(shapiro_original$p.value, digits = 3)),
       x = "Productivity (units)", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(data.frame(x = power_law_no_outliers), aes(x = x)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(x)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "After IQR Outlier Removal", 
       subtitle = paste("n =", length(power_law_no_outliers), "| Shapiro p =", format.pval(shapiro_no_outliers$p.value, digits = 3)),
       x = "Productivity (units)", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p3 <- ggplot(data.frame(x = power_law_no_top5), aes(x = x)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(x)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "After Removing Top 5%", 
       subtitle = paste("n =", length(power_law_no_top5), "| Shapiro p =", format.pval(shapiro_no_top5$p.value, digits = 3)),
       x = "Productivity (units)", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Show what we removed
p4 <- ggplot(data.frame(x = removed_top5), aes(x = x)) +
  geom_histogram(bins = 15, fill = "gold", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "REMOVED Top 5% - Your Star Performers!", 
       subtitle = paste("n =", length(removed_top5), "| Mean =", round(mean(removed_top5), 1)),
       x = "Productivity (units)", 
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "red"))

# Save the combined plot
png("power_law_outlier_comparison.png", width = 12, height = 10, units = "in", res = 300)
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
dev.off()

cat("Plot saved as 'power_law_outlier_comparison.png'\n\n")

cat("CRITICAL HR INSIGHTS\n")
cat("====================\n")
cat("1. ORIGINAL POWER LAW: Clearly not normal (p < 0.05)\n")
cat("   - Shows the true 'superstar' distribution\n")
cat("   - Heavy right tail represents exceptional performers\n\n")

cat("2. AFTER OUTLIER REMOVAL: May appear more normal\n")
cat("   - Traditional statistical 'cleaning' removes valuable signal\n")
cat("   - We lose information about our best performers\n\n")

cat("3. REMOVED DATA: These are your STAR PERFORMERS!\n")
cat("   - Mean productivity of removed top 5%:", round(mean(removed_top5), 1), "units\n")
cat("   - This is", round(mean(removed_top5)/mean(power_law_no_top5), 1), "times higher than the 'cleaned' average\n")
cat("   - These people drive disproportionate organizational value\n\n")

cat("BUSINESS IMPLICATIONS\n")
cat("=====================\n")
cat("• Standard outlier removal in HR analytics can be DANGEROUS\n")
cat("• 'Outliers' in performance data are often your most valuable employees\n")
cat("• Removing them creates a false sense of normal distribution\n")
cat("• This leads to:\n")
cat("  - Underestimating the value of top performers\n")
cat("  - Missing retention risks for stars\n")
cat("  - Inappropriate performance management strategies\n")
cat("  - Suboptimal compensation decisions\n\n")

cat("RECOMMENDATION\n")
cat("==============\n")
cat("• Always examine your 'outliers' before removing them\n")
cat("• In performance data, high outliers are often your most important data points\n")
cat("• Use power law-aware analytical approaches\n")
cat("• Focus on understanding and retaining extreme high performers\n")
cat("• Don't let statistical 'normality' blind you to business reality\n") 