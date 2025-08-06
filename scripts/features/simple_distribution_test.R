# Simple Distribution Test - Clear demonstration of Normal vs Power Law
# This script shows the key differences using reliable statistical tests

# Set seed for reproducibility
set.seed(42)

# Generate samples
n <- 500
cat("Generating samples of size", n, "\n")
cat("==============================\n\n")

# Normal sample (like in main app)
normal_sample <- rnorm(n, mean = 200, sd = 40)

# Power law sample - using simple approach
# Generate from uniform and transform to power law
u <- runif(n)
alpha <- 2.5
xmin <- 100
power_law_sample <- xmin * (1 - u)^(-1/(alpha - 1))
# Scale to match target mean of 200
power_law_sample <- power_law_sample * (200 / mean(power_law_sample))

# Basic descriptive statistics
cat("DESCRIPTIVE STATISTICS\n")
cat("======================\n")
results <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum", 
                "90th Percentile", "95th Percentile", "99th Percentile",
                "Skewness", "Kurtosis"),
  Normal_Sample = c(
    round(mean(normal_sample), 1),
    round(median(normal_sample), 1),
    round(sd(normal_sample), 1),
    round(min(normal_sample), 1),
    round(max(normal_sample), 1),
    round(quantile(normal_sample, 0.90), 1),
    round(quantile(normal_sample, 0.95), 1),
    round(quantile(normal_sample, 0.99), 1),
    round(moments::skewness(normal_sample), 2),
    round(moments::kurtosis(normal_sample), 2)
  ),
  Power_Law_Sample = c(
    round(mean(power_law_sample), 1),
    round(median(power_law_sample), 1),
    round(sd(power_law_sample), 1),
    round(min(power_law_sample), 1),
    round(max(power_law_sample), 1),
    round(quantile(power_law_sample, 0.90), 1),
    round(quantile(power_law_sample, 0.95), 1),
    round(quantile(power_law_sample, 0.99), 1),
    round(moments::skewness(power_law_sample), 2),
    round(moments::kurtosis(power_law_sample), 2)
  )
)

print(results, row.names = FALSE)

# Statistical tests
cat("\n\nSTATISTICAL TESTS\n")
cat("=================\n")

# Normality tests
shapiro_normal <- shapiro.test(normal_sample)
shapiro_powerlaw <- shapiro.test(power_law_sample)

ks_normal_vs_normal <- ks.test(normal_sample, "pnorm", mean = mean(normal_sample), sd = sd(normal_sample))
ks_powerlaw_vs_normal <- ks.test(power_law_sample, "pnorm", mean = mean(power_law_sample), sd = sd(power_law_sample))

# Test results table
test_results <- data.frame(
  Test = c(
    "Shapiro-Wilk W statistic",
    "Shapiro-Wilk p-value", 
    "Shapiro-Wilk Result",
    "",
    "Kolmogorov-Smirnov D statistic",
    "Kolmogorov-Smirnov p-value",
    "Kolmogorov-Smirnov Result"
  ),
  Normal_Sample = c(
    round(shapiro_normal$statistic, 4),
    format.pval(shapiro_normal$p.value, digits = 4),
    ifelse(shapiro_normal$p.value > 0.05, "NORMAL", "NOT NORMAL"),
    "",
    round(ks_normal_vs_normal$statistic, 4),
    format.pval(ks_normal_vs_normal$p.value, digits = 4),
    ifelse(ks_normal_vs_normal$p.value > 0.05, "NORMAL", "NOT NORMAL")
  ),
  Power_Law_Sample = c(
    round(shapiro_powerlaw$statistic, 4),
    format.pval(shapiro_powerlaw$p.value, digits = 4),
    ifelse(shapiro_powerlaw$p.value > 0.05, "NORMAL", "NOT NORMAL"),
    "",
    round(ks_powerlaw_vs_normal$statistic, 4),
    format.pval(ks_powerlaw_vs_normal$p.value, digits = 4),
    ifelse(ks_powerlaw_vs_normal$p.value > 0.05, "NORMAL", "NOT NORMAL")
  )
)

print(test_results, row.names = FALSE)

# Visual comparison of distributions
cat("\n\nKEY DIFFERENCES SUMMARY\n")
cat("=======================\n")
cat("Normal Sample:\n")
cat("  • Passes normality tests (p > 0.05)\n")
cat("  • Mean ≈ Median (symmetric distribution)\n")
cat("  • Moderate standard deviation\n")
cat("  • Low skewness and kurtosis\n")
cat("  • 99th percentile not extremely different from mean\n\n")

cat("Power Law Sample:\n")
cat("  • Fails normality tests (p < 0.05)\n")
cat("  • Mean >> Median (right-skewed distribution)\n")
cat("  • Very high standard deviation\n")
cat("  • High positive skewness and kurtosis\n")
cat("  • 99th percentile much higher than mean (heavy tail)\n\n")

# Ratio comparisons
cat("RATIO COMPARISONS\n")
cat("=================\n")
cat("Mean/Median ratio:\n")
cat("  Normal:", round(mean(normal_sample)/median(normal_sample), 2), "\n")
cat("  Power Law:", round(mean(power_law_sample)/median(power_law_sample), 2), "\n\n")

cat("Standard Deviation/Mean ratio:\n")
cat("  Normal:", round(sd(normal_sample)/mean(normal_sample), 2), "\n")
cat("  Power Law:", round(sd(power_law_sample)/mean(power_law_sample), 2), "\n\n")

cat("99th percentile/Median ratio:\n")
cat("  Normal:", round(quantile(normal_sample, 0.99)/median(normal_sample), 2), "\n")
cat("  Power Law:", round(quantile(power_law_sample, 0.99)/median(power_law_sample), 2), "\n\n")

cat("CONCLUSION\n")
cat("==========\n")
cat("The statistical tests clearly distinguish between the two distributions:\n")
cat("• Normal sample: Symmetric, moderate variability, passes normality tests\n")
cat("• Power Law sample: Highly skewed, extreme variability, fails normality tests\n")
cat("• Power law shows the 'few superstars, many average performers' pattern\n")
cat("• This has major implications for HR strategy and performance management\n") 