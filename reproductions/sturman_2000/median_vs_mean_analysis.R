# MEDIAN VS MEAN ANALYSIS: THE KEY TO STURMAN'S 291%
# ==================================================
# Sturman specifically said: "Because the distributions of the effects were heavily skewed, 
# it made more sense to examine the median effect as a measure of adjustment strength 
# rather than mean effect, which could be biased by a few extreme values."
#
# He reported: "After applying all five adjustments, the median effect size of the 
# total set of adjustments was 291%, and the mean effect was 298%."

library(dplyr)
library(ggplot2)

set.seed(42)
n_sims <- 10000

cat("MEDIAN VS MEAN ANALYSIS: STURMAN'S KEY INSIGHT\n")
cat("=============================================\n")
cat("Sturman emphasized medians over means due to heavily skewed distributions.\n")
cat("Let's examine our distributions and compare median vs mean effects.\n\n")

# Standard normal ordinate function
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

# Generate parameters using Sturman's Table 1 ranges
params <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  sr = runif(n_sims, 0.05, 1.0),
  r = runif(n_sims, 0.10, 0.70),
  sdy = runif(n_sims, 5000, 50000),
  cost = exp(runif(n_sims, log(10), log(1000))),
  
  # Economic parameters
  discount = runif(n_sims, 0.05, 0.12),
  tax = runif(n_sims, 0.25, 0.35),
  vc = runif(n_sims, 0.15, 0.25),
  
  # Multiple devices
  r_old = runif(n_sims, 0.10, 0.30),
  
  # Other adjustments
  reject_rate = runif(n_sims, 0.15, 0.35),
  corr_perf_accept = runif(n_sims, -0.25, -0.15),
  prob_cutoff = runif(n_sims, -1.2, -0.8),
  turnover_rate = runif(n_sims, 0.08, 0.20),
  perf_turn_corr = runif(n_sims, -0.25, -0.15)
)

# Calculate all utility values
params$utility_basic <- with(params, {
  n * t * ux(sr) * r * sdy - (n/sr) * cost
})

# All adjustments combined
params$utility_all <- with(params, {
  incremental_r <- r - r_old
  p_accept <- 1 - reject_rate
  z_adj <- ux(sr) * p_accept + corr_perf_accept * dnorm(qnorm(1 - reject_rate))
  prob_retained <- pnorm(prob_cutoff, lower.tail = FALSE)
  
  effective_n <- n * prob_retained
  avg_workforce <- effective_n * (1 - turnover_rate/2)
  performance_effect <- 1 + (perf_turn_corr * turnover_rate)
  
  annual_benefit <- avg_workforce * incremental_r * z_adj * sdy * performance_effect
  annual_variable_costs <- annual_benefit * vc
  annual_net_benefit <- annual_benefit - annual_variable_costs
  
  pv_factor <- ifelse(discount > 0, 
                     (1 - (1 + discount)^(-t)) / discount,
                     t)
  
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - tax)
  initial_costs <- (n/sr) * cost
  replacement_costs <- n * turnover_rate * t * cost * 0.3
  
  after_tax_pv - initial_costs - replacement_costs
})

# Calculate percentage reduction
params$pct_reduction <- 100 * (params$utility_basic - params$utility_all) / abs(params$utility_basic)

# Remove infinite and NA values
params_clean <- params[is.finite(params$pct_reduction), ]
cat("Removed", nrow(params) - nrow(params_clean), "infinite/NA values\n")
cat("Analyzing", nrow(params_clean), "valid scenarios\n\n")

# Calculate statistics
median_reduction <- median(params_clean$pct_reduction, na.rm = TRUE)
mean_reduction <- mean(params_clean$pct_reduction, na.rm = TRUE)
sd_reduction <- sd(params_clean$pct_reduction, na.rm = TRUE)

cat("DISTRIBUTION STATISTICS\n")
cat("======================\n")
cat("Median reduction:", round(median_reduction, 1), "%\n")
cat("Mean reduction:", round(mean_reduction, 1), "%\n")
cat("Standard deviation:", round(sd_reduction, 1), "%\n")
cat("Difference (Mean - Median):", round(mean_reduction - median_reduction, 1), "%\n\n")

# Check for skewness
skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  (n / ((n-1) * (n-2))) * sum(((x - mean_x) / sd_x)^3)
}

skew_value <- skewness(params_clean$pct_reduction)
cat("Skewness:", round(skew_value, 3), "\n")
cat("(Positive = right-skewed, Negative = left-skewed, 0 = symmetric)\n\n")

# Quantiles
quantiles <- quantile(params_clean$pct_reduction, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
cat("DISTRIBUTION QUANTILES\n")
cat("=====================\n")
for(i in 1:length(quantiles)) {
  cat(sprintf("%2.0f%%: %6.1f%%\n", as.numeric(names(quantiles)[i]) * 100, quantiles[i]))
}
cat("\n")

# Check for extreme values
cat("EXTREME VALUES ANALYSIS\n")
cat("======================\n")
cat("Minimum reduction:", round(min(params_clean$pct_reduction), 1), "%\n")
cat("Maximum reduction:", round(max(params_clean$pct_reduction), 1), "%\n")

# Count extreme values
extreme_high <- sum(params_clean$pct_reduction > 500)
extreme_low <- sum(params_clean$pct_reduction < 0)
cat("Values > 500%:", extreme_high, "(", round(100 * extreme_high / nrow(params_clean), 2), "%)\n")
cat("Values < 0% (negative):", extreme_low, "(", round(100 * extreme_low / nrow(params_clean), 2), "%)\n\n")

cat("COMPARISON WITH STURMAN'S RESULTS\n")
cat("================================\n")
cat("Sturman's median: 291%\n")
cat("Our median:", round(median_reduction, 1), "%\n")
cat("Gap:", round(abs(median_reduction - 291), 1), "percentage points\n\n")

cat("Sturman's mean: 298%\n")
cat("Our mean:", round(mean_reduction, 1), "%\n")
cat("Gap:", round(abs(mean_reduction - 298), 1), "percentage points\n\n")

# Look at the distribution shape
cat("DISTRIBUTION SHAPE ANALYSIS\n")
cat("==========================\n")
if(abs(skew_value) > 1) {
  cat("Distribution is HEAVILY skewed (|skewness| > 1)\n")
} else if(abs(skew_value) > 0.5) {
  cat("Distribution is MODERATELY skewed (|skewness| > 0.5)\n")
} else {
  cat("Distribution is APPROXIMATELY symmetric (|skewness| < 0.5)\n")
}

if(skew_value > 0) {
  cat("Right-skewed: Long tail extending toward higher values\n")
  cat("This means a few very high reduction values are pulling the mean up\n")
} else if(skew_value < 0) {
  cat("Left-skewed: Long tail extending toward lower values\n")
  cat("This means a few very low reduction values are pulling the mean down\n")
}

cat("\nSTURMAN'S INSIGHT:\n")
cat("Sturman specifically chose MEDIAN over MEAN because the distributions\n")
cat("were 'heavily skewed' and the mean 'could be biased by a few extreme values.'\n")
cat("Our skewness of", round(skew_value, 3), "suggests")
if(abs(skew_value) > 1) {
  cat(" HEAVY skewness, confirming Sturman's observation.\n")
} else {
  cat(" our distribution may be less skewed than Sturman's.\n")
}

cat("\nCONCLUSION:\n")
cat("==========\n")
cat("Even focusing on medians (as Sturman recommended), we still have:\n")
cat("- Our median:", round(median_reduction, 1), "%\n")
cat("- Sturman's target: 291%\n")
cat("- Gap:", round(abs(median_reduction - 291), 1), "percentage points\n")
cat("\nThe median vs mean distinction doesn't explain the 291% discrepancy.\n")
cat("The issue appears to be more fundamental - likely in parameter ranges,\n")
cat("calculation methods, or utility formulas.\n") 