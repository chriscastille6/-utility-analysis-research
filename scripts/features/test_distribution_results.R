# Test Distribution Results - Demonstrating Normal vs Power Law
# This script generates samples and applies statistical tests to clearly show
# the difference between normal and power law distributions

library(poweRlaw)

# Set seed for reproducibility
set.seed(42)

# Generate samples
n <- 500
normal_sample <- rnorm(n, mean = 200, sd = 40)

# Generate power law sample with target mean of 200
alpha <- 2.5
xmin <- 100
power_law_sample <- rplcon(n, xmin = xmin, alpha = alpha)
# Scale to match target mean
power_law_sample <- power_law_sample * (200 / mean(power_law_sample))

# Function to apply all tests and return results
test_distribution <- function(data, distribution_name) {
  results <- list()
  
  # Basic descriptive statistics
  results$mean <- mean(data)
  results$median <- median(data)
  results$sd <- sd(data)
  
  # Normality tests
  if (length(data) <= 5000) {
    shapiro_test <- shapiro.test(data)
    results$shapiro_w <- shapiro_test$statistic
    results$shapiro_p <- shapiro_test$p.value
  } else {
    results$shapiro_w <- NA
    results$shapiro_p <- NA
  }
  
  # KS test against normal distribution
  ks_normal <- ks.test(data, "pnorm", mean = mean(data), sd = sd(data))
  results$ks_normal_d <- ks_normal$statistic
  results$ks_normal_p <- ks_normal$p.value
  
  # Power law specific tests
  pl_fit <- try({
    pl_obj <- displ$new(data)
    pl_obj$setXmin(estimate_xmin(pl_obj))
    pl_obj
  }, silent = TRUE)
  
  if (!inherits(pl_fit, "try-error")) {
    # Clauset-Shalizi-Newman test
    csn_test <- try({
      bootstrap_p(pl_fit, no_of_sims = 100, threads = 1)
    }, silent = TRUE)
    
    if (!inherits(csn_test, "try-error")) {
      results$csn_p <- csn_test$p
    } else {
      results$csn_p <- NA
    }
    
    # Parameter estimates
    results$alpha_estimate <- pl_fit$getPars()
    results$xmin_estimate <- pl_fit$getXmin()
    
    # Likelihood ratio tests
    # vs Exponential
    exp_fit <- try({
      exp_obj <- disexp$new(data)
      exp_obj$setXmin(pl_fit$getXmin())
      exp_obj$setPars(estimate_pars(exp_obj))
      exp_obj
    }, silent = TRUE)
    
    if (!inherits(exp_fit, "try-error")) {
      lr_exp <- try({
        compare_distributions(pl_fit, exp_fit)
      }, silent = TRUE)
      
      if (!inherits(lr_exp, "try-error")) {
        results$lr_exp_p <- lr_exp$p
        results$lr_exp_ratio <- lr_exp$test_statistic
      } else {
        results$lr_exp_p <- NA
        results$lr_exp_ratio <- NA
      }
    } else {
      results$lr_exp_p <- NA
      results$lr_exp_ratio <- NA
    }
    
    # vs Log-normal
    ln_fit <- try({
      ln_obj <- dislnorm$new(data)
      ln_obj$setXmin(pl_fit$getXmin())
      ln_obj$setPars(estimate_pars(ln_obj))
      ln_obj
    }, silent = TRUE)
    
    if (!inherits(ln_fit, "try-error")) {
      lr_ln <- try({
        compare_distributions(pl_fit, ln_fit)
      }, silent = TRUE)
      
      if (!inherits(lr_ln, "try-error")) {
        results$lr_ln_p <- lr_ln$p
        results$lr_ln_ratio <- lr_ln$test_statistic
      } else {
        results$lr_ln_p <- NA
        results$lr_ln_ratio <- NA
      }
    } else {
      results$lr_ln_p <- NA
      results$lr_ln_ratio <- NA
    }
    
  } else {
    results$csn_p <- NA
    results$alpha_estimate <- NA
    results$xmin_estimate <- NA
    results$lr_exp_p <- NA
    results$lr_exp_ratio <- NA
    results$lr_ln_p <- NA
    results$lr_ln_ratio <- NA
  }
  
  return(results)
}

# Apply tests to both samples
cat("Testing Normal and Power Law Samples\n")
cat("====================================\n\n")

normal_results <- test_distribution(normal_sample, "Normal")
powerlaw_results <- test_distribution(power_law_sample, "Power Law")

# Helper function to format values safely
format_value <- function(val, digits = 4) {
  if (is.na(val)) return("N/A")
  if (is.numeric(val)) {
    if (val < 0.0001) return(format.pval(val, digits = digits))
    return(round(val, digits))
  }
  return(as.character(val))
}

# Create comprehensive results table with careful counting
results_data <- list(
  # Row 1-4: Basic statistics
  c("Sample Size", n, n),
  c("Mean", round(normal_results$mean, 1), round(powerlaw_results$mean, 1)),
  c("Median", round(normal_results$median, 1), round(powerlaw_results$median, 1)),
  c("Standard Deviation", round(normal_results$sd, 1), round(powerlaw_results$sd, 1)),
  
  # Row 5: Spacer
  c("", "", ""),
  
  # Row 6: Header
  c("=== NORMALITY TESTS ===", "", ""),
  
  # Row 7-9: Shapiro-Wilk
  c("Shapiro-Wilk W statistic", format_value(normal_results$shapiro_w), format_value(powerlaw_results$shapiro_w)),
  c("Shapiro-Wilk p-value", format.pval(normal_results$shapiro_p, digits = 4), format.pval(powerlaw_results$shapiro_p, digits = 4)),
  c("Shapiro-Wilk Interpretation", 
    ifelse(normal_results$shapiro_p > 0.05, "Normal", "Not Normal"),
    ifelse(powerlaw_results$shapiro_p > 0.05, "Normal", "Not Normal")),
  
  # Row 10: Spacer
  c("", "", ""),
  
  # Row 11-13: KS test
  c("KS vs Normal D statistic", format_value(normal_results$ks_normal_d), format_value(powerlaw_results$ks_normal_d)),
  c("KS vs Normal p-value", format.pval(normal_results$ks_normal_p, digits = 4), format.pval(powerlaw_results$ks_normal_p, digits = 4)),
  c("KS vs Normal Interpretation",
    ifelse(normal_results$ks_normal_p > 0.05, "Normal", "Not Normal"),
    ifelse(powerlaw_results$ks_normal_p > 0.05, "Normal", "Not Normal")),
  
  # Row 14: Spacer
  c("", "", ""),
  
  # Row 15: Header
  c("=== POWER LAW TESTS ===", "", ""),
  
  # Row 16-17: CSN test
  c("Clauset-Shalizi-Newman p-value",
    ifelse(is.na(normal_results$csn_p), "Test Failed", format.pval(normal_results$csn_p, digits = 4)),
    ifelse(is.na(powerlaw_results$csn_p), "Test Failed", format.pval(powerlaw_results$csn_p, digits = 4))),
  c("CSN Interpretation",
    ifelse(is.na(normal_results$csn_p), "N/A", ifelse(normal_results$csn_p > 0.05, "Power Law", "Not Power Law")),
    ifelse(is.na(powerlaw_results$csn_p), "N/A", ifelse(powerlaw_results$csn_p > 0.05, "Power Law", "Not Power Law"))),
  
  # Row 18: Spacer
  c("", "", ""),
  
  # Row 19-20: Parameters
  c("Power Law α (alpha)",
    ifelse(is.na(normal_results$alpha_estimate), "N/A", round(normal_results$alpha_estimate, 3)),
    ifelse(is.na(powerlaw_results$alpha_estimate), "N/A", round(powerlaw_results$alpha_estimate, 3))),
  c("Power Law xmin",
    ifelse(is.na(normal_results$xmin_estimate), "N/A", round(normal_results$xmin_estimate, 1)),
    ifelse(is.na(powerlaw_results$xmin_estimate), "N/A", round(powerlaw_results$xmin_estimate, 1))),
  
  # Row 21: Spacer
  c("", "", ""),
  
  # Row 22: Header
  c("=== LIKELIHOOD RATIO TESTS ===", "", ""),
  
  # Row 23-24: vs Exponential
  c("Power Law vs Exponential p-value",
    ifelse(is.na(normal_results$lr_exp_p), "Test Failed", format.pval(normal_results$lr_exp_p, digits = 4)),
    ifelse(is.na(powerlaw_results$lr_exp_p), "Test Failed", format.pval(powerlaw_results$lr_exp_p, digits = 4))),
  c("PL vs Exp Interpretation",
    ifelse(is.na(normal_results$lr_exp_p), "N/A", ifelse(normal_results$lr_exp_p < 0.05, "Power Law Better", "No Difference")),
    ifelse(is.na(powerlaw_results$lr_exp_p), "N/A", ifelse(powerlaw_results$lr_exp_p < 0.05, "Power Law Better", "No Difference"))),
  
  # Row 25: Spacer
  c("", "", ""),
  
  # Row 26-27: vs Log-Normal
  c("Power Law vs Log-Normal p-value",
    ifelse(is.na(normal_results$lr_ln_p), "Test Failed", format.pval(normal_results$lr_ln_p, digits = 4)),
    ifelse(is.na(powerlaw_results$lr_ln_p), "Test Failed", format.pval(powerlaw_results$lr_ln_p, digits = 4))),
  c("PL vs LN Interpretation",
    ifelse(is.na(normal_results$lr_ln_p), "N/A", ifelse(normal_results$lr_ln_p < 0.05, "Power Law Better", "No Difference")),
    ifelse(is.na(powerlaw_results$lr_ln_p), "N/A", ifelse(powerlaw_results$lr_ln_p < 0.05, "Power Law Better", "No Difference")))
)

# Convert to data frame
results_table <- do.call(rbind, results_data)
colnames(results_table) <- c("Test", "Normal_Sample", "Power_Law_Sample")
results_table <- as.data.frame(results_table, stringsAsFactors = FALSE)

# Print the results table
print(results_table, row.names = FALSE)

cat("\n\nINTERPRETATION:\n")
cat("==============\n")
cat("• Normality Tests: p > 0.05 = Normal, p ≤ 0.05 = Not Normal\n")
cat("• Clauset-Shalizi-Newman Test: p > 0.05 = Power Law, p ≤ 0.05 = Not Power Law\n") 
cat("• Likelihood Ratio Tests: p < 0.05 = Power Law significantly better\n")
cat("• α (alpha): Lower values = heavier tail, higher values = lighter tail\n")
cat("• xmin: Minimum value from which power law behavior begins\n\n")

cat("EXPECTED RESULTS:\n")
cat("================\n")
cat("• Normal Sample: Should pass normality tests, fail power law tests\n")
cat("• Power Law Sample: Should fail normality tests, pass power law tests\n")
cat("• Power Law Sample: Should show preference for power law in likelihood ratios\n") 