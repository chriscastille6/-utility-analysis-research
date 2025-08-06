# De Corte (1994) Sensitivity Analysis
# This script explores how utility estimates change with different parameter values

# Load required libraries
library(dplyr)
library(ggplot2)
library(pracma)
library(mvtnorm)
library(tidyr)
library(scales)

# Set theme for plots
theme_set(theme_bw() +
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 14, face = "bold"),
                  legend.text = element_text(size = 10),
                  panel.background = element_rect(fill = "white"),
                  plot.background = element_rect(fill = "white")))

# ============================================================================
# BASE PARAMETERS (from De Corte 1994)
# ============================================================================

base_params <- list(
  N = 17,           # Number of hires
  n = 136,          # Size of applicant pool  
  mu_s = 22500,     # Average service cost
  Time = 11,        # Average number of time periods on the job
  S_p = 0.853,      # Success ratio of the predictor
  mu_y = 25000,     # Average payoff of job performance
  C_t = 10000,      # Training cost per hire
  C_p = 200,        # Cost per candidate of using the predictor
  C_s = 1000,       # Separation cost per unsuccessful employee
  rho_yR = 0.85,    # Correlation between payoff and performance
  sigma_y = 7000,   # Standard deviation of performance
  rho = 0.35        # Predictor validity
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Standard normal density function
phi.1 <- function(x) {
  result <- (1/sqrt(2 * pi) * exp(-(x^2) / 2))
  return(result)
}

# Function to calculate utility with given parameters
calculate_utility <- function(params) {
  # Extract parameters
  N <- params$N
  n <- params$n
  mu_s <- params$mu_s
  Time <- params$Time
  S_p <- params$S_p
  mu_y <- params$mu_y
  C_t <- params$C_t
  C_p <- params$C_p
  C_s <- params$C_s
  rho_yR <- params$rho_yR
  sigma_y <- params$sigma_y
  rho <- params$rho
  
  # Calculate critical values
  x_c <- abs(qnorm(N/n))
  r_c <- (mu_s - mu_y)/(rho_yR*sigma_y)
  
  # Calculate conditional means
  PHI.1.1 <- 1 - pnorm((x_c - rho*r_c)/sqrt(1-rho^2))
  PHI.1.2 <- 1 - pnorm((r_c - rho*x_c)/sqrt(1-rho^2))
  
  # Bivariate normal probability (using fixed bounds from original)
  sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
  lower <- c(-.42, 1.15)
  upper <- c(Inf, Inf)
  PHI.2 <- pmvnorm(lower = lower, upper = upper, mean = rep(0, 2), corr = sigma)
  
  # Calculate conditional means
  mu_R_xcrc <- (phi.1(r_c)*PHI.1.1 + rho*phi.1(x_c)*PHI.1.2)/PHI.2[1]
  mu_y_xcrc <- mu_y + rho_yR * sigma_y * mu_R_xcrc
  
  mu_x_xc <- phi.1(x_c)/(1 - pnorm(x_c))
  mu_R_xc <- rho * mu_x_xc
  mu_y_xc <- mu_y + rho_yR * sigma_y * mu_R_xc
  
  S_0 <- pnorm(-r_c)
  mu_R_rc <- phi.1(r_c)/S_0
  mu_y_rc <- mu_y + rho_yR * sigma_y * mu_R_rc
  
  # Calculate utilities
  U_p <- N * (mu_y_xc - mu_s) + (Time - 1) * N * S_p * (mu_y_xcrc - mu_s) - N * C_t - n * C_p
  U_0 <- N * (mu_y - mu_s) + (Time - 1) * N * S_0 * (mu_y_rc - mu_s) - N * C_t
  Delta_U <- U_p - U_0
  
  return(list(
    U_p = U_p,
    U_0 = U_0,
    Delta_U = Delta_U,
    x_c = x_c,
    r_c = r_c,
    S_0 = S_0
  ))
}

# ============================================================================
# SENSITIVITY ANALYSES
# ============================================================================

# 1. Predictor Validity Sensitivity
validity_range <- seq(0.1, 0.8, by = 0.05)
validity_results <- data.frame()

for (rho in validity_range) {
  params <- base_params
  params$rho <- rho
  
  result <- calculate_utility(params)
  
  validity_results <- rbind(validity_results, data.frame(
    rho = rho,
    U_p = result$U_p,
    U_0 = result$U_0,
    Delta_U = result$Delta_U,
    S_0 = result$S_0
  ))
}

# 2. Success Ratio Sensitivity
success_range <- seq(0.6, 0.95, by = 0.05)
success_results <- data.frame()

for (S_p in success_range) {
  params <- base_params
  params$S_p <- S_p
  
  result <- calculate_utility(params)
  
  success_results <- rbind(success_results, data.frame(
    S_p = S_p,
    U_p = result$U_p,
    U_0 = result$U_0,
    Delta_U = result$Delta_U
  ))
}

# 3. Training Cost Sensitivity
training_cost_range <- seq(5000, 20000, by = 1000)
training_results <- data.frame()

for (C_t in training_cost_range) {
  params <- base_params
  params$C_t <- C_t
  
  result <- calculate_utility(params)
  
  training_results <- rbind(training_results, data.frame(
    C_t = C_t,
    U_p = result$U_p,
    U_0 = result$U_0,
    Delta_U = result$Delta_U
  ))
}

# 4. Performance Standard Deviation Sensitivity
sd_range <- seq(5000, 12000, by = 500)
sd_results <- data.frame()

for (sigma_y in sd_range) {
  params <- base_params
  params$sigma_y <- sigma_y
  
  result <- calculate_utility(params)
  
  sd_results <- rbind(sd_results, data.frame(
    sigma_y = sigma_y,
    U_p = result$U_p,
    U_0 = result$U_0,
    Delta_U = result$Delta_U
  ))
}

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

# 1. Validity Sensitivity Plot
p1 <- ggplot(validity_results, aes(x = rho)) +
  geom_line(aes(y = Delta_U/1000), color = "blue", size = 1) +
  geom_point(aes(y = Delta_U/1000), color = "blue", size = 2) +
  geom_vline(xintercept = 0.35, linetype = "dashed", color = "red") +
  labs(x = "Predictor Validity (ρ)", 
       y = "Utility Difference (ΔU) in $K",
       title = "Sensitivity to Predictor Validity",
       subtitle = "Red dashed line = Original value (0.35)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# 2. Success Ratio Sensitivity Plot
p2 <- ggplot(success_results, aes(x = S_p)) +
  geom_line(aes(y = Delta_U/1000), color = "green", size = 1) +
  geom_point(aes(y = Delta_U/1000), color = "green", size = 2) +
  geom_vline(xintercept = 0.853, linetype = "dashed", color = "red") +
  labs(x = "Success Ratio (S_p)", 
       y = "Utility Difference (ΔU) in $K",
       title = "Sensitivity to Success Ratio",
       subtitle = "Red dashed line = Original value (0.853)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# 3. Training Cost Sensitivity Plot
p3 <- ggplot(training_results, aes(x = C_t/1000)) +
  geom_line(aes(y = Delta_U/1000), color = "purple", size = 1) +
  geom_point(aes(y = Delta_U/1000), color = "purple", size = 2) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red") +
  labs(x = "Training Cost (C_t) in $K", 
       y = "Utility Difference (ΔU) in $K",
       title = "Sensitivity to Training Cost",
       subtitle = "Red dashed line = Original value ($10K)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# 4. Performance SD Sensitivity Plot
p4 <- ggplot(sd_results, aes(x = sigma_y/1000)) +
  geom_line(aes(y = Delta_U/1000), color = "orange", size = 1) +
  geom_point(aes(y = Delta_U/1000), color = "orange", size = 2) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
  labs(x = "Performance Standard Deviation (σ_y) in $K", 
       y = "Utility Difference (ΔU) in $K",
       title = "Sensitivity to Performance Variability",
       subtitle = "Red dashed line = Original value ($7K)") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Save plots
ggsave("sensitivity_validity.png", p1, width = 8, height = 6, dpi = 300)
ggsave("sensitivity_success_ratio.png", p2, width = 8, height = 6, dpi = 300)
ggsave("sensitivity_training_cost.png", p3, width = 8, height = 6, dpi = 300)
ggsave("sensitivity_performance_sd.png", p4, width = 8, height = 6, dpi = 300)

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

# Calculate summary statistics for each sensitivity analysis
sensitivity_summary <- data.frame(
  Parameter = c("Predictor Validity (ρ)", "Success Ratio (S_p)", 
                "Training Cost (C_t)", "Performance SD (σ_y)"),
  Original_Value = c(0.35, 0.853, 10000, 7000),
  Min_Utility = c(min(validity_results$Delta_U), min(success_results$Delta_U),
                  min(training_results$Delta_U), min(sd_results$Delta_U)),
  Max_Utility = c(max(validity_results$Delta_U), max(success_results$Delta_U),
                  max(training_results$Delta_U), max(sd_results$Delta_U)),
  Range = c(max(validity_results$Delta_U) - min(validity_results$Delta_U),
            max(success_results$Delta_U) - min(success_results$Delta_U),
            max(training_results$Delta_U) - min(training_results$Delta_U),
            max(sd_results$Delta_U) - min(sd_results$Delta_U))
)

# Save results
write.csv(validity_results, "sensitivity_validity_results.csv", row.names = FALSE)
write.csv(success_results, "sensitivity_success_results.csv", row.names = FALSE)
write.csv(training_results, "sensitivity_training_results.csv", row.names = FALSE)
write.csv(sd_results, "sensitivity_sd_results.csv", row.names = FALSE)
write.csv(sensitivity_summary, "sensitivity_summary.csv", row.names = FALSE)

# Print summary
cat("\n=== SENSITIVITY ANALYSIS SUMMARY ===\n")
cat("Results saved to CSV files\n")
cat("Plots saved as PNG files\n")
cat("\nParameter ranges tested:\n")
cat("- Predictor validity: 0.1 to 0.8\n")
cat("- Success ratio: 0.6 to 0.95\n")
cat("- Training cost: $5K to $20K\n")
cat("- Performance SD: $5K to $12K\n")

# Display summary table
print(sensitivity_summary) 