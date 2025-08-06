# De Corte (1994) Reproduction Analysis
# "Utility analysis for the one-cohort selection-retention decision with a probationary period"
# 
# This script reproduces the utility analysis calculations from De Corte (1994)
# which examines selection decisions with probationary periods and retention considerations.

# Load required libraries
library(dplyr)
library(ggplot2)
library(pracma)
library(mvtnorm)
library(knitr)
library(kableExtra)
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

# Create timestamp for this analysis
analysis_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ============================================================================
# INPUT PARAMETERS FROM DE CORTE (1994)
# ============================================================================

# Basic parameters
N <- 17         # Number of hires
n <- 136        # Size of applicant pool  
mu_s <- 22500   # Average service cost
Time <- 11      # Average number of time periods on the job for successful selectees
S_p <- 0.853    # Success ratio of the predictor
mu_y <- 25000   # Average payoff of job performance (one time period) in applicant population
C_t <- 10000    # Training cost per hire
C_p <- 200      # Cost per candidate of using the predictor
C_s <- 1000     # Separation cost per unsuccessful employee
rho_yR <- 0.85  # Correlation between money-valued payoff (Y) and observed rated performance (R)
sigma_y <- 7000 # Standard deviation of performance
rho <- 0.35     # Predictor validity (predictor-observed performance correlation)

# Recruitment costs
C_r_N0 <- 100   # Recruit cost per random selection
C_r_n <- 400    # Recruit cost per predictor selection

# ============================================================================
# CALCULATE CRITICAL VALUES
# ============================================================================

# Solve for x_c using the inverse CDF (quantile function)
x_c <- abs(qnorm(N/n))
cat("Critical predictor score (x_c):", round(x_c, 4), "\n")

# Solve for r_c using inputs
r_c <- (mu_s - mu_y)/(rho_yR*sigma_y)
cat("Critical performance score (r_c):", round(r_c, 4), "\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Standard normal density function
phi.1 <- function(x) {
  result <- (1/sqrt(2 * pi) * exp(-(x^2) / 2))
  return(result)
}

# ============================================================================
# FIGURE 1: SELECTION RATIO VISUALIZATION
# ============================================================================

# Create a data frame to map x values to y values
x_values.1 <- seq(-4, 4, length.out = 100)
data.1 <- data.frame(x = x_values.1, y = phi.1(x_values.1))
x_end.1 <- phi.1(x_c)

# Create the plot
plot.1 <- ggplot(data.1, aes(x = x, y = y)) +
  geom_line() +
  geom_segment(aes(xend = x_c, yend = phi.1(x_c)), x = x_c, y = 0, color = "blue") +
  annotate("text", x = x_end.1 + 1.4, y = phi.1(x_c), label = "phi_1(xc)", color = "blue") +
  annotate("text", x = x_c, y = -.015, label = "xc", color = "blue") +
  annotate("text", x = x_c + 1.65, y = phi.1(x_c)/2.5, label = "PHI_1(xc), selection ratio", color = "blue") +
  geom_ribbon(data = subset(data.1, x > x_c), aes(ymax = y, ymin = 0), fill = "lightblue", alpha = 0.5) +
  labs(x = "x", y = "y", 
       title = "Figure 1: Selection Ratio", 
       subtitle = "Selection ratio corresponding to the critical predictor score, xc") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Save the plot
ggsave("de_corte_figure1_selection_ratio.png", plot.1, width = 8, height = 6, dpi = 300)

# ============================================================================
# CALCULATE CONDITIONAL MEANS
# ============================================================================

# Define PHI.1 and PHI.2 (density of univariate std norm dist)
PHI.1.1 <- 1 - pnorm((x_c - rho*r_c)/sqrt(1-rho^2))
PHI.1.2 <- 1 - pnorm((r_c - rho*x_c)/sqrt(1-rho^2))

# Setup PHI.2(X,R) - bivariate normal probability
# Correlation matrix
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)

# Define lower and upper bounds for the region of interest
lower <- c(-.42, 1.15)  # These values need to be verified from the paper
upper <- c(Inf, Inf)

# Solve PHI.2 for density (probability) in specified region
PHI.2 <- pmvnorm(lower = lower, upper = upper, mean = rep(0, 2), corr = sigma)

cat("PHI.2 probability:", round(PHI.2[1], 4), "\n")

# Solve for mu_R_xcrc (conditional mean of R given x_c and r_c)
mu_R_xcrc <- (phi.1(r_c)*PHI.1.1 + rho*phi.1(x_c)*PHI.1.2)/PHI.2[1]

# Solve for mu_y_xcrc (conditional mean of Y given x_c and r_c)
mu_y_xcrc <- mu_y + rho_yR * sigma_y * mu_R_xcrc

# Solve for mu_x_xc (conditional mean of X given x_c)
mu_x_xc <- phi.1(x_c)/(1 - pnorm(x_c))

# Solve for mu_R_xc (conditional mean of R given x_c)
mu_R_xc <- rho * mu_x_xc

# Solve for mu_y_xc (conditional mean of Y given x_c)
mu_y_xc <- mu_y + rho_yR * sigma_y * mu_R_xc

# Calculate S_0 and mu_y_rc
S_0 <- pnorm(-r_c)

# Solve for mu_R_rc (conditional mean of R given r_c)
mu_R_rc <- phi.1(r_c)/S_0

# Solve for mu_y_rc (conditional mean of Y given r_c)
mu_y_rc <- mu_y + rho_yR * sigma_y * mu_R_rc

# ============================================================================
# UTILITY CALCULATIONS
# ============================================================================

# Utility of the predictor selected workforce (uncorrected for separation and recruitment costs)
U_p <- N * (mu_y_xc - mu_s) + (Time - 1) * N * S_p * (mu_y_xcrc - mu_s) - N * C_t - n * C_p

# Utility of random selection
U_0 <- N * (mu_y - mu_s) + (Time - 1) * N * S_0 * (mu_y_rc - mu_s) - N * C_t

# Utility difference
Delta_U <- U_p - U_0

cat("\n=== UTILITY ANALYSIS RESULTS ===\n")
cat("Utility with predictor selection (U_p):", round(U_p, 2), "\n")
cat("Utility with random selection (U_0):", round(U_0, 2), "\n")
cat("Utility difference (Delta_U):", round(Delta_U, 2), "\n")

# ============================================================================
# INTERMEDIATE CONDITIONAL PERFORMANCE CALCULATIONS
# ============================================================================

cat("\n=== INTERMEDIATE CONDITIONAL PERFORMANCE CALCULATIONS ===\n")
cat("μ_y(x_c) - Average payoff of all selectees:", round(mu_y_xc, 2), "\n")
cat("μ_y(x_c,r_c) - Average payoff of successful selectees:", round(mu_y_xcrc, 2), "\n")
cat("μ_y(r_c) - Average payoff of random successful employees:", round(mu_y_rc, 2), "\n")

# ============================================================================
# FIXED QUOTA ANALYSIS
# ============================================================================

# Fixed quota of successful selectees
N_0 <- (N/S_0) # Random selection required to get N successful hires

# Solve for N_p (number of applicants needed with predictor)
# Adjust x_c until N/n probability is achieved
x_c.adjust <- 1.046  # This value needs to be verified from the paper

# Correlation matrix for adjusted calculation
sigma.2 <- matrix(c(1, rho, rho, 1), nrow = 2)

# Define lower and upper bounds for adjusted calculation
lower <- c(-.42, x_c.adjust)
upper <- c(Inf, Inf)

# Solve PHI.2.adjust for density (probability) in specified region
PHI.2.adjust <- pmvnorm(lower = lower, upper = upper, mean = rep(0, 2), corr = sigma)

N_p <- (n * (1 - pnorm(x_c.adjust)))

PHI.1.1.adjust <- 1 - pnorm((x_c.adjust - rho*r_c)/sqrt(1-rho^2))
PHI.1.2.adjust <- 1 - pnorm((r_c - rho*x_c.adjust)/sqrt(1-rho^2))

# Solve for mu_y_xc.adjust.rc
mu_R_xc.adjust.rc <- (phi.1(r_c)*PHI.1.1.adjust + rho*phi.1(x_c.adjust)*PHI.1.2.adjust)/PHI.2.adjust[1]
mu_y_xc.adjust.rc <- mu_y + rho_yR * sigma_y * mu_R_xc.adjust.rc

# Solve for mu_y_xc.adjust
mu_x_xc.adjust <- phi.1(x_c.adjust)/(1 - pnorm(x_c.adjust))
mu_R_xc.adjust <- rho * mu_x_xc.adjust
mu_y_xc.adjust <- mu_y + rho_yR * sigma_y * mu_R_xc.adjust

# Solve Delta_U for a fixed quota of successful selectees
Delta_U.fixed.quota <-
  N_p * mu_y_xc.adjust - N_0 * mu_y -
  (N_p - N_0) * mu_s + (Time - 1) * N * 
  (mu_y_xc.adjust.rc - mu_y_rc) - (N_p - N_0) *
  C_t - n * C_p

# Adjust for separation and recruitment costs
Delta_U.fixed.quota.corrected <- 
  Delta_U.fixed.quota - (N_p - N_0) * C_s -
  (n * C_r_n - N_0 * C_r_N0)

cat("\n=== FIXED QUOTA ANALYSIS ===\n")
cat("Number needed with random selection (N_0):", round(N_0, 2), "\n")
cat("Number needed with predictor (N_p):", round(N_p, 2), "\n")
cat("Utility difference (fixed quota):", round(Delta_U.fixed.quota, 2), "\n")
cat("Utility difference (corrected):", round(Delta_U.fixed.quota.corrected, 2), "\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Create results summary
results_summary <- data.frame(
  Parameter = c("Critical predictor score (x_c)", 
                "Critical performance score (r_c)",
                "Selection ratio (N/n)",
                "Success ratio predictor (S_p)",
                "Success ratio random (S_0)",
                "μ_y(x_c) - Average payoff of all selectees",
                "μ_y(x_c,r_c) - Average payoff of successful selectees", 
                "μ_y(r_c) - Average payoff of random successful employees",
                "Utility with predictor (U_p)",
                "Utility with random (U_0)", 
                "Utility difference (Delta_U)",
                "Fixed quota utility difference",
                "Fixed quota utility difference (corrected)"),
  Value = c(round(x_c, 4),
            round(r_c, 4),
            round(N/n, 4),
            round(S_p, 4),
            round(S_0, 4),
            round(mu_y_xc, 2),
            round(mu_y_xcrc, 2),
            round(mu_y_rc, 2),
            round(U_p, 2),
            round(U_0, 2),
            round(Delta_U, 2),
            round(Delta_U.fixed.quota, 2),
            round(Delta_U.fixed.quota.corrected, 2))
)

# Save results
write.csv(results_summary, "de_corte_1994_results.csv", row.names = FALSE)

# Save R data
save(list = ls(), file = "de_corte_1994_results.RData")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to: de_corte_1994_results.csv\n")
cat("R data saved to: de_corte_1994_results.RData\n")
cat("Figure saved to: de_corte_figure1_selection_ratio.png\n") 