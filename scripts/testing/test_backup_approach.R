# Test Backup App's Calculation Approach
# Compare to current implementation to see which gives better Table 2 results

library(iopsych)
library(mvtnorm)
library(dplyr)

set.seed(42)
n_sims <- 10000

cat("=== TESTING BACKUP APP'S CALCULATION APPROACH ===\n\n")

# Generate parameters (using Sturman's ranges)
utility_data <- data.frame(
  n = round(exp(runif(n_sims, log(1), log(1100)))),
  t = runif(n_sims, 1, 10),
  select = runif(n_sims, 0.05, 1.0),
  r1 = runif(n_sims, 0.10, 0.70),
  sdy = runif(n_sims, 5000, 50000),
  cost = exp(runif(n_sims, log(10), log(1000))),
  
  # Economic parameters
  i = runif(n_sims, 0.05, 0.15),
  tax = runif(n_sims, 0.20, 0.40),
  vc = runif(n_sims, 0.10, 0.30),
  
  # Multiple devices
  incremental_r = runif(n_sims, 0.05, 0.25),
  
  # Top-down hiring
  initial_accept = runif(n_sims, 0.10, 0.40),
  perf_corr = runif(n_sims, -0.3, -0.1),
  
  # Probationary period
  cutoff_score = runif(n_sims, -1.5, -0.5),
  
  # Employee flows
  turnover_probability = runif(n_sims, 0.05, 0.25),
  performance_correlation_turnover = runif(n_sims, -0.3, -0.1),
  
  # Temporal validity
  performance_stability = runif(n_sims, 0.5, 0.9)
)

cat("Calculating utilities using backup app approach...\n")

# Basic utility (same in both approaches)
utility_data$unadjusted_utility <- with(utility_data, {
  n * t * iopsych::ux(select) * r1 * sdy - (n/select) * cost
})

# BACKUP APP'S ECONOMIC ADJUSTMENT
adjusted_utility_econ <- function(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t) {
  return((n * x2 * r1 * uxs * sdy * (1 - vc) * (1 - tax)) - ((n/sr) * cost * (1 - tax)))
}

utility_data$adjusted_utility_econ_backup <- apply(utility_data, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  r1 <- as.numeric(row["r1"])
  x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  uxs = iopsych::ux(sr)
  return(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t))
})

# BACKUP APP'S MULTIPLE DEVICES
utility_data$adjusted_utility_mult_backup <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  return(as.numeric(n*t*r*iopsych::ux(sr)*sdy-round((n/sr)*cost, 0)))
})

# BACKUP APP'S TOP-DOWN HIRING
utility.topdown <- function(n, t, r, pa, bxy, sdy, sr, cost) {
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  z <- (pa*n*iopsych::ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  return(n*t*r*z*sdy-round((n/sr)*cost, 0))
}

utility_data$adjusted_utility_topdown_backup <-  apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  return(as.numeric(utility.topdown(n, t, r, pa, bxy, sdy, sr, cost)))
})

# BACKUP APP'S PROBATIONARY PERIOD
utility.probation <- function(n, t, r, sr, sdy, rc, cost) {
  xc <- qnorm(1-sr)
  PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
  PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
  PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
  mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  return((n*sdy*r*iopsych::ux(sr)+(t-1)*n*sp*sdy*mur.xcrc-(n/sr)*cost)-((t-1)*n*sdy*so*mur.rc))
}

utility_data$adjusted_utility_probation_backup <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  return(as.numeric(utility.probation(n,t,r,sr,sdy,rc,cost)))
})

# BACKUP APP'S EMPLOYEE FLOWS
utility.flow <- function(n, t, r, sdy, pto, corrto, sr, cost){
  num_years <- seq(1, t)
  n_added <- c(rep(n, 1), rep(0, t - 1))
  n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
  n_cum <- cumsum(n_added - n_turn)
  zbarx <- c(rep(iopsych::ux(sr), 1), rep(iopsych::ux(sr)+corrto*iopsych::ux(1-pto), t-1))
  
  #Financially Adjusted Costs of Program in period (year)
  c_adj <- (n_added / sr) * cost
  unadjusted_utility <- n_cum * r * zbarx * sdy
  
  adjusted_utility <- unadjusted_utility - c_adj
  adjusted_utility <- sum(round(adjusted_utility, 0))
  return(adjusted_utility)
}

utility_data$adjusted_utility_flow_backup <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  return(as.numeric(utility.flow(n, t, r, sdy, pto, corrto, sr, cost)))
})

# BACKUP APP'S TEMPORAL VALIDITY
utility_data$adjusted_utility_temp_backup <- apply(utility_data, 1, function(row){
  t <- as.numeric(row["t"])
  n <- as.numeric(row["n"])
  select <- as.numeric(row["select"])
  r1 <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  cost <- as.numeric(row["cost"])
  stab <- as.numeric(row["performance_stability"])
  return(as.numeric((t * n * stab * iopsych::ux(select) * r1 * sdy) - (n/select)*cost))
})

# Calculate percentage changes
utility_data$percent_decrease_econ_backup <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_backup"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$percent_decrease_mult_backup <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_mult_backup"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$percent_decrease_topdown_backup <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_topdown_backup"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$percent_decrease_probation_backup <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_probation_backup"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$percent_decrease_flow_backup <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_flow_backup"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$percent_decrease_temp_backup <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_temp_backup"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

# Calculate results
cat("\n=== BACKUP APP APPROACH RESULTS ===\n")

backup_results <- data.frame(
  Adjustment = c("Economic", "Multiple", "TopDown", "Probation", "Flows", "Temporal"),
  Median = c(
    median(utility_data$percent_decrease_econ_backup, na.rm = TRUE),
    median(utility_data$percent_decrease_mult_backup, na.rm = TRUE),
    median(utility_data$percent_decrease_topdown_backup, na.rm = TRUE),
    median(utility_data$percent_decrease_probation_backup, na.rm = TRUE),
    median(utility_data$percent_decrease_flow_backup, na.rm = TRUE),
    median(utility_data$percent_decrease_temp_backup, na.rm = TRUE)
  ),
  Mean = c(
    mean(utility_data$percent_decrease_econ_backup, na.rm = TRUE),
    mean(utility_data$percent_decrease_mult_backup, na.rm = TRUE),
    mean(utility_data$percent_decrease_topdown_backup, na.rm = TRUE),
    mean(utility_data$percent_decrease_probation_backup, na.rm = TRUE),
    mean(utility_data$percent_decrease_flow_backup, na.rm = TRUE),
    mean(utility_data$percent_decrease_temp_backup, na.rm = TRUE)
  )
) %>%
  arrange(desc(Median))

print(backup_results)

cat("\n=== COMPARISON TO STURMAN'S TABLE 2 TARGETS ===\n")
cat("Backup App Results vs Sturman Targets:\n")
cat(sprintf("  Economic: %.1f%% (Target: 64%%)\n", backup_results$Median[backup_results$Adjustment == "Economic"]))
cat(sprintf("  Multiple: %.1f%% (Target: 53%%)\n", backup_results$Median[backup_results$Adjustment == "Multiple"]))
cat(sprintf("  TopDown:  %.1f%% (Target: 23%%)\n", backup_results$Median[backup_results$Adjustment == "TopDown"]))
cat(sprintf("  Probation:%.1f%% (Target: 22%%)\n", backup_results$Median[backup_results$Adjustment == "Probation"]))
cat(sprintf("  Flows:    %.1f%% (Target: 1%%)\n", backup_results$Median[backup_results$Adjustment == "Flows"]))
cat(sprintf("  Temporal: %.1f%% (Not in Sturman Table 2)\n", backup_results$Median[backup_results$Adjustment == "Temporal"]))

# Check ranking
sturman_ranking <- c("Economic", "Multiple", "TopDown", "Probation", "Flows")
backup_ranking <- backup_results$Adjustment[backup_results$Adjustment %in% sturman_ranking]
ranking_match <- identical(backup_ranking, sturman_ranking)

cat("\nRanking Comparison:\n")
cat("Backup ranking: ", paste(backup_ranking, collapse = " > "), "\n")
cat("Sturman ranking:", paste(sturman_ranking, collapse = " > "), "\n")
cat("Ranking matches Sturman:", ranking_match, "\n")

if (ranking_match) {
  cat("✅ SUCCESS: Backup app ranking matches Sturman!\n")
} else {
  cat("⚠️  Ranking differs from Sturman\n")
}

# Calculate differences from targets
econ_diff <- abs(backup_results$Median[backup_results$Adjustment == "Economic"] - 64)
mult_diff <- abs(backup_results$Median[backup_results$Adjustment == "Multiple"] - 53)
topdown_diff <- abs(backup_results$Median[backup_results$Adjustment == "TopDown"] - 23)
prob_diff <- abs(backup_results$Median[backup_results$Adjustment == "Probation"] - 22)
flows_diff <- abs(backup_results$Median[backup_results$Adjustment == "Flows"] - 1)

cat("\n=== ASSESSMENT ===\n")
cat("Differences from Sturman targets (percentage points):\n")
cat(sprintf("  Economic: %.1fpp\n", econ_diff))
cat(sprintf("  Multiple: %.1fpp\n", mult_diff))
cat(sprintf("  TopDown:  %.1fpp\n", topdown_diff))
cat(sprintf("  Probation:%.1fpp\n", prob_diff))
cat(sprintf("  Flows:    %.1fpp\n", flows_diff))

avg_diff <- mean(c(econ_diff, mult_diff, topdown_diff, prob_diff, flows_diff))
cat(sprintf("  Average difference: %.1fpp\n", avg_diff))

if (avg_diff < 10 && ranking_match) {
  cat("✅ EXCELLENT: Backup app approach is much closer to Sturman's Table 2!\n")
} else if (avg_diff < 20 && ranking_match) {
  cat("✅ GOOD: Backup app approach is closer to Sturman's results\n")
} else {
  cat("⚠️  Still some differences, but may be an improvement\n")
} 