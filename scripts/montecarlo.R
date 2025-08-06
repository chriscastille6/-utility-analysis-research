##### Sturman (2000) #####
set.seed(12845)

# load packages
library(scales)
library(ggplot2)
library(iopsych)
library(mvtnorm)

##### Step 1: Build basic utility formula parameters (adjustment 0 in Sturman table 1) #####
# Define the population of parameters to randomly sample from
t = c(1:10)                               # expected tenure of staying in years
n = seq(log(1), log(1100), by=log(1100)/1100)       # Create a sequence from 1 to 1100 in increments of 1 for n         # 1-50 applicants per hire
select = seq(1, 50, by = .01)          # for example, from 0.01 to 1 in increments of 0.01
sdy = seq(4000, 40000, by=200)            # standard deviation of job performance value
cost = seq(0, log(1100), by=log(1100)/110)# cost per applicant of operating the new selection program
r1 = seq(.10, .77, by=.01)                # operational validity of the new selection program
r2 = seq(.05, .38, by=.01)                # operational validity of the old selection program
# Number of samples. Sturman called for 10000
num_samples <- 10000
cost = sample(cost, num_samples, replace = TRUE)
n = sample(n, num_samples, replace = TRUE)
select = sample(select, num_samples, replace = TRUE)
# Generate random samples for all parameters
utility_data <- data.frame(
  t = sample(t, num_samples, replace = TRUE),
  n = exp(n),
  select = 1/select,
  sdy = sample(sdy, num_samples, replace = TRUE),
  cost = exp(cost),
  r1 = sample(r1, num_samples, replace = TRUE)
)


utility_data$unadjusted_cost <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  cost <- as.numeric(row["cost"])
  select <- as.numeric(row["select"])
  return(as.numeric(round(n * cost / select, 0)))
})

utility_data$unadjusted_utility <- apply(utility_data, 1, function(row){
  t <- as.numeric(row["t"])
  n <- as.numeric(row["n"])
  select <- as.numeric(row["select"])
  r1 <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  unadjusted_cost <- as.numeric(row["unadjusted_cost"])
  return(as.numeric((t * n * iopsych::ux(select) * r1 * sdy) - unadjusted_cost))
})

##### Step 2: Account for # Economic adjustments (adjustment 1 in Sturman table 1).  #####
# Variables for adjustment
i = seq(.01, .11, by=.01)          # discount rate
tax = seq(.30, .63, by=.01)        # marginal tax rate
vc = seq(.02,.35, by=.01)          # variable costs

# Place these values into the dataset that we built. Sample random values for each row in the dataset
utility_data$i <- sample(i, nrow(utility_data), replace = TRUE)
utility_data$tax <- sample(tax, nrow(utility_data), replace = TRUE)
utility_data$vc <- sample(vc, nrow(utility_data), replace = TRUE)

# build adjustment formula for econ
adjusted_utility_econ <- function(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t) {
  return ((n*x2*r1*uxs*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
}

# compute adjusted utility for each row
utility_data$adjusted_utility_econ <- apply(utility_data, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  t <- as.numeric(row["t"])
  r1 <- as.numeric(row["r1"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  uxs = iopsych::ux(sr)
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

#compute effect
utility_data$percent_decrease_econ <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

# View the first few rows of the updated dataset
mean.econ <- mean(utility_data$percent_decrease_econ)
median.econ <- median(utility_data$percent_decrease_econ)

##### Step 3: Account for # Multiple Devices (adjustment 5 in Sturman table 1).  #####

incremental_r = seq(.05, .38, by = .01)
utility_data$incremental_r <- sample(incremental_r, nrow(utility_data), replace = TRUE)
utility <- function(n, t, r, sdy, sr, cost) {
  return(n*t*r*iopsych::ux(sr)*sdy-round((n/sr)*cost, 0))
}

#compute adjusted utility for multiple devices
utility_data$adjusted_utility_mult <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  return(as.numeric(utility(n, t, r, sdy, sr, cost)))
})

#compute effect
utility_data$percent_decrease_mult <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_mult"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

mean.mult <- mean(utility_data$percent_decrease_mult)
median.mult <- median(utility_data$percent_decrease_mult)

##### Step 4: Account for # Deviations from Top-Down Hiring (adjustment 6 in Sturman table 1).  #####
pa <- seq(.2, .7, by = .01) # initial acceptance rate
bxy <- seq(-.5, 0, by = .01) # correlation between acceptance and performance
utility_data$initial_accept <- sample(pa, nrow(utility_data), replace = TRUE)
utility_data$perf_corr <- sample(bxy, nrow(utility_data), replace = TRUE)
utility.topdown <- function(n, t, r, pa, bxy, sdy, sr, cost) {
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  return(n*t*r*z*sdy-round((n/sr)*cost, 0))
}

#compute adjusted utility for deviations from top down hiring

utility_data$adjusted_utility_topdown <-  apply(utility_data, 1, function(row){
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

#compute effect
utility_data$decrease_topdown <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_topdown"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

mean.topdown <- mean(utility_data$decrease_topdown)
median.topdown <- median(utility_data$decrease_topdown)

##### Step 5: Account for # Probationary Period (adjustment 4 in Sturman table 1).  #####

rc <- seq(-2, 0, by = .01) # cutoff score
utility_data$cutoff_score <- sample(rc, nrow(utility_data), replace = TRUE)

utility.probation <- function(n, t, r, sr, sdy, rc, cost) {
  xc <- qnorm(1-sr)
  PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
  PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
  PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
  mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  return((n*sdy*r*ux(sr)+(t-1)*n*sp*sdy*mur.xcrc-(n/sr)*cost)-((t-1)*n*sdy*so*mur.rc))
}

utility_data$adjusted_utility_probation <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  return(as.numeric(utility.probation(n,t,r,sr,sdy,rc,cost)))
})

#compute effect
utility_data$decrease_probation <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_probation"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

mean.probation <- mean(utility_data$decrease_probation)
median.probation <- median(utility_data$decrease_probation)

##### Step 6: Account for # Employee Flows (adjustment 2 in Sturman table 1).  #####
pto <- seq(0, .33, by = .01) # turnover probability
corrto <- seq(0, .5, by = .01) # turnover performance correlation

utility_data$turnover_probability <- sample(pto, nrow(utility_data), replace = TRUE)
utility_data$performance_correlation_turnover <- sample(corrto, nrow(utility_data), replace = TRUE)

utility.flow <- function(n, t, r, sdy, pto, corrto, sr, cost){
  num_years <- seq(1, t)
  n_added <- c(rep(n, 1), rep(0, t - 1))
  n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
  n_cum <- cumsum(n_added - n_turn)
  zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
  
  #Financially Adjusted Costs of Program in period (year)
  c_adj <- (n_added / sr) * cost
  unadjusted_utility <- n_cum * r * zbarx * sdy
  
  adjusted_utility <- unadjusted_utility - c_adj
  adjusted_utility <- sum(round(adjusted_utility, 0))
  return(adjusted_utility)
}

utility_data$adjusted_utility_flow <- apply(utility_data, 1, function(row){
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

#compute effect
utility_data$decrease_flow <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_flow"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

mean.flow <- mean(utility_data$decrease_flow)
median.flow <- median(utility_data$decrease_flow)

##### Step 7: Account for # Temporal Validity (adjustment 2 in Sturman table 1). #####

stab <- seq(.5, 1, by = .01) #stability of performance
utility_data$performance_stability <- sample(stab, nrow(utility_data), replace = TRUE)

utility_data$adjusted_utility_temp <- apply(utility_data, 1, function(row){
  t <- as.numeric(row["t"])
  n <- as.numeric(row["n"])
  select <- as.numeric(row["select"])
  r1 <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  unadjusted_cost <- as.numeric(row["unadjusted_cost"])
  stab <- as.numeric(row["performance_stability"])
  return(as.numeric((t * n * stab * ux(select) * r1 * sdy) - unadjusted_cost))
})

#compute effect
utility_data$decrease_temp <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_temp"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

mean.temp <- mean(utility_data$decrease_temp)
median.temp <- median(utility_data$decrease_temp)

#economic variables + multiple devices

utility_data$adjusted_utility_econ_mult <- apply(utility_data, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  r1 <- as.numeric(row["incremental_r"])
  x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  uxs = ux(sr)
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

#compute effect
utility_data$decrease_econmult <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over1 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_mult"])/abs(row["adjusted_utility_econ"])
  return(as.numeric(100*change))
})

mean.econmult <- mean(utility_data$decrease_econmult)
median.econmult <- median(utility_data$decrease_econmult)
mean.over1 <- mean(utility_data$decrease_over1)
median.over1 <- median(utility_data$decrease_over1)

#economic variables + temporal validity

utility_data$adjusted_utility_econ_tv <- apply(utility_data, 1, function(row) {
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
  stab = as.numeric(row["performance_stability"])
  uxs = ux(sr)*stab
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

#compute effect
utility_data$decrease_econtv <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_tv"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over1.2 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_tv"])/abs(row["adjusted_utility_econ"])
  return(as.numeric(100*change))
})

mean.econtv <- mean(utility_data$decrease_econtv)
median.econtv <- median(utility_data$decrease_econtv)
mean.over1.2 <- mean(utility_data$decrease_over1.2)
median.over1.2 <- median(utility_data$decrease_over1.2)

#economic variables + top down hiring

utility.econtd <- function(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc){
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  x2 <- (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  return ((n*x2*r*z*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
}

utility_data$adjusted_utility_econ_td <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  return(utility.econtd(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc))
})

#compute effect
utility_data$decrease_econ_td <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_td"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over1.3 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_td"])/abs(row["adjusted_utility_econ"])
  return(as.numeric(100*change))
})

mean.econtd <- mean(utility_data$decrease_econ_td)
median.econtd <- median(utility_data$decrease_econ_td)
mean.over1.3 <- mean(utility_data$decrease_over1.3)
median.over1.3 <- median(utility_data$decrease_over1.3)

#economic variables + probation

utility.econ.probation <- function(n, t, r, sr, sdy, rc, cost, i, vc, tax) {
  xc <- qnorm(1-sr)
  PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
  PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
  PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
  mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  x1 <- 1/(1+i)
  x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
  return((n*x1*sdy*r*ux(sr)*(1-tax)*(1-vc)+x2*n*sp*sdy*mur.xcrc*(1-vc)*(1-tax)-(n/sr)*cost*(1-tax))-(x2*n*sdy*so*mur.rc*(1-vc)*(1-tax)))
}

utility_data$adjusted_utility_econ_prob <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  return(as.numeric(utility.econ.probation(n,t,r,sr,sdy,rc,cost, i, vc, tax)))
})

#compute effect
utility_data$decrease_econ_prob <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_prob"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over1.4 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_prob"])/abs(row["adjusted_utility_econ"])
  return(as.numeric(100*change))
})

mean.econprob <- mean(utility_data$decrease_econ_prob)
median.econprob <- median(utility_data$decrease_econ_prob)
mean.over1.4 <- mean(utility_data$decrease_over1.4)
median.over1.4 <- median(utility_data$decrease_over1.4)

#economic variables + employee flows

utility.econ.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax){
  num_years <- seq(1, t)
  n_added <- c(rep(n, 1), rep(0, t - 1))
  n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
  n_cum <- cumsum(n_added - n_turn)
  zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
  discount_factor <- 1/(1 + i)^num_years
  
  #Financially Adjusted Costs of Program in period (year)
  c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
  unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
  
  adjusted_utility <- unadjusted_utility - c_adj
  adjusted_utility <- sum(round(adjusted_utility, 0))
  return(adjusted_utility)
}

utility_data$adjusted_utility_econ_flow <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r1"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  return(as.numeric(utility.econ.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax)))
})

#compute effect
utility_data$decrease_econ_flow <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_flow"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over1.5 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_flow"])/abs(row["adjusted_utility_econ"])
  return(as.numeric(100*change))
})

mean.econflow <- mean(utility_data$decrease_econ_flow)
median.econflow <- median(utility_data$decrease_econ_flow)
mean.over1.5 <- mean(utility_data$decrease_over1.5)
median.over1.5 <- median(utility_data$decrease_over1.5)

#economic variables + multiple devices + temporal validity

utility_data$adjusted_utility_econ_mult_tv <- apply(utility_data, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  r1 <- as.numeric(row["incremental_r"])
  x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  stab = as.numeric(row["performance_stability"])
  uxs = ux(sr)*stab
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

#compute effect
utility_data$decrease_econmulttv <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over2 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_tv"])/abs(row["adjusted_utility_econ_mult"])
  return(as.numeric(100*change))
})

mean.econmulttv <- mean(utility_data$decrease_econmulttv)
median.econmulttv <- median(utility_data$decrease_econmulttv)
mean.over2 <- mean(utility_data$decrease_over2)
median.over2 <- median(utility_data$decrease_over2)

#economic variables + multiple devices + top down

utility_data$adjusted_utility_econ_mult_td <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  return(utility.econtd(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc))
})

#compute effect
utility_data$decrease_econmulttd <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_td"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over2.1 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_td"])/abs(row["adjusted_utility_econ_mult"])
  return(as.numeric(100*change))
})

mean.econmulttd <- mean(utility_data$decrease_econmulttd)
median.econmulttd <- median(utility_data$decrease_econmulttd)
mean.over2.1 <- mean(utility_data$decrease_over2.1)
median.over2.1 <- median(utility_data$decrease_over2.1)

#economic variables + multiple devices + probation

utility_data$adjusted_utility_econ_mult_prob <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  return(as.numeric(utility.econ.probation(n,t,r,sr,sdy,rc,cost, i, vc, tax)))
})

#compute effect
utility_data$decrease_econmultprob <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_prob"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over2.2 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_prob"])/abs(row["adjusted_utility_econ_mult"])
  return(as.numeric(100*change))
})

mean.econmultprob <- mean(utility_data$decrease_econmultprob)
median.econmultprob <- median(utility_data$decrease_econmultprob)
mean.over2.2 <- mean(utility_data$decrease_over2.2)
median.over2.2 <- median(utility_data$decrease_over2.2)

#economic variables + multiple devices + employee flows

utility.econ.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax){
  num_years <- seq(1, t)
  n_added <- c(rep(n, 1), rep(0, t - 1))
  n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
  n_cum <- cumsum(n_added - n_turn)
  zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
  discount_factor <- 1/(1 + i)^num_years
  
  #Financially Adjusted Costs of Program in period (year)
  c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
  unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
  
  adjusted_utility <- unadjusted_utility - c_adj
  adjusted_utility <- sum(round(adjusted_utility, 0))
  return(adjusted_utility)
}

utility_data$adjusted_utility_econ_mult_flow <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  return(as.numeric(utility.econ.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax)))
})

#compute effect
utility_data$decrease_econmultflow <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_flow"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over2.3 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_flow"])/abs(row["adjusted_utility_econ_mult"])
  return(as.numeric(100*change))
})

mean.econmultflow <- mean(utility_data$decrease_econmultflow)
median.econmultflow <- median(utility_data$decrease_econmultflow)
mean.over2.3 <- mean(utility_data$decrease_over2.3)
median.over2.3 <- median(utility_data$decrease_over2.3)

#economic variables + multiple devices + temporal validity + top down

utility.econtdtv <- function(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab){
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  x2 <- (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  return ((n*x2*r*stab*z*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
}

utility_data$adjusted_utility_econ_mult_tv_td <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  stab <- as.numeric(row["performance_stability"])
  return(utility.econtdtv(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab))
})

#compute effect
utility_data$decrease_econmulttvtd <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over3 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_td"])/abs(row["adjusted_utility_econ_mult_tv"])
  return(as.numeric(100*change))
})

mean.econmulttvtd <- mean(utility_data$decrease_econmulttvtd)
median.econmulttvtd <- median(utility_data$decrease_econmulttvtd)
mean.over3 <- mean(utility_data$decrease_over3)
median.over3 <- median(utility_data$decrease_over3)

#economic variables + multiple devices + temporal validity + prob

utility.econ.tv.probation <- function(n, t, r, sr, sdy, rc, cost, i, vc, tax, stab) {
  xc <- qnorm(1-sr)
  PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
  PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
  PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
  mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  x1 <- 1/(1+i)
  x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
  return((n*x1*stab*sdy*r*ux(sr)*(1-tax)*(1-vc)+x2*n*sp*stab*sdy*mur.xcrc*(1-vc)*(1-tax)-(n/sr)*cost*(1-tax))-(x2*n*stab*sdy*so*mur.rc*(1-vc)*(1-tax)))
}

utility_data$adjusted_utility_econ_mult_tv_prob <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  return(as.numeric(utility.econ.tv.probation(n, t, r, sr, sdy, rc, cost, i, vc, tax, stab)))
})

#compute effect
utility_data$decrease_econmulttvprob <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_prob"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over3.1 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_prob"])/abs(row["adjusted_utility_econ_mult_tv"])
  return(as.numeric(100*change))
})

mean.econmulttvprob <- mean(utility_data$decrease_econmulttvprob)
median.econmulttvprob <- median(utility_data$decrease_econmulttvprob)
mean.over3.1 <- mean(utility_data$decrease_over3.1)
median.over3.1 <- median(utility_data$decrease_over3.1)

#economic variables + multiple devices + temporal validity + flows

utility.econ.tv.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab){
  num_years <- seq(1, t)
  n_added <- c(rep(n, 1), rep(0, t - 1))
  n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
  n_cum <- cumsum(n_added - n_turn)
  zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
  discount_factor <- 1/(1 + i)^num_years
  
  #Financially Adjusted Costs of Program in period (year)
  c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
  unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
  
  adjusted_utility <- stab*unadjusted_utility - c_adj
  adjusted_utility <- sum(round(adjusted_utility, 0))
  return(adjusted_utility)
}

utility_data$adjusted_utility_econ_mult_tv_flow <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  return(as.numeric(utility.econ.tv.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab)))
})

#compute effect
utility_data$decrease_econmulttvflow <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_flow"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over3.2 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_flow"])/abs(row["adjusted_utility_econ_mult_tv"])
  return(as.numeric(100*change))
})

mean.econmulttvflow <- mean(utility_data$decrease_econmulttvflow)
median.econmulttvflow <- median(utility_data$decrease_econmulttvflow)
mean.over3.2 <- mean(utility_data$decrease_over3.2)
median.over3.2 <- median(utility_data$decrease_over3.2)

#economic variables + multiple devices + temporal validity + top down + probation

utility.econ.mult.tv.td.prob <- function(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab){
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  xc <- qnorm(1-sr)
  xc.pa <- qnorm(1-pa)
  xc.p2 <- qnorm(1-p2)
  PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
  PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
  PHI1.1.pa <- (xc.pa-r*rc)/sqrt(1-r^2)
  PHI1.2.pa <- (rc-r*xc.pa)/sqrt(1-r^2)
  PHI1.1.p2 <- (xc.p2-r*rc)/sqrt(1-r^2)
  PHI1.2.p2 <- (rc-r*xc.p2)/sqrt(1-r^2)
  PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
  mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
  mur.xcrc.top <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))
  mur.xcrc.top.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))
  mur.xcrc.top.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  z.mod <- (pa*n*mur.xcrc+n*(bxy)* mur.xcrc.top.pa+(n/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr))*(mur.xcrc.top.p2-mur.xcrc.top))/n
  x1 <- 1/(1+i)
  x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
  return((n*x1*stab*sdy*r*z*(1-tax)*(1-vc)+x2*n*sp*sdy*stab*z.mod*(1-tax)*(1-vc)-(n/sr)*cost*(1-tax))-(x2*n*sdy*so*stab*mur.rc*(1-tax)*(1-vc)))
}

utility_data$adjusted_utility_econ_mult_tv_td_prob <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  return(as.numeric(utility.econ.mult.tv.td.prob(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab)))
})

#compute effect
utility_data$decrease_econmulttvtdprob <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td_prob"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over4 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult_tv_td"]-row["adjusted_utility_econ_mult_tv_td_prob"])/abs(row["adjusted_utility_econ_mult_tv_td"])
  return(as.numeric(100*change))
})

mean.econmulttvtdprob <- mean(utility_data$decrease_econmulttvtdprob)
median.econmulttvtdprob <- median(utility_data$decrease_econmulttvtdprob)
mean.over4 <- mean(utility_data$decrease_over4)
median.over4 <- median(utility_data$decrease_over4)

#economic variables + multiple devices + temporal validity + top down + flow

utility.econ.tv.td.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy){
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  num_years <- seq(1, t)
  n_added <- c(rep(n, 1), rep(0, t - 1))
  n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
  n_cum <- cumsum(n_added - n_turn)
  zbarx <- c(rep((pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n, 1), rep(((pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n)+corrto*ux(1-pto), t-1))
  discount_factor <- 1/(1 + i)^num_years
  
  #Financially Adjusted Costs of Program in period (year)
  c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
  unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
  
  adjusted_utility <- stab*unadjusted_utility - c_adj
  adjusted_utility <- sum(round(adjusted_utility, 0))
  return(adjusted_utility)
}

utility_data$adjusted_utility_econ_mult_tv_td_flow <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  return(as.numeric(utility.econ.tv.td.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy)))
})

#compute effect
utility_data$decrease_econmulttvtdflow <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td_flow"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over4.1 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult_tv_td"]-row["adjusted_utility_econ_mult_tv_td_flow"])/abs(row["adjusted_utility_econ_mult_tv_td"])
  return(as.numeric(100*change))
})

mean.econmulttvtdflow <- mean(utility_data$decrease_econmulttvtdflow)
median.econmulttvtdflow <- median(utility_data$decrease_econmulttvtdflow)
mean.over4.1 <- mean(utility_data$decrease_over4.1)
median.over4.1 <- median(utility_data$decrease_over4.1)

# final adjustment

utility.complete <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc){
  p2 <- ((n-(pa*n))/(n/sr))+sr
  if (p2 > 1){
    p2 <- 1
  }
  xc <- qnorm(1-sr)
  xc.pa <- qnorm(1-pa)
  xc.p2 <- qnorm(1-p2)
  xc.pto <- qnorm(pto)
  PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
  PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
  PHI1.1.pa <- (xc.pa-r*rc)/sqrt(1-r^2)
  PHI1.2.pa <- (rc-r*xc.pa)/sqrt(1-r^2)
  PHI1.1.p2 <- (xc.p2-r*rc)/sqrt(1-r^2)
  PHI1.2.p2 <- (rc-r*xc.p2)/sqrt(1-r^2)
  PHI1.1.pto <- (xc.pto-r*rc)/sqrt(1-r^2)
  PHI1.2.pto <- (rc-r*xc.pto)/sqrt(1-r^2)
  PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
  mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
  mur.xcrc.pto <- (dnorm(rc)*pnorm(PHI1.1.pto, lower.tail = FALSE)+r*dnorm(xc.pto)*pnorm(PHI1.2.pto, lower.tail = FALSE))/pmvnorm(c(xc.pto, rc), Inf, corr = PHI2.corr)
  mur.xcrc.top <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))
  mur.xcrc.top.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))
  mur.xcrc.top.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  z.mod <- (pa*n*mur.xcrc+n*(bxy)* mur.xcrc.top.pa+(n/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr))*(mur.xcrc.top.p2-mur.xcrc.top))/n
  x1 <- 1/(1+i)
  #Financially Adjusted Costs of Program in period (year)
  a <- n*x1*stab*sdy*r*z*(1-tax)*(1-vc)
  c <- (n/sr)*cost*(1-tax)
  if (t>1){
    num_years <- seq(1, t-1)
    n_added <- c(rep(n, 1), rep(0, t - 2))
    n_turn <- c(rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep((z.mod+corrto*ux(1-pto))*sp, t-1))
    discount_factor <- 1/(1 + i)^(num_years+1)
    b <- stab*n_cum * discount_factor * zbarx * sdy * (1 - vc) * (1 - tax)
    utility_0 <- stab * n_cum * discount_factor * so * (mur.rc+corrto*ux(1-pto)) * sdy * (1 - vc) * (1 - tax)
  }
  else{
    b <- 0
    utility_0 <- 0
  }
  adjusted_utility <- a+sum(b)-c-sum(utility_0)
  return(adjusted_utility)
}

utility_data$adjusted_utility_final <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  rc <- as.numeric(row["cutoff_score"])
  return(as.numeric(utility.complete(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc)))
})

#compute effect
utility_data$decrease_final <- apply(utility_data, 1, function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility_final"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})

utility_data$decrease_over5 <- apply(utility_data, 1, function(row){
  change <- (row["adjusted_utility_econ_mult_tv_td_prob"]-row["adjusted_utility_final"])/abs(row["adjusted_utility_econ_mult_tv_td_prob"])
  return(as.numeric(100*change))
})

mean.final <- mean(utility_data$decrease_final)
median.final <- median(utility_data$decrease_final)
mean.over5 <- mean(utility_data$decrease_over5)
median.over5 <- median(utility_data$decrease_over5)

# Effect Table
table_2 <- data.frame(
  net_effect_1 = c(paste0(round(-median.econ, 0), "%"), paste0(round(-mean.econ, 0), "%"), paste0(round(-median.mult, 0), "%"), paste0(round(-mean.mult, 0), "%"), paste0(round(-median.temp, 0), "%"), paste0(-round(mean.temp, 0), "%"),  paste0(round(-median.topdown, 0), "%"),
                   paste0(round(-mean.topdown, 0), "%"), paste0(round(-median.probation, 0), "%"), paste0(round(-mean.probation, 0), "%"), paste0(round(-median.flow, 0), "%"),  paste0(round(-mean.flow, 0), "%")),
  net_effect_2a = c("", "", paste0(round(-median.econmult, 0), "%"), paste0(round(-mean.econmult, 0), "%"), paste0(round(-median.econtv, 0), "%"), paste0(round(-mean.econtv, 0), "%"), paste0(round(-median.econtd, 0), "%"), paste0(round(-mean.econtd, 0), "%"), paste0(round(-median.econprob, 0), "%"), paste0(round(-mean.econprob, 0), "%"), paste0(round(-median.econflow, 0), "%"), paste0(round(-mean.econflow, 0), "%")),
  over_prev_2b =c("", "", paste0(round(-median.over1, 0), "%"), paste0(round(-mean.over1, 0), "%"), paste0(round(-median.over1.2, 0), "%"), paste0(round(-mean.over1.2, 0), "%"), paste0(round(-median.over1.3, 0), "%"), paste0(round(-mean.over1.3, 0), "%"), paste0(round(-median.over1.4, 0), "%"), paste0(round(-mean.over1.4, 0), "%"), paste0(round(-median.over1.5, 0), "%"), paste0(round(-mean.over1.5, 0), "%")),
  net_effect_3a = c("", "", "", "", paste0(round(-median.econmulttv, 0), "%"), paste0(round(-mean.econmulttv, 0), "%"), paste0(round(-median.econmulttd, 0), "%"), paste0(round(-mean.econmulttd, 0), "%"), paste0(round(-median.econmultprob, 0), "%"), paste0(round(-mean.econmultprob, 0), "%"), paste0(round(-median.econmultflow, 0), "%"), paste0(round(-mean.econmultflow, 0), "%")),
  over_prev_3b =c("", "", "", "", paste0(round(-median.over2, 0), "%"), paste0(round(-mean.over2, 0), "%"), paste0(round(-median.over2.1, 0), "%"), paste0(round(-mean.over2.1, 0), "%"), paste0(round(-median.over2.2, 0), "%"), paste0(round(-mean.over2.2, 0), "%"), paste0(round(-median.over2.3, 0), "%"), paste0(round(-mean.over2.3, 0), "%")),
  net_effect_4a = c("", "", "", "", "", "", paste0(round(-median.econmulttvtd, 0), "%"), paste0(round(-mean.econmulttvtd, 0), "%"), paste0(round(-median.econmulttvprob, 0), "%"), paste0(round(-mean.econmulttvprob, 0), "%"), paste0(round(-median.econmulttvflow, 0), "%"), paste0(round(-mean.econmulttvflow, 0), "%")),
  over_prev_4b =c("", "", "", "", "", "", paste0(round(-median.over3, 0), "%"), paste0(round(-mean.over3, 0), "%"), paste0(round(-median.over3.1, 0), "%"), paste0(round(-mean.over3.1, 0), "%"), paste0(round(-median.over3.2, 0), "%"), paste0(round(-mean.over3.2, 0), "%")),
  net_effect_5a = c("", "", "", "", "", "", "", "", paste0(round(-median.econmulttvtdprob, 0), "%"), paste0(round(-mean.econmulttvtdprob, 0), "%"), paste0(round(-median.econmulttvtdflow, 0), "%"), paste0(round(-mean.econmulttvtdflow, 0), "%")),
  over_prev_5b =c("", "", "", "", "", "", "", "", paste0(round(-median.over4, 0), "%"), paste0(round(-mean.over4, 0), "%"), paste0(round(-median.over4.1, 0), "%"), paste0(round(-mean.over4.1, 0), "%")),
  net_effect_6a = c("", "", "", "", "", "", "", "", "", "", paste0(round(-median.final, 0), "%"), paste0(round(-mean.final, 0), "%")),
  over_prev_6b =c("", "", "", "", "", "", "", "", "", "", paste0(round(-median.over5, 0), "%"), paste0(round(-mean.over5, 0), "%"))
)
rownames(table_2) <- c("Economic Variables Median", "Economic Variables Mean", "Multiple Devices Median", "Multiple Devices Mean", "Temporal Validity Median", "Temporal Validity Mean", "Deviations from Top Down Hiring Median", "Deviations from Top Down Hiring Mean", "Probationary Period Median", "Probationary Period Mean", "Employee Flows Median", "Employee Flows Mean")
colnames(table_2) <- c("Net Effect1", "Net Effect2a", "Over Previous Modification2b", "Net Effect3a", "Over Previous Modification3b", "Net Effect4a", "Over Previous Modification4b", "Net Effect5a", "Over Previous Modification5b", "Net Effect6a", "Over Previous Modification6b")

print(table_2)

#table 3

latham_whyte <- data.frame(
  n = rep(470, 10000),
  t = rep(18, 10000),
  r1 = rep(.4, 10000),
  sdy = rep(16290, 10000),
  select = rep(.33, 10000),
  cost = rep(304.33, 10000),
  incremental_r = utility_data$incremental_r,
  turnover_probability = utility_data$turnover_probability,
  performance_correlation_turnover = utility_data$performance_correlation_turnover,
  i = utility_data$i,
  tax = utility_data$tax,
  vc = utility_data$vc,
  performance_stability = utility_data$performance_stability,
  initial_accept = utility_data$initial_accept,
  perf_corr = utility_data$perf_corr,
  cutoff_score = utility_data$cutoff_score
)

latham_whyte$step1 <- apply(latham_whyte, 1, function(row){
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
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

latham_whyte$step2 <- apply(latham_whyte, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  r1 <- as.numeric(.4 - row["incremental_r"])
  x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  uxs = ux(sr)
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

latham_whyte$step3 <- apply(latham_whyte, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  r1 <- as.numeric(.4 - row["incremental_r"])
  x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
  stab = as.numeric(row["performance_stability"])
  uxs = ux(sr)*stab
  return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
})

latham_whyte$step4 <- apply(latham_whyte, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(.4 - row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  stab <- as.numeric(row["performance_stability"])
  return(utility.econtdtv(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab))
})

latham_whyte$step5 <- apply(latham_whyte, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(.4 - row["incremental_r"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  rc <- as.numeric(row["cutoff_score"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  return(as.numeric(utility.econ.mult.tv.td.prob(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab)))
})

latham_whyte$step6 <- apply(latham_whyte, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(.4 - row["incremental_r"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  pto <- as.numeric(row["turnover_probability"])
  corrto <- as.numeric(row["performance_correlation_turnover"])
  i <- as.numeric(row["i"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  stab <- as.numeric(row["performance_stability"])
  pa <- as.numeric(row["initial_accept"])
  bxy <- as.numeric(row["perf_corr"])
  rc <- as.numeric(row["cutoff_score"])
  return(as.numeric(utility.complete(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc)))
})
min(latham_whyte)
max(latham_whyte)

bin_edges <- seq(from = round(min(latham_whyte), -6), to = round(max(latham_whyte),-6), by = 1000000)

# Create the bins and count the values in each bin
value_counts1 <- table(cut(latham_whyte$step1, breaks = bin_edges))
value_counts2 <- table(cut(latham_whyte$step2, breaks = bin_edges))
value_counts3 <- table(cut(latham_whyte$step3, breaks = bin_edges))
value_counts4 <- table(cut(latham_whyte$step4, breaks = bin_edges))
value_counts5 <- table(cut(latham_whyte$step5, breaks = bin_edges))
value_counts6 <- table(cut(latham_whyte$step6, breaks = bin_edges))
table_3 <- data.frame(
  UA_Estimate = sprintf("%d to %d", as.integer(sub("\\((.*),(.*)\\]", "\\1", names(value_counts1))),
                        as.integer(sub("\\((.*),(.*)\\]", "\\2", names(value_counts1)))),
  Economic_Factors = as.vector(value_counts1),
  and_Multiple_Selection_Devices = as.vector(value_counts2),
  and_Temporal_Validity = as.vector(value_counts3),
  and_Deviations_from_Top_Down_Hiring = as.vector(value_counts4),
  and_Probation_Period = as.vector(value_counts5),
  and_Employee_Flows = as.vector(value_counts6)
)
table_3$Economic_Factors[table_3$Economic_Factors == 0] <- ""
table_3$and_Multiple_Selection_Devices[table_3$and_Multiple_Selection_Devices == 0] <- ""
table_3$and_Temporal_Validity[table_3$and_Temporal_Validity == 0] <- ""
table_3$and_Deviations_from_Top_Down_Hiring[table_3$and_Deviations_from_Top_Down_Hiring == 0] <- ""
table_3$and_Probation_Period[table_3$and_Probation_Period == 0] <- ""
table_3$and_Employee_Flows[table_3$and_Employee_Flows == 0] <- ""
# Display the data frame
print(table_3)
