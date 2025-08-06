#### Employee Flows ####
library(iopsych)

# Variables needed: number hired, tenure, selection ratio, cost increase, marginal tax, WACC, incremental r, ux, variable costs                        # 1-50 applicants per hire
select = 0.25    # for example, from 0.01 to 1 in increments of 0.01
sdy = 6000        # standard deviation of job performance value
cost = 100             # cost per applicant of operating the new selection program
incremental_r = 0.2    # incremental operational validity of the new selection program (r1 must be .05 to .38 higher than r2)
initial_cohorts <- 5   # Number of initial cohorts
n <- 100               # Number of employees per cohort
years <- 5             # Number of years for initial cohorts
total_years <- 30      # Total number of years including future projection
t <- 3                 # Expected employee tenure in years
discount_rate <- 0.10  # Discount rate for calculating present value
tax <- 0.45            # Marginal tax rate
v <- -0.05             # variable costs

num_years <- seq(1, total_years)
n_added <- c(rep(n, years), rep(0, total_years - years))
n_turn <- c(rep(0, t), rep(n, years), rep(0, total_years - years - t))
n_cum <- cumsum(n_added - n_turn)
discount_factor <- 1/(1 + discount_rate)^num_years

#Financially Adjusted Costs of Program in period (year)
c_adj <- (n_added / 0.25) * cost * (1 - tax) * (1/(1 + discount_rate)^(num_years - 1))
unadjusted_utility <- n_cum * discount_factor * incremental_r * iopsych::ux(select) * sdy * (1 + v) * (1 - tax)

adjusted_utility <- unadjusted_utility - c_adj

