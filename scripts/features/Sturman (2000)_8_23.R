##### Sturman (2000) #####
set.seed(12845)

# load packages
library(scales)
library(ggplot2)
library(iopsych)

##### Step 1: Build basic utility formula parameters (adjustment 0 in Sturman table 1) #####
# Define the population of parameters to randomly sample from
t = c(1:10)                               # expected tenure of staying in years
n = seq(1, 1100)                          # Create a sequence from 1 to 1100 in increments of 1 for n                          # 1-50 applicants per hire
select = seq(0.02, 1, by=0.0075)          # for example, from 0.01 to 1 in increments of 0.01
sdy = seq(4000, 40000, by=200)            # standard deviation of job performance value
cost = seq(0, 1100, by=10)                # cost per applicant of operating the new selection program
incremental_r = seq(.05, .38, by= .01)    # incremental operational validity of the new selection program (r1 must be .05 to .38 higher than r2)
r1 = seq(.10, .77, by=.01)                # operational validity of the new selection program
r2 = seq(.05, .38, by=.01)                # operational validity of the old selection program

# Number of samples. Sturman called for 10000
num_samples <- 10000

# Generate random samples for all parameters
random_samples <- data.frame(
  tenure = sample(t, num_samples, replace = TRUE),
  num_hires = sample(n, num_samples, replace = TRUE),
  selection_ratio = sample(select, num_samples, replace = TRUE),
  sd = sample(sdy, num_samples, replace = TRUE),
  c = sample(cost, num_samples, replace = TRUE),
  incremental_r_value = sample(incremental_r, num_samples, replace = TRUE)
)

# Initialize an empty data frame to store the results
utility_data <- data.frame(
  t = numeric(),
  n = numeric(),
  select = numeric(),
  sdy = numeric(),
  cost = numeric(),
  incremental_r = numeric(),
  unadjusted_cost=numeric(), 
  unadjusted_util=numeric()
)

# Loop through random samples
for (i in 1:num_samples) {
  tenure <- random_samples$tenure[i]
  num_hires <- random_samples$num_hires[i]
  selection_ratio <- random_samples$selection_ratio[i]
  sd <- random_samples$sd[i]
  c <- random_samples$c[i]
  incremental_r_value <- random_samples$incremental_r_value[i]
  
  # Compute the unadjusted cost and utility for this combination of parameters
  unadjusted_cost <- round(num_hires * c / selection_ratio, 0)
  unadjusted_utility <- (tenure * num_hires * iopsych::ux(selection_ratio) * incremental_r_value * sd) - unadjusted_cost
  
  # Append the results to the data frame
  utility_data <- rbind(
    utility_data, 
    data.frame(
      t = tenure,
      n = num_hires,
      sdy = sd,
      cost = c,
      select = selection_ratio,
      incremental_r = incremental_r_value,
      unadjusted_cost = unadjusted_cost,
      unadjusted_utility = unadjusted_utility
    )
  )
}


# Determine if a variable is normally distributed using the Chi Square Goodness-of-Fit test

# Perform Chi-Square goodness-of-fit tests
# Tenure
max(utility_data$t)
min(utility_data$t)
expected_prob.t <- 1/(length(t))
expected_probs.t <- rep(expected_prob.t, length(unique(utility_data$t)))
chi_square_test.t <- chisq.test(table(utility_data$t), p = expected_probs.t)
print(chi_square_test.t)
hist(utility_data$t, main = "Histogram of t Values", xlab = "t Values", ylab = "Frequency")

# N selected (need to revise)
max(utility_data$n)
min(utility_data$n)
expected_prob.n <- 1/length(n)
expected_probs.n <- rep(expected_prob.n, length(unique(utility_data$n)))
#chi_square_test.n <- chisq.test(table(utility_data$n), p = expected_probs.n)
#print(chi_square_test.n)
hist(utility_data$n, main = "Histogram of n Values", xlab = "n Values", ylab = "Frequency")

# sdy
max(utility_data$sdy)
min(utility_data$sdy)
expected_prob.sdy <- 1/length(sdy)
expected_probs.sdy <- rep(expected_prob.sdy, length(unique(utility_data$sdy)))
chi_square_test.sdy <- chisq.test(table(utility_data$sdy), p = expected_probs.sdy)
print(chi_square_test.sdy)
hist(utility_data$sdy, main = "Histogram of sdy Values", xlab = "sdy Values", ylab = "Frequency")

# cost
max(utility_data$cost)
min(utility_data$cost)
expected_prob.cost <- 1/length(cost)
expected_probs.cost <- rep(expected_prob.cost, length(unique(utility_data$cost)))
chi_square_test.cost <- chisq.test(table(utility_data$cost), p = expected_probs.cost)
print(chi_square_test.cost)
hist(utility_data$cost, main = "Histogram of cost Values", xlab = "cost Values", ylab = "Frequency")

# selection rate
max(utility_data$select) # ideally is 1 but this is close enough
min(utility_data$select)
expected_prob.select <- 1/length(select)
expected_probs.select <- rep(expected_prob.select, length(unique(utility_data$select)))
chi_square_test.select <- chisq.test(table(utility_data$select), p = expected_probs.select)
print(chi_square_test.select)
hist(utility_data$select, main = "Histogram of select Values", xlab = "select Values", ylab = "Frequency")

# incremental r
max(utility_data$incremental_r)
min(utility_data$incremental_r)
expected_prob.incremental_r <- 1/length(incremental_r)
expected_probs.incremental_r <- rep(expected_prob.incremental_r, length(unique(utility_data$incremental_r)))
chi_square_test.incremental_r <- chisq.test(table(utility_data$incremental_r), p = expected_probs.incremental_r)
print(chi_square_test.incremental_r)
hist(utility_data$incremental_r, main = "Histogram of incremental_r Values", xlab = "incremental_r Values", ylab = "Frequency")

##### Step 2: Account for # Economic adjustments (adjustment 1 in Sturman table 1).  #####
# Variables for adjustment
i = seq(.01, .11, by=.01)          # discount rate
tax = seq(.30, .63, by=.01)        # marginal tax rate
vc = seq(.02,.35, by=.01)          # variable costs

# Place these values into the dataset that we built. Sample random values for each row in the dataset
utility_data$i <- sample(i, nrow(utility_data), replace = TRUE)
utility_data$tax <- sample(tax, nrow(utility_data), replace = TRUE)
utility_data$vc <- sample(vc, nrow(utility_data), replace = TRUE)

# Determine if a variable is normally distributed using the Chi Square Goodness-of-Fit test

# interest
max(utility_data$i)
min(utility_data$i)
expected_prob.i <- 1/length(i)
expected_probs.i <- rep(expected_prob.i, length(unique(utility_data$i)))
chi_square_test.i <- chisq.test(table(utility_data$i), p = expected_probs.i)
print(chi_square_test.i)
hist(utility_data$i, main = "Histogram of i Values", xlab = "i Values", ylab = "Frequency")

# tax
max(utility_data$tax)
min(utility_data$tax)
expected_prob.tax <- 1/length(tax)
expected_probs.tax <- rep(expected_prob.tax, length(unique(utility_data$tax)))
chi_square_test.tax <- chisq.test(table(utility_data$tax), p = expected_probs.tax)
print(chi_square_test.tax)
hist(utility_data$tax, main = "Histogram of tax Values", xlab = "tax Values", ylab = "Frequency")

# variable costs
max(utility_data$vc)
min(utility_data$vc)
expected_prob.vc <- 1/length(vc)
expected_probs.vc <- rep(expected_prob.vc, length(unique(utility_data$vc)))
chi_square_test.vc <- chisq.test(table(utility_data$vc), p = expected_probs.vc)
print(chi_square_test.vc)
hist(utility_data$vc, main = "Histogram of vc Values", xlab = "vc Values", ylab = "Frequency")

# build adjustment formula for each cohort
adjusted_utility_cohort <- function(n, x2, incremental_r, uxs, sdy, vc, tax, sr, cost, i, t) {
  return ((x2*x2*incremental_r*uxs*sdy*(1+vc)*(1-tax))-((n/sr)*cost*(1-tax)*(1/(1+i)^(t-1))))
}

# compute adjusted utility for each row
utility_data$adjusted_utility <- apply(utility_data, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["select"])
  cost <- as.numeric(row["cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  incremental_r <- as.numeric(row["incremental_r"])
  x2 = 1/(1+i)^t
  uxs = iopsych::ux(sr)
  return(as.numeric(adjusted_utility_cohort(n, x2, incremental_r, uxs, sdy, vc, tax, sr, cost, i, t)))
})

utility_data$percent_decrease <- apply(utility_data,1,function(row){
  change <- (row["unadjusted_utility"]-row["adjusted_utility"])/abs(row["unadjusted_utility"])
  return(as.numeric(100*change))
})
# View the first few rows of the updated dataset
head(utility_data)
