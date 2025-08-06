##### Sturman (2000) #####
set.seed(12345)

# load packages
library(scales)
library(ggplot2)
library(GGally)
library(iopsych)

##### Step 1: Build basic utility formula parameters (adjustment 0 in Sturman table 1) #####
# Define the population of parameters to randomly sample from
t = c(1:10)                               # expected tenure of staying in years
n = seq(1, 1100)                          # Create a sequence from 1 to 1100 in increments of 1 for n
app = seq(1, 50)                          # 1-50 applicants per hire
selection_ratio = seq(0.02, 1, by=0.01)  # for example, from 0.01 to 1 in increments of 0.01
sdy = seq(4000, 40000, by=1000)           # standard deviation of job performance value
cost = seq(0, 1100, by=100)               # cost per applicant of operating the new selection program
incremental_r = seq(.05, .38, by= .01)    # incremental operational validity of the new selection program (r1 must be .05 to .38 higher than r2)
r1 = seq(.10, .77, by=.01)                # operational validity of the new selection program
r2 = seq(.05, .38, by=.01)                # operational validity of the old selection program

# Number of samples. Sturman called for 10000
num_samples <- 100

# Initialize an empty data frame to store the results
utility_data <- data.frame(t=numeric(), n=numeric(), app=numeric(), sdy=numeric(), cost=numeric(), r2=numeric(), selection_ratio=numeric(), unadjusted_cost=numeric(), unadjusted_util=numeric())

while (nrow(utility_data) < num_samples) {
  tenure <- sample(t, 1)
  num_hires <- sample(n, 1)
  applicants_per_hire <- sample(app[-1], 1)
  total_applicants = num_hires * applicants_per_hire
  sd <- sample(sdy, 1)
  c <- sample(cost, 1)
  incremental_r_value = sample(r2, 1)
  
  # Compute the unadjusted cost and utility for this combination of parameters
  unadjusted_cost = num_hires * c / applicants_per_hire
  unadjusted_util = (tenure * num_hires * iopsych::ux(1/applicants_per_hire) * incremental_r_value * sd) - unadjusted_cost
  print(num_hires)
  
  # Append the results to the data frame
  utility_data <- rbind(utility_data, data.frame(t=tenure, n=num_hires, app=applicants_per_hire, total_applicants=total_applicants, sdy=sd, cost=c, selection_ratio=1/applicants_per_hire, incremental_r=incremental_r_value, unadjusted_cost=unadjusted_cost, unadjusted_utility=unadjusted_util))
}

# determine if a variable is normally distributed using the Chi Square Goodness-of-Fit test and histograms

# Expected probabilities for a uniform distribution
expected_prob <- 1/10
expected_probs <- rep(expected_prob, 10)
expected_probs

# Perform Chi-Square goodness-of-fit test
chi_square_test <- chisq.test(table(utility_data$t), p = expected_probs)

# Print test results
print(chi_square_test)

# determine if a variable is normally distributed using the KS test and histograms
ks.test(utility_data$t, "punif", min(utility_data$t), max(utility_data$t)) # not uniform for some reason
hist(utility_data$t)
ks.test(utility_data$n, "punif", min(utility_data$n), max(utility_data$n)) # not uniform for some reason
hist(utility_data$n)
ks.test(utility_data$app, "punif", min(utility_data$app), max(utility_data$app)) # not uniform for some reason
hist(utility_data$app)
ks.test(utility_data$sdy, "punif", min(utility_data$sdy), max(utility_data$sdy)) # not uniform for some reason
hist(utility_data$sdy)
ks.test(utility_data$cost, "punif", min(utility_data$cost), max(utility_data$cost)) # not uniform for some reason
hist(utility_data$cost)
ks.test(utility_data$r1, "punif", min(utility_data$r1), max(utility_data$r1)) # not uniform for some reason
hist(utility_data$r1)
ks.test(utility_data$r2, "punif", min(utility_data$r2), max(utility_data$r2)) # not uniform for some reason
hist(utility_data$r2)
ks.test(utility_data$selection_ratio, "punif", min(utility_data$selection_ratio), max(utility_data$selection_ratio)) # not uniform for some reason
hist(utility_data$selection_ratio)

# display a pairplot of all four columns of data and the correlation matrix that emerges. Some parameters are correlated. 
GGally::ggpairs(utility_data)

# Plot 1. Unadjusted utility estimates 

# Create new variable for fill based on whether x is negative or positive
utility_data$Fill <- ifelse(utility_data$unadjusted_utility < 0, "Negative", "Positive")

# Add in density
density_df <- data.frame(Utility_Total = density(utility_data$unadjusted_utility)$x,
                         Density = density(utility_data$unadjusted_utility)$y)

# Create new variable for fill based on whether x is negative or positive
density_df$Fill <- ifelse(density_df$Utility_Total < 0, "Negative", "Positive")

# Create a function for formatting axis labels
dollar_format2 <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", round(x/1e6, 1), "M"), 
         ifelse(abs(x) >= 1e3, paste0("$", round(x/1e3, 1), "K"), 
                paste0("$", round(x, 1))))
}

# Calculate percentage of estimates that are negative
pct_negative <- round(sum(density_df$Utility_Total < 0) / length(density_df$Utility_Total) * 100, 2)

# Calculate average utility estimate
avg_utility_estimate <- round(median(density_df$Utility_Total), 2)

# get the positive value
positive_utility_df <- density_df[density_df$Utility_Total > 0,]
avg_positive_utility <- median(positive_utility_df$Utility_Total)
dollar(avg_positive_utility)

# build plot
ggplot(density_df, aes(x = Utility_Total, y = Density)) + 
  geom_area(aes(fill = Fill), alpha=0.4) +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "green")) +
  geom_line(color="black") +
  scale_x_continuous(labels = dollar_format2) +
  labs(title = "Fig 1.", 
       subtitle = paste0(pct_negative, "% of financially adjusted utility estimates are negative."),
       x = "Adjusted Utility Total", 
       y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, color = "black")) +
  annotate("text", x = quantile(density_df$Utility_Total, 0.7), y = max(density_df$Density)*0.85, 
           label = paste("Average estimate:\n", dollar_format2(avg_utility_estimate)), hjust = 0) +
  annotate("text", x = quantile(density_df$Utility_Total, 0.7), y = max(density_df$Density)*0.75, 
           label = paste("Average of + estimates:\n", dollar_format2(avg_positive_utility)), hjust = 0) +
  guides(fill=FALSE)  # Remove legend

##### Step 2: Account for # Economic adjustments (adjustment 1 in Sturman table 1).  #####
# Variables for adjustment
i = seq(.01, .11, by=.01)          # discount rate
tax = seq(.30, .63, by=.01)        # marginal tax rate
vc = seq(.02,.35, by=.01)          # variable costs

# Place these values into the dataset that we built. Sample random values for each row in the dataset
utility_data$i <- sample(i, nrow(utility_data), replace = TRUE)
utility_data$tax <- sample(tax, nrow(utility_data), replace = TRUE)
utility_data$vc <- sample(vc, nrow(utility_data), replace = TRUE)

# View the first few rows of the updated dataset
head(utility_data)

# build adjustment formula
adjusted_utility_cohort <- function(n, x2, incremental_r, uxs, sdy, vc, tax, sr, cost, i, t) {
  return ((x2*x2*incremental_r*uxs*sdy*(1+vc)*(1-tax))-((n/sr)*cost*(1-tax)*(1/(1+i)^(t-1))))
}

# compute adjusted utility for each row
utility_data$adjusted_utility <- apply(utility_data, 1, function(row) {
  n <- as.numeric(row["n"])
  sdy <- as.numeric(row["sdy"])
  sr <- as.numeric(row["selection_ratio"])
  cost <- as.numeric(row["unadjusted_cost"])
  i <- as.numeric(row["i"])
  t <- as.numeric(row["t"])
  tax <- as.numeric(row["tax"])
  vc <- as.numeric(row["vc"])
  r1 <- as.numeric(row["r1"])
  r2 <- as.numeric(row["r2"])
  
  x2 = 1/(1+i)^t
  incremental_r = r1-r2
  uxs = n/sr
  
  return(adjusted_utility_cohort(n, x2, incremental_r, uxs, sdy, vc, tax, sr, cost, i, t))
})

# View the first few rows of the updated dataset
head(utility_data)

# Plot 2.
# Plot 1. Unadjusted utility estimates 

# Create new variable for fill based on whether x is negative or positive
utility_data$Fill <- ifelse(utility_data$adjusted < 0, "Negative", "Positive")

# Add in density
density_df <- data.frame(Utility_Total = density(utility_data$adjusted)$x,
                         Density = density(utility_data$adjusted)$y)

# Create new variable for fill based on whether x is negative or positive
density_df$Fill <- ifelse(density_df$Utility_Total < 0, "Negative", "Positive")

# Calculate percentage of estimates that are negative
pct_negative <- round(sum(density_df$Utility_Total < 0) / length(density_df$Utility_Total) * 100, 2)

# Calculate average utility estimate
avg_utility_estimate <- round(median(density_df$Utility_Total), 2)

# get the positive value
positive_utility_df <- density_df[density_df$Utility_Total > 0,]
avg_positive_utility <- median(positive_utility_df$Utility_Total)
dollar(avg_positive_utility)

# add credibility interval. Calculate the 10th and 90th percentiles
cumulative_density <- cumsum(density_df$Density) * diff(density_df$Utility_Total)[1]
lower_bound <- density_df$Utility_Total[which.max(cumulative_density >= 0.10)]
upper_bound <- density_df$Utility_Total[which.max(cumulative_density >= 0.90)]

# build plot
ggplot(density_df, aes(x = Utility_Total, y = Density)) + 
  geom_area(aes(fill = Fill), alpha=0.4) +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "green")) +
  geom_line(color="black") +
  geom_vline(aes(xintercept = lower_bound), linetype="dashed", color="blue") +  # Lower bound of 80% CI
  geom_vline(aes(xintercept = upper_bound), linetype="dashed", color="blue") +  # Upper bound of 80% CI
  scale_x_continuous(labels = dollar_format2) +
  labs(title = "Fig 2.", 
       subtitle = paste0(pct_negative, "% of financially adjusted utility estimates are negative."),
       x = "Adjusted Utility Total", 
       y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, color = "black")) +
  annotate("text", x = quantile(density_df$Utility_Total, 0.7), y = max(density_df$Density)*0.85, 
           label = paste("Average estimate:\n", dollar_format2(avg_utility_estimate)), hjust = 0) +
  annotate("text", x = quantile(density_df$Utility_Total, 0.7), y = max(density_df$Density)*0.75, 
           label = paste("Average of + estimates:\n", dollar_format2(avg_positive_utility)), hjust = 0) +
  guides(fill=FALSE)  # Remove legend
