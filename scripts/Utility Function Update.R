set.seed(12345)

library(scales)
library(ggplot2)

# Parameters for Latham & White
t = c(1:10)           # expected tenure of staying in years
n = 470               # number selected
sdy = 16290           # standard deviation of job performance value
r1 = 0.40             # operational validity of the new selection program
r2 = 0                # operational validity of the old program
incremental_r = r1-r2 # incremental validity
sr = .33              # selection ratio
uxs = 1.09            # standard score on the selection program composite
c1 = 304.33           # cost per applicant of operating the new selection program
c2 = 0                # cost per applicant of operating the old selection program
c_diff = c1-c2        # added cost or cost savings from switching
vc = -0.35            # variable costs
tax = 0.63            # marginal tax rate
i = 0.11              # discount rate

# cost of operating the selection program
unadjusted_costs <- n*c_diff/sr

# unadjusted utility  
unadjusted_utility <- t*n*uxs*(r1-r2)*sdy-unadjusted_costs 

# unadjusted utility per selectee
uu_selectee <- unadjusted_utility/n

# unadjusted utility per selectee per year they stay
uu_selectee_ry <- unadjusted_utility/n/t

# break even value of SDy needed to recover costs (leave for Luis)

# Variables for adjustment
x1 = n               # cumulative number of total workers hired
x2 = 1/(1+i)^t       # discount factor
incremental_r=r1-r2  # procedure incremental validity
uxs = uxs            # standard score on the selection procedure
sdy = sdy            # SDy
vc = vc              # additional variable costs
tax = tax            # tax rate adjustment
n = n                # new hires
sr = sr              # selection rate
c_diff = c1 - c2     # cost difference between two procedures

# Function to calculate adjusted utility for a single cohort
adjusted_utility_cohort <- function(x1, x2, incremental_r, uxs, sdy, vc, tax, n, sr, c_diff, i, t) {
  return ((x1*x2*incremental_r*uxs*sdy*(1+vc)*(1-tax))-((n/sr)*c_diff*(1-tax)*(1/(1+i)^(t-1))))
}

# Calculate adjusted utility for each year of the program's operation
adjusted_utility_total = 0
for (year in 1:t) {
  adjusted_utility_total = adjusted_utility_total + adjusted_utility_cohort(n, x2, incremental_r, uxs, sdy, vc, vc, tax, n, c_diff, i, year)
}

# output
dollar(mean(unadjusted_utility))
dollar(uu_selectee)
dollar(uu_selectee_ry)
dollar(mean(adjusted_utility_total))

# factor in the spread of potential operational validity coefficients of switching from structured to unstructured interviews
# build distributions
# Parameters
mean1 <- 0.42 # stuctured interview operational validity
sd1 <- 0.19
mean2 <- 0.19 # unstructured interview operational validity
sd2 <- 0.16

# Mean and standard deviation of the difference
mean_diff <- mean1 - mean2
sd_diff <- round(sqrt(sd1^2 + sd2^2),2)

# Create a sequence of x values
x <- seq(-1, 1, length.out = 1000)

# Create data frames for each distribution
df1 <- data.frame(x = x, y = dnorm(x, mean1, sd1), type = 'Structured Interviews')
df2 <- data.frame(x = x, y = dnorm(x, mean2, sd2), type = 'Unstructured Interviews')
df_diff <- data.frame(x = x, y = dnorm(x, mean_diff, sd_diff), type = 'Difference')

# convert df_diff to useful metric of 10,000 cases
incremental_r <- rnorm(10000, mean_diff, sd_diff)

# unadjusted utility  
unadjusted_utility <- t*n*uxs*(incremental_r)*sdy-unadjusted_costs 

# Calculate adjusted utility for each year of the program's operation
adjusted_utility_total = 0
for (year in 1:t) {
  adjusted_utility_total = adjusted_utility_total + adjusted_utility_cohort(n, x2, incremental_r, uxs, sdy, vc, vc, tax, n, c_diff, i, year)
}

# Create a data frame for the plot
adjusted_utility_df <- data.frame(Adjusted_Utility_Total = adjusted_utility_total)

# Calculate density
density_df <- data.frame(Adjusted_Utility_Total = density(adjusted_utility_df$Adjusted_Utility_Total)$x,
                         Density = density(adjusted_utility_df$Adjusted_Utility_Total)$y)

# Create new variable for fill based on whether x is negative or positive
density_df$Fill <- ifelse(density_df$Adjusted_Utility_Total < 0, "Negative", "Positive")

# Create a function for formatting axis labels
dollar_format2 <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", round(x/1e6, 1), "M"), 
         ifelse(abs(x) >= 1e3, paste0("$", round(x/1e3, 1), "K"), 
                paste0("$", round(x, 1))))
}

# Calculate percentage of estimates that are negative
pct_negative <- round(sum(density_df$Adjusted_Utility_Total < 0) / length(density_df$Adjusted_Utility_Total) * 100, 2)

# Calculate average utility estimate
avg_utility_estimate <- round(mean(density_df$Adjusted_Utility_Total), 2)

# get the positive value
positive_utility_df <- density_df[density_df$Adjusted_Utility_Total > 0,]
avg_positive_utility <- mean(positive_utility_df$Adjusted_Utility_Total)
dollar(avg_positive_utility)

# build plot
ggplot(density_df, aes(x = Adjusted_Utility_Total, y = Density)) + 
  geom_area(aes(fill = Fill), alpha=0.4) +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "green")) +
  geom_line(color="black") +
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
  annotate("text", x = quantile(density_df$Adjusted_Utility_Total, 0.7), y = max(density_df$Density)*0.85, 
           label = paste("Average estimate:\n", dollar_format2(avg_utility_estimate)), hjust = 0) +
  annotate("text", x = quantile(density_df$Adjusted_Utility_Total, 0.7), y = max(density_df$Density)*0.75, 
           label = paste("Average of + estimates:\n", dollar_format2(avg_positive_utility)), hjust = 0) +
  guides(fill=FALSE)  # Remove legend


# Calculate adjusted utility for each year of the program's operation and record the parameter values
adjusted_utility_df <- data.frame()
for (year in 1:t) {
  for(i in 1:length(incremental_r)){
    adjusted_utility <- adjusted_utility_cohort(n, x2, incremental_r[i], uxs, sdy, vc, vc, tax, n, c_diff, i, year)
    adjusted_utility_df <- rbind(adjusted_utility_df, data.frame(Year = year, Adjusted_Utility = adjusted_utility, N = n, X2 = x2, Incremental_r = incremental_r[i], Uxs = uxs, Sdy = sdy, Vc = vc, Tax = tax, C_diff = c_diff, I = i))
  }
}

# use machine learning to identify predictors of negative utility.
library(randomForest)

# Create a binary response variable: is the adjusted utility negative?
adjusted_utility_df$Negative_Utility <- ifelse(adjusted_utility_df$Adjusted_Utility < 0, 1, 0)

# Run the random forest model
rf_model <- randomForest(Negative_Utility ~ . - Adjusted_Utility, data = adjusted_utility_df, importance = TRUE)

# Display the importance of each variable
importance(rf_model)

#### FIND MINIMUM INCREMENTAL R TO YIELD POSITIVE RESULT

# Initialize an empty vector to store incremental_r values and corresponding utility
results <- data.frame(Incremental_r = numeric(), Utility = numeric())

# Loop over incremental_r values
for (incremental_r in seq(-1, 1, by = 0.01)) {
  
  # Calculate adjusted utility total for current incremental_r
  adjusted_utility_total = 0
  for (year in 1:t) {
    adjusted_utility_total = adjusted_utility_total + adjusted_utility_cohort(n, x2, incremental_r, uxs, sdy, vc, vc, tax, n, c_diff, i, year)
  }
  
  # Add the current incremental_r and its corresponding utility to the results
  results <- rbind(results, data.frame(Incremental_r = incremental_r, Utility = adjusted_utility_total))
}

# Find the minimum incremental_r that yields a positive utility
min_inc_r_positive_utility <- min(results$Incremental_r[results$Utility > 0])

min_inc_r_positive_utility
