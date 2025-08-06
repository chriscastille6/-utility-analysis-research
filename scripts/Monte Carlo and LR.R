set.seed(12845)

# load packages
library(scales)
library(ggplot2)
library(iopsych)
library(mvtnorm)


# Define the population of parameters to randomly sample from
t = c(1:3)                               # expected tenure of staying in years
n = seq(150, 200, by = 1)       # Create a sequence from 1 to 1100 in increments of 1 for n         # 1-50 applicants per hire
select = seq(2, 10, by = .01)          # for example, from 0.01 to 1 in increments of 0.01
sdy = seq(12800, 35200, by = 200)            # standard deviation of job performance value
cost = rep(600, 10000)# cost per applicant of operating the new selection program
r = rep(.2, 10000)                # operational validity of the old selection program
# Number of samples. Sturman called for 10000
num_samples <- 10000
n = sample(n, num_samples, replace = TRUE)
select = sample(select, num_samples, replace = TRUE)
# Generate random samples for all parameters
utility_data <- data.frame(
  t = sample(t, num_samples, replace = TRUE),
  n = n,
  select = 1/select,
  sdy = sample(sdy, num_samples, replace = TRUE),
  cost = cost,
  r = r
)
i = seq(.08, .11, by=.005)          # discount rate
tax = seq(.21, .28, by=.0025)        # marginal tax rate
vc = seq(.02,.35, by=.01)          # variable costs

# Place these values into the dataset that we built. Sample random values for each row in the dataset
utility_data$i <- sample(i, nrow(utility_data), replace = TRUE)
utility_data$tax <- sample(tax, nrow(utility_data), replace = TRUE)
utility_data$vc <- sample(vc, nrow(utility_data), replace = TRUE)
pa <- seq(.2, .7, by = .01) # initial acceptance rate
bxy <- seq(-.5, 0, by = .01) # correlation between acceptance and performance
utility_data$initial_accept <- sample(pa, nrow(utility_data), replace = TRUE)
utility_data$perf_corr <- sample(bxy, nrow(utility_data), replace = TRUE)
rc <- seq(-2, 0, by = .01) # cutoff score
utility_data$cutoff_score <- sample(rc, nrow(utility_data), replace = TRUE)
pto <- seq(0, .33, by = .01) # turnover probability
corrto <- seq(0, .5, by = .01) # turnover performance correlation
utility_data$turnover_probability <- sample(pto, nrow(utility_data), replace = TRUE)
utility_data$performance_correlation_turnover <- sample(corrto, nrow(utility_data), replace = TRUE)
utility_data$py <- rep(5, 10000)
stab <- runif(10000 ,.5 ,1) #stability of performance
utility_data$performance_stability <- stab


#utility function
utility.complete <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc, py){
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
  mur.xcrc.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))/pmvnorm(c(xc.pa, rc), Inf, corr = PHI2.corr)
  mur.xcrc.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))/pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)
  sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
  sp.p2 <- pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)/pnorm(xc.p2, lower.tail = FALSE)
  so <- pnorm(rc, lower.tail = FALSE)
  mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
  z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
  z.mod <- sp*(pa*mur.xcrc+pa*bxy*ux(pa)*r)+sp.p2*((p2/sr)*mur.xcrc.p2-mur.xcrc)
  x1 <- 1/(1+i)
  #Financially Adjusted Costs of Program in period (year)
  a <- n*x1*stab*sdy*r*z*(1-tax)*(1-vc)
  c <- (n/sr)*cost*(1-tax)
  if (t>1){
    num_years <- seq(1, t-1)
    n_added <- c(rep(n, 1), rep(0, t - 2))
    n_turn <- c(rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep((z.mod+corrto*ux(1-pto)*sp), t-1))
    discount_factor <- 1/(1 + i)^(num_years+1)
    b <- stab*n_cum * discount_factor * zbarx * sdy * (1 - vc) * (1 - tax)
    utility_0 <- stab * n_cum * discount_factor * so * (mur.rc+corrto*ux(1-pto)) * sdy * (1 - vc) * (1 - tax)
  }
  if (t==1){
    b <- 0
    utility_0 <- 0
  }
  adjusted_utility <- a+sum(b)-sum(c)-sum(utility_0)
  adj2 <- rep(adjusted_utility, py)
  pyi <- seq(0, py-1, by = 1)
  disc <- 1/(1+i)^pyi
  adjusted_utility <- sum(disc*adjusted_utility)
  return(adjusted_utility)
}

utility_data$adjusted_utility_final <- apply(utility_data, 1, function(row){
  n <- as.numeric(row["n"])
  t <- as.numeric(row["t"])
  r <- as.numeric(row["r"])
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
  py <- as.numeric(row["py"])
  return(as.numeric(utility.complete(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc, py)))
})

# Create a data frame for the plot
adjusted_utility_df <- data.frame(Adjusted_Utility_Total = utility_data$adjusted_utility_final)

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
avg_utility_estimate <- round(median(density_df$Adjusted_Utility_Total), 2)

# get the positive value
positive_utility_df <- density_df[density_df$Adjusted_Utility_Total > 0,]
avg_positive_utility <- median(positive_utility_df$Adjusted_Utility_Total)
dollar(avg_positive_utility)

# build plot
ggplot(density_df, aes(x = Adjusted_Utility_Total, y = Density)) + 
  geom_area(aes(fill = Fill), alpha=0.4) +
  scale_fill_manual(values = c("Negative" = "red", "Positive" = "green")) +
  geom_line(color="black") +
  scale_x_continuous(labels = dollar_format2, breaks = extended_breaks(6)) +
  labs(title = "",
       x = "Return on Investment Total", 
       y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, color = "black"),
        plot.background = element_rect(fill = "white", linetype = "blank"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 17),
        axis.title = element_text(size =18)) +
  annotate("text", x = quantile(density_df$Adjusted_Utility_Total, 0.7), y = max(density_df$Density)*0.85, 
           label = paste("Median estimate:\n", dollar_format2(avg_utility_estimate)), hjust = 0, size = 6) +
  annotate("text", x = quantile(density_df$Adjusted_Utility_Total, 0.7), y = max(density_df$Density)*0.75, 
           label = paste("Median of positive estimates:\n", dollar_format2(avg_positive_utility)), hjust = 0, size = 6) +
  guides(fill=FALSE)  # Remove legend
ggsave("monte.jpg", width = 16, height = 7.1, units = "in")

#### IDENTIFY GOALS TO SET ####
# Load necessary libraries
library(readr)
library(dplyr)
library(caret)
library(broom)

# Create the Positive_Utility variable
utility_data <- utility_data %>% 
  mutate(Positive_Utility = ifelse(adjusted_utility_final > 0, 1, 0))

# Exclude columns with no variation or no correlation
data <- utility_data %>% select(-c(adjusted_utility_final, cost, r, py))

# Create the logistic regression model
logitModel <- glm(Positive_Utility ~ ., data = data, family = "binomial")

# Summarize the model
summary(logitModel)

# Extract coefficients
coefficients <- coef(logitModel)

# Convert to odds ratios
odds_ratios <- exp(coefficients)

# Create a data frame to display them nicely
odds_ratios_df <- data.frame(Variable = names(odds_ratios), OddsRatio = odds_ratios)

# Print the odds ratios
print(odds_ratios_df)
