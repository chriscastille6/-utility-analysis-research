#### De Corte 1994 ####

# load packages
library(dplyr)
library(ggplot2)

#### Inputs ####
N <- 17         #number of hires
n <- 136        #size of applicant pool  
mu_s <- 22500   #average service cost
Time <- 11      #average number of time periods on the job for the successful selectees
S_p <- 0.853    #Success ratio of the predictor
mu_y <- 25000   #average payoff of job performance (one time period) in the applicant population
C_t <- 10000    #training cost per hire
C_p <- 200      #cost per candidate of using the predictor
C_s <- 1000     #separation cost per unsuccessful employee
rho_yR <- 0.85  #Correlation between money-valued payoff ( Y ) and observed rated performance (R)
sigma_y <- 7000 #SD
rho <- 0.35     #Predictor validity (predictor-observed performance, r)
C_r_N0 <- 100   #Recruit cost per random selection
C_r_n <- 400   #Recruit cost per predictor selection

# Solve for x_c using the inverse CDF (quantile function)
x_c <- abs(qnorm(N/n))

# Solve for r_c using inputs
r_c <- (mu_s - mu_y)/(rho_yR*sigma_y)

#### Calculate the average performance payoff predictor and randomly selected workforce ####

# Setup phi.1 function
phi.1 <- function(x) {
  result <- (1/sqrt(2 * pi) * exp(-(x^2) / 2))
  return(result)
}

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
  labs(x = "x", y = "y", title = "Figure 1", subtitle = "Selection ratio corresponding to the critical predictor score, xc") +
  theme_minimal()

# Print the plot
print(plot.1)

# Define PHI.1 and PHI.2 (density of univatiate std norm dist)
PHI.1.1 <- 1 - pnorm((x_c - rho*r_c)/sqrt(1-rho^2))
PHI.1.2 <- 1 - pnorm((r_c - rho*x_c)/sqrt(1-rho^2))

# Setup PHI.2(X,R)
library(pracma)
library(mvtnorm)

# Correlation matrix
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)

# Define lower and upper bounds
lower <- c(-.42, 1.15)
upper <- c(Inf, Inf)

# Adjust bounds for correlation
adjusted_lower <- lower
adjusted_upper <- upper

# Solve PHI.2 for density (probability) in specified region
PHI.2 <- pmvnorm(lower = adjusted_lower, upper = adjusted_upper, mean = rep(0, 2), corr = sigma)

#Solve for mu_R_xcrc 
mu_R_xcrc <- (phi.1(r_c)*PHI.1.1 + rho*phi.1(x_c)*PHI.1.2)/PHI.2[1]

# Solve for mu_y_xcrc
mu_y_xcrc <- mu_y + rho_yR * sigma_y * mu_R_xcrc

# Solve for mu_x_xc
mu_x_xc <- phi.1(x_c)/(1 - pnorm(x_c))

# Solve for mu_R_xc
mu_R_xc <- rho * mu_x_xc

# Solve for mu_y_xc
mu_y_xc <- mu_y + rho_yR * sigma_y * mu_R_xc

# Calculate S_0 and mu_y_rc
S_0 <- pnorm(-r_c)

#Solve for mu_R_rc 
mu_R_rc <- phi.1(r_c)/S_0

#Solve for mu_y_rc
mu_y_rc <- mu_y + rho_yR * sigma_y * mu_R_rc

#### Utility of the predictor selected workforce (uncorrected for separation and recruitment costs) ####
U_p <- N * (mu_y_xc - mu_s) + (Time - 1) * N * S_p * (mu_y_xcrc - mu_s) - N * C_t - n * C_p

U_0 <- N * (mu_y - mu_s) + (Time - 1) * N * S_0 * (mu_y_rc - mu_s) - N * C_t

Delta_U <- U_p - U_0

Delta_U

#### Fixed quota of successful selectees ####
N_0 <- (N/S_0) #random selection

#Solve for N_p
# Correlation matrix
sigma.2 <- matrix(c(1, rho, rho, 1), nrow = 2)

# Define lower and upper bounds
x_c.adjust <- 1.046
lower <- c(-.42, x_c.adjust) #adjust xc until N/n probability is acheived
upper <- c(Inf, Inf)

# Adjust bounds for correlation
adjusted_lower.2 <- lower
adjusted_upper.2 <- upper

# Solve PHI.2.adjust for density (probability) in specified region
PHI.2.adjust <- pmvnorm(lower = adjusted_lower.2, upper = adjusted_upper.2, mean = rep(0, 2), corr = sigma)

N_p <- (n * (1 - pnorm(x_c.adjust)))

PHI.1.1.adjust <- 1 - pnorm((x_c.adjust - rho*r_c)/sqrt(1-rho^2))
PHI.1.2.adjust <- 1 - pnorm((r_c - rho*x_c.adjust)/sqrt(1-rho^2))

#Check = N/n
PHI.2.adjust

#Solve for mu_y_xc.adjust.rc
mu_R_xc.adjust.rc <- (phi.1(r_c)*PHI.1.1.adjust + rho*phi.1(x_c.adjust)*PHI.1.2.adjust)/PHI.2.adjust[1]

mu_y_xc.adjust.rc <- mu_y + rho_yR * sigma_y * mu_R_xc.adjust.rc

#Solve for mu_y_xc.adjust
mu_x_xc.adjust <- phi.1(x_c.adjust)/(1 - pnorm(x_c.adjust))
mu_R_xc.adjust <- rho * mu_x_xc.adjust
mu_y_xc.adjust <- mu_y + rho_yR * sigma_y * mu_R_xc.adjust

#Solve Delta_U for a fixed quota of successful selectees
Delta_U.fixed.quota <-
  N_p * mu_y_xc.adjust - N_0 * mu_y -
  (N_p - N_0) * mu_s + (Time - 1) * N * 
  (mu_y_xc.adjust.rc - mu_y_rc) - (N_p - N_0) *
  C_t - n * C_p
Delta_U.fixed.quota

#Adjust for separation and recruitment costs
Delta_U.fixed.quota.corrected <- 
  Delta_U.fixed.quota - (N_p - N_0) * C_s -
  (n * C_r_n - N_0 * C_r_N0)

Delta_U.fixed.quota.corrected
