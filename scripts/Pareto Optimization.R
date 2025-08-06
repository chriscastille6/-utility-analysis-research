#### Pareto Optimization ####
# package
library(usethis)
library(devtools)
library(ParetoR)
library(psych)
library(lavaan)
library(MASS)
library(ggplot2)
library(ggrepel)

#### EXAMPLE 1 FROM KRISTEN ####
# create correlation matrix
# Roth et al. 2011 (Table 6) & Sackett 2021 (Table 3)
lower<-'
1.00,
 .37, 1.00,
 .03,  .51, 1.00,
 .31,  .16,  .13, 1.00,
 .02,  .25,  .34,  -.02, 1.00,
 .31,  .38,  .25,  .42,  .31,  1.00
 '

Table_2.data <- getCov(lower, diagonal = TRUE, names = c("x1", "x2", "x3", "x4", "x5", "x6"))
Table_2.data

# Scenario 1
# sr = (# of selected applicants)/(total # of applicants)
sr <- 0.25
# prop = (# of minority applicants)/(total # of applicants)
prop <- 0.10
# Subgroup differences (d): standardized mean differences between minority and majority subgroups, on each predictor (in applicant pool)
d <- c(.72, .57, .06, .32, .04)
out <- ParetoR(prop, sr, d, Table_2.data)

# Assuming ‘out’ is the result from ParetoR function
# Extract the 10th set of optimal weights
optimal_weights = out$Pareto_Xmat[10, ]
optimal_weights = as.data.frame(optimal_weights)

# Example: Applying these weights to a hypothetical cohort of 100 applicants
# Assuming ‘applicant_scores’ is a matrix/data.frame of scores of 100 applicants on each predictor
# and ‘minority_status’ is a vector indicating if each applicant is a minority (1) or not (0)

# Number of applicants
n_applicants = 100

# Create correlation matrix (using your specific correlation matrix)
cor_matrix = matrix(c(
  1.00, .37, .03, .31, .02, .31,
  .37, 1.00, .51, .16, .25, .38,
  .03, .51, 1.00, .13, .34, .25,
  .31, .16, .13, 1.00, -.02, .42,
  .02, .25, .34, -.02, 1.00, .31,
  .31, .38, .25, .42, .31, 1.00
), nrow=6, byrow=TRUE)

# Simulate scores for 100 applicants
applicant_scores = mvrnorm(n_applicants, mu=rep(0, 6), Sigma=cor_matrix)
applicant_scores = as.data.frame(applicant_scores)

# Generate minority status
# 35% of 100 applicants will be minority
minority_status = sample(c(rep(1, 35), rep(0, 65)))

# Combining scores and minority status
applicant_data = data.frame(applicant_scores, Minority_Status = minority_status)

# Displaying first few rows of the simulated applicant data
head(applicant_data)

# Assuming the first 6 columns of applicant_data are score-related and should match applicant_scores
score_column_names = colnames(applicant_data)[1:6]  # Adjust the number as per your data structure

# Rename the columns of applicant_scores
colnames(applicant_scores) = score_column_names

# Calculating weighted scores for each applicant
applicant_data$weighted_score = optimal_weights[1,]*applicant_scores$V1 +
  optimal_weights$optimal_weights[2]*applicant_scores$V2 +
  optimal_weights$optimal_weights[3]*applicant_scores$V3 +
  optimal_weights$optimal_weights[4]*applicant_scores$V4 +
  optimal_weights$optimal_weights[5]*applicant_scores$V5

# assign ranks to applicants
applicant_data$rank <- rank(-applicant_data$weighted_score)

# Assign value 1 to top x candidates , 0 to others
applicant_data$selected = ifelse(applicant_data$rank <= 100*sr, 1, 0)

# Current workforce details
current_minority_proportion = 0.10  # 10% minorities in the current workforce
current_workforce_size = 1000       # Example current workforce size (adjust as needed)

# Filter selected applicants
selected_applicants <- applicant_data[applicant_data$selected == 1, ]

# Count minorities among selected
minorities_selected_count <- sum(selected_applicants$Minority_Status)

# Calculate the proportion of minorities hired
proportion_minorities_hired <- minorities_selected_count / nrow(selected_applicants)

# New hires details from Pareto optimization
# This needs to be replaced with the actual output from Pareto optimization
expected_proportion_minorities_new_hires = proportion_minorities_hired  # Example value, replace with actual output
number_of_new_hires = 150

# Calculate the new total workforce size
new_total_workforce_size = current_workforce_size + number_of_new_hires

# Calculate the new proportion of minorities
new_minority_proportion = ((current_minority_proportion * current_workforce_size) +
                             (expected_proportion_minorities_new_hires * number_of_new_hires)) / new_total_workforce_size

# Convert the proportion to a percentage
new_minority_percentage = round(new_minority_proportion * 100,2)

# Print the new proportion of minorities
print(paste("New Proportion of Minorities in the Workforce:", new_minority_percentage, "%"))

# Initial workforce composition
current_minority_proportion = 0.10  # 10% minorities
current_workforce_size = 1000       # Total workforce size

# Target minority ratio and turnover rate
target_minority_ratio = 0.35        # 35%
quarterly_turnover_rate = 0.10      # 10% turnover

# Initialize the counter for quarters passed
quarters_passed = 0

# Loop until the target minority ratio is reached
while (current_minority_proportion < target_minority_ratio) {
  # Perform Pareto optimization for hiring here
  # (Use your existing code to determine the proportion of minorities hired)
  
  # Calculate the number of minorities and non-minorities leaving (10% turnover)
  minorities_leaving = round(current_minority_proportion * current_workforce_size * quarterly_turnover_rate)
  non_minorities_leaving = round((1 - current_minority_proportion) * current_workforce_size * quarterly_turnover_rate)
  
  # Update workforce size and minority proportion after turnover
  current_workforce_size = current_workforce_size - minorities_leaving - non_minorities_leaving
  current_minority_count = current_minority_proportion * current_workforce_size
  
  # Add new hires (including the proportion of minorities)
  current_workforce_size = current_workforce_size + number_of_new_hires
  current_minority_count = current_minority_count + (expected_proportion_minorities_new_hires * number_of_new_hires)
  
  # Update current minority proportion
  current_minority_proportion = current_minority_count / current_workforce_size
  
  # Increment time (quarter)
  quarters_passed = quarters_passed + 1
}

# Calculate years taken to reach the target
years_taken = quarters_passed / 4

# Output the result
print(paste("Years taken to reach the target minority ratio:", years_taken))

library(mvtnorm)
library(ggplot2)
library(ggrepel)
n = 200
t = 2
year = 5
r = .5
sr = .25
cut = 0
crit = qnorm(1-sr)
phi.corr = matrix(c(1, r, r, 1), nrow = 2)
sp = pmvnorm(c(crit, cut), c(Inf, Inf), corr = phi.corr)/pnorm(crit, lower.tail = FALSE)
total_years = t + year
total.seq = numeric(length(total_years))
n.seq = c()
sub.seq = c(rep(0, total_years))
min.seq = c(rep(0,t))
sub <- 0
for (i in 1:year){
  sub.seq1 = c(rep(0, i))
  sub <- ceiling(n*(1-sp))
  sub.seq1 <- c(sub.seq1, rep(-sub, t-1), rep(0, total_years-i-(t-1)))
  sub.seq <- mapply(function(x, y) x + y, sub.seq1, sub.seq, USE.NAMES = FALSE)
  min.seq = c(min.seq, -n-(sub.seq[2]*(t-1)))
}
n.seq <- c(rep(n, year), rep(0, t))
for (i in 1:(total_years)){
  partial_sum <- sum(n.seq[1:i]) + sum(min.seq[1:i]) + sum(sub.seq[1:i])  
  total.seq[i] <- partial_sum
}
data <- data.frame(
  Year = 1:total_years,
  Hires = total.seq
)

plot <- ggplot(data, aes(x = Year, y = Hires)) +
  geom_line() +
  geom_text_repel(aes(label = Hires), size = 6, box.padding = 1, point.padding = 10, nudge_y = 10, direction = "y") +
  labs(
    x = "Year",
    y = "Hires"
  ) +
  ylim(0, 50 + max(total.seq)) +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1), labels = seq(min(data$Year), max(data$Year), by = 1)) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", linetype = "blank"),
        text = element_text(size = 16),  
        axis.text.x = element_text(size = 16),  
        axis.text.y = element_text(size = 16),  
        axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)  )
print(plot)

# Assuming you have a vector 'percent_minorities' with the percentage of minorities over time
# Add it to your data frame
data$Percent_Minorities <- target_minority_ratio

# Base plot with the hires
plot <- ggplot(data, aes(x = Year)) +
  geom_line(aes(y = Hires)) +
  geom_text_repel(aes(y = Hires, label = Hires), size = 6, box.padding = 1, point.padding = 10, nudge_y = 10, direction = "y") +
  scale_y_continuous(name = "Hires", sec.axis = sec_axis(~ ., name = "Percent of Minorities in the Workforce"))

# Add the line for percent minorities
plot <- plot + geom_line(aes(y = Percent_Minorities * max(Hires) / 100, color = "Percent Minorities"), size = 1)

# Customize the plot
plot <- plot +
  scale_color_manual(values = c("Percent Minorities" = "blue")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", linetype = "blank"),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.position = "none") +
  labs(
    x = "Year",
    y = "Hires"
  )

# Print the plot
print(plot)

