#### Pareto Optimization ####
rm(list = ls())
set.seed(123)  # For reproducibility

# package
library(usethis)
library(devtools)
library(ParetoR)
library(psych)
library(lavaan)
library(MASS)

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
sr <- 0.25 # vary this as well. 
# prop = (# of minority applicants)/(total # of applicants)
prop <- 0.35 # vary this figure. 
# Subgroup differences (d): standardized mean differences between minority and majority subgroups, on each predictor (in applicant pool)
d <- c(.72, .57, .06, .32, .04)
out <- ParetoR(prop, sr, d, Table_2.data)

# Assuming 'out' is the result from ParetoR function
# Extract the 10th set of optimal weights
optimal_weights = out$Pareto_Xmat[10, ]
optimal_weights = as.data.frame(optimal_weights)

# Example: Applying these weights to a hypothetical cohort of 100 applicants
# Assuming 'applicant_scores' is a matrix/data.frame of scores of 100 applicants on each predictor
# and 'minority_status' is a vector indicating if each applicant is a minority (1) or not (0)

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
number_of_new_hires = 100

# Calculate the new total workforce size
new_total_workforce_size = current_workforce_size + number_of_new_hires

# Calculate the new proportion of minorities
new_minority_proportion = ((current_minority_proportion * current_workforce_size) +
                             (expected_proportion_minorities_new_hires * number_of_new_hires)) / new_total_workforce_size

# Convert the proportion to a percentage
new_minority_percentage = round(new_minority_proportion * 100,2)

# Print the new proportion of minorities
print(paste("New Proportion of Minorities in the Workforce:", new_minority_percentage, "%"))
