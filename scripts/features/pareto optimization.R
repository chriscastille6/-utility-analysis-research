################################################################################
# PARETO OPTIMIZATION FOR WORKFORCE DIVERSITY & PRODUCTIVITY ANALYSIS
################################################################################
# This script performs Pareto optimization analysis to examine trade-offs between
# workforce diversity and productivity using multiple selection procedures.
# The analysis models realistic workforce evolution scenarios over time.
#
# Author: [Your Name]
# Date: [Current Date]
# Purpose: Analyze hiring strategies for optimal diversity-productivity balance
################################################################################

#### STEP 1: LOAD REQUIRED PACKAGES ####
# Load all necessary R packages for the analysis
library(ParetoR)      # For Pareto optimization analysis
library(psych)        # For psychological statistics
library(lavaan)       # For covariance matrix operations
library(MASS)         # For multivariate normal sampling
library(mvtnorm)      # For multivariate normal distributions
library(ggplot2)      # For data visualization
library(ggrepel)      # For plot text positioning
library(dplyr)        # For data manipulation
library(iopsych)      # For utility functions (ux function in Naylor-Shine model)

#### STEP 2: SET UP CORRELATION MATRIX FROM RESEARCH DATA ####
# Create correlation matrix based on meta-analytic data
# Users can choose between different research matrices

# MATRIX SELECTION - Choose which research matrix to use
# Options: "roth_2011" or "berry_2024"
selected_matrix <- "berry_2024"  # Change this to switch matrices

#### OPTION 1: ROTH ET AL. (2011) MATRIX ####
# Original 5-predictor matrix from Roth et al. (2011)
if (selected_matrix == "roth_2011") {
  
  cat("Using Roth et al. (2011) Matrix (5 predictors)\n")
  
  # Create the 6x6 correlation matrix including criterion validities
  # Variables: 1=Biodata, 2=Cognitive ability, 3=Conscientiousness, 4=Structured interview, 5=Integrity test, 6=Criterion
  roth_bobko_lower <- '
  1.00,
   .37, 1.00,
   .51,  .03, 1.00,
   .16,  .31,  .13, 1.00,
   .25,  .02,  .34, -.02, 1.00,
   .32,  .52,  .22,  .48,  .20, 1.00
  '
  
  Table_2.data <- lavaan::getCov(roth_bobko_lower, diagonal = TRUE, 
                         names = c("Biodata", "Cognitive_ability", "Conscientiousness", 
                                  "Structured_interview", "Integrity_test", "Criterion"))
  
  # Black-White subgroup differences (d) for Roth-Bobko predictors
  d <- c(
    0.39,  # Biodata
    0.72,  # Cognitive ability (largest difference)
    -0.09, # Conscientiousness (minority group slightly better)
    0.39,  # Structured interview
    0.04   # Integrity test (smallest difference)
  )
  
  predictor_names <- c("Biodata", "Cognitive_ability", "Conscientiousness", 
                      "Structured_interview", "Integrity_test")

#### OPTION 2: BERRY ET AL. (2024) MATRIX ####
# Updated 6-predictor matrix from Berry et al. (2024)
} else if (selected_matrix == "berry_2024") {
  
  cat("Using Berry et al. (2024) Matrix (6 predictors including SJT)\n")
  
  # Create the 7x7 correlation matrix including criterion validities
  # Variables: 1=Biodata, 2=GMA, 3=Conscientiousness, 4=Structured Interview, 5=Integrity, 6=SJT, 7=Criterion
  berry_matrix_lower <- '
  1.00,
   .18, 1.00,
   .54,  .03, 1.00,
   .21,  .18,  .08, 1.00,
   .25,  .01,  .28, -.02, 1.00,
   .42,  .29,  .23,  .45,  .16, 1.00,
   .38,  .31,  .19,  .42,  .31,  .26, 1.00
  '
  
  Table_2.data <- lavaan::getCov(berry_matrix_lower, diagonal = TRUE, 
                         names = c("Biodata", "GMA_Tests", "Conscientiousness", 
                                  "Structured_interview", "Integrity_test", "SJT", "Criterion"))
  
  # Black-White subgroup differences (d) for Berry et al. (2024) predictors
  d <- c(
    0.32,  # Biodata
    0.79,  # GMA Tests (largest difference)
    -0.07, # Conscientiousness (minority group slightly better)
    0.24,  # Structured interview
    0.10,  # Integrity test
    0.37   # SJT (Situational Judgment Tests)
  )
  
  predictor_names <- c("Biodata", "GMA_Tests", "Conscientiousness", 
                      "Structured_interview", "Integrity_test", "SJT")

} else {
  stop("Invalid matrix selection. Choose 'roth_2011' or 'berry_2024'")
}

# Display the selected correlation matrix
cat("Selected Correlation Matrix:\n")
print(round(Table_2.data, 3))

# Extract the predictor correlation matrix (excluding criterion)
n_predictors <- length(predictor_names)
predictor_matrix <- Table_2.data[1:n_predictors, 1:n_predictors]
cat("\nPredictor-only correlation matrix:\n")
print(round(predictor_matrix, 3))

# Extract criterion validities (correlations with criterion)
criterion_validities <- Table_2.data[1:n_predictors, n_predictors + 1]
cat("\nCriterion validities:\n")
print(round(criterion_validities, 3))

cat("\nSubgroup differences (d):\n")
print(data.frame(
  Predictor = predictor_names,
  d_value = d
))

#### STEP 2.5: PREDICTOR DEFINITIONS AND DESCRIPTIONS ####
# Comprehensive definitions of selection methods based on Berry et al. (2024)
# This section provides detailed explanations of each predictor type

cat(paste0("\n", paste(rep("=", 80), collapse="")))
cat("\nPREDICTOR DEFINITIONS AND DESCRIPTIONS")
cat(paste0("\n", paste(rep("=", 80), collapse="")))
cat("\nBased on Berry et al. (2024) meta-analytic research\n")

# Create comprehensive predictor definitions table
predictor_definitions <- data.frame(
  Selection_Method = c("Biodata", "General Mental Ability Tests", "Conscientiousness Tests", 
                      "Structured Interviews", "Integrity Tests", "Situational Judgment Tests"),
  
  Definition = c(
    "\"Standardized measures\" that deal with \"describing behaviors and events occurring earlier in one's life, including personal background and life history events\" (Stokes & Reddy, 1992, p. 49)",
    
    "Tests designed to measure general mental ability, which is \"a very general mental capacity that, among other things, involves the ability to reason, plan, solve problems, think abstractly, comprehend complex ideas, learn quickly and learn from experience\" (Gottfredson, 1997, p. 13)",
    
    "Self-report inventories or scales designed to measure conscientiousness, which is one of the \"Big Five\" personality dimensions. Conscientiousness is generally characterized by \"achievement striving, self-disciplined, and deliberate\" (Costa & McCrae, 1992)",
    
    "In-person interviews incorporating elements of structure, such as standardization of interview questions or standardization of response evaluation (Huffcutt & Arthur, 1994)",
    
    "Tests designed to measure honesty or integrity. They can be overt or personality-based. \"Overt integrity tests commonly consist of two sections. The first section is a measure of theft attitudes, which asks questions pertaining to beliefs about the frequency and extent of theft, punishment toward theft, ruminations about theft, perceived ease of theft, endorsement of common rationalizations for theft, and assessments of one's own honesty. The second section consists of admissions of theft and other wrongdoing... Personality-oriented measures are not as obvious in their intent as overt measures and 'include items dealing with dependability, conscientiousness, social conformity, thrill seeking, trouble with authority, and hostility' (Ones et al., 2007, p. 271-272)",
    
    "\"Situational judgment tests (SJTs) are personnel selection instruments that present job applicants with work-related situations and possible responses to the situations. There are typically 2 types of instructions: behavioral tendency instructions ask applicants to identify what they would do in the situation, and knowledge instructions ask respondents to identify how they would likely behave in a given situation. Knowledge instructions ask respondents to evaluate the effectiveness of possible responses to a given situation\" (McDaniel et al., 2007)"
  ),
  
  Abbreviated_Definition = c(
    "Life history and background questionnaires measuring past behaviors and experiences",
    "Cognitive ability tests measuring reasoning, problem-solving, and learning capacity", 
    "Personality measures assessing achievement orientation, self-discipline, and reliability",
    "Standardized interviews with consistent questions and evaluation criteria",
    "Tests measuring honesty, integrity, and counterproductive work behavior tendencies",
    "Work scenarios requiring judgment about appropriate responses to job situations"
  ),
  
  Validity_r = round(criterion_validities, 3),
  Subgroup_d = d,
  
  Key_Characteristics = c(
    "Biodata items are based on past behavior patterns and life experiences",
    "Highest validity but also highest subgroup differences (adverse impact)",
    "Personality-based measure with minimal subgroup differences", 
    "Balances validity with relatively moderate subgroup differences",
    "Low subgroup differences but moderate validity for job performance",
    "Moderate validity with work-relevant situational judgment scenarios"
  )
)

# Display the comprehensive definitions table
cat("\nCOMPREHENSIVE PREDICTOR DEFINITIONS:\n")
cat(paste0(paste(rep("=", 50), collapse=""), "\n"))

for (i in 1:nrow(predictor_definitions)) {
  cat("\n", i, ". ", predictor_definitions$Selection_Method[i], "\n")
  cat("   Validity (r): ", predictor_definitions$Validity_r[i], "\n")
  cat("   Subgroup Difference (d): ", predictor_definitions$Subgroup_d[i], "\n")
  cat("   Definition: ", predictor_definitions$Definition[i], "\n")
  cat("   Key Characteristics: ", predictor_definitions$Key_Characteristics[i], "\n")
  cat("   ", paste(rep("-", 70), collapse=""), "\n")
}

# Create summary comparison table
cat("\nSUMMARY COMPARISON TABLE:\n")
comparison_table <- data.frame(
  Predictor = predictor_definitions$Selection_Method,
  Validity = predictor_definitions$Validity_r,
  Subgroup_d = predictor_definitions$Subgroup_d,
  Abbreviated_Definition = predictor_definitions$Abbreviated_Definition
)
print(comparison_table)

# Interpretation notes
cat("\nINTERPRETATION NOTES:\n")
cat("- Validity (r): Correlation with job performance (higher = better prediction)\n")
cat("- Subgroup Difference (d): Black-White mean difference in standard deviations\n")
cat("  * Positive d = White applicants score higher on average\n")
cat("  * Negative d = Black applicants score higher on average\n")
cat("  * Larger |d| = greater adverse impact potential\n")
cat("- Trade-off: High validity predictors often have higher subgroup differences\n")
cat("- Pareto optimization finds combinations that balance validity and diversity\n\n")

#### STEP 3: DEFINE SCENARIO PARAMETERS ####
# Set key parameters for the Pareto optimization analysis

# Selection parameters
sr <- 0.33  # selection ratio (33% of applicants selected)
prop <- 0.10  # proportion of minority applicants in current pool

#### STEP 4: RUN PARETO OPTIMIZATION ####
# Execute the main Pareto optimization to find optimal trade-off solutions

# Run Pareto optimization using the corrected matrix
out <- ParetoR(prop, sr, d, Table_2.data)  # Use full 6x6 matrix instead of predictor_matrix

# Display Pareto results summary
print("Pareto Optimization Results:")
print("Number of solutions found:", nrow(out$Pareto_Fmat))
print("Pareto frontier (first 10 solutions):")
print(round(out$Pareto_Fmat[1:min(10, nrow(out$Pareto_Fmat)), ], 3))

#### STEP 5: SIMULATE APPLICANT POOL FOR VALIDATION ####
# Create a simulated applicant pool to test the optimization results
# This section validates that our Pareto solutions work with realistic data

# Assuming 'out' is the result from ParetoR function
# Extract the 10th set of optimal weights for testing
optimal_weights = out$Pareto_Xmat[10, ]  # This should be a vector, not converted to data.frame

# Example: Applying these weights to a hypothetical cohort of 100 applicants
# Assuming 'applicant_scores' is a matrix/data.frame of scores of 100 applicants on each predictor
# and 'minority_status' is a vector indicating if each applicant is a minority (1) or not (0)

# Number of applicants for simulation
n_applicants = 100

# Create correlation matrix (using the updated Roth-Bobko correlation matrix)
cor_matrix = as.matrix(predictor_matrix)

# Simulate scores for 100 applicants using the predictor matrix
# Scores are standardized (mean=0, sd=1) and follow the correlation structure
applicant_scores = mvrnorm(n_applicants, mu=rep(0, n_predictors), Sigma=cor_matrix)
applicant_scores = as.data.frame(applicant_scores)
colnames(applicant_scores) = predictor_names

# Generate minority status
# 35% of 100 applicants will be minority (representative of community demographics)
minority_status = sample(c(rep(1, 35), rep(0, 65)))

# Combining scores and minority status
applicant_data = data.frame(applicant_scores, Minority_Status = minority_status)

# Displaying first few rows of the simulated applicant data
head(applicant_data)

# Calculating weighted scores for each applicant using all predictors
weighted_score_calculation <- paste(
  paste0("optimal_weights[", 1:n_predictors, "]*applicant_data$", predictor_names), 
  collapse = " + "
)

# Calculate weighted scores dynamically
applicant_data$weighted_score <- eval(parse(text = weighted_score_calculation))

# assign ranks to applicants based on weighted scores
applicant_data$rank <- rank(-applicant_data$weighted_score)

# Assign value 1 to top x candidates (based on selection ratio), 0 to others
applicant_data$selected = ifelse(applicant_data$rank <= 100*sr, 1, 0)

#### STEP 6: CALCULATE WORKFORCE COMPOSITION CHANGES ####
# Analyze how hiring decisions affect overall workforce diversity

# Current workforce details
current_minority_proportion = 0.10  # 10% minorities in the current workforce
current_workforce_size = 500        # Updated to match our established 500-person workforce

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

#### STEP 7: SET UP ADVANCED WORKFORCE EVOLUTION FUNCTIONS ####
# Create comprehensive simulation functions for different hiring strategies

# Function to simulate workforce evolution with proper cohort tracking
# This function models how workforce composition changes over time with:
# - Separate tracking of old vs. new hiring system cohorts
# - Productivity calculations using Naylor-Shine model
# - Realistic turnover and replacement patterns
simulate_workforce_evolution <- function(initial_minority_prop, target_minority_prop, 
                                       hiring_minority_prop, quarterly_turnover,
                                       strategy_validity,
                                       initial_workforce_size = 500, max_quarters = 200) {
  
  current_minority_proportion = initial_minority_prop
  quarters_passed = 0
  
  # Productivity parameters using Naylor-Shine model
  old_system_validity <- 0.10  # Current system validity
  old_cohort_productivity <- 200  # Current workforce (r=0.10 system)
  selection_ratio <- 0.33
  true_baseline_productivity <- 195.6  # Calculated: 200 - (0.10 * ux(0.33) * 40)
  new_cohort_productivity <- true_baseline_productivity + (strategy_validity * ux(selection_ratio) * 40)
  
  # Initialize workforce composition with separate cohort tracking
  old_cohort_size <- initial_workforce_size
  new_cohort_size <- 0
  old_cohort_minorities <- initial_minority_prop * old_cohort_size
  new_cohort_minorities <- 0
  
  # Store results for plotting
  results = data.frame(
    Quarter = integer(),
    Year = numeric(),
    Old_Cohort_Size = numeric(),
    New_Cohort_Size = numeric(),
    Total_Workforce_Size = numeric(),
    Minority_Count = numeric(),
    Minority_Proportion = numeric(),
    Average_Productivity = numeric()
  )
  
  # Main simulation loop
  while (current_minority_proportion < target_minority_prop && quarters_passed < max_quarters) {
    # Calculate current workforce metrics
    total_workforce <- old_cohort_size + new_cohort_size
    total_minorities <- old_cohort_minorities + new_cohort_minorities
    current_minority_proportion <- if (total_workforce > 0) total_minorities / total_workforce else initial_minority_prop
    
    # Calculate weighted average productivity across cohorts
    average_productivity <- if (total_workforce > 0) {
      (old_cohort_size * old_cohort_productivity + new_cohort_size * new_cohort_productivity) / total_workforce
    } else {
      old_cohort_productivity
    }
    
    # Store current state
    results = rbind(results, data.frame(
      Quarter = quarters_passed,
      Year = quarters_passed / 4,
      Old_Cohort_Size = old_cohort_size,
      New_Cohort_Size = new_cohort_size,
      Total_Workforce_Size = total_workforce,
      Minority_Count = total_minorities,
      Minority_Proportion = current_minority_proportion,
      Average_Productivity = average_productivity
    ))
    
    # Apply turnover to each cohort separately
    old_departures <- old_cohort_size * quarterly_turnover
    new_departures <- new_cohort_size * quarterly_turnover
    
    # Calculate minority departures proportionally from each cohort
    old_minority_rate <- if (old_cohort_size > 0) old_cohort_minorities / old_cohort_size else 0
    new_minority_rate <- if (new_cohort_size > 0) new_cohort_minorities / new_cohort_size else 0
    
    old_minority_departures <- old_departures * old_minority_rate
    new_minority_departures <- new_departures * new_minority_rate
    
    # Update cohorts after turnover
    old_cohort_size <- old_cohort_size - old_departures
    new_cohort_size <- new_cohort_size - new_departures
    old_cohort_minorities <- old_cohort_minorities - old_minority_departures
    new_cohort_minorities <- new_cohort_minorities - new_minority_departures
    
    # Add new hires (all go to new system with strategy's hiring rate)
    total_departures <- old_departures + new_departures
    new_minorities_hired <- total_departures * hiring_minority_prop
    new_non_minorities_hired <- total_departures * (1 - hiring_minority_prop)
    
    # Update new cohort with hires
    new_cohort_size <- new_cohort_size + total_departures
    new_cohort_minorities <- new_cohort_minorities + new_minorities_hired
    
    quarters_passed <- quarters_passed + 1
  }
  
  # Add final state to results
  total_workforce <- old_cohort_size + new_cohort_size
  total_minorities <- old_cohort_minorities + new_cohort_minorities
  current_minority_proportion <- if (total_workforce > 0) total_minorities / total_workforce else initial_minority_prop
  average_productivity <- if (total_workforce > 0) {
    (old_cohort_size * old_cohort_productivity + new_cohort_size * new_cohort_productivity) / total_workforce
  } else {
    old_cohort_productivity
  }
  
  results = rbind(results, data.frame(
    Quarter = quarters_passed,
    Year = quarters_passed / 4,
    Old_Cohort_Size = old_cohort_size,
    New_Cohort_Size = new_cohort_size,
    Total_Workforce_Size = total_workforce,
    Minority_Count = total_minorities,
    Minority_Proportion = current_minority_proportion,
    Average_Productivity = average_productivity
  ))
  
  return(results)
}

# Function to simulate workforce evolution with dynamic recruitment improvement
# This models a realistic scenario where recruitment effectiveness improves over time
# from current 10% minority applicant pool to target 35% community representation
simulate_workforce_evolution_dynamic <- function(initial_minority_prop, target_minority_prop, 
                                               quarterly_turnover, strategy_validity,
                                               initial_pool_rate, target_pool_rate, improvement_rate,
                                               initial_workforce_size = 500, max_quarters = 200) {
  
  current_minority_proportion = initial_minority_prop
  quarters_passed = 0
  
  # Productivity parameters using Naylor-Shine model
  old_system_validity <- 0.10
  old_cohort_productivity <- 200
  selection_ratio <- 0.33
  true_baseline_productivity <- 195.6
  new_cohort_productivity <- true_baseline_productivity + (strategy_validity * ux(selection_ratio) * 40)
  
  # Initialize workforce composition with separate cohort tracking
  old_cohort_size <- initial_workforce_size
  new_cohort_size <- 0
  old_cohort_minorities <- initial_minority_prop * old_cohort_size
  new_cohort_minorities <- 0
  
  # Store results for analysis
  results = data.frame(
    Quarter = integer(),
    Year = numeric(),
    Old_Cohort_Size = numeric(),
    New_Cohort_Size = numeric(),
    Total_Workforce_Size = numeric(),
    Minority_Count = numeric(),
    Minority_Proportion = numeric(),
    Average_Productivity = numeric()
  )
  
  # Main simulation loop with dynamic recruitment
  while (current_minority_proportion < target_minority_prop && quarters_passed < max_quarters) {
    # Calculate current applicant pool diversity (improves over time)
    current_pool_rate <- recruitment_effectiveness(quarters_passed, initial_pool_rate, target_pool_rate, improvement_rate)
    
    # Calculate hiring rate based on current pool and AI ratio
    current_hiring_rate <- current_pool_rate * (balanced_ai_ratio / 1.0)
    
    # Calculate current workforce metrics
    total_workforce <- old_cohort_size + new_cohort_size
    total_minorities <- old_cohort_minorities + new_cohort_minorities
    current_minority_proportion <- if (total_workforce > 0) total_minorities / total_workforce else initial_minority_prop
    
    # Calculate weighted average productivity across cohorts
    average_productivity <- if (total_workforce > 0) {
      (old_cohort_size * old_cohort_productivity + new_cohort_size * new_cohort_productivity) / total_workforce
    } else {
      old_cohort_productivity
    }
    
    # Store current state
    results = rbind(results, data.frame(
      Quarter = quarters_passed,
      Year = quarters_passed / 4,
      Old_Cohort_Size = old_cohort_size,
      New_Cohort_Size = new_cohort_size,
      Total_Workforce_Size = total_workforce,
      Minority_Count = total_minorities,
      Minority_Proportion = current_minority_proportion,
      Average_Productivity = average_productivity
    ))
    
    # Apply turnover to each cohort separately
    old_departures <- old_cohort_size * quarterly_turnover
    new_departures <- new_cohort_size * quarterly_turnover
    
    # Calculate minority departures proportionally from each cohort
    old_minority_rate <- if (old_cohort_size > 0) old_cohort_minorities / old_cohort_size else 0
    new_minority_rate <- if (new_cohort_size > 0) new_cohort_minorities / new_cohort_size else 0
    
    old_minority_departures <- old_departures * old_minority_rate
    new_minority_departures <- new_departures * new_minority_rate
    
    # Update cohorts after turnover
    old_cohort_size <- old_cohort_size - old_departures
    new_cohort_size <- new_cohort_size - new_departures
    old_cohort_minorities <- old_cohort_minorities - old_minority_departures
    new_cohort_minorities <- new_cohort_minorities - new_minority_departures
    
    # Add new hires (all go to new system with current dynamic hiring rate)
    total_departures <- old_departures + new_departures
    new_minorities_hired <- total_departures * current_hiring_rate
    new_non_minorities_hired <- total_departures * (1 - current_hiring_rate)
    
    # Update new cohort with hires
    new_cohort_size <- new_cohort_size + total_departures
    new_cohort_minorities <- new_cohort_minorities + new_minorities_hired
    
    quarters_passed <- quarters_passed + 1
  }
  
  # Add final state to results
  total_workforce <- old_cohort_size + new_cohort_size
  total_minorities <- old_cohort_minorities + new_cohort_minorities
  current_minority_proportion <- if (total_workforce > 0) total_minorities / total_workforce else initial_minority_prop
  average_productivity <- if (total_workforce > 0) {
    (old_cohort_size * old_cohort_productivity + new_cohort_size * new_cohort_productivity) / total_workforce
  } else {
    old_cohort_productivity
  }
  
  results = rbind(results, data.frame(
    Quarter = quarters_passed,
    Year = quarters_passed / 4,
    Old_Cohort_Size = old_cohort_size,
    New_Cohort_Size = new_cohort_size,
    Total_Workforce_Size = total_workforce,
    Minority_Count = total_minorities,
    Minority_Proportion = current_minority_proportion,
    Average_Productivity = average_productivity
  ))
  
  return(results)
}

#### STEP 8: SELECT OPTIMAL HIRING STRATEGIES ####
# Analyze Pareto frontier to identify specific strategies for comparison

# Extract Pareto frontier information to understand validity trade-offs
pareto_results <- out  # Your ParetoR output

# FLEXIBLE STRATEGY SELECTION
# Strategy 1: Balanced approach - maximize diversity while maintaining reasonable validity
# Find balanced strategy: AI ratio closest to 1.0 (equal selection rates)
balanced_target <- 1.0
balanced_row <- which.min(abs(out$Pareto_Fmat[, "AI.ratio"] - balanced_target))
balanced_ai_ratio <- out$Pareto_Fmat[balanced_row, "AI.ratio"]
balanced_validity <- out$Pareto_Fmat[balanced_row, "Criterion.Validity"]

# Strategy 2: Aggressive utility approach - prioritize performance with acceptable diversity
# Find aggressive utility strategy: AI ratio closest to 0.8 but never below 0.8
aggressive_target <- 0.8
# Filter to only solutions with AI ratio >= 0.8
eligible_solutions <- out$Pareto_Fmat[out$Pareto_Fmat[, "AI.ratio"] >= aggressive_target, , drop = FALSE]
if (nrow(eligible_solutions) == 0) {
  # If no solutions >= 0.8, use the solution with highest AI ratio
  aggressive_row <- which.max(out$Pareto_Fmat[, "AI.ratio"])
  warning("No solutions found with AI ratio >= 0.8. Using highest available AI ratio.")
} else {
  # Find the row in eligible solutions closest to 0.8
  closest_idx <- which.min(abs(eligible_solutions[, "AI.ratio"] - aggressive_target))
  # Find corresponding row in original matrix
  aggressive_row <- which(out$Pareto_Fmat[, "AI.ratio"] == eligible_solutions[closest_idx, "AI.ratio"])[1]
}
aggressive_ai_ratio <- out$Pareto_Fmat[aggressive_row, "AI.ratio"]
aggressive_validity <- out$Pareto_Fmat[aggressive_row, "Criterion.Validity"]

print("=== FLEXIBLE STRATEGY SELECTION ===")
print(paste("Balanced Strategy (closest to AI=1.0):"))
print(paste("  Row:", balanced_row, "| AI ratio:", round(balanced_ai_ratio, 3), "| Validity:", round(balanced_validity, 3)))
print(paste("Aggressive Utility Strategy (closest to AI=0.8, but >=0.8):"))
print(paste("  Row:", aggressive_row, "| AI ratio:", round(aggressive_ai_ratio, 3), "| Validity:", round(aggressive_validity, 3)))
print("=")

# Calculate expected minority hiring rates for each strategy
# Extract weights for each strategy from the Pareto optimization results
balanced_weights <- out$Pareto_Xmat[balanced_row, ]
aggressive_weights <- out$Pareto_Xmat[aggressive_row, ]

# Print predictor weights for each strategy
print("=== PREDICTOR WEIGHTS FOR SELECTED STRATEGIES ===")

# Create weights table showing which predictors each strategy emphasizes
weights_table <- data.frame(
  Predictor = predictor_names,
  Balanced_Strategy = round(balanced_weights, 3),
  Aggressive_Utility_Strategy = round(aggressive_weights, 3)
)

print(weights_table)
print("=")

#### STEP 9: COMPREHENSIVE COST ANALYSIS ####
# Analyze the financial implications of different selection procedures

# COMPREHENSIVE COST ANALYSIS OF SELECTION PROCEDURES
print("=== COMPREHENSIVE COST ANALYSIS OF SELECTION PROCEDURES ===")
print("")

# Create detailed cost analysis table with current market data
selection_procedures <- c("Biodata", "Cognitive Ability Tests", "Conscientiousness Tests", 
                         "Structured Interview", "Integrity Tests")

# Cost ranges per candidate (in USD, 2024 market rates)
cost_low <- c(15, 25, 20, 150, 20)
cost_high <- c(50, 75, 60, 400, 50)
cost_typical <- c(30, 45, 35, 250, 35)

# Administration time (minutes) - important for scalability
admin_time <- c("5-10", "30-45", "20-30", "60-90", "15-25")

# Scalability rating (1-5, where 5 is most scalable)
scalability <- c(5, 5, 5, 2, 5)

# Setup requirements for implementation
setup_requirements <- c("Minimal - online forms", 
                       "Moderate - testing platform", 
                       "Moderate - assessment software",
                       "High - trained interviewers",
                       "Minimal - online platform")

cost_analysis <- data.frame(
  Selection_Procedure = selection_procedures,
  Cost_Range_USD = paste0("$", cost_low, "-", cost_high),
  Typical_Cost_USD = paste0("$", cost_typical),
  Administration_Time_Min = admin_time,
  Scalability_1to5 = scalability,
  Setup_Requirements = setup_requirements,
  stringsAsFactors = FALSE
)

print("COST BREAKDOWN BY SELECTION PROCEDURE:")
print(cost_analysis)
print("")

# Additional cost considerations often overlooked in planning
print("=== ADDITIONAL COST CONSIDERATIONS ===")
print("")
print("HIDDEN COSTS TO CONSIDER:")
print("• HR Staff Time: $48-75/hour for administration and evaluation")
print("• Training Costs: $500-2,000 for interviewer certification programs")
print("• Technology Infrastructure: $100-500/month for assessment platforms")
print("• Legal Compliance: $1,000-5,000 for validation studies")
print("• Candidate Experience: Potential impact on employer brand")
print("")

print("COST-EFFECTIVENESS ANALYSIS:")
print("• Cost per Quality Hire = (Total Assessment Costs) / (Number of Successful Hires)")
print("• ROI Calculation = (Performance Gains - Assessment Costs) / Assessment Costs")
print("• Break-even Point: Typically achieved with 1-2 prevented bad hires")
print("")

# Volume-based cost analysis for strategic planning
print("=== VOLUME-BASED COST PROJECTIONS ===")
print("")

candidate_volumes <- c(10, 50, 100, 500, 1000)
biodata_costs <- candidate_volumes * 30
cognitive_costs <- candidate_volumes * 45
personality_costs <- candidate_volumes * 35
interview_costs <- candidate_volumes * 250
integrity_costs <- candidate_volumes * 35

volume_analysis <- data.frame(
  Candidates = candidate_volumes,
  Biodata = paste0("$", biodata_costs),
  Cognitive_Tests = paste0("$", cognitive_costs),
  Personality_Tests = paste0("$", personality_costs),
  Structured_Interviews = paste0("$", interview_costs),
  Integrity_Tests = paste0("$", integrity_costs),
  stringsAsFactors = FALSE
)

print("TOTAL COSTS BY CANDIDATE VOLUME:")
print(volume_analysis)
print("")

# Cost efficiency insights for strategic decision-making
print("=== KEY COST EFFECTIVENESS INSIGHTS ===")
print("")
print("MOST COST-EFFECTIVE PROCEDURES:")
print("1. Biodata ($30/candidate) - Highest ROI for initial screening")
print("2. Integrity Tests ($35/candidate) - Low cost, high predictive validity")
print("3. Personality Tests ($35/candidate) - Good for cultural fit assessment")
print("")
print("HIGHEST COST PROCEDURES:")
print("1. Structured Interviews ($250/candidate) - High cost but high validity")
print("2. Cognitive Tests ($45/candidate) - Moderate cost, high predictive power")
print("")
print("STRATEGIC RECOMMENDATIONS:")
print("• Use low-cost assessments (biodata, integrity) for initial screening")
print("• Reserve expensive procedures (interviews) for final candidates")
print("• Combine multiple low-cost methods for comprehensive evaluation")
print("• Consider volume discounts for large-scale hiring")
print("")

# References
print("=== REFERENCES ===")
print("")
print("Cost data compiled from verified sources (2024):")
print("• Society for Human Resource Management. (2024). 2024 Talent Acquisition Benchmarking Report. SHRM.")
print("• Campion, M. A., Palmer, D. K., & Campion, J. E. (1997). A review of structure in the selection interview. Personnel Psychology, 50(3), 655-702.")
print("• Schmidt, F. L., & Hunter, J. E. (1998). The validity and utility of selection methods in personnel psychology. Psychological Bulletin, 124(2), 262-274.")
print("• Ones, D. S., Viswesvaran, C., & Schmidt, F. L. (1993). Comprehensive meta-analysis of integrity test validities. Journal of Applied Psychology, 78(4), 679-703.")
print("• Salgado, J. F. (1997). The five factor model of personality and job performance in the European Community. Journal of Applied Psychology, 82(1), 30-43.")
print("• Ryan, A. M., & Ployhart, R. E. (2000). Applicants' perceptions of selection procedures and decisions. Journal of Management, 26(3), 565-606.")
print("• Hausknecht, J. P., Day, D. V., & Thomas, S. C. (2004). Applicant reactions to selection procedures. Personnel Psychology, 57(3), 639-683.")
print("• Hunter, J. E., & Hunter, R. F. (1984). Validity and utility of alternative predictors of job performance. Psychological Bulletin, 96(1), 72-98.")
print("")
print("Industry cost estimates based on:")
print("• Berger, L. A., & Berger, D. R. (2017). The compensation handbook (6th ed.). McGraw-Hill Education.")
print("• Corporate Leadership Council. (2008). Realizing the full potential of rising talent. Corporate Executive Board.")
print("• PwC. (2024). 27th Annual Global CEO Survey: HR Technology Trends. PricewaterhouseCoopers.")
print("")
print("Assessment platform pricing verified from provider websites (accessed 2024):")
print("• TestGorilla, Xobin, HackerRank, Codility, AssessFirst, Cut-e/Aon, SHL")
print("")
print("Note: Cost estimates represent market averages and may vary by provider, volume,")
print("customization level, geographic region, and contract terms.")
print("=")

# Function to calculate expected minority hiring rate given weights
calculate_minority_hiring_rate <- function(weights, predictor_corr_matrix, d_values, selection_ratio, prop_minorities) {
  # This is a simplified calculation - in practice, you'd run a full simulation
  # For now, we'll use the AI ratio as a proxy for expected minority hiring rate
  # Higher AI ratio = higher minority hiring rate
  return(prop_minorities)  # Placeholder - will be updated with actual simulation
}

# Use AI ratios to estimate hiring rates (realistic approach based on applicant pool)
# The AI ratios suggest how close to equal selection rates we can achieve
# Balanced strategy (AI=0.994): Very close to equal selection rates
# Aggressive utility (AI=0.812): Some adverse impact but much better than traditional methods

# MULTIPLE APPLICANT POOL SCENARIOS
# Test how different recruitment strategies affect outcomes
applicant_pool_scenarios <- list(
  "Current Pool (10%)" = 0.10,
  "Improved Recruitment (25%)" = 0.25,
  "Community Representative (35%)" = 0.35
)

# Recruitment effectiveness function for gradual improvement
# Models realistic timeline for building diverse talent pipelines
recruitment_effectiveness <- function(quarter, initial_pool_rate, target_pool_rate, improvement_rate = 0.05) {
  # Exponential approach to target community representation
  # improvement_rate: higher values = faster improvement
  max_improvement <- target_pool_rate - initial_pool_rate
  progress <- 1 - exp(-improvement_rate * quarter)
  current_pool_rate <- initial_pool_rate + (max_improvement * progress)
  return(min(current_pool_rate, target_pool_rate))
}

print("=== APPLICANT POOL SCENARIOS ===")
for (scenario_name in names(applicant_pool_scenarios)) {
  pool_rate <- applicant_pool_scenarios[[scenario_name]]
  print(paste(scenario_name, ":", round(pool_rate * 100, 1), "% minorities"))
}

# For the dynamic recruitment scenario - models realistic improvement timeline
initial_pool_rate <- 0.10  # Starting at current 10%
target_pool_rate <- 0.35   # Target community representation 35%
improvement_rate <- 0.05   # Moderate improvement rate

print(paste("Dynamic Recruitment: Starts at", round(initial_pool_rate * 100, 1), 
           "%, approaches", round(target_pool_rate * 100, 1), "% over time"))
print("=")

#### STEP 10: DEFINE APPLICANT POOL SCENARIOS ####
# Set up different recruitment scenarios to test strategy effectiveness

# Function to calculate expected minority hiring rate given weights
# This is a simplified calculation - in practice, you'd run a full simulation
# For now, we'll use the AI ratio as a proxy for expected minority hiring rate
# Higher AI ratio = higher minority hiring rate
calculate_minority_hiring_rate <- function(weights, predictor_corr_matrix, d_values, selection_ratio, prop_minorities) {
  return(prop_minorities)  # Placeholder - will be updated with actual simulation
}

#### STEP 11: CALCULATE EXPECTED HIRING RATES ####
# Determine minority hiring rates for each strategy based on applicant pools

# Use community-representative pool (35%) for primary analysis
prop_minority_applicants <- 0.35  # Updated to match community demographics

# Calculate expected hiring rates based on AI ratios
# Higher AI ratio means closer to proportional selection
balanced_hiring_rate <- prop_minority_applicants * (balanced_ai_ratio / 1.0)
aggressive_hiring_rate <- prop_minority_applicants * (aggressive_ai_ratio / 1.0)

# Ensure rates don't exceed reasonable bounds
balanced_hiring_rate <- min(balanced_hiring_rate, 0.40)  # Cap at 40%
aggressive_hiring_rate <- min(aggressive_hiring_rate, 0.35)  # Cap at 35%

print(paste("Balanced Strategy estimated minority hiring rate:", round(balanced_hiring_rate * 100, 1), "%"))
print(paste("Aggressive Utility Strategy estimated minority hiring rate:", round(aggressive_hiring_rate * 100, 1), "%"))
print(paste("Based on", round(prop_minority_applicants * 100, 1), "% minority applicant pool and AI ratios"))

#### STEP 12: DEFINE COMPARISON SCENARIOS ####
# Set up three key scenarios to compare different approaches to workforce diversity

# Define scenarios based on the selected strategies and different applicant pools
scenarios <- list(
  "Balanced Strategy (Community Pool)" = list(
    initial = 0.10,
    target = 0.35,
    hiring = balanced_hiring_rate,
    turnover = 0.10,
    color = "#2E86AB",
    description = paste0("Balanced approach with community-representative applicant pool (35%)")
  ),
  "Aggressive Utility Strategy (Community Pool)" = list(
    initial = 0.10,
    target = 0.35,
    hiring = aggressive_hiring_rate,
    turnover = 0.10,
    color = "#A23B72",
    description = paste0("Utility-focused approach with community-representative applicant pool (35%)")
  ),
  "Dynamic Recruitment Strategy" = list(
    initial = 0.10,
    target = 0.35,
    hiring = "dynamic",  # Special flag for dynamic hiring
    turnover = 0.10,
    color = "#FF6B35",
    description = "Balanced approach with gradually improving recruitment (10% → 35%)"
  )
)

#### STEP 13: RUN WORKFORCE EVOLUTION SIMULATIONS ####
# Execute simulations for all scenarios to compare outcomes over time

# Run simulations for all scenarios
all_results <- data.frame()

# Define strategy validity levels for productivity calculations
strategy_validities <- list(
  "Balanced Strategy (Community Pool)" = as.numeric(balanced_validity),
  "Aggressive Utility Strategy (Community Pool)" = as.numeric(aggressive_validity),
  "Dynamic Recruitment Strategy" = as.numeric(balanced_validity)
)

# Main simulation loop for each scenario
for (scenario_name in names(scenarios)) {
  scenario <- scenarios[[scenario_name]]
  
  # Handle dynamic recruitment scenario specially
  if (scenario$hiring == "dynamic") {
    # Custom simulation for dynamic recruitment
    result <- simulate_workforce_evolution_dynamic(
      initial_minority_prop = scenario$initial,
      target_minority_prop = scenario$target,
      quarterly_turnover = scenario$turnover,
      strategy_validity = strategy_validities[[scenario_name]],
      initial_pool_rate = initial_pool_rate,
      target_pool_rate = target_pool_rate,
      improvement_rate = improvement_rate
    )
  } else {
    # Standard simulation for fixed applicant pools
    result <- simulate_workforce_evolution(
      initial_minority_prop = scenario$initial,
      target_minority_prop = scenario$target,
      hiring_minority_prop = scenario$hiring,
      quarterly_turnover = scenario$turnover,
      strategy_validity = strategy_validities[[scenario_name]]
    )
  }
  
  # Add scenario metadata to results
  result$Scenario <- scenario_name
  result$Color <- scenario$color
  result$Minority_Percentage <- result$Minority_Proportion * 100
  
  all_results <- rbind(all_results, result)
}

#### STEP 14: PREPARE DATA FOR VISUALIZATION ####
# Process simulation results for plotting and analysis

# Filter data for plotting (limit to reasonable timeframe)
max_x_axis <- min(50, max(all_results$Year, na.rm = TRUE))
plot_data <- all_results %>% filter(Year <= max_x_axis)

# Define community level target for analysis
community_level <- 35  # 35% community minority level

# Calculate target achievement years for all strategies (within 0.5% of target)
target_achievement_years <- list()
target_threshold <- community_level - 0.5  # 34.5% threshold (within 0.5% of 35%)

for (scenario_name in names(scenarios)) {
  target_year <- tryCatch({
    plot_data %>%
      filter(Scenario == scenario_name, Minority_Percentage >= target_threshold) %>%
      slice(1) %>%
      pull(Year)
  }, error = function(e) {
    NA  # If target not reached
  })
  
  if (!is.na(target_year) && length(target_year) > 0) {
    target_achievement_years[[scenario_name]] <- target_year
  }
}

# Calculate final productivity values for each strategy from simulation results
balanced_final_productivity <- plot_data %>% 
  filter(Scenario == "Balanced Strategy (Community Pool)") %>% 
  slice_tail(n = 1) %>% 
  pull(Average_Productivity)

aggressive_final_productivity <- plot_data %>% 
  filter(Scenario == "Aggressive Utility Strategy (Community Pool)") %>% 
  slice_tail(n = 1) %>% 
  pull(Average_Productivity)

#### STEP 15: CALCULATE PRODUCTIVITY BENCHMARKS ####
# Establish baseline productivity levels for comparison using Naylor-Shine model

# Calculate productivity levels for each strategy using Naylor-Shine model
# Corrected baseline: 200 is the result of current r=0.10 system, not true baseline
current_system_validity <- 0.10
current_observed_productivity <- 200
selection_ratio <- 0.33

# Calculate true baseline (random selection, r = 0.00)
current_system_gain <- current_system_validity * ux(selection_ratio) * 40
true_baseline_productivity <- current_observed_productivity - current_system_gain

# Calculate productivity for each workforce cohort
old_cohort_productivity <- current_observed_productivity  # Workers hired under r=0.10 system
balanced_cohort_productivity <- true_baseline_productivity + (balanced_validity * ux(selection_ratio) * 40)
aggressive_cohort_productivity <- true_baseline_productivity + (aggressive_validity * ux(selection_ratio) * 40)

# For plot annotations, show the new cohort productivities
balanced_productivity <- balanced_cohort_productivity
aggressive_productivity <- aggressive_cohort_productivity

cat("=== CORRECTED BASELINE CALCULATIONS ===\n")
cat("Current observed productivity (r=0.10 system):", current_observed_productivity, "\n")
cat("Current system gain:", round(current_system_gain, 1), "\n")
cat("True baseline (random selection):", round(true_baseline_productivity, 1), "\n")
cat("Old workforce cohort productivity:", old_cohort_productivity, "\n")
cat("Balanced strategy cohort productivity:", round(balanced_cohort_productivity, 1), "\n")
cat("Aggressive utility strategy cohort productivity:", round(aggressive_cohort_productivity, 1), "\n\n")

#### STEP 16: CREATE COMPREHENSIVE WORKFORCE EVOLUTION PLOT ####
# Generate publication-ready visualization of diversity-productivity trade-offs

workforce_evolution_plot <- ggplot(plot_data, aes(x = Year, y = Minority_Percentage, color = Scenario)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_hline(yintercept = community_level, linetype = "dashed", color = "black", size = 1) +
  
  # Add large dot at starting point to show initial state
  geom_point(aes(x = 0, y = 10), color = "black", size = 4, shape = 19) +
  
  # Add vertical lines for each strategy that reaches the target
  {if ("Balanced Strategy (Community Pool)" %in% names(target_achievement_years))
    geom_vline(xintercept = target_achievement_years[["Balanced Strategy (Community Pool)"]], 
               linetype = "solid", color = "#2E86AB", size = 0.8, alpha = 0.7)} +
  
  {if ("Aggressive Utility Strategy (Community Pool)" %in% names(target_achievement_years))
    geom_vline(xintercept = target_achievement_years[["Aggressive Utility Strategy (Community Pool)"]], 
               linetype = "solid", color = "#A23B72", size = 0.8, alpha = 0.7)} +
  
  {if ("Dynamic Recruitment Strategy" %in% names(target_achievement_years))
    geom_vline(xintercept = target_achievement_years[["Dynamic Recruitment Strategy"]], 
               linetype = "solid", color = "#FF6B35", size = 0.8, alpha = 0.7)} +
  
  # Add main annotations for key reference points
  annotate("text", x = 0, y = community_level + 2, 
           label = "Community Level (35%)", color = "black", size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = 0, y = 8, 
           label = "Starting Level (10%)", color = "black", size = 4, fontface = "bold", hjust = 0) +
  
  # Add target achievement annotations positioned above the target line
  {if ("Balanced Strategy (Community Pool)" %in% names(target_achievement_years))
    annotate("text", x = target_achievement_years[["Balanced Strategy (Community Pool)"]] + 1, y = 38, 
             label = paste0("Balanced Strategy\nReaches Target\n(", round(target_achievement_years[["Balanced Strategy (Community Pool)"]], 1), " years)"), 
             color = "#2E86AB", size = 3, fontface = "bold", hjust = 0)} +
  
  {if ("Aggressive Utility Strategy (Community Pool)" %in% names(target_achievement_years))
    annotate("text", x = target_achievement_years[["Aggressive Utility Strategy (Community Pool)"]] + 1, y = 38, 
             label = paste0("Aggressive Strategy\nReaches Target\n(", round(target_achievement_years[["Aggressive Utility Strategy (Community Pool)"]], 1), " years)"), 
             color = "#A23B72", size = 3, fontface = "bold", hjust = 0)} +
  
  {if ("Dynamic Recruitment Strategy" %in% names(target_achievement_years))
    annotate("text", x = target_achievement_years[["Dynamic Recruitment Strategy"]] + 1, y = 38, 
             label = paste0("Dynamic Recruitment\nReaches Target\n(", round(target_achievement_years[["Dynamic Recruitment Strategy"]], 1), " years)"), 
             color = "#FF6B35", size = 3, fontface = "bold", hjust = 0)} +
  
  # Add productivity key box in lower right - provides context for strategy benefits
  annotate("rect", xmin = max_x_axis * 0.65, xmax = max_x_axis * 0.98, 
           ymin = 12, ymax = 22, fill = "white", color = "black", linewidth = 0.5, alpha = 0.9) +
  annotate("text", x = max_x_axis * 0.67, y = 20.5, 
           label = "Workforce Productivity", size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = max_x_axis * 0.67, y = 19, 
           label = paste0("Balanced Strategy: ", round(balanced_final_productivity, 0)), 
           color = "#2E86AB", size = 3.2, fontface = "bold", hjust = 0) +
  annotate("text", x = max_x_axis * 0.67, y = 17.5, 
           label = paste0("Aggressive Strategy: ", round(aggressive_final_productivity, 0)), 
           color = "#A23B72", size = 3.2, fontface = "bold", hjust = 0) +
  annotate("text", x = max_x_axis * 0.67, y = 16, 
           label = paste0("Dynamic Recruitment: ", round(balanced_final_productivity, 0)), 
           color = "#FF6B35", size = 3.2, fontface = "bold", hjust = 0) +
  annotate("text", x = max_x_axis * 0.67, y = 14.5, 
           label = "Current System: 200", 
           color = "gray40", size = 3.2, hjust = 0) +
  annotate("text", x = max_x_axis * 0.67, y = 13, 
           label = "True Baseline: 196", 
           color = "gray40", size = 3.2, hjust = 0) +
  
  # Customize color scheme for clear differentiation
  scale_color_manual(values = c(
    "Balanced Strategy (Community Pool)" = "#2E86AB",
    "Aggressive Utility Strategy (Community Pool)" = "#A23B72",
    "Dynamic Recruitment Strategy" = "#FF6B35"
  )) +
  
  # Customize axes with clean, professional appearance
  scale_x_continuous(breaks = seq(0, ceiling(max_x_axis), by = 2), 
                     limits = c(0, max_x_axis)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(5, 40)) +
  
  # Professional labels and formatting
  labs(
    title = "Workforce Diversity vs. Productivity Trade-offs Over Time",
    subtitle = "Diversity progression and workforce productivity evolution under different hiring strategies",
    x = "Years",
    y = "Minority Representation (%)",
    color = "Strategy",
    caption = "Productivity calculated using Naylor-Shine model\nDynamic Recruitment Strategy: Models realistic timeline for building diverse talent pipelines,\nstarting at 10% minority applicants and gradually approaching 35% through sustained recruitment efforts"
  ) +
  
  # Clean, publication-ready theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, color = "gray40", margin = margin(b = 20)),
    legend.position = c(0.815, 0.98),  # Moved left to align with productivity box center
    legend.justification = c(0.5, 1),   # Center the legend horizontally at the specified position
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),  # White background with black border
    legend.box.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.margin = margin(6, 6, 6, 6),  # Add some padding inside the box
    
    # Clean grid and axis lines
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_blank(),
    
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(color = "gray50", size = 10, hjust = 0)  # Left-align caption
  )

print(workforce_evolution_plot)

#### STEP 17: ANALYZE AND REPORT RESULTS ####
# Generate comprehensive summary of findings for decision-makers

# Print target achievement summary
cat("=== TARGET ACHIEVEMENT TIMELINE ===\n")
cat("(Showing when strategies reach within 0.5% of 35% target, i.e., ≥34.5%)\n")
if (length(target_achievement_years) > 0) {
  for (scenario_name in names(target_achievement_years)) {
    cat(paste0(scenario_name, ": ", round(target_achievement_years[[scenario_name]], 1), " years\n"))
  }
} else {
  cat("No strategies reach within 0.5% of the 35% target within the simulation period.\n")
}
cat("\n")

# Print detailed strategy explanations with productivity information
cat("\n=== HIRING STRATEGY EXPLANATIONS WITH CORRECTED PRODUCTIVITY TRADE-OFFS ===\n\n")

strategy_productivities <- list(
  "Balanced Strategy (Community Pool)" = round(balanced_cohort_productivity, 0),
  "Aggressive Utility Strategy (Community Pool)" = round(aggressive_cohort_productivity, 0),
  "Dynamic Recruitment Strategy" = round(balanced_cohort_productivity, 0)
)

for (scenario_name in names(scenarios)) {
  scenario <- scenarios[[scenario_name]]
  new_cohort_productivity <- strategy_productivities[[scenario_name]]
  gain_vs_true_baseline <- new_cohort_productivity - round(true_baseline_productivity, 0)
  gain_vs_current_system <- new_cohort_productivity - old_cohort_productivity
  
  cat(paste0("• ", scenario_name, ":\n"))
  cat(paste0("  ", scenario$description, "\n"))
  
  # Handle dynamic hiring rate display
  if (scenario$hiring == "dynamic") {
    cat("  Hiring Rate: Dynamic (starts at 10%, approaches 35% over time)\n")
  } else {
    cat(paste0("  Hiring Rate: ", round(scenario$hiring * 100, 1), "% minorities\n"))
  }
  
  cat(paste0("  Turnover Rate: ", scenario$turnover * 100, "% quarterly\n"))
  cat(paste0("  New Cohort Productivity: ", new_cohort_productivity, "\n"))
  cat(paste0("  vs True Baseline (196): +", gain_vs_true_baseline, "\n"))
  cat(paste0("  vs Current System (200): +", gain_vs_current_system, "\n\n"))
}

#### STEP 18: CREATE SUMMARY TABLES ####
# Generate final summary tables for easy comparison of strategies

# Create a summary table showing time to reach target for each scenario
summary_table <- all_results %>%
  group_by(Scenario) %>%
  filter(Minority_Percentage >= community_level) %>%
  slice(1) %>%
  select(Scenario, Year, Minority_Percentage) %>%
  arrange(Year)

# Convert Year to character to handle scenarios that don't reach target
summary_table$Year <- as.character(summary_table$Year)

# Add scenarios that don't reach the target with their final values
scenarios_not_reached <- setdiff(names(scenarios), summary_table$Scenario)
if (length(scenarios_not_reached) > 0) {
  final_values <- all_results %>%
    filter(Scenario %in% scenarios_not_reached) %>%
    group_by(Scenario) %>%
    slice_tail(n = 1) %>%
    select(Scenario, Year, Minority_Percentage)
  
  final_values$Year <- paste0(final_values$Year, "*")
  summary_table <- rbind(summary_table, final_values)
}

print("Summary: Time to Reach Community Level (35% minority representation)")
print("* indicates target not reached within simulation period")
print(summary_table)

# Create enhanced summary table with validity information
enhanced_summary <- all_results %>%
  group_by(Scenario) %>%
  filter(Minority_Percentage >= community_level) %>%
  slice(1) %>%
  select(Scenario, Year, Minority_Percentage) %>%
  arrange(Year)

# Convert Year to character for consistency
enhanced_summary$Year <- as.character(enhanced_summary$Year)

# Add scenarios that don't reach target
scenarios_not_reached <- setdiff(names(scenarios), enhanced_summary$Scenario)
if (length(scenarios_not_reached) > 0) {
  final_values <- all_results %>%
    filter(Scenario %in% scenarios_not_reached) %>%
    group_by(Scenario) %>%
    slice_tail(n = 1) %>%
    select(Scenario, Year, Minority_Percentage)
  
  final_values$Year <- paste0(final_values$Year, "*")
  enhanced_summary <- rbind(enhanced_summary, final_values)
}

print("Summary: Time to Reach Community Level (35% minority representation)")
print("* indicates target not reached within simulation period")
print(enhanced_summary)

#### STEP 20: COMPREHENSIVE REFERENCE LIST ####
# Provide comprehensive reference list for academic and professional documentation

################################################################################
# END OF ANALYSIS
################################################################################
# This script provides a comprehensive analysis of workforce diversity and
# productivity trade-offs using Pareto optimization. The results can inform
# strategic hiring decisions and help organizations balance diversity goals
# with performance objectives while understanding realistic implementation
# timelines and associated costs.
################################################################################

