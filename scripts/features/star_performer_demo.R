# Star Performer Utility Analysis Demonstration
# Shows the impact of using star performer SDy vs traditional SDy

# Load the star performer functions
source("scripts/utilities/star_performer_functions.R")

# Function to create comparison table
create_comparison_table <- function() {
  
  # Define scenarios
  scenarios <- list(
    list(name = "Latham & Whyte (1994)", n = 618, validity = 0.76, sr = 0.05, salary = 29000, cost = 10, years = 10),
    list(name = "Tech Company Hiring", n = 100, validity = 0.50, sr = 0.10, salary = 75000, cost = 500, years = 5),
    list(name = "Sales Team Selection", n = 50, validity = 0.60, sr = 0.15, salary = 60000, cost = 200, years = 3),
    list(name = "Executive Search", n = 10, validity = 0.40, sr = 0.02, salary = 150000, cost = 5000, years = 7),
    list(name = "Entry Level Hiring", n = 200, validity = 0.30, sr = 0.25, salary = 35000, cost = 100, years = 2)
  )
  
  # Create results table
  results <- data.frame(
    Scenario = character(),
    Traditional_Utility = numeric(),
    Star_Utility = numeric(),
    Improvement = numeric(),
    Percent_Improvement = numeric(),
    Traditional_SDy = numeric(),
    Star_SDy = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (scenario in scenarios) {
    analysis <- general_star_performer_analysis(
      n_selected = scenario$n,
      validity = scenario$validity,
      selection_ratio = scenario$sr,
      mean_salary = scenario$salary,
      cost_per_applicant = scenario$cost,
      time_horizon = scenario$years,
      scenario_name = scenario$name
    )
    
    results <- rbind(results, data.frame(
      Scenario = scenario$name,
      Traditional_Utility = analysis$traditional_utility,
      Star_Utility = analysis$star_utility,
      Improvement = analysis$utility_improvement,
      Percent_Improvement = analysis$percentage_improvement,
      Traditional_SDy = analysis$sdy_analysis$traditional_sdy,
      Star_SDy = analysis$sdy_analysis$star_performer_sdy,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

# Function to print formatted comparison
print_comparison_summary <- function() {
  cat("===============================================\n")
  cat("STAR PERFORMER UTILITY ANALYSIS COMPARISON\n")
  cat("Multiple Scenarios Demonstration\n")
  cat("===============================================\n\n")
  
  results <- create_comparison_table()
  
  for (i in 1:nrow(results)) {
    cat(paste("SCENARIO", i, ":", results$Scenario[i], "\n"))
    cat(paste("• Traditional Utility:", format_currency(results$Traditional_Utility[i]), "\n"))
    cat(paste("• Star Performer Utility:", format_currency(results$Star_Utility[i]), "\n"))
    cat(paste("• Improvement:", format_currency(results$Improvement[i]), 
              paste0("(", round(results$Percent_Improvement[i], 1), "%)\n")))
    cat(paste("• SDy Increase:", format_currency(results$Traditional_SDy[i]), "→", 
              format_currency(results$Star_SDy[i]), "\n\n"))
  }
  
  # Summary statistics
  avg_improvement <- mean(results$Percent_Improvement)
  min_improvement <- min(results$Percent_Improvement)
  max_improvement <- max(results$Percent_Improvement)
  
  cat("SUMMARY STATISTICS:\n")
  cat(paste("• Average improvement:", round(avg_improvement, 1), "%\n"))
  cat(paste("• Range:", round(min_improvement, 1), "% to", round(max_improvement, 1), "%\n"))
  cat(paste("• SDy multiplier: 2.75x (consistent across all scenarios)\n"))
  cat(paste("• Methodology: Joo et al. (2022) global procedure\n\n"))
  
  cat("KEY TAKEAWAY:\n")
  cat("Star performer adjustments consistently increase utility estimates\n")
  cat("by approximately 175-182%, reflecting the disproportionate value\n")
  cat("contribution of top performers in non-normal distributions.\n")
  cat("===============================================\n")
}

# Function to demonstrate the mathematical relationship
demonstrate_sdy_relationship <- function() {
  cat("===============================================\n")
  cat("SDy CALCULATION METHODOLOGY\n")
  cat("===============================================\n\n")
  
  # Example salary
  salary <- 50000
  sdy_calc <- calculate_star_performer_sdy(salary)
  
  cat("TRADITIONAL APPROACH:\n")
  cat(paste("• Formula:", sdy_calc$formula_traditional, "\n"))
  cat(paste("• Example: 0.40 × $50,000 =", format_currency(sdy_calc$traditional_sdy), "\n\n"))
  
  cat("STAR PERFORMER APPROACH:\n")
  cat(paste("• Formula:", sdy_calc$formula_star, "\n"))
  cat(paste("• Example: 1.1 × $50,000 =", format_currency(sdy_calc$star_performer_sdy), "\n"))
  cat(paste("• Derivation: 2.75 × 0.40 × $50,000 =", format_currency(sdy_calc$star_performer_sdy), "\n\n"))
  
  cat("THEORETICAL BASIS:\n")
  cat("• Burke & Frederick (1986): SDy/SDO ratio = 2.75\n")
  cat("• Joo et al. (2022): Global procedure for non-normal distributions\n")
  cat("• Sturman et al. (2023): 2.75 × 0.4 × salary = 1.1 × salary\n\n")
  
  cat("MULTIPLIER EFFECT:\n")
  cat(paste("• Star SDy is", round(sdy_calc$multiplier, 2), "times larger than traditional SDy\n"))
  cat("• This reflects disproportionate value of star performers\n")
  cat("• Accounts for heavy-tailed (non-normal) performance distributions\n")
  cat("===============================================\n")
}

# Run demonstrations if script is executed directly
if (interactive() || identical(environment(), globalenv())) {
  
  cat("STAR PERFORMER UTILITY ANALYSIS DEMONSTRATION\n")
  cat("==============================================\n\n")
  
  # 1. Show the Latham & Whyte example
  cat("1. CLASSIC EXAMPLE: Latham & Whyte (1994)\n")
  cat("------------------------------------------\n")
  print_latham_whyte_star_analysis()
  cat("\n\n")
  
  # 2. Show the SDy methodology
  cat("2. METHODOLOGY EXPLANATION\n")
  cat("---------------------------\n")
  demonstrate_sdy_relationship()
  cat("\n\n")
  
  # 3. Show multiple scenarios
  cat("3. MULTIPLE SCENARIO COMPARISON\n")
  cat("-------------------------------\n")
  print_comparison_summary()
  
} 