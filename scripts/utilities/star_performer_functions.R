# Star Performer Utility Analysis Functions
# Based on Sturman et al. (2023) and Joo et al. (2022)
# Implements modified SDy calculations to account for non-normal performance distributions

#' Calculate Star Performer Adjusted SDy
#' 
#' This function implements the star performer adjustment to SDy based on 
#' Sturman et al. (2023) commentary on Joo et al. (2022).
#' 
#' @param base_salary Numeric. Base salary level
#' @param star_percentage Numeric. Percentage of workforce that are star performers (0-1)
#' @param star_multiplier Numeric. Multiplier for star performer value (default 2.75 from Burke & Frederick, 1986)
#' @param method Character. Method to use: "global", "observed", "mixed"
#' @param performance_data Numeric vector. Optional performance ratings for observed method
#' @param performance_weights Numeric vector. Optional weights for performance ratings
#' 
#' @return List containing adjusted SDy values and method details
calculate_star_sdy <- function(base_salary, 
                              star_percentage = 0.05,
                              star_multiplier = 2.75,
                              method = "global",
                              performance_data = NULL,
                              performance_weights = NULL) {
  
  # Validate inputs
  if (star_percentage < 0 || star_percentage > 1) {
    stop("star_percentage must be between 0 and 1")
  }
  
  if (base_salary <= 0) {
    stop("base_salary must be positive")
  }
  
  # Traditional SDy (40% rule)
  traditional_sdy <- 0.40 * base_salary
  
  # Calculate star-adjusted SDy based on method
  if (method == "global") {
    # Global procedure from Joo et al. (2022)
    # SDy = star_multiplier * traditional_sdy = 2.75 * 0.40 * salary = 1.1 * salary
    star_sdy <- star_multiplier * traditional_sdy
    
    method_details <- list(
      description = "Global procedure using Burke & Frederick (1986) multiplier",
      multiplier = star_multiplier,
      formula = paste0(star_multiplier, " * 0.40 * salary = ", star_multiplier * 0.40, " * salary")
    )
    
  } else if (method == "observed" && !is.null(performance_data)) {
    # Observed distribution procedure
    if (is.null(performance_weights)) {
      performance_weights <- rep(1, length(performance_data))
    }
    
    # Calculate observed standard deviation
    weighted_mean <- sum(performance_data * performance_weights) / sum(performance_weights)
    weighted_var <- sum(performance_weights * (performance_data - weighted_mean)^2) / sum(performance_weights)
    observed_sd <- sqrt(weighted_var)
    
    # Calculate theoretical SD for comparison (assuming normal distribution)
    theoretical_sd <- sd(performance_data)
    
    # Ratio of observed to theoretical
    sd_ratio <- observed_sd / theoretical_sd
    
    # Adjust SDy based on observed distribution
    star_sdy <- sd_ratio * traditional_sdy * star_multiplier
    
    method_details <- list(
      description = "Observed distribution procedure",
      observed_sd = observed_sd,
      theoretical_sd = theoretical_sd,
      sd_ratio = sd_ratio,
      adjustment_factor = sd_ratio * star_multiplier
    )
    
  } else if (method == "mixed") {
    # Mixed approach: Apply star multiplier only to top performers
    regular_sdy <- traditional_sdy
    star_sdy_value <- star_multiplier * traditional_sdy
    
    # Weighted average based on star percentage
    star_sdy <- (1 - star_percentage) * regular_sdy + star_percentage * star_sdy_value
    
    method_details <- list(
      description = "Mixed approach with differential SDy by performance level",
      regular_sdy = regular_sdy,
      star_sdy_value = star_sdy_value,
      star_percentage = star_percentage,
      weighted_average = star_sdy
    )
    
  } else {
    stop("Invalid method or missing performance_data for observed method")
  }
  
  return(list(
    traditional_sdy = traditional_sdy,
    star_adjusted_sdy = star_sdy,
    adjustment_ratio = star_sdy / traditional_sdy,
    star_percentage = star_percentage,
    method = method,
    method_details = method_details
  ))
}

#' Apply Star Performer Analysis to Utility Calculation
#' 
#' @param n Numeric. Number of hires
#' @param validity Numeric. Validity coefficient
#' @param selection_ratio Numeric. Selection ratio
#' @param base_salary Numeric. Base salary
#' @param cost_per_applicant Numeric. Cost per applicant
#' @param star_percentage Numeric. Percentage of star performers
#' @param star_method Character. Star adjustment method
#' @param time_horizon Numeric. Time horizon in years (default 1)
#' 
#' @return List with traditional and star-adjusted utility estimates
star_utility_analysis <- function(n, validity, selection_ratio, base_salary, cost_per_applicant,
                                 star_percentage = 0.05, star_method = "global", time_horizon = 1) {
  
  # Calculate traditional utility
  traditional_sdy_result <- calculate_star_sdy(base_salary, method = "global", star_multiplier = 1)
  traditional_sdy <- traditional_sdy_result$traditional_sdy
  
  # Calculate ordinate for selection ratio
  ordinate <- dnorm(qnorm(1 - selection_ratio))
  
  # Traditional utility calculation
  traditional_utility <- n * traditional_sdy * validity * (ordinate / selection_ratio) * time_horizon - 
                        (n / selection_ratio) * cost_per_applicant
  
  # Calculate star-adjusted utility
  star_sdy_result <- calculate_star_sdy(base_salary, star_percentage, method = star_method)
  star_sdy <- star_sdy_result$star_adjusted_sdy
  
  # Star-adjusted utility calculation
  star_utility <- n * star_sdy * validity * (ordinate / selection_ratio) * time_horizon - 
                  (n / selection_ratio) * cost_per_applicant
  
  # Calculate improvement
  utility_improvement <- star_utility - traditional_utility
  percentage_improvement <- (utility_improvement / abs(traditional_utility)) * 100
  
  return(list(
    traditional_utility = traditional_utility,
    star_utility = star_utility,
    utility_improvement = utility_improvement,
    percentage_improvement = percentage_improvement,
    traditional_sdy = traditional_sdy,
    star_sdy = star_sdy,
    sdy_adjustment_ratio = star_sdy_result$adjustment_ratio,
    star_percentage = star_percentage,
    method = star_method,
    parameters = list(
      n = n,
      validity = validity,
      selection_ratio = selection_ratio,
      base_salary = base_salary,
      cost_per_applicant = cost_per_applicant,
      time_horizon = time_horizon
    )
  ))
}

#' Calculate Star Performer SDy using Joo et al. (2022) Global Procedure
#' 
#' Based on Sturman et al. (2023) footnote 1:
#' "their estimate from the global procedure ultimately equals 
#' (2.75 * 0.4 * mean salary =) 1.1 * mean salary"
#' 
#' @param mean_salary Numeric. Mean salary level
#' @return List with traditional and star performer SDy values
calculate_star_performer_sdy <- function(mean_salary) {
  
  # Traditional SDy using 40% rule
  traditional_sdy <- 0.40 * mean_salary
  
  # Star performer SDy using Joo et al. (2022) global procedure
  # Based on Burke & Frederick (1986) ratio of 2.75
  star_performer_sdy <- 1.1 * mean_salary
  
  # Calculate the multiplier effect
  multiplier <- star_performer_sdy / traditional_sdy  # Should be 2.75
  
  return(list(
    traditional_sdy = traditional_sdy,
    star_performer_sdy = star_performer_sdy,
    multiplier = multiplier,
    formula_traditional = "0.40 * mean_salary",
    formula_star = "1.1 * mean_salary",
    source = "Joo et al. (2022) global procedure via Sturman et al. (2023) footnote 1"
  ))
}

#' General Star Performer Utility Analysis
#' 
#' Applies star performer SDy adjustment to any utility analysis scenario
#' 
#' @param n_selected Numeric. Number of people selected/hired
#' @param validity Numeric. Validity coefficient of selection procedure
#' @param selection_ratio Numeric. Selection ratio (proportion hired)
#' @param mean_salary Numeric. Mean salary level
#' @param cost_per_applicant Numeric. Cost per applicant
#' @param time_horizon Numeric. Time horizon in years (default 1)
#' @param scenario_name Character. Name for the scenario (optional)
#' 
#' @return List with complete analysis comparing traditional vs star performer utility
general_star_performer_analysis <- function(n_selected, validity, selection_ratio, 
                                           mean_salary, cost_per_applicant, 
                                           time_horizon = 1, scenario_name = "Custom Analysis") {
  
  # Calculate number of applicants
  n_applicants <- n_selected / selection_ratio
  
  # Calculate SDy values
  sdy_results <- calculate_star_performer_sdy(mean_salary)
  
  # Calculate ordinate (height of normal curve at selection ratio cutoff)
  z_score <- qnorm(1 - selection_ratio)  # z-score for selection cutoff
  ordinate <- dnorm(z_score)             # Height of normal curve at that point
  
  # Traditional utility calculation
  # U = Ns * SDy * r * (φ/p) * T - Na * Ca
  traditional_utility <- n_selected * sdy_results$traditional_sdy * validity * 
                        (ordinate / selection_ratio) * time_horizon - 
                        n_applicants * cost_per_applicant
  
  # Star performer utility calculation
  star_utility <- n_selected * sdy_results$star_performer_sdy * validity * 
                 (ordinate / selection_ratio) * time_horizon - 
                 n_applicants * cost_per_applicant
  
  # Calculate improvements
  utility_improvement <- star_utility - traditional_utility
  percentage_improvement <- (utility_improvement / abs(traditional_utility)) * 100
  
  # Per-hire calculations
  traditional_per_hire <- traditional_utility / n_selected
  star_per_hire <- star_utility / n_selected
  per_hire_improvement <- star_per_hire - traditional_per_hire
  
  return(list(
    # Scenario information
    scenario_name = scenario_name,
    
    # Parameters
    parameters = list(
      n_selected = n_selected,
      n_applicants = n_applicants,
      validity = validity,
      selection_ratio = selection_ratio,
      mean_salary = mean_salary,
      cost_per_applicant = cost_per_applicant,
      time_horizon = time_horizon,
      z_score = z_score,
      ordinate = ordinate
    ),
    
    # SDy analysis
    sdy_analysis = sdy_results,
    
    # Utility results
    traditional_utility = traditional_utility,
    star_utility = star_utility,
    utility_improvement = utility_improvement,
    percentage_improvement = percentage_improvement,
    
    # Per-hire analysis
    traditional_per_hire = traditional_per_hire,
    star_per_hire = star_per_hire,
    per_hire_improvement = per_hire_improvement,
    per_hire_percentage_improvement = (per_hire_improvement / abs(traditional_per_hire)) * 100
  ))
}

#' Apply Star Performer Analysis to Latham & Whyte Example
#' 
#' Applies the star performer SDy adjustment to the classic Latham & Whyte case
#' 
#' @return List with complete analysis comparing traditional vs star performer utility
latham_whyte_star_analysis <- function() {
  
  # Original Latham & Whyte (1994) parameters
  n_selected <- 618        # Number of people selected
  validity <- 0.76         # Validity coefficient of the selection test
  selection_ratio <- 0.05  # Selection ratio (5% of applicants hired)
  mean_salary <- 29000     # Mean salary (1980s dollars)
  cost_per_applicant <- 10 # Cost per applicant
  time_horizon <- 10       # Time horizon (10 years)
  
  # Use general function
  results <- general_star_performer_analysis(
    n_selected = n_selected,
    validity = validity,
    selection_ratio = selection_ratio,
    mean_salary = mean_salary,
    cost_per_applicant = cost_per_applicant,
    time_horizon = time_horizon,
    scenario_name = "Latham & Whyte (1994)"
  )
  
  # Add specific context
  results$case_study <- "Latham & Whyte (1994)"
  results$context <- "Budget analyst selection in government agency"
  
  return(results)
}

#' Format Currency for Display
#' 
#' @param amount Numeric value to format
#' @param currency Character. Currency symbol (default "$")
#' @return Character. Formatted currency string
format_currency <- function(amount, currency = "$") {
  paste0(currency, formatC(round(amount), format = "d", big.mark = ","))
}

#' Generate Star Performer Analysis Report
#' 
#' @param analysis_results List from latham_whyte_star_analysis()
#' @return Character vector with formatted report
generate_star_performer_report <- function(analysis_results) {
  
  # Format key numbers
  trad_utility <- format_currency(analysis_results$traditional_utility)
  star_utility <- format_currency(analysis_results$star_utility)
  improvement <- format_currency(analysis_results$utility_improvement)
  
  trad_per_hire <- format_currency(analysis_results$traditional_per_hire)
  star_per_hire <- format_currency(analysis_results$star_per_hire)
  per_hire_imp <- format_currency(analysis_results$per_hire_improvement)
  
  trad_sdy <- format_currency(analysis_results$sdy_analysis$traditional_sdy)
  star_sdy <- format_currency(analysis_results$sdy_analysis$star_performer_sdy)
  
  report <- c(
    "===============================================",
    "STAR PERFORMER UTILITY ANALYSIS",
    paste("Scenario:", analysis_results$scenario_name),
    "===============================================",
    "",
    if (!is.null(analysis_results$context)) {
      c("BACKGROUND:",
        paste("Context:", analysis_results$context),
        "")
    } else NULL,
    "PARAMETERS:",
    paste("• Number selected:", analysis_results$parameters$n_selected),
    paste("• Number of applicants:", format_currency(analysis_results$parameters$n_applicants, "")),
    paste("• Selection ratio:", paste0(analysis_results$parameters$selection_ratio * 100, "%")),
    paste("• Validity coefficient:", analysis_results$parameters$validity),
    paste("• Mean salary:", format_currency(analysis_results$parameters$mean_salary)),
    paste("• Cost per applicant:", format_currency(analysis_results$parameters$cost_per_applicant)),
    paste("• Time horizon:", paste0(analysis_results$parameters$time_horizon, " years")),
    "",
    "SDy COMPARISON:",
    paste("• Traditional SDy (40% rule):", trad_sdy),
    paste("• Star Performer SDy (1.1 × salary):", star_sdy),
    paste("• Multiplier effect:", paste0(round(analysis_results$sdy_analysis$multiplier, 2), "x")),
    "",
    "UTILITY ANALYSIS RESULTS:",
    paste("• Traditional utility:", trad_utility),
    paste("• Star performer utility:", star_utility),
    paste("• Improvement:", improvement),
    paste("• Percentage improvement:", paste0(round(analysis_results$percentage_improvement, 1), "%")),
    "",
    "PER-HIRE ANALYSIS:",
    paste("• Traditional value per hire:", trad_per_hire),
    paste("• Star performer value per hire:", star_per_hire),
    paste("• Per-hire improvement:", per_hire_imp),
    paste("• Per-hire % improvement:", paste0(round(analysis_results$per_hire_percentage_improvement, 1), "%")),
    "",
    "KEY INSIGHTS:",
    paste("1. The star performer adjustment increases total utility by", 
          format_currency(analysis_results$utility_improvement)),
    paste("2. This represents a", paste0(round(analysis_results$percentage_improvement, 1), "%"), 
          "improvement over traditional estimates"),
    paste("3. Each hire is worth an additional", per_hire_imp, "when accounting for star performers"),
    "4. This reflects the disproportionate value created by top performers in non-normal distributions",
    "",
    "METHODOLOGY:",
    paste("• Traditional SDy formula:", analysis_results$sdy_analysis$formula_traditional),
    paste("• Star performer SDy formula:", analysis_results$sdy_analysis$formula_star),
    paste("• Source:", analysis_results$sdy_analysis$source),
    "",
    paste("Report generated:", Sys.Date()),
    "==============================================="
  )
  
  return(report[!is.null(report)])
}

#' Print Star Performer Analysis
#' 
#' Convenience function to run analysis and print results
print_latham_whyte_star_analysis <- function() {
  results <- latham_whyte_star_analysis()
  report <- generate_star_performer_report(results)
  cat(paste(report, collapse = "\n"))
  invisible(results)
}

#' Print General Star Performer Analysis
#' 
#' Convenience function to run general analysis and print results
print_star_performer_analysis <- function(n_selected, validity, selection_ratio, 
                                         mean_salary, cost_per_applicant, 
                                         time_horizon = 1, scenario_name = "Custom Analysis") {
  results <- general_star_performer_analysis(
    n_selected = n_selected,
    validity = validity,
    selection_ratio = selection_ratio,
    mean_salary = mean_salary,
    cost_per_applicant = cost_per_applicant,
    time_horizon = time_horizon,
    scenario_name = scenario_name
  )
  report <- generate_star_performer_report(results)
  cat(paste(report, collapse = "\n"))
  invisible(results)
} 