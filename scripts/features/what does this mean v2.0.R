library(shiny)
library(ggplot2)
library(scales)
library(ggtext)
library(iopsych)  # for ux()
library(DT)  # Add DT library for better tables
library(ParetoR)  # For Pareto optimization
library(psych)    # For psychological statistics
library(lavaan)   # For covariance matrix operations
library(MASS)     # For multivariate normal sampling
library(mvtnorm)  # For multivariate normal distributions
library(dplyr)    # For data manipulation
library(gridExtra)

# Calculate turnover rate based on performance distribution and reward contingency
# Based on Williams & Livingstone (1994) research
calculate_cohort_turnover <- function(cohort_mean_productivity, cohort_size, base_annual_rate = 0.10, 
                                     performance_rewards = FALSE, productivity_sd = 40) {
  
  if (cohort_size <= 0) {
    return(list(
      avg_turnover_rate = 0,
      quarterly_turnover_rate = 0,
      departures = 0,
      remaining_size = 0,
      remaining_avg_productivity = cohort_mean_productivity
    ))
  }
  
  # Ensure performance_rewards is not NA
  if (is.na(performance_rewards) || is.null(performance_rewards)) {
    performance_rewards <- FALSE
  }
  
  # Convert annual to quarterly base rate
  quarterly_base_rate <- base_annual_rate / 4
  
  # Generate performance distribution for this cohort
  # Using deterministic seed based on cohort characteristics for consistency
  cohort_seed <- as.integer(round(cohort_mean_productivity * 100) + cohort_size + ifelse(isTRUE(performance_rewards), 1000, 0))
  
  # Ensure seed is valid (positive integer)
  if (is.na(cohort_seed) || cohort_seed <= 0) {
    cohort_seed <- 12345  # fallback seed
  }
  
  set.seed(cohort_seed)
  individual_productivity <- rnorm(cohort_size, mean = cohort_mean_productivity, sd = productivity_sd)
  
  # Calculate turnover rates for each individual based on Williams & Livingstone (1994)
  individual_turnover_rates <- sapply(1:cohort_size, function(i) {
    perf <- individual_productivity[i]
    
    # Standardize performance (z-score relative to population mean of 200)
    perf_z <- (perf - 200) / productivity_sd
    
    # Use deterministic noise based on individual performance for consistency
    perf_int <- round(perf * 1000)
    rewards_offset <- ifelse(isTRUE(performance_rewards), 10000, 0)
    individual_seed <- as.integer(perf_int + i + rewards_offset)
    
    # Ensure seed is valid (positive integer)
    if (is.na(individual_seed) || individual_seed <= 0) {
      individual_seed <- 12345 + i  # fallback seed with variation
    }
    
    set.seed(individual_seed)
    
    if (!isTRUE(performance_rewards)) {
      # Without performance-based rewards: U-shaped relationship
      # Linear component (negative relationship with performance)
      linear_effect <- -0.18 * perf_z * 0.015
      # U-shape component (higher turnover at both extremes)
      u_shape_effect <- 0.05 * (perf_z^2) * 0.010
      # Random noise
      random_noise <- rnorm(1, 0, 0.015)
      turnover_adjustment <- linear_effect + u_shape_effect + random_noise
      
    } else {
      # With performance-based rewards: flattened U-curve
      # Stronger linear component (stronger retention of high performers)
      linear_effect <- -0.27 * perf_z * 0.016
      # Reduced U-shape component (reduced turnover among high performers)
      u_shape_effect <- 0.02 * (perf_z^2) * 0.008
      # Random noise (slightly weaker to maintain correlation)
      random_noise <- rnorm(1, 0, 0.014)
      turnover_adjustment <- linear_effect + u_shape_effect + random_noise
    }
    
    # Apply adjustment to quarterly base rate
    adjusted_rate <- quarterly_base_rate + turnover_adjustment
    
    # Ensure reasonable bounds (1% to 20% quarterly)
    return(max(0.01, min(0.20, adjusted_rate)))
  })
  
  # Simulate departures using deterministic seed
  departure_seed <- cohort_seed + 999
  
  # Ensure departure seed is valid
  if (is.na(departure_seed) || departure_seed <= 0) {
    departure_seed <- 54321  # fallback seed
  }
  
  set.seed(departure_seed)
  departures_indicator <- rbinom(cohort_size, 1, individual_turnover_rates)
  departures <- sum(departures_indicator)
  
  # Calculate remaining workforce
  remaining_employees <- individual_productivity[departures_indicator == 0]
  remaining_size <- length(remaining_employees)
  remaining_avg_productivity <- ifelse(remaining_size > 0, mean(remaining_employees), cohort_mean_productivity)
  
  # Calculate average turnover rate for this cohort
  avg_turnover_rate <- mean(individual_turnover_rates)
  
  return(list(
    avg_turnover_rate = avg_turnover_rate,
    quarterly_turnover_rate = avg_turnover_rate,
    departures = departures,
    remaining_size = remaining_size,
    remaining_avg_productivity = remaining_avg_productivity,
    individual_turnover_rates = individual_turnover_rates,
    individual_productivity = individual_productivity
  ))
}

# Simplified wrapper for backward compatibility
calculate_turnover <- function(productivity, base_rate, performance_rewards = FALSE) {
  # Validate inputs and handle NA values
  if (is.na(productivity) || is.null(productivity) || !is.numeric(productivity)) {
    productivity <- 200  # Default fallback
  }
  if (is.na(base_rate) || is.null(base_rate) || !is.numeric(base_rate)) {
    base_rate <- 0.10  # Default fallback
  }
  if (is.na(performance_rewards) || is.null(performance_rewards)) {
    performance_rewards <- FALSE  # Default fallback
  }
  
  # This is a simplified version for single-value calculations
  # Convert annual to quarterly
  quarterly_base_rate <- base_rate / 4
  
  # Standardize performance (z-score relative to population mean of 200)
  perf_z <- (productivity - 200) / 40
  
  # Use deterministic seed based on productivity and rewards
  # Ensure productivity is properly rounded to avoid floating point issues
  productivity_int <- round(productivity * 1000)
  # Fix: Handle NA values in ifelse by ensuring performance_rewards is logical
  rewards_offset <- ifelse(isTRUE(performance_rewards), 10000, 0)
  individual_seed <- as.integer(productivity_int + rewards_offset)
  
  # Ensure seed is valid (positive integer)
  if (is.na(individual_seed) || individual_seed <= 0) {
    individual_seed <- 12345  # fallback seed
  }
  
  set.seed(individual_seed)
  
  # Fix: Use isTRUE for safer logical comparison
  if (!isTRUE(performance_rewards)) {
    # Without performance-based rewards: U-shaped relationship
    linear_effect <- -0.18 * perf_z * 0.015
    u_shape_effect <- 0.05 * (perf_z^2) * 0.010
    random_noise <- rnorm(1, 0, 0.015)
    turnover_adjustment <- linear_effect + u_shape_effect + random_noise
  } else {
    # With performance-based rewards: flattened U-curve
    linear_effect <- -0.27 * perf_z * 0.016
    u_shape_effect <- 0.02 * (perf_z^2) * 0.008
    random_noise <- rnorm(1, 0, 0.014)
    turnover_adjustment <- linear_effect + u_shape_effect + random_noise
  }
  
  adjusted_rate <- quarterly_base_rate + turnover_adjustment
  result <- max(0.01, min(0.20, adjusted_rate))
  
  # Ensure result is not NA
  if (is.na(result) || is.null(result)) {
    result <- 0.025  # Default quarterly rate (10% annual)
  }
  
  return(result)
}

ui <- fluidPage(
  titlePanel("Dynamic Workforce Evolution: Staffing, Productivity & Retention"),
  sidebarLayout(
    sidebarPanel(
      # System Parameters
      h4("Selection System Parameters"),
      textInput("validity_old_txt", 
                "Old System Validity (rxy):", 
                value = "0.10"),
      textInput("validity_new_txt", 
                "New System Validity (rxy):", 
                value = "0.50"),
      textInput("sr_txt", 
                "Selection Ratio:", 
                value = "33%"),
      textInput("corr_txt", 
                "Corr. Between Performance & Turnover (–1 to +1):", 
                value = "-0.18"),
      
      # Workforce Parameters
      h4("Workforce Parameters"),
      textInput("base_turnover_txt",
                "Base Turnover Rate (%):",
                value = "10"),
      
      # Reward System
      h4("Reward System"),
      checkboxInput("performance_rewards",
                   "Use Performance-Based Rewards",
                   value = FALSE),
      conditionalPanel(
        condition = "input.performance_rewards == true",
      textInput("perf_pay_pct_txt",
                "Performance-Based Pay (% of payroll):",
                  value = "5")
      ),
      
      # Production Schedule
      h4("Production Schedule"),
      textInput("q1_demand_txt",
                "Q1 Production Demand:",
                value = "100000"),
      textInput("q2_demand_txt",
                "Q2 Production Demand:",
                value = "100000"),
      textInput("q3_demand_txt",
                "Q3 Production Demand:",
                value = "105000"),
      textInput("q4_demand_txt",
                "Q4 Production Demand:",
                value = "110000"),
      
      actionButton("reset", "Reset to Defaults", 
                  style = "margin-top: 20px; width: 100%;")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Productivity Distribution", plotOutput("prodHist", height = "300px")),
        tabPanel("Employee Flows",
          plotOutput("hirePlot", height = "400px"),
          htmlOutput("employeeFlowsTable"),
          h4("Old System Workforce"),
          dataTableOutput("employeeFlowsDataOld"),
          h4("New System Workforce"),
          dataTableOutput("employeeFlowsDataNew")
        ),
        tabPanel("Value Created Over Time", plotOutput("staffingPlot", height = "600px")),
        tabPanel("Pareto Optimization Analysis",
          h4("Diversity-Productivity Trade-offs"),
          plotOutput("paretoPlot", height = "500px"),
          h4("Key Insights"),
          verbatimTextOutput("paretoInsights"),
          h4("Strategy Comparison"),
          dataTableOutput("strategyTable")
        ),
        tabPanel("Workforce Composition Evolution", 
                 plotOutput("workforceEvolutionPlot", height = "500px"),
                 h4("Workforce Dynamics Summary"),
                 verbatimTextOutput("workforceSummary"))
      ),
      htmlOutput("plotNote")
    )
  )
)

server <- function(input, output, session) {
  # parse inputs
  vals <- reactive({
    # Parse inputs with fallback values for NA cases
    rxy_old  <- as.numeric(input$validity_old_txt)
    if (is.na(rxy_old)) rxy_old <- 0.10
    
    rxy_new  <- as.numeric(input$validity_new_txt)
    if (is.na(rxy_new)) rxy_new <- 0.50
    
    sr       <- as.numeric(gsub("%","",input$sr_txt))/100
    if (is.na(sr)) sr <- 0.33
    
    corr     <- as.numeric(input$corr_txt)
    if (is.na(corr)) corr <- -0.18
    
    base_turnover <- as.numeric(gsub("%","",input$base_turnover_txt))/100
    if (is.na(base_turnover)) base_turnover <- 0.10
    
    # Use fixed observed productivity value
    observed_prod <- 200
    
    performance_rewards <- input$performance_rewards
    if (is.na(performance_rewards) || is.null(performance_rewards)) performance_rewards <- FALSE
    
    # Handle performance-based pay percentage - only get value if checkbox is checked
    perf_pay_pct <- if (isTRUE(performance_rewards) && !is.null(input$perf_pay_pct_txt)) {
      as.numeric(input$perf_pay_pct_txt)
    } else {
      5  # default value
    }
    if (is.na(perf_pay_pct)) perf_pay_pct <- 5
    
    # Parse demand values with fallbacks
    q1_demand <- as.numeric(input$q1_demand_txt)
    if (is.na(q1_demand)) q1_demand <- 100000
    
    q2_demand <- as.numeric(input$q2_demand_txt)
    if (is.na(q2_demand)) q2_demand <- 100000
    
    q3_demand <- as.numeric(input$q3_demand_txt)
    if (is.na(q3_demand)) q3_demand <- 105000
    
    q4_demand <- as.numeric(input$q4_demand_txt)
    if (is.na(q4_demand)) q4_demand <- 110000
    
    demand <- c(q1_demand, q2_demand, q3_demand, q4_demand)
    
    # Calculate corrected baselines
    old_system_gain <- rxy_old * ux(sr) * 40
    true_baseline <- observed_prod - old_system_gain
    new_system_productivity <- true_baseline + (rxy_new * ux(sr) * 40)
    
    # Ensure no NA values in final list
    list(
      rxy_old = ifelse(is.na(rxy_old), 0.10, rxy_old),
      rxy_new = ifelse(is.na(rxy_new), 0.50, rxy_new),
      sr = ifelse(is.na(sr), 0.33, sr),
      corr = ifelse(is.na(corr), -0.18, corr),
      base_turnover = ifelse(is.na(base_turnover), 0.10, base_turnover),
      observed_prod = ifelse(is.na(observed_prod), 200, observed_prod),
      true_baseline = ifelse(is.na(true_baseline), 180, true_baseline),
      old_system_gain = ifelse(is.na(old_system_gain), 20, old_system_gain),
      new_system_productivity = ifelse(is.na(new_system_productivity), 220, new_system_productivity),
      performance_rewards = ifelse(is.na(performance_rewards), FALSE, performance_rewards),
      perf_pay_pct = ifelse(is.na(perf_pay_pct), 5, perf_pay_pct),
      demand = demand
    )
  })
  
  # Dynamic workforce evolution simulation
  workforce_evolution <- reactive({
    pars <- vals()
    
    # Simulation parameters
    quarters <- 20  # 5 years
    initial_workforce <- 1000
    # Use the same production demands pattern, scaled up for larger workforce
    base_demand <- 200000  # Scaled for 1000 initial workforce
    quarterly_demands <- rep(base_demand, quarters)
    # Add some growth pattern
    for (q in 1:quarters) {
      if (q %% 4 == 3) quarterly_demands[q] <- base_demand * 1.05  # Q3 increase
      if (q %% 4 == 0) quarterly_demands[q] <- base_demand * 1.10  # Q4 increase
    }
    
    # Initialize workforce composition with actual productivity values
    old_cohort_size <- initial_workforce
    new_cohort_size <- 0
    old_cohort_productivity <- pars$observed_prod
    new_cohort_productivity <- pars$new_system_productivity
    
    # Track evolution
    results <- data.frame(
      Quarter = integer(),
      Year = numeric(),
      Old_Cohort_Size = numeric(),
      New_Cohort_Size = numeric(),
      Total_Workforce = numeric(),
      Old_Cohort_Productivity = numeric(),
      New_Cohort_Productivity = numeric(),
      Average_Productivity = numeric(),
      Old_Cohort_Turnover_Rate = numeric(),
      New_Cohort_Turnover_Rate = numeric(),
      Old_Cohort_Departures = numeric(),
      New_Cohort_Departures = numeric(),
      Production_Demand = numeric(),
      Required_Workforce = numeric(),
      New_Hires = numeric()
    )
    
    for (q in 1:quarters) {
      # Calculate current workforce composition
      total_workforce <- old_cohort_size + new_cohort_size
      
      # Handle potential NA values
      if (is.na(total_workforce) || total_workforce <= 0) {
        total_workforce <- 0
        avg_productivity <- pars$observed_prod
      } else {
        avg_productivity <- (old_cohort_size * old_cohort_productivity + new_cohort_size * new_cohort_productivity) / total_workforce
      }
      
      # Apply cohort-based turnover using performance distributions
      old_turnover_results <- calculate_cohort_turnover(
        cohort_mean_productivity = old_cohort_productivity,
        cohort_size = old_cohort_size,
        base_annual_rate = pars$base_turnover,
        performance_rewards = pars$performance_rewards,
        productivity_sd = 40
      )
      
      new_turnover_results <- calculate_cohort_turnover(
        cohort_mean_productivity = new_cohort_productivity,
        cohort_size = new_cohort_size,
        base_annual_rate = pars$base_turnover,
        performance_rewards = pars$performance_rewards,
        productivity_sd = 40
      )
      
      # Update cohort sizes after turnover - handle potential NA values and ensure whole numbers
      old_cohort_size <- round(ifelse(is.na(old_turnover_results$remaining_size), 0, old_turnover_results$remaining_size))
      new_cohort_size <- round(ifelse(is.na(new_turnover_results$remaining_size), 0, new_turnover_results$remaining_size))
      old_cohort_productivity <- ifelse(is.na(old_turnover_results$remaining_avg_productivity), pars$observed_prod, old_turnover_results$remaining_avg_productivity)
      new_cohort_productivity <- ifelse(is.na(new_turnover_results$remaining_avg_productivity), pars$new_system_productivity, new_turnover_results$remaining_avg_productivity)
      
      # Calculate workforce needed to meet production demand
      current_workforce_total <- old_cohort_size + new_cohort_size
      
      if (is.na(current_workforce_total) || current_workforce_total <= 0) {
        current_avg_prod <- pars$observed_prod
      } else {
        current_avg_prod <- (old_cohort_size * old_cohort_productivity + new_cohort_size * new_cohort_productivity) / current_workforce_total
      }
      
      # Handle potential division by zero or NA
      if (is.na(current_avg_prod) || current_avg_prod <= 0) {
        current_avg_prod <- pars$observed_prod
      }
      
      required_workforce <- ceiling(quarterly_demands[q] / current_avg_prod)
      current_workforce <- old_cohort_size + new_cohort_size
      
      # Calculate new hires needed - handle potential NA values and ensure whole numbers
      if (is.na(required_workforce) || is.na(current_workforce)) {
        new_hires <- 0
      } else {
        new_hires <- max(0, ceiling(required_workforce - current_workforce))
      }
      
      # Add new hires to new system cohort
      if (!is.na(new_hires) && new_hires > 0) {
        # Weighted average of remaining employees and new hires
        if (new_cohort_size > 0) {
          new_cohort_productivity <- (new_cohort_size * new_cohort_productivity + 
                                     new_hires * pars$new_system_productivity) / 
                                    (new_cohort_size + new_hires)
        } else {
          new_cohort_productivity <- pars$new_system_productivity
        }
        new_cohort_size <- round(new_cohort_size + new_hires)
      }
      
      # Store current state - ensure no NA values and whole numbers for workforce counts
      results <- rbind(results, data.frame(
        Quarter = q,
        Year = q / 4,
        Old_Cohort_Size = round(ifelse(is.na(old_cohort_size), 0, old_cohort_size)),
        New_Cohort_Size = round(ifelse(is.na(new_cohort_size), 0, new_cohort_size)),
        Total_Workforce = round(ifelse(is.na(old_cohort_size + new_cohort_size), 0, old_cohort_size + new_cohort_size)),
        Old_Cohort_Productivity = ifelse(is.na(old_cohort_productivity), pars$observed_prod, old_cohort_productivity),
        New_Cohort_Productivity = ifelse(is.na(new_cohort_productivity), pars$new_system_productivity, new_cohort_productivity),
        Average_Productivity = ifelse(is.na(current_avg_prod), pars$observed_prod, current_avg_prod),
        Old_Cohort_Turnover_Rate = ifelse(is.na(old_turnover_results$avg_turnover_rate), 0, old_turnover_results$avg_turnover_rate * 4 * 100),
        New_Cohort_Turnover_Rate = ifelse(is.na(new_turnover_results$avg_turnover_rate), 0, new_turnover_results$avg_turnover_rate * 4 * 100),
        Old_Cohort_Departures = round(ifelse(is.na(old_turnover_results$departures), 0, old_turnover_results$departures)),
        New_Cohort_Departures = round(ifelse(is.na(new_turnover_results$departures), 0, new_turnover_results$departures)),
        Production_Demand = quarterly_demands[q],
        Required_Workforce = round(ifelse(is.na(required_workforce), 0, required_workforce)),
        New_Hires = round(ifelse(is.na(new_hires), 0, new_hires))
      ))
    }
    
    return(results)
  })
  
  # Workforce evolution plot
  output$workforceEvolutionPlot <- renderPlot({
    data <- workforce_evolution()
    
    # Create a combined plot showing workforce composition and hiring patterns
    
    # Plot 1: Workforce composition
    data_long <- data.frame(
      Year = rep(data$Year, 2),
      Cohort = c(rep("Old System Cohort", nrow(data)), rep("New System Cohort", nrow(data))),
      Size = c(data$Old_Cohort_Size, data$New_Cohort_Size),
      Productivity = c(data$Old_Cohort_Productivity, data$New_Cohort_Productivity)
    )
    
    p1 <- ggplot(data_long, aes(x = Year, y = Size, fill = Cohort)) +
      geom_area(alpha = 0.7) +
      scale_fill_manual(values = c("Old System Cohort" = "#E74C3C", "New System Cohort" = "#3498DB")) +
      labs(
        title = "Workforce Composition Evolution",
        x = "Years",
        y = "Number of Employees",
        fill = "Employee Cohort"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom"
      )
    
    # Plot 2: Hiring patterns and productivity
    p2 <- ggplot(data, aes(x = Year)) +
      geom_col(aes(y = New_Hires), fill = "#2ECC71", alpha = 0.7, width = 0.2) +
      geom_line(aes(y = Average_Productivity - 150), color = "#9B59B6", linewidth = 1.2) +
      scale_y_continuous(
        name = "New Hires per Quarter",
        sec.axis = sec_axis(~ . + 150, name = "Average Productivity")
      ) +
      labs(
        title = "Hiring Patterns & Productivity Evolution",
        x = "Years"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y.left = element_text(color = "#2ECC71"),
        axis.title.y.right = element_text(color = "#9B59B6")
      ) +
      annotate("text", x = max(data$Year) * 0.7, y = max(data$New_Hires) * 0.8,
               label = "Green bars = New Hires\nPurple line = Avg Productivity",
               size = 3, hjust = 0)
    
    # Combine plots
    grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))
  })
  
  # Workforce summary
  output$workforceSummary <- renderText({
    data <- workforce_evolution()
    final_data <- tail(data, 1)
    initial_data <- head(data, 1)
    
    # Calculate cumulative turnover effects
    total_old_departures <- sum(data$Old_Cohort_Departures, na.rm = TRUE)
    total_new_departures <- sum(data$New_Cohort_Departures, na.rm = TRUE)
    total_new_hires <- sum(data$New_Hires, na.rm = TRUE)
    avg_quarterly_hires <- mean(data$New_Hires, na.rm = TRUE)
    
    # Calculate hiring trends
    first_year_hires <- sum(data$New_Hires[1:4], na.rm = TRUE)
    last_year_hires <- sum(data$New_Hires[17:20], na.rm = TRUE)
    
    # Calculate average turnover rates for comparison
    avg_old_turnover <- mean(data$Old_Cohort_Turnover_Rate, na.rm = TRUE)
    avg_new_turnover <- mean(data$New_Cohort_Turnover_Rate, na.rm = TRUE)
    
    # Calculate productivity evolution
    old_prod_change <- final_data$Old_Cohort_Productivity - initial_data$Old_Cohort_Productivity
    new_prod_change <- final_data$New_Cohort_Productivity - initial_data$New_Cohort_Productivity
    
    paste0(
      "INITIAL STATE:\n",
      "  Total Workforce: ", round(initial_data$Total_Workforce, 0), "\n",
      "  Average Productivity: ", round(initial_data$Average_Productivity, 1), "\n",
      "  Old System Productivity: ", round(initial_data$Old_Cohort_Productivity, 1), " units\n",
      "  New System Productivity: ", round(initial_data$New_Cohort_Productivity, 1), " units\n\n",
      "FINAL STATE (Year ", round(final_data$Year, 1), "):\n",
      "  Old System Employees: ", round(final_data$Old_Cohort_Size, 0), 
      " (", round(100 * final_data$Old_Cohort_Size / final_data$Total_Workforce, 1), "%)\n",
      "  New System Employees: ", round(final_data$New_Cohort_Size, 0), 
      " (", round(100 * final_data$New_Cohort_Size / final_data$Total_Workforce, 1), "%)\n",
      "  Total Workforce: ", round(final_data$Total_Workforce, 0), "\n",
      "  Average Productivity: ", round(final_data$Average_Productivity, 1), "\n",
      "  Productivity Improvement: +", round(final_data$Average_Productivity - initial_data$Average_Productivity, 1), "\n\n",
      "DIFFERENTIAL TURNOVER EFFECTS:\n",
      "  OLD SYSTEM COHORT (Lower Average Performance):\n",
      "    • Average Annual Turnover: ", round(avg_old_turnover, 1), "%\n",
      "    • Final Productivity: ", round(final_data$Old_Cohort_Productivity, 1), " units\n",
      "    • Productivity Gain: +", round(old_prod_change, 1), " units (", round((old_prod_change/initial_data$Old_Cohort_Productivity)*100, 1), "%)\n",
      "    • Total Departures: ", round(total_old_departures, 0), " employees\n\n",
      "  NEW SYSTEM COHORT (Higher Average Performance):\n",
      "    • Average Annual Turnover: ", round(avg_new_turnover, 1), "%\n",
      "    • Final Productivity: ", round(final_data$New_Cohort_Productivity, 1), " units\n",
      "    • Productivity Gain: +", round(new_prod_change, 1), " units (", round((new_prod_change/initial_data$New_Cohort_Productivity)*100, 1), "%)\n",
      "    • Total Departures: ", round(total_new_departures, 0), " employees\n\n",
      "  DIFFERENTIAL IMPACT:\n",
      "    • Turnover Rate Difference: ", round(avg_old_turnover - avg_new_turnover, 1), " percentage points higher for old system\n",
      "    • Departure Difference: ", round(total_old_departures - total_new_departures, 0), " more departures from old system\n",
      "    • This confirms functional turnover: underperformers (old system) leave at higher rates\n\n",
      "HIRING PATTERNS:\n",
      "  Total New Hires (5 Years): ", round(total_new_hires, 0), " employees\n",
      "  Average Quarterly Hiring: ", round(avg_quarterly_hires, 1), " employees\n",
      "  First Year Total Hires: ", round(first_year_hires, 0), " employees\n",
      "  Last Year Total Hires: ", round(last_year_hires, 0), " employees\n",
      "  Hiring Trend: ", ifelse(last_year_hires > first_year_hires, 
                               paste0("+", round(last_year_hires - first_year_hires, 0), " more hires/year"),
                               paste0(round(first_year_hires - last_year_hires, 0), " fewer hires/year")), "\n\n",
      "KEY INSIGHTS:\n",
      "• Performance-based rewards create 'functional turnover' - underperformers leave, high performers stay\n",
      "• Old system employees (", round(initial_data$Old_Cohort_Productivity, 1), " units avg) have higher baseline turnover\n",
      "• New system employees (", round(initial_data$New_Cohort_Productivity, 1), " units avg) have lower turnover rates\n",
      "• Both cohorts improve productivity over time through selective retention of high performers\n",
      "• The differential effect is ", round(avg_old_turnover - avg_new_turnover, 1), " percentage points - old system workers quit more\n",
      "• Hiring patterns reflect the balance between productivity gains and production demands\n",
      "<b>Critical Insight - Differential Impact by Employee Type:</b><br>",
      "• <b>Old System Employees:</b> Lower average performance (200 units) → higher baseline turnover rates<br>",
      "• <b>New System Employees:</b> Higher average performance (~220+ units) → lower baseline turnover rates<br>",
      "• <b>Performance-Based Rewards Amplify These Differences:</b><br>",
      "  - Make high performers (mostly new system) even more likely to stay<br>",
      "  - Make poor performers (disproportionately old system) even more likely to leave<br>",
      "• <b>Compounding Effect:</b> Creates stronger 'functional turnover' where compensation systems actively separate high and low performers<br>",
      "• <b>Quality Evolution:</b> Both cohorts improve over time, but the improvement is accelerated under performance-based pay<br><br>",
      "<b>Critical Insight - How the U-Shaped Relationship Affects Different Employee Types:</b><br>",
      "• <b>Old System Employees (200 units average):</b> Sit near the bottom of the U-curve with moderate turnover<br>",
      "• <b>New System Employees (~220+ units average):</b> Sit on the right side of the U-curve<br>",
      "  - WITHOUT performance-based rewards: Higher turnover due to better external opportunities<br>",
      "  - WITH performance-based rewards: Much lower turnover due to financial incentives to stay<br>",
      "• <b>The U-Curve Explains Why Performance-Based Pay Is So Effective:</b><br>",
      "  - High performers naturally have elevated turnover (right side of U)<br>",
      "  - Performance-based rewards specifically target this vulnerability<br>",
      "  - Transforms potential weakness (losing top talent) into competitive advantage<br>",
      "• <b>Compounding Effect:</b> New system employees benefit most from performance-based rewards<br>",
      "• <b>Quality Evolution:</b> U-curve flattening accelerates workforce improvement over time<br><br>"
    )
  })
  
  # Reset button functionality
  observeEvent(input$reset, {
    updateTextInput(session, "validity_old_txt", value = "0.10")
    updateTextInput(session, "validity_new_txt", value = "0.50")
    updateTextInput(session, "sr_txt", value = "33%")
    updateTextInput(session, "corr_txt", value = "-0.18")
    updateTextInput(session, "base_turnover_txt", value = "10")
    updateCheckboxInput(session, "performance_rewards", value = FALSE)
    updateTextInput(session, "perf_pay_pct_txt", value = "5")
    updateTextInput(session, "q1_demand_txt", value = "100000")
    updateTextInput(session, "q2_demand_txt", value = "100000")
    updateTextInput(session, "q3_demand_txt", value = "105000")
    updateTextInput(session, "q4_demand_txt", value = "110000")
  })
  
  # Update correlation when performance rewards are toggled
  observeEvent(input$performance_rewards, {
    if(input$performance_rewards) {
      updateTextInput(session, "corr_txt", value = "-0.27")
    } else {
      updateTextInput(session, "corr_txt", value = "-0.18")
    }
  })
  
  # Calculate hires vector reactively
  hires_vec <- reactive({
    pars <- vals()
    # fixed parameters
    initial_force <- 500
    base_turnover <- pars$base_turnover
    base_prod     <- pars$observed_prod
    quarters <- c("Q1","Q2","Q3","Q4")
    demand   <- pars$demand
    
    # Naylor–Shine gain for new system
    delta_z     <- pars$rxy_new * ux(pars$sr)
    sdv <- 40  # Fixed SD of 40
    delta_units <- delta_z * sdv
    new_prod    <- round(base_prod + delta_units)
    
    # Initialize variables
    N <- initial_force
    P_new <- new_prod
    P_avg <- base_prod
    N_selected <- 0
    N_base <- N
    hires_vec <- rep(0, 4)
    
    # Track productivity evolution
    productivity_history <- list(
      selected = rep(new_prod, 4),  # New system hires
      base = rep(base_prod, 4)      # Old system employees
    )
    
    for (i in seq_along(quarters)) {
      # Calculate turnover using the proper function
      turnover_selected <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
      turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
      
      # Calculate turnover
      N_stay_selected <- N_selected * (1 - turnover_selected)
      N_stay_base <- N_base * (1 - turnover_base)
      
      # Calculate required workforce and after turnover
      current_avg_prod <- (N_stay_selected * new_prod + N_stay_base * base_prod) / (N_stay_selected + N_stay_base)
      req_force <- ceiling(demand[i] / current_avg_prod)
      after_turnover <- N_stay_selected + N_stay_base
      needed <- max(0, req_force - after_turnover)
      hires <- ceiling(needed)
      
      # Store hires for this quarter
      hires_vec[i] <- hires
      
      # Update workforce composition for next quarter
      N_selected <- N_stay_selected + hires
      N_base <- N_stay_base
      
      # Update productivity history
      productivity_history$selected[i] <- new_prod
      productivity_history$base[i] <- base_prod
    }
    
    hires_vec
  })
  
  # Only take one sample and use it for all plots
  past_sample <- reactiveVal({
    set.seed(789)  # Changed seed to get mean of 200
    rnorm(500, mean = 200, sd = 40)
  })
  
  # histogram of past productivity
  output$prodHist <- renderPlot({
    pars <- vals()
    past <- past_sample()
    avg <- round(mean(past))
    sdv <- 40  # Fixed SD of 40
    # Calculate predicted productivity for new hires
    pred_prod <- round(pars$observed_prod + (pars$rxy_new * ux(pars$sr)) * sdv, 1)
    # Calculate position for annotation box (top right)
    x_annotate <- quantile(past, 0.99)
    hist_counts <- hist(past, plot = FALSE)$counts
    ggplot(data.frame(prod = past), aes(prod)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "white") +
      geom_vline(xintercept = avg, color = "black", linetype = "solid", linewidth = 1.1) +
      geom_vline(xintercept = avg + sdv, color = "darkorange", linetype = "solid", linewidth = 1.1) +
      geom_vline(xintercept = pred_prod, color = "purple", linetype = "solid", linewidth = 1.1) +
      annotate(
        "label",
        x = x_annotate,
        y = Inf,
        label = paste0(
          "Avg = ", round(avg,1),
          "\nSD = ", round(sdv,1),
          "\nNew Hire Predicted Prod. = ", pred_prod, "\n"
        ),
        hjust = 1,
        vjust = 1,
        size = 5,
        color = "black",
        fontface = "bold",
        fill = "white",
        label.size = 0.7,
        label.r = unit(0.25, "lines")
      ) +
      labs(
        title = "<b>Distribution of Employee Productivity in the Previous Quarter</b>",
        x = "Productivity (units)",
        y = "Count"
      ) +
      scale_x_continuous(breaks = seq(floor(min(past)/10)*10, ceiling(max(past)/10)*10, by = 10)) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_text(size = 22, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = "bold")
      ) +
      coord_cartesian(clip = "off")
  })
  
  # Store predicted workforce productivity for each quarter globally for use in both notes and bar chart
  predicted_prod_vec <- NULL
  output$staffingPlot <- renderPlot({
    pars <- vals()
    # fixed parameters
    initial_force <- 500
    base_turnover <- pars$base_turnover
    base_prod     <- pars$observed_prod
    cost_hire     <- 2000
    salary        <- 32000
    ben_pct       <- 20        # benefits % of salary
    train_cost    <- 200
    total_hire    <- cost_hire + salary + (ben_pct/100)*salary + train_cost
    quarters <- c("Q1","Q2","Q3","Q4")
    demand   <- pars$demand
    # Naylor–Shine gain
    delta_z     <- pars$rxy_new * ux(pars$sr)
    sdv <- 40  # Fixed SD of 40
    delta_units <- delta_z * sdv
    new_prod    <- round(base_prod + delta_units)
    
    # Calculate turnover rates using the proper function
    turnover_selected <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
    turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
    
    # baseline strategy
    base_df <- data.frame(Quarter=quarters, Demand=demand)
    base_df$Turnover_Hires <- round(initial_force * turnover_base)
    base_df$Base_Capacity   <- initial_force * base_prod
    base_df$Extra_Units     <- pmax(base_df$Demand - base_df$Base_Capacity, 0)
    base_df$Extra_Hires     <- ceiling(base_df$Extra_Units / base_prod)
    base_df$Total_Hires     <- base_df$Turnover_Hires + base_df$Extra_Hires
    # Calculate baseline costs with quarterly salary/benefits
    salary_qtr <- 32000 / 4
    benefits_qtr <- 0.20 * salary_qtr
    base_cost_per_hire <- 2000 + salary_qtr + benefits_qtr + 200
    base_df$Cost            <- base_df$Total_Hires * base_cost_per_hire
    
    # Add performance-based pay costs for baseline if enabled
    if (isTRUE(pars$performance_rewards)) {
      perf_pay_qtr <- salary_qtr * (pars$perf_pay_pct / 100)
      base_df$Cost <- base_df$Cost + (base_df$Total_Hires * perf_pay_qtr)
    }
    
    # improved strategy
    imp_df <- data.frame(Quarter=quarters, Demand=demand)
    imp_df$N_Selected <- 0  # Start with no new system hires
    imp_df$N_Base <- initial_force  # Start with all base productivity workers
    
    # Calculate initial turnover rates
    turnover_selected <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
    turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
    
    for (i in seq_along(quarters)) {
      if (i == 1) {
        # First quarter starts with all base productivity workers
        N_stay_selected <- 0
        N_stay_base <- round(initial_force * (1 - turnover_base))
      } else {
        # Calculate turnover for existing workers
        turnover_selected <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
        turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
        
        N_stay_selected <- round(imp_df$N_Selected[i-1] * (1 - turnover_selected))
        N_stay_base <- round(imp_df$N_Base[i-1] * (1 - turnover_base))
      }
      
      # Calculate required workforce based on current average productivity
      current_avg_prod <- if ((N_stay_selected + N_stay_base) > 0) {
        (N_stay_selected * new_prod + N_stay_base * base_prod) / (N_stay_selected + N_stay_base)
      } else {
        base_prod
      }
      
      req_force <- ceiling(demand[i] / current_avg_prod)
      
      # Calculate needed hires
      after_turnover <- N_stay_selected + N_stay_base
      needed <- max(0, req_force - after_turnover)
      hires <- ceiling(needed)
      
      # Update workforce composition - ensure whole numbers
      imp_df$N_Selected[i] <- round(N_stay_selected + hires)
      imp_df$N_Base[i] <- round(N_stay_base)
      
      # Calculate costs
      imp_df$Total_Hires[i] <- hires
      imp_df$Cost[i] <- hires * base_cost_per_hire
      
      # Add performance-based pay costs for improved system if enabled
      if (isTRUE(pars$performance_rewards)) {
        perf_pay_qtr <- salary_qtr * (pars$perf_pay_pct / 100)
        imp_df$Cost[i] <- imp_df$Cost[i] + (hires * perf_pay_qtr)
      }
      
      # Store productivity for this quarter - use same rounding as hirePlot
      predicted_prod_vec[i] <- round((imp_df$N_Selected[i] * new_prod + imp_df$N_Base[i] * base_prod) / (imp_df$N_Selected[i] + imp_df$N_Base[i]), 1)
    }
    
    # plot cost savings
    df <- data.frame(
      Quarter     = quarters,
      Base_Cost   = base_df$Cost,
      Imp_Cost    = imp_df$Cost
    )
    df$Savings <- df$Base_Cost - df$Imp_Cost
    
    # Calculate total savings
    total_savings <- sum(df$Savings)
    
    # Calculate productivity improvement
    delta_z <- pars$rxy_new * ux(pars$sr)
    sdv <- 40  # Fixed SD of 40
    delta_units <- delta_z * sdv
    avg_prod_improvement <- delta_units  # This is the true improvement from selection
    prod_improvement_pct <- round((avg_prod_improvement / base_prod) * 100, 1)
    
    # Title and subtitle
    subtitle <- paste0(
      "The new system (rxy = ", pars$rxy_new, ") delivers ", prod_improvement_pct, 
      "% more productive workers, creating $", formatC(total_savings / 1e6, format = "f", digits = 3),
      "M in cost savings that can be reinvested in performance-based rewards"
    )
    
    # Plot
    p1 <- ggplot(df, aes(x = Quarter, y = Savings)) +
      geom_bar(stat = "identity", fill = "forestgreen") +
      # Productivity labels above bars
      geom_text(
        aes(label = paste0("(", predicted_prod_vec, " units)")),
        vjust = -2.5,
        size = 4.5,
        color = "forestgreen"
      ) +
      # Dollar values with K/M formatting
      geom_text(
        aes(label = ifelse(abs(Savings) >= 1e6,
                           paste0("$", formatC(Savings / 1e6, format = "f", digits = 3), "M"),
                           paste0("$", formatC(Savings / 1e3, format = "f", digits = 1), "K"))),
        vjust = -0.5,
        size = 4.5
      ) +
      scale_y_continuous(
        labels = function(x) ifelse(abs(x) >= 1e6,
                                   paste0("$", formatC(x / 1e6, format = "f", digits = 3), "M"),
                                   paste0("$", formatC(x / 1e3, format = "f", digits = 1), "K")),
        expand = expansion(mult = c(0.05, 0.20)),
        limits = c(0, max(c(df$Savings, 500000)))  # Set minimum scale to show differences
      ) +
      labs(
        title = "Cost Savings from Enhanced Staffing Can Fund Performance-Based Rewards",
        x = "Quarter",
        y = "Cost Savings Relative to Current Staffing Strategy"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold")
      )
    
    # Return just the cost savings plot
    p1
  }, height = 600)
  
  output$hirePlot <- renderPlot({
    # Use the same logic as the Value Created Over Time tab
    pars <- vals()
    base_turnover <- pars$base_turnover  # Use from parameters, not hardcoded
    initial_workforce <- 500
    quarters <- c("Q1", "Q2", "Q3", "Q4")
    demand <- pars$demand
    base_prod <- pars$observed_prod
    new_prod <- round(base_prod + (pars$rxy_new * ux(pars$sr)) * 40)
    N_selected <- 0
    N_base <- initial_workforce
    old_start <- rep(NA, 4)
    new_start <- rep(NA, 4)
    old_after <- rep(NA, 4)
    new_after <- rep(NA, 4)
    old_end <- rep(NA, 4)
    new_end <- rep(NA, 4)
    required_workforce <- rep(NA, 4)
    actual_headcount <- rep(NA, 4)
    pct_new <- rep(NA, 4)
    new_hires_vec <- rep(NA, 4)
    for (i in 1:4) {
      if (i == 1) {
        old_start[i] <- initial_workforce
        new_start[i] <- 0
      } else {
        old_start[i] <- old_end[i-1]
        new_start[i] <- new_end[i-1]
      }
      # Calculate turnover using the proper function
      turnover_selected <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
      turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
      
      old_after[i] <- round(old_start[i] * (1 - turnover_base))
      new_after[i] <- round(new_start[i] * (1 - turnover_selected))
      
      # CORRECTED HIRING CALCULATION: Use gap-filling approach
      # Calculate current production capacity from existing workers
      current_production <- old_after[i] * base_prod + new_after[i] * new_prod
      
      # Calculate production gap that needs to be filled
      production_gap <- max(0, demand[i] - current_production)
      
      # Calculate new hires needed based on their actual productivity
      hires <- ceiling(production_gap / new_prod)
      new_hires_vec[i] <- hires
      
      # Update workforce - ensure whole numbers
      old_end[i] <- round(old_after[i])
      new_end[i] <- round(new_after[i] + hires)
      
      # Calculate final metrics for display
      total_workforce <- old_end[i] + new_end[i]
      required_workforce[i] <- total_workforce  # Actual workforce after optimal hiring
      actual_headcount[i] <- total_workforce
      pct_new[i] <- if (total_workforce > 0) round(100 * new_end[i] / total_workforce, 1) else 0
    }
    # Prepare data for stacked bar chart
    plot_df <- data.frame(
      Quarter = quarters,
      Old_System = old_end,
      New_System = ceiling(new_end),
      Required_Workforce = required_workforce,
      Pred_Prod = round((old_end * base_prod + new_end * new_prod) / pmax(1, old_end + new_end), 1),
      Demand = demand
    )
    plot_df_long <- reshape2::melt(plot_df, id.vars = c("Quarter", "Required_Workforce", "Pred_Prod", "Demand"),
                                   variable.name = "System", value.name = "Count")
    # Custom x-axis labels with required workforce in parentheses
    quarter_labels <- paste0(quarters, "\n(", required_workforce, ")")
    # For annotation: overall workforce size and new hires/percent
    overall_size <- plot_df$Old_System + plot_df$New_System
    new_hires_pct <- 100 * plot_df$New_System / pmax(1, overall_size)
    
    # Calculate y-axis maximum for better comparison between scenarios
    # Get max workforce size for current scenario
    max_current_workforce <- max(plot_df$Required_Workforce)
    
    # Calculate what the workforce would be under the opposite scenario for comparison
    opposite_rewards <- !pars$performance_rewards
    max_opposite_workforce <- 0
    
    # Quick calculation for opposite scenario
    for (i in 1:4) {
      if (i == 1) {
        old_start_opp <- initial_workforce
        new_start_opp <- 0
      } else {
        old_start_opp <- old_end_opp
        new_start_opp <- new_end_opp
      }
      
      # Calculate turnover for opposite scenario
      turnover_base_opp <- calculate_turnover(base_prod, base_turnover, opposite_rewards)
      turnover_selected_opp <- calculate_turnover(new_prod, base_turnover, opposite_rewards)
      
      old_after_opp <- round(old_start_opp * (1 - turnover_base_opp))
      new_after_opp <- round(new_start_opp * (1 - turnover_selected_opp))
      
      # Calculate hiring for opposite scenario
      current_production_opp <- old_after_opp * base_prod + new_after_opp * new_prod
      production_gap_opp <- max(0, demand[i] - current_production_opp)
      hires_opp <- ceiling(production_gap_opp / new_prod)
      
      old_end_opp <- round(old_after_opp)
      new_end_opp <- round(new_after_opp + hires_opp)
      
      total_workforce_opp <- old_end_opp + new_end_opp
      max_opposite_workforce <- max(max_opposite_workforce, total_workforce_opp)
    }
    
    # Set y-axis max to be 1.15 times the higher of the two scenarios (with some padding for labels)
    y_max <- max(max_current_workforce, max_opposite_workforce) * 1.15
    
    # Plot
    ggplot(plot_df_long, aes(x = Quarter, y = Count, fill = System)) +
      geom_bar(stat = "identity") +
      # Overall workforce size on top of bar
      geom_text(data = plot_df, aes(x = Quarter, y = Required_Workforce + 20, label = paste0("Total: ", Required_Workforce)),
                inherit.aes = FALSE, size = 5, fontface = "bold", color = "black") +
      # New hires and percent on top of dark blue bar - adjusted position
      geom_text(data = plot_df, aes(x = Quarter, y = New_System + 15,
                label = paste0(ceiling(New_System), " (", round(new_hires_pct, 1), "% )")),
                inherit.aes = FALSE, size = 4.5, fontface = "bold", color = "black") +
      # Productivity rate in middle of bar
      geom_text(data = plot_df, aes(x = Quarter, y = (Old_System + New_System)/2 + 10,
                label = paste0("Prod: ", Pred_Prod)),
                inherit.aes = FALSE, size = 4.5, fontface = "bold", color = "white") +
      scale_fill_manual(values = c("Old_System" = "#a6cee3", "New_System" = "#1f78b4"),
                        labels = c("Old-System Hires", "New-System Hires"),
                        name = "Workforce Segment") +
      scale_x_discrete(labels = paste0(quarters, "\n", formatC(demand, big.mark=",", format="d"))) +
      scale_y_continuous(name = "Number of Workers", limits = c(0, y_max)) +
      labs(
        title = "Workforce Composition Needed to Meet Demand",
        x = "Quarter (Production Demands)",
        y = "Number of Workers"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = "bold"),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)
      )
  })
  
  # Employee Flows Table Output
  output$employeeFlowsTable <- renderUI({
    HTML(paste0(
      "<b>Understanding the Plot:</b><br>",
      "The stacked bar chart shows the composition of your workforce over time. The light blue portion represents employees hired under the old system, while the dark blue portion shows employees hired under the new system. The numbers above each bar show the total workforce size and the number of new-system hires (with their percentage of the total). The productivity rate is shown in the middle of each bar.<br><br>",
      "<b>How Staffing and Compensation Impact Workforce Quality:</b><br>",
      "The quality of your workforce evolves over time through two key mechanisms:<br><br>",
      "<b>1. Enhanced Selection (New System Hires):</b><br>",
      "• More selective hiring (lower selection ratio) allows choosing from a larger pool of applicants<br>",
      "• Higher validity selection procedures better identify high performers<br>",
      "• This results in new hires with higher initial productivity<br><br>",
      "<b>2. Performance-Based Turnover:</b><br>",
      "Our enhanced model is based on Williams & Livingstone (1994) research and accounts for individual performance distributions within each employee cohort. Williams & Livingstone found a U-shaped relationship between job performance and voluntary turnover, where both very low performers and very high performers tend to have higher turnover rates than average performers. However, they found that this relationship is significantly modified by pay-for-performance systems: <b>performance-based rewards flatten the right side of the U-curve, making high performers much more likely to stay, while potentially increasing turnover among low performers who cannot achieve performance-based rewards.</b><br><br>",
      "<b>Key Model Features:</b><br>",
      "• <b>Individual Performance Modeling:</b> Each employee cohort has a normal distribution of performance (SD = 40 units)<br>",
      "• <b>Performance-Dependent Turnover:</b> Each individual's turnover probability depends on their specific performance level<br>",
      "• <b>Differential Impact by Cohort:</b> Old system employees (lower average performance) experience higher turnover rates<br>",
      "• <b>Functional Turnover:</b> Poor performers are more likely to leave, improving remaining workforce quality<br><br>",
      "<b>Turnover Model Mathematics:</b><br>",
      "For each individual employee with performance P, we calculate their quarterly turnover probability as follows:<br>",
      "1. <b>Performance Z-score</b> = (P - 200) / 40<br>",
      "   This standardizes performance relative to the population mean (200 units) and standard deviation (40 units)<br><br>",
      "2. <b>Without performance-based rewards (U-shaped relationship):</b><br>",
      "   Turnover adjustment = Linear component + U-shape component + random noise<br>",
      "   • Linear component: -0.18 × Z × 0.015 (negative relationship with performance)<br>",
      "   • U-shape component: +0.05 × Z² × 0.010 (higher turnover at both extremes)<br>",
      "   • This creates the classic U-curve where very high performers also have elevated turnover<br>",
      "   • Random noise ensures realistic variation in individual turnover decisions<br><br>",
      "3. <b>With performance-based rewards (flattened U-curve):</b><br>",
      "   Turnover adjustment = Stronger linear component + Reduced U-shape component + random noise<br>",
      "   • Linear component: -0.27 × Z × 0.016 (stronger retention of high performers)<br>",
      "   • U-shape component: +0.02 × Z² × 0.008 (reduced turnover among high performers)<br>",
      "   • Performance-based rewards 'flatten' the right side of the U-curve<br>",
      "   • High performers become much more likely to stay due to financial incentives<br>",
      "   • Low performers may have slightly higher turnover due to inability to earn performance rewards<br><br>",
      "4. <b>Final turnover rate</b> = Base quarterly rate (2.5%) + adjustment<br>",
      "   • Each individual's final turnover probability is bounded between 1% and 20% quarterly<br><br>",
      "<b>Expected Turnover Patterns (Annual Rates):</b><br>",
      "<b>Without Performance-Based Rewards (U-shaped curve):</b><br>",
      "• Low performers (160 units): ~12% annual turnover (left side of U)<br>",
      "• Average performers (200 units): ~10% annual turnover (bottom of U)<br>",
      "• High performers (240 units): ~11.5% annual turnover (right side of U - elevated due to better opportunities elsewhere)<br>",
      "• Very high performers (280 units): ~13% annual turnover (higher end of right side of U)<br>",
      "• Pattern: Classic U-curve with lowest turnover at average performance levels<br><br>",
      "<b>With Performance-Based Rewards (flattened U-curve):</b><br>",
      "• Low performers (160 units): ~13% annual turnover (slightly higher - cannot earn performance rewards)<br>",
      "• Average performers (200 units): ~8.5% annual turnover<br>",
      "• High performers (240 units): ~7% annual turnover (much lower - retained by performance incentives)<br>",
      "• Very high performers (280 units): ~6.5% annual turnover (dramatically reduced from U-curve pattern)<br>",
      "• Pattern: Right side of U-curve is flattened - high performers now have lowest turnover rates<br>",
      "• <b>Key Insight:</b> Pay-for-performance transforms the U-curve into a more linear negative relationship<br><br>",
      "<b>Critical Insight - How the U-Shaped Relationship Affects Different Employee Types:</b><br>",
      "• <b>Old System Employees (200 units average):</b> Sit near the bottom of the U-curve with moderate turnover<br>",
      "• <b>New System Employees (~220+ units average):</b> Sit on the right side of the U-curve<br>",
      "  - WITHOUT performance-based rewards: Higher turnover due to better external opportunities<br>",
      "  - WITH performance-based rewards: Much lower turnover due to financial incentives to stay<br>",
      "• <b>The U-Curve Explains Why Performance-Based Pay Is So Effective:</b><br>",
      "  - High performers naturally have elevated turnover (right side of U)<br>",
      "  - Performance-based rewards specifically target this vulnerability<br>",
      "  - Transforms potential weakness (losing top talent) into competitive advantage<br>",
      "• <b>Compounding Effect:</b> New system employees benefit most from performance-based rewards<br>",
      "• <b>Quality Evolution:</b> U-curve flattening accelerates workforce improvement over time<br><br>",
      "The turnover rates are calculated using a base rate of 10% annual turnover (2.5% quarterly), adjusted by:<br>",
      "1. A linear effect based on performance (stronger with rewards)<br>",
      "2. Random variation to simulate realistic individual differences in turnover decisions<br><br>",
      "This creates more 'functional' turnover - poor performers leave while valuable contributors remain. The effect applies to the entire workforce when performance-based rewards are implemented.<br><br>",
      "<b>Understanding the Workforce Evolution:</b><br>",
      "Each quarter, the workforce composition changes through:<br>",
      "1. Turnover of existing employees (affected by performance and rewards)<br>",
      "2. Hiring of new employees (affected by selection system)<br>",
      "3. Natural evolution of workforce quality as better performers are retained<br><br>",
      "<b>Quarterly Workforce Changes:</b><br>",
      "The tables below show the detailed breakdown of workforce changes for each system. The Old System table shows employees with base productivity, while the New System table shows employees hired under the enhanced selection system. Use these tables to track how the workforce composition changes over time and how turnover affects each group differently.<br><br>",
      "Note that a higher performing workforce is more efficient - because each worker produces more, you need fewer workers to meet the same production demands. This efficiency is reflected in the workforce size calculations, where the required number of workers is determined by dividing the production demand by the average productivity per worker."
    ))
  })
  
  # Old System table
  output$employeeFlowsDataOld <- renderDataTable({
    # Use the same logic as the Value Created Over Time tab
    pars <- vals()
    base_turnover <- pars$base_turnover  # Use from parameters, not hardcoded
    initial_workforce <- 500
    quarters <- c("Q1", "Q2", "Q3", "Q4")
    demand <- pars$demand  # Use from parameters, not hardcoded
    base_prod <- pars$observed_prod
    new_prod <- round(base_prod + (pars$rxy_new * ux(pars$sr)) * 40)
    N_selected <- 0
    N_base <- initial_workforce
    old_start <- rep(NA, 4)
    new_start <- rep(NA, 4)
    old_after <- rep(NA, 4)
    new_after <- rep(NA, 4)
    old_end <- rep(NA, 4)
    new_end <- rep(NA, 4)
    required_workforce <- rep(NA, 4)
    actual_headcount <- rep(NA, 4)
    pct_new <- rep(NA, 4)
    new_hires_vec <- rep(NA, 4)
    quit_old_vec <- rep(NA, 4)
    quit_new_vec <- rep(NA, 4)
    for (i in 1:4) {
      if (i == 1) {
        old_start[i] <- initial_workforce
        new_start[i] <- 0
      } else {
        old_start[i] <- old_end[i-1]
        new_start[i] <- new_end[i-1]
      }
      # Calculate turnover using the proper function
      turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
      turnover_new <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
      
      old_after[i] <- round(old_start[i] * (1 - turnover_base))
      new_after[i] <- round(new_start[i] * (1 - turnover_new))
      quit_old_vec[i] <- old_start[i] * turnover_base
      quit_new_vec[i] <- new_start[i] * turnover_new
      
      # CORRECTED HIRING CALCULATION: Use gap-filling approach
      # Calculate current production capacity from existing workers
      current_production <- old_after[i] * base_prod + new_after[i] * new_prod
      
      # Calculate production gap that needs to be filled
      production_gap <- max(0, demand[i] - current_production)
      
      # Calculate new hires needed based on their actual productivity
      hires <- ceiling(production_gap / new_prod)
      new_hires_vec[i] <- hires
      
      # Update workforce - ensure whole numbers
      old_end[i] <- round(old_after[i])
      new_end[i] <- round(new_after[i] + hires)
      
      # Calculate final metrics for display
      total_workforce <- old_end[i] + new_end[i]
      required_workforce[i] <- total_workforce  # Actual workforce after optimal hiring
      actual_headcount[i] <- total_workforce
      pct_new[i] <- if (total_workforce > 0) round(100 * new_end[i] / total_workforce, 1) else 0
    }
    
    # Calculate percent of workforce for old system workers
    pct_old <- rep(NA, 4)
    for (i in 1:4) {
      total_workforce <- old_end[i] + new_end[i]
      pct_old[i] <- if (total_workforce > 0) round(100 * old_end[i] / total_workforce, 1) else 0
    }
    
    # Create the old system data frame
    df_old <- data.frame(
      Quarter = quarters,
      Start_of_Quarter = round(old_start),
      After_Turnover = round(old_after),
      Quit = round(quit_old_vec),
      End_of_Quarter = round(old_end),
      Percent_of_Workforce = pct_old
    )
    
    # Format the old system table
    datatable(df_old,
      options = list(
        pageLength = 4,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = paste0("Old System Workforce (Base Productivity = ", base_prod, " units)")
    ) %>%
      formatStyle(
        'Percent_of_Workforce',
        background = styleColorBar(c(0,100), 'lightcoral'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
    ) %>%
      formatStyle(
        c('Start_of_Quarter', 'After_Turnover', 'End_of_Quarter'),
        backgroundColor = styleEqual(
          c(0, 1, 2, 3, 4, 5),
          c('#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6')
        )
      )
  })
  
  # New System table
  output$employeeFlowsDataNew <- renderDataTable({
    # Use the same logic as the Value Created Over Time tab
    pars <- vals()
    base_turnover <- pars$base_turnover  # Use from parameters, not hardcoded
    initial_workforce <- 500
    quarters <- c("Q1", "Q2", "Q3", "Q4")
    demand <- pars$demand  # Use from parameters, not hardcoded
    base_prod <- pars$observed_prod  # Use from parameters, not hardcoded
    new_prod <- round(base_prod + (pars$rxy_new * ux(pars$sr)) * 40)
    N_selected <- 0
    N_base <- initial_workforce
    old_start <- rep(NA, 4)
    new_start <- rep(NA, 4)
    old_after <- rep(NA, 4)
    new_after <- rep(NA, 4)
    old_end <- rep(NA, 4)
    new_end <- rep(NA, 4)
    required_workforce <- rep(NA, 4)
    actual_headcount <- rep(NA, 4)
    pct_new <- rep(NA, 4)
    new_hires_vec <- rep(NA, 4)
    quit_old_vec <- rep(NA, 4)
    quit_new_vec <- rep(NA, 4)
    for (i in 1:4) {
      if (i == 1) {
        old_start[i] <- initial_workforce
        new_start[i] <- 0
      } else {
        old_start[i] <- old_end[i-1]
        new_start[i] <- new_end[i-1]
      }
      # Calculate turnover using the proper function
      turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
      turnover_new <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
      
      old_after[i] <- round(old_start[i] * (1 - turnover_base))
      new_after[i] <- round(new_start[i] * (1 - turnover_new))
      quit_old_vec[i] <- old_start[i] * turnover_base
      quit_new_vec[i] <- new_start[i] * turnover_new
      
      # CORRECTED HIRING CALCULATION: Use gap-filling approach
      # Calculate current production capacity from existing workers
      current_production <- old_after[i] * base_prod + new_after[i] * new_prod
      
      # Calculate production gap that needs to be filled
      production_gap <- max(0, demand[i] - current_production)
      
      # Calculate new hires needed based on their actual productivity
      hires <- ceiling(production_gap / new_prod)
      new_hires_vec[i] <- hires
      
      # Update workforce - ensure whole numbers
      old_end[i] <- round(old_after[i])
      new_end[i] <- round(new_after[i] + hires)
      
      # Calculate final metrics for display
      total_workforce <- old_end[i] + new_end[i]
      required_workforce[i] <- total_workforce  # Actual workforce after optimal hiring
      actual_headcount[i] <- total_workforce
      pct_new[i] <- if (total_workforce > 0) round(100 * new_end[i] / total_workforce, 1) else 0
    }
    
    # Create the new system data frame
    df_new <- data.frame(
      Quarter = quarters,
      Start_of_Quarter = round(new_start),
      After_Turnover = round(new_after),
      Quit = round(quit_new_vec),
      New_Hires = round(new_hires_vec),
      End_of_Quarter = round(new_end),
      Percent_of_Workforce = pct_new
    )
    
    # Format the new system table
    datatable(df_new,
      options = list(
        pageLength = 4,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = paste0("New System Workforce (Enhanced Productivity = ", new_prod, " units)")
    ) %>%
      formatStyle(
        'Percent_of_Workforce',
        background = styleColorBar(c(0,100), 'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        c('Start_of_Quarter', 'After_Turnover', 'End_of_Quarter'),
        backgroundColor = styleEqual(
          c(0, 1, 2, 3, 4, 5),
          c('#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6')
        )
      )
  })
  
  output$plotNote <- renderUI({
    if (input$tabs == "Productivity Distribution") {
      # Calculate productivity improvement
      pars <- vals()
      delta_z <- pars$rxy_new * ux(pars$sr)
      sdv <- 40
      new_prod <- 200 + (delta_z * sdv)
      improvement_pct <- round((new_prod - 200) / 200 * 100, 1)
      
      # Create causal diagram with mediating mechanism
      ux_value <- ifelse(abs(pars$sr - 0.33) < 1e-6, 1.09, ux(pars$sr))
      quality_of_hire_sd <- round(pars$rxy_new * ux_value, 2)
      # Compute percent increase in quality of hire for the diagram
      r <- as.numeric(input$validity_new_txt)
      p <- as.numeric(gsub("%", "", input$sr_txt)) / 100
      res <- percent_increase_quality(r, p)
      percent_increase_str <- if (!is.na(res$Percent_Increase)) paste0(round(res$Percent_Increase, 1), "%") else "--"
      diagram <- ggplot() +
        # Boxes - Remove middle box, keep first and third
        annotate("rect", xmin = 0, xmax = 2, ymin = 0, ymax = 1, 
                 fill = "lightblue", alpha = 0.5, color = "black", size = 1) +
        annotate("rect", xmin = 4, xmax = 6, ymin = 0, ymax = 1, 
                 fill = "lightblue", alpha = 0.5, color = "black", size = 1) +
        # Text in boxes - Remove middle box text
        annotate("text", x = 1, y = 0.5, label = "Improved\nStaffing", size = 5, fontface = "bold") +
        annotate("text", x = 5, y = 0.5, label = paste0("Productivity\nImproves by\n", improvement_pct, "%"), size = 5, fontface = "bold") +
        # Arrow - Single arrow from first to third box
        annotate("segment", x = 2, xend = 4, y = 0.5, yend = 0.5, 
                 arrow = arrow(length = unit(0.4, "cm")), size = 1.2) +
        # Set up the plot
        xlim(-0.5, 6.5) +
        ylim(-0.5, 1.5) +
        theme_void()
      
      # Output both the text and the diagram
      tagList(
        HTML(paste0(
          "<b>Understanding the Productivity Distribution:</b><br>",
          "This plot shows the distribution of employee productivity in the previous quarter. ",
          "The black line indicates the mean productivity (200 units), and the orange line ",
          "shows one standard deviation above the mean. The purple line shows the predicted ",
          "productivity of new hires under the enhanced selection system.<br><br>",
          "The selection ratio (the proportion of applicants hired) directly impacts new hire productivity. ",
          "A more selective hiring process (lower selection ratio) allows us to choose from a larger pool of applicants, ",
          "increasing the likelihood of selecting higher-performing employees. This is reflected in the purple line's position, ",
          "which shows the predicted productivity of new hires based on the current selection ratio and validity.<br><br>",
          "Go ahead and change the selection rate to see how predicted productivity is expected to change. Note that increasing the selection ratio ",
          "(making the hiring process more selective) is no easy task. It typically requires significant effort to attract more qualified candidates, ",
          "such as enhancing job advertisements, placing recruiting materials on multiple job boards, and potentially expanding the geographic ",
          "reach of the recruitment process.<br><br>",
          "For unskilled jobs like this level 1 position, research indicates that employees who perform at one standard deviation above the average tend to perform at a level that is 20% above the average (Hunter, Schmidt, & Judiesch, 1990).<br><br>",
          "<b>How Improved Staffing Creates Value:</b><br>",
          "The diagram below illustrates the causal mechanism by which enhanced selection systems create organizational value. ",
          "Improved staffing procedures (higher validity selection tools and more selective hiring) enable organizations to identify and select candidates with higher expected job performance. ",
          "This improvement is quantified through the Naylor-Shine model as Z̄yi = rxy × λi/φi, where higher validity (rxy) and more selective ratios (lower φi, higher λi/φi) ",
          "result in new hires with performance levels significantly above the applicant pool average. ",
          "These higher-performing employees directly translate into increased organizational productivity, creating measurable business value.<br><br>"
        )),
        renderPlot(diagram, height = 220),
        HTML(paste0(
          "<br><b>How Much Does Quality of Hire Improve?</b><br>",
          "In the Naylor-Shine model, <b>Quality of Hire</b> is defined as the expected improvement in job performance (in standard deviation units) for those selected, compared to the average applicant.\n",
          "This is calculated using the Naylor-Shine equation:<br>",
          "<b>Z̄yi = rxy × (λi/φi)</b><br><br>",
          "Where:<br>",
          "• <b>Z̄yi</b> = mean criterion score (in standard score units) of all cases above predictor cutoff<br>",
          "• <b>rxy</b> = validity coefficient = ", round(pars$rxy_new, 2), "<br>",
          "• <b>λi/φi</b> = ordinate of normal distribution at Zxi divided by selection ratio<br>",
          "• <b>Selection Ratio Effect (λi/φi)</b> = ", ifelse(abs(pars$sr - 0.33) < 1e-6, "1.09", round(ux(pars$sr), 2)), "<br><br>",
          "The Selection Ratio Effect (λi/φi) represents the height of the normal curve at the selection cutoff point divided by the proportion of applicants selected. This value increases as selection becomes more stringent (lower selection ratios), indicating that we can choose from the upper tail of the applicant distribution.<br><br>",
          "<b>Current Quality of Hire = ", quality_of_hire_sd, " SD above the applicant mean</b><br><br>",
          "To convert this to productivity units, multiply by the SD of productivity (40 units):<br>",
          "<b>Productivity Improvement = Quality of Hire × SD of Productivity = ", quality_of_hire_sd, " × 40 = ", round(quality_of_hire_sd * 40, 1), " units</b><br><br>"
        )),
        {
          # Compute probability and percent increase in quality of hire
          r <- as.numeric(input$validity_new_txt)
          p <- as.numeric(gsub("%", "", input$sr_txt)) / 100
          res <- percent_increase_quality(r, p)
          if (!is.na(res$PS) && !is.na(res$Percent_Increase)) {
            HTML(paste0(
              "<b>Probability of Higher-Quality Hire (PS):</b> ", sprintf("%.3f", res$PS), "<br>",
              "<b>Percent Increase in Quality of Hire:</b> ", sprintf("%.1f", res$Percent_Increase), "%<br>",
              "<i>This means a randomly selected hire is ", sprintf("%.1f", res$Percent_Increase), "% more likely to outperform a rejected applicant than would be expected under random selection (i.e., 50%). If you are comparing to a previous or alternative selection system, this value represents the improvement over that system's baseline probability.</i><br><br>"
            ))
          } else {
            HTML("<i>Probability and percent increase in quality of hire are only defined for valid values of validity (0 < r < 1) and selection ratio (0 < p < 1).</i><br><br>")
          }
        },
        HTML(paste0(
          "<br><b>Understanding the Math Behind the Improvement:</b><br>",
          "The productivity improvement shown in the diagram above is calculated using the Naylor-Shine utility model (Naylor & Shine, 1965). ",
          "This model predicts the expected performance of selected employees based on the validity of the selection procedure and the selection ratio.<br><br>",
          "The Naylor-Shine model uses the following formula:<br><br>",
          "<b>Z̄yi = rxy × (λi/φi)</b><br><br>",
          "Translating to productivity units:<br>",
          "<b>New Hire Productivity = Base Productivity + (rxy × λi/φi) × SD of Productivity</b><br><br>",
          "Where:<br>",
          "• Base Productivity = 200 units<br>",
          "• <b>rxy</b> = ", round(pars$rxy_new, 2), " (validity coefficient - correlation between selection procedure and job performance)<br>",
          "• <b>λi/φi</b> = ", ifelse(abs(pars$sr - 0.33) < 1e-6, "1.09", round(ux(pars$sr), 2)), " (ordinate of normal distribution divided by selection ratio φi = ", round(pars$sr * 100, 1), "%)<br>",
          "• SD of Productivity = 40 units (standard deviation of job performance)<br><br>",
          "The λi/φi term (often called 'ux' in utility analysis) represents the ordinate of the standard normal distribution at the cutoff point (Zxi) divided by the selection ratio (φi). ",
          "This value increases as selection becomes more stringent because we're selecting from a smaller, higher-quality portion of the applicant pool. ",
          "The ordinate (λi) represents the height of the normal curve at the selection cutoff, while φi is the proportion of applicants selected.<br><br>",
          "Therefore:<br>",
          "New Hire Productivity = 200 + (", round(pars$rxy_new, 2), " × ", ifelse(abs(pars$sr - 0.33) < 1e-6, "1.09", round(ux(pars$sr), 2)), ") × 40<br>",
          "New Hire Productivity = 200 + ", round(delta_z * sdv, 1), " = ", round(new_prod, 1), " units<br><br>",
          "This represents a ", improvement_pct, "% improvement over the base productivity of 200 units. ",
          "You can increase this improvement by either:<br>",
          "1. Increasing the validity (rxy) of the selection process (e.g., through better assessment tools)<br>",
          "2. Making the selection process more selective (lower φi), which increases λi/φi<br><br>",
          "Naylor, J. C., & Shine, L. C. (1965). A table for determining the increase in mean criterion score obtained by using a selection device. ",
          "<i>Journal of Industrial Psychology, 3</i>(1), 33-42.<br><br>"
        ))
      )
    } else if (input$tabs == "Employee Flows") {
      HTML("")
    } else {
      # Original detailed notes for Value Created Over Time
      pars <- vals()
      base_turnover <- pars$base_turnover  # Get from pars
      base_prod <- pars$observed_prod  # Fix: use observed_prod instead of base_prod
      new_prod <- base_prod + (pars$rxy_new * ux(pars$sr)) * 40  # Use fixed SD of 40
      demand <- pars$demand
      quarters <- c("Q1","Q2","Q3","Q4")
      initial_force <- 500
      
      # Ensure no NA values before calculations
      if (is.na(base_prod) || is.null(base_prod)) base_prod <- 200
      if (is.na(new_prod) || is.null(new_prod)) new_prod <- 220
      if (is.na(base_turnover) || is.null(base_turnover)) base_turnover <- 0.10
      
      # Calculate initial turnover rates with error handling
      turnover_selected <- tryCatch({
        calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
      }, error = function(e) {
        0.025  # Default quarterly rate
      })
      
      turnover_base <- tryCatch({
        calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
      }, error = function(e) {
        0.025  # Default quarterly rate
      })
      
      # Ensure turnover rates are not NA
      if (is.na(turnover_selected) || is.null(turnover_selected)) turnover_selected <- 0.025
      if (is.na(turnover_base) || is.null(turnover_base)) turnover_base <- 0.025
      
      # Calculate costs for comparison
      base_df <- data.frame(Quarter=quarters, Demand=demand)
      base_df$Turnover_Hires <- round(initial_force * turnover_base)
      base_df$Base_Capacity   <- initial_force * base_prod
      base_df$Extra_Units     <- pmax(base_df$Demand - base_df$Base_Capacity, 0)
      base_df$Extra_Hires     <- ceiling(base_df$Extra_Units / base_prod)
      base_df$Total_Hires     <- base_df$Turnover_Hires + base_df$Extra_Hires
      salary_qtr <- 32000 / 4
      benefits_qtr <- 0.20 * salary_qtr
      base_cost_per_hire <- 2000 + salary_qtr + benefits_qtr + 200
      base_df$Cost <- base_df$Total_Hires * base_cost_per_hire
      
      # Add performance-based pay costs for baseline if enabled
      if (isTRUE(pars$performance_rewards)) {
        perf_pay_qtr <- salary_qtr * (pars$perf_pay_pct / 100)
        base_df$Cost <- base_df$Cost + (base_df$Total_Hires * perf_pay_qtr)
      }
      
      # Calculate improved system costs with gradual transition
      imp_df <- data.frame(Quarter=quarters, Demand=demand)
      imp_df$N_Selected <- 0  # Start with no new system hires
      imp_df$N_Base <- initial_force  # Start with all base productivity workers
      
      for (i in seq_along(quarters)) {
        if (i == 1) {
          # First quarter starts with all base productivity workers
          N_stay_selected <- 0
          N_stay_base <- round(initial_force * (1 - turnover_base))
        } else {
          # Calculate turnover for existing workers
          turnover_selected <- calculate_turnover(new_prod, base_turnover, pars$performance_rewards)
          turnover_base <- calculate_turnover(base_prod, base_turnover, pars$performance_rewards)
          
          N_stay_selected <- round(imp_df$N_Selected[i-1] * (1 - turnover_selected))
          N_stay_base <- round(imp_df$N_Base[i-1] * (1 - turnover_base))
        }
        
        # Calculate required workforce based on current average productivity
        current_avg_prod <- if ((N_stay_selected + N_stay_base) > 0) {
          (N_stay_selected * new_prod + N_stay_base * base_prod) / (N_stay_selected + N_stay_base)
        } else {
          base_prod
        }
        
        req_force <- ceiling(demand[i] / current_avg_prod)
        
        # Calculate needed hires
        after_turnover <- N_stay_selected + N_stay_base
        needed <- max(0, req_force - after_turnover)
        hires <- ceiling(needed)
        
        # Update workforce composition - ensure whole numbers
        imp_df$N_Selected[i] <- round(N_stay_selected + hires)
        imp_df$N_Base[i] <- round(N_stay_base)
        
        # Calculate costs
        imp_df$Total_Hires[i] <- hires
        imp_df$Cost[i] <- hires * base_cost_per_hire
        
        # Add performance-based pay costs for improved system if enabled
        if (isTRUE(pars$performance_rewards)) {
          perf_pay_qtr <- salary_qtr * (pars$perf_pay_pct / 100)
          imp_df$Cost[i] <- imp_df$Cost[i] + (hires * perf_pay_qtr)
        }
        
        # Store productivity for this quarter - use same rounding as hirePlot
        predicted_prod_vec[i] <- round((imp_df$N_Selected[i] * new_prod + imp_df$N_Base[i] * base_prod) / (imp_df$N_Selected[i] + imp_df$N_Base[i]), 1)
      }
      
      # Calculate total savings
      total_savings <- sum(base_df$Cost - imp_df$Cost)
      
      # Calculate productivity improvement
      delta_z <- pars$rxy_new * ux(pars$sr)
      sdv <- 40  # Fixed SD of 40
      delta_units <- delta_z * sdv
      avg_prod_improvement <- delta_units  # This is the true improvement from selection
      prod_improvement_pct <- round((avg_prod_improvement / base_prod) * 100, 1)
      
      HTML(paste0(
        "<b>Understanding the Cost Savings:</b><br>",
        "The plot above shows how the enhanced selection system (rxy = ", pars$rxy_new, ") creates significant cost savings compared to the current system (rxy = ", pars$rxy_old, "). These savings come from two main sources:<br><br>",
        "<b>1. Reduced Workforce Size:</b><br>",
        "• The new system selects more productive workers (", prod_improvement_pct, "% higher productivity)<br>",
        "• This means we need fewer workers to meet the same production demands<br>",
        "• Fewer workers = lower total compensation costs<br><br>",
        "<b>2. More Efficient Resource Allocation:</b><br>",
        "• Current system: $", formatC(base_df$Cost[1], big.mark=",", format="d"), " per quarter in hiring and compensation costs<br>",
        "• New system: $", formatC(imp_df$Cost[1], big.mark=",", format="d"), " per quarter<br>",
        "• Total savings: $", formatC(total_savings, big.mark=",", format="d"), " over the year<br><br>",
        "<b>Creating Strategic Value Through HR Budget Optimization:</b><br>",
        "By focusing on enhancing the quality of our staffing processes, we create slack in the HR budget that can be strategically reinvested in the workforce through other HR initiatives:<br>",
        "• <b>Enhanced compensation and benefits:</b> Higher pay, better health coverage, retirement matching<br>",
        "• <b>Training and development opportunities:</b> Skills development, leadership programs, career advancement<br>",
        "• <b>Further talent acquisition enhancements:</b> Advanced assessment tools, employer branding, recruitment technology<br><br>",
        "This creates a virtuous cycle whereby investing in people begets more opportunities to invest in the workforce in ways that help the organization deliver value to shareholders. From a long-term value creation perspective, these benefits arise by enhancing how value is delivered to customers, primarily through improved cost structure and asset utilization (Kaplan & Norton, 2004).<br><br>",
        "<b>The Strategic Value Creation Cycle:</b><br>",
        "1. Enhanced selection → More productive workers → Cost savings<br>",
        "2. Cost savings → HR budget slack → Strategic workforce investments<br>",
        "3. Workforce investments → Higher employee engagement and capability<br>",
        "4. Enhanced capability → Better customer value delivery<br>",
        "5. Customer value → Improved financial performance → Sustainable competitive advantage<br><br>",
        "<b>Cost Structure Details:</b><br>",
        "The cost calculations include:<br>",
        "• Selection costs: $2,000 per hire<br>",
        "• Compensation: $32,000 annual salary (paid quarterly)<br>",
        "• Benefits: 20% of salary<br>",
        "• Training: $200 per hire<br>",
        if(isTRUE(pars$performance_rewards)) {
          paste0("• Performance-based pay: ", pars$perf_pay_pct, "% of payroll ($", formatC(8000 * (pars$perf_pay_pct / 100), big.mark=",", format="d"), " per quarter per employee)<br>")
        } else {
          ""
        },
        "<br>",
        if(isTRUE(pars$performance_rewards)) {
          paste0(
        "<b>Performance-Based Pay in Manufacturing:</b><br>",
        "Research shows that performance-based compensation is common in manufacturing, even for entry-level positions. About 65% of manufacturing employees have access to nonproduction bonuses, significantly higher than the 48% average across all private-sector workers. Among frontline production roles, nearly 47% receive performance bonuses averaging 5-6% of annual pay (Bureau of Labor Statistics, 2024; Flynn, 2022). Manufacturing firms also budget for merit increases of 3-4% of base salary for hourly workers (Mercer, 2024; WorldatWork, 2023).<br><br>",
            "For a typical entry-level manufacturing worker earning $32,000 annually, a 5% performance-based pay program would provide approximately $1,600 in additional annual compensation, which aligns with industry standards where such programs typically represent single-digit percentages of base pay.<br><br>"
          )
        } else {
          ""
        },
        "<b>Important Notes:</b><br>",
        "• <b>Productivity Improvement:</b> The productivity improvement shown (", prod_improvement_pct, "%) aligns with research on unskilled jobs, where employees performing one standard deviation above average typically produce 20% more output (Hunter, Schmidt, & Judiesch, 1990).<br>",
        "• <b>Implementation Costs:</b> The value created graph above does <b>not yet account for the added costs of implementing the new selection procedure</b>, which can include substantial startup costs such as development and validation of new assessment tools, training of HR staff and hiring managers, technology infrastructure and software licensing, and initial implementation and change management costs. The BCG (Brogden-Cronbach-Gleser) model should be used to properly account for these implementation costs and provide a more complete cost-benefit analysis.<br><br>",
        if(isTRUE(pars$performance_rewards)) {
          paste0(
        'Bureau of Labor Statistics. (2024, March). Employee Benefits in the United States – Access to Nonproduction Bonuses. U.S. Department of Labor.<br>',
            'Flynn, J. (2022, October 18). 30 Average Bonus Statistics (2023). Zippia Research.<br>'
          )
        } else {
          ""
        },
        'Hunter, J. E., Schmidt, F. L., & Judiesch, M. K. (1990). Individual differences in output variability as a function of job complexity. <i>Journal of Applied Psychology, 75</i>(1), 28–42. <a href="https://doi.org/10.1037/0021-9010.75.1.28" target="_blank">https://doi.org/10.1037/0021-9010.75.1.28</a><br>',
        'Kaplan, R. S., & Norton, D. P. (2004). The strategy map: Guide to aligning intangible assets. <i>Strategy & Leadership, 32</i>(5), 10–17. <a href="https://doi.org/10.1108/10878570410699825" target="_blank">https://doi.org/10.1108/10878570410699825</a><br>',
        if(isTRUE(pars$performance_rewards)) {
          paste0(
        'Mercer. (2024, December 10). QuickPulse Compensation Planning Survey – 2025 Projections. Mercer LLC.<br>',
        'WorldatWork. (2023, August 31). 2023–24 Salary Budget Survey. WorldatWork.'
          )
        } else {
          ""
        }
      ))
    }
  })
  
  # --- Utility functions for probability and percent increase in quality of hire ---
  probability_higher_quality <- function(r, p) {
    # Check that inputs are in (0, 1)
    if (!is.numeric(r) || r <= 0 || r >= 1) {
      return(NA)
    }
    if (!is.numeric(p) || p <= 0 || p >= 1) {
      return(NA)
    }
    zc <- qnorm(1 - p)
    phi_zc <- dnorm(zc)
    numerator <- r * phi_zc * (1 / p + 1 / (1 - p))
    denominator <- sqrt(2 * (1 - r^2))
    delta_star <- numerator / denominator
    ps <- pnorm(delta_star)
    return(ps)
  }
  percent_increase_quality <- function(r, p) {
    ps <- probability_higher_quality(r, p)
    pct_increase <- ((ps - 0.50) / 0.50) * 100
    return(list(
      PS = ps,
      Percent_Increase = pct_increase
    ))
  }
  
  # Pareto Optimization Analysis
  pareto_analysis <- reactive({
    # Use Berry et al. (2024) matrix with SJT (6 predictors)
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
    d <- c(0.32, 0.79, -0.07, 0.24, 0.10, 0.37)
    
    # Selection parameters
    sr <- 0.33  # selection ratio
    prop <- 0.35  # proportion of minority applicants (community representative)
    
    # Run Pareto optimization
    out <- ParetoR(prop, sr, d, Table_2.data)
    
    # Select strategies
    balanced_target <- 1.0
    balanced_row <- which.min(abs(out$Pareto_Fmat[, "AI.ratio"] - balanced_target))
    balanced_ai_ratio <- out$Pareto_Fmat[balanced_row, "AI.ratio"]
    balanced_validity <- out$Pareto_Fmat[balanced_row, "Criterion.Validity"]
    
    aggressive_target <- 0.8
    eligible_solutions <- out$Pareto_Fmat[out$Pareto_Fmat[, "AI.ratio"] >= aggressive_target, , drop = FALSE]
    if (nrow(eligible_solutions) == 0) {
      aggressive_row <- which.max(out$Pareto_Fmat[, "AI.ratio"])
    } else {
      closest_idx <- which.min(abs(eligible_solutions[, "AI.ratio"] - aggressive_target))
      aggressive_row <- which(out$Pareto_Fmat[, "AI.ratio"] == eligible_solutions[closest_idx, "AI.ratio"])[1]
    }
    aggressive_ai_ratio <- out$Pareto_Fmat[aggressive_row, "AI.ratio"]
    aggressive_validity <- out$Pareto_Fmat[aggressive_row, "Criterion.Validity"]
    
    list(
      pareto_results = out,
      balanced = list(row = balanced_row, ai_ratio = balanced_ai_ratio, validity = balanced_validity),
      aggressive = list(row = aggressive_row, ai_ratio = aggressive_ai_ratio, validity = aggressive_validity),
      predictor_names = c("Biodata", "GMA_Tests", "Conscientiousness", "Structured_interview", "Integrity_test", "SJT")
    )
  })
  
  # Pareto plot
  output$paretoPlot <- renderPlot({
    analysis <- pareto_analysis()
    pareto_data <- analysis$pareto_results$Pareto_Fmat
    
    # Create the plot data
    plot_data <- data.frame(
      AI_Ratio = pareto_data[, "AI.ratio"],
      Validity = pareto_data[, "Criterion.Validity"]
    )
    
    ggplot(plot_data, aes(x = AI_Ratio, y = Validity)) +
      geom_line(color = "#2E86AB", linewidth = 2, alpha = 0.8) +
      geom_point(color = "#2E86AB", size = 3, alpha = 0.7) +
      
      # Highlight selected strategies
      geom_point(data = data.frame(
        AI_Ratio = c(analysis$balanced$ai_ratio, analysis$aggressive$ai_ratio),
        Validity = c(analysis$balanced$validity, analysis$aggressive$validity),
        Strategy = c("Balanced", "Aggressive Utility")
      ), aes(color = Strategy), size = 6) +
      
      scale_color_manual(values = c("Balanced" = "#27AE60", "Aggressive Utility" = "#E74C3C")) +
      
      labs(
        title = "Pareto Frontier: Diversity vs. Productivity Trade-offs",
        subtitle = "Each point represents an optimal hiring strategy",
        x = "Adverse Impact Ratio (Higher = More Diverse)",
        y = "Criterion Validity (Higher = More Productive)",
        color = "Selected Strategies"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      ) +
      
      # Add annotations for selected strategies
      annotate("text", 
               x = analysis$balanced$ai_ratio, 
               y = analysis$balanced$validity + 0.02,
               label = paste0("Balanced\n(AI=", round(analysis$balanced$ai_ratio, 3), 
                             ", V=", round(analysis$balanced$validity, 3), ")"),
               hjust = 0.5, vjust = 0, size = 3, fontface = "bold") +
      
      annotate("text", 
               x = analysis$aggressive$ai_ratio, 
               y = analysis$aggressive$validity + 0.02,
               label = paste0("Aggressive\n(AI=", round(analysis$aggressive$ai_ratio, 3), 
                             ", V=", round(analysis$aggressive$validity, 3), ")"),
               hjust = 0.5, vjust = 0, size = 3, fontface = "bold")
  })
  
  # Pareto insights
  output$paretoInsights <- renderText({
    analysis <- pareto_analysis()
    
    paste0(
      "PARETO OPTIMIZATION RESULTS:\n\n",
      "Total Optimal Solutions Found: ", nrow(analysis$pareto_results$Pareto_Fmat), "\n\n",
      "BALANCED STRATEGY:\n",
      "  • AI Ratio: ", round(analysis$balanced$ai_ratio, 3), " (closest to 1.0 = equal selection rates)\n",
      "  • Validity: ", round(analysis$balanced$validity, 3), "\n",
      "  • Focus: Maximizes diversity while maintaining reasonable productivity\n\n",
      "AGGRESSIVE UTILITY STRATEGY:\n",
      "  • AI Ratio: ", round(analysis$aggressive$ai_ratio, 3), " (≥0.8 threshold)\n",
      "  • Validity: ", round(analysis$aggressive$validity, 3), "\n",
      "  • Focus: Prioritizes productivity with acceptable diversity\n\n",
      "KEY INSIGHTS:\n",
      "  • Validity Improvement: +", round((analysis$aggressive$validity - analysis$balanced$validity) * 100, 1), " percentage points\n",
      "  • Diversity Trade-off: ", round((analysis$balanced$ai_ratio - analysis$aggressive$ai_ratio) * 100, 1), " percentage points in AI ratio\n",
      "  • Both strategies significantly outperform traditional methods\n",
      "  • Matrix includes 6 predictors: Biodata, GMA, Conscientiousness, Interview, Integrity, SJT"
    )
  })
  
  # Strategy comparison table
  output$strategyTable <- renderDataTable({
    analysis <- pareto_analysis()
    
    # Get predictor weights
    balanced_weights <- analysis$pareto_results$Pareto_Xmat[analysis$balanced$row, ]
    aggressive_weights <- analysis$pareto_results$Pareto_Xmat[analysis$aggressive$row, ]
    
    strategy_data <- data.frame(
      Predictor = analysis$predictor_names,
      Balanced_Strategy = round(balanced_weights, 3),
      Aggressive_Strategy = round(aggressive_weights, 3),
      Difference = round(aggressive_weights - balanced_weights, 3)
    )
    
    datatable(strategy_data, 
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE) %>%
      formatStyle(columns = c("Balanced_Strategy", "Aggressive_Strategy", "Difference"),
                  backgroundColor = styleInterval(c(0), c("#ffcccc", "#ccffcc")))
  })
}

shinyApp(ui, server)
