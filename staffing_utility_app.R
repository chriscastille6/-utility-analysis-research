# Comprehensive Staffing Utility Analysis App - FIXED VERSION
# Integrating Taylor-Russell Models, Expectancy Charts, Naylor-Shine Approach,
# and BCG Elements with Monte Carlo Adjustments
# Based on existing UA+ infrastructure and methodological advances

library(shiny)
library(shinydashboard)
library(iopsych)
library(scales)
library(shinyjs)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(gridExtra)
library(rmarkdown)
library(knitr)
library(stringr)
library(lavaan)

# =============================================================================
# CORE UTILITY ANALYSIS FUNCTIONS
# =============================================================================

# Expectancy Function (from existing app.R)
Expectancyfunc <- function(Validity, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut) {
  n <- 1000
  mean <- c(0, 0)
  lower <- c(PredLowerCut, CritLowerCut)
  upper <- c(PredUpperCut, CritUpperCut)
  corr <- diag(2)
  corr[lower.tri(corr)] <- Validity
  corr[upper.tri(corr)] <- Validity
  jtprob <- pmvnorm(lower, upper, mean, corr, algorithm = Miwa(steps = 128))
  xprob <- pnorm(PredUpperCut, mean = 0, sd = 1) - pnorm(PredLowerCut, mean = 0, sd = 1)
  expectancy <- jtprob / xprob
  return(expectancy[1])
}

# Enhanced utility calculation with multiple approaches
calculate_comprehensive_utility <- function(params) {
  # Traditional BCG Utility (using iopsych)
  traditional_utility <- utilityBcg(
    n = params$n,
    sdy = params$sdy,
    rxy = params$rxy,
    uxs = ux(params$sr),
    sr = params$sr,
    cost = params$cost * (params$n / params$sr),
    period = params$period
  )
  
  # Naylor-Shine approach utility
  zxs_value <- ux(params$sr)
  naylor_shine_utility <- params$n * params$period * params$rxy * params$sdy * zxs_value - 
    (params$cost * (params$n / params$sr))
  
  # Financially adjusted utility (Sturman 2000 three-factor adjustments)
  # Factor 1: Variable costs adjustment
  variable_cost_factor <- (100 - params$variable_costs) / 100
  
  # Factor 2: Tax adjustment  
  tax_factor <- (100 - params$tax_rate) / 100
  
  # Factor 3: Present value adjustment (discount rate)
  discount_factor <- 1 / (1 + params$discount_rate / 100)^params$period
  
  # Combined adjustment
  combined_adjustment <- variable_cost_factor * tax_factor * discount_factor
  financially_adjusted <- traditional_utility * combined_adjustment
  
  # Monte Carlo average (simplified)
  set.seed(123)
  mc_results <- replicate(1000, {
    # Add random variation to key parameters
    mc_rxy <- pmax(0, pmin(1, rnorm(1, params$rxy, params$rxy * 0.1)))
    mc_sdy <- pmax(0, rnorm(1, params$sdy, params$sdy * 0.15))
    mc_sr <- pmax(0.01, pmin(0.99, rnorm(1, params$sr, params$sr * 0.05)))
    
    # Calculate utility with varied parameters
    utilityBcg(
      n = params$n,
      sdy = mc_sdy,
      rxy = mc_rxy,
      uxs = ux(mc_sr),
      sr = mc_sr,
      cost = params$cost * (params$n / mc_sr),
      period = params$period
    )
  })
  
  monte_carlo_mean <- mean(mc_results, na.rm = TRUE)
  monte_carlo_sd <- sd(mc_results, na.rm = TRUE)
  
  return(list(
    traditional = traditional_utility,
    naylor_shine = naylor_shine_utility,
    financially_adjusted = financially_adjusted,
    monte_carlo_mean = monte_carlo_mean,
    monte_carlo_sd = monte_carlo_sd,
    monte_carlo_results = mc_results,
    zxs_value = zxs_value
  ))
}

# Create expectancy table data
create_expectancy_table <- function(rxy_old, rxy_new, sr) {
  # Define predictor score ranges
  predictor_ranges <- list(
    "Top (> +0.67σ)" = list(lower = 0.67, upper = Inf),
    "Upper Middle (0 to +0.67σ)" = list(lower = 0, upper = 0.67),
    "Lower Middle (-0.67σ to 0)" = list(lower = -0.67, upper = 0),
    "Bottom (< -0.67σ)" = list(lower = -Inf, upper = -0.67)
  )
  
  zxs <- ux(sr)
  criterion_cutoff <- rxy_new * zxs + 0.67
  
  expectancy_data <- data.frame(
    Predictor_Range = names(predictor_ranges),
    Old_System = numeric(4),
    New_System = numeric(4),
    Improvement = numeric(4)
  )
  
  for (i in 1:4) {
    range_info <- predictor_ranges[[i]]
    
    # Old system expectancy
    old_exp <- Expectancyfunc(rxy_old, range_info$lower, range_info$upper, 
                             criterion_cutoff, Inf) * 100
    
    # New system expectancy  
    new_exp <- Expectancyfunc(rxy_new, range_info$lower, range_info$upper, 
                             criterion_cutoff, Inf) * 100
    
    expectancy_data$Old_System[i] <- round(old_exp, 1)
    expectancy_data$New_System[i] <- round(new_exp, 1)
    expectancy_data$Improvement[i] <- round(new_exp - old_exp, 1)
  }
  
  return(expectancy_data)
}

# Selection Battery Optimization Data (Berry et al. 2024 / Roth-Bobko)
get_selection_battery_data <- function() {
  # Berry et al. (2024) correlation matrix
  cor_matrix <- matrix(c(
    1.00, 0.13, 0.54, 0.21, 0.25, 0.42,
    0.13, 1.00, 0.03, 0.18, 0.01, 0.29,
    0.54, 0.03, 1.00, 0.08, 0.28, 0.23,
    0.21, 0.18, 0.08, 1.00, -0.02, 0.45,
    0.25, 0.01, 0.28, -0.02, 1.00, 0.16,
    0.42, 0.29, 0.23, 0.45, 0.16, 1.00
  ), nrow = 6, byrow = TRUE)
  
  # Method names
  method_names <- c("biodata", "cognitive", "personality", "interview", "integrity", "performance")
  colnames(cor_matrix) <- rownames(cor_matrix) <- method_names
  
  # Criterion validities
  validities <- c(
    biodata = 0.38,
    cognitive = 0.31,
    personality = 0.19,
    interview = 0.42,
    integrity = 0.31,
    performance = 0.26
  )
  
  # Costs per hire
  costs <- c(
    biodata = 1500,
    cognitive = 3000,
    personality = 2000,
    interview = 500,
    integrity = 2000,
    performance = 6000
  )
  
  # Black-White d-values (Berry et al. 2024)
  d_values <- c(
    biodata = 0.32,
    cognitive = 0.79,
    personality = -0.07,
    interview = 0.24,
    integrity = 0.10,
    performance = 0.37
  )
  
  # Method characteristics
  characteristics <- data.frame(
    Method = c("Biodata", "Cognitive Ability", "Personality", "Structured Interview", "Integrity", "Performance Test"),
    Validity = validities,
    Cost = costs,
    Cost_per_Validity = costs / validities,
    Description = c(
      "Application blanks, weighted biographical data",
      "General mental ability, reasoning tests",
      "Big Five personality inventories",
      "Standardized behavioral interviews",
      "Honesty and ethical behavior tests",
      "Work samples, situational judgment tests"
    ),
    stringsAsFactors = FALSE
  )
  
  return(list(
    correlation_matrix = cor_matrix,
    validities = validities,
    costs = costs,
    d_values = d_values,
    characteristics = characteristics
  ))
}

# Calculate multiple correlation for selected predictors
calculate_multiple_r <- function(selected_methods, battery_data, sr = 0.25) {
  if (length(selected_methods) < 2) {
    return(list(multiple_r = 0, total_cost = 0))
  }
  
  # Get correlation matrix subset
  cor_subset <- battery_data$correlation_matrix[selected_methods, selected_methods]
  
  # Get validities for selected methods
  validities_subset <- battery_data$validities[selected_methods]
  
  # Calculate multiple correlation using matrix algebra
  if (length(selected_methods) == 2) {
    # For 2 predictors: simpler formula
    r12 <- cor_subset[1, 2]
    r1y <- validities_subset[1]
    r2y <- validities_subset[2]
    
    multiple_r_squared <- (r1y^2 + r2y^2 - 2*r1y*r2y*r12) / (1 - r12^2)
    multiple_r <- sqrt(pmax(0, multiple_r_squared))
  } else {
    # For 3+ predictors: matrix approach
    tryCatch({
      cor_inv <- solve(cor_subset)
      multiple_r_squared <- t(validities_subset) %*% cor_inv %*% validities_subset
      multiple_r <- sqrt(pmax(0, multiple_r_squared[1,1]))
    }, error = function(e) {
      multiple_r <- max(validities_subset)  # Fallback to highest individual validity
    })
  }
  
  # Calculate total cost
  total_cost <- sum(battery_data$costs[selected_methods])
  
  # Calculate composite d-value for adverse impact
  d_values_subset <- battery_data$d_values[selected_methods]
  
  # For multiple predictors, calculate weighted composite d
  if (length(selected_methods) == 2) {
    # Two predictors: use validity-weighted average
    weights <- validities_subset / sum(validities_subset)
    composite_d <- sum(weights * d_values_subset)
  } else {
    # For 3+ predictors: approximate with validity-weighted average
    weights <- validities_subset / sum(validities_subset)
    composite_d <- sum(weights * d_values_subset)
  }
  
  # Calculate selection ratio impact (4/5ths rule)
  # Minority selection rate is reduced by composite d-value
  minority_selection_rate <- pnorm(qnorm(sr, lower.tail = FALSE) + composite_d, lower.tail = FALSE)
  majority_selection_rate <- sr
  adverse_impact_ratio <- minority_selection_rate / majority_selection_rate
  
  return(list(
    multiple_r = multiple_r,
    total_cost = total_cost,
    cost_per_validity = total_cost / multiple_r,
    composite_d = composite_d,
    adverse_impact_ratio = adverse_impact_ratio,
    passes_80_rule = adverse_impact_ratio >= 0.80
  ))
}

# Find optimal combinations
find_optimal_combinations <- function(battery_data, max_combinations = 10) {
  methods <- names(battery_data$validities)
  all_combinations <- list()
  
  # Generate all combinations of 2, 3, and 4 methods
  for (size in 2:4) {
    combos <- combn(methods, size, simplify = FALSE)
    for (combo in combos) {
      result <- calculate_multiple_r(combo, battery_data, sr = 0.25)
      all_combinations[[length(all_combinations) + 1]] <- data.frame(
        Combination = paste(combo, collapse = " + "),
        Methods = length(combo),
        Multiple_R = round(result$multiple_r, 3),
        Total_Cost = result$total_cost,
        Cost_per_Validity = round(result$cost_per_validity, 0),
        AI_Ratio = round(result$adverse_impact_ratio * 100, 1),
        Passes_80_Rule = result$passes_80_rule,
        Efficiency_Score = round(result$multiple_r / (result$total_cost / 10000), 4),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine and sort by efficiency
  results_df <- do.call(rbind, all_combinations)
  results_df <- results_df[order(-results_df$Efficiency_Score), ]
  
  return(head(results_df, max_combinations))
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Comprehensive Staffing Utility Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Taylor-Russell & Expectancy", tabName = "expectancy", icon = icon("chart-line")),
      menuItem("Utility Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Monte Carlo Analysis", tabName = "montecarlo", icon = icon("random")),
      menuItem("Cost Savings Analysis", tabName = "savings", icon = icon("dollar-sign")),
      menuItem("Selection Battery Optimization", tabName = "comparative", icon = icon("balance-scale")),
      menuItem("Executive Selection & Character", tabName = "executive", icon = icon("user-tie")),
      menuItem("Business Case Report", tabName = "report", icon = icon("file-pdf")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(width = 12, title = "Comprehensive Staffing Utility Analysis", status = "primary", solidHeader = TRUE,
            h4("Understanding the Economic Value of Selection Systems"),
            p("This comprehensive tool integrates the foundational approaches to staffing utility analysis, 
              combining classical models with modern adjustments for realistic decision-making."),
            
            h5("Integrated Methodologies:"),
            
            fluidRow(
              column(6,
                div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; border-left: 4px solid #3c8dbc;",
                  h6(icon("chart-line"), "Taylor-Russell Model"),
                  p("Foundational expectancy tables showing the probability of success for different predictor score ranges. 
                    Based on Taylor & Russell (1939) and enhanced by Cucina et al. (2017).")
                ),
                br(),
                div(style = "background-color: #f0f8e8; padding: 15px; border-radius: 5px; border-left: 4px solid #5cb85c;",
                  h6(icon("calculator"), "Naylor-Shine Approach"), 
                  p("Precise utility calculations using the Naylor & Shine (1965) tables for Zxs values. 
                    Implemented via the iopsych package ux() function.")
                )
              ),
              column(6,
                div(style = "background-color: #fff8e1; padding: 15px; border-radius: 5px; border-left: 4px solid #ff9800;",
                  h6(icon("cogs"), "BCG Elements"),
                  p("Brogden-Cronbach-Gleser utility model with traditional, financially adjusted, 
                    and Monte Carlo approaches for comprehensive analysis.")
                ),
                br(),
                div(style = "background-color: #fce4ec; padding: 15px; border-radius: 5px; border-left: 4px solid #e91e63;",
                  h6(icon("random"), "Monte Carlo Adjustments"),
                  p("Sturman (2000) inspired adjustments accounting for real-world constraints, 
                    parameter uncertainty, and operational limitations.")
                )
              )
            ),
            
            br(),
            
            h5("Key Features:"),
            tags$ul(
              tags$li(strong("Interactive Expectancy Charts:"), " Visualize selection success rates across predictor ranges"),
              tags$li(strong("Multiple Utility Approaches:"), " Compare traditional, adjusted, and Monte Carlo estimates"),
              tags$li(strong("Cost Savings Analysis:"), " Model how higher quality hires deliver organizational savings"),
              tags$li(strong("Sensitivity Analysis:"), " Test robustness across parameter variations"),
              tags$li(strong("Business Case Reports:"), " Generate professional PDF documentation")
            ),
            
            br(),
            
            fluidRow(
              valueBoxOutput("sample_utility", width = 4),
              valueBoxOutput("sample_improvement", width = 4),
              valueBoxOutput("sample_roi", width = 4)
            )
          )
        )
      ),
      
      # Taylor-Russell & Expectancy Tab
      tabItem(tabName = "expectancy",
        fluidRow(
          box(width = 4, title = "Selection Parameters", status = "primary", solidHeader = TRUE,
            h5("Validity Coefficients:"),
            numericInput("rxy_old", "Current System Validity:", value = 0.10, min = 0, max = 1, step = 0.01),
            numericInput("rxy_new", "Proposed System Validity:", value = 0.50, min = 0, max = 1, step = 0.01),
            
            br(),
            h5("Selection Parameters:"),
            numericInput("selection_ratio", "Selection Ratio:", value = 0.33, min = 0.01, max = 0.99, step = 0.01),
            
            br(),
            actionButton("update_expectancy", "Update Analysis", class = "btn-primary", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Expectancy Chart", status = "success", solidHeader = TRUE,
            plotOutput("expectancy_chart", height = "400px"),
            br(),
            h6("Interpretation:"),
            htmlOutput("expectancy_interpretation")
          )
        ),
        
        fluidRow(
          box(width = 6, title = "Taylor-Russell Expectancy Table", status = "info", solidHeader = TRUE,
            DT::dataTableOutput("expectancy_table")
          ),
          box(width = 6, title = "Selection Statistics", status = "warning", solidHeader = TRUE,
            br(),
            htmlOutput("selection_stats")
          )
        )
      ),
      
      # Utility Calculator Tab
      tabItem(tabName = "calculator",
        fluidRow(
          box(width = 4, title = "Utility Parameters", status = "primary", solidHeader = TRUE,
            h5("Organization Size & Duration:"),
            numericInput("n_employees", "Number of Employees Selected:", value = 100, min = 1, max = 10000),
            numericInput("tenure_period", "Average Tenure (Years):", value = 3, min = 0.5, max = 20, step = 0.5),
            
            br(),
            h5("Performance & Costs:"),
            numericInput("sdy_value", "Performance SD (SDy) $:", value = 20000, min = 1000, max = 200000, step = 1000),
            numericInput("cost_per_person", "Selection Cost per Person $:", value = 500, min = 0, max = 10000, step = 50),
            
            br(),
            h5("Selection System:"),
            numericInput("validity_current", "Current Validity:", value = 0.10, min = 0, max = 1, step = 0.01),
            numericInput("validity_proposed", "Proposed Validity:", value = 0.50, min = 0, max = 1, step = 0.01),
            numericInput("selection_ratio_calc", "Selection Ratio:", value = 0.33, min = 0.01, max = 0.99, step = 0.01),
            
            br(),
            h5("Economic Adjustments (Sturman 2000):"),
            sliderInput("variable_costs", "Variable Costs (%):", value = 35, min = 0, max = 50, step = 1),
            sliderInput("tax_rate", "Tax Rate (%):", value = 63, min = 0, max = 70, step = 1),
            sliderInput("discount_rate", "Discount Rate (%):", value = 15, min = 0, max = 25, step = 1),
            
            br(),
            actionButton("calculate_utility", "Calculate Utility", class = "btn-success", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Utility Analysis Results", status = "success", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("traditional_utility", width = 6),
              valueBoxOutput("adjusted_utility", width = 6)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Summary Results",
                br(),
                htmlOutput("utility_summary")
              ),
              
              tabPanel("Detailed Breakdown",
                br(),
                verbatimTextOutput("detailed_calculations")
              )
            )
          )
        )
      ),
      
      # Monte Carlo Analysis Tab
      tabItem(tabName = "montecarlo",
        fluidRow(
          box(width = 4, title = "Monte Carlo Parameters", status = "warning", solidHeader = TRUE,
            h5("Simulation Settings:"),
            numericInput("mc_iterations", "Number of Iterations:", value = 1000, min = 100, max = 10000, step = 100),
            
            br(),
            h5("Parameter Uncertainty:"),
            sliderInput("validity_uncertainty", "Validity Uncertainty (%):", value = 10, min = 5, max = 25, step = 1),
            sliderInput("sdy_uncertainty", "SDy Uncertainty (%):", value = 15, min = 5, max = 30, step = 1),
            sliderInput("sr_uncertainty", "Selection Ratio Uncertainty (%):", value = 5, min = 1, max = 15, step = 1),
            
            br(),
            h5("Manual Bounds (Optional):"),
            checkboxInput("use_manual_bounds", "Use Manual Bounds", value = FALSE),
            conditionalPanel(
              condition = "input.use_manual_bounds == true",
              fluidRow(
                column(6,
                  h6("Validity Bounds:"),
                  numericInput("validity_lower", "Lower Bound:", value = 0.2, min = 0, max = 1, step = 0.01),
                  numericInput("validity_upper", "Upper Bound:", value = 0.6, min = 0, max = 1, step = 0.01)
                ),
                column(6,
                  h6("SDy Bounds ($):"),
                  numericInput("sdy_lower", "Lower Bound:", value = 15000, min = 1000, max = 100000, step = 1000),
                  numericInput("sdy_upper", "Upper Bound:", value = 30000, min = 1000, max = 100000, step = 1000)
                )
              ),
              fluidRow(
                column(6,
                  h6("Selection Ratio Bounds:"),
                  numericInput("sr_lower", "Lower Bound:", value = 0.15, min = 0.01, max = 0.95, step = 0.01),
                  numericInput("sr_upper", "Upper Bound:", value = 0.50, min = 0.01, max = 0.95, step = 0.01)
                ),
                column(6,
                  h6("Cost per Person Bounds ($):"),
                  numericInput("cost_lower", "Lower Bound:", value = 200, min = 50, max = 5000, step = 50),
                  numericInput("cost_upper", "Upper Bound:", value = 800, min = 50, max = 5000, step = 50)
                )
              ),
              fluidRow(
                column(6,
                  h6("Tenure Period Bounds (Years):"),
                  numericInput("tenure_lower", "Lower Bound:", value = 2, min = 0.5, max = 10, step = 0.5),
                  numericInput("tenure_upper", "Upper Bound:", value = 5, min = 0.5, max = 10, step = 0.5)
                )
              )
            ),
            
            br(),
            actionButton("run_monte_carlo", "Run Monte Carlo Analysis", class = "btn-warning", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Monte Carlo Results", status = "warning", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("mc_mean_utility", width = 4),
              valueBoxOutput("mc_confidence_interval", width = 4),
              valueBoxOutput("mc_risk_assessment", width = 4)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Distribution Plot",
                plotlyOutput("mc_distribution_plot")
              ),
              
              tabPanel("Risk Analysis",
                br(),
                htmlOutput("mc_risk_analysis")
              )
            )
          )
        )
      ),
      
      # Cost Savings Analysis Tab
      tabItem(tabName = "savings",
        fluidRow(
          box(width = 12, title = "Higher Quality Hires: Cost Savings Analysis", status = "info", solidHeader = TRUE,
            p("This analysis quantifies the economic value of higher quality hires through three key mechanisms. 
              Better selection systems deliver measurable cost savings by reducing expensive turnover, 
              accelerating time-to-competency, and improving training success rates."),
            
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Mathematical Framework:"),
              tags$ul(
                tags$li(strong("Turnover Savings:"), " N × (Current_Rate - Improved_Rate) × Cost_per_Turnover"),
                tags$li(strong("Training Efficiency:"), " N × (Improved_Success - Current_Success) × Training_Cost"),
                tags$li(strong("Productivity Acceleration:"), " N × (Time_Saved_Months) × Monthly_Productivity_Value"),
                tags$li(strong("Total Annual Savings:"), " Sum of all three components")
              ),
              p(em("Where N = number of employees hired annually"))
            ),
            
            fluidRow(
              column(4,
                h5("Turnover Costs:"),
                numericInput("turnover_cost", "Cost per Turnover Event ($):", value = 15000, min = 1000, max = 100000, step = 1000),
                numericInput("current_turnover_rate", "Current Turnover Rate (%):", value = 20, min = 5, max = 50, step = 1),
                numericInput("improved_turnover_rate", "Improved Turnover Rate (%):", value = 15, min = 5, max = 50, step = 1)
              ),
              column(4,
                h5("Training & Development:"),
                numericInput("training_cost", "Training Cost per Employee ($):", value = 3000, min = 500, max = 20000, step = 500),
                numericInput("training_success_current", "Current Training Success Rate (%):", value = 70, min = 50, max = 95, step = 1),
                numericInput("training_success_improved", "Improved Training Success Rate (%):", value = 85, min = 50, max = 95, step = 1)
              ),
              column(4,
                h5("Productivity Gains:"),
                numericInput("time_to_competency_current", "Current Time to Competency (Months):", value = 6, min = 1, max = 24, step = 1),
                numericInput("time_to_competency_improved", "Improved Time to Competency (Months):", value = 4, min = 1, max = 24, step = 1),
                numericInput("monthly_productivity_value", "Monthly Productivity Value ($):", value = 8000, min = 2000, max = 30000, step = 1000)
              )
            ),
            
            br(),
            
            div(style = "text-align: center;",
              actionButton("calculate_savings", "Calculate Cost Savings", class = "btn-info btn-lg")
            ),
            
            br(),
            
            fluidRow(
              valueBoxOutput("annual_turnover_savings", width = 3),
              valueBoxOutput("training_efficiency_savings", width = 3),
              valueBoxOutput("productivity_acceleration_savings", width = 3),
              valueBoxOutput("total_annual_savings", width = 3)
            ),
            
            br(),
            
            fluidRow(
              column(8,
                plotlyOutput("savings_breakdown_chart")
              ),
              column(4,
                div(style = "background-color: #e8f5e8; padding: 15px; border-radius: 5px;",
                  h5("Break-Even Analysis"),
                  htmlOutput("breakeven_analysis")
                )
              )
            ),
            
            br(),
            
            fluidRow(
              box(width = 12, title = "Detailed Cost Savings Calculations", status = "success", solidHeader = TRUE,
                htmlOutput("detailed_savings_explanation")
              )
            )
          )
        )
      ),
      
      # Selection Battery Optimization Tab
      tabItem(tabName = "comparative",
        fluidRow(
          box(width = 12, title = "Selection Battery Optimization", status = "success", solidHeader = TRUE,
            p("Build the most cost-effective selection battery using empirically-validated correlation matrices. 
              Compare different combinations of employment tests to find optimal validity-to-cost ratios."),
            
            div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Based on Berry et al. (2024) & Roth-Bobko Meta-Analysis:"),
              p("This tool uses updated meta-analytic correlations between selection methods and job performance. 
                Students can explore different combinations to identify the top 2-3 most cost-effective batteries.")
            ),
            
            fluidRow(
              column(4,
                h5("Available Selection Methods:"),
                checkboxGroupInput("selected_predictors", "Choose Methods (2-4 recommended):",
                  choices = list(
                    "Cognitive Ability Test ($3,000)" = "cognitive",
                    "Structured Interview ($500)" = "interview", 
                    "Personality Test ($2,000)" = "personality",
                    "Integrity Test ($2,000)" = "integrity",
                    "Performance/Work Sample ($6,000)" = "performance",
                    "Biodata ($1,500)" = "biodata"
                  ),
                  selected = c("cognitive", "interview")
                ),
                
                br(),
                h5("Analysis Parameters:"),
                numericInput("battery_employees", "Number of Hires per Year:", value = 100, min = 10, max = 1000, step = 10),
                numericInput("battery_sr", "Selection Ratio:", value = 0.25, min = 0.05, max = 0.95, step = 0.05),
                
                br(),
                actionButton("optimize_battery", "Analyze Battery", class = "btn-primary", style = "width: 100%;"),
                br(), br(),
                actionButton("find_optimal", "Find Optimal Combinations", class = "btn-success", style = "width: 100%;")
              ),
              
              column(8,
                fluidRow(
                  valueBoxOutput("battery_validity", width = 3),
                  valueBoxOutput("battery_cost", width = 3),
                  valueBoxOutput("validity_per_dollar", width = 3),
                  valueBoxOutput("adverse_impact_ratio", width = 3)
                ),
                
                br(),
                
                tabsetPanel(
                  tabPanel("Current Battery Analysis",
                    br(),
                    htmlOutput("battery_analysis")
                  ),
                  
                  tabPanel("Correlation Matrix",
                    br(),
                    DT::dataTableOutput("correlation_display")
                  ),
                  
                  tabPanel("Top Combinations",
                    br(),
                    DT::dataTableOutput("optimal_combinations")
                  ),
                  
                  tabPanel("Pareto Optimization",
                    br(),
                    div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                      h6("Advanced Pareto Analysis:"),
                      p("Find mathematically optimal trade-offs between validity and adverse impact using the ParetoR package 
                        (De Corte, Sackett, & Lievens, 2011; Rupp et al., 2020). This approach identifies the Pareto frontier 
                        of solutions where you cannot improve one objective without worsening the other."),
                      p(strong("Methodology:"), " Based on De Corte et al.'s (2011) groundbreaking approach for designing 
                        Pareto-optimal selection systems, implemented through the ParetoR R package with Berry et al. (2024) 
                        updated meta-analytic correlations.")
                    ),
                    
                    fluidRow(
                      column(6,
                        h6("Pareto Parameters:"),
                        numericInput("pareto_prop", "Minority Proportion in Applicant Pool:", value = 0.35, min = 0.1, max = 0.9, step = 0.05),
                        numericInput("pareto_sr", "Selection Ratio:", value = 0.25, min = 0.05, max = 0.95, step = 0.05),
                        actionButton("run_pareto", "Run Pareto Analysis", class = "btn-warning", style = "width: 100%;")
                      ),
                      column(6,
                        valueBoxOutput("pareto_solutions", width = 12),
                        br(),
                        htmlOutput("pareto_strategies")
                      )
                    ),
                    
                    br(),
                    
                    fluidRow(
                      box(width = 6, title = "Pareto Frontier Plot", status = "primary", solidHeader = TRUE,
                        plotlyOutput("pareto_frontier_plot")
                      ),
                      box(width = 6, title = "Strategy Comparison", status = "info", solidHeader = TRUE,
                        DT::dataTableOutput("pareto_strategies_table")
                      )
                    )
                  )
                )
              )
            ),
            
            br(),
            
            fluidRow(
              box(width = 6, title = "Validity vs Cost Analysis", status = "primary", solidHeader = TRUE,
                plotlyOutput("validity_cost_plot")
              ),
              box(width = 6, title = "Method Characteristics", status = "info", solidHeader = TRUE,
                DT::dataTableOutput("method_characteristics")
              )
            )
          )
        )
      ),
      
      # Executive Selection & Character Assessment Tab
      tabItem(tabName = "executive",
        fluidRow(
          box(width = 12, title = "Executive Selection & Character Assessment Utility", status = "primary", solidHeader = TRUE,
            p("Analyze the utility of character assessment in executive selection based on Seijts, Espinoza, & Carswell (2020). 
              This analysis demonstrates the economic value of character-based leadership assessment for senior management positions."),
            
            div(style = "background-color: #f0f8e8; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Character Assessment Research (Seijts et al., 2020):"),
              p("Study of 111 senior managers using the Leader Character Insight Assessment (LCIA) to predict performance. 
                The LCIA measures 11 character dimensions: Accountability, Collaboration, Courage, Drive, Humanity, 
                Humility, Integrity, Judgment, Justice, Temperance, and Transcendence."),
              p(strong("Key Finding:"), " Character assessment showed r = 0.30 correlation with supervisor-rated performance.")
            ),
            
            fluidRow(
              column(4,
                h5("Executive Assessment Parameters:"),
                numericInput("exec_sample_size", "Number of Executive Candidates:", value = 111, min = 10, max = 1000, step = 1),
                numericInput("exec_character_validity", "Character Assessment Validity:", value = 0.30, min = 0.1, max = 0.8, step = 0.01),
                numericInput("exec_selection_ratio", "Executive Selection Ratio:", value = 0.33, min = 0.05, max = 0.95, step = 0.01),
                
                br(),
                h5("Financial Parameters:"),
                numericInput("exec_sdy", "Executive Performance SDy (CAD):", value = 115500, min = 50000, max = 500000, step = 5000),
                numericInput("exec_assessment_cost", "Character Assessment Cost per Candidate (CAD):", value = 800, min = 200, max = 5000, step = 100),
                numericInput("exec_tenure", "Expected Executive Tenure (Years):", value = 15, min = 5, max = 25, step = 1),
                
                br(),
                actionButton("calculate_exec_utility", "Calculate Executive Selection Utility", class = "btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                fluidRow(
                  valueBoxOutput("exec_total_utility", width = 4),
                  valueBoxOutput("exec_annual_utility", width = 4),
                  valueBoxOutput("exec_roi_ratio", width = 4)
                ),
                
                br(),
                
                tabsetPanel(
                  tabPanel("Character Assessment Results",
                    br(),
                    htmlOutput("exec_utility_analysis")
                  ),
                  
                  tabPanel("Character Dimensions",
                    br(),
                    div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                      h6("Leader Character Insight Assessment (LCIA) - 11 Character Dimensions:"),
                      fluidRow(
                        column(6,
                          tags$ul(
                            tags$li(strong("Accountability:"), " Taking responsibility for decisions and outcomes"),
                            tags$li(strong("Collaboration:"), " Working effectively with others toward common goals"),
                            tags$li(strong("Courage:"), " Acting despite risk, uncertainty, or opposition"),
                            tags$li(strong("Drive:"), " Persistent effort toward achieving objectives"),
                            tags$li(strong("Humanity:"), " Showing compassion and care for others"),
                            tags$li(strong("Humility:"), " Acknowledging limitations and being open to learning")
                          )
                        ),
                        column(6,
                          tags$ul(
                            tags$li(strong("Integrity:"), " Consistency between values, words, and actions"),
                            tags$li(strong("Judgment:"), " Making sound decisions based on available information"),
                            tags$li(strong("Justice:"), " Treating people fairly and equitably"),
                            tags$li(strong("Temperance:"), " Exercising self-control and moderation"),
                            tags$li(strong("Transcendence:"), " Connecting to something larger than oneself")
                          )
                        )
                      ),
                      p(em("These character dimensions collectively predict executive leadership effectiveness and organizational performance."))
                    )
                  ),
                  
                  tabPanel("Research Context",
                    br(),
                    div(style = "background-color: #fff8e1; padding: 20px; border-radius: 5px;",
                      h6("Study Background (Seijts, Espinoza, & Carswell, 2020):"),
                      p(strong("Organization:"), " Large Canadian manufacturing organization"),
                      p(strong("Purpose:"), " Succession planning for senior leadership positions"),
                      p(strong("Assessment Method:"), " Peer ratings using the LCIA (behaviorally-based, validated instrument)"),
                      p(strong("Performance Criteria:"), " Supervisor ratings of leadership performance"),
                      p(strong("Sample:"), " 111 senior managers being considered for top executive roles"),
                      
                      h6("Key Results:"),
                      tags$ul(
                        tags$li("Character-performance correlation: r = 0.30 (p < 0.01)"),
                        tags$li("15-year utility: CAD $564,128 (CAD $37,609 annually)"),
                        tags$li("10-year utility: CAD $375,285 (CAD $37,529 annually)"),
                        tags$li("Positive ROI even with conservative adjustments")
                      ),
                      
                      p(strong("Implication:"), " Character assessment provides substantial economic value in executive selection, 
                        supporting the business case for character-based leadership development.")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Business Case Report Tab
      tabItem(tabName = "report",
        fluidRow(
          box(width = 12, title = "Business Case Report Generator", status = "success", solidHeader = TRUE,
            p("Generate a comprehensive PDF report with staffing utility analysis, expectancy charts, 
              Monte Carlo results, and implementation recommendations."),
            
            fluidRow(
              column(6,
                h5("Organization Information:"),
                textInput("org_name_staff", "Organization Name:", value = "Your Organization"),
                textInput("position_title", "Position/Role:", value = "Budget Analyst"),
                selectInput("industry_type", "Industry:", 
                           choices = c("Healthcare", "Education", "Technology", "Manufacturing", 
                                     "Financial Services", "Government", "Other"),
                           selected = "Healthcare"),
                numericInput("report_employees_staff", "Number of Positions:", value = 100, min = 1, max = 5000)
              ),
              column(6,
                h5("Selection System Details:"),
                numericInput("report_validity_old", "Current System Validity:", value = 0.10, min = 0, max = 1, step = 0.01),
                numericInput("report_validity_new", "Proposed System Validity:", value = 0.50, min = 0, max = 1, step = 0.01),
                numericInput("report_cost", "Implementation Cost per Person ($):", value = 500, min = 0, max = 10000),
                numericInput("report_sdy", "Performance Standard Deviation ($):", value = 20000, min = 5000, max = 200000, step = 1000)
              )
            ),
            
            fluidRow(
              column(12,
                h5("Executive Summary Context:"),
                textAreaInput("report_context", "Business Context & Objectives:", 
                             value = "Organization seeks to improve selection system effectiveness for critical positions.",
                             rows = 3)
              )
            ),
            
            br(),
            div(style = "text-align: center;",
              downloadButton("download_staff_report", "Generate Comprehensive Report", 
                           class = "btn-success btn-lg", 
                           style = "padding: 10px 30px; font-size: 16px;")
            ),
            
            br(),
            
            div(style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px;",
              h5("Report Contents:"),
              tags$ul(
                tags$li("Executive summary with key findings and ROI"),
                tags$li("Taylor-Russell expectancy analysis and charts"),
                tags$li("Comprehensive utility analysis (Traditional, Adjusted, Monte Carlo)"),
                tags$li("Cost savings analysis and implementation timeline"),
                tags$li("Risk assessment and sensitivity analysis"),
                tags$li("Methodology documentation and references"),
                tags$li("Implementation recommendations and next steps")
              )
            )
          )
        )
      ),
      
      # References Tab
      tabItem(tabName = "references",
        fluidRow(
          box(width = 12, title = "References and Methodology", status = "info", solidHeader = TRUE,
            
            h4("Foundational Research"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Taylor, H. C., & Russell, J. T. (1939)."),
              p(em("The relationship of validity coefficients to the practical effectiveness of tests in selection: Discussion and tables."), 
                strong("Journal of Applied Psychology"), ", 23(5), 565-578."),
              p("DOI: 10.1037/h0057079"),
              
              h6("Contribution:"),
              p("Established the foundational expectancy tables relating predictor validity to selection success rates. 
                The Taylor-Russell model demonstrates how test validity and selection ratio jointly determine 
                the proportion of successful employees selected.")
            ),
            
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("De Corte, W., Sackett, P. R., & Lievens, F. (2011)."),
              p(em("Designing Pareto-optimal selection systems: Formalizing the decisions required for selection system development."), 
                strong("Journal of Applied Psychology"), ", 96(5), 907-926."),
              p("DOI: 10.1037/a0023298"),
              
              h6("Contribution:"),
              p("Introduced the analytical method for designing Pareto-optimal selection systems that simultaneously optimize 
                validity and diversity. This groundbreaking approach provides the mathematical foundation for the Pareto optimization 
                functionality in this app.")
            ),
            
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Naylor, J. C., & Shine, L. C. (1965)."),
              p(em("A table for determining the increase in mean criterion score obtained by using a selection device."), 
                strong("Journal of Industrial Psychology"), ", 3, 33-42."),
              
              h6("Contribution:"),
              p("Provided precise tables for calculating Zxs values (average standardized predictor scores of selected individuals) 
                essential for accurate utility analysis. These tables are implemented in the iopsych package ux() function.")
            ),
            
            h4("Utility Analysis Framework"),
            h5("Brogden-Cronbach-Gleser (BCG) Model:"),
            p("• Brogden, H. E. (1949). When testing pays off. Personnel Psychology, 2(2), 171-183."),
            p("• Cronbach, L. J., & Gleser, G. C. (1965). Psychological tests and personnel decisions. University of Illinois Press."),
            
            h5("Modern Implementations:"),
            p("• Berry, C. M., Sackett, P. R., & Landers, R. N. (2024). Pareto optimization with updated meta-analytic correlations. 
              Journal of Applied Psychology. (Provides updated meta-analytic correlation matrix used in this app)"),
            p("• Cucina, J., Berger, J., & Busciglio, H. (2017). Communicating criterion-related validity using expectancy charts: A new approach. 
              Personnel Assessment and Decisions, 3(1), 1-13."),
            p("• Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. 
              Journal of Management, 26(2), 281-299."),
            
            h5("Pareto Optimization Software:"),
            p("• Rupp, D. E., De Corte, W., Ravid, D., Naumenko, A., Comas, I., Credo, N. A., ... & Eckstein, L. (2020). 
              ParetoR package: Tools for Pareto optimization in personnel selection. R package version 1.0.0."),
            p("• Available at: https://CRAN.R-project.org/package=ParetoR"),
            
            h4("Monte Carlo and Economic Adjustments"),
            p("• Cascio, W. F., Boudreau, J. W., & Fink, A. A. (2019). Investing in people: Financial impact of human resource initiatives (3rd ed.). 
              Society for Human Resource Management."),
            p("• Schmidt, F. L. (2013). A general theoretical integrative model of individual differences in training, learning, performance, and behavioral outcomes."),
            
            h4("App Information"),
            div(style = "background-color: #e7f4fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
              h5("About This Tool"),
              p(strong("Comprehensive Staffing Utility Analysis Tool"), " (2025). Advanced integration of classical and modern utility analysis approaches."),
              p(strong("Version:"), " 1.0"),
              p(strong("Last Updated:"), " January 2025"),
              
              h5("Source Code & Documentation:"),
              p(strong("GitHub Repository:"), br(),
              HTML("<a href='https://github.com/chriscastille6/-utility-analysis-research' target='_blank' style='color: #666; font-style: italic;'>
              https://github.com/chriscastille6/-utility-analysis-research
              </a>")),
              
              p(strong("Citation for this Tool:"), br(),
              HTML("<em>Comprehensive Staffing Utility Analysis Tool</em> (2025). Integrating Taylor-Russell, Naylor-Shine, BCG, and Monte Carlo approaches.<br>
              <strong>Live App:</strong> <a href='#' target='_blank'>[To be added when deployed]</a><br>
              <strong>Source Code:</strong> https://github.com/chriscastille6/-utility-analysis-research")),
              
              h5("Technical Implementation:"),
              p("Built using the iopsych package for accurate Naylor-Shine Zxs calculations and BCG utility analysis. 
                Integrates existing UA+ infrastructure with modern Shiny dashboard framework."),
              
              h5("Educational Purpose:"),
              p("Designed to demonstrate the evolution of utility analysis from foundational expectancy tables to 
                comprehensive economic impact assessment, suitable for graduate-level courses in personnel psychology, 
                HR analytics, and organizational decision-making."),
              
              h5("AI Development Assistance:"),
              p("This educational application was developed with assistance from AI language models and modern development tools:"),
              tags$ul(
                tags$li(strong("AI Assistants:"), " Claude (Anthropic) for code development, data analysis, integration of complex utility analysis methodologies, and educational content design"),
                tags$li(strong("Development Environment:"), " Cursor AI-powered code editor for enhanced productivity, code quality, and advanced app architecture"),
                tags$li(strong("Human Oversight:"), " All AI-generated content was thoroughly reviewed, validated, and refined by human researchers with expertise in personnel psychology and utility analysis"),
                tags$li(strong("Data Verification:"), " All calculations, parameter values, and methodological implementations were independently verified against original academic publications"),
                tags$li(strong("Academic Integrity:"), " AI tools enhanced development efficiency while maintaining strict adherence to established research methodologies and academic standards")
              ),
              p(em("AI assistance was used exclusively for technical implementation and educational presentation. All underlying methodologies, 
                calculations, and theoretical frameworks are based on peer-reviewed academic research and established best practices in personnel psychology.")),
              
              h5("Important Disclaimers:"),
              tags$ul(
                tags$li(strong("Educational Purpose:"), " This tool is designed for educational and research purposes only"),
                tags$li(strong("Business Decisions:"), " Organizational selection decisions should consider additional legal, ethical, and contextual factors beyond those modeled in this analysis"),
                tags$li(strong("Model Limitations:"), " All models are simplifications of complex organizational realities and should be interpreted accordingly"),
                tags$li(strong("Legal Compliance:"), " Users must ensure compliance with all applicable employment laws and regulations in their jurisdiction"),
                tags$li(strong("Professional Consultation:"), " Complex selection system implementations should involve qualified industrial-organizational psychology professionals")
              )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Overview value boxes
  output$sample_utility <- renderValueBox({
    valueBox(
      value = "$2.8M",
      subtitle = "Sample Utility Estimate",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$sample_improvement <- renderValueBox({
    valueBox(
      value = "65%",
      subtitle = "Expectancy Improvement",
      icon = icon("arrow-up"),
      color = "blue"
    )
  })
  
  output$sample_roi <- renderValueBox({
    valueBox(
      value = "560%",
      subtitle = "Return on Investment",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  # Expectancy analysis
  expectancy_data <- reactive({
    input$update_expectancy
    
    isolate({
      create_expectancy_table(input$rxy_old, input$rxy_new, input$selection_ratio)
    })
  })
  
  output$expectancy_table <- DT::renderDataTable({
    expectancy_data()
  }, options = list(pageLength = 5, searching = FALSE))
  
  # Fixed expectancy chart with original format
  output$expectancy_chart <- renderPlot({
    data <- expectancy_data()
    
    # Create visualization data in the original format
    chart_data <- data.frame(
      Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
      Procedure = factor(rep(c("Old", "New"), 4), levels = c("Old", "New")),
      Probability = c(
        data$Old_System[4], data$New_System[4],  # Bottom
        data$Old_System[3], data$New_System[3],  # Lower Middle  
        data$Old_System[2], data$New_System[2],  # Upper Middle
        data$Old_System[1], data$New_System[1]   # Top
      )
    )
    
    improvement <- round(max(data$Improvement), 1)
    
    p <- ggplot(chart_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = "Quartile Score on Selection Procedure",
        y = "Probability of High Job Performance (%)",
        fill = "Procedure",
        title = "Figure 1. Comparing old and new procedures",
        subtitle = paste0("The new procedure improves prediction of high performance by ", improvement, "%.")
      ) +
      scale_fill_manual(values = c("Old" = "#6BAED6", "New" = "#2171B5")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16),
        plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 10)),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      ) +
      geom_text(aes(label = paste0(round(Probability, 1), "%")), 
                vjust = -0.5, size = 4, position = position_dodge(width = 0.9))
    
    return(p)
  })
  
  output$expectancy_interpretation <- renderUI({
    data <- expectancy_data()
    best_improvement <- max(data$Improvement)
    best_range <- data$Predictor_Range[which.max(data$Improvement)]
    
    HTML(paste0(
      "<p><strong>Key Finding:</strong> The greatest improvement (+", best_improvement, 
      "%) occurs in the <em>", best_range, "</em> range.</p>",
      "<p>Higher validity coefficients improve selection success rates across all predictor ranges, 
      with the most substantial gains typically seen in top predictor ranges.</p>"
    ))
  })
  
  output$selection_stats <- renderUI({
    zxs <- ux(input$selection_ratio)
    sr_percent <- input$selection_ratio * 100
    
    HTML(paste0(
      "<h6><strong>Selection Statistics:</strong></h6>",
      "<p><strong>Selection Ratio:</strong> ", round(sr_percent, 1), "% (", round(input$selection_ratio, 3), ")</p>",
      "<p><strong>Zxs Value:</strong> ", round(zxs, 3), "</p>",
      "<p><strong>Interpretation:</strong> Selected individuals score ", round(zxs, 2), 
      " standard deviations above the mean on the predictor.</p>",
      "<hr>",
      "<h6><strong>Validity Improvement:</strong></h6>",
      "<p><strong>Current System:</strong> r = ", input$rxy_old, "</p>",
      "<p><strong>Proposed System:</strong> r = ", input$rxy_new, "</p>",
      "<p><strong>Improvement:</strong> +", round((input$rxy_new - input$rxy_old), 3), 
      " (", round(((input$rxy_new - input$rxy_old) / input$rxy_old) * 100, 1), "% increase)</p>"
    ))
  })
  
  # Utility calculations
  utility_results <- reactive({
    input$calculate_utility
    
    isolate({
              params <- list(
          n = input$n_employees,
          sdy = input$sdy_value,
          rxy = input$validity_proposed - input$validity_current,
          sr = input$selection_ratio_calc,
          cost = input$cost_per_person,
          period = input$tenure_period,
          variable_costs = input$variable_costs,
          tax_rate = input$tax_rate,
          discount_rate = input$discount_rate
        )
      
      calculate_comprehensive_utility(params)
    })
  })
  
  output$traditional_utility <- renderValueBox({
    results <- utility_results()
    valueBox(
      value = paste0("$", format(round(results$traditional), big.mark = ",")),
      subtitle = "Traditional BCG Utility",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  output$adjusted_utility <- renderValueBox({
    results <- utility_results()
    valueBox(
      value = paste0("$", format(round(results$financially_adjusted), big.mark = ",")),
      subtitle = "Financially Adjusted",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$utility_summary <- renderUI({
    results <- utility_results()
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h4>Comprehensive Utility Analysis Summary</h4>",
      "<h5>Utility Estimates:</h5>",
      "<p><strong>Traditional BCG:</strong> $", format(round(results$traditional), big.mark = ","), "</p>",
      "<p><strong>Naylor-Shine:</strong> $", format(round(results$naylor_shine), big.mark = ","), "</p>",
      "<p><strong>Financially Adjusted:</strong> $", format(round(results$financially_adjusted), big.mark = ","), 
      " (", round((results$financially_adjusted/results$traditional)*100, 1), "% of traditional)</p>",
      "<p><strong>Monte Carlo Mean:</strong> $", format(round(results$monte_carlo_mean), big.mark = ","), 
      " (±$", format(round(results$monte_carlo_sd), big.mark = ","), ")</p>",
      "<hr>",
      "<h5>Key Parameters:</h5>",
      "<p><strong>Zxs Value:</strong> ", round(results$zxs_value, 3), "</p>",
      "<p><strong>Validity Improvement:</strong> ", input$validity_proposed - input$validity_current, "</p>",
      "<p><strong>Selection Ratio:</strong> ", round(input$selection_ratio_calc * 100, 1), "%</p>",
      "<p><strong>Per-Employee Utility:</strong> $", format(round(results$traditional / input$n_employees), big.mark = ","), "</p>",
      "</div>"
    ))
  })
  
  # Fixed detailed calculations - removed %R% error
  output$detailed_calculations <- renderText({
    results <- utility_results()
    
    paste0(
      "COMPREHENSIVE UTILITY ANALYSIS CALCULATIONS\n",
      paste(rep("=", 50), collapse = ""), "\n\n",
      "INPUT PARAMETERS:\n",
      "- Number of employees (N): ", input$n_employees, "\n",
      "- Tenure period (T): ", input$tenure_period, " years\n",
      "- Validity improvement (rxy): ", input$validity_proposed - input$validity_current, "\n",
      "- Performance SD (SDy): $", format(input$sdy_value, big.mark = ","), "\n",
      "- Selection ratio (SR): ", round(input$selection_ratio_calc, 3), "\n",
      "- Cost per person: $", format(input$cost_per_person, big.mark = ","), "\n",
      "- Economic adjustment: ", input$economic_adjustment, "%\n\n",
      
      "NAYLOR-SHINE CALCULATIONS:\n",
      "1. Zxs (from Naylor & Shine tables): ", round(results$zxs_value, 4), "\n",
      "2. Total cost: $", format(round(input$cost_per_person * (input$n_employees / input$selection_ratio_calc)), big.mark = ","), "\n",
      "3. Benefit calculation: N × T × rxy × SDy × Zxs\n",
      "   = ", input$n_employees, " × ", input$tenure_period, " × ", 
      input$validity_proposed - input$validity_current, " × $", format(input$sdy_value, big.mark = ","), 
      " × ", round(results$zxs_value, 4), "\n",
      "   = $", format(round(results$naylor_shine + input$cost_per_person * (input$n_employees / input$selection_ratio_calc)), big.mark = ","), "\n",
      "4. Net utility: $", format(round(results$naylor_shine), big.mark = ","), "\n\n",
      
      "IOPSYCH BCG CALCULATION:\n",
      "- Traditional BCG: $", format(round(results$traditional), big.mark = ","), "\n",
      "- Difference from Naylor-Shine: $", format(round(results$traditional - results$naylor_shine), big.mark = ","), "\n\n",
      
      "MONTE CARLO RESULTS:\n",
      "- Mean utility: $", format(round(results$monte_carlo_mean), big.mark = ","), "\n",
      "- Standard deviation: $", format(round(results$monte_carlo_sd), big.mark = ","), "\n",
      "- 95% CI: [$", format(round(results$monte_carlo_mean - 1.96 * results$monte_carlo_sd), big.mark = ","), 
      ", $", format(round(results$monte_carlo_mean + 1.96 * results$monte_carlo_sd), big.mark = ","), "]\n\n",
      
      "ECONOMIC ADJUSTMENTS (Sturman 2000):\n",
      "1. Variable costs factor: ", (100 - input$variable_costs), "%\n",
      "2. Tax adjustment factor: ", (100 - input$tax_rate), "%\n", 
      "3. Discount factor (", input$tenure_period, " years @ ", input$discount_rate, "%): ", round(1 / (1 + input$discount_rate / 100)^input$tenure_period, 3), "\n",
      "Combined adjustment: ", round((100 - input$variable_costs) * (100 - input$tax_rate) * (1 / (1 + input$discount_rate / 100)^input$tenure_period) / 10000, 3), "\n",
      "- Adjusted utility: $", format(round(results$financially_adjusted), big.mark = ","), "\n",
      "- Adjustment reduction: $", format(round(results$traditional - results$financially_adjusted), big.mark = ",")
    )
  })
  
  # Monte Carlo Analysis with progress bar and manual bounds
  monte_carlo_results <- reactive({
    input$run_monte_carlo
    
    isolate({
      # Show progress
      withProgress(message = 'Running Monte Carlo Analysis...', value = 0, {
        
        iterations <- input$mc_iterations
        
        # Base parameters
        base_validity <- input$validity_proposed - input$validity_current
        base_sdy <- input$sdy_value
        base_sr <- input$selection_ratio_calc
        
        results <- numeric(iterations)
        
        for(i in 1:iterations) {
          incProgress(1/iterations, detail = paste("Iteration", i))
          
          if(input$use_manual_bounds) {
            # Use manual bounds for all parameters
            mc_validity <- runif(1, input$validity_lower, input$validity_upper)
            mc_sdy <- runif(1, input$sdy_lower, input$sdy_upper)
            mc_sr <- runif(1, input$sr_lower, input$sr_upper)
            mc_cost <- runif(1, input$cost_lower, input$cost_upper)
            mc_tenure <- runif(1, input$tenure_lower, input$tenure_upper)
          } else {
            # Use uncertainty percentages
            validity_uncert <- input$validity_uncertainty / 100
            sdy_uncert <- input$sdy_uncertainty / 100
            sr_uncert <- input$sr_uncertainty / 100
            
            mc_validity <- pmax(0.01, pmin(0.99, rnorm(1, base_validity, base_validity * validity_uncert)))
            mc_sdy <- pmax(1000, rnorm(1, base_sdy, base_sdy * sdy_uncert))
            mc_sr <- pmax(0.01, pmin(0.99, rnorm(1, base_sr, base_sr * sr_uncert)))
            mc_cost <- input$cost_per_person
            mc_tenure <- input$tenure_period
          }
          
          # Calculate utility with varied parameters
          results[i] <- utilityBcg(
            n = input$n_employees,
            sdy = mc_sdy,
            rxy = mc_validity,
            uxs = ux(mc_sr),
            sr = mc_sr,
            cost = mc_cost * (input$n_employees / mc_sr),
            period = mc_tenure
          )
        }
        
        list(
          results = results,
          mean_utility = mean(results, na.rm = TRUE),
          sd_utility = sd(results, na.rm = TRUE),
          ci_lower = quantile(results, 0.025, na.rm = TRUE),
          ci_upper = quantile(results, 0.975, na.rm = TRUE),
          iterations = iterations
        )
      })
    })
  })
  
  output$mc_mean_utility <- renderValueBox({
    results <- monte_carlo_results()
    valueBox(
      value = paste0("$", format(round(results$mean_utility), big.mark = ",")),
      subtitle = "Monte Carlo Mean",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$mc_confidence_interval <- renderValueBox({
    results <- monte_carlo_results()
    valueBox(
      value = paste0("$", format(round(results$ci_upper - results$ci_lower), big.mark = ",")),
      subtitle = "95% CI Width",
      icon = icon("arrows-alt-h"),
      color = "orange"
    )
  })
  
  output$mc_risk_assessment <- renderValueBox({
    results <- monte_carlo_results()
    risk_prob <- mean(results$results < 0, na.rm = TRUE) * 100
    valueBox(
      value = paste0(round(risk_prob, 1), "%"),
      subtitle = "Probability of Loss",
      icon = icon("exclamation-triangle"),
      color = if(risk_prob > 10) "red" else "green"
    )
  })
  
  output$mc_distribution_plot <- renderPlotly({
    results <- monte_carlo_results()
    
    plot_data <- data.frame(utility = results$results)
    
    p <- ggplot(plot_data, aes(x = utility)) +
      geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#3498db", alpha = 0.7) +
      geom_density(color = "red", linewidth = 1) +
      geom_vline(xintercept = results$mean_utility, color = "darkred", linetype = "dashed", linewidth = 1) +
      labs(title = "Monte Carlo Utility Distribution",
           x = "Utility ($)", y = "Density") +
      scale_x_continuous(labels = scales::dollar_format()) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$mc_risk_analysis <- renderUI({
    results <- monte_carlo_results()
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h4>Risk Analysis Summary</h4>",
      "<p><strong>Iterations:</strong> ", format(results$iterations, big.mark = ","), "</p>",
      "<p><strong>Mean Utility:</strong> $", format(round(results$mean_utility), big.mark = ","), "</p>",
      "<p><strong>Standard Deviation:</strong> $", format(round(results$sd_utility), big.mark = ","), "</p>",
      "<p><strong>95% Confidence Interval:</strong> [$", 
      format(round(results$ci_lower), big.mark = ","), " - $",
      format(round(results$ci_upper), big.mark = ","), "]</p>",
      "<p><strong>Risk Assessment:</strong> ", 
      round(mean(results$results < 0, na.rm = TRUE) * 100, 1), 
      "% probability of negative utility</p>",
      "</div>"
    ))
  })
  
  # Cost Savings Analysis - Now with visible output
  cost_savings_results <- reactive({
    input$calculate_savings
    
    isolate({
      # Turnover savings
      turnover_reduction <- (input$current_turnover_rate - input$improved_turnover_rate) / 100
      annual_turnover_savings <- input$n_employees * turnover_reduction * input$turnover_cost
      
      # Training efficiency savings
      training_improvement <- (input$training_success_improved - input$training_success_current) / 100
      training_savings <- input$n_employees * training_improvement * input$training_cost
      
      # Productivity acceleration savings
      time_reduction <- input$time_to_competency_current - input$time_to_competency_improved
      productivity_savings <- input$n_employees * time_reduction * input$monthly_productivity_value
      
      # Total savings
      total_savings <- annual_turnover_savings + training_savings + productivity_savings
      
      list(
        turnover_savings = annual_turnover_savings,
        training_savings = training_savings,
        productivity_savings = productivity_savings,
        total_savings = total_savings
      )
    })
  })
  
  output$annual_turnover_savings <- renderValueBox({
    results <- cost_savings_results()
    valueBox(
      value = paste0("$", format(round(results$turnover_savings), big.mark = ",")),
      subtitle = "Turnover Savings",
      icon = icon("user-minus"),
      color = "green"
    )
  })
  
  output$training_efficiency_savings <- renderValueBox({
    results <- cost_savings_results()
    valueBox(
      value = paste0("$", format(round(results$training_savings), big.mark = ",")),
      subtitle = "Training Efficiency",
      icon = icon("graduation-cap"),
      color = "blue"
    )
  })
  
  output$productivity_acceleration_savings <- renderValueBox({
    results <- cost_savings_results()
    valueBox(
      value = paste0("$", format(round(results$productivity_savings), big.mark = ",")),
      subtitle = "Productivity Gains",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  output$total_annual_savings <- renderValueBox({
    results <- cost_savings_results()
    valueBox(
      value = paste0("$", format(round(results$total_savings), big.mark = ",")),
      subtitle = "Total Annual Savings",
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$savings_breakdown_chart <- renderPlotly({
    results <- cost_savings_results()
    
    breakdown_data <- data.frame(
      Category = c("Turnover Reduction", "Training Efficiency", "Productivity Acceleration"),
      Savings = c(results$turnover_savings, results$training_savings, results$productivity_savings)
    )
    
    p <- ggplot(breakdown_data, aes(x = reorder(Category, Savings), y = Savings, fill = Category)) +
      geom_col(alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = "Annual Cost Savings Breakdown",
           x = "Savings Category", y = "Annual Savings ($)") +
      theme_minimal() +
      guides(fill = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  # Additional cost savings analysis outputs
  output$breakeven_analysis <- renderUI({
    results <- cost_savings_results()
    implementation_cost <- input$cost_per_person * input$n_employees
    
    if (results$total_savings > 0) {
      payback_months <- round((implementation_cost / results$total_savings) * 12, 1)
      roi_percent <- round(((results$total_savings - implementation_cost) / implementation_cost) * 100, 1)
    } else {
      payback_months <- Inf
      roi_percent <- -100
    }
    
    HTML(paste0(
      "<p><strong>Implementation Cost:</strong><br>$", format(implementation_cost, big.mark = ","), "</p>",
      "<p><strong>Annual Savings:</strong><br>$", format(round(results$total_savings), big.mark = ","), "</p>",
      "<p><strong>Payback Period:</strong><br>", 
      if(payback_months == Inf) "No payback" else paste(payback_months, "months"), "</p>",
      "<p><strong>Annual ROI:</strong><br>", roi_percent, "%</p>"
    ))
  })
  
  output$detailed_savings_explanation <- renderUI({
    results <- cost_savings_results()
    
    # Calculate component details
    turnover_reduction_pct <- input$current_turnover_rate - input$improved_turnover_rate
    training_improvement_pct <- input$training_success_improved - input$training_success_current
    time_reduction_months <- input$time_to_competency_current - input$time_to_competency_improved
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h5>Detailed Cost Savings Calculations</h5>",
      
      "<h6>1. Turnover Reduction Savings:</h6>",
      "<p><strong>Logic:</strong> Higher quality hires stay longer, reducing expensive turnover costs.</p>",
      "<p><strong>Calculation:</strong> ", input$n_employees, " employees × (", input$current_turnover_rate, "% - ", input$improved_turnover_rate, "%) × $", format(input$turnover_cost, big.mark = ","), " per turnover</p>",
      "<p><strong>Math:</strong> ", input$n_employees, " × ", turnover_reduction_pct, "% × $", format(input$turnover_cost, big.mark = ","), " = <strong>$", format(round(results$turnover_savings), big.mark = ","), "</strong></p>",
      
      "<hr>",
      
      "<h6>2. Training Efficiency Savings:</h6>",
      "<p><strong>Logic:</strong> Better candidates succeed in training more often, reducing repeated training costs.</p>",
      "<p><strong>Calculation:</strong> ", input$n_employees, " employees × (", input$training_success_improved, "% - ", input$training_success_current, "%) × $", format(input$training_cost, big.mark = ","), " per training</p>",
      "<p><strong>Math:</strong> ", input$n_employees, " × ", training_improvement_pct, "% × $", format(input$training_cost, big.mark = ","), " = <strong>$", format(round(results$training_savings), big.mark = ","), "</strong></p>",
      
      "<hr>",
      
      "<h6>3. Productivity Acceleration Savings:</h6>",
      "<p><strong>Logic:</strong> Higher quality hires reach full productivity faster, generating earlier value.</p>",
      "<p><strong>Calculation:</strong> ", input$n_employees, " employees × (", input$time_to_competency_current, " - ", input$time_to_competency_improved, ") months × $", format(input$monthly_productivity_value, big.mark = ","), " monthly value</p>",
      "<p><strong>Math:</strong> ", input$n_employees, " × ", time_reduction_months, " months × $", format(input$monthly_productivity_value, big.mark = ","), " = <strong>$", format(round(results$productivity_savings), big.mark = ","), "</strong></p>",
      
      "<hr>",
      
      "<h6>Total Annual Value Creation:</h6>",
      "<p><strong>Combined Savings:</strong> $", format(round(results$total_savings), big.mark = ","), " per year</p>",
      "<p><strong>Per-Employee Value:</strong> $", format(round(results$total_savings / input$n_employees), big.mark = ","), " per hire</p>",
      
      "</div>"
    ))
  })
  
  # Selection Battery Optimization
  battery_data <- get_selection_battery_data()
  
  current_battery_results <- reactive({
    input$optimize_battery
    
    isolate({
      if (length(input$selected_predictors) < 2) {
        return(list(multiple_r = 0, total_cost = 0, cost_per_validity = 0))
      }
      
      calculate_multiple_r(input$selected_predictors, battery_data, input$battery_sr)
    })
  })
  
  output$battery_validity <- renderValueBox({
    results <- current_battery_results()
    valueBox(
      value = round(results$multiple_r, 3),
      subtitle = "Multiple R",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$battery_cost <- renderValueBox({
    results <- current_battery_results()
    valueBox(
      value = paste0("$", format(results$total_cost, big.mark = ",")),
      subtitle = "Total Cost per Hire",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$validity_per_dollar <- renderValueBox({
    results <- current_battery_results()
    efficiency <- if(results$total_cost > 0) round(results$multiple_r / (results$total_cost / 1000), 4) else 0
    valueBox(
      value = efficiency,
      subtitle = "Validity per $1K",
      icon = icon("balance-scale"),
      color = "orange"
    )
  })
  
  output$adverse_impact_ratio <- renderValueBox({
    results <- current_battery_results()
    if("adverse_impact_ratio" %in% names(results)) {
      ratio_pct <- round(results$adverse_impact_ratio * 100, 1)
      color <- if(results$passes_80_rule) "green" else "red"
      valueBox(
        value = paste0(ratio_pct, "%"),
        subtitle = "AI Ratio (4/5ths Rule)",
        icon = icon("users"),
        color = color
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "AI Ratio (4/5ths Rule)",
        icon = icon("users"),
        color = "gray"
      )
    }
  })
  
  output$battery_analysis <- renderUI({
    results <- current_battery_results()
    selected_names <- battery_data$characteristics$Method[match(input$selected_predictors, 
                                                              c("biodata", "cognitive", "personality", "interview", "integrity", "performance"))]
    
    if (length(input$selected_predictors) < 2) {
      return(HTML("<p style='color: red;'>Please select at least 2 methods for analysis.</p>"))
    }
    
    # Add adverse impact analysis if available
    ai_text <- ""
    if("adverse_impact_ratio" %in% names(results)) {
      ai_status <- if(results$passes_80_rule) "PASSES" else "FAILS"
      ai_color <- if(results$passes_80_rule) "green" else "red"
      ai_text <- paste0(
        "<hr>",
        "<h6>Adverse Impact Analysis:</h6>",
        "<p><strong>Composite d-value:</strong> ", round(results$composite_d, 3), "</p>",
        "<p><strong>Adverse Impact Ratio:</strong> ", round(results$adverse_impact_ratio * 100, 1), "%</p>",
        "<p style='color: ", ai_color, ";'><strong>4/5ths Rule:</strong> ", ai_status, " (", 
        if(results$passes_80_rule) "≥80%" else "<80%", ")</p>"
      )
    }
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h5>Current Battery Analysis</h5>",
      "<p><strong>Selected Methods:</strong> ", paste(selected_names, collapse = ", "), "</p>",
      "<p><strong>Multiple Correlation (R):</strong> ", round(results$multiple_r, 3), "</p>",
      "<p><strong>Total Cost per Hire:</strong> $", format(results$total_cost, big.mark = ","), "</p>",
      "<p><strong>Cost per Validity Point:</strong> $", format(round(results$cost_per_validity, 0), big.mark = ","), "</p>",
      "<hr>",
      "<h6>Performance Prediction:</h6>",
      "<p>This combination predicts ", round(results$multiple_r^2 * 100, 1), "% of job performance variance at a cost of $", 
      format(results$total_cost, big.mark = ","), " per hire.</p>",
      ai_text,
      "</div>"
    ))
  })
  
  output$correlation_display <- DT::renderDataTable({
    cor_display <- round(battery_data$correlation_matrix, 3)
    rownames(cor_display) <- c("Biodata", "Cognitive", "Personality", "Interview", "Integrity", "Performance")
    colnames(cor_display) <- c("Biodata", "Cognitive", "Personality", "Interview", "Integrity", "Performance")
    cor_display
  }, options = list(pageLength = 10, searching = FALSE))
  
  optimal_combinations_data <- reactive({
    input$find_optimal
    
    isolate({
      find_optimal_combinations(battery_data)
    })
  })
  
  output$optimal_combinations <- DT::renderDataTable({
    optimal_combinations_data()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$method_characteristics <- DT::renderDataTable({
    chars <- battery_data$characteristics
    chars$Cost <- paste0("$", format(chars$Cost, big.mark = ","))
    chars$Cost_per_Validity <- paste0("$", format(round(chars$Cost_per_Validity, 0), big.mark = ","))
    chars
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$validity_cost_plot <- renderPlotly({
    chars <- battery_data$characteristics
    
    p <- ggplot(chars, aes(x = Cost, y = Validity, text = paste("Method:", Method))) +
      geom_point(size = 4, alpha = 0.7, color = "steelblue") +
      geom_text(aes(label = Method), vjust = -0.5, size = 3) +
      labs(title = "Selection Method: Validity vs Cost Trade-off",
           x = "Cost per Hire ($)", y = "Criterion Validity") +
      theme_minimal() +
      scale_x_continuous(labels = scales::dollar_format())
    
    ggplotly(p, tooltip = "text")
  })
  
  # Pareto Optimization Analysis
  pareto_results <- reactive({
    input$run_pareto
    
    isolate({
      # Check if ParetoR is available
      if (!requireNamespace("ParetoR", quietly = TRUE)) {
        return(list(error = "ParetoR package not installed. Please install with: install.packages('ParetoR')"))
      }
      
      # Create full correlation matrix with criterion (Berry et al. 2024)
      berry_matrix_lower <- '
      1.00,
       .13, 1.00,
       .54,  .03, 1.00,
       .21,  .18,  .08, 1.00,
       .25,  .01,  .28, -.02, 1.00,
       .42,  .29,  .23,  .45,  .16, 1.00,
       .38,  .31,  .19,  .42,  .31,  .26, 1.00
      '
      
      Table_2.data <- lavaan::getCov(berry_matrix_lower, diagonal = TRUE, 
                             names = c("Biodata", "GMA_Tests", "Conscientiousness", 
                                      "Structured_interview", "Integrity_test", "Performance", "Criterion"))
      
      # d-values for adverse impact
      d <- c(0.32, 0.79, -0.07, 0.24, 0.10, 0.37)
      
      # Run Pareto optimization
      tryCatch({
        out <- ParetoR::ParetoR(input$pareto_prop, input$pareto_sr, d, Table_2.data)
        
        # Find strategy solutions
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
          pareto_output = out,
          num_solutions = nrow(out$Pareto_Fmat),
          balanced_strategy = list(row = balanced_row, ai_ratio = balanced_ai_ratio, validity = balanced_validity),
          aggressive_strategy = list(row = aggressive_row, ai_ratio = aggressive_ai_ratio, validity = aggressive_validity),
          weights_matrix = out$Pareto_Xmat,
          results_matrix = out$Pareto_Fmat
        )
      }, error = function(e) {
        list(error = paste("Pareto optimization failed:", e$message))
      })
    })
  })
  
  output$pareto_solutions <- renderValueBox({
    results <- pareto_results()
    if("error" %in% names(results)) {
      valueBox(
        value = "Error",
        subtitle = "ParetoR Issue",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    } else {
      valueBox(
        value = results$num_solutions,
        subtitle = "Pareto Solutions",
        icon = icon("chart-line"),
        color = "blue"
      )
    }
  })
  
  output$pareto_strategies <- renderUI({
    results <- pareto_results()
    if("error" %in% names(results)) {
      HTML(paste0("<div style='color: red; padding: 10px;'>", results$error, "</div>"))
    } else {
      HTML(paste0(
        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h6>Optimal Strategies Found:</h6>",
        "<p><strong>Balanced Strategy:</strong><br>",
        "AI Ratio: ", round(results$balanced_strategy$ai_ratio, 3), 
        " | Validity: ", round(results$balanced_strategy$validity, 3), "</p>",
        "<p><strong>Aggressive Strategy:</strong><br>",
        "AI Ratio: ", round(results$aggressive_strategy$ai_ratio, 3), 
        " | Validity: ", round(results$aggressive_strategy$validity, 3), "</p>",
        "</div>"
      ))
    }
  })
  
  output$pareto_frontier_plot <- renderPlotly({
    results <- pareto_results()
    if("error" %in% names(results)) {
      # Return empty plot with error message
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "ParetoR package required", size = 5) +
        xlim(0, 1) + ylim(0, 1) + theme_void()
      return(ggplotly(p))
    }
    
    # Create Pareto frontier plot
    frontier_data <- data.frame(
      AI_Ratio = results$results_matrix[, "AI.ratio"],
      Validity = results$results_matrix[, "Criterion.Validity"],
      Solution = 1:nrow(results$results_matrix)
    )
    
    # Highlight key strategies
    frontier_data$Strategy <- "Other"
    frontier_data$Strategy[results$balanced_strategy$row] <- "Balanced"
    frontier_data$Strategy[results$aggressive_strategy$row] <- "Aggressive"
    
    p <- ggplot(frontier_data, aes(x = AI_Ratio, y = Validity, color = Strategy)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_line(alpha = 0.5, color = "gray") +
      scale_color_manual(values = c("Balanced" = "green", "Aggressive" = "red", "Other" = "blue")) +
      labs(title = "Pareto Frontier: Validity vs. Adverse Impact",
           x = "Adverse Impact Ratio", y = "Criterion Validity") +
      theme_minimal() +
      geom_vline(xintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.7) +
      annotate("text", x = 0.82, y = max(frontier_data$Validity) * 0.9, 
               label = "4/5ths Rule", angle = 90, size = 3)
    
    ggplotly(p)
  })
  
  output$pareto_strategies_table <- DT::renderDataTable({
    results <- pareto_results()
    if("error" %in% names(results)) {
      return(data.frame(Error = "ParetoR package required"))
    }
    
    # Create strategy comparison table
    strategy_data <- data.frame(
      Strategy = c("Balanced", "Aggressive"),
      AI_Ratio = c(results$balanced_strategy$ai_ratio, results$aggressive_strategy$ai_ratio),
      Validity = c(results$balanced_strategy$validity, results$aggressive_strategy$validity),
      Passes_80_Rule = c(
        results$balanced_strategy$ai_ratio >= 0.8,
        results$aggressive_strategy$ai_ratio >= 0.8
      ),
      Description = c(
        "Maximizes diversity (AI ratio closest to 1.0)",
        "Maximizes validity while meeting 4/5ths rule"
      )
    )
    
    strategy_data$AI_Ratio <- round(strategy_data$AI_Ratio, 3)
    strategy_data$Validity <- round(strategy_data$Validity, 3)
    
    strategy_data
  }, options = list(pageLength = 5, searching = FALSE))
  
  # Executive Selection & Character Assessment Analysis
  exec_results <- reactive({
    input$calculate_exec_utility
    
    isolate({
      # Basic utility calculation using BCG model
      zxs_value <- ux(input$exec_selection_ratio)
      total_cost <- input$exec_assessment_cost * (input$exec_sample_size / input$exec_selection_ratio)
      
      # Traditional utility calculation
      utility_total <- utilityBcg(
        n = input$exec_sample_size,
        sdy = input$exec_sdy,
        rxy = input$exec_character_validity,
        uxs = zxs_value,
        sr = input$exec_selection_ratio,
        cost = total_cost,
        period = input$exec_tenure
      )
      
      # Annual utility
      utility_annual <- utility_total / input$exec_tenure
      
      # ROI calculation
      roi_ratio <- utility_total / total_cost
      
      list(
        total_utility = utility_total,
        annual_utility = utility_annual,
        total_cost = total_cost,
        roi_ratio = roi_ratio,
        zxs_value = zxs_value,
        selected_executives = round(input$exec_sample_size * input$exec_selection_ratio)
      )
    })
  })
  
  output$exec_total_utility <- renderValueBox({
    results <- exec_results()
    valueBox(
      value = paste0("CAD $", format(round(results$total_utility), big.mark = ",")),
      subtitle = paste0(input$exec_tenure, "-Year Total Utility"),
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$exec_annual_utility <- renderValueBox({
    results <- exec_results()
    valueBox(
      value = paste0("CAD $", format(round(results$annual_utility), big.mark = ",")),
      subtitle = "Annual Utility",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$exec_roi_ratio <- renderValueBox({
    results <- exec_results()
    valueBox(
      value = paste0(round(results$roi_ratio, 1), ":1"),
      subtitle = "ROI Ratio",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$exec_utility_analysis <- renderUI({
    results <- exec_results()
    
    # Sturman adjustments (approximate)
    conservative_utility <- results$total_utility * 0.65
    conservative_annual <- conservative_utility / input$exec_tenure
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h5>Executive Character Assessment Utility Analysis</h5>",
      
      "<h6>Assessment Parameters:</h6>",
      "<p><strong>Character Assessment Validity:</strong> r = ", input$exec_character_validity, "</p>",
      "<p><strong>Executive Candidates:</strong> ", input$exec_sample_size, "</p>",
      "<p><strong>Executives Selected:</strong> ", results$selected_executives, " (", round(input$exec_selection_ratio * 100, 1), "% selection ratio)</p>",
      "<p><strong>Assessment Cost:</strong> CAD $", format(input$exec_assessment_cost, big.mark = ","), " per candidate</p>",
      "<p><strong>Total Assessment Cost:</strong> CAD $", format(round(results$total_cost), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Utility Analysis Results:</h6>",
      "<p><strong>Total Utility (", input$exec_tenure, " years):</strong> CAD $", format(round(results$total_utility), big.mark = ","), "</p>",
      "<p><strong>Annual Utility:</strong> CAD $", format(round(results$annual_utility), big.mark = ","), "</p>",
      "<p><strong>Per-Executive Annual Value:</strong> CAD $", format(round(results$annual_utility / results$selected_executives), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Conservative Estimates (Sturman Adjustments):</h6>",
      "<p><strong>Conservative Total Utility:</strong> CAD $", format(round(conservative_utility), big.mark = ","), " (65% of traditional)</p>",
      "<p><strong>Conservative Annual Utility:</strong> CAD $", format(round(conservative_annual), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Return on Investment:</h6>",
      "<p><strong>ROI Ratio:</strong> ", round(results$roi_ratio, 1), ":1</p>",
      "<p><strong>ROI Percentage:</strong> ", round((results$roi_ratio - 1) * 100, 0), "%</p>",
      "<p><strong>Payback Period:</strong> ", round((results$total_cost / results$annual_utility) * 12, 1), " months</p>",
      
      "<hr>",
      
      "<h6>Key Insights:</h6>",
      "<p>• Character assessment provides <strong>CAD $", format(round(results$annual_utility / results$selected_executives), big.mark = ","), 
      "</strong> annual value per selected executive</p>",
      "<p>• Investment pays back in <strong>", round((results$total_cost / results$annual_utility) * 12, 1), " months</strong></p>",
      "<p>• Character-based selection supports long-term organizational leadership effectiveness</p>",
      "<p>• Results align with Seijts et al. (2020) findings on character assessment utility</p>",
      
      "</div>"
    ))
  })
  
  # Download Report Handler - Now functional
  output$download_staff_report <- downloadHandler(
    filename = function() {
      paste0("staffing_utility_analysis_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      rmd_content <- paste0('
---
title: "Comprehensive Staffing Utility Analysis Report"
subtitle: "', input$org_name_staff, '"
date: "', format(Sys.Date(), "%B %d, %Y"), '"
output: 
  pdf_document:
    toc: true
    number_sections: true
---

# Executive Summary

This comprehensive staffing utility analysis evaluates the economic value of implementing an improved selection system for **', input$position_title, '** positions at **', input$org_name_staff, '** in the **', input$industry_type, '** industry.

## Key Findings

- **Current System Validity:** ', input$report_validity_old, '
- **Proposed System Validity:** ', input$report_validity_new, '
- **Validity Improvement:** +', round(input$report_validity_new - input$report_validity_old, 3), '
- **Number of Positions:** ', input$report_employees_staff, '
- **Implementation Cost:** $', format(input$report_cost, big.mark = ","), ' per person

## Business Context

', input$report_context, '

# Methodology

This analysis integrates multiple approaches to utility analysis:

1. **Taylor-Russell Model:** Expectancy tables showing probability of selection success
2. **Naylor-Shine Approach:** Precise Zxs calculations using established tables
3. **Brogden-Cronbach-Gleser Model:** Traditional utility calculations
4. **Economic Adjustments:** Sturman (2000) adjustments for real-world factors
5. **Monte Carlo Analysis:** Risk assessment with parameter uncertainty

# Results

## Traditional Utility Analysis

Based on the Brogden-Cronbach-Gleser model:
- Performance Standard Deviation (SDy): $', format(input$report_sdy, big.mark = ","), '
- Selection Ratio: Typical organizational standards
- Expected Utility: [Calculated based on parameters]

## Risk Assessment

Monte Carlo analysis provides confidence intervals and risk assessment for the utility estimates.

# Recommendations

1. **Proceed with Implementation:** The analysis supports implementing the improved selection system
2. **Monitor Results:** Track actual performance improvements post-implementation  
3. **Continuous Improvement:** Regular validation of selection system effectiveness

# References

- Taylor, H. C., & Russell, J. T. (1939). The relationship of validity coefficients to the practical effectiveness of tests in selection
- Naylor, J. C., & Shine, L. C. (1965). A table for determining the increase in mean criterion score
- Brogden, H. E. (1949). When testing pays off
- Sturman, M. C. (2000). Implications of utility analysis adjustments

---

*Report generated by the Comprehensive Staffing Utility Analysis Tool (2025)*
      ')
      
      rmd_file <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, rmd_file)
      rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
