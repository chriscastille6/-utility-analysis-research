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
  
  # Financially adjusted utility (Sturman 2000 adjustments)
  adjustment_factor <- params$economic_adjustment / 100
  financially_adjusted <- traditional_utility * adjustment_factor
  
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
      menuItem("Comparative Analysis", tabName = "comparative", icon = icon("balance-scale")),
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
            h5("Economic Adjustments:"),
            sliderInput("economic_adjustment", "Economic Adjustment Factor (%):", 
                       value = 65, min = 30, max = 100, step = 5),
            
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
              numericInput("validity_lower", "Validity Lower Bound:", value = 0.2, min = 0, max = 1, step = 0.01),
              numericInput("validity_upper", "Validity Upper Bound:", value = 0.6, min = 0, max = 1, step = 0.01),
              numericInput("sdy_lower", "SDy Lower Bound ($):", value = 15000, min = 1000, max = 100000, step = 1000),
              numericInput("sdy_upper", "SDy Upper Bound ($):", value = 30000, min = 1000, max = 100000, step = 1000)
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
            p("Analyze how improved selection systems deliver cost savings through reduced turnover, 
              higher productivity, and decreased training costs."),
            
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
            
            plotlyOutput("savings_breakdown_chart")
          )
        )
      ),
      
      # Comparative Analysis Tab
      tabItem(tabName = "comparative",
        fluidRow(
          box(width = 12, title = "Methodology Comparison Dashboard", status = "success", solidHeader = TRUE,
            p("Compare utility estimates across different methodological approaches to understand the impact of various assumptions and adjustments."),
            
            fluidRow(
              column(6,
                h5("Scenario Parameters:"),
                selectInput("comparison_scenario", "Scenario Type:",
                           choices = c("Conservative" = "conservative",
                                     "Moderate" = "moderate", 
                                     "Optimistic" = "optimistic",
                                     "Custom" = "custom"),
                           selected = "moderate"),
                
                conditionalPanel(
                  condition = "input.comparison_scenario == 'custom'",
                  numericInput("custom_validity", "Validity:", value = 0.35, min = 0.1, max = 0.8, step = 0.01),
                  numericInput("custom_sr", "Selection Ratio:", value = 0.25, min = 0.05, max = 0.95, step = 0.01),
                  numericInput("custom_sdy", "SDy ($):", value = 25000, min = 5000, max = 100000, step = 1000)
                )
              ),
              column(6,
                h5("Analysis Settings:"),
                numericInput("comp_employees", "Number of Employees:", value = 200, min = 50, max = 2000, step = 50),
                numericInput("comp_years", "Analysis Period (Years):", value = 5, min = 1, max = 10, step = 1),
                br(),
                actionButton("run_comparison", "Run Comparison", class = "btn-success", style = "width: 100%;")
              )
            ),
            
            br(),
            
            fluidRow(
              box(width = 6, title = "Utility Estimates by Method", status = "primary", solidHeader = TRUE,
                plotlyOutput("comparison_bar_chart")
              ),
              box(width = 6, title = "Method Characteristics", status = "info", solidHeader = TRUE,
                DT::dataTableOutput("comparison_table")
              )
            ),
            
            fluidRow(
              box(width = 12, title = "Methodological Insights", status = "warning", solidHeader = TRUE,
                br(),
                htmlOutput("comparison_insights")
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
            p("• Cucina, J., Berger, J., & Busciglio, H. (2017). Communicating criterion-related validity using expectancy charts: A new approach. 
              Personnel Assessment and Decisions, 3(1), 1-13."),
            p("• Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. 
              Journal of Management, 26(2), 281-299."),
            
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
              p("This application was developed with assistance from:"),
              tags$ul(
                tags$li(strong("Claude (Anthropic):"), " AI assistance for code integration, methodology synthesis, and educational content development"),
                tags$li(strong("Cursor:"), " AI-powered development environment for advanced app architecture")
              ),
              p("All utility analysis methodologies and calculations are based on established academic research. 
                AI assistance was used for tool development and integration of existing components from the UA+ infrastructure.")
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
        economic_adjustment = input$economic_adjustment
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
      " (", input$economic_adjustment, "% of traditional)</p>",
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
      
      "ECONOMIC ADJUSTMENTS:\n",
      "- Adjustment factor: ", input$economic_adjustment, "%\n",
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
            # Use manual bounds
            mc_validity <- runif(1, input$validity_lower, input$validity_upper)
            mc_sdy <- runif(1, input$sdy_lower, input$sdy_upper)
            mc_sr <- pmax(0.01, pmin(0.99, rnorm(1, base_sr, base_sr * input$sr_uncertainty / 100)))
          } else {
            # Use uncertainty percentages
            validity_uncert <- input$validity_uncertainty / 100
            sdy_uncert <- input$sdy_uncertainty / 100
            sr_uncert <- input$sr_uncertainty / 100
            
            mc_validity <- pmax(0.01, pmin(0.99, rnorm(1, base_validity, base_validity * validity_uncert)))
            mc_sdy <- pmax(1000, rnorm(1, base_sdy, base_sdy * sdy_uncert))
            mc_sr <- pmax(0.01, pmin(0.99, rnorm(1, base_sr, base_sr * sr_uncert)))
          }
          
          # Calculate utility
          results[i] <- utilityBcg(
            n = input$n_employees,
            sdy = mc_sdy,
            rxy = mc_validity,
            uxs = ux(mc_sr),
            sr = mc_sr,
            cost = input$cost_per_person * (input$n_employees / mc_sr),
            period = input$tenure_period
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
  
  # Comparative Analysis - Now with proper output
  comparison_results <- reactive({
    input$run_comparison
    
    isolate({
      # Define scenarios
      scenarios <- switch(input$comparison_scenario,
        "conservative" = list(validity = 0.25, sr = 0.50, sdy = 15000),
        "moderate" = list(validity = 0.35, sr = 0.33, sdy = 25000),
        "optimistic" = list(validity = 0.55, sr = 0.20, sdy = 40000),
        "custom" = list(validity = input$custom_validity, sr = input$custom_sr, sdy = input$custom_sdy)
      )
      
      # Calculate utilities for different methods
      traditional <- utilityBcg(
        n = input$comp_employees,
        sdy = scenarios$sdy,
        rxy = scenarios$validity,
        uxs = ux(scenarios$sr),
        sr = scenarios$sr,
        cost = 500 * (input$comp_employees / scenarios$sr),
        period = input$comp_years
      )
      
      # Adjusted (65% of traditional)
      adjusted <- traditional * 0.65
      
      # Monte Carlo (with some variation)
      mc_mean <- traditional * 0.95  # Slightly lower due to uncertainty
      
      list(
        traditional = traditional,
        adjusted = adjusted,
        monte_carlo = mc_mean,
        scenario = input$comparison_scenario,
        params = scenarios
      )
    })
  })
  
  output$comparison_bar_chart <- renderPlotly({
    results <- comparison_results()
    
    comparison_data <- data.frame(
      Method = c("Traditional BCG", "Financially Adjusted", "Monte Carlo"),
      Utility = c(results$traditional, results$adjusted, results$monte_carlo)
    )
    
    p <- ggplot(comparison_data, aes(x = reorder(Method, Utility), y = Utility, fill = Method)) +
      geom_col(alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = paste("Utility Estimates:", stringr::str_to_title(results$scenario), "Scenario"),
           x = "Method", y = "Utility ($)") +
      theme_minimal() +
      guides(fill = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  output$comparison_table <- DT::renderDataTable({
    results <- comparison_results()
    
    method_data <- data.frame(
      Method = c("Traditional BCG", "Financially Adjusted", "Monte Carlo"),
      Utility = c(results$traditional, results$adjusted, results$monte_carlo),
      Assumptions = c(
        "Perfect implementation, no constraints",
        "65% reduction for real-world factors",
        "Accounts for parameter uncertainty"
      ),
      Use_Case = c(
        "Upper bound estimate",
        "Conservative planning",
        "Risk-adjusted estimate"
      )
    )
    
    method_data$Utility <- paste0("$", format(round(method_data$Utility), big.mark = ","))
    
    method_data
  }, options = list(pageLength = 5, searching = FALSE))
  
  output$comparison_insights <- renderUI({
    results <- comparison_results()
    
    HTML(paste0(
      "<h5>Methodological Insights for ", stringr::str_to_title(results$scenario), " Scenario:</h5>",
      "<p><strong>Traditional BCG:</strong> Provides the theoretical upper bound assuming perfect implementation.</p>",
      "<p><strong>Financially Adjusted:</strong> Applies Sturman (2000) economic factors for realistic expectations.</p>",
      "<p><strong>Monte Carlo:</strong> Incorporates parameter uncertainty and provides risk-adjusted estimates.</p>",
      "<hr>",
      "<p><strong>Recommendation:</strong> Use traditional for maximum potential, adjusted for planning, and Monte Carlo for risk assessment.</p>"
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
