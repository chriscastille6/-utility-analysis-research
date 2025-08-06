# ============================================================================
# COMPLETE MODULAR UTILITY ANALYSIS APP
# ============================================================================
# This version includes Sturman (Comp & Ben) and Training modules
# Based on the original UA+ app with modular architecture

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(scales)
library(dplyr)
library(shinyjs)
library(grid)
library(gridExtra)
library(stringr)
library(tidyr)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# Custom ggplot theme for consistent styling
custom_theme <- function() {
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    legend.position = "bottom"
  )
}

# Dollar formatting function
dollar_format2 <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", round(x/1e6, 1), "M"), 
         ifelse(abs(x) >= 1e3, paste0("$", round(x/1e3, 1), "K"), 
                paste0("$", round(x, 1))))
}

# ============================================================================
# STURMAN MODULE (COMP & BEN) - COMPREHENSIVE VERSION
# ============================================================================

# Sturman UI Module
sturmanUI <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "Compensation & Benefits Utility"),
    dashboardSidebar(
      sidebarMenu(
        id = ns("sidebarComp"),
        menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
        menuItem("Step 1: Pay Strategies", tabName = "pay_strategy", icon = icon("chart-line")),
        menuItem("Step 2: Turnover Analysis", tabName = "turnover_analysis", icon = icon("users")),
        menuItem("Steps 3-4: Cost Analysis", tabName = "cost_analysis", icon = icon("dollar-sign")),
        menuItem("Steps 5-10: Service Value", tabName = "service_value", icon = icon("chart-bar")),
        menuItem("Step 11: Investment Value", tabName = "investment_analysis", icon = icon("trophy"))
      ),
      
      # Shared Analysis Parameters Box
      conditionalPanel(
        condition = paste0("input['", ns("sidebarComp"), "'] != 'overview'"),
        div(style = "margin-top: 20px; padding: 15px; border: 2px solid #3c8dbc; border-radius: 5px; background-color: #f4f4f4;",
            HTML('<h3 style="margin-top: 0; color: #3c8dbc;">Analysis Parameters</h3>'),
            
            # Base parameters
            numericInput(ns("paylevel_base"), "Base Pay Level ($):", value = 47983, min = 30000, max = 100000, step = 1000),
            
            # Strategy 1 parameters
            h5("Strategy 1: Across-the-Board"),
            numericInput(ns("strategy1_rate"), "Annual Increase Rate (%):", value = 4, min = 0, max = 10, step = 0.5),
            
            # Strategy 2 parameters  
            h5("Strategy 2: Merit-Based"),
            numericInput(ns("strategy2_base"), "Base Rate (%):", value = 4, min = 0, max = 10, step = 0.5),
            selectInput(ns("strategy2_merit_start"), "Merit Threshold Rating:", 
                        choices = list("Meets Expectations (3.0)" = 3.0, 
                                      "Exceeds Expectations (3.5)" = 3.5,
                                      "Far Exceeds Expectations (4.0)" = 4.0),
                        selected = 3.0),
            numericInput(ns("strategy2_merit_rate"), "Merit Increment per Rating Point (%):", value = 1, min = 0, max = 5, step = 0.5),
            
            # Strategy 3 parameters
            h5("Strategy 3: Performance-Based"),
            numericInput(ns("strategy3_min"), "Minimum Rate (%):", value = 0, min = 0, max = 5, step = 0.5),
            numericInput(ns("strategy3_max"), "Maximum Rate (%):", value = 8, min = 5, max = 15, step = 0.5),
            
            # Cost parameters
            h5("Cost Parameters"),
            numericInput(ns("move_cost_mult"), "Movement Cost Multiplier:", value = 2.0, min = 1.0, max = 5.0, step = 0.1),
            numericInput(ns("serv_cost_mult"), "Service Cost Multiplier:", value = 1.37, min = 1.0, max = 3.0, step = 0.01),
            numericInput(ns("serv_value_mult"), "Service Value Multiplier:", value = 1.754, min = 1.0, max = 3.0, step = 0.001),
            
            # SDy parameters
            h5("Performance Variability (SDy)"),
            numericInput(ns("sdy_low"), "Low SDy (% of salary):", value = 30, min = 10, max = 50, step = 5),
            numericInput(ns("sdy_medium"), "Medium SDy (% of salary):", value = 60, min = 40, max = 80, step = 5),
            numericInput(ns("sdy_high"), "High SDy (% of salary):", value = 90, min = 70, max = 120, step = 5),
            checkboxGroupInput(ns("sdy_levels"), "Select SDy Levels to Analyze:",
                              choices = list("Low" = "low", "Medium" = "medium", "High" = "high"),
                              selected = c("low", "medium", "high")),
            
            # Star Power option
            h5("Star Performer Analysis"),
            checkboxInput(ns("enable_star_power"), "Enable Star Power Analysis", value = FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("enable_star_power"), "'] == true"),
              numericInput(ns("star_sdy"), "Star SDy (% of salary):", value = 150, min = 100, max = 300, step = 10),
              numericInput(ns("star_percentage"), "% of Workforce that are Stars:", value = 5, min = 1, max = 20, step = 1)
            ),
            
            actionButton(ns("calculate"), "Run Analysis", class = "btn-primary", style = "width: 100%; margin-top: 20px;"),
            br(), br(),
            downloadButton(ns("download_complete_report"), "Download Complete Analysis", class = "btn-success", style = "width: 100%;")
        )
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
                fluidPage(
                  titlePanel("Understanding Pay Strategy Utility Analysis"),
                  
                  h4("Purpose"),
                  p("This analysis helps organizations evaluate the financial impact of different compensation strategies 
                    by comparing their total costs against the value they create over a ", tags$strong("4-year period"), ". The goal is to 
                    identify which pay strategy provides the highest return on investment."),
                  
                  h4("Research Foundation"),
                  p("This analysis is based on the seminal research by Sturman, Trevor, Boudreau, and Gerhart (2003), 
                    who developed a comprehensive utility analysis framework for evaluating performance-based pay systems 
                    using real organizational data from 5,143 employees."),
                  
                  tags$div(
                    style = "background-color: #e9ecef; padding: 10px; margin: 10px 0; border-left: 3px solid #6c757d; font-size: 12px;",
                    tags$strong("Citation: "), "Sturman, M. C., Trevor, C. O., Boudreau, J. W., & Gerhart, B. (2003). 
                    Is it worth it to win the talent war? Evaluating the utility of performance-based pay. 
                    Personnel Psychology, 56(4), 997–1035. https://doi.org/10.1111/j.1744-6570.2003.tb00248.x"
                  ),
                  
                  h4("Analysis Timeframe"),
                  p("All calculations in this analysis project outcomes over a ", tags$strong("4-year period"), 
                    ", allowing for the cumulative effects of different pay strategies to become apparent. 
                    This timeframe captures both immediate costs and longer-term value creation."),
                  
                  h4("The Three Pay Strategies"),
                  tags$div(
                    style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #007bff;",
                    tags$strong("Strategy 1: Across-the-Board (ATB)"), br(),
                    "Everyone receives the same percentage increase regardless of performance level. This is the most equitable 
                    approach but doesn't differentiate based on contribution."
                  ),
                  tags$div(
                    style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #28a745;",
                    tags$strong("Strategy 2: Merit-Based (Moderate Differentiation)"), br(),
                    "All employees receive a base increase, with additional merit increases for those meeting or exceeding 
                    performance thresholds. This balances equity with performance recognition."
                  ),
                  tags$div(
                    style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #dc3545;",
                    tags$strong("Strategy 3: Performance-Based (High Differentiation)"), br(),
                    "Pay increases are directly tied to performance ratings, with top performers receiving the highest 
                    increases and low performers receiving minimal or no increases."
                  ),
                  
                  h4("Key Analysis Steps"),
                  tags$ol(
                    tags$li(tags$strong("Pay Strategy Design:"), " Define how much each performance level receives under each strategy"),
                    tags$li(tags$strong("Turnover Analysis:"), " Estimate how many employees will leave under each strategy"),
                    tags$li(tags$strong("Cost Calculation:"), " Calculate movement costs (hiring/training) and service costs (compensation)"),
                    tags$li(tags$strong("Service Value Assessment:"), " Estimate the economic value employees create"),
                    tags$li(tags$strong("Investment Analysis:"), " Compare total value created minus total costs to find the best strategy")
                  ),
                  
                  h4("Key Parameters You Can Adjust"),
                  tags$ul(
                    tags$li(tags$strong("Base Pay Level:"), " Starting salary for analysis calculations"),
                    tags$li(tags$strong("Merit Threshold:"), " Performance level required to receive merit increases (Strategy 2)"),
                    tags$li(tags$strong("Cost Multipliers:"), " How expensive it is to replace employees and provide benefits"),
                    tags$li(tags$strong("SDy (Performance Variability):"), " How much economic value differs between high and low performers")
                  ),
                  
                  h4("Understanding SDy (Standard Deviation of Performance)"),
                  p("SDy represents how much more valuable high performers are compared to average performers, expressed as 
                    a percentage of salary. For example, if SDy = 60% and the average salary is $50,000, then a high performer 
                    (1 standard deviation above average) creates $30,000 more economic value per year than an average performer:"),
                  tags$ul(
                    tags$li(tags$strong("Low SDy (30% of salary):"), " Performance differences create modest economic impact"),
                    tags$li(tags$strong("Medium SDy (60% of salary):"), " Performance differences create moderate economic impact"), 
                    tags$li(tags$strong("High SDy (90% of salary):"), " Performance differences create substantial economic impact (star performers)")
                  ),
                  p("Higher SDy values make performance-based strategies more valuable because the economic benefit 
                    of retaining high performers and losing low performers becomes greater."),
                  
                  h4("Star Performer Analysis"),
                  p("Use the 'Star Power' option in the sidebar to model scenarios with exceptionally high-performing employees. 
                    Star performers can create economic value far exceeding normal distributions, making performance-based 
                    strategies even more valuable when such talent exists in your organization.")
                )
        ),
        
        tabItem(tabName = "pay_strategy",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Step 1: Pay Strategy Configuration and Analysis"),
                  
                  h4("How Pay Strategies Work"),
                  p("This step calculates the final pay levels after 4 years for each performance rating under each strategy. 
                    The analysis shows how much employees at different performance levels will earn after receiving annual increases 
                    according to each strategy's formula."),
                  
                  h4("Mathematical Example"),
                  div(style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #17a2b8;",
                      tags$strong("Example Calculation:"), br(),
                      "Starting salary: $47,983", br(),
                      "Strategy 1 (4% annual increase): Final pay = $47,983 × (1.04)⁴ = $56,126", br(),
                      "Strategy 2 (4% base + 1% merit for rating ≥3.0): For rating 4.0 = 4% + 1% = 5% annual", br(),
                      "Final pay = $47,983 × (1.05)⁴ = $58,345", br(),
                      "Strategy 3 (0% to 8% based on rating): For rating 4.0 = 5.33% annual", br(),
                      "Final pay = $47,983 × (1.0533)⁴ = $58,345"
                  ),
                  
                  h4("Pay Levels by Performance Rating After 4 Years"),
                  plotOutput(ns("payPlot"), height = "500px"),
                  br(),
                  
                  h4("Strategic Implications"),
                  p(tags$strong("Across-the-Board"), " maintains pay equity but provides no performance incentives. 
                    All employees receive identical treatment regardless of contribution."),
                  p(tags$strong("Merit-Based"), " balances equity with performance recognition by providing a base 
                    increase to everyone while rewarding higher performers with additional merit increases."),
                  p(tags$strong("Performance-Based"), " creates the strongest performance incentives but may raise 
                    equity concerns. The large pay differences can motivate high performance but may also increase 
                    turnover among lower performers."),
                  
                  h4("Strategy Comparison Tables"),
                  p("The three tables below show the specific annual pay increases and final pay levels (Year 4 salary) 
                    for each performance rating under each strategy."),
                  
                  fluidRow(
                    column(4, 
                           h5("Strategy 1: Across-the-Board", style = "color: #1f77b4; font-weight: bold;"),
                           DT::dataTableOutput(ns("payTable1"))),
                    column(4,
                           h5("Strategy 2: Merit-Based", style = "color: #ff7f0e; font-weight: bold;"),
                           DT::dataTableOutput(ns("payTable2"))),
                    column(4,
                           h5("Strategy 3: Performance-Based", style = "color: #2ca02c; font-weight: bold;"),
                           DT::dataTableOutput(ns("payTable3")))
                  )
                )
        ),
        
        tabItem(tabName = "turnover_analysis",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Step 2: Turnover Analysis"),
                  
                  h4("Understanding Turnover Patterns"),
                  p("Different pay strategies affect employee turnover differently. Performance-based strategies 
                    typically increase turnover among low performers (functional turnover) while reducing turnover 
                    among high performers. This step analyzes these differential effects."),
                  
                  h4("Mathematical Example"),
                  div(style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #17a2b8;",
                      tags$strong("Example Calculation:"), br(),
                      "For performance rating 2.0 with 1,171 employees:", br(),
                      "Strategy 1 turnover rate: 38% → Employees retained: 1,171 × (1 - 0.38) = 726", br(),
                      "Strategy 3 turnover rate: 60% → Employees retained: 1,171 × (1 - 0.60) = 468", br(),
                      "Performance-based strategy loses more low performers (functional turnover)"
                  ),
                  
                  h4("Performance Rating Distribution"),
                  plotOutput(ns("perfDistPlot"), height = "350px"),
                  br(),
                  
                  h4("Turnover Rates by Strategy and Performance Level"),
                  plotOutput(ns("turnoverPlot"), height = "450px"),
                  
                  div(style = "background-color: #fff3cd; padding: 10px; border: 1px solid #ffeaa7; border-radius: 5px; font-size: 14px; margin-top: 10px;",
                      tags$strong("Note about the Turnover Plot: "), "Strategy 1 (blue line) may be difficult to see 
                      for performance ratings 1-3 because it overlaps with Strategy 2 (orange line). Both strategies 
                      have identical turnover rates for lower performers, differing only for higher performers."
                  ),
                  br(),
                  
                  h4("What This Shows"),
                  p("The performance distribution shows the original employee data from Sturman (2003) - 5,143 employees. 
                    Most employees cluster around average performance (ratings 2.5-3.5)."),
                  
                  p("The turnover chart shows annual turnover rates for each performance level under each strategy. 
                    Notice how performance-based strategies increase turnover among low performers while 
                    reducing it among high performers."),
                  
                  h4("Employee Retention Summary"),
                  DT::dataTableOutput(ns("turnoverTable")),
                  br(),
                  
                  h4("Key Insight"),
                  p(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px;", 
                    "Performance-based strategies typically show higher overall turnover due to increased departures 
                    among low performers, but this can be beneficial if it represents 'functional turnover' of poor performers.")
                )
        ),
        
        tabItem(tabName = "cost_analysis",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Steps 3-4: Cost Analysis"),
                  
                  h4("Understanding the Costs of Pay Strategies"),
                  p("Every pay strategy involves two main types of costs: service costs (compensation and benefits) 
                    and movement costs (recruiting, hiring, and training replacements for departing employees). 
                    This analysis calculates the total 4-year costs for each strategy."),
                  
                  h4("Mathematical Example"),
                  div(style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #17a2b8;",
                      tags$strong("Example Calculation:"), br(),
                      "Movement Cost = Number of separations × Average salary × Movement cost multiplier", br(),
                      "If Strategy 1 has 1,500 separations, average final pay $56,126, multiplier 2.0:", br(),
                      "Movement Cost = 1,500 × $56,126 × 2.0 = $168.4M", br(), br(),
                      "Service Cost = Total employees × Average service cost × 4 years", br(),
                      "Service Cost = 5,143 × ($47,983 × 1.37) × 4 = $1.35B"
                  ),
                  
                  h4("Cost Components"),
                  tags$ul(
                    tags$li(tags$strong("Movement Costs:"), " Vary by strategy based on turnover rates - strategies with higher turnover 
                      have higher movement costs"),
                    tags$li(tags$strong("Service Costs:"), " Reflect the total compensation expense over 4 years")
                  ),
                  
                  h4("Movement and Service Costs Comparison"),
                  plotOutput(ns("costPlot"), height = "450px"),
                  br(),
                  
                  h4("Detailed Cost Breakdown"),
                  DT::dataTableOutput(ns("costTable")),
                  br(),
                  
                  h4("Key Insight"),
                  p(style = "background-color: #e7f3ff; padding: 10px; border-radius: 5px;", 
                    "Pay attention to how movement costs (replacement expenses) vary with turnover levels, while service costs reflect the total 
                    compensation investment. Higher turnover strategies have higher movement costs but may be justified if they retain high performers.")
                )
        ),
        
        tabItem(tabName = "service_value",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Steps 5-10: Service Value Analysis"),
                  
                  h4("Understanding Service Value"),
                  p("Service value represents the total economic contribution that employees make to the organization. 
                    It includes both the base value that all employees provide plus additional value based on their 
                    performance level."),
                  
                  h4("Mathematical Example"),
                  div(style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #17a2b8;",
                      tags$strong("Example Calculation (Medium SDy = 60%):"), br(),
                      "Base service value per employee = $47,983 × 1.754 = $84,176", br(),
                      "For a high performer (rating 4.5, z-score ≈ 1.2):", br(),
                      "Additional value = 60% × 1.2 × $47,983 = $34,628", br(),
                      "Total service value = $84,176 + $34,628 = $118,804 per year", br(),
                      "For average performer: $84,176 per year", br(),
                      "High performer creates $34,628 more value annually"
                  ),
                  
                  h4("How Service Value Works"),
                  p("Service value combines two components:"),
                  tags$ol(
                    tags$li("Base value that all employees provide through their work"),
                    tags$li("Additional value that varies by performance level")
                  ),
                  p("Higher-performing employees create more service value, and this difference 
                    becomes more pronounced as SDy (performance variability) increases."),
                  
                  h4("Service Value by Performance Variability Level"),
                  plotOutput(ns("serviceValuePlot"), height = "450px"),
                  br(),
                  
                  h4("Service Value Analysis"),
                  DT::dataTableOutput(ns("serviceValueTable")),
                  br(),
                  
                  div(id = "serviceValueHighlight", 
                      style = "background-color: #d4edda; padding: 15px; border: 1px solid #c3e6cb; border-radius: 5px;",
                      h5("Key Insight:", style = "color: #155724; margin-top: 0;"),
                      textOutput(ns("serviceValueInsight"))),
                  
                  h4("Strategic Insight"),
                  p("Performance-based strategies show greater value at higher SDy levels due to better retention 
                    of high performers and functional turnover of low performers.")
                )
        ),
        
        tabItem(tabName = "investment_analysis",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Step 11: Final Investment Analysis - Which Strategy Creates the Most Value?"),
                  
                  h4("Investment Value Calculation"),
                  p("Investment value is calculated as Service Value minus all Costs (service + movement costs). 
                    This represents the net economic benefit of each pay strategy over the 4-year period. 
                    The strategy with the highest investment value provides the best return on investment."),
                  
                  h4("Mathematical Example"),
                  div(style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #17a2b8;",
                      tags$strong("Example Calculation (Medium SDy):"), br(),
                      "Strategy 1:", br(),
                      "Service Value: $1,500M", br(),
                      "Service Cost: $1,350M", br(),
                      "Movement Cost: $168M", br(),
                      "Investment Value = $1,500M - $1,350M - $168M = -$18M", br(), br(),
                      "Strategy 3:", br(),
                      "Service Value: $1,520M (higher due to better performer retention)", br(),
                      "Service Cost: $1,360M", br(),
                      "Movement Cost: $200M (higher due to more turnover)", br(),
                      "Investment Value = $1,520M - $1,360M - $200M = -$40M", br(),
                      "Strategy 1 performs better in this example"
                  ),
                  
                  h4("4-Year Net Investment Value by Strategy"),
                  plotOutput(ns("investmentPlot"), height = "450px"),
                  br(),
                  
                  h4("Investment Analysis Summary"),
                  DT::dataTableOutput(ns("investmentTable")),
                  br(),
                  
                  div(id = "investmentHighlight", 
                      style = "background-color: #d1ecf1; padding: 15px; border: 1px solid #bee5eb; border-radius: 5px;",
                      h5("Strategic Recommendation:", style = "color: #0c5460; margin-top: 0;"),
                      textOutput(ns("investmentRecommendation"))),
                  
                  br(),
                  h4("Key Strategic Insights"),
                  div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                      textOutput(ns("strategicNarrative"))),
                  
                  h4("Strategic Questions to Consider"),
                  tags$ul(
                    tags$li("Which strategy provides the best ROI?"),
                    tags$li("How sensitive are results to SDy assumptions?"),
                    tags$li("What are the implementation risks?"),
                    tags$li("How does this align with organizational culture?")
                  ),
                  
                  h4("Key Strategic Principle"),
                  p(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;", 
                    "Performance-based strategies tend to be most valuable when performance variability (SDy) is high. 
                    Merit-based strategies often provide the best balance when performance differences are moderate. 
                    The key is matching your pay strategy to your organization's performance variability and strategic priorities.")
                )
        )
      )
    )
  )
}

# Sturman Server Module
sturmanServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Original performance rating data from Sturman 2003
    perf_rating <- seq(1, 5, by = 0.5)
    n_per_perf_rating <- c(60, 97, 1171, 1090, 1667, 672, 317, 46, 23)
    
    # Toggle states
    observe({
      toggleState(
        id = "calculate",
        condition = input$paylevel_base > 0
      )
    })
    
    # Get SDy values based on user input
    get_sdy_values <- reactive({
      sdy_map <- list(
        "low" = input$sdy_low / 100,
        "medium" = input$sdy_medium / 100,
        "high" = input$sdy_high / 100
      )
      
      selected_sdy <- input$sdy_levels
      sdy_values <- sdy_map[selected_sdy]
      names(sdy_values) <- paste0(names(sdy_values), " (", c(input$sdy_low, input$sdy_medium, input$sdy_high)[match(names(sdy_values), c("low", "medium", "high"))], "%)")
      
      # Add star power if enabled
      if(input$enable_star_power) {
        sdy_values[["star"]] <- input$star_sdy / 100
        names(sdy_values)[names(sdy_values) == "star"] <- paste0("star (", input$star_sdy, "%)")
      }
      
      return(sdy_values)
    })
    
    # Reactive calculations
    analysis_results <- reactive({
      if(input$calculate == 0) return(NULL)
      
      isolate({
        # Step 1: Define pay strategies
        paylevel_base <- input$paylevel_base
        
        # Strategy 1: Across-the-board
        strat1 <- rep(input$strategy1_rate / 100, length(perf_rating))
        
        # Strategy 2: Merit-based
        strat2 <- rep(input$strategy2_base / 100, length(perf_rating))
        merit_start <- as.numeric(input$strategy2_merit_start)
        strat2[perf_rating >= merit_start] <- (input$strategy2_base / 100) + 
          (input$strategy2_merit_rate / 100) * (perf_rating[perf_rating >= merit_start] - merit_start)
        
        # Strategy 3: Performance-based
        strat3 <- seq(input$strategy3_min / 100, input$strategy3_max / 100, length.out = length(perf_rating))
        
        # Calculate 4-year pay levels
        paylevel_final_s1 <- paylevel_base * (1 + strat1)^4
        paylevel_final_s2 <- paylevel_base * (1 + strat2)^4
        paylevel_final_s3 <- paylevel_base * (1 + strat3)^4
        
        # Step 2: Turnover probabilities (from original Sturman data)
        strat1_turnover <- c(0.96, 0.65, 0.38, 0.25, 0.21, 0.22, 0.27, 0.41, 0.66)
        strat2_turnover <- c(0.96, 0.65, 0.38, 0.25, 0.21, 0.14, 0.11, 0.11, 0.14)
        strat3_turnover <- c(0.99, 0.88, 0.6, 0.35, 0.21, 0.14, 0.11, 0.11, 0.14)
        
        # Calculate retained employees
        strat1_proj_rt <- round((1 - strat1_turnover) * n_per_perf_rating)
        strat2_proj_rt <- round((1 - strat2_turnover) * n_per_perf_rating)
        strat3_proj_rt <- round((1 - strat3_turnover) * n_per_perf_rating)
        
        # Calculate separations
        strat1_proj_rep <- round(strat1_turnover * n_per_perf_rating)
        strat2_proj_rep <- round(strat2_turnover * n_per_perf_rating)
        strat3_proj_rep <- round(strat3_turnover * n_per_perf_rating)
        
        n_sep <- c(sum(strat1_proj_rep), sum(strat2_proj_rep), sum(strat3_proj_rep))
        
        # Step 3: Movement costs
        avg_paylevel_final <- c(
          weighted.mean(paylevel_final_s1, strat1_proj_rt),
          weighted.mean(paylevel_final_s2, strat2_proj_rt),
          weighted.mean(paylevel_final_s3, strat3_proj_rt)
        )
        
        move_cost_mult <- input$move_cost_mult
        avg_move_cost_base <- paylevel_base * move_cost_mult
        avg_move_cost_final <- avg_paylevel_final * move_cost_mult
        avg_yearly_move_cost_increase <- (avg_move_cost_final - avg_move_cost_base) / 4
        avg_move_cost_year2 <- avg_move_cost_base + avg_yearly_move_cost_increase
        avg_move_cost_0407 <- (avg_move_cost_year2 + avg_move_cost_final) / 2
        total_move_cost <- avg_move_cost_0407 * n_sep
        
        # Step 4: Service costs
        avg_serv_cost_per_n_mult <- input$serv_cost_mult
        avg_serv_cost_base <- paylevel_base * avg_serv_cost_per_n_mult
        avg_serv_cost_final <- avg_paylevel_final * avg_serv_cost_per_n_mult
        avg_yearly_serv_cost_increase <- (avg_serv_cost_final - avg_serv_cost_base) / 4
        avg_serv_cost_year2 <- avg_serv_cost_base + avg_yearly_serv_cost_increase
        avg_serv_cost_0407 <- (avg_serv_cost_final + avg_serv_cost_year2) / 2
        total_serv_cost <- 4 * sum(n_per_perf_rating) * avg_serv_cost_0407
        
        # Step 5: Performance calculations
        avg_perf <- weighted.mean(perf_rating, n_per_perf_rating)
        n_times_delta_r_squared <- n_per_perf_rating * (perf_rating - avg_perf)^2
        std_dev_perf <- sqrt(sum(n_times_delta_r_squared) / sum(n_per_perf_rating))
        z_per_perf <- (perf_rating - avg_perf) / std_dev_perf
        
        # Service value calculations for each SDy level
        sdy_values <- get_sdy_values()
        service_value_results <- list()
        
        for(i in seq_along(sdy_values)) {
          sdy <- sdy_values[[i]]
          sdy_name <- names(sdy_values)[i]
          
          # Step 6-10: Service value calculations
          avg_serv_value_mult <- input$serv_value_mult
          avg_serv_value_per_perf_base <- avg_serv_value_mult * paylevel_base
          avg_serv_value_per_perf_final <- avg_serv_value_mult * avg_paylevel_final[1]  # Use strategy 1 as baseline
          
          # Calculate incremental service value
          if(grepl("star", sdy_name) && input$enable_star_power) {
            # Star power modeling: apply exceptional SDy to top performers only
            # Calculate cumulative employee percentages to find star threshold
            total_employees <- sum(n_per_perf_rating)
            star_count_target <- ceiling(total_employees * input$star_percentage / 100)
            
            # Find performance ratings that represent the top star_percentage of employees
            cumsum_employees <- cumsum(rev(n_per_perf_rating))  # Start from highest performers
            star_employee_indices <- which(cumsum_employees <= star_count_target)
            
            if(length(star_employee_indices) > 0) {
              # Identify which performance ratings are "stars"
              star_rating_indices <- length(perf_rating) - star_employee_indices + 1
              is_star <- seq_along(perf_rating) %in% star_rating_indices
            } else {
              # If star percentage is very small, just make the top rating group stars
              is_star <- perf_rating == max(perf_rating)
            }
            
            # Apply star SDy to identified stars, regular high SDy to others
            incr_serv_value_base <- ifelse(is_star, 
                                          sdy * z_per_perf * paylevel_base,
                                          (input$sdy_high/100) * z_per_perf * paylevel_base)
            incr_serv_value_final <- ifelse(is_star,
                                           sdy * z_per_perf * avg_paylevel_final[1],
                                           (input$sdy_high/100) * z_per_perf * avg_paylevel_final[1])
          } else {
            # Standard SDy modeling
            incr_serv_value_base <- sdy * z_per_perf * paylevel_base
            incr_serv_value_final <- sdy * z_per_perf * avg_paylevel_final[1]
          }
          
          # Total individual service value
          tot_serv_value_per_n_base <- avg_serv_value_per_perf_base + incr_serv_value_base
          tot_serv_value_per_n_final <- avg_serv_value_per_perf_final + incr_serv_value_final
          
          # Total service value for base year
          tot_serv_value_base <- sum(n_per_perf_rating * tot_serv_value_per_n_base)
          
          # Service value for retained employees (final year)
          tot_serv_value_final_s1 <- sum(strat1_proj_rt * tot_serv_value_per_n_final)
          tot_serv_value_final_s2 <- sum(strat2_proj_rt * tot_serv_value_per_n_final)
          tot_serv_value_final_s3 <- sum(strat3_proj_rt * tot_serv_value_per_n_final)
          
          sum_tot_serv_value_final <- c(tot_serv_value_final_s1, tot_serv_value_final_s2, tot_serv_value_final_s3)
          
          # Service value of replacement employees
          avg_serv_value <- tot_serv_value_final_s1 / sum(strat1_proj_rt)
          tot_serv_value_rep <- avg_serv_value * n_sep
          
          # Total service value of final year workforce
          tot_serv_value_final <- sum_tot_serv_value_final + tot_serv_value_rep
          
          # 4-year total service value
          avg_serv_value_inc <- (tot_serv_value_final - tot_serv_value_base) / 4
          serv_value_year2 <- tot_serv_value_base + avg_serv_value_inc
          avg_serv_value_0407 <- (serv_value_year2 + tot_serv_value_final) / 2
          tot_serv_value_0407 <- avg_serv_value_0407 * 4
          
          service_value_results[[sdy_name]] <- list(
            sdy = sdy,
            sdy_name = sdy_name,
            tot_serv_value_0407 = tot_serv_value_0407,
            tot_serv_value_final = tot_serv_value_final
          )
        }
        
        # Return comprehensive results
        list(
          # Pay data
          perf_rating = perf_rating,
          paylevel_final_s1 = paylevel_final_s1,
          paylevel_final_s2 = paylevel_final_s2,
          paylevel_final_s3 = paylevel_final_s3,
          strat1 = strat1 * 100,
          strat2 = strat2 * 100,
          strat3 = strat3 * 100,
          
          # Turnover data
          n_per_perf_rating = n_per_perf_rating,
          strat1_turnover = strat1_turnover,
          strat2_turnover = strat2_turnover,
          strat3_turnover = strat3_turnover,
          strat1_proj_rt = strat1_proj_rt,
          strat2_proj_rt = strat2_proj_rt,
          strat3_proj_rt = strat3_proj_rt,
          n_sep = n_sep,
          
          # Cost data
          total_move_cost = total_move_cost,
          total_serv_cost = total_serv_cost,
          avg_paylevel_final = avg_paylevel_final,
          
          # Service value data
          service_value_results = service_value_results,
          avg_perf = avg_perf,
          std_dev_perf = std_dev_perf
        )
      })
    })
    
    # Step 1: Pay Strategy Plot
    output$payPlot <- renderPlot({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_data <- data.frame(
        Rating = rep(results$perf_rating, 3),
        Strategy = rep(c("Strategy 1 (ATB)", "Strategy 2 (Merit)", "Strategy 3 (Performance)"), each = length(results$perf_rating)),
        Pay_Final = c(results$paylevel_final_s1, results$paylevel_final_s2, results$paylevel_final_s3),
        Increase = c(results$strat1, results$strat2, results$strat3)
      )
      
      ggplot(pay_data, aes(x = Rating, y = Pay_Final, color = Strategy, group = Strategy)) +
        geom_line(size = 1.5) +
        geom_point(size = 4) +
        scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
        scale_y_continuous(labels = dollar_format()) +
        labs(title = "Pay Levels After 4 Years by Performance Rating",
             x = "Performance Rating",
             y = "Final Pay Level",
             color = "Strategy") +
        custom_theme()
    })
    
    # Pay Tables - Strategy 1
    output$payTable1 <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_table <- data.frame(
        Rating = results$perf_rating,
        Increase = paste0(round(results$strat1, 1), "%"),
        Final_Pay = dollar(results$paylevel_final_s1)
      )
      
      DT::datatable(pay_table, options = list(pageLength = 10, dom = 't', scrollY = "300px"), rownames = FALSE)
    })
    
    # Pay Tables - Strategy 2
    output$payTable2 <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_table <- data.frame(
        Rating = results$perf_rating,
        Increase = paste0(round(results$strat2, 1), "%"),
        Final_Pay = dollar(results$paylevel_final_s2)
      )
      
      DT::datatable(pay_table, options = list(pageLength = 10, dom = 't', scrollY = "300px"), rownames = FALSE)
    })
    
    # Pay Tables - Strategy 3
    output$payTable3 <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_table <- data.frame(
        Rating = results$perf_rating,
        Increase = paste0(round(results$strat3, 1), "%"),
        Final_Pay = dollar(results$paylevel_final_s3)
      )
      
      DT::datatable(pay_table, options = list(pageLength = 10, dom = 't', scrollY = "300px"), rownames = FALSE)
    })
    
    # Performance Distribution Plot
    output$perfDistPlot <- renderPlot({
      perf_dist_data <- data.frame(
        Rating = perf_rating,
        Count = n_per_perf_rating
      )
      
      ggplot(perf_dist_data, aes(x = Rating, y = Count)) +
        geom_bar(stat = "identity", fill = "#1f77b4", alpha = 1.0) +
        labs(title = "Distribution of Performance Ratings (N = 5,143)",
             x = "Performance Rating",
             y = "Number of Employees") +
        custom_theme()
    })
    
    # Turnover Plot
    output$turnoverPlot <- renderPlot({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      turnover_data <- data.frame(
        Rating = rep(results$perf_rating, 3),
        Strategy = rep(c("Strategy 1", "Strategy 2", "Strategy 3"), each = length(results$perf_rating)),
        Turnover_Rate = c(results$strat1_turnover, results$strat2_turnover, results$strat3_turnover) * 100
      )
      
      ggplot(turnover_data, aes(x = Rating, y = Turnover_Rate, color = Strategy, group = Strategy)) +
        geom_line(size = 1.5) +
        geom_point(size = 4) +
        scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
        labs(title = "Annual Turnover Rates by Performance Rating",
             x = "Performance Rating",
             y = "Turnover Rate (%)",
             color = "Strategy") +
        custom_theme()
    })
    
    # Turnover Table
    output$turnoverTable <- renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      turnover_summary <- data.frame(
        Strategy = c("Strategy 1 (ATB)", "Strategy 2 (Merit)", "Strategy 3 (Performance)"),
        Total_Employees = rep(sum(results$n_per_perf_rating), 3),
        Employees_Retained = c(sum(results$strat1_proj_rt), sum(results$strat2_proj_rt), sum(results$strat3_proj_rt)),
        Employees_Lost = results$n_sep,
        Four_Year_Turnover_Rate = paste0(round((results$n_sep / sum(results$n_per_perf_rating)) * 100, 1), "%"),
        Annual_Turnover_Rate = paste0(round((results$n_sep / sum(results$n_per_perf_rating)) / 4 * 100, 1), "%"),
        Quarterly_Turnover_Rate = paste0(round((results$n_sep / sum(results$n_per_perf_rating)) / 16 * 100, 1), "%")
      )
      
      colnames(turnover_summary) <- c("Strategy", "Total Employees", "Employees Retained", "Employees Lost", 
                                     "4-Year Turnover Rate", "Annual Turnover Rate", "Quarterly Turnover Rate")
      
      DT::datatable(turnover_summary, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    # Cost Plot
    output$costPlot <- renderPlot({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      cost_data <- data.frame(
        Strategy = c("Strategy 1", "Strategy 2", "Strategy 3"),
        Movement_Cost = results$total_move_cost / 1e6,  # Convert to millions
        Service_Cost = results$total_serv_cost / 1e6    # Convert to millions
      )
      
      cost_data_long <- cost_data %>%
        pivot_longer(cols = c(Movement_Cost, Service_Cost), names_to = "Cost_Type", values_to = "Cost") %>%
        mutate(Cost_Type = ifelse(Cost_Type == "Movement_Cost", "Movement", "Service"))
      
      # Calculate total costs for annotations
      total_costs <- cost_data$Movement_Cost + cost_data$Service_Cost
      
      p <- ggplot(cost_data_long, aes(x = Strategy, y = Cost, fill = Cost_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("#d62728", "#ff7f0e"), name = "Cost Type") +
        scale_y_continuous(labels = function(x) paste0("$", round(x, 1), "M")) +
        labs(title = "4-Year Movement and Service Costs",
             x = "Strategy",
             y = "Cost (Millions)") +
        custom_theme()
      
      # Add total cost annotations on top of bars
      for(i in 1:3) {
        p <- p + annotate("text", x = i, y = total_costs[i] + max(total_costs) * 0.02, 
                         label = paste0("$", round(total_costs[i], 1), "M"), 
                         size = 4, fontface = "bold")
      }
      
      p
    })
    
    # Cost Table
    output$costTable <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      cost_table <- data.frame(
        Strategy = c("Strategy 1 (ATB)", "Strategy 2 (Merit)", "Strategy 3 (Performance)"),
        Separations = results$n_sep,
        Movement_Cost = dollar(results$total_move_cost),
        Service_Cost = dollar(results$total_serv_cost),
        Total_Cost = dollar(results$total_move_cost + results$total_serv_cost),
        Avg_Final_Pay = dollar(results$avg_paylevel_final)
      )
      
      DT::datatable(cost_table, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    # Service Value Plot
    output$serviceValuePlot <- renderPlot({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      if(length(results$service_value_results) == 0) return(NULL)
      
      service_value_data <- data.frame()
      for(sdy_name in names(results$service_value_results)) {
        sdy_result <- results$service_value_results[[sdy_name]]
        temp_data <- data.frame(
          SDy = sdy_name,
          Strategy = c("Strategy 1", "Strategy 2", "Strategy 3"),
          Service_Value = sdy_result$tot_serv_value_0407
        )
        service_value_data <- rbind(service_value_data, temp_data)
      }
      
      if(nrow(service_value_data) > 0) {
        ggplot(service_value_data, aes(x = Strategy, y = Service_Value, fill = SDy)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")[1:length(unique(service_value_data$SDy))]) +
          scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
          labs(title = "4-Year Total Service Value by SDy Level",
               x = "Strategy",
               y = "Service Value (Millions)",
               fill = "SDy Level") +
          custom_theme()
      }
    })
    
    # Service Value Table
    output$serviceValueTable <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      if(length(results$service_value_results) == 0) return(NULL)
      
      service_table_data <- data.frame()
      for(sdy_name in names(results$service_value_results)) {
        sdy_result <- results$service_value_results[[sdy_name]]
        temp_data <- data.frame(
          SDy_Level = sdy_name,
          Strategy_1_Value = dollar(sdy_result$tot_serv_value_0407[1]),
          Strategy_2_Value = dollar(sdy_result$tot_serv_value_0407[2]),
          Strategy_3_Value = dollar(sdy_result$tot_serv_value_0407[3])
        )
        service_table_data <- rbind(service_table_data, temp_data)
      }
      
      DT::datatable(service_table_data, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    # Service Value Insight
    output$serviceValueInsight <- renderText({
      results <- analysis_results()
      if(is.null(results) || length(results$service_value_results) == 0) return("")
      
      # Find the best strategy for each SDy level
      insights <- c()
      for(sdy_name in names(results$service_value_results)) {
        sdy_result <- results$service_value_results[[sdy_name]]
        best_strategy <- which.max(sdy_result$tot_serv_value_0407)
        strategy_names <- c("Across-the-Board", "Merit-Based", "Performance-Based")
        insights <- c(insights, paste0("At ", sdy_name, " SDy, ", strategy_names[best_strategy], " creates the highest service value."))
      }
      
      paste(insights, collapse = " ")
    })
    
    # Investment Value Plot
    output$investmentPlot <- renderPlot({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      if(length(results$service_value_results) == 0) return(NULL)
      
      investment_data <- data.frame()
      for(sdy_name in names(results$service_value_results)) {
        sdy_result <- results$service_value_results[[sdy_name]]
        investment_value <- sdy_result$tot_serv_value_0407 - results$total_serv_cost - results$total_move_cost
        temp_data <- data.frame(
          SDy = sdy_name,
          Strategy = c("Strategy 1", "Strategy 2", "Strategy 3"),
          Investment_Value = investment_value
        )
        investment_data <- rbind(investment_data, temp_data)
      }
      
      if(nrow(investment_data) > 0) {
        ggplot(investment_data, aes(x = Strategy, y = Investment_Value, fill = SDy)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")[1:length(unique(investment_data$SDy))]) +
          scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
          labs(title = "4-Year Net Investment Value by Strategy",
               x = "Strategy",
               y = "Investment Value (Millions)",
               fill = "SDy Level") +
          custom_theme()
      }
    })
    
    # Investment Table
    output$investmentTable <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      if(length(results$service_value_results) == 0) return(NULL)
      
      investment_table_data <- data.frame()
      for(sdy_name in names(results$service_value_results)) {
        sdy_result <- results$service_value_results[[sdy_name]]
        investment_value <- sdy_result$tot_serv_value_0407 - results$total_serv_cost - results$total_move_cost
        temp_data <- data.frame(
          SDy_Level = sdy_name,
          Strategy_1_Investment = dollar(investment_value[1]),
          Strategy_2_Investment = dollar(investment_value[2]),
          Strategy_3_Investment = dollar(investment_value[3])
        )
        investment_table_data <- rbind(investment_table_data, temp_data)
      }
      
      DT::datatable(investment_table_data, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    # Investment Recommendation
    output$investmentRecommendation <- renderText({
      results <- analysis_results()
      if(is.null(results) || length(results$service_value_results) == 0) return("")
      
      # Find the overall best strategy across all SDy levels
      all_values <- c()
      all_strategies <- c()
      all_sdy <- c()
      
      for(sdy_name in names(results$service_value_results)) {
        sdy_result <- results$service_value_results[[sdy_name]]
        investment_value <- sdy_result$tot_serv_value_0407 - results$total_serv_cost - results$total_move_cost
        all_values <- c(all_values, investment_value)
        all_strategies <- c(all_strategies, 1:3)
        all_sdy <- c(all_sdy, rep(sdy_name, 3))
      }
      
      best_idx <- which.max(all_values)
      best_strategy <- all_strategies[best_idx]
      best_sdy <- all_sdy[best_idx]
      best_value <- all_values[best_idx]
      
      strategy_names <- c("Across-the-Board", "Merit-Based", "Performance-Based")
      
      paste0("Based on your current parameters, ", strategy_names[best_strategy], 
             " provides the highest investment value of ", dollar(best_value), 
             " at ", best_sdy, " SDy level.")
    })
    
    # Strategic Narrative
    output$strategicNarrative <- renderText({
      results <- analysis_results()
      if(is.null(results) || length(results$service_value_results) == 0) return("")
      
      narrative <- "Performance-based pay strategies tend to be most valuable when performance variability (SDy) is high, meaning there are significant economic differences between high and low performers. In these situations, the ability to retain high performers and encourage functional turnover of low performers creates substantial value. However, when performance differences are smaller (low SDy), merit-based strategies often provide the best balance of performance incentives and cost control. Across-the-board strategies are rarely optimal from a pure financial perspective but may be chosen for equity or simplicity reasons. The key is matching your pay strategy to your organization's performance variability and strategic priorities."
      
      narrative
    })
    
    # Download handler for complete analysis report
    output$download_complete_report <- downloadHandler(
      filename = function() {
        paste0("sturman_analysis_complete_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # Get current analysis results and input parameters
        results <- analysis_results()
        if(is.null(results)) {
          showNotification("Please run the analysis first before downloading the report.", type = "error")
          return()
        }
        
        # Create comprehensive PDF report with all plots and analysis
        withProgress(message = 'Generating comprehensive PDF report...', value = 0, {
          incProgress(0.1, detail = "Preparing comprehensive analysis content...")
          
          # Create comprehensive analysis summary with all educational content
          analysis_overview <- paste0(
            "This report presents a comprehensive compensation strategy analysis using the methodology from Sturman, Trevor, Boudreau, and Gerhart (2003). ",
            "The analysis evaluates the financial impact of three distinct pay strategies over a 4-year period for ", sum(results$n_per_perf_rating), " employees. ",
            "This research-based approach provides organizations with quantitative insights to make informed decisions about performance-based pay systems. ",
            "The analysis incorporates real employee performance data, turnover patterns, and economic value calculations to determine which strategy provides the highest return on investment."
          )
          
          strategy_explanations <- paste0(
            "Three distinct compensation strategies are evaluated: (1) Across-the-Board strategy provides equal ", input$strategy1_rate, "% annual increases for all employees regardless of performance, ",
            "emphasizing equity and simplicity. (2) Merit-Based strategy combines a ", input$strategy2_base, "% base rate for all employees with additional merit increases starting at performance rating ", 
            input$strategy2_merit_start, ", balancing equity with performance recognition. (3) Performance-Based strategy directly links pay increases to performance ratings, ",
            "ranging from ", input$strategy3_min, "% to ", input$strategy3_max, "% based on individual performance levels, creating the strongest performance incentives."
          )
          
          economic_methodology <- paste0(
            "The economic analysis incorporates multiple cost factors and value calculations. Movement costs account for employee turnover using a multiplier of ", input$move_cost_mult, 
            " times average salary to represent recruiting, hiring, and training expenses. Service costs include total compensation and benefits over the 4-year period using a multiplier of ", 
            input$serv_cost_mult, ". Service value calculations employ performance variability (SDy) measures of ", input$sdy_low, "%, ", input$sdy_medium, "%, and ", input$sdy_high, 
            "% of salary to quantify the economic differences between high and low performers. A service value multiplier of ", input$serv_value_mult, 
            " captures the total economic contribution employees make to organizational performance beyond their direct compensation costs."
          )
          
          # Add star power explanation if enabled
          if(input$enable_star_power) {
            star_explanation <- paste0(
              " Additionally, this analysis includes Star Performer modeling with ", input$star_sdy, "% SDy for the top ", input$star_percentage, 
              "% of the workforce, representing exceptionally high-value employees who create economic impact far exceeding normal performance distributions."
            )
            economic_methodology <- paste0(economic_methodology, star_explanation)
          }
          
          # Mathematical examples
          math_examples <- paste0(
            "Mathematical Example - Pay Strategy Calculation: Starting salary $", format(input$paylevel_base, big.mark = ","), 
            ". Strategy 1 (", input$strategy1_rate, "% annual): Final pay = $", format(input$paylevel_base, big.mark = ","), " × (1.", sprintf("%02d", input$strategy1_rate), ")⁴ = $", 
            format(round(input$paylevel_base * (1 + input$strategy1_rate/100)^4), big.mark = ","), ". ",
            "Turnover Example: For rating 2.0 with 1,171 employees, Strategy 1 retains ", round(1171 * (1 - 0.38)), " employees (62% retention), ",
            "while Strategy 3 retains ", round(1171 * (1 - 0.60)), " employees (40% retention), demonstrating functional turnover of low performers."
          )
          
          key_findings <- paste0(
            "The analysis reveals critical insights about compensation strategy effectiveness. Performance-based strategies demonstrate highest value when performance variability (SDy) is substantial, ",
            "as the economic benefit of retaining high performers and encouraging functional turnover of low performers creates significant organizational value. ",
            "Merit-based approaches often provide optimal balance when performance differences are moderate, offering performance incentives while maintaining cost control. ",
            "Across-the-board strategies rarely achieve optimal financial returns but may be selected for equity considerations or organizational culture alignment. ",
            "The key strategic insight is matching compensation philosophy to organizational performance variability and strategic priorities."
          )
          
          # Wrap text sections professionally
          overview_wrap <- strwrap(analysis_overview, width = 97, indent = 8, simplify = TRUE)
          overview_wrap <- paste(overview_wrap, collapse = "\n")
          
          strategy_wrap <- strwrap(strategy_explanations, width = 97, indent = 8, simplify = TRUE)
          strategy_wrap <- paste(strategy_wrap, collapse = "\n")
          
          methodology_wrap <- strwrap(economic_methodology, width = 97, indent = 8, simplify = TRUE)
          methodology_wrap <- paste(methodology_wrap, collapse = "\n")
          
          math_wrap <- strwrap(math_examples, width = 97, indent = 8, simplify = TRUE)
          math_wrap <- paste(math_wrap, collapse = "\n")
          
          findings_wrap <- strwrap(key_findings, width = 97, indent = 8, simplify = TRUE)
          findings_wrap <- paste(findings_wrap, collapse = "\n")
          
          incProgress(0.2, detail = "Creating comprehensive visualizations...")
          
          # Create all main plots for the report
          
          # 1. Pay Strategy Plot
          pay_data <- data.frame(
            Rating = rep(results$perf_rating, 3),
            Strategy = rep(c("Strategy 1 (ATB)", "Strategy 2 (Merit)", "Strategy 3 (Performance)"), each = length(results$perf_rating)),
            Pay_Final = c(results$paylevel_final_s1, results$paylevel_final_s2, results$paylevel_final_s3)
          )
          
          pay_plot <- ggplot(pay_data, aes(x = Rating, y = Pay_Final, color = Strategy, group = Strategy)) +
            geom_line(size = 1.2) +
            geom_point(size = 2.5) +
            scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
            scale_y_continuous(labels = dollar_format()) +
            labs(
              title = "Figure 1: Pay Levels After 4 Years by Performance Rating",
              x = "Performance Rating",
              y = "Final Pay Level",
              color = "Strategy"
            ) +
            custom_theme() +
            theme(plot.title = element_text(size = 14, hjust = 0))
          
          # 2. Turnover Plot
          turnover_data <- data.frame(
            Rating = rep(results$perf_rating, 3),
            Strategy = rep(c("Strategy 1", "Strategy 2", "Strategy 3"), each = length(results$perf_rating)),
            Turnover_Rate = c(results$strat1_turnover, results$strat2_turnover, results$strat3_turnover) * 100
          )
          
          turnover_plot <- ggplot(turnover_data, aes(x = Rating, y = Turnover_Rate, color = Strategy, group = Strategy)) +
            geom_line(size = 1.2) +
            geom_point(size = 2.5) +
            scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
            labs(
              title = "Figure 2: Annual Turnover Rates by Performance Rating",
              x = "Performance Rating",
              y = "Turnover Rate (%)",
              color = "Strategy"
            ) +
            custom_theme() +
            theme(plot.title = element_text(size = 14, hjust = 0))
          
          # 3. Cost Analysis Plot
          cost_data <- data.frame(
            Strategy = c("Strategy 1", "Strategy 2", "Strategy 3"),
            Movement_Cost = results$total_move_cost / 1e6,
            Service_Cost = results$total_serv_cost / 1e6
          )
          
          cost_data_long <- cost_data %>%
            pivot_longer(cols = c(Movement_Cost, Service_Cost), names_to = "Cost_Type", values_to = "Cost") %>%
            mutate(Cost_Type = ifelse(Cost_Type == "Movement_Cost", "Movement", "Service"))
          
          cost_plot <- ggplot(cost_data_long, aes(x = Strategy, y = Cost, fill = Cost_Type)) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_manual(values = c("#d62728", "#ff7f0e"), name = "Cost Type") +
            scale_y_continuous(labels = function(x) paste0("$", round(x, 1), "M")) +
            labs(
              title = "Figure 3: 4-Year Movement and Service Costs",
              x = "Strategy",
              y = "Cost (Millions)"
            ) +
            custom_theme() +
            theme(plot.title = element_text(size = 14, hjust = 0))
          
          # 4. Service Value Plot (if analysis results available)
          if(length(results$service_value_results) > 0) {
            service_value_data <- data.frame()
            for(sdy_name in names(results$service_value_results)) {
              sdy_result <- results$service_value_results[[sdy_name]]
              temp_data <- data.frame(
                SDy = sdy_name,
                Strategy = c("Strategy 1", "Strategy 2", "Strategy 3"),
                Service_Value = sdy_result$tot_serv_value_0407
              )
              service_value_data <- rbind(service_value_data, temp_data)
            }
            
            service_plot <- ggplot(service_value_data, aes(x = Strategy, y = Service_Value, fill = SDy)) +
              geom_bar(stat = "identity", position = "dodge") +
              scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")[1:length(unique(service_value_data$SDy))]) +
              scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
              labs(
                title = "Figure 4: 4-Year Total Service Value by SDy Level",
                x = "Strategy",
                y = "Service Value (Millions)",
                fill = "SDy Level"
              ) +
              custom_theme() +
              theme(plot.title = element_text(size = 14, hjust = 0))
          } else {
            service_plot <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "Run analysis to generate service value plot", size = 4) +
              theme_void()
          }
          
          # 5. Investment Value Plot (if analysis results available)
          if(length(results$service_value_results) > 0) {
            investment_data <- data.frame()
            for(sdy_name in names(results$service_value_results)) {
              sdy_result <- results$service_value_results[[sdy_name]]
              investment_value <- sdy_result$tot_serv_value_0407 - results$total_serv_cost - results$total_move_cost
              temp_data <- data.frame(
                SDy = sdy_name,
                Strategy = c("Strategy 1", "Strategy 2", "Strategy 3"),
                Investment_Value = investment_value
              )
              investment_data <- rbind(investment_data, temp_data)
            }
            
            investment_plot <- ggplot(investment_data, aes(x = Strategy, y = Investment_Value, fill = SDy)) +
              geom_bar(stat = "identity", position = "dodge") +
              scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")[1:length(unique(investment_data$SDy))]) +
              scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
              labs(
                title = "Figure 5: 4-Year Net Investment Value by Strategy",
                x = "Strategy",
                y = "Investment Value (Millions)",
                fill = "SDy Level"
              ) +
              custom_theme() +
              theme(plot.title = element_text(size = 14, hjust = 0))
          } else {
            investment_plot <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "Run analysis to generate investment plot", size = 4) +
              theme_void()
          }
          
          incProgress(0.3, detail = "Assembling comprehensive professional layout...")
          
          # Create professional layout elements
          title <- textGrob(
            "Sturman (2003) Comprehensive Compensation Strategy Analysis",
            x = 0.5, y = unit(0.5, "in"),
            gp = gpar(fontsize = 18, fontface = "bold")
          )
          
          # Text sections
          par1 <- textGrob(
            overview_wrap,
            x = unit(0.5, "in"), y = 0.9,
            gp = gpar(fontsize = 10),
            hjust = 0
          )
          
          par2 <- textGrob(
            strategy_wrap,
            x = unit(0.5, "in"), y = 0.7,
            gp = gpar(fontsize = 10),
            hjust = 0
          )
          
          par3 <- textGrob(
            methodology_wrap,
            x = unit(0.5, "in"), y = 0.5,
            gp = gpar(fontsize = 10),
            hjust = 0
          )
          
          par4 <- textGrob(
            math_wrap,
            x = unit(0.5, "in"), y = 0.3,
            gp = gpar(fontsize = 10),
            hjust = 0
          )
          
          par5 <- textGrob(
            findings_wrap,
            x = unit(0.5, "in"), y = 0.1,
            gp = gpar(fontsize = 10),
            hjust = 0
          )
          
          incProgress(0.2, detail = "Creating multi-page comprehensive report...")
          
          # Create multi-page layout
          # Page 1: Title and Overview
          page1 <- gridExtra::arrangeGrob(
            title, par1, par2, par3,
            nrow = 4,
            heights = unit(c(1, 2, 2, 2), "null")
          )
          
          # Page 2: Mathematical Examples and Pay Strategy Plot
          page2 <- gridExtra::arrangeGrob(
            par4, pay_plot,
            nrow = 2,
            heights = unit(c(1.5, 4), "null")
          )
          
          # Page 3: Turnover and Cost Analysis
          page3 <- gridExtra::arrangeGrob(
            turnover_plot, cost_plot,
            nrow = 2,
            heights = unit(c(3, 3), "null")
          )
          
          # Page 4: Service Value and Investment Analysis
          page4 <- gridExtra::arrangeGrob(
            service_plot, investment_plot,
            nrow = 2,
            heights = unit(c(3, 3), "null")
          )
          
          # Page 5: Key Findings
          page5 <- gridExtra::arrangeGrob(
            par5,
            nrow = 1,
            heights = unit(c(6), "null")
          )
          
          incProgress(0.1, detail = "Finalizing comprehensive report...")
          
          # Save multi-page PDF
          pdf(file, width = 8.5, height = 11, onefile = TRUE)
          grid.draw(page1)
          grid.newpage()
          grid.draw(page2)
          grid.newpage()
          grid.draw(page3)
          grid.newpage()
          grid.draw(page4)
          grid.newpage()
          grid.draw(page5)
          dev.off()
          
          showNotification("Comprehensive multi-page PDF report generated successfully!", type = "message")
        })
      }
    )
  })
}

# ============================================================================
# TRAINING MODULE
# ============================================================================

# Training UI Module
trainingUI <- function(id) {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "Training Utility"),
    dashboardSidebar(
      sidebarMenu(
        id = ns("sidebarTrain"),
        menuItem("Training Utility", tabName = "training_utility", icon = icon("file-lines")),
        menuItem("Effect Size", tabName = "effect_size", icon = icon("chart-simple")),
        menuItem("Utility Outputs", tabName = "utility_outputs", icon = icon("sliders-h"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "training_utility",
                fluidPage(
                  titlePanel("Training Utility"),
                  p("This is the training utility section of the UA+ app. Training utility can be described as a return on investment in the development of workers (Avolio et al., 2010). 
                    This return on development investment (RODI) is calculated using the effect size of the training process. In the panel on the left, there is a tab for effect size that gives
                    a descriptive analysis of the effect size that you choose to input. This consists of a Binomial Effect Size Display (BESD) chart that shows the practical significance
                    of training interventions by comparing success rates between trained and untrained groups."),
                  br(),
                  p("The Utility Outputs tab displays the results of utility analysis as well as a plain text description. This utility analysis takes into account the quality and quantity of
                  the training program and workers and compares this value to the cost of the program. It additionally takes into account economic factors such as variable costs, taxes, and investments.
                  The results from this analysis can be displayed in two forms, the RODI as discussed earlier, or as the dollar value of goal-setting. Displaying the results of utility analysis in the form
                  of RODI provides managers with an easily understandable way of seeing the production gains from training programs (Avolio et al., 2010). On the other hand, the goal-setting approach has been 
                  shown to be a useful tool for increasing job performance that managers react positively to (Schmidt, 2013). Sets of inputs from both studies have been provided.")
                )
        ),
        tabItem(tabName = "effect_size",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Binomial Effect Size Display (BESD)"),
                  sidebarLayout(
                    sidebarPanel(
                      id = "sidebar",
                      HTML('<h3>Effect Size Inputs</h3>'),
                      numericInput(ns("dTrain2_1"), "Training Effectiveness (Cohen's d)", 0),
                      selectInput(ns("effect_size_dropdown"), "Or select from common interventions:",
                                  choices = list(
                                    "Custom (enter above)" = "",
                                    "Training: Lower bound (minimal program)" = 0.20,
                                    "Training: Average estimate" = 0.62,
                                    "Training: Upper bound (intensive program)" = 1.00,
                                    "Leadership development: Lower bound" = 0.35,
                                    "Leadership development: Average estimate" = 0.65,
                                    "Leadership development: Upper bound" = 1.37,
                                    "Goal setting" = 0.46
                                  ),
                                  selected = ""),
                      br(),
                      actionButton(ns("goEffect"), "Display Effect Size", class = "btn-primary", style = "width: 100%;")
                    ),
                    mainPanel(
                      plotOutput(ns("training_graph_1")),
                      br(),
                      textOutput(ns("effectText")),
                      downloadButton(ns("effect_download"), "Download PDF")
                    )
                  )
                )
        ),
        tabItem(tabName = "utility_outputs",
                fluidPage(
                  useShinyjs(),
                  titlePanel("Predicting Returns on Training and Developing Employees"),
                  sidebarLayout(
                    sidebarPanel(
                      id = "sidebar",
                      tabsetPanel(
                        id = ns("pageTabs"),
                        tabPanel("Page 1",
                                 HTML('<h3>Utility Inputs (Page 1)</h3>'),
                                 numericInput(ns("nTrain"), "Number of Employees Trained", 0, min = 0, step = 1),
                                 numericInput(ns("tTrain"), "Training Effect Duration", 0, min = 0),
                                 numericInput(ns("dTrain2"), "Effect Size of Procedure", 0),
                                 numericInput(ns("sdyTrain"), "SD of Performance in Monetary Units", 0),
                                 numericInput(ns("sdP"), "SD of Work Output as % of Mean Output", 20, min = 0, max = 100),
                                 numericInput(ns("costTrain2"), "Cost per Employee of Procedure", 0, min = 0),
                                 actionButton(ns("goAvolio"), "RODI Inputs"),
                                 actionButton(ns("goSchmidt"), "Goal Setting Inputs"),
                                 br(),
                                 actionButton(ns("goUn"), "Compute Utility(No Adjustments)")
                        ),
                        tabPanel("Page 2",
                                 HTML('<h3>Utility Inputs (Page 2)</h3>'),
                                 HTML('<h4>Economic Factors</h4>'),
                                 numericInput(ns("vrateTrain"), "Variable Costs (%)", 35, min = 0, max = 100),
                                 numericInput(ns("taxTrain"), "Tax Rate (%)", 63, min = 0, max = 100),
                                 numericInput(ns("discTrain"), "Discount Rate (%)", 11, min = 0, max = 100),
                                 HTML('<h4>Employee Flows</h4>'),
                                 numericInput(ns("lengthTrain"), "Program Length", 15, min = 0),
                                 numericInput(ns("addTrain"), "Employees Trained per Year", 618, min = 0),
                                 numericInput(ns("subTrain"), "Loss of Trained Employees per Year (After Effect Duration)", 618, min = 0),
                                 radioButtons(ns("outputOpt"), "Output Type:",
                                              choices = c("Training Program", "Goal-Setting"),
                                              selected = "Training Program"),
                                 actionButton(ns("go4"), "Compute Utility")
                        )
                      )
                    ),
                    mainPanel(
                      uiOutput(ns("h3")),
                      textOutput(ns("training_utilityUn")),
                      textOutput(ns("break_even_train")),
                      uiOutput(ns("h4")),
                      textOutput(ns("training_utility")),
                      textOutput(ns("training_utility_per_employee")),
                      textOutput(ns("training_utility_per_employee_per_year")),
                      br(),
                      textOutput(ns("trainingText1")),
                      br(),
                      textOutput(ns("trainingText2")),
                      br(),
                      textOutput(ns("trainingText3")),
                      downloadButton(ns("training_download"), "Export Report")
                    )
                  )
                )
        )
      )
    )
  )
}

# Training Server Module
trainingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Toggle states
    observe({
      toggleState(
        id = "goEffect",
        condition = input$dTrain2_1 != 0
      )
    })
    
    # Update effect size when dropdown changes
    observeEvent(input$effect_size_dropdown, {
      if(input$effect_size_dropdown != "") {
        updateNumericInput(session, "dTrain2_1", value = as.numeric(input$effect_size_dropdown))
      }
    })
    
    observeEvent(input$goAvolio, {
      disable("sdP")
      updateNumericInput(session, "nTrain", value = 30, min = 0, step = 1)
      updateNumericInput(session, "tTrain", value = 0.167, min = 0)
      updateNumericInput(session, "dTrain2", value = 0.52)
      updateNumericInput(session, "sdyTrain", value = 40000, min = 0)
      updateNumericInput(session, "costTrain2", value = 2155.77, min = 0)
      updateNumericInput(session, "addTrain", value = 30, min = 0)
      updateNumericInput(session, "subTrain", value = 30, min = 0)
    })
    
    observeEvent(input$goSchmidt, {
      enable("sdP")
      updateNumericInput(session, "nTrain", value = 35, min = 0, step = 1)
      updateNumericInput(session, "tTrain", value = 5, min = 0)
      updateNumericInput(session, "dTrain2", value = 0.46)
      updateNumericInput(session, "sdyTrain", value = 20000, min = 0)
      updateNumericInput(session, "costTrain2", value = 200, min = 0)
      updateNumericInput(session, "addTrain", value = 35, min = 0)
      updateNumericInput(session, "subTrain", value = 35, min = 0)
    })
    
    # Effect Size Tab - BESD Chart
    observeEvent(input$goEffect, {
      enable("effect_download")
      
      dt <- input$dTrain2_1
      
      # Calculate BESD values
      r <- dt / sqrt(dt^2 + 4)
      success_rate_with <- 0.5 + (r / 2)
      success_rate_without <- 0.5 - (r / 2)
      
      # Create BESD data
      besd_data <- data.frame(
        Group = c("Untreated", "Treated"),
        SuccessRate = c(success_rate_without, success_rate_with)
      )
      besd_data$Group <- factor(besd_data$Group, levels = c("Untreated", "Treated"))
      
      # Calculate improvement percentage
      diff_percent <- round((success_rate_with - success_rate_without) * 100, 1)
      
      # Determine intervention type from dropdown or default
      intervention_type <- "Training Program"
      if(input$effect_size_dropdown != "") {
        effect_choices <- list(
          "Training: Lower bound (minimal program)" = 0.20,
          "Training: Average estimate" = 0.62,
          "Training: Upper bound (intensive program)" = 1.00,
          "Leadership development: Lower bound" = 0.35,
          "Leadership development: Average estimate" = 0.65,
          "Leadership development: Upper bound" = 1.37,
          "Goal setting" = 0.46
        )
        selected_label <- names(effect_choices)[which(abs(as.numeric(effect_choices) - as.numeric(input$effect_size_dropdown)) < 0.01)][1]
        if(!is.na(selected_label)) {
          intervention_type <- trimws(sub("[:(].*", "", selected_label))
        }
      }
      
      # Create subtitle
      subtitle_text <- paste0(intervention_type, ": Success rate improves by ", diff_percent, "%")
      
      # Create BESD bar plot
      gg <- ggplot(besd_data, aes(x = Group, y = SuccessRate * 100, fill = Group)) +
        geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = paste0(round(SuccessRate * 100, 1), "%"), y = SuccessRate * 100), 
                  color = "black", size = 7, vjust = -0.5) +
        scale_fill_manual(values = c("Untreated" = "#6BAED6", "Treated" = "#2171B5")) +
        ylim(0, 100) +
        labs(
          x = NULL,
          y = "Success Rate (%)",
          title = "Figure 1. Comparing treated to untreated employees",
          subtitle = subtitle_text
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
          plot.subtitle = element_text(hjust = 0, size = 16, margin = margin(b = 10)),
          axis.text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 18),
          axis.line = element_line(size = 1, colour = "black"),
          panel.grid.minor.y = element_line(color = 'grey80', size = 0.5)
        )
      
      # Render the plot
      output$training_graph_1 <- renderPlot(gg)
      
      # Plain Text explanation
      output$effectText <- renderText({
        paste0("The above chart shows the Binomial Effect Size Display (BESD) for the training program. With an effect size of ", dt, 
               ", the success rate for the treated group is ", round(success_rate_with*100,1), 
               "% compared to ", round(success_rate_without*100,1), "% for the untreated group. ",
               "This means that implementing this intervention improves the success rate by ", diff_percent, 
               " percentage points, demonstrating the practical significance of the training program.")
      })
      
      # Download handler
      output$effect_download <- downloadHandler(
        filename = function() {
          "binomial_effect_size_display.pdf"
        },
        content = function(file) {
          caption_text <- paste0("The above chart shows the Binomial Effect Size Display (BESD) for the training program. With an effect size of ", dt, 
                                ", the success rate for the treated group is ", round(success_rate_with*100,1), 
                                "% compared to ", round(success_rate_without*100,1), "% for the untreated group.")
          
          plot_with_caption <- gg + 
            labs(caption = str_wrap(caption_text, width = 100))
          
          ggsave(
            file,
            plot_with_caption,
            device = "pdf",
            width = 10.625,  
            height = 6.875,  
            units = "in",  
            dpi = 300  
          )
        }
      )
    })
    
    # Training Utility Output Tab
    observeEvent(input$goUn, {
      utilityTrainUn <- input$nTrain * input$dTrain2 * input$sdyTrain * input$tTrain - input$nTrain * input$costTrain2
      breakEven <- (input$nTrain * input$costTrain2) / (input$nTrain * input$tTrain * input$dTrain2)
      
      formatted_breakEvenTrain <- label_dollar(scale = .001, prefix = "$", suffix = "K")(signif(breakEven, 2))
      formatted_utilityTrainUn <- label_dollar(scale = .001, prefix = "$", suffix = "K")(signif(utilityTrainUn, 2))
      
      output$h3 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')})
      output$training_utilityUn <- renderText({
        paste("The opportunity costs of failing to use the program are:", formatted_utilityTrainUn)
      })
      output$break_even_train <- renderText({
        paste("The break even value of SDy is:", formatted_breakEvenTrain)
      })
    })
    
    # Adjusted utility calculation
    observeEvent(input$go4, {
      enable("training_download")
      
      # Create effect size graph for PDF
      u3 <- round(pnorm(input$dTrain2), 2) * 100
      sup <- round(pnorm(input$dTrain2/sqrt(2)), 2) * 100
      
      option <- input$outputOpt
      varCosts <- -input$vrateTrain/100
      tax <- input$taxTrain
      disc <- input$discTrain
      costAc <- input$costTrain2
      validAc <- input$dTrain2
      SDjp <- input$sdyTrain
      tenure1 <- input$tTrain
      last <- input$lengthTrain
      add <- input$addTrain
      subt <- input$subTrain
      
      # Compute adjusted training utility
      discProp <- disc / 100
      valid <- validAc
      ck <- add * (costAc)
      taxProp <- tax / 100
      discRat <- 1 / (1 + discProp)
      numyr <- tenure1 + last
      
      totDelta <- 0
      totDelta1 <- 0
      nk <- 0
      
      for (i in 1:numyr) {
        if (i > ceiling(tenure1)) {nk <- 0}
        if (i <= last) {nk <- nk + add}
        if (i > last) {ck <- 0}
        if (nk > 0) {
          delta1 <- nk * ((discRat^i) * (valid*add*tenure1)/nk * SDjp * (1 + varCosts) * (1 - taxProp))
          delta3 <- nk * ((discRat^i) * (valid*add*tenure1)/nk * SDjp * (-varCosts) * (taxProp))
        }
        if (nk == 0) {
          delta1 <- 0
        }
        delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
        totDelta1 <- totDelta1 + delta2 + delta3
        delta <- delta1 - delta2
        totDelta <- totDelta + delta
      }
      
      adjusted_utility <- totDelta
      adjusted_utility_perHire <- totDelta / (last * add)
      adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
      
      # Format
      formatted_adjusted_utility <- label_dollar(scale = .001, prefix = "$", suffix = "K")(signif(adjusted_utility, 2))
      formatted_adjusted_per_hire_utility <- label_dollar(scale = .001, prefix = "$", suffix = "K")(signif(adjusted_utility_perHire, 2))
      formatted_adjusted_per_year_utility <- label_dollar(scale = .001, prefix = "$", suffix = "K")(signif(adjusted_utility_perHire_perYear, 2))
      
      output$h4 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')})
      
      # if else for months/years
      if (input$tTrain < 1) {
        ym <- paste(round(input$tTrain*12, 0), "months")
      } else {
        ym <- paste(input$tTrain, "years")
      }
      
      # Output for different options
      if (option == "Goal-Setting") {
        if (adjusted_utility < 0) {
          output$training_utility <- renderText({
            paste("The cost of this program will wipe out any potential gains from the program and result in a loss of:", 
                  formatted_adjusted_utility, "in RODI.")
          })
        } else {
          output$training_utility <- renderText({
            paste("Failing to use this training program will have total RODI losses to the company of:", 
                  formatted_adjusted_utility, "in RODI.")
          })
        }
      } else {
        if (adjusted_utility < 0) {
          output$training_utility <- renderText({
            paste("The cost of this program will wipe out any potential gains from the program and result in a loss of:", 
                  formatted_adjusted_utility, "in production value.")
          })
        } else {
          output$training_utility <- renderText({
            paste("Failing to use this program will have total lost production costs to the company of:", 
                  formatted_adjusted_utility, "in production value.")
          })
        }
      }
      
      output$training_utility_per_employee <- renderText({
        paste("Per employee impact:", formatted_adjusted_per_hire_utility)
      })
      
      output$training_utility_per_employee_per_year <- renderText({
        paste("Per employee per year impact:", formatted_adjusted_per_year_utility)
      })
      
      # Narrative text
      output$trainingText1 <- renderText({
        paste0("Analysis conducted for ", input$nTrain, " employees with effect size of ", input$dTrain2, 
               ". Research shows ", u3, "% of untrained employees perform below the mean of trained employees.")
      })
      
      output$trainingText2 <- renderText({
        paste0("Cost analysis: $", input$costTrain2, " per employee. Economic adjustments applied: ",
               input$vrateTrain, "% variable costs, ", input$taxTrain, "% tax rate, ", 
               input$discTrain, "% discount rate.")
      })
      
      output$trainingText3 <- renderText({
        paste0("Training effects last ", ym, ". Program duration: ", input$lengthTrain, 
               " years with ", input$addTrain, " employees trained annually.")
      })
    })
    
    # Training download handler
    output$training_download <- downloadHandler(
      filename = function() {
        "training_utility_report.pdf"
      },
      content = function(file) {
        # Create professional training report matching the format style
        withProgress(message = 'Generating training utility report...', value = 0, {
          incProgress(0.3, detail = "Preparing analysis content...")
          
          # Create comprehensive analysis text sections
          analysis_overview <- paste0(
            "This report presents a comprehensive training utility analysis using established methodologies for evaluating return on development investment (RODI). ",
            "The analysis examines the economic impact of training ", input$nTrain, " employees with an effect size of ", input$dTrain2, ". ",
            "Training effects are projected to last ", input$tTrain, " years at a cost of $", input$costTrain2, " per employee. ",
            "Research demonstrates that ", round(pnorm(input$dTrain2), 2) * 100, "% of untrained employees perform below the mean of trained employees."
          )
          
          economic_factors <- paste0(
            "The analysis incorporates critical economic adjustments to provide realistic projections. ",
            "Variable costs of ", input$vrateTrain, "% account for increased materials and operational expenses associated with higher performance. ",
            "Tax implications are calculated at ", input$taxTrain, "% to reflect the company's effective tax rate on additional profits. ",
            "A discount rate of ", input$discTrain, "% is applied to calculate net present value over the program duration of ", input$lengthTrain, " years."
          )
          
          program_impact <- paste0(
            "The training program will affect ", input$addTrain, " employees annually, with performance improvements lasting an average of ", input$tTrain, " years per participant. ",
            "Based on standard deviation of performance (SDy) of $", input$sdyTrain, ", the analysis calculates both unadjusted and economically adjusted utility values. ",
            "These calculations provide managers with clear ROI metrics and production value estimates for informed decision-making."
          )
          
          # Wrap text sections professionally
          overview_wrap <- strwrap(analysis_overview, width = 97, indent = 8, simplify = TRUE)
          overview_wrap <- paste(overview_wrap, collapse = "\\n")
          
          economic_wrap <- strwrap(economic_factors, width = 97, indent = 8, simplify = TRUE)
          economic_wrap <- paste(economic_wrap, collapse = "\\n")
          
          impact_wrap <- strwrap(program_impact, width = 97, indent = 8, simplify = TRUE)
          impact_wrap <- paste(impact_wrap, collapse = "\\n")
          
          incProgress(0.4, detail = "Creating visualization...")
          
          # Create effect size visualization if available
          if (!is.null(input$dTrain2) && input$dTrain2 > 0) {
            # Create BESD-style visualization
            success_without <- 0.5 - (input$dTrain2 * sqrt(1/8))
            success_with <- 0.5 + (input$dTrain2 * sqrt(1/8))
            
            besd_data <- data.frame(
              Group = c("Untrained", "Trained"),
              Success_Rate = c(success_without * 100, success_with * 100),
              Failure_Rate = c((1 - success_without) * 100, (1 - success_with) * 100)
            )
            
            # Create caption
            caption_text <- paste0(
              "The above chart demonstrates the practical significance of the training program through a Binomial Effect Size Display (BESD). ",
              "With an effect size of ", input$dTrain2, ", the success rate improves from ", round(success_without * 100, 1), 
              "% for untrained employees to ", round(success_with * 100, 1), "% for trained employees, representing a ", 
              round((success_with - success_without) * 100, 1), " percentage point improvement in performance outcomes."
            )
            
            cap_wrap <- strwrap(caption_text, width = 97, simplify = TRUE)
            cap_wrap <- paste(cap_wrap, collapse = "\\n")
            
            main_plot <- ggplot(besd_data, aes(x = Group, y = Success_Rate, fill = Group)) +
              geom_bar(stat = "identity", alpha = 1.0) +
              geom_text(aes(label = paste0(round(Success_Rate, 1), "%")), vjust = -0.5, size = 5, fontweight = "bold") +
              scale_fill_manual(values = c("Untrained" = "#e74c3c", "Trained" = "#27ae60")) +
              scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
              labs(
                title = paste0("Figure 1\\nTraining Effect Size Impact (d = ", input$dTrain2, ")"),
                x = "Group",
                y = "Success Rate (%)",
                caption = cap_wrap
              ) +
              custom_theme() +
              theme(
                legend.position = "none",
                plot.caption = element_text(
                  hjust = 0,
                  margin = margin(t = 10, unit = "pt"),
                  size = 12
                ),
                plot.margin = margin(2, 1, 1, 1, "in"),
                plot.title = element_text(size = 16, hjust = 0.5)
              )
          } else {
            # Create a placeholder if no effect size
            main_plot <- ggplot() +
              annotate("text", x = 0.5, y = 0.5, label = "Run analysis to generate visualization", size = 6) +
              theme_void()
          }
          
          incProgress(0.2, detail = "Assembling professional layout...")
          
          # Create professional layout elements
          title <- textGrob(
            "Training Utility Analysis Report",
            x = 0.5, y = unit(0.5, "in"),
            gp = gpar(fontsize = 20, fontface = "bold")
          )
          
          par1 <- textGrob(
            overview_wrap,
            x = unit(1, "in"), y = 0.8,
            gp = gpar(fontsize = 12),
            hjust = 0
          )
          
          par2 <- textGrob(
            economic_wrap,
            x = unit(1, "in"), y = 0.4,
            gp = gpar(fontsize = 12),
            hjust = 0
          )
          
          par3 <- textGrob(
            impact_wrap,
            x = unit(1, "in"), y = 0.0,
            gp = gpar(fontsize = 12),
            hjust = 0
          )
          
          incProgress(0.1, detail = "Finalizing report...")
          
          # Combine using professional layout
          report_layout <- gridExtra::arrangeGrob(
            title, par1, par2, par3, main_plot,
            nrow = 5,
            heights = unit(c(1, 1.5, 1.5, 1.5, 6), "null")
          )
          
          # Save with professional dimensions
          ggsave(
            file,
            report_layout,
            device = "pdf",
            width = 10.625,
            height = 13.75,
            units = "in",
            dpi = 300
          )
          
          showNotification("Professional training report generated successfully!", type = "message")
        })
      }
    )
  })
}

# ============================================================================
# MAIN APP UI
# ============================================================================

ui <- navbarPage(
  "UA+ Complete Modular",
  
  tabPanel("Overview", 
    fluidPage(
      h2("Utility Analysis+ Complete App"),
      p("This complete modular version includes both Compensation & Benefits (Sturman analysis) and Training Utility modules."),
      
      h4("Available Modules:"),
      tags$ul(
        tags$li(tags$strong("Staffing Utility:"), " Placeholder for staffing analysis tools"),
        tags$li(tags$strong("Comp & Ben Utility:"), " Sturman (2003) performance-based pay analysis"),
        tags$li(tags$strong("Training Utility:"), " Complete training ROI and effect size analysis")
      ),
      
      h4("Benefits of This Architecture:"),
      tags$ul(
        tags$li("Modular design with manageable file sizes"),
        tags$li("Independent modules for easy maintenance"),
        tags$li("Consistent styling across all modules"),
        tags$li("Easy to add new functionality"),
        tags$li("Better organization and testing capabilities")
      ),
      
      p("Navigate to the different tabs above to explore each utility analysis module.")
    )
  ),
  
  tabPanel("Staffing Utility", 
    fluidPage(
      h3("Staffing Utility"),
      p("Staffing utility analysis module will be implemented here."),
      p("This module will include tools for analyzing the ROI of improved selection procedures, expectancy charts, and staffing decision support.")
    )
  ),
  
  tabPanel("Comp & Ben Utility", 
    sturmanUI("sturman")
  ),
  
  tabPanel("Training Utility", 
    trainingUI("training")
  )
)

# ============================================================================
# MAIN APP SERVER
# ============================================================================

server <- function(input, output, session) {
  # Call module servers
  sturmanServer("sturman")
  trainingServer("training")
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server) 