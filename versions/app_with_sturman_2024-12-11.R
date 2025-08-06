# Load Libraries
library(shiny)
library(shinydashboard)
library(iopsych)
library(scales)
library(shinyjs)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(stringr)
library(grid)
library(gridExtra)
library(gridtext)
library(plotly)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(DT)

# Source the original app components
source("app.R")

# Define the Sturman Comp & Ben UI
sturman_comp_ben_ui <- fluidPage(
  titlePanel("Sturman (2003): Performance-Based Pay Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Analysis Parameters"),
      
      # Base parameters
      numericInput("paylevel_base", "Base Pay Level ($):", value = 47983, min = 30000, max = 100000, step = 1000),
      
      # Strategy 1 parameters
      h5("Strategy 1: Across-the-Board"),
      numericInput("strategy1_rate", "Annual Increase Rate (%):", value = 4, min = 0, max = 10, step = 0.5),
      
      # Strategy 2 parameters  
      h5("Strategy 2: Merit-Based"),
      numericInput("strategy2_base", "Base Rate (%):", value = 4, min = 0, max = 10, step = 0.5),
      selectInput("strategy2_merit_start", "Merit Threshold Rating:", 
                  choices = list("Meets Expectations (3.0)" = 3.0, 
                                "Exceeds Expectations (3.5)" = 3.5,
                                "Far Exceeds Expectations (4.0)" = 4.0),
                  selected = 3.0),
      numericInput("strategy2_merit_rate", "Merit Increment per Rating Point (%):", value = 1, min = 0, max = 5, step = 0.5),
      
      # Strategy 3 parameters
      h5("Strategy 3: Performance-Based"),
      numericInput("strategy3_min", "Minimum Rate (%):", value = 0, min = 0, max = 5, step = 0.5),
      numericInput("strategy3_max", "Maximum Rate (%):", value = 8, min = 5, max = 15, step = 0.5),
      
      # Cost parameters
      h5("Cost Parameters"),
      numericInput("move_cost_mult", "Movement Cost Multiplier:", value = 2.0, min = 1.0, max = 5.0, step = 0.1),
      numericInput("serv_cost_mult", "Service Cost Multiplier:", value = 1.37, min = 1.0, max = 3.0, step = 0.01),
      numericInput("serv_value_mult", "Service Value Multiplier:", value = 1.754, min = 1.0, max = 3.0, step = 0.001),
      
      # SDy parameters
      h5("Performance Variability (SDy)"),
      numericInput("sdy_low", "Low SDy (% of salary):", value = 30, min = 10, max = 50, step = 5),
      numericInput("sdy_medium", "Medium SDy (% of salary):", value = 60, min = 40, max = 80, step = 5),
      numericInput("sdy_high", "High SDy (% of salary):", value = 90, min = 70, max = 120, step = 5),
      checkboxGroupInput("sdy_levels", "Select SDy Levels to Analyze:",
                        choices = list("Low" = "low", "Medium" = "medium", "High" = "high"),
                        selected = c("low", "medium", "high")),
      
      # Star Power option
      h5("Star Performer Analysis"),
      checkboxInput("enable_star_power", "Enable Star Power Analysis", value = FALSE),
      conditionalPanel(
        condition = "input.enable_star_power == true",
        numericInput("star_sdy", "Star SDy (% of salary):", value = 150, min = 100, max = 300, step = 10),
        numericInput("star_percentage", "% of Workforce that are Stars:", value = 5, min = 1, max = 20, step = 1),
        p(style = "font-size: 12px; color: #666;", 
          "Star performers create exceptional value beyond normal distributions.")
      ),
      
      actionButton("calculate", "Run Analysis", class = "btn-primary", style = "width: 100%; margin-top: 20px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Understanding Pay Strategy Utility Analysis"),
                 p("This analysis helps organizations evaluate the financial impact of different compensation strategies 
                   by comparing their total costs against the value they create over a 4-year period."),
                 
                 h4("Research Foundation"),
                 p("Based on Sturman, Trevor, Boudreau, and Gerhart (2003) research using data from 5,143 employees."),
                 
                 h4("The Three Pay Strategies"),
                 tags$div(
                   style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #007bff;",
                   tags$strong("Strategy 1: Across-the-Board"), br(),
                   "Equal percentage increases for all employees regardless of performance."
                 ),
                 tags$div(
                   style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #28a745;",
                   tags$strong("Strategy 2: Merit-Based"), br(),
                   "Base increase plus merit increases for higher performers."
                 ),
                 tags$div(
                   style = "background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-left: 4px solid #dc3545;",
                   tags$strong("Strategy 3: Performance-Based"), br(),
                   "Pay increases directly tied to performance ratings."
                 )
        ),
        
        tabPanel("Step 1: Pay Strategies",
                 h4("Pay Levels by Performance Rating After 4 Years"),
                 plotOutput("payPlot", height = "500px"),
                 h4("Strategy Comparison Tables"),
                 fluidRow(
                   column(4, h5("Strategy 1: Across-the-Board"), DT::dataTableOutput("payTable1")),
                   column(4, h5("Strategy 2: Merit-Based"), DT::dataTableOutput("payTable2")),
                   column(4, h5("Strategy 3: Performance-Based"), DT::dataTableOutput("payTable3"))
                 )
        ),
        
        tabPanel("Step 2: Turnover Analysis",
                 h4("Performance Rating Distribution"),
                 plotOutput("perfDistPlot", height = "350px"),
                 h4("Turnover Rates by Strategy"),
                 plotOutput("turnoverPlot", height = "450px"),
                 h4("Employee Retention Summary"),
                 DT::dataTableOutput("turnoverTable")
        ),
        
        tabPanel("Steps 3-4: Cost Analysis",
                 h4("Movement and Service Costs Comparison"),
                 plotOutput("costPlot", height = "450px"),
                 h4("Detailed Cost Breakdown"),
                 DT::dataTableOutput("costTable")
        ),
        
        tabPanel("Steps 5-10: Service Value",
                 h4("Service Value by Performance Variability Level"),
                 plotOutput("serviceValuePlot", height = "450px"),
                 h4("Service Value Analysis"),
                 DT::dataTableOutput("serviceValueTable"),
                 div(id = "serviceValueHighlight", textOutput("serviceValueInsight"))
        ),
        
        tabPanel("Step 11: Investment Value",
                 h4("4-Year Net Investment Value by Strategy"),
                 plotOutput("investmentPlot", height = "450px"),
                 h4("Investment Analysis Summary"),
                 DT::dataTableOutput("investmentTable"),
                 div(id = "investmentHighlight", textOutput("investmentRecommendation")),
                 div(textOutput("strategicNarrative"))
        )
      )
    )
  )
)

# Create new navbar UI with integrated Sturman analysis
new_ui <- navbarPage(
  "UA+",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Comp & Ben Utility", sturman_comp_ben_ui),
  tabPanel("Training Utility", training_ui),
  tabPanel("Glossary", glossary_ui),
  tabPanel("References", reference_ui)
)

# Extended server function that includes Sturman analysis
extended_server <- function(input, output, session) {
  
  # Call the original server function
  main_server(input, output, session)
  
  # Add Sturman analysis server logic
  source("scripts/features/sturman_2003_simple_app.R", local = TRUE)
  
  # Extract just the server components from the sourced file
  # (This will include all the reactive functions, plots, and tables)
}

# Run the enhanced app
shinyApp(ui = new_ui, server = extended_server) 