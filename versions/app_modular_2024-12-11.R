# ============================================================================
# MODULAR UTILITY ANALYSIS APP
# ============================================================================
# This version separates the app into manageable modules to avoid large file issues
# and make it easier to add new functionality

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(scales)
library(dplyr)
library(shinyjs)

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
# STURMAN MODULE (COMP & BEN)
# ============================================================================

# Sturman UI Module
sturmanUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Sturman (2003): Performance-Based Pay Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Analysis Parameters"),
        
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
          numericInput(ns("star_percentage"), "% of Workforce that are Stars:", value = 5, min = 1, max = 20, step = 1),
          p(style = "font-size: 12px; color: #666;", 
            "Star performers create exceptional value beyond normal distributions.")
        ),
        
        actionButton(ns("calculate"), "Run Analysis", class = "btn-primary", style = "width: 100%; margin-top: 20px;")
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
          
          tabPanel("Pay Strategies",
                   h4("Pay Levels by Performance Rating After 4 Years"),
                   plotOutput(ns("payPlot"), height = "500px"),
                   h4("Strategy Comparison Tables"),
                   fluidRow(
                     column(4, h5("Strategy 1"), DT::dataTableOutput(ns("payTable1"))),
                     column(4, h5("Strategy 2"), DT::dataTableOutput(ns("payTable2"))),
                     column(4, h5("Strategy 3"), DT::dataTableOutput(ns("payTable3")))
                   )
          ),
          
          tabPanel("Turnover Analysis",
                   h4("Performance Rating Distribution"),
                   plotOutput(ns("perfDistPlot"), height = "350px"),
                   h4("Turnover Rates by Strategy"),
                   plotOutput(ns("turnoverPlot"), height = "450px"),
                   h4("Employee Retention Summary"),
                   DT::dataTableOutput(ns("turnoverTable"))
          ),
          
          tabPanel("Cost Analysis",
                   h4("Movement and Service Costs"),
                   plotOutput(ns("costPlot"), height = "450px"),
                   h4("Cost Breakdown"),
                   DT::dataTableOutput(ns("costTable"))
          ),
          
          tabPanel("Service Value",
                   h4("Service Value by SDy Level"),
                   plotOutput(ns("serviceValuePlot"), height = "450px"),
                   h4("Service Value Analysis"),
                   DT::dataTableOutput(ns("serviceValueTable")),
                   div(id = ns("serviceValueHighlight"), textOutput(ns("serviceValueInsight")))
          ),
          
          tabPanel("Investment Value",
                   h4("Net Investment Value by Strategy"),
                   plotOutput(ns("investmentPlot"), height = "450px"),
                   h4("Investment Analysis"),
                   DT::dataTableOutput(ns("investmentTable")),
                   div(id = ns("investmentHighlight"), textOutput(ns("investmentRecommendation"))),
                   div(textOutput(ns("strategicNarrative")))
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
        
        # Step 2: Turnover probabilities (from original data)
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
        
        # Return simplified results for demonstration
        list(
          perf_rating = perf_rating,
          paylevel_final_s1 = paylevel_final_s1,
          paylevel_final_s2 = paylevel_final_s2,
          paylevel_final_s3 = paylevel_final_s3,
          strat1 = strat1 * 100,
          strat2 = strat2 * 100,
          strat3 = strat3 * 100,
          n_per_perf_rating = n_per_perf_rating,
          n_sep = n_sep
        )
      })
    })
    
    # Pay Strategy Plot
    output$payPlot <- renderPlot({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_data <- data.frame(
        Rating = rep(results$perf_rating, 3),
        Strategy = rep(c("Strategy 1", "Strategy 2", "Strategy 3"), each = length(results$perf_rating)),
        Pay_Final = c(results$paylevel_final_s1, results$paylevel_final_s2, results$paylevel_final_s3)
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
    
    # Pay Tables
    output$payTable1 <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_table <- data.frame(
        Rating = results$perf_rating,
        Increase = paste0(round(results$strat1, 1), "%"),
        Final_Pay = dollar(results$paylevel_final_s1)
      )
      
      DT::datatable(pay_table, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    output$payTable2 <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_table <- data.frame(
        Rating = results$perf_rating,
        Increase = paste0(round(results$strat2, 1), "%"),
        Final_Pay = dollar(results$paylevel_final_s2)
      )
      
      DT::datatable(pay_table, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    output$payTable3 <- DT::renderDataTable({
      results <- analysis_results()
      if(is.null(results)) return(NULL)
      
      pay_table <- data.frame(
        Rating = results$perf_rating,
        Increase = paste0(round(results$strat3, 1), "%"),
        Final_Pay = dollar(results$paylevel_final_s3)
      )
      
      DT::datatable(pay_table, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
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
    
    # Placeholder outputs for other tabs
    output$turnoverPlot <- renderPlot({ 
      plot(1:10, main = "Turnover Analysis (Coming Soon)")
    })
    
    output$turnoverTable <- DT::renderDataTable({
      data.frame(Message = "Analysis in progress...")
    })
    
    output$costPlot <- renderPlot({ 
      plot(1:10, main = "Cost Analysis (Coming Soon)")
    })
    
    output$costTable <- DT::renderDataTable({
      data.frame(Message = "Analysis in progress...")
    })
    
    output$serviceValuePlot <- renderPlot({ 
      plot(1:10, main = "Service Value Analysis (Coming Soon)")
    })
    
    output$serviceValueTable <- DT::renderDataTable({
      data.frame(Message = "Analysis in progress...")
    })
    
    output$serviceValueInsight <- renderText("Analysis in progress...")
    
    output$investmentPlot <- renderPlot({ 
      plot(1:10, main = "Investment Analysis (Coming Soon)")
    })
    
    output$investmentTable <- DT::renderDataTable({
      data.frame(Message = "Analysis in progress...")
    })
    
    output$investmentRecommendation <- renderText("Analysis in progress...")
    output$strategicNarrative <- renderText("Analysis in progress...")
  })
}

# ============================================================================
# MAIN APP UI
# ============================================================================

ui <- navbarPage(
  "UA+ Modular",
  
  tabPanel("Overview", 
    fluidPage(
      h2("Utility Analysis+ App"),
      p("This modular version demonstrates a better architecture for the app."),
      h4("Benefits of Modular Approach:"),
      tags$ul(
        tags$li("Smaller, manageable file sizes"),
        tags$li("Easier to add new functionality"),
        tags$li("Better code organization"),
        tags$li("Reduced integration conflicts"),
        tags$li("Independent module testing")
      )
    )
  ),
  
  tabPanel("Staffing Utility", 
    fluidPage(
      h3("Staffing Utility"),
      p("Staffing utility analysis will be implemented as a module here.")
    )
  ),
  
  tabPanel("Comp & Ben Utility", 
    sturmanUI("sturman")
  ),
  
  tabPanel("Training Utility", 
    fluidPage(
      h3("Training Utility"),
      p("Training utility analysis will be implemented as a module here.")
    )
  )
)

# ============================================================================
# MAIN APP SERVER
# ============================================================================

server <- function(input, output, session) {
  # Call Sturman module server
  sturmanServer("sturman")
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server) 