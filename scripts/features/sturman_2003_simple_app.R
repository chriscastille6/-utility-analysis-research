library(shiny)
library(ggplot2)
library(DT)
library(scales)

# Define UI
ui <- fluidPage(
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
          "Star performers create exceptional value beyond normal distributions. 
          This models scenarios with a small percentage of extremely high performers.")
      ),
      
      actionButton("calculate", "Run Analysis", class = "btn-primary", style = "width: 100%; margin-top: 20px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Understanding Pay Strategy Utility Analysis"),
                 
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
        ),
        
        tabPanel("Step 1: Pay Strategies",
                 h4("Pay Levels by Performance Rating After 4 Years"),
                 p("This analysis shows how much employees at different performance levels will earn after 4 years 
                   under each compensation strategy. ", tags$strong("Final Pay"), " represents the projected salary 
                   in Year 4, after receiving annual increases according to each strategy's formula. The key insight 
                   is understanding how much differentiation each strategy creates between high and low performers."),
                 plotOutput("payPlot", height = "500px"),
                 br(),
                 
                 h4("Strategy Comparison Tables"),
                 p("The three tables below show the specific annual pay increases and final pay levels (Year 4 salary) 
                   for each performance rating under each strategy. ", tags$strong("Rating"), " refers to the employee's 
                   performance rating on a 1-5 scale. ", tags$strong("Increase"), " shows the annual percentage raise 
                   that employee would receive. ", tags$strong("Final Pay"), " shows their projected salary after 
                   4 years of receiving those annual increases."),
                 
                 fluidRow(
                   column(4, 
                          h5("Strategy 1: Across-the-Board", style = "color: #1f77b4; font-weight: bold;"),
                          DT::dataTableOutput("payTable1")),
                   column(4,
                          h5("Strategy 2: Merit-Based", style = "color: #ff7f0e; font-weight: bold;"),
                          DT::dataTableOutput("payTable2")),
                   column(4,
                          h5("Strategy 3: Performance-Based", style = "color: #2ca02c; font-weight: bold;"),
                          DT::dataTableOutput("payTable3"))
                 ),
                 
                 br(),
                 h4("Strategic Implications"),
                 p(tags$strong("Across-the-Board"), " maintains pay equity but provides no performance incentives. 
                   All employees receive identical treatment regardless of contribution."),
                 p(tags$strong("Merit-Based"), " balances equity with performance recognition by providing a base 
                   increase to everyone while rewarding higher performers with additional merit increases."),
                 p(tags$strong("Performance-Based"), " creates the strongest performance incentives but may raise 
                   equity concerns. The large pay differences can motivate high performance but may also increase 
                   turnover among lower performers.")
        ),
        
        tabPanel("Step 2: Turnover Analysis",
                 h4("Understanding Turnover Patterns"),
                 p("Different pay strategies affect employee turnover differently. Performance-based strategies 
                   typically increase turnover among low performers (functional turnover) while reducing turnover 
                   among high performers. This step analyzes these differential effects."),
                 
                 h4("Performance Rating Distribution"),
                 p("This shows the distribution of performance ratings in our analysis sample (5,143 employees). 
                   Most employees cluster around average performance (ratings 2.5-3.5)."),
                 plotOutput("perfDistPlot", height = "350px"),
                 br(),
                 
                 h4("Turnover Rates by Strategy and Performance Level"),
                 p("This chart shows annual turnover rates for each performance level under each strategy. 
                   Notice how performance-based strategies increase turnover among low performers while 
                   reducing it among high performers."),
                 plotOutput("turnoverPlot", height = "450px"),
                 br(),
                 
                 h4("Employee Retention Summary"),
                 p("This table summarizes the total turnover impact of each strategy. ", 
                   tags$strong("Total Employees"), " shows the starting workforce size. ",
                   tags$strong("Employees Retained"), " shows how many stay after 4 years. ",
                   tags$strong("Employees Lost"), " shows departures due to turnover. ",
                   tags$strong("4-Year Turnover Rate"), " shows the total percentage who leave over the analysis period. ",
                   tags$strong("Annual Turnover Rate"), " shows the equivalent yearly turnover rate. ",
                   tags$strong("Quarterly Turnover Rate"), " shows the equivalent quarterly turnover rate. ",
                   "Performance-based strategies typically show higher overall turnover due to increased departures 
                   among low performers, but this can be beneficial if it represents 'functional turnover' of poor performers."),
                 DT::dataTableOutput("turnoverTable"),
                 br(),
                 
                 tags$div(
                   style = "background-color: #fff3cd; padding: 10px; border: 1px solid #ffeaa7; border-radius: 5px;",
                   tags$strong("Note about the Turnover Plot: "), "Strategy 1 (blue line) may be difficult to see 
                   for performance ratings 1-3 because it overlaps with Strategy 2 (orange line). Both strategies 
                   have identical turnover rates for lower performers, differing only for higher performers."
                 )
        ),
        
        tabPanel("Steps 3-4: Cost Analysis",
                 h4("Understanding the Costs of Pay Strategies"),
                 p("Every pay strategy involves two main types of costs: service costs (compensation and benefits) 
                   and movement costs (recruiting, hiring, and training replacements for departing employees). 
                   This analysis calculates the total 4-year costs for each strategy."),
                 
                 h4("Movement and Service Costs Comparison"),
                 p("Movement costs vary by strategy based on turnover rates - strategies with higher turnover 
                   have higher movement costs. Service costs reflect the total compensation expense over 4 years."),
                 plotOutput("costPlot", height = "450px"),
                 br(),
                 
                 h4("Detailed Cost Breakdown"),
                 p("This table shows the specific cost components for each strategy. ",
                   tags$strong("Separations"), " shows the number of employees who leave. ",
                   tags$strong("Movement Cost"), " represents the total expense of recruiting, hiring, and training replacements. ",
                   tags$strong("Service Cost"), " represents the total compensation and benefits expense over 4 years. ",
                   tags$strong("Total Cost"), " combines both movement and service costs. ",
                   tags$strong("Avg Final Pay"), " shows the average salary in Year 4. ",
                   "Pay attention to how movement costs (replacement expenses) vary with turnover levels, while service costs reflect the total 
                   compensation investment."),
                 DT::dataTableOutput("costTable")
        ),
        
        tabPanel("Steps 5-10: Service Value",
                 h4("Understanding Service Value"),
                 p("Service value represents the total economic contribution that employees make to the organization. 
                   It includes both the base value that all employees provide plus additional value based on their 
                   performance level. Higher-performing employees create more service value, and this difference 
                   becomes more pronounced as SDy increases. The analysis calculates how much total service value 
                   is retained or lost under each pay strategy based on turnover patterns."),
                 
                 h4("Calculating the Value Employees Create"),
                 p("Service value combines two components: (1) the base value that all employees provide through their work, and 
                   (2) additional value that varies by performance level. Higher-performing employees create more 
                   service value, and this difference becomes more pronounced as SDy (performance variability) increases."),
                 
                 h4("Service Value by Performance Variability Level"),
                 p("The chart below shows total service value for each strategy at different SDy levels. 
                   Notice how higher SDy levels increase the value advantage of performance-based strategies 
                   because they better retain high-value employees and encourage functional turnover of low performers."),
                 plotOutput("serviceValuePlot", height = "450px"),
                 br(),
                 
                 h4("Service Value Analysis"),
                 p("This table shows the total 4-year service value created under each strategy at different SDy levels. ",
                   tags$strong("SDy Level"), " indicates the performance variability assumption (higher = more difference between high and low performers). ",
                   tags$strong("Strategy 1/2/3 Value"), " shows the total economic value created by the retained and replacement workforce. ",
                   "Performance-based strategies show greater value at higher SDy levels due to better retention 
                   of high performers and functional turnover of low performers."),
                 DT::dataTableOutput("serviceValueTable"),
                 br(),
                 
                 div(id = "serviceValueHighlight", 
                     style = "background-color: #d4edda; padding: 15px; border: 1px solid #c3e6cb; border-radius: 5px;",
                     h5("Key Insight:", style = "color: #155724; margin-top: 0;"),
                     textOutput("serviceValueInsight"))
        ),
        
        tabPanel("Step 11: Investment Value",
                 h4("Final Investment Analysis: Which Strategy Creates the Most Value?"),
                 p("Investment value is calculated as Service Value minus all Costs (service + movement costs). 
                   This represents the net economic benefit of each pay strategy over the 4-year period. 
                   The strategy with the highest investment value provides the best return on investment."),
                 
                 h4("4-Year Net Investment Value by Strategy"),
                 p("The chart below shows the final investment value for each strategy. Positive values indicate 
                   profitable strategies, while the height of each bar shows the relative advantage of each approach."),
                 plotOutput("investmentPlot", height = "450px"),
                 br(),
                 
                 h4("Investment Analysis Summary"),
                 p("This table shows the final investment value calculations. The strategy with the highest 
                   positive value provides the best return on your compensation investment."),
                 DT::dataTableOutput("investmentTable"),
                 br(),
                 
                 div(id = "investmentHighlight", 
                     style = "background-color: #d1ecf1; padding: 15px; border: 1px solid #bee5eb; border-radius: 5px;",
                     h5("Strategic Recommendation:", style = "color: #0c5460; margin-top: 0;"),
                     textOutput("investmentRecommendation")),
                 
                 br(),
                 h4("Key Strategic Insights"),
                 div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                     textOutput("strategicNarrative"))
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Original performance rating data from Sturman 2003
  perf_rating <- seq(1, 5, by = 0.5)
  n_per_perf_rating <- c(60, 97, 1171, 1090, 1667, 672, 317, 46, 23)
  
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
    
    cost_data_long <- reshape2::melt(cost_data, id.vars = "Strategy", variable.name = "Cost_Type", value.name = "Cost")
    cost_data_long$Cost_Type <- ifelse(cost_data_long$Cost_Type == "Movement_Cost", "Movement", "Service")
    
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
}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE)) 