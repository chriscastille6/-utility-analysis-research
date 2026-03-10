# Compensation Analysis Webapp
# Educational tool for understanding market data analysis and pay policy decisions
# Based on compensation management principles and market survey methodologies

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(scales)
library(rmarkdown)
library(knitr)

# =============================================================================
# SAMPLE COMPENSATION DATA
# =============================================================================

# Create realistic compensation survey data for different jobs and sources
create_market_data <- function() {
  set.seed(123)  # For reproducible data
  
  # Job families and positions
  jobs <- data.frame(
    job_family = rep(c("Engineering", "Sales", "Marketing", "HR", "Finance", "Operations"), each = 3),
    job_title = c(
      "Software Engineer I", "Software Engineer II", "Senior Software Engineer",
      "Sales Associate", "Account Manager", "Sales Director", 
      "Marketing Coordinator", "Marketing Manager", "Marketing Director",
      "HR Specialist", "HR Manager", "HR Director",
      "Financial Analyst", "Finance Manager", "Finance Director",
      "Operations Specialist", "Operations Manager", "Operations Director"
    ),
    job_level = rep(c("Individual Contributor", "Manager", "Director"), 6),
    stringsAsFactors = FALSE
  )
  
  # Survey sources with different characteristics
  survey_sources <- data.frame(
    source_name = c("National Compensation Survey", "Tech Industry Report", 
                   "Regional Salary Guide", "Executive Compensation Study", 
                   "Benefits & Compensation Survey"),
    sample_size = c(15000, 3500, 8000, 1200, 5500),
    survey_date = as.Date(c("2024-01-15", "2023-09-30", "2024-02-28", "2023-12-31", "2024-01-30")),
    geographic_scope = c("National", "National", "Regional", "National", "National"),
    industry_focus = c("All Industries", "Technology", "All Industries", "All Industries", "All Industries"),
    quality_rating = c(4.2, 4.5, 3.8, 4.0, 3.9),
    stringsAsFactors = FALSE
  )
  
  # Generate base salaries for each job
  generate_salary_data <- function(job_row, survey_row) {
    # Base salary ranges by job level
    base_salaries <- list(
      "Individual Contributor" = c(45000, 85000),
      "Manager" = c(75000, 130000), 
      "Director" = c(120000, 220000)
    )
    
    # Job family multipliers
    family_multipliers <- list(
      "Engineering" = 1.15,
      "Sales" = 1.05,
      "Marketing" = 1.00,
      "HR" = 0.95,
      "Finance" = 1.08,
      "Operations" = 0.98
    )
    
    # Survey source adjustments
    source_adjustments <- list(
      "National Compensation Survey" = 1.00,
      "Tech Industry Report" = 1.12,
      "Regional Salary Guide" = 0.93,
      "Executive Compensation Study" = 1.05,
      "Benefits & Compensation Survey" = 0.98
    )
    
    base_range <- base_salaries[[job_row$job_level]]
    family_mult <- family_multipliers[[job_row$job_family]]
    source_adj <- source_adjustments[[survey_row$source_name]]
    
    # Calculate 25th, 50th, 75th percentiles
    p25 <- base_range[1] * family_mult * source_adj * rnorm(1, 1, 0.05)
    p50 <- base_range[1] * 1.25 * family_mult * source_adj * rnorm(1, 1, 0.05)
    p75 <- base_range[2] * 0.85 * family_mult * source_adj * rnorm(1, 1, 0.05)
    
    return(data.frame(
      p25 = round(p25, -2),
      p50 = round(p50, -2), 
      p75 = round(p75, -2)
    ))
  }
  
  # Create comprehensive market data
  market_data <- expand.grid(
    job_id = seq_len(nrow(jobs)),
    source_id = seq_len(nrow(survey_sources)),
    stringsAsFactors = FALSE
  )
  
  # Add salary data
  salary_data <- purrr::map2_dfr(market_data$job_id, market_data$source_id, function(j, s) {
    cbind(
      job_id = j,
      source_id = s,
      generate_salary_data(jobs[j, ], survey_sources[s, ])
    )
  })
  
  # Combine all data
  full_market_data <- market_data %>%
    left_join(salary_data, by = c("job_id", "source_id")) %>%
    left_join(jobs %>% mutate(job_id = row_number()), by = "job_id") %>%
    left_join(survey_sources %>% mutate(source_id = row_number()), by = "source_id")
  
  return(list(
    market_data = full_market_data,
    jobs = jobs,
    survey_sources = survey_sources
  ))
}

# Calculate survey weights based on quality metrics
calculate_survey_weights <- function(survey_data, weight_method = "quality_sample") {
  if (weight_method == "equal") {
    survey_data$weight <- 1 / nrow(survey_data)
  } else if (weight_method == "sample_size") {
    survey_data$weight <- survey_data$sample_size / sum(survey_data$sample_size)
  } else if (weight_method == "quality_rating") {
    survey_data$weight <- survey_data$quality_rating / sum(survey_data$quality_rating)
  } else { # quality_sample (default)
    # Combine quality rating and sample size
    normalized_quality <- survey_data$quality_rating / max(survey_data$quality_rating)
    normalized_sample <- survey_data$sample_size / max(survey_data$sample_size)
    combined_score <- (normalized_quality + normalized_sample) / 2
    survey_data$weight <- combined_score / sum(combined_score)
  }
  return(survey_data)
}

# Apply aging factors to market data
apply_aging_factor <- function(salary, survey_date, current_date = Sys.Date(), merit_budget = 0.03) {
  months_old <- as.numeric(difftime(current_date, survey_date, units = "days")) / 30.44
  aging_factor <- (1 + merit_budget) ^ (months_old / 12)
  return(salary * aging_factor)
}

# Calculate weighted market rates
calculate_market_rates <- function(market_data, selected_job, weight_method, aging_enabled, merit_budget) {
  job_data <- market_data %>%
    dplyr::filter(.data$job_title == selected_job)
  
  # Apply aging if enabled
  if (aging_enabled) {
    job_data <- job_data %>%
      dplyr::mutate(
        p25_aged = apply_aging_factor(.data$p25, .data$survey_date, merit_budget = merit_budget),
        p50_aged = apply_aging_factor(.data$p50, .data$survey_date, merit_budget = merit_budget),
        p75_aged = apply_aging_factor(.data$p75, .data$survey_date, merit_budget = merit_budget)
      )
    percentile_cols <- c("p25_aged", "p50_aged", "p75_aged")
  } else {
    percentile_cols <- c("p25", "p50", "p75")
  }
  
  # Calculate weights
  survey_weights <- calculate_survey_weights(
    job_data %>% dplyr::select(.data$source_name, .data$sample_size, .data$quality_rating) %>% dplyr::distinct(),
    weight_method
  )
  
  job_data <- job_data %>%
    dplyr::left_join(survey_weights %>% dplyr::select(.data$source_name, .data$weight), by = "source_name")
  
  # Calculate weighted averages
  weighted_rates <- job_data %>%
    dplyr::summarise(
      p25_weighted = sum(.data[[percentile_cols[1]]] * .data$weight),
      p50_weighted = sum(.data[[percentile_cols[2]]] * .data$weight),
      p75_weighted = sum(.data[[percentile_cols[3]]] * .data$weight),
      .groups = "drop"
    )
  
  return(list(
    job_data = job_data,
    weighted_rates = weighted_rates
  ))
}

# Apply pay policy (lead, match, lag)
apply_pay_policy <- function(market_rates, policy = "match", policy_percentage = 0) {
  if (policy == "lead") {
    multiplier <- 1 + (policy_percentage / 100)
  } else if (policy == "lag") {
    multiplier <- 1 - (policy_percentage / 100)
  } else { # match
    multiplier <- 1
  }
  
  policy_rates <- market_rates * multiplier
  
  return(list(
    market_rates = market_rates,
    policy_rates = policy_rates,
    multiplier = multiplier,
    policy = policy,
    percentage = policy_percentage
  ))
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Compensation Analysis Lab"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Market Data Explorer", tabName = "market_data", icon = icon("database")),
      menuItem("Survey Weighting", tabName = "weighting", icon = icon("balance-scale")),
      menuItem("Aging Analysis", tabName = "aging", icon = icon("calendar")),
      menuItem("Pay Policy Analysis", tabName = "pay_policy", icon = icon("dollar-sign")),
      menuItem("Competitive Analysis", tabName = "competitive", icon = icon("chart-line")),
      menuItem("Salary Structure", tabName = "structure", icon = icon("layer-group")),
      menuItem("Reports", tabName = "reports", icon = icon("file-pdf")),
      menuItem("Methodology", tabName = "methodology", icon = icon("book"))
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
        .info-box {
          border-radius: 5px;
        }
        .small-box {
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
          box(width = 12, title = "Compensation Analysis Lab", status = "primary", solidHeader = TRUE,
            h4("Learning Market Data Analysis for Compensation Decisions"),
            p("This interactive tool helps students understand how compensation professionals use market survey data 
              to make informed pay decisions. You'll learn about survey weighting, aging factors, and pay policy impacts."),
            
            div(style = "background-color: #e8f4fd; padding: 20px; border-radius: 5px; margin: 20px 0;",
              h5("🎯 Learning Objectives:"),
              tags$ul(
                tags$li("Understand how to weight different compensation surveys based on quality and relevance"),
                tags$li("Learn to apply aging factors to account for data currency and salary inflation"),
                tags$li("Analyze the impact of different pay policies (lead, match, lag) on compensation costs"),
                tags$li("Create competitive analysis and salary structures based on market data"),
                tags$li("Make data-driven recommendations for compensation strategy")
              )
            ),
            
            fluidRow(
              column(6,
                h5("📊 Market Data Process"),
                div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                  tags$ol(
                    tags$li(strong("Gather Survey Data:"), " Collect salary information from multiple compensation surveys"),
                    tags$li(strong("Weight Sources:"), " Apply weights based on survey quality, sample size, and relevance"),
                    tags$li(strong("Age Data:"), " Adjust for time elapsed since data collection using merit budget estimates"),
                    tags$li(strong("Apply Pay Policy:"), " Implement lead, match, or lag strategy relative to market"),
                    tags$li(strong("Create Structure:"), " Develop salary ranges and competitive positioning")
                  )
                )
              ),
              
              column(6,
                h5("💼 Business Applications"),
                div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px;",
                  tags$ul(
                    tags$li(strong("Annual Salary Reviews:"), " Setting competitive pay levels for existing roles"),
                    tags$li(strong("New Position Pricing:"), " Determining market-competitive offers for new hires"),
                    tags$li(strong("Budget Planning:"), " Projecting compensation costs under different pay strategies"),
                    tags$li(strong("Retention Strategy:"), " Understanding competitive positioning to reduce turnover"),
                    tags$li(strong("M&A Analysis:"), " Harmonizing pay structures across merged organizations")
                  )
                )
              )
            ),
            
            br(),
            
            fluidRow(
              column(4,
                div(style = "background-color: #d4edda; padding: 15px; border-radius: 5px; text-align: center;",
                  h4(style = "color: #155724; margin-top: 0;", "18"),
                  h6("Job Positions"),
                  p("Across 6 job families and 3 levels")
                )
              ),
              column(4,
                div(style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; text-align: center;",
                  h4(style = "color: #0c5460; margin-top: 0;", "5"),
                  h6("Survey Sources"),
                  p("With varying quality and sample sizes")
                )
              ),
              column(4,
                div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; text-align: center;",
                  h4(style = "color: #856404; margin-top: 0;", "90"),
                  h6("Data Points"),
                  p("Total market observations for analysis")
                )
              )
            ),
            
            br(),
            
            div(style = "background-color: #f0f8ff; padding: 20px; border-radius: 5px;",
              h5("🚀 Getting Started"),
              p(strong("1. Explore Market Data:"), " Start with the Market Data Explorer to see raw survey results"),
              p(strong("2. Weight Surveys:"), " Learn how different weighting methods affect market rates"),
              p(strong("3. Apply Aging:"), " See how data currency impacts market competitiveness"),
              p(strong("4. Test Pay Policies:"), " Compare lead, match, and lag strategies"),
              p(strong("5. Build Structures:"), " Create comprehensive salary ranges and competitive analysis")
            )
          )
        )
      ),
      
      # Market Data Explorer Tab
      tabItem(tabName = "market_data",
        fluidRow(
          box(width = 4, title = "Data Selection", status = "primary", solidHeader = TRUE,
            h5("Job Selection:"),
            selectInput("selected_job", "Choose Position:",
                       choices = NULL,  # Will be populated by server
                       selected = NULL),
            
            br(),
            h5("View Options:"),
            checkboxInput("show_all_sources", "Show All Survey Sources", value = TRUE),
            conditionalPanel(
              condition = "!input.show_all_sources",
              checkboxGroupInput("selected_sources", "Select Sources:",
                               choices = NULL,  # Will be populated by server
                               selected = NULL)
            ),
            
            br(),
            h5("Display Format:"),
            radioButtons("percentile_display", "Salary Percentiles:",
                        choices = list("Show All (25th, 50th, 75th)" = "all",
                                     "50th Percentile Only" = "median",
                                     "Range (25th - 75th)" = "range"),
                        selected = "all"),
            
            br(),
            actionButton("refresh_data", "Refresh Analysis", class = "btn-primary", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Raw Market Data", status = "info", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Data Table",
                br(),
                DT::dataTableOutput("market_data_table")
              ),
              
              tabPanel("Salary Distribution",
                br(),
                plotlyOutput("salary_distribution_plot", height = "400px")
              ),
              
              tabPanel("Source Comparison",
                br(),
                plotlyOutput("source_comparison_plot", height = "400px")
              ),
              
              tabPanel("Data Quality",
                br(),
                fluidRow(
                  column(6,
                    h6("Survey Source Metrics:"),
                    DT::dataTableOutput("source_quality_table")
                  ),
                  column(6,
                    h6("Data Currency Analysis:"),
                    plotlyOutput("data_age_plot", height = "300px")
                  )
                )
              )
            ),
            
            br(),
            
            fluidRow(
              valueBoxOutput("min_salary", width = 4),
              valueBoxOutput("max_salary", width = 4),
              valueBoxOutput("salary_spread", width = 4)
            )
          )
        )
      ),
      
      # Survey Weighting Tab
      tabItem(tabName = "weighting",
        fluidRow(
          box(width = 4, title = "Weighting Parameters", status = "warning", solidHeader = TRUE,
            h5("Position Selection:"),
            selectInput("weighting_job", "Choose Position:",
                       choices = NULL,  # Will be populated by server
                       selected = NULL),
            
            br(),
            h5("Weighting Method:"),
            radioButtons("weight_method", "How to Weight Surveys:",
                        choices = list(
                          "Equal Weight" = "equal",
                          "Sample Size" = "sample_size", 
                          "Quality Rating" = "quality_rating",
                          "Combined (Quality + Sample)" = "quality_sample"
                        ),
                        selected = "quality_sample"),
            
            div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 15px 0;",
              h6("Weighting Methods:"),
              tags$ul(
                tags$li(strong("Equal:"), " All surveys weighted equally"),
                tags$li(strong("Sample Size:"), " Weight by number of participants"),
                tags$li(strong("Quality Rating:"), " Weight by survey methodology quality"),
                tags$li(strong("Combined:"), " Balance both quality and sample size")
              )
            ),
            
            br(),
            h5("Custom Weights:"),
            checkboxInput("use_custom_weights", "Enable Custom Survey Weights", value = FALSE),
            
            conditionalPanel(
              condition = "input.use_custom_weights",
              div(id = "custom_weights_inputs",
                p("Adjust individual survey weights (must sum to 1.0):"),
                # Custom weight inputs will be generated dynamically
                uiOutput("custom_weight_inputs")
              )
            ),
            
            br(),
            actionButton("calculate_weighted", "Calculate Weighted Rates", class = "btn-warning", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Weighted Market Analysis", status = "success", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("weighted_p25", width = 4),
              valueBoxOutput("weighted_p50", width = 4), 
              valueBoxOutput("weighted_p75", width = 4)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Weighting Impact",
                br(),
                plotlyOutput("weighting_comparison_plot"),
                br(),
                htmlOutput("weighting_analysis")
              ),
              
              tabPanel("Survey Weights",
                br(),
                fluidRow(
                  column(6,
                    h6("Applied Weights:"),
                    DT::dataTableOutput("survey_weights_table")
                  ),
                  column(6,
                    h6("Weight Distribution:"),
                    plotlyOutput("weight_distribution_plot", height = "300px")
                  )
                )
              ),
              
              tabPanel("Sensitivity Analysis",
                br(),
                p("See how different weighting methods affect your market rates:"),
                plotlyOutput("sensitivity_analysis_plot"),
                br(),
                htmlOutput("sensitivity_interpretation")
              )
            )
          )
        )
      ),
      
      # Aging Analysis Tab  
      tabItem(tabName = "aging",
        fluidRow(
          box(width = 4, title = "Aging Parameters", status = "info", solidHeader = TRUE,
            h5("Position & Weighting:"),
            selectInput("aging_job", "Choose Position:",
                       choices = NULL,
                       selected = NULL),
            selectInput("aging_weight_method", "Weighting Method:",
                       choices = list(
                         "Equal Weight" = "equal",
                         "Sample Size" = "sample_size",
                         "Quality Rating" = "quality_rating", 
                         "Combined (Quality + Sample)" = "quality_sample"
                       ),
                       selected = "quality_sample"),
            
            br(),
            h5("Aging Settings:"),
            checkboxInput("enable_aging", "Apply Aging Factor", value = TRUE),
            
            conditionalPanel(
              condition = "input.enable_aging",
              numericInput("merit_budget", "Annual Merit Budget (%):",
                          value = 3.0, min = 0, max = 10, step = 0.1),
              
              div(style = "background-color: #e7f4fd; padding: 10px; border-radius: 5px; margin: 10px 0;",
                p(strong("Merit Budget:"), " The average annual salary increase used to age older survey data forward to current date.")
              )
            ),
            
            br(),
            h5("Comparison Date:"),
            dateInput("comparison_date", "Analysis Date:",
                     value = Sys.Date(),
                     min = as.Date("2023-01-01"),
                     max = Sys.Date() + 365),
            
            br(),
            actionButton("calculate_aging", "Apply Aging Analysis", class = "btn-info", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Aging Impact Analysis", status = "primary", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("aged_p25", width = 4),
              valueBoxOutput("aged_p50", width = 4),
              valueBoxOutput("aged_p75", width = 4)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Before vs After Aging",
                br(),
                plotlyOutput("aging_comparison_plot"),
                br(),
                htmlOutput("aging_impact_summary")
              ),
              
              tabPanel("Survey Age Analysis", 
                br(),
                fluidRow(
                  column(6,
                    h6("Survey Data Ages:"),
                    DT::dataTableOutput("survey_age_table")
                  ),
                  column(6,
                    h6("Aging Factors Applied:"),
                    plotlyOutput("aging_factors_plot", height = "300px")
                  )
                )
              ),
              
              tabPanel("Merit Budget Sensitivity",
                br(),
                h6("Impact of Different Merit Budget Assumptions:"),
                plotlyOutput("merit_sensitivity_plot"),
                br(),
                htmlOutput("merit_sensitivity_analysis")
              )
            )
          )
        )
      ),
      
      # Pay Policy Analysis Tab
      tabItem(tabName = "pay_policy",
        fluidRow(
          box(width = 4, title = "Pay Policy Settings", status = "success", solidHeader = TRUE,
            h5("Analysis Parameters:"),
            selectInput("policy_job", "Choose Position:",
                       choices = NULL,
                       selected = NULL),
            selectInput("policy_weight_method", "Weighting Method:",
                       choices = list(
                         "Equal Weight" = "equal",
                         "Sample Size" = "sample_size",
                         "Quality Rating" = "quality_rating",
                         "Combined (Quality + Sample)" = "quality_sample"
                       ),
                       selected = "quality_sample"),
            checkboxInput("policy_enable_aging", "Apply Aging Factor", value = TRUE),
            
            conditionalPanel(
              condition = "input.policy_enable_aging",
              numericInput("policy_merit_budget", "Merit Budget (%):", value = 3.0, min = 0, max = 10, step = 0.1)
            ),
            
            br(),
            h5("Pay Policy Strategy:"),
            radioButtons("pay_policy", "Market Position:",
                        choices = list(
                          "Match Market (0%)" = "match",
                          "Lead Market" = "lead", 
                          "Lag Market" = "lag"
                        ),
                        selected = "match"),
            
            conditionalPanel(
              condition = "input.pay_policy != 'match'",
              numericInput("policy_percentage", "Policy Percentage:",
                          value = 5, min = 1, max = 25, step = 1),
              
              conditionalPanel(
                condition = "input.pay_policy == 'lead'",
                div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px;",
                  p(strong("Lead Strategy:"), " Pay above market to attract top talent and reduce turnover")
                )
              ),
              
              conditionalPanel(
                condition = "input.pay_policy == 'lag'",
                div(style = "background-color: #f8d7da; padding: 10px; border-radius: 5px;",
                  p(strong("Lag Strategy:"), " Pay below market to control costs, may increase turnover risk")
                )
              )
            ),
            
            br(),
            actionButton("calculate_policy", "Apply Pay Policy", class = "btn-success", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Pay Policy Results", status = "warning", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("policy_p25", width = 4),
              valueBoxOutput("policy_p50", width = 4),
              valueBoxOutput("policy_p75", width = 4)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Policy Impact",
                br(),
                plotlyOutput("policy_comparison_plot"),
                br(),
                htmlOutput("policy_impact_analysis")
              ),
              
              tabPanel("Cost Analysis",
                br(),
                fluidRow(
                  column(6,
                    h6("Annual Cost Implications:"),
                    numericInput("employees_count", "Number of Employees:", value = 10, min = 1, max = 1000),
                    htmlOutput("cost_analysis_results")
                  ),
                  column(6,
                    h6("Policy Comparison:"),
                    plotlyOutput("cost_comparison_plot", height = "300px")
                  )
                )
              ),
              
              tabPanel("Strategic Scenarios",
                br(),
                h6("Compare Multiple Pay Policies:"),
                plotlyOutput("scenario_analysis_plot"),
                br(),
                htmlOutput("scenario_recommendations")
              )
            )
          )
        )
      ),
      
      # Competitive Analysis Tab
      tabItem(tabName = "competitive",
        fluidRow(
          box(width = 12, title = "Comprehensive Competitive Analysis", status = "primary", solidHeader = TRUE,
            h5("Multi-Position Analysis"),
            p("Compare market competitiveness across multiple positions and job families."),
            
            fluidRow(
              column(4,
                h6("Analysis Settings:"),
                checkboxGroupInput("competitive_jobs", "Select Positions:",
                                 choices = NULL,  # Populated by server
                                 selected = NULL),
                
                br(),
                selectInput("competitive_weight_method", "Weighting Method:",
                           choices = list(
                             "Combined (Quality + Sample)" = "quality_sample",
                             "Quality Rating" = "quality_rating",
                             "Sample Size" = "sample_size",
                             "Equal Weight" = "equal"
                           ),
                           selected = "quality_sample"),
                
                checkboxInput("competitive_aging", "Apply Aging", value = TRUE),
                conditionalPanel(
                  condition = "input.competitive_aging",
                  numericInput("competitive_merit", "Merit Budget (%):", value = 3.0, min = 0, max = 10, step = 0.1)
                ),
                
                br(),
                selectInput("competitive_policy", "Pay Policy:",
                           choices = list(
                             "Match Market" = "match",
                             "Lead Market 5%" = "lead_5",
                             "Lead Market 10%" = "lead_10",
                             "Lag Market 5%" = "lag_5",
                             "Lag Market 10%" = "lag_10"
                           ),
                           selected = "match"),
                
                br(),
                actionButton("run_competitive", "Run Competitive Analysis", class = "btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                tabsetPanel(
                  tabPanel("Market Positioning",
                    br(),
                    plotlyOutput("competitive_positioning_plot", height = "400px")
                  ),
                  
                  tabPanel("Job Family Analysis",
                    br(),
                    plotlyOutput("job_family_analysis_plot", height = "400px")
                  ),
                  
                  tabPanel("Competitiveness Matrix",
                    br(),
                    DT::dataTableOutput("competitiveness_matrix")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Salary Structure Tab
      tabItem(tabName = "structure",
        fluidRow(
          box(width = 12, title = "Salary Structure Builder", status = "info", solidHeader = TRUE,
            p("Create salary ranges and grade structures based on market analysis."),
            
            fluidRow(
              column(4,
                h5("Structure Parameters:"),
                numericInput("range_spread", "Range Spread (%):",
                            value = 50, min = 25, max = 100, step = 5),
                
                div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
                  p(strong("Range Spread:"), " The difference between minimum and maximum of a salary range, typically 40-60% for most positions.")
                ),
                
                numericInput("range_midpoint_position", "Midpoint Market Position:",
                            value = 50, min = 25, max = 75, step = 5),
                
                selectInput("structure_policy", "Overall Pay Policy:",
                           choices = list(
                             "Match Market (50th percentile)" = "match",
                             "Lead Market (60th percentile)" = "lead", 
                             "Conservative (40th percentile)" = "lag"
                           ),
                           selected = "match"),
                
                br(),
                h5("Grade Assignment:"),
                checkboxInput("auto_grade", "Auto-assign Salary Grades", value = TRUE),
                
                conditionalPanel(
                  condition = "!input.auto_grade",
                  p("Manual grade assignment coming soon...")
                ),
                
                br(),
                actionButton("build_structure", "Build Salary Structure", class = "btn-info", style = "width: 100%;")
              ),
              
              column(8,
                tabsetPanel(
                  tabPanel("Salary Ranges",
                    br(),
                    DT::dataTableOutput("salary_structure_table")
                  ),
                  
                  tabPanel("Structure Visualization",
                    br(),
                    plotlyOutput("salary_structure_plot", height = "500px")
                  ),
                  
                  tabPanel("Grade Analysis",
                    br(),
                    fluidRow(
                      column(6,
                        h6("Grade Statistics:"),
                        DT::dataTableOutput("grade_stats_table")
                      ),
                      column(6,
                        h6("Progression Analysis:"),
                        plotlyOutput("grade_progression_plot", height = "300px")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Reports Tab
      tabItem(tabName = "reports",
        fluidRow(
          box(width = 12, title = "Compensation Analysis Reports", status = "success", solidHeader = TRUE,
            p("Generate comprehensive reports for compensation analysis and decision-making."),
            
            fluidRow(
              column(6,
                h5("Report Parameters:"),
                textInput("report_org_name", "Organization Name:", value = "Your Organization"),
                textInput("report_analyst_name", "Analyst Name:", value = "Compensation Analyst"),
                dateInput("report_date", "Report Date:", value = Sys.Date()),
                
                br(),
                h5("Analysis Scope:"),
                checkboxGroupInput("report_sections", "Include Sections:",
                                 choices = list(
                                   "Executive Summary" = "exec_summary",
                                   "Market Data Analysis" = "market_data",
                                   "Survey Weighting Methodology" = "weighting",
                                   "Aging and Currency Analysis" = "aging",
                                   "Pay Policy Recommendations" = "pay_policy",
                                   "Competitive Positioning" = "competitive",
                                   "Salary Structure" = "structure",
                                   "Implementation Plan" = "implementation"
                                 ),
                                 selected = c("exec_summary", "market_data", "pay_policy", "competitive")),
                
                br(),
                h5("Report Format:"),
                radioButtons("report_format", "Output Format:",
                            choices = list("PDF Report" = "pdf", "HTML Dashboard" = "html"),
                            selected = "pdf")
              ),
              
              column(6,
                h5("Analysis Context:"),
                textAreaInput("report_purpose", "Report Purpose:",
                             value = "Annual compensation review and market competitiveness analysis",
                             rows = 3),
                textAreaInput("report_methodology", "Key Methodology Notes:",
                             value = "Analysis based on multiple compensation surveys with quality-based weighting and aging factors applied",
                             rows = 3),
                textAreaInput("report_recommendations", "Key Recommendations:",
                             value = "Maintain competitive market positioning while managing cost implications",
                             rows = 3),
                
                br(),
                h5("Export Options:"),
                div(style = "background-color: #e7f4fd; padding: 15px; border-radius: 5px;",
                  p(strong("Available Exports:")),
                  tags$ul(
                    tags$li("Comprehensive compensation analysis report"),
                    tags$li("Market data summary tables"),
                    tags$li("Salary structure recommendations"),
                    tags$li("Executive summary presentation")
                  )
                )
              )
            ),
            
            br(),
            
            div(style = "text-align: center;",
              downloadButton("download_report", "Generate Comprehensive Report", 
                           class = "btn-success btn-lg", 
                           style = "padding: 10px 30px; font-size: 16px; margin-right: 15px;"),
              downloadButton("download_data", "Export Analysis Data", 
                           class = "btn-info btn-lg",
                           style = "padding: 10px 30px; font-size: 16px;")
            )
          )
        )
      ),
      
      # Methodology Tab
      tabItem(tabName = "methodology",
        fluidRow(
          box(width = 12, title = "Compensation Analysis Methodology", status = "info", solidHeader = TRUE,
            
            h4("Market Data Analysis Framework"),
            
            div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin: 20px 0;",
              h5("1. Data Collection and Validation"),
              p("Compensation surveys provide the foundation for market analysis. Key considerations include:"),
              tags$ul(
                tags$li(strong("Survey Source Quality:"), " Methodology rigor, sample representativeness, and data validation processes"),
                tags$li(strong("Job Matching:"), " Ensuring accurate comparison between organization roles and survey positions"),
                tags$li(strong("Geographic Scope:"), " Relevance of survey geography to organization's labor market"),
                tags$li(strong("Industry Alignment:"), " Industry-specific surveys vs. general market data")
              )
            ),
            
            div(style = "background-color: #fff3cd; padding: 20px; border-radius: 5px; margin: 20px 0;",
              h5("2. Survey Weighting Methodology"),
              p("Different weighting approaches balance survey quality and relevance:"),
              
              h6("Quality-Based Weighting:"),
              p("W(i) = Quality(i) / Σ Quality(j), where Quality includes methodology rigor, sample size, and currency"),
              
              h6("Sample Size Weighting:"),
              p("W(i) = n(i) / Σ n(j), where n(i) is the sample size for survey i"),
              
              h6("Combined Weighting:"),
              p("W(i) = [α × Quality(i) + β × SampleSize(i)] / Σ [α × Quality(j) + β × SampleSize(j)]"),
              
              p("Where α and β are weighting parameters (typically α = β = 0.5 for equal balance)")
            ),
            
            div(style = "background-color: #e7f4fd; padding: 20px; border-radius: 5px; margin: 20px 0;",
              h5("3. Aging Factor Calculations"),
              p("Compensation data becomes less relevant over time. Aging factors adjust historical data to current market levels:"),
              
              h6("Aging Formula:"),
              p("Aged_Salary = Original_Salary × (1 + Merit_Budget)^(Months_Elapsed / 12)"),
              
              p("Where:"),
              tags$ul(
                tags$li("Merit_Budget is the estimated annual salary increase rate (typically 2-4%)"),
                tags$li("Months_Elapsed is the time between survey date and analysis date"),
                tags$li("The formula assumes compound growth in market rates")
              ),
              
              h6("Data Currency Considerations:"),
              tags$ul(
                tags$li(strong("Fresh Data (0-6 months):"), " Minimal aging adjustment needed"),
                tags$li(strong("Recent Data (6-12 months):"), " Moderate aging factor applied"),
                tags$li(strong("Older Data (12+ months):"), " Significant aging adjustment or data exclusion")
              )
            ),
            
            div(style = "background-color: #d4edda; padding: 20px; border-radius: 5px; margin: 20px 0;",
              h5("4. Pay Policy Implementation"),
              p("Organizations adopt different market positioning strategies:"),
              
              h6("Market Matching:"),
              p("Target_Salary = Market_Rate × 1.0 (typically 50th percentile)"),
              
              h6("Market Leading:"),
              p("Target_Salary = Market_Rate × (1 + Lead_Percentage)"),
              p("Common lead percentages: 5-15% above market"),
              
              h6("Market Lagging:"),
              p("Target_Salary = Market_Rate × (1 - Lag_Percentage)"),
              p("Common lag percentages: 5-10% below market"),
              
              h6("Strategic Considerations:"),
              tags$ul(
                tags$li(strong("Lead Strategy:"), " Higher costs, better talent attraction/retention, competitive advantage"),
                tags$li(strong("Match Strategy:"), " Balanced approach, market-competitive positioning"),
                tags$li(strong("Lag Strategy:"), " Cost control, relies on non-monetary value proposition")
              )
            ),
            
            div(style = "background-color: #f8d7da; padding: 20px; border-radius: 5px; margin: 20px 0;",
              h5("5. Salary Structure Development"),
              p("Creating systematic pay ranges based on market analysis:"),
              
              h6("Range Construction:"),
              p("• Minimum = Midpoint × (1 - Range_Spread/200)"),
              p("• Maximum = Midpoint × (1 + Range_Spread/200)"),
              p("• Midpoint = Market_Rate × Policy_Multiplier"),
              
              h6("Typical Range Spreads:"),
              tags$ul(
                tags$li("Individual Contributors: 40-50%"),
                tags$li("Middle Management: 50-60%"),
                tags$li("Senior Management: 60-80%"),
                tags$li("Executives: 80-100%+")
              )
            ),
            
            h4("Key References and Standards"),
            
            div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px;",
              h5("Professional Standards:"),
              p("• WorldatWork Total Rewards Certification"),
              p("• Society for Human Resource Management (SHRM) Compensation Guidelines"),
              p("• Compensation and Benefits Review - Academic Research"),
              
              h5("Common Survey Sources:"),
              p("• Bureau of Labor Statistics (BLS) - National Compensation Survey"),
              p("• PayScale, Salary.com - Technology and general market data"),
              p("• Willis Towers Watson, Mercer - Comprehensive industry surveys"),
              p("• Robert Half, Hays - Specialized industry reports"),
              
              h5("Statistical Considerations:"),
              p("• Use of percentiles (25th, 50th, 75th) rather than means to handle outliers"),
              p("• Minimum sample size requirements for reliable data (typically n ≥ 10)"),
              p("• Geographic and industry adjustments for local market conditions")
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
  
  # Load market data
  market_data_list <- create_market_data()
  market_data <- market_data_list$market_data
  jobs <- market_data_list$jobs
  survey_sources <- market_data_list$survey_sources
  
  # Update job choices in all selectInputs
  observe({
    job_choices <- setNames(jobs$job_title, paste0(jobs$job_title, " (", jobs$job_family, ")"))
    
    updateSelectInput(session, "selected_job", choices = job_choices, selected = job_choices[1])
    updateSelectInput(session, "weighting_job", choices = job_choices, selected = job_choices[1])
    updateSelectInput(session, "aging_job", choices = job_choices, selected = job_choices[1])
    updateSelectInput(session, "policy_job", choices = job_choices, selected = job_choices[1])
    
    updateCheckboxGroupInput(session, "competitive_jobs", choices = job_choices, 
                           selected = job_choices[seq_len(min(3, length(job_choices)))])
  })
  
  # Update source choices
  observe({
    source_choices <- setNames(survey_sources$source_name, survey_sources$source_name)
    updateCheckboxGroupInput(session, "selected_sources", choices = source_choices, selected = source_choices)
  })
  
  # Market Data Table
  output$market_data_table <- DT::renderDataTable({
    req(input$selected_job)
    
    job_data <- market_data %>%
      dplyr::filter(.data$job_title == input$selected_job)
    
    if (!input$show_all_sources && !is.null(input$selected_sources)) {
      job_data <- job_data %>%
        dplyr::filter(.data$source_name %in% input$selected_sources)
    }
    
    display_data <- job_data %>%
      dplyr::select(.data$source_name, .data$survey_date, .data$sample_size, .data$quality_rating, .data$p25, .data$p50, .data$p75) %>%
      dplyr::mutate(
        survey_date = format(.data$survey_date, "%Y-%m-%d"),
        p25 = paste0("$", format(.data$p25, big.mark = ",")),
        p50 = paste0("$", format(.data$p50, big.mark = ",")),
        p75 = paste0("$", format(.data$p75, big.mark = ","))
      )
    
    colnames(display_data) <- c("Survey Source", "Date", "Sample Size", "Quality", "25th %ile", "50th %ile", "75th %ile")
    
    DT::datatable(display_data, 
                 options = list(pageLength = 10, scrollX = TRUE),
                 caption = paste("Market Data for", input$selected_job)) %>%
      DT::formatStyle("Quality", 
                     backgroundColor = DT::styleInterval(c(3.5, 4.0, 4.5), 
                                                       c("#f8d7da", "#fff3cd", "#d1ecf1", "#d4edda")))
  })
  
  # Value boxes for market data
  output$min_salary <- renderValueBox({
    req(input$selected_job)
    job_data <- market_data %>% dplyr::filter(.data$job_title == input$selected_job)
    min_val <- min(job_data$p25, na.rm = TRUE)
    
    valueBox(
      value = paste0("$", format(min_val, big.mark = ",")),
      subtitle = "Minimum (25th %ile)",
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$max_salary <- renderValueBox({
    req(input$selected_job)
    job_data <- market_data %>% dplyr::filter(.data$job_title == input$selected_job)
    max_val <- max(job_data$p75, na.rm = TRUE)
    
    valueBox(
      value = paste0("$", format(max_val, big.mark = ",")),
      subtitle = "Maximum (75th %ile)", 
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$salary_spread <- renderValueBox({
    req(input$selected_job)
    job_data <- market_data %>% dplyr::filter(.data$job_title == input$selected_job)
    min_val <- min(job_data$p25, na.rm = TRUE)
    max_val <- max(job_data$p75, na.rm = TRUE)
    spread_pct <- round(((max_val - min_val) / min_val) * 100, 0)
    
    valueBox(
      value = paste0(spread_pct, "%"),
      subtitle = "Market Spread",
      icon = icon("expand-arrows-alt"),
      color = "blue"
    )
  })
  
  # Salary Distribution Plot
  output$salary_distribution_plot <- renderPlotly({
    req(input$selected_job)
    
    job_data <- market_data %>%
      dplyr::filter(.data$job_title == input$selected_job)
    
    if (!input$show_all_sources && !is.null(input$selected_sources)) {
      job_data <- job_data %>%
        dplyr::filter(.data$source_name %in% input$selected_sources)
    }
    
    # Reshape data for plotting
    plot_data <- job_data %>%
      dplyr::select(.data$source_name, .data$p25, .data$p50, .data$p75) %>%
      tidyr::pivot_longer(cols = c("p25", "p50", "p75"), names_to = "percentile", values_to = "salary") %>%
      dplyr::mutate(percentile = factor(.data$percentile, levels = c("p25", "p50", "p75"), 
                               labels = c("25th %ile", "50th %ile", "75th %ile")))
    
    p <- ggplot(plot_data, aes(x = .data$source_name, y = .data$salary, fill = .data$percentile)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(values = c("25th %ile" = "#3498db", "50th %ile" = "#2ecc71", "75th %ile" = "#e74c3c")) +
      labs(title = paste("Salary Distribution -", input$selected_job),
           x = "Survey Source", y = "Salary", fill = "Percentile") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Source Comparison Plot
  output$source_comparison_plot <- renderPlotly({
    req(input$selected_job)
    
    job_data <- market_data %>%
      dplyr::filter(.data$job_title == input$selected_job)
    
    p <- ggplot(job_data, aes(x = .data$quality_rating, y = .data$p50, size = .data$sample_size, color = .data$source_name)) +
      geom_point(alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_size_continuous(name = "Sample Size", range = c(3, 10)) +
      labs(title = "Survey Quality vs. Market Rate",
           x = "Quality Rating", y = "50th Percentile Salary", color = "Survey Source") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Additional outputs would continue here with weighting analysis, aging factors, etc.
  # For brevity, showing the structure and key components
  
  # Weighting analysis reactive
  weighting_results <- eventReactive(input$calculate_weighted, {
    req(input$weighting_job, input$weight_method)
    calculate_market_rates(market_data, input$weighting_job, input$weight_method, FALSE, 0.03)
  })
  
  # Aging analysis reactive  
  aging_results <- eventReactive(input$calculate_aging, {
    req(input$aging_job, input$aging_weight_method)
    calculate_market_rates(market_data, input$aging_job, input$aging_weight_method, 
                          input$enable_aging, input$merit_budget / 100)
  })
  
  # Pay policy analysis reactive
  policy_results <- eventReactive(input$calculate_policy, {
    req(input$policy_job, input$policy_weight_method, input$pay_policy)
    
    # First get market rates
    market_result <- calculate_market_rates(market_data, input$policy_job, input$policy_weight_method,
                                          input$policy_enable_aging, input$policy_merit_budget / 100)
    
    # Then apply pay policy
    policy_pct <- if(input$pay_policy == "match") 0 else input$policy_percentage
    policy_result <- apply_pay_policy(market_result$weighted_rates, input$pay_policy, policy_pct)
    
    list(
      market_rates = market_result$weighted_rates,
      policy_rates = policy_result$policy_rates,
      policy_details = policy_result
    )
  })
  
  # Weighted rates value boxes
  output$weighted_p25 <- renderValueBox({
    results <- weighting_results()
    valueBox(
      value = paste0("$", format(round(results$weighted_rates$p25_weighted), big.mark = ",")),
      subtitle = "25th Percentile",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$weighted_p50 <- renderValueBox({
    results <- weighting_results()
    valueBox(
      value = paste0("$", format(round(results$weighted_rates$p50_weighted), big.mark = ",")),
      subtitle = "50th Percentile",
      icon = icon("chart-bar"),
      color = "green"
    )
  })
  
  output$weighted_p75 <- renderValueBox({
    results <- weighting_results()
    valueBox(
      value = paste0("$", format(round(results$weighted_rates$p75_weighted), big.mark = ",")),
      subtitle = "75th Percentile", 
      icon = icon("chart-bar"),
      color = "orange"
    )
  })
  
  # Aged rates value boxes
  output$aged_p25 <- renderValueBox({
    results <- aging_results()
    valueBox(
      value = paste0("$", format(round(results$weighted_rates$p25_weighted), big.mark = ",")),
      subtitle = "25th Percentile (Aged)",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$aged_p50 <- renderValueBox({
    results <- aging_results()
    valueBox(
      value = paste0("$", format(round(results$weighted_rates$p50_weighted), big.mark = ",")),
      subtitle = "50th Percentile (Aged)",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$aged_p75 <- renderValueBox({
    results <- aging_results()
    valueBox(
      value = paste0("$", format(round(results$weighted_rates$p75_weighted), big.mark = ",")),
      subtitle = "75th Percentile (Aged)",
      icon = icon("calendar"),
      color = "orange"
    )
  })
  
  # Policy rates value boxes
  output$policy_p25 <- renderValueBox({
    results <- policy_results()
    valueBox(
      value = paste0("$", format(round(results$policy_rates$p25_weighted), big.mark = ",")),
      subtitle = "25th Percentile (Policy)",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$policy_p50 <- renderValueBox({
    results <- policy_results()
    valueBox(
      value = paste0("$", format(round(results$policy_rates$p50_weighted), big.mark = ",")),
      subtitle = "50th Percentile (Policy)",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$policy_p75 <- renderValueBox({
    results <- policy_results()
    valueBox(
      value = paste0("$", format(round(results$policy_rates$p75_weighted), big.mark = ",")),
      subtitle = "75th Percentile (Policy)",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  # Weighting comparison plot
  output$weighting_comparison_plot <- renderPlotly({
    results <- weighting_results()
    
    # Create comparison data for different weighting methods
    job_data <- market_data %>%
      dplyr::filter(.data$job_title == input$weighting_job)
    
    # Calculate rates for all weighting methods
    equal_rates <- calculate_market_rates(market_data, input$weighting_job, "equal", FALSE, 0.03)
    sample_rates <- calculate_market_rates(market_data, input$weighting_job, "sample_size", FALSE, 0.03)
    quality_rates <- calculate_market_rates(market_data, input$weighting_job, "quality_rating", FALSE, 0.03)
    combined_rates <- calculate_market_rates(market_data, input$weighting_job, "quality_sample", FALSE, 0.03)
    
    comparison_data <- data.frame(
      method = rep(c("Equal", "Sample Size", "Quality", "Combined"), each = 3),
      percentile = rep(c("25th", "50th", "75th"), 4),
      rate = c(
        equal_rates$weighted_rates$p25_weighted, equal_rates$weighted_rates$p50_weighted, equal_rates$weighted_rates$p75_weighted,
        sample_rates$weighted_rates$p25_weighted, sample_rates$weighted_rates$p50_weighted, sample_rates$weighted_rates$p75_weighted,
        quality_rates$weighted_rates$p25_weighted, quality_rates$weighted_rates$p50_weighted, quality_rates$weighted_rates$p75_weighted,
        combined_rates$weighted_rates$p25_weighted, combined_rates$weighted_rates$p50_weighted, combined_rates$weighted_rates$p75_weighted
      )
    )
    
    p <- ggplot(comparison_data, aes(x = .data$method, y = .data$rate, fill = .data$percentile)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(values = c("25th" = "#3498db", "50th" = "#2ecc71", "75th" = "#e74c3c")) +
      labs(title = "Impact of Weighting Methods on Market Rates",
           x = "Weighting Method", y = "Market Rate", fill = "Percentile") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Survey weights table
  output$survey_weights_table <- DT::renderDataTable({
    results <- weighting_results()
    
    weights_data <- results$job_data %>%
      dplyr::select(.data$source_name, .data$sample_size, .data$quality_rating, .data$weight) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        weight_pct = round(.data$weight * 100, 1),
        weight = round(.data$weight, 3)
      ) %>%
      dplyr::arrange(dplyr::desc(.data$weight))
    
    colnames(weights_data) <- c("Survey Source", "Sample Size", "Quality Rating", "Weight", "Weight %")
    
    DT::datatable(weights_data, 
                 options = list(pageLength = 10, dom = 't'),
                 caption = "Applied Survey Weights") %>%
      DT::formatStyle("Weight %", 
                     backgroundColor = DT::styleColorBar(range(weights_data$`Weight %`), "#d1ecf1"))
  })
  
  # Aging impact analysis
  output$aging_comparison_plot <- renderPlotly({
    req(input$enable_aging)
    
    # Get both aged and non-aged results
    aged_results <- aging_results()
    non_aged_results <- calculate_market_rates(market_data, input$aging_job, input$aging_weight_method, FALSE, 0.03)
    
    comparison_data <- data.frame(
      scenario = rep(c("Original", "Aged"), each = 3),
      percentile = rep(c("25th", "50th", "75th"), 2),
      rate = c(
        non_aged_results$weighted_rates$p25_weighted, non_aged_results$weighted_rates$p50_weighted, non_aged_results$weighted_rates$p75_weighted,
        aged_results$weighted_rates$p25_weighted, aged_results$weighted_rates$p50_weighted, aged_results$weighted_rates$p75_weighted
      )
    )
    
    p <- ggplot(comparison_data, aes(x = .data$percentile, y = .data$rate, fill = .data$scenario)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(values = c("Original" = "#6c757d", "Aged" = "#28a745")) +
      labs(title = "Impact of Aging on Market Rates",
           x = "Percentile", y = "Market Rate", fill = "Data Status") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Policy comparison plot
  output$policy_comparison_plot <- renderPlotly({
    results <- policy_results()
    
    comparison_data <- data.frame(
      scenario = rep(c("Market Rate", "Policy Rate"), each = 3),
      percentile = rep(c("25th", "50th", "75th"), 2),
      rate = c(
        results$market_rates$p25_weighted, results$market_rates$p50_weighted, results$market_rates$p75_weighted,
        results$policy_rates$p25_weighted, results$policy_rates$p50_weighted, results$policy_rates$p75_weighted
      )
    )
    
    policy_color <- switch(input$pay_policy,
                          "lead" = "#28a745",
                          "lag" = "#dc3545", 
                          "match" = "#6c757d")
    
    p <- ggplot(comparison_data, aes(x = .data$percentile, y = .data$rate, fill = .data$scenario)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(values = c("Market Rate" = "#6c757d", "Policy Rate" = policy_color)) +
      labs(title = paste("Pay Policy Impact:", stringr::str_to_title(input$pay_policy), "Strategy"),
           x = "Percentile", y = "Rate", fill = "Rate Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Download handlers
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("compensation_analysis_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Report generation logic would go here
      cat("Comprehensive compensation analysis report would be generated here", file = file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("market_data_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(market_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
