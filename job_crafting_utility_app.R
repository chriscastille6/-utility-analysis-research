# Job Crafting Intervention Utility Analysis App
# Based on Oprea et al. (2019) - European Journal of Work and Organizational Psychology
# Interactive tool for analyzing the economic value of job crafting interventions

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(rmarkdown)
library(knitr)

# Load job crafting data based on Oprea et al. (2019) meta-analysis
load_jc_data <- function() {
  
  # Meta-analysis effect sizes from Oprea et al. (2019)
  meta_effects <- tibble::tibble(
    outcome = c("Overall Job Crafting", "Seeking Challenges", "Reducing Demands", 
               "Work Engagement", "Task Performance (Healthcare)", "Contextual Performance"),
    effect_size = c(0.26, 0.19, 0.44, 0.31, 0.47, 0.39),
    ci_lower = c(0.11, 0.05, 0.19, 0.14, 0.21, 0.01),
    ci_upper = c(0.40, 0.33, 0.69, 0.50, 0.73, 0.78),
    k_studies = c(14, 8, 7, 10, 3, 4),
    sample_size = c(1204, 578, 388, 765, 178, 221),
    significance = c("***", "**", "***", "***", "***", "*"),
    interpretation = c("Small-Medium", "Small", "Small-Medium", "Small-Medium", 
                      "Small-Medium", "Small-Medium")
  )
  
  # Utility analysis parameters from Oprea et al. (2019)
  utility_params <- list(
    baseline_salary = 50000,  # Annual salary baseline
    sdy_percentage = 0.40,    # SDy as percentage of salary (40%)
    intervention_cost = 40,   # Cost per employee
    intervention_duration = 0.25,  # 3 months = 25% of year
    effect_size_healthcare = 0.47,  # Most reliable effect (healthcare task performance)
    sdp_percentage = 0.30,    # Standard deviation of performance as % of mean output
    break_even_costs = c(2350, 1410, 940)  # Break-even costs for different salary levels
  )
  
  # Industry effect moderators (based on paper findings)
  industry_effects <- tibble::tibble(
    industry = c("Healthcare", "Education", "Manufacturing", "Service", "Technology", "Other"),
    task_performance_effect = c(0.47, 0.20, 0.15, 0.25, 0.30, 0.26),
    engagement_effect = c(0.35, 0.31, 0.28, 0.31, 0.33, 0.31),
    reliability = c("High", "Medium", "Medium", "Medium", "Medium", "Low"),
    evidence_base = c("3 studies", "Estimated", "Estimated", "Estimated", "Estimated", "Overall effect")
  )
  
  return(list(
    meta_effects = meta_effects,
    utility_params = utility_params,
    industry_effects = industry_effects
  ))
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Job Crafting Intervention Utility Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Meta-Analysis Results", tabName = "meta", icon = icon("chart-bar")),
      menuItem("Utility Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Break-Even Analysis", tabName = "breakeven", icon = icon("balance-scale")),
      menuItem("Industry Comparison", tabName = "industry", icon = icon("industry")),
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
        .small-box {
          border-radius: 5px;
        }
        .btn {
          border-radius: 4px;
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
          box(width = 12, title = "Job Crafting Intervention Utility Analysis", status = "primary", solidHeader = TRUE,
            h4("Understanding the Economic Value of Job Crafting Interventions"),
            p("This interactive tool helps organizations understand the financial benefits of implementing job crafting interventions, 
              based on the comprehensive meta-analysis and utility analysis by", 
              strong("Oprea et al. (2019)"), "published in the European Journal of Work and Organizational Psychology."),
            
            h5("What is Job Crafting?"),
            p("Job crafting (JC) represents the proactive changes made by employees regarding their job demands and job resources. 
              It includes three main dimensions:"),
            tags$ul(
              tags$li(strong("Structural crafting:"), "Changing the number, scope, or type of job tasks"),
              tags$li(strong("Social crafting:"), "Changing the quality and amount of interactions with others"),
              tags$li(strong("Cognitive crafting:"), "Changing how one perceives the job and its purpose")
            ),
            
            h5("Key Research Findings"),
            p("The meta-analysis of 14 studies with 1,204 participants revealed significant effects of job crafting interventions on:"),
            
            br(),
            
            fluidRow(
              valueBoxOutput("overall_effect", width = 4),
              valueBoxOutput("engagement_effect", width = 4),
              valueBoxOutput("performance_effect", width = 4)
            ),
            
            br(),
            
            h5("Economic Impact Highlights"),
            p("Utility analysis for healthcare professionals (the most reliable effect) shows:"),
            
            fluidRow(
              infoBoxOutput("dollar_value", width = 4),
              infoBoxOutput("percentage_increase", width = 4),
              infoBoxOutput("cost_reduction", width = 4)
            ),
            
            br(),
            
            div(style = "background-color: #e7f4fd; padding: 15px; border-radius: 5px; border-left: 4px solid #3c8dbc;",
              h5(icon("lightbulb"), "Why This Matters"),
              p("Job crafting interventions offer a cost-effective, bottom-up approach to improving employee performance and engagement. 
                Unlike traditional top-down job design approaches, job crafting empowers employees to proactively modify their work 
                to better align with their strengths, values, and career goals.")
            )
          )
        )
      ),
      
      # Meta-Analysis Results Tab
      tabItem(tabName = "meta",
        fluidRow(
          box(width = 8, title = "Meta-Analysis Effect Sizes", status = "primary", solidHeader = TRUE,
            plotlyOutput("meta_forest_plot", height = "400px")
          ),
          box(width = 4, title = "Effect Size Interpretations", status = "info", solidHeader = TRUE,
            h5("Cohen's Guidelines:"),
            p(strong("Small effect:"), "d = 0.20"),
            p(strong("Medium effect:"), "d = 0.50"), 
            p(strong("Large effect:"), "d = 0.80"),
            br(),
            h5("Statistical Significance:"),
            p("*** p < 0.001"),
            p("** p < 0.01"),
            p("* p < 0.05"),
            br(),
            div(style = "background-color: #f0f8ff; padding: 10px; border-radius: 3px;",
              h6("Key Finding:"),
              p("The strongest and most reliable effect was found for", 
                strong("task performance in healthcare"), 
                "with an effect size of g = 0.47.")
            )
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Detailed Meta-Analysis Results", status = "primary", solidHeader = TRUE,
            DT::dataTableOutput("meta_table")
          )
        )
      ),
      
      # Utility Calculator Tab
      tabItem(tabName = "calculator",
        fluidRow(
          box(width = 4, title = "Input Parameters", status = "primary", solidHeader = TRUE,
            h5("Employee & Salary Information"),
            numericInput("num_employees", "Number of Employees:", value = 100, min = 1, max = 10000),
            numericInput("annual_salary", "Annual Salary per Employee ($):", value = 50000, min = 20000, max = 200000, step = 1000),
            
            br(),
            h5("Intervention Parameters"),
            numericInput("intervention_cost", "Cost per Employee ($):", value = 40, min = 0, max = 5000),
            numericInput("intervention_duration", "Duration (Years):", value = 0.25, min = 0.1, max = 5, step = 0.25),
            
            br(),
            h5("Effect Size Selection"),
            selectInput("industry_type", "Industry/Context:", 
                       choices = c("Healthcare (High Evidence)" = 0.47,
                                 "Education (Estimated)" = 0.20,
                                 "Service Industry (Estimated)" = 0.25,
                                 "Technology (Estimated)" = 0.30,
                                 "Manufacturing (Estimated)" = 0.15,
                                 "Other/General (Meta-average)" = 0.26),
                       selected = 0.47),
            
            br(),
            h5("Advanced Parameters"),
            numericInput("sdy_percentage", "SDy (% of salary):", value = 40, min = 20, max = 60),
            
            br(),
            actionButton("calculate", "Calculate Utility", class = "btn-primary", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Utility Analysis Results", status = "success", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("gross_benefit", width = 4),
              valueBoxOutput("net_benefit", width = 4),
              valueBoxOutput("roi_ratio", width = 4)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Summary Results",
                br(),
                htmlOutput("utility_summary")
              ),
              
              tabPanel("Detailed Calculations",
                br(),
                verbatimTextOutput("detailed_calc")
              ),
              
              tabPanel("Sensitivity Analysis",
                br(),
                plotlyOutput("sensitivity_plot")
              )
            )
          )
        )
      ),
      
      # Break-Even Analysis Tab
      tabItem(tabName = "breakeven",
        fluidRow(
          box(width = 6, title = "Break-Even Analysis", status = "warning", solidHeader = TRUE,
            p("Determine the maximum cost per employee where the intervention remains profitable."),
            
            h5("Parameters:"),
            numericInput("be_salary", "Annual Salary ($):", value = 50000, min = 20000, max = 200000, step = 1000),
            numericInput("be_effect_size", "Expected Effect Size:", value = 0.47, min = 0.1, max = 1.0, step = 0.01),
            numericInput("be_duration", "Duration (Years):", value = 0.25, min = 0.1, max = 5, step = 0.25),
            numericInput("be_sdy", "SDy (% of salary):", value = 40, min = 20, max = 60),
            
            br(),
            actionButton("calc_breakeven", "Calculate Break-Even", class = "btn-warning", style = "width: 100%;")
          ),
          
          box(width = 6, title = "Break-Even Results", status = "warning", solidHeader = TRUE,
            br(),
            htmlOutput("breakeven_results"),
            br(),
            plotOutput("breakeven_plot")
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Break-Even Scenarios", status = "info", solidHeader = TRUE,
            p("Explore how break-even costs vary across different salary levels and effect sizes:"),
            plotlyOutput("breakeven_heatmap", height = "500px")
          )
        )
      ),
      
      # Industry Comparison Tab
      tabItem(tabName = "industry",
        fluidRow(
          box(width = 12, title = "Industry-Specific Effects and ROI", status = "info", solidHeader = TRUE,
            p("Compare the effectiveness and economic benefits of job crafting interventions across different industries:"),
            
            fluidRow(
              column(6,
                h5("Industry Parameters:"),
                numericInput("ind_employees", "Number of Employees:", value = 100, min = 1, max = 1000),
                numericInput("ind_cost", "Cost per Employee ($):", value = 40, min = 0, max = 1000)
              ),
              column(6,
                h5("Comparison Settings:"),
                numericInput("ind_duration", "Duration (Years):", value = 0.25, min = 0.1, max = 2, step = 0.25),
                actionButton("compare_industries", "Compare Industries", class = "btn-info", style = "width: 100%;")
              )
            )
          )
        ),
        
        fluidRow(
          box(width = 6, title = "Industry Effect Sizes", status = "primary", solidHeader = TRUE,
            plotOutput("industry_effects_plot")
          ),
          box(width = 6, title = "ROI by Industry", status = "success", solidHeader = TRUE,
            plotOutput("industry_roi_plot")
          )
        ),
        
        fluidRow(
          box(width = 12, title = "Industry Comparison Table", status = "primary", solidHeader = TRUE,
            DT::dataTableOutput("industry_table")
          )
        )
      ),
      
      # Business Case Report Tab
      tabItem(tabName = "report",
        fluidRow(
          box(width = 12, title = "Business Case Report Generator", status = "success", solidHeader = TRUE,
            p("Generate a comprehensive PDF report with your job crafting intervention analysis and business case."),
            
            fluidRow(
              column(6,
                h5("Report Parameters:"),
                textInput("org_name", "Organization Name:", value = "Your Organization"),
                selectInput("report_industry", "Industry:", 
                           choices = c("Healthcare", "Education", "Manufacturing", "Service", "Technology", "Other"),
                           selected = "Healthcare"),
                numericInput("report_employees", "Target Employees:", value = 100, min = 1, max = 10000),
                numericInput("report_salary", "Average Salary ($):", value = 50000, min = 20000, max = 200000, step = 1000)
              ),
              column(6,
                h5("Business Context:"),
                numericInput("report_cost", "Budget per Employee ($):", value = 40, min = 0, max = 5000),
                numericInput("report_duration", "Implementation Period (Years):", value = 0.25, min = 0.1, max = 5, step = 0.25),
                textAreaInput("report_context", "Additional Context:", 
                             value = "Enter any specific organizational context or goals for this intervention.",
                             rows = 3)
              )
            ),
            
            br(),
            div(style = "text-align: center;",
              downloadButton("download_report", "Generate Business Case Report", 
                           class = "btn-success btn-lg", 
                           style = "padding: 10px 30px; font-size: 16px;")
            ),
            
            br(),
            
            div(style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px;",
              h5("Report Contents:"),
              tags$ul(
                tags$li("Executive summary with key findings"),
                tags$li("Utility analysis calculations and ROI projections"),
                tags$li("Break-even analysis and risk assessment"),
                tags$li("Implementation recommendations"),
                tags$li("Meta-analysis evidence base"),
                tags$li("References and methodology")
              )
            )
          )
        )
      ),
      
      # References Tab
      tabItem(tabName = "references",
        fluidRow(
          box(width = 12, title = "References and Citations", status = "info", solidHeader = TRUE,
            
            h4("Primary Research Base"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Oprea, B. T., Barzin, L., Vîrgă, D., Iliescu, D., & Rusu, A. (2019)."),
              p(em("Effectiveness of job crafting interventions: A meta-analysis and utility analysis."), 
                strong("European Journal of Work and Organizational Psychology"), ", 29(6), 834-849."),
              p("DOI: 10.1080/1359432X.2019.1646728"),
              
              h6("Abstract:"),
              p("Job crafting (JC) is a form of bottom-up job design with a high potential for increasing work engagement and performance. 
                The authors meta-analyzed the effectiveness of interventions on increasing JC behaviours, work engagement, and job performance, 
                and estimated the economic value of JC interventions using utility analysis. Meta-analyses of 14 studies revealed significant 
                effects on overall JC (g = 0.26), work engagement (g = 0.31), and contextual performance (g = 0.39). 
                Utility analysis indicated substantial benefits regarding dollar value increases in output, percentage increase in output, 
                and reduced labour costs for healthcare professionals.")
            ),
            
            h4("Related Research"),
            h5("Job Crafting Theory and Measurement:"),
            p("• Wrzesniewski, A., & Dutton, J. E. (2001). Crafting a job: Revisioning employees as active crafters of their work. 
              Academy of Management Review, 26(2), 179-201."),
            p("• Tims, M., Bakker, A. B., & Derks, D. (2012). Development and validation of the job crafting scale. 
              Journal of Vocational Behavior, 80(1), 173-186."),
            
            h5("Utility Analysis Methodology:"),
            p("• Schmidt, F. L. (2013). A general theoretical integrative model of individual differences in training, learning, 
              performance, and behavioral outcomes. In N. Schmitt & S. Highhouse (Eds.), Handbook of psychology: Industrial and organizational psychology (pp. 84-119)."),
            p("• Cascio, W. F., & Boudreau, J. W. (2011). Investing in people: Financial impact of human resource initiatives (2nd ed.). 
              Upper Saddle River, NJ: Pearson Education."),
            
            h4("App Information"),
            div(style = "background-color: #e7f4fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
              h5("About This Tool"),
              p(strong("Job Crafting Intervention Utility Analysis Tool"), " (2025). Interactive educational application based on Oprea et al. (2019)."),
              p(strong("Version:"), " 1.0"),
              p(strong("Last Updated:"), " January 2025"),
              
              h5("Source Code & Documentation:"),
              p(strong("GitHub Repository:"), br(),
              HTML("<a href='https://github.com/chriscastille6/-utility-analysis-research' target='_blank' style='color: #666; font-style: italic;'>
              https://github.com/chriscastille6/-utility-analysis-research
              </a>")),
              
              p(strong("Citation for this Tool:"), br(),
              HTML("<em>Job Crafting Intervention Utility Analysis Tool</em> (2025). Interactive educational application based on Oprea et al. (2019).<br>
              <strong>Live App:</strong> <a href='https://christopher-m-castille.shinyapps.io/job-crafting-utility-analysis/' target='_blank'>https://christopher-m-castille.shinyapps.io/job-crafting-utility-analysis/</a><br>
              <strong>Source Code:</strong> https://github.com/chriscastille6/-utility-analysis-research")),
              
              h5("Technical Requirements:"),
              tags$ul(
                tags$li("R (version 4.0+)"),
                tags$li("Shiny framework"),
                tags$li("Modern web browser"),
                tags$li("LaTeX for PDF reports (optional)")
              ),
              
              h5("Educational Use:"),
              p("This tool is designed for educational purposes to help students, researchers, and practitioners understand 
                the economic value of job crafting interventions. It demonstrates the application of utility analysis 
                principles to evidence-based organizational interventions."),
              
              h5("AI Development Assistance:"),
              p("This application was developed with assistance from:"),
              tags$ul(
                tags$li(strong("Claude (Anthropic):"), " AI assistance for code development, analysis interpretation, and educational content creation"),
                tags$li(strong("Cursor:"), " AI-powered development environment for coding and debugging")
              ),
              p("The utility analysis methodology and all research findings are based on peer-reviewed academic research, 
                primarily Oprea et al. (2019). AI assistance was used solely for tool development and educational presentation of the research.")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load data
  jc_data <- load_jc_data()
  
  # Overview tab outputs
  output$overall_effect <- renderValueBox({
    valueBox(
      value = "g = 0.26",
      subtitle = "Overall Job Crafting Effect",
      icon = icon("arrow-up"),
      color = "blue"
    )
  })
  
  output$engagement_effect <- renderValueBox({
    valueBox(
      value = "g = 0.31", 
      subtitle = "Work Engagement Effect",
      icon = icon("heart"),
      color = "green"
    )
  })
  
  output$performance_effect <- renderValueBox({
    valueBox(
      value = "g = 0.47",
      subtitle = "Task Performance (Healthcare)",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  output$dollar_value <- renderInfoBox({
    infoBox(
      title = "Annual Value Increase",
      value = "$9,400",
      subtitle = "per employee",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$percentage_increase <- renderInfoBox({
    infoBox(
      title = "Output Increase",
      value = "14.1%",
      subtitle = "productivity gain",
      icon = icon("percentage"),
      color = "blue"
    )
  })
  
  output$cost_reduction <- renderInfoBox({
    infoBox(
      title = "Labor Cost Reduction",
      value = "12.4%",
      subtitle = "equivalent savings",
      icon = icon("piggy-bank"),
      color = "orange"
    )
  })
  
  # Meta-analysis forest plot
  output$meta_forest_plot <- renderPlotly({
    data <- jc_data$meta_effects
    
    p <- ggplot(data, aes(x = effect_size, y = reorder(outcome, effect_size))) +
      geom_point(size = 3, color = "#3c8dbc") +
      geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "#3c8dbc") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(xintercept = c(0.2, 0.5, 0.8), linetype = "dotted", color = "gray", alpha = 0.7) +
      labs(title = "Effect Sizes from Job Crafting Intervention Meta-Analysis",
           x = "Effect Size (Hedges' g)", y = "Outcome Variable") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Meta-analysis table
  output$meta_table <- DT::renderDataTable({
    jc_data$meta_effects %>%
      select(Outcome = outcome, `Effect Size` = effect_size, `95% CI Lower` = ci_lower, 
             `95% CI Upper` = ci_upper, `Studies (k)` = k_studies, `Sample Size` = sample_size, 
             Significance = significance, Interpretation = interpretation)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Utility calculations
  utility_results <- reactive({
    input$calculate
    
    isolate({
      n_employees <- input$num_employees
      salary <- input$annual_salary
      cost <- input$intervention_cost
      duration <- input$intervention_duration
      effect_size <- as.numeric(input$industry_type)
      sdy_pct <- input$sdy_percentage / 100
      
      # Calculate SDy in dollars
      sdy <- salary * sdy_pct
      
      # Calculate gross benefit per employee
      gross_benefit_per_employee <- effect_size * sdy * duration
      
      # Calculate total costs and benefits
      total_cost <- n_employees * cost
      total_gross_benefit <- n_employees * gross_benefit_per_employee
      total_net_benefit <- total_gross_benefit - total_cost
      
      # Calculate ROI
      roi_ratio <- total_gross_benefit / total_cost
      roi_percentage <- ((total_gross_benefit - total_cost) / total_cost) * 100
      
      # Calculate percentage increase in output
      sdp <- 0.30  # 30% as per Oprea et al.
      percentage_increase <- effect_size * sdp * 100
      
      # Labor cost reduction equivalent
      labor_cost_reduction <- percentage_increase * 0.877  # Conversion factor from paper
      
      list(
        n_employees = n_employees,
        salary = salary,
        cost = cost,
        duration = duration,
        effect_size = effect_size,
        sdy = sdy,
        gross_benefit_per_employee = gross_benefit_per_employee,
        total_cost = total_cost,
        total_gross_benefit = total_gross_benefit,
        total_net_benefit = total_net_benefit,
        roi_ratio = roi_ratio,
        roi_percentage = roi_percentage,
        percentage_increase = percentage_increase,
        labor_cost_reduction = labor_cost_reduction
      )
    })
  })
  
  # Utility result boxes
  output$gross_benefit <- renderValueBox({
    results <- utility_results()
    valueBox(
      value = paste0("$", format(round(results$total_gross_benefit), big.mark = ",")),
      subtitle = "Total Gross Benefit",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$net_benefit <- renderValueBox({
    results <- utility_results()
    color <- if(results$total_net_benefit > 0) "blue" else "red"
    valueBox(
      value = paste0("$", format(round(results$total_net_benefit), big.mark = ",")),
      subtitle = "Total Net Benefit",
      icon = icon("calculator"),
      color = color
    )
  })
  
  output$roi_ratio <- renderValueBox({
    results <- utility_results()
    valueBox(
      value = paste0(round(results$roi_ratio, 2), ":1"),
      subtitle = "Return on Investment",
      icon = icon("percentage"),
      color = "orange"
    )
  })
  
  # Utility summary
  output$utility_summary <- renderUI({
    results <- utility_results()
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h4>Utility Analysis Summary</h4>",
      "<p><strong>Scenario:</strong> ", results$n_employees, " employees, $", format(results$salary, big.mark = ","), " average salary</p>",
      "<p><strong>Effect Size:</strong> ", results$effect_size, " (", names(which(c(0.47, 0.20, 0.25, 0.30, 0.15, 0.26) == results$effect_size)), ")</p>",
      "<p><strong>Duration:</strong> ", results$duration, " years</p>",
      "<hr>",
      "<h5>Financial Results:</h5>",
      "<p><strong>Gross Benefit per Employee:</strong> $", format(round(results$gross_benefit_per_employee), big.mark = ","), "</p>",
      "<p><strong>Total Investment:</strong> $", format(results$total_cost, big.mark = ","), "</p>",
      "<p><strong>Total Gross Return:</strong> $", format(round(results$total_gross_benefit), big.mark = ","), "</p>",
      "<p><strong>Net Benefit:</strong> $", format(round(results$total_net_benefit), big.mark = ","), "</p>",
      "<p><strong>ROI:</strong> ", round(results$roi_percentage, 1), "% (", round(results$roi_ratio, 2), ":1 ratio)</p>",
      "<hr>",
      "<h5>Performance Impact:</h5>",
      "<p><strong>Productivity Increase:</strong> ", round(results$percentage_increase, 1), "%</p>",
      "<p><strong>Equivalent Labor Cost Reduction:</strong> ", round(results$labor_cost_reduction, 1), "%</p>",
      "</div>"
    ))
  })
  
  # Detailed calculations
  output$detailed_calc <- renderText({
    results <- utility_results()
    
    paste0(
      "Utility Analysis Calculations (Based on Oprea et al., 2019)\n",
      "=" %R% 60, "\n\n",
      "INPUT PARAMETERS:\n",
      "- Number of employees (N): ", results$n_employees, "\n",
      "- Annual salary per employee: $", format(results$salary, big.mark = ","), "\n",
      "- Effect size (d): ", results$effect_size, "\n",
      "- SDy percentage: ", input$sdy_percentage, "%\n",
      "- Intervention cost per employee (C): $", results$cost, "\n",
      "- Duration (T): ", results$duration, " years\n\n",
      
      "CALCULATIONS:\n",
      "1. SDy (Standard deviation of performance in dollars):\n",
      "   SDy = Salary × SDy% = $", format(results$salary, big.mark = ","), " × ", input$sdy_percentage, "% = $", format(round(results$sdy), big.mark = ","), "\n\n",
      
      "2. Gross benefit per employee:\n",
      "   Benefit = d × SDy × T = ", results$effect_size, " × $", format(round(results$sdy), big.mark = ","), " × ", results$duration, " = $", format(round(results$gross_benefit_per_employee), big.mark = ","), "\n\n",
      
      "3. Total costs and benefits:\n",
      "   Total Cost = N × C = ", results$n_employees, " × $", results$cost, " = $", format(results$total_cost, big.mark = ","), "\n",
      "   Total Gross Benefit = N × Benefit = ", results$n_employees, " × $", format(round(results$gross_benefit_per_employee), big.mark = ","), " = $", format(round(results$total_gross_benefit), big.mark = ","), "\n",
      "   Net Benefit = Gross Benefit - Cost = $", format(round(results$total_net_benefit), big.mark = ","), "\n\n",
      
      "4. Return on Investment:\n",
      "   ROI Ratio = Gross Benefit ÷ Cost = ", round(results$roi_ratio, 2), ":1\n",
      "   ROI Percentage = (Net Benefit ÷ Cost) × 100 = ", round(results$roi_percentage, 1), "%\n\n",
      
      "5. Performance metrics:\n",
      "   Productivity increase = d × SDp × 100 = ", results$effect_size, " × 30% × 100 = ", round(results$percentage_increase, 1), "%\n",
      "   Labor cost reduction equivalent = ", round(results$labor_cost_reduction, 1), "%"
    )
  })
  
  # Sensitivity analysis plot
  output$sensitivity_plot <- renderPlotly({
    results <- utility_results()
    
    # Create sensitivity data for different effect sizes
    effect_sizes <- seq(0.1, 0.8, 0.05)
    roi_values <- sapply(effect_sizes, function(es) {
      gross <- results$n_employees * es * results$sdy * results$duration
      (gross - results$total_cost) / results$total_cost * 100
    })
    
    sens_data <- data.frame(
      effect_size = effect_sizes,
      roi_percentage = roi_values
    )
    
    p <- ggplot(sens_data, aes(x = effect_size, y = roi_percentage)) +
      geom_line(color = "#3c8dbc", linewidth = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(xintercept = results$effect_size, linetype = "dotted", color = "orange", linewidth = 1) +
      labs(title = "ROI Sensitivity to Effect Size",
           x = "Effect Size", y = "ROI (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Break-even analysis
  breakeven_results <- reactive({
    input$calc_breakeven
    
    isolate({
      salary <- input$be_salary
      effect_size <- input$be_effect_size
      duration <- input$be_duration
      sdy_pct <- input$be_sdy / 100
      
      sdy <- salary * sdy_pct
      max_cost <- effect_size * sdy * duration
      
      list(
        salary = salary,
        effect_size = effect_size,
        duration = duration,
        sdy = sdy,
        max_cost = max_cost
      )
    })
  })
  
  output$breakeven_results <- renderUI({
    results <- breakeven_results()
    
    HTML(paste0(
      "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;'>",
      "<h4>Break-Even Analysis Results</h4>",
      "<p><strong>Maximum Cost per Employee:</strong> $", format(round(results$max_cost), big.mark = ","), "</p>",
      "<p><strong>Annual Salary:</strong> $", format(results$salary, big.mark = ","), "</p>",
      "<p><strong>Effect Size:</strong> ", results$effect_size, "</p>",
      "<p><strong>Duration:</strong> ", results$duration, " years</p>",
      "<hr>",
      "<p><em>Any intervention cost below $", format(round(results$max_cost), big.mark = ","), " per employee will be profitable.</em></p>",
      "</div>"
    ))
  })
  
  # Break-even plot
  output$breakeven_plot <- renderPlot({
    results <- breakeven_results()
    
    costs <- seq(0, results$max_cost * 1.5, length.out = 100)
    net_benefits <- (results$effect_size * results$sdy * results$duration) - costs
    
    plot_data <- data.frame(cost = costs, net_benefit = net_benefits)
    
    ggplot(plot_data, aes(x = cost, y = net_benefit)) +
      geom_line(color = "#3c8dbc", linewidth = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(xintercept = results$max_cost, linetype = "dotted", color = "orange", linewidth = 1) +
      labs(title = "Break-Even Analysis", x = "Cost per Employee ($)", y = "Net Benefit per Employee ($)") +
      theme_minimal() +
      annotate("text", x = results$max_cost, y = max(net_benefits) * 0.8, 
               label = paste("Break-even:\n$", round(results$max_cost)), 
               hjust = -0.1, color = "orange")
  })
  
  # Break-even heatmap
  output$breakeven_heatmap <- renderPlotly({
    salaries <- seq(30000, 100000, 5000)
    effect_sizes <- seq(0.1, 0.6, 0.05)
    
    heatmap_data <- expand.grid(salary = salaries, effect_size = effect_sizes)
    heatmap_data$breakeven_cost <- heatmap_data$effect_size * (heatmap_data$salary * 0.4) * 0.25
    
    p <- ggplot(heatmap_data, aes(x = salary, y = effect_size, fill = breakeven_cost)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", 
                          midpoint = median(heatmap_data$breakeven_cost),
                          name = "Break-even\nCost ($)") +
      labs(title = "Break-Even Cost by Salary and Effect Size",
           x = "Annual Salary ($)", y = "Effect Size") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Industry comparison
  industry_comparison <- reactive({
    input$compare_industries
    
    isolate({
      n_emp <- input$ind_employees
      cost <- input$ind_cost
      duration <- input$ind_duration
      
      results <- jc_data$industry_effects %>%
        mutate(
          salary = case_when(
            industry == "Healthcare" ~ 65000,
            industry == "Education" ~ 45000,
            industry == "Technology" ~ 80000,
            industry == "Manufacturing" ~ 55000,
            industry == "Service" ~ 40000,
            industry == "Other" ~ 50000
          ),
          sdy = salary * 0.4,
          gross_benefit = task_performance_effect * sdy * duration,
          total_cost = cost,
          net_benefit = gross_benefit - total_cost,
          roi = (gross_benefit / total_cost - 1) * 100
        )
      
      results
    })
  })
  
  output$industry_effects_plot <- renderPlot({
    data <- industry_comparison()
    
    ggplot(data, aes(x = reorder(industry, task_performance_effect), y = task_performance_effect, fill = reliability)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("High" = "#2ecc71", "Medium" = "#f39c12", "Low" = "#e74c3c")) +
      labs(title = "Task Performance Effect Sizes by Industry", 
           x = "Industry", y = "Effect Size", fill = "Evidence") +
      theme_minimal()
  })
  
  output$industry_roi_plot <- renderPlot({
    data <- industry_comparison()
    
    ggplot(data, aes(x = reorder(industry, roi), y = roi, fill = industry)) +
      geom_col() +
      coord_flip() +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      labs(title = "ROI by Industry", x = "Industry", y = "ROI (%)") +
      theme_minimal() +
      guides(fill = "none")
  })
  
  output$industry_table <- DT::renderDataTable({
    industry_comparison() %>%
      select(Industry = industry, `Effect Size` = task_performance_effect,
             `Avg Salary` = salary, `Gross Benefit` = gross_benefit,
             `Net Benefit` = net_benefit, `ROI (%)` = roi, 
             `Evidence Base` = evidence_base) %>%
      mutate(
        `Avg Salary` = paste0("$", format(round(`Avg Salary`), big.mark = ",")),
        `Gross Benefit` = paste0("$", format(round(`Gross Benefit`), big.mark = ",")),
        `Net Benefit` = paste0("$", format(round(`Net Benefit`), big.mark = ",")),
        `ROI (%)` = round(`ROI (%)`, 1)
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("job_crafting_business_case_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Capture input values
      org_name_val <- if(is.null(input$org_name)) "Your Organization" else input$org_name
      industry_val <- if(is.null(input$report_industry)) "Healthcare" else input$report_industry
      employees_val <- if(is.null(input$report_employees)) 100 else input$report_employees
      salary_val <- if(is.null(input$report_salary)) 50000 else input$report_salary
      cost_val <- if(is.null(input$report_cost)) 40 else input$report_cost
      duration_val <- if(is.null(input$report_duration)) 0.25 else input$report_duration
      context_val <- if(is.null(input$report_context)) "General organizational improvement initiative" else input$report_context
      
      # Get industry-specific effect size
      effect_size_val <- case_when(
        industry_val == "Healthcare" ~ 0.47,
        industry_val == "Education" ~ 0.20,
        industry_val == "Technology" ~ 0.30,
        industry_val == "Manufacturing" ~ 0.15,
        industry_val == "Service" ~ 0.25,
        TRUE ~ 0.26
      )
      
      # Calculate results for report
      sdy_val <- salary_val * 0.4
      gross_benefit_val <- effect_size_val * sdy_val * duration_val
      total_cost_val <- employees_val * cost_val
      total_gross_val <- employees_val * gross_benefit_val
      net_benefit_val <- total_gross_val - total_cost_val
      roi_val <- (total_gross_val / total_cost_val - 1) * 100
      
      # Create R Markdown content
      rmd_content <- paste0('
---
title: "Job Crafting Intervention Business Case"
subtitle: "', org_name_val, '"
date: "', format(Sys.Date(), "%B %d, %Y"), '"
output: 
  pdf_document:
    latex_engine: xelatex
    toc: true
    number_sections: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(ggplot2)
library(scales)

# Global variables for calculations
org_name <<- "', org_name_val, '"
industry <<- "', industry_val, '"
employees <<- ', employees_val, '
salary <<- ', salary_val, '
cost <<- ', cost_val, '
duration <<- ', duration_val, '
effect_size <<- ', effect_size_val, '
sdy <<- ', sdy_val, '
gross_benefit <<- ', gross_benefit_val, '
total_cost <<- ', total_cost_val, '
total_gross <<- ', total_gross_val, '
net_benefit <<- ', net_benefit_val, '
roi <<- ', roi_val, '
context_text <<- "', gsub('"', '\\\\"', context_val), '"
```

# Executive Summary

This business case analysis evaluates the potential return on investment of implementing a job crafting intervention for **', org_name_val, '** in the **', industry_val, '** industry. Based on meta-analytic evidence from Oprea et al. (2019), job crafting interventions show significant positive effects on employee performance and work engagement.

## Key Findings

```{r key_findings}
if(!exists("net_benefit") || is.null(net_benefit)) {
  net_benefit <<- total_gross - total_cost
  roi <<- (total_gross / total_cost - 1) * 100
}

cat("• Investment: $", format(total_cost, big.mark = ","), " (", employees, " employees × $", cost, " per employee)\\n")
cat("• Expected Return: $", format(round(total_gross), big.mark = ","), " over ", duration, " years\\n")
cat("• Net Benefit: $", format(round(net_benefit), big.mark = ","), "\\n")
cat("• Return on Investment: ", round(roi, 1), "%\\n")
cat("• Break-even point: $", format(round(gross_benefit), big.mark = ","), " per employee\\n")
```

**Recommendation:** The analysis supports proceeding with the job crafting intervention, projecting a positive ROI of `r round(roi, 1)`%.

# Intervention Overview

## What is Job Crafting?

Job crafting represents proactive changes made by employees regarding their job demands and job resources. Research identifies three main types:

- **Structural crafting:** Changing the number, scope, or type of job tasks
- **Social crafting:** Changing the quality and amount of interactions with others  
- **Cognitive crafting:** Changing how one perceives the job and its purpose

## Evidence Base

This analysis is based on the comprehensive meta-analysis by Oprea et al. (2019), which examined 14 studies with 1,204 participants. The research found significant positive effects of job crafting interventions on:

- Overall job crafting behaviors (g = 0.26)
- Work engagement (g = 0.31)
- Task performance (g = 0.47 for healthcare; g = `r effect_size` for `r industry`)
- Contextual performance (g = 0.39)

# Financial Analysis

## Utility Analysis Methodology

This analysis follows the utility analysis framework established by Schmidt (2013) and Cascio & Boudreau (2011), using the formula:

**Dollar Value = (T)(N)(d)(SDy) - (N)(C)**

Where:
- T = Duration (`r duration` years)
- N = Number of employees (`r employees`)
- d = Effect size (`r effect_size`)
- SDy = Standard deviation of performance in dollars ($`r format(round(sdy), big.mark = ",")`)
- C = Cost per employee ($`r cost`)

## Financial Projections

```{r financial_analysis}
if(!exists("roi") || is.null(roi)) {
  roi <<- (total_gross / total_cost - 1) * 100
}

# Create financial summary table
financial_data <- data.frame(
  Metric = c("Total Investment", "Gross Benefits", "Net Benefits", "ROI", "Payback Period"),
  Value = c(
    paste0("$", format(total_cost, big.mark = ",")),
    paste0("$", format(round(total_gross), big.mark = ",")),
    paste0("$", format(round(net_benefit), big.mark = ",")),
    paste0(round(roi, 1), "%"),
    paste0(round(duration * 12, 1), " months")
  )
)

kable(financial_data, caption = "Financial Analysis Summary")
```

## Sensitivity Analysis

```{r sensitivity_plot, fig.height=4}
# Sensitivity analysis for different effect sizes
effect_range <- seq(0.1, 0.6, 0.05)
roi_range <- sapply(effect_range, function(es) {
  gross <- employees * es * sdy * duration
  ((gross - total_cost) / total_cost) * 100
})

plot_data <- data.frame(Effect_Size = effect_range, ROI = roi_range)

ggplot(plot_data, aes(x = Effect_Size, y = ROI)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = effect_size, linetype = "dotted", color = "orange") +
  labs(title = "ROI Sensitivity to Effect Size", 
       x = "Effect Size", y = "ROI (%)") +
  theme_minimal() +
  annotate("text", x = effect_size, y = max(roi_range) * 0.8, 
           label = paste("Current\\nEstimate\\n(", effect_size, ")"), 
           hjust = -0.1, color = "orange")
```

# Implementation Recommendations

## Intervention Design

Based on the meta-analytic findings, effective job crafting interventions should include:

1. **Training component** (2-4 hours): Education about job crafting concepts and techniques
2. **Planning sessions** (1-2 hours): Guided development of individual job crafting plans
3. **Goal setting**: Integration of both personal and organizational objectives
4. **Follow-up support**: Regular check-ins and progress monitoring

## Timeline and Costs

```{r implementation_timeline}
timeline_data <- data.frame(
  Phase = c("Planning & Design", "Training Delivery", "Implementation", "Follow-up & Evaluation"),
  Duration = c("2 weeks", "1 week", "8-12 weeks", "4 weeks"),
  Cost_per_Employee = c("$10", "$20", "$5", "$5"),
  Total_Cost = c(
    paste0("$", format(10 * employees, big.mark = ",")),
    paste0("$", format(20 * employees, big.mark = ",")),
    paste0("$", format(5 * employees, big.mark = ",")),
    paste0("$", format(5 * employees, big.mark = ","))
  )
)

kable(timeline_data, caption = "Implementation Timeline and Costs")
```

## Success Factors

Research indicates higher effectiveness when interventions:

- Include both individual and organizational goal alignment
- Provide structured planning tools and templates
- Offer ongoing managerial support
- Focus on achievable, specific crafting actions

# Risk Assessment

## Potential Challenges

1. **Employee engagement:** Not all employees may be equally motivated to participate
2. **Manager support:** Requires buy-in from supervisors and management
3. **Measurement difficulties:** Some benefits may be difficult to quantify immediately
4. **Sustainability:** Long-term effects require ongoing reinforcement

## Mitigation Strategies

- Voluntary participation with clear communication of benefits
- Manager training and involvement in the process
- Multiple outcome measures including engagement surveys
- Integration with existing performance management systems

# Conclusion and Next Steps

## Business Case Summary

The financial analysis strongly supports implementing job crafting interventions for `r org_name`. With an expected ROI of `r round(roi, 1)`% and net benefits of $`r format(round(net_benefit), big.mark = ",")`, the intervention represents a sound investment in human capital.

## Recommended Actions

1. **Approve budget allocation** of $`r format(total_cost, big.mark = ",")` for intervention implementation
2. **Identify pilot group** of `r min(employees, 50)` employees for initial rollout
3. **Select training provider** with demonstrated expertise in job crafting interventions
4. **Establish measurement system** to track outcomes and validate projections
5. **Plan organization-wide rollout** based on pilot results

## Context-Specific Considerations

`r context_text`

# References and Methodology

## Primary Research Source

**Oprea, B. T., Barzin, L., Vîrgă, D., Iliescu, D., & Rusu, A. (2019).** Effectiveness of job crafting interventions: A meta-analysis and utility analysis. *European Journal of Work and Organizational Psychology*, 29(6), 834-849. DOI: 10.1080/1359432X.2019.1646728

## Utility Analysis Framework

- **Schmidt, F. L. (2013).** A general theoretical integrative model of individual differences in training, learning, performance, and behavioral outcomes.
- **Cascio, W. F., & Boudreau, J. W. (2011).** Investing in people: Financial impact of human resource initiatives (2nd ed.).

## Tool Information

This business case was generated using the **Job Crafting Intervention Utility Analysis Tool** (2025), an interactive application designed for evidence-based HR decision making.

- **Source Code:** https://github.com/chriscastille6/-utility-analysis-research
- **Methodology:** Based on peer-reviewed meta-analytic evidence
- **Last Updated:** January 2025

---

*This analysis is provided for educational and planning purposes. Actual results may vary based on organizational context, implementation quality, and external factors.*
      ')
      
      # Write R Markdown file
      rmd_file <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, rmd_file)
      
      # Render to PDF
      rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
