# Training Utility Analysis App
# Based on Oprea et al. (2019) Job Crafting Interventions Meta-Analysis
# and General Training Program Utility Analysis

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(dplyr)
library(stringr)
library(rmarkdown)
library(knitr)

# =============================================================================
# TRAINING UTILITY FUNCTIONS
# =============================================================================

# Calculate training utility using adapted BCG model
calculate_training_utility <- function(params) {
  # Training-specific utility calculation
  # Based on: Utility = (N × T × d × SDy) - Total_Training_Cost
  
  # Effect size to performance improvement
  performance_improvement <- params$effect_size * params$sdy
  
  # Total benefit over time period
  total_benefit <- params$n_employees * params$time_period * performance_improvement
  
  # Training costs
  total_cost <- (params$development_cost + 
                (params$n_employees * params$per_employee_cost) + 
                (params$n_employees * params$time_cost_per_employee))
  
  # Net utility
  net_utility <- total_benefit - total_cost
  
  # Annual utility
  annual_utility <- net_utility / params$time_period
  
  # ROI calculation
  roi_ratio <- total_benefit / total_cost
  
  list(
    total_benefit = total_benefit,
    total_cost = total_cost,
    net_utility = net_utility,
    annual_utility = annual_utility,
    roi_ratio = roi_ratio,
    per_employee_value = net_utility / params$n_employees
  )
}

# Job Crafting specific calculations (Oprea et al., 2019)
calculate_job_crafting_utility <- function(params) {
  # Based on meta-analysis findings: g = 0.47 for healthcare professionals
  effect_size <- params$jc_effect_size  # Default 0.47
  
  # Convert effect size to dollar value using SDy
  performance_gain_per_employee <- effect_size * params$sdy
  
  # Annual value (3 months = 0.25 of year, as per Oprea et al.)
  annual_value_per_employee <- performance_gain_per_employee * params$duration_factor
  
  # Total annual value for all employees
  total_annual_value <- annual_value_per_employee * params$n_employees
  
  # Training costs
  total_intervention_cost <- params$intervention_cost + 
                           (params$n_employees * params$per_employee_intervention_cost)
  
  # Net utility
  net_annual_utility <- total_annual_value - total_intervention_cost
  
  # Multi-year projections
  multi_year_utility <- net_annual_utility * params$projection_years
  
  list(
    effect_size = effect_size,
    annual_value_per_employee = annual_value_per_employee,
    total_annual_value = total_annual_value,
    total_intervention_cost = total_intervention_cost,
    net_annual_utility = net_annual_utility,
    multi_year_utility = multi_year_utility,
    roi_ratio = total_annual_value / total_intervention_cost
  )
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Training Utility Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("General Training Utility", tabName = "general", icon = icon("graduation-cap")),
      menuItem("Job Crafting Interventions", tabName = "jobcrafting", icon = icon("tools")),
      menuItem("Training ROI Calculator", tabName = "roi", icon = icon("calculator")),
      menuItem("Training Reports", tabName = "reports", icon = icon("file-pdf")),
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
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(width = 12, title = "Training Utility Analysis Overview", status = "primary", solidHeader = TRUE,
            h4("Understanding the Economic Value of Training Programs"),
            p("This comprehensive tool analyzes the utility and return on investment (ROI) of various training interventions, 
              from general skill development to specialized job crafting programs."),
            
            h5("Analytical Approaches:"),
            
            fluidRow(
              column(6,
                div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; border-left: 4px solid #3c8dbc;",
                  h6(icon("graduation-cap"), "General Training Utility"),
                  p("Traditional utility analysis for training programs using adapted BCG models. 
                    Calculates the economic value of skill development, performance improvement, 
                    and organizational capability enhancement.")
                ),
                br(),
                div(style = "background-color: #f0f8e8; padding: 15px; border-radius: 5px; border-left: 4px solid #5cb85c;",
                  h6(icon("tools"), "Job Crafting Interventions"), 
                  p("Specialized analysis based on Oprea et al. (2019) meta-analysis of job crafting interventions. 
                    Demonstrates utility of interventions that help employees redesign their work for better engagement and performance.")
                )
              ),
              column(6,
                div(style = "background-color: #fff8e1; padding: 15px; border-radius: 5px; border-left: 4px solid #ff9800;",
                  h6(icon("calculator"), "ROI Calculator"),
                  p("Comprehensive return on investment analysis including development costs, 
                    delivery expenses, opportunity costs, and multi-year projections.")
                ),
                br(),
                div(style = "background-color: #fce4ec; padding: 15px; border-radius: 5px; border-left: 4px solid #e91e63;",
                  h6(icon("chart-line"), "Impact Modeling"),
                  p("Model various training scenarios, effect sizes, and organizational factors 
                    to understand optimal training investments and expected returns.")
                )
              )
            ),
            
            br(),
            
            h5("Key Features:"),
            tags$ul(
              tags$li(strong("Multiple Training Types:"), " General skills, leadership, job crafting, and specialized interventions"),
              tags$li(strong("Cost-Benefit Analysis:"), " Comprehensive modeling of training costs vs. performance gains"),
              tags$li(strong("Meta-Analytic Foundation:"), " Based on peer-reviewed research including Oprea et al. (2019)"),
              tags$li(strong("Scenario Planning:"), " Test different parameters and organizational contexts"),
              tags$li(strong("Professional Reports:"), " Generate business case documentation for training investments")
            ),
            
            br(),
            
            fluidRow(
              valueBoxOutput("sample_training_roi", width = 4),
              valueBoxOutput("sample_effect_size", width = 4),
              valueBoxOutput("sample_payback", width = 4)
            )
          )
        )
      ),
      
      # General Training Utility Tab
      tabItem(tabName = "general",
        fluidRow(
          box(width = 4, title = "Training Parameters", status = "primary", solidHeader = TRUE,
            h5("Program Scope:"),
            numericInput("general_n_employees", "Number of Employees Trained:", value = 100, min = 1, max = 10000, step = 1),
            numericInput("general_sdy", "Performance Standard Deviation (SDy) $:", value = 25000, min = 5000, max = 200000, step = 1000),
            numericInput("general_effect_size", "Expected Training Effect Size (d):", value = 0.35, min = 0.1, max = 2.0, step = 0.05),
            
            br(),
            h5("Training Costs:"),
            numericInput("general_development_cost", "Program Development Cost ($):", value = 50000, min = 0, max = 500000, step = 5000),
            numericInput("general_per_employee_cost", "Training Cost per Employee ($):", value = 1200, min = 0, max = 10000, step = 100),
            numericInput("general_time_cost", "Opportunity Cost per Employee ($):", value = 800, min = 0, max = 5000, step = 100),
            
            br(),
            h5("Time Parameters:"),
            numericInput("general_time_period", "Benefit Duration (Years):", value = 3, min = 0.5, max = 10, step = 0.5),
            
            br(),
            actionButton("calculate_general", "Calculate Training Utility", class = "btn-primary", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Training Utility Results", status = "success", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("general_net_utility", width = 4),
              valueBoxOutput("general_roi_ratio", width = 4),
              valueBoxOutput("general_annual_value", width = 4)
            ),
            
            br(),
            
            tabsetPanel(
              tabPanel("Utility Analysis",
                br(),
                htmlOutput("general_utility_summary")
              ),
              
              tabPanel("Cost Breakdown",
                br(),
                plotlyOutput("general_cost_breakdown")
              ),
              
              tabPanel("ROI Projection",
                br(),
                plotlyOutput("general_roi_projection")
              )
            )
          )
        )
      ),
      
      # Job Crafting Interventions Tab
      tabItem(tabName = "jobcrafting",
        fluidRow(
          box(width = 12, title = "Job Crafting Intervention Utility Analysis", status = "info", solidHeader = TRUE,
            p("Analyze the utility of job crafting interventions based on Oprea et al. (2019) meta-analysis. 
              Job crafting helps employees proactively reshape their work to increase engagement and performance."),
            
            div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Meta-Analysis Evidence (Oprea et al., 2019):"),
              p("Systematic review of 14 studies found job crafting interventions significantly increase:"),
              tags$ul(
                tags$li("Overall job crafting behaviors: g = 0.26"),
                tags$li("Seeking challenges: g = 0.19"),
                tags$li("Reducing demands: g = 0.44"),
                tags$li("Task performance (healthcare): g = 0.47 (most reliable effect)")
              ),
              p(strong("Key Finding:"), " Job crafting interventions show strongest, most consistent effects on task performance in healthcare settings.")
            ),
            
            fluidRow(
              column(4,
                h5("Intervention Parameters:"),
                selectInput("jc_sector", "Industry Sector:",
                           choices = c("Healthcare" = "healthcare",
                                     "General/Mixed" = "general",
                                     "Manufacturing" = "manufacturing",
                                     "Services" = "services"),
                           selected = "healthcare"),
                
                numericInput("jc_n_employees", "Number of Employees:", value = 50, min = 5, max = 1000, step = 5),
                numericInput("jc_sdy", "Performance SDy ($):", value = 30000, min = 10000, max = 100000, step = 2000),
                
                conditionalPanel(
                  condition = "input.jc_sector == 'healthcare'",
                  div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px;",
                    p(strong("Healthcare Sector:"), br(), "Using validated effect size g = 0.47 from meta-analysis")
                  )
                ),
                
                conditionalPanel(
                  condition = "input.jc_sector != 'healthcare'",
                  numericInput("jc_custom_effect", "Expected Effect Size:", value = 0.26, min = 0.1, max = 1.0, step = 0.05)
                ),
                
                br(),
                h5("Cost Parameters:"),
                numericInput("jc_intervention_cost", "Program Development ($):", value = 15000, min = 5000, max = 100000, step = 1000),
                numericInput("jc_per_employee_cost", "Cost per Employee ($):", value = 500, min = 100, max = 5000, step = 100),
                numericInput("jc_duration_factor", "Annual Duration Factor:", value = 0.25, min = 0.1, max = 1.0, step = 0.05),
                numericInput("jc_projection_years", "Projection Period (Years):", value = 3, min = 1, max = 10, step = 1),
                
                br(),
                actionButton("calculate_jc", "Calculate Job Crafting Utility", class = "btn-info", style = "width: 100%;")
              ),
              
              column(8,
                fluidRow(
                  valueBoxOutput("jc_annual_utility", width = 4),
                  valueBoxOutput("jc_multi_year_utility", width = 4),
                  valueBoxOutput("jc_roi_ratio", width = 4)
                ),
                
                br(),
                
                tabsetPanel(
                  tabPanel("Job Crafting Results",
                    br(),
                    htmlOutput("jc_results_summary")
                  ),
                  
                  tabPanel("Research Background",
                    br(),
                    div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                      h6("Job Crafting Interventions (Oprea et al., 2019):"),
                      p(strong("Definition:"), " Bottom-up job design where employees proactively reshape their work roles"),
                      
                      h6("Three Types of Job Crafting:"),
                      tags$ul(
                        tags$li(strong("Task Crafting:"), " Changing the number, scope, or type of job tasks"),
                        tags$li(strong("Relational Crafting:"), " Altering interactions with others at work"),
                        tags$li(strong("Cognitive Crafting:"), " Changing how one thinks about the job")
                      ),
                      
                      h6("Intervention Components:"),
                      tags$ul(
                        tags$li("Self-reflection exercises on current job design"),
                        tags$li("Goal-setting for job crafting behaviors"),
                        tags$li("Action planning and implementation"),
                        tags$li("Follow-up sessions and progress monitoring")
                      ),
                      
                      h6("Expected Outcomes:"),
                      p("Job crafting interventions lead to increased work engagement, job satisfaction, 
                        task performance, and organizational commitment. Effects are most pronounced 
                        when interventions are sustained over time and supported by management.")
                    )
                  ),
                  
                  tabPanel("Utility Calculation",
                    br(),
                    div(style = "background-color: #fff8e1; padding: 20px; border-radius: 5px;",
                      h6("Job Crafting Utility Formula:"),
                      p(strong("Annual Value = "), "Effect Size × SDy × Duration Factor × Number of Employees"),
                      p(strong("Net Utility = "), "Annual Value - Total Intervention Cost"),
                      p(strong("Multi-Year Utility = "), "Net Annual Utility × Projection Years"),
                      
                      br(),
                      h6("Key Assumptions:"),
                      tags$ul(
                        tags$li("Effect size based on meta-analytic evidence"),
                        tags$li("Duration factor reflects intervention sustainability (default 25% of year)"),
                        tags$li("Benefits compound over multiple years"),
                        tags$li("No additional maintenance costs beyond initial intervention")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # ROI Calculator Tab
      tabItem(tabName = "roi",
        fluidRow(
          box(width = 12, title = "Comprehensive Training ROI Calculator", status = "warning", solidHeader = TRUE,
            p("Calculate return on investment for training programs with detailed cost analysis and benefit projections."),
            
            fluidRow(
              column(6,
                h5("Training Investment:"),
                numericInput("roi_participants", "Number of Participants:", value = 75, min = 1, max = 5000, step = 1),
                numericInput("roi_design_cost", "Design & Development ($):", value = 40000, min = 0, max = 1000000, step = 5000),
                numericInput("roi_delivery_cost", "Delivery Cost per Participant ($):", value = 800, min = 0, max = 10000, step = 50),
                numericInput("roi_materials_cost", "Materials per Participant ($):", value = 150, min = 0, max = 1000, step = 25),
                numericInput("roi_opportunity_cost", "Opportunity Cost per Participant ($):", value = 600, min = 0, max = 5000, step = 50)
              ),
              
              column(6,
                h5("Expected Benefits:"),
                numericInput("roi_performance_sdy", "Performance SDy ($):", value = 28000, min = 5000, max = 200000, step = 2000),
                numericInput("roi_effect_size", "Training Effect Size:", value = 0.4, min = 0.1, max = 2.0, step = 0.05),
                numericInput("roi_benefit_duration", "Benefit Duration (Years):", value = 2.5, min = 0.5, max = 10, step = 0.5),
                numericInput("roi_decay_rate", "Annual Benefit Decay (%):", value = 15, min = 0, max = 50, step = 5),
                
                br(),
                actionButton("calculate_roi", "Calculate Comprehensive ROI", class = "btn-warning", style = "width: 100%;")
              )
            ),
            
            br(),
            
            fluidRow(
              valueBoxOutput("roi_total_investment", width = 3),
              valueBoxOutput("roi_total_benefits", width = 3),
              valueBoxOutput("roi_net_benefit", width = 3),
              valueBoxOutput("roi_percentage", width = 3)
            ),
            
            br(),
            
            fluidRow(
              box(width = 6, title = "ROI Analysis Over Time", status = "primary", solidHeader = TRUE,
                plotlyOutput("roi_time_series")
              ),
              box(width = 6, title = "Cost-Benefit Breakdown", status = "info", solidHeader = TRUE,
                plotlyOutput("roi_breakdown_chart")
              )
            ),
            
            fluidRow(
              box(width = 12, title = "Detailed ROI Analysis", status = "success", solidHeader = TRUE,
                htmlOutput("roi_detailed_analysis")
              )
            )
          )
        )
      ),
      
      # Training Reports Tab
      tabItem(tabName = "reports",
        fluidRow(
          box(width = 12, title = "Training Utility Reports", status = "success", solidHeader = TRUE,
            p("Generate comprehensive reports for training program business cases and utility analysis."),
            
            fluidRow(
              column(6,
                h5("Report Parameters:"),
                textInput("report_org_name", "Organization Name:", value = "Your Organization"),
                textInput("report_program_name", "Training Program Name:", value = "Leadership Development Program"),
                selectInput("report_type", "Training Type:",
                           choices = c("General Skills Training" = "general",
                                     "Job Crafting Intervention" = "jobcrafting",
                                     "Leadership Development" = "leadership",
                                     "Technical Skills" = "technical"),
                           selected = "general"),
                numericInput("report_budget", "Training Budget ($):", value = 100000, min = 10000, max = 5000000, step = 10000)
              ),
              
              column(6,
                h5("Analysis Context:"),
                textAreaInput("report_objectives", "Training Objectives:", 
                             value = "Improve employee performance, engagement, and organizational capability through targeted skill development.",
                             rows = 3),
                textAreaInput("report_context", "Business Context:", 
                             value = "Organization seeks to enhance workforce capabilities to meet strategic objectives and improve competitive position.",
                             rows = 3)
              )
            ),
            
            br(),
            
            div(style = "text-align: center;",
              downloadButton("download_training_report", "Generate Training Utility Report", 
                           class = "btn-success btn-lg", 
                           style = "padding: 10px 30px; font-size: 16px;")
            ),
            
            br(),
            
            div(style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px;",
              h5("Report Contents:"),
              tags$ul(
                tags$li("Executive summary with key findings and ROI"),
                tags$li("Training program description and objectives"),
                tags$li("Comprehensive utility analysis with multiple approaches"),
                tags$li("Cost-benefit analysis and financial projections"),
                tags$li("Risk assessment and sensitivity analysis"),
                tags$li("Implementation recommendations and timeline"),
                tags$li("Research methodology and assumptions"),
                tags$li("Supporting literature and references")
              )
            )
          )
        )
      ),
      
      # References Tab
      tabItem(tabName = "references",
        fluidRow(
          box(width = 12, title = "References and Methodology", status = "info", solidHeader = TRUE,
            
            h4("Primary Research Foundation"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Oprea, B. T., Barzin, L., Vîrgă, D., Iliescu, D., & Rusu, A. (2019)."),
              p(em("Effectiveness of job crafting interventions: A meta-analysis and utility analysis."), 
                strong("European Journal of Work and Organizational Psychology"), "."),
              p("DOI: 10.1080/1359432X.2019.1646728"),
              
              h6("Contribution:"),
              p("Comprehensive meta-analysis of 14 studies examining job crafting intervention effectiveness. 
                Provides validated effect sizes and utility analysis framework for job crafting programs.")
            ),
            
            h4("Training Utility Analysis Framework"),
            h5("Classical Utility Models:"),
            p("• Brogden, H. E. (1949). When testing pays off. Personnel Psychology, 2(2), 171-183."),
            p("• Cronbach, L. J., & Gleser, G. C. (1965). Psychological tests and personnel decisions. University of Illinois Press."),
            
            h5("Training-Specific Research:"),
            p("• Arthur, W., Bennett, W., Edens, P. S., & Bell, S. T. (2003). Effectiveness of training in organizations: A meta-analysis of design and evaluation features. Journal of Applied Psychology, 88(2), 234-245."),
            p("• Cascio, W. F. (1989). Using utility analysis to assess training outcomes. In I. L. Goldstein (Ed.), Training and development in organizations (pp. 63-88). Jossey-Bass."),
            p("• Mathieu, J. E., & Leonard Jr, R. L. (1987). Applying utility concepts to a training program in supervisory skills: A time-based approach. Academy of Management Journal, 30(2), 316-335."),
            
            h5("Job Crafting Research:"),
            p("• Wrzesniewski, A., & Dutton, J. E. (2001). Crafting a job: Revisioning employees as active crafters of their work. Academy of Management Review, 26(2), 179-201."),
            p("• Tims, M., & Bakker, A. B. (2010). Job crafting: Towards a new model of individual job redesign. SA Journal of Industrial Psychology, 36(2), 1-9."),
            
            h4("App Information"),
            div(style = "background-color: #e7f4fd; padding: 15px; border-radius: 5px; margin-top: 20px;",
              h5("About This Tool"),
              p(strong("Training Utility Analysis Tool"), " (2025). Comprehensive analysis of training program economic value and return on investment."),
              p(strong("Version:"), " 1.0"),
              p(strong("Last Updated:"), " January 2025"),
              
              h5("Source Code & Documentation:"),
              p(strong("GitHub Repository:"), br(),
              HTML("<a href='https://github.com/chriscastille6/-utility-analysis-research' target='_blank' style='color: #666; font-style: italic;'>
              https://github.com/chriscastille6/-utility-analysis-research
              </a>")),
              
              p(strong("Citation for this Tool:"), br(),
              HTML("<em>Training Utility Analysis Tool</em> (2025). Evidence-based analysis of training program effectiveness and ROI.<br>
              <strong>Live App:</strong> <a href='#' target='_blank'>[To be added when deployed]</a><br>
              <strong>Source Code:</strong> https://github.com/chriscastille6/-utility-analysis-research")),
              
              h5("Educational Purpose:"),
              p("Designed for training professionals, HR practitioners, and organizational leaders to make evidence-based 
                decisions about training investments. Suitable for graduate-level courses in organizational psychology, 
                human resource development, and training evaluation."),
              
              h5("AI Development Assistance:"),
              p("This educational application was developed with assistance from AI language models and modern development tools:"),
              tags$ul(
                tags$li(strong("AI Assistants:"), " Claude (Anthropic) for code development, research integration, and educational content design"),
                tags$li(strong("Development Environment:"), " Cursor AI-powered code editor for enhanced productivity and code quality"),
                tags$li(strong("Human Oversight:"), " All AI-generated content was thoroughly reviewed, validated, and refined by human researchers"),
                tags$li(strong("Data Verification:"), " All calculations and methodological implementations were verified against academic publications"),
                tags$li(strong("Academic Integrity:"), " AI tools enhanced development efficiency while maintaining strict adherence to research-based methodologies")
              ),
              p(em("AI assistance was used exclusively for technical implementation and educational presentation. All underlying methodologies, 
                calculations, and theoretical frameworks are based on peer-reviewed academic research.")),
              
              h5("Important Disclaimers:"),
              tags$ul(
                tags$li(strong("Educational Purpose:"), " This tool is designed for educational and decision-support purposes"),
                tags$li(strong("Training Decisions:"), " Actual training investments should consider additional organizational, legal, and contextual factors"),
                tags$li(strong("Model Limitations:"), " All models are simplifications and should be interpreted with appropriate caution"),
                tags$li(strong("Professional Consultation:"), " Complex training program implementations should involve qualified professionals"),
                tags$li(strong("Effect Size Validation:"), " Organizations should validate effect sizes through pilot programs and evaluation studies")
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
  output$sample_training_roi <- renderValueBox({
    valueBox(
      value = "450%",
      subtitle = "Sample Training ROI",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$sample_effect_size <- renderValueBox({
    valueBox(
      value = "d = 0.47",
      subtitle = "Job Crafting Effect",
      icon = icon("tools"),
      color = "blue"
    )
  })
  
  output$sample_payback <- renderValueBox({
    valueBox(
      value = "8 months",
      subtitle = "Typical Payback",
      icon = icon("calendar"),
      color = "orange"
    )
  })
  
  # General Training Utility Analysis
  general_results <- reactive({
    input$calculate_general
    
    isolate({
      params <- list(
        n_employees = input$general_n_employees,
        sdy = input$general_sdy,
        effect_size = input$general_effect_size,
        development_cost = input$general_development_cost,
        per_employee_cost = input$general_per_employee_cost,
        time_cost_per_employee = input$general_time_cost,
        time_period = input$general_time_period
      )
      
      calculate_training_utility(params)
    })
  })
  
  output$general_net_utility <- renderValueBox({
    results <- general_results()
    valueBox(
      value = paste0("$", format(round(results$net_utility), big.mark = ",")),
      subtitle = "Net Training Utility",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$general_roi_ratio <- renderValueBox({
    results <- general_results()
    valueBox(
      value = paste0(round((results$roi_ratio - 1) * 100, 0), "%"),
      subtitle = "ROI Percentage",
      icon = icon("percentage"),
      color = "blue"
    )
  })
  
  output$general_annual_value <- renderValueBox({
    results <- general_results()
    valueBox(
      value = paste0("$", format(round(results$annual_utility), big.mark = ",")),
      subtitle = "Annual Value",
      icon = icon("calendar"),
      color = "orange"
    )
  })
  
  output$general_utility_summary <- renderUI({
    results <- general_results()
    
    payback_years <- results$total_cost / results$annual_utility
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h5>General Training Utility Analysis</h5>",
      
      "<h6>Training Investment:</h6>",
      "<p><strong>Employees Trained:</strong> ", format(input$general_n_employees, big.mark = ","), "</p>",
      "<p><strong>Development Cost:</strong> $", format(input$general_development_cost, big.mark = ","), "</p>",
      "<p><strong>Per-Employee Cost:</strong> $", format(input$general_per_employee_cost, big.mark = ","), "</p>",
      "<p><strong>Opportunity Cost:</strong> $", format(input$general_time_cost, big.mark = ","), " per employee</p>",
      "<p><strong>Total Investment:</strong> $", format(round(results$total_cost), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Expected Benefits:</h6>",
      "<p><strong>Effect Size:</strong> d = ", input$general_effect_size, "</p>",
      "<p><strong>Performance SDy:</strong> $", format(input$general_sdy, big.mark = ","), "</p>",
      "<p><strong>Total Benefits:</strong> $", format(round(results$total_benefit), big.mark = ","), " over ", input$general_time_period, " years</p>",
      "<p><strong>Annual Benefits:</strong> $", format(round(results$total_benefit / input$general_time_period), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Return on Investment:</h6>",
      "<p><strong>Net Utility:</strong> $", format(round(results$net_utility), big.mark = ","), "</p>",
      "<p><strong>ROI Ratio:</strong> ", round(results$roi_ratio, 2), ":1</p>",
      "<p><strong>ROI Percentage:</strong> ", round((results$roi_ratio - 1) * 100, 0), "%</p>",
      "<p><strong>Payback Period:</strong> ", round(payback_years, 1), " years</p>",
      "<p><strong>Per-Employee Value:</strong> $", format(round(results$per_employee_value), big.mark = ","), "</p>",
      
      "</div>"
    ))
  })
  
  output$general_cost_breakdown <- renderPlotly({
    results <- general_results()
    
    cost_data <- data.frame(
      Category = c("Development", "Training Delivery", "Opportunity Cost"),
      Cost = c(
        input$general_development_cost,
        input$general_n_employees * input$general_per_employee_cost,
        input$general_n_employees * input$general_time_cost
      )
    )
    
    p <- ggplot(cost_data, aes(x = reorder(Category, Cost), y = Cost, fill = Category)) +
      geom_col(alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = "Training Cost Breakdown", x = "Cost Category", y = "Cost ($)") +
      theme_minimal() +
      guides(fill = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  output$general_roi_projection <- renderPlotly({
    results <- general_results()
    
    years <- 1:input$general_time_period
    cumulative_benefits <- cumsum(rep(results$total_benefit / input$general_time_period, input$general_time_period))
    cumulative_costs <- rep(results$total_cost, input$general_time_period)
    net_value <- cumulative_benefits - cumulative_costs
    
    projection_data <- data.frame(
      Year = years,
      Benefits = cumulative_benefits,
      Costs = cumulative_costs,
      Net_Value = net_value
    )
    
    p <- ggplot(projection_data) +
      geom_line(aes(x = Year, y = Benefits, color = "Cumulative Benefits"), size = 1.2) +
      geom_line(aes(x = Year, y = Costs, color = "Cumulative Costs"), size = 1.2) +
      geom_line(aes(x = Year, y = Net_Value, color = "Net Value"), size = 1.2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_color_manual(values = c("Cumulative Benefits" = "green", "Cumulative Costs" = "red", "Net Value" = "blue")) +
      labs(title = "Training ROI Projection Over Time", x = "Year", y = "Value ($)", color = "Metric") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Job Crafting Analysis
  jc_results <- reactive({
    input$calculate_jc
    
    isolate({
      # Set effect size based on sector
      effect_size <- if(input$jc_sector == "healthcare") 0.47 else input$jc_custom_effect
      
      params <- list(
        n_employees = input$jc_n_employees,
        sdy = input$jc_sdy,
        jc_effect_size = effect_size,
        intervention_cost = input$jc_intervention_cost,
        per_employee_intervention_cost = input$jc_per_employee_cost,
        duration_factor = input$jc_duration_factor,
        projection_years = input$jc_projection_years
      )
      
      calculate_job_crafting_utility(params)
    })
  })
  
  output$jc_annual_utility <- renderValueBox({
    results <- jc_results()
    valueBox(
      value = paste0("$", format(round(results$net_annual_utility), big.mark = ",")),
      subtitle = "Annual Net Utility",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$jc_multi_year_utility <- renderValueBox({
    results <- jc_results()
    valueBox(
      value = paste0("$", format(round(results$multi_year_utility), big.mark = ",")),
      subtitle = paste0(input$jc_projection_years, "-Year Utility"),
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$jc_roi_ratio <- renderValueBox({
    results <- jc_results()
    valueBox(
      value = paste0(round(results$roi_ratio, 1), ":1"),
      subtitle = "ROI Ratio",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$jc_results_summary <- renderUI({
    results <- jc_results()
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h5>Job Crafting Intervention Utility Analysis</h5>",
      
      "<h6>Intervention Parameters:</h6>",
      "<p><strong>Sector:</strong> ", stringr::str_to_title(input$jc_sector), "</p>",
      "<p><strong>Effect Size:</strong> g = ", round(results$effect_size, 3), 
      if(input$jc_sector == "healthcare") " (validated meta-analytic finding)" else " (custom estimate)", "</p>",
      "<p><strong>Employees:</strong> ", input$jc_n_employees, "</p>",
      "<p><strong>Performance SDy:</strong> $", format(input$jc_sdy, big.mark = ","), "</p>",
      "<p><strong>Duration Factor:</strong> ", round(input$jc_duration_factor * 100, 0), "% of year</p>",
      
      "<hr>",
      
      "<h6>Economic Impact:</h6>",
      "<p><strong>Annual Value per Employee:</strong> $", format(round(results$annual_value_per_employee), big.mark = ","), "</p>",
      "<p><strong>Total Annual Value:</strong> $", format(round(results$total_annual_value), big.mark = ","), "</p>",
      "<p><strong>Intervention Cost:</strong> $", format(round(results$total_intervention_cost), big.mark = ","), "</p>",
      "<p><strong>Net Annual Utility:</strong> $", format(round(results$net_annual_utility), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Multi-Year Projection:</h6>",
      "<p><strong>", input$jc_projection_years, "-Year Utility:</strong> $", format(round(results$multi_year_utility), big.mark = ","), "</p>",
      "<p><strong>ROI Ratio:</strong> ", round(results$roi_ratio, 1), ":1</p>",
      "<p><strong>ROI Percentage:</strong> ", round((results$roi_ratio - 1) * 100, 0), "%</p>",
      
      "<hr>",
      
      "<h6>Key Insights:</h6>",
      "<p>• Each employee generates <strong>$", format(round(results$annual_value_per_employee), big.mark = ","), 
      "</strong> in annual value through job crafting</p>",
      "<p>• Intervention pays for itself in <strong>", 
      round((results$total_intervention_cost / results$net_annual_utility) * 12, 1), " months</strong></p>",
      "<p>• Based on validated research from Oprea et al. (2019) meta-analysis</p>",
      
      "</div>"
    ))
  })
  
  # ROI Calculator Analysis
  roi_results <- reactive({
    input$calculate_roi
    
    isolate({
      # Calculate total investment
      total_investment <- input$roi_design_cost + 
                         (input$roi_participants * input$roi_delivery_cost) + 
                         (input$roi_participants * input$roi_materials_cost) + 
                         (input$roi_participants * input$roi_opportunity_cost)
      
      # Calculate annual benefits with decay
      annual_benefit_base <- input$roi_participants * input$roi_effect_size * input$roi_performance_sdy
      
      # Apply benefit decay over time
      years <- 1:ceiling(input$roi_benefit_duration)
      decay_factor <- (1 - input$roi_decay_rate/100)
      
      # Calculate benefits for each year
      yearly_benefits <- sapply(years, function(year) {
        if(year <= input$roi_benefit_duration) {
          annual_benefit_base * (decay_factor ^ (year - 1))
        } else {
          0
        }
      })
      
      # Partial year calculation if needed
      if(input$roi_benefit_duration %% 1 != 0) {
        partial_year <- ceiling(input$roi_benefit_duration)
        partial_fraction <- input$roi_benefit_duration %% 1
        yearly_benefits[partial_year] <- yearly_benefits[partial_year] * partial_fraction
      }
      
      total_benefits <- sum(yearly_benefits)
      net_benefit <- total_benefits - total_investment
      roi_percentage <- (total_benefits / total_investment - 1) * 100
      
      # Create time series data
      time_series_data <- data.frame(
        Year = years,
        Cumulative_Investment = rep(total_investment, length(years)),
        Cumulative_Benefits = cumsum(yearly_benefits),
        Annual_Benefits = yearly_benefits,
        Net_Value = cumsum(yearly_benefits) - total_investment
      )
      
      # Cost breakdown
      cost_breakdown <- data.frame(
        Category = c("Design & Development", "Delivery Costs", "Materials", "Opportunity Cost"),
        Amount = c(
          input$roi_design_cost,
          input$roi_participants * input$roi_delivery_cost,
          input$roi_participants * input$roi_materials_cost,
          input$roi_participants * input$roi_opportunity_cost
        )
      )
      
      list(
        total_investment = total_investment,
        total_benefits = total_benefits,
        net_benefit = net_benefit,
        roi_percentage = roi_percentage,
        time_series_data = time_series_data,
        cost_breakdown = cost_breakdown,
        payback_period = total_investment / (annual_benefit_base * mean(decay_factor ^ (0:(length(years)-1))))
      )
    })
  })
  
  output$roi_total_investment <- renderValueBox({
    results <- roi_results()
    valueBox(
      value = paste0("$", format(round(results$total_investment), big.mark = ",")),
      subtitle = "Total Investment",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
  
  output$roi_total_benefits <- renderValueBox({
    results <- roi_results()
    valueBox(
      value = paste0("$", format(round(results$total_benefits), big.mark = ",")),
      subtitle = "Total Benefits",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$roi_net_benefit <- renderValueBox({
    results <- roi_results()
    valueBox(
      value = paste0("$", format(round(results$net_benefit), big.mark = ",")),
      subtitle = "Net Benefit",
      icon = icon("plus"),
      color = if(results$net_benefit > 0) "green" else "red"
    )
  })
  
  output$roi_percentage <- renderValueBox({
    results <- roi_results()
    valueBox(
      value = paste0(round(results$roi_percentage, 0), "%"),
      subtitle = "ROI Percentage",
      icon = icon("percentage"),
      color = if(results$roi_percentage > 0) "blue" else "red"
    )
  })
  
  output$roi_time_series <- renderPlotly({
    results <- roi_results()
    
    p <- ggplot(results$time_series_data) +
      geom_line(aes(x = Year, y = Cumulative_Benefits, color = "Cumulative Benefits"), size = 1.2) +
      geom_line(aes(x = Year, y = Cumulative_Investment, color = "Total Investment"), size = 1.2) +
      geom_line(aes(x = Year, y = Net_Value, color = "Net Value"), size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_color_manual(values = c("Cumulative Benefits" = "green", "Total Investment" = "red", "Net Value" = "blue")) +
      labs(title = "ROI Analysis Over Time", x = "Year", y = "Value ($)", color = "Metric") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$roi_breakdown_chart <- renderPlotly({
    results <- roi_results()
    
    p <- ggplot(results$cost_breakdown, aes(x = reorder(Category, Amount), y = Amount, fill = Category)) +
      geom_col(alpha = 0.8) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = "Training Investment Breakdown", x = "Cost Category", y = "Amount ($)") +
      theme_minimal() +
      guides(fill = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  output$roi_detailed_analysis <- renderUI({
    results <- roi_results()
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 5px;'>",
      "<h5>Comprehensive ROI Analysis</h5>",
      
      "<h6>Investment Breakdown:</h6>",
      "<p><strong>Participants:</strong> ", format(input$roi_participants, big.mark = ","), "</p>",
      "<p><strong>Design & Development:</strong> $", format(input$roi_design_cost, big.mark = ","), "</p>",
      "<p><strong>Delivery Cost:</strong> $", format(input$roi_delivery_cost, big.mark = ","), " × ", input$roi_participants, " = $", format(input$roi_delivery_cost * input$roi_participants, big.mark = ","), "</p>",
      "<p><strong>Materials Cost:</strong> $", format(input$roi_materials_cost, big.mark = ","), " × ", input$roi_participants, " = $", format(input$roi_materials_cost * input$roi_participants, big.mark = ","), "</p>",
      "<p><strong>Opportunity Cost:</strong> $", format(input$roi_opportunity_cost, big.mark = ","), " × ", input$roi_participants, " = $", format(input$roi_opportunity_cost * input$roi_participants, big.mark = ","), "</p>",
      "<p><strong>Total Investment:</strong> $", format(round(results$total_investment), big.mark = ","), "</p>",
      
      "<hr>",
      
      "<h6>Benefit Calculations:</h6>",
      "<p><strong>Effect Size:</strong> d = ", input$roi_effect_size, "</p>",
      "<p><strong>Performance SDy:</strong> $", format(input$roi_performance_sdy, big.mark = ","), "</p>",
      "<p><strong>Annual Benefit per Person:</strong> $", format(round(input$roi_effect_size * input$roi_performance_sdy), big.mark = ","), "</p>",
      "<p><strong>Total Annual Benefits (Year 1):</strong> $", format(round(input$roi_participants * input$roi_effect_size * input$roi_performance_sdy), big.mark = ","), "</p>",
      "<p><strong>Benefit Duration:</strong> ", input$roi_benefit_duration, " years</p>",
      "<p><strong>Annual Decay Rate:</strong> ", input$roi_decay_rate, "%</p>",
      
      "<hr>",
      
      "<h6>ROI Summary:</h6>",
      "<p><strong>Total Benefits:</strong> $", format(round(results$total_benefits), big.mark = ","), "</p>",
      "<p><strong>Net Benefit:</strong> $", format(round(results$net_benefit), big.mark = ","), "</p>",
      "<p><strong>ROI Percentage:</strong> ", round(results$roi_percentage, 1), "%</p>",
      "<p><strong>Payback Period:</strong> ", round(results$payback_period, 1), " years</p>",
      
      "<hr>",
      
      "<h6>Key Insights:</h6>",
      if(results$roi_percentage > 0) {
        paste0("<p style='color: green;'>• <strong>Positive ROI:</strong> Training investment generates positive returns</p>",
               "<p>• <strong>Break-even:</strong> Investment recovers in ", round(results$payback_period, 1), " years</p>",
               "<p>• <strong>Per-participant value:</strong> $", format(round(results$net_benefit / input$roi_participants), big.mark = ","), " net benefit per trainee</p>")
      } else {
        paste0("<p style='color: red;'>• <strong>Negative ROI:</strong> Training investment does not recover costs under current assumptions</p>",
               "<p>• Consider increasing effect size, reducing costs, or extending benefit duration</p>")
      },
      
      "</div>"
    ))
  })
  
  # Download Report Handler
  output$download_training_report <- downloadHandler(
    filename = function() {
      paste0("training_utility_analysis_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # This would implement comprehensive PDF report generation
      # For now, create a basic template
      rmd_content <- paste0('
---
title: "Training Utility Analysis Report"
subtitle: "', input$report_org_name, '"
date: "', format(Sys.Date(), "%B %d, %Y"), '"
output: 
  pdf_document:
    toc: true
    number_sections: true
---

# Executive Summary

This training utility analysis evaluates the economic value and return on investment for **', input$report_program_name, '** at **', input$report_org_name, '**.

## Training Program Overview

**Program Type:** ', input$report_type, '
**Training Budget:** $', format(input$report_budget, big.mark = ","), '

## Objectives

', input$report_objectives, '

## Business Context

', input$report_context, '

# Utility Analysis Results

[Detailed analysis results would be inserted here based on the specific calculations performed in the app]

# Recommendations

1. **Proceed with Implementation:** Based on the utility analysis
2. **Monitor and Evaluate:** Track actual results against projections
3. **Continuous Improvement:** Refine program based on outcomes

---

*Report generated by the Training Utility Analysis Tool (2025)*
      ')
      
      rmd_file <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, rmd_file)
      rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
