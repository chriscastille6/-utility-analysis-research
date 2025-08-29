# Fisher & Connelly (2017) Educational Web App
# Interactive tool for learning about the business case for contingent workers

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(plotly)
library(tidyr)
library(rmarkdown)
library(knitr)
library(ggtext)
library(grid)
library(gridExtra)

# Load the data from the reproduction analysis
load_reproduction_data <- function() {
  # Load the saved results from the reproduction
  results_path <- "reproductions/fisher_connelly_2017/fisher_connelly_2017_results.rds"
  if (file.exists(results_path)) {
    results <- readRDS(results_path)
    return(results$basic_net_value)
  } else {
    # Fallback data if file doesn't exist
    return(tibble::tibble(
      worker_type = c("Permanent employees", "IC, direct", "IC, AOR", "Temporary workers"),
      service_value = c(82955, 105768, 105768, 69683),
      service_costs_no_to = c(63181, 60502, 63518, 50969),
      net_value_no_to = c(19774, 45266, 42250, 18714),
      turnover_costs = c(13243, 18090, 19899, 23837),
      total_service_costs_with_to = c(76424, 78592, 83417, 74806),
      net_value_with_to = c(6532, 27176, 22351, -5093)
    ))
  }
}

# Six organizational strategies from Fisher & Connelly (2017) Table 8
get_strategy_data <- function() {
  # Data from Table 8: Results from Scenario Simulations
  tibble::tibble(
    strategy = c("NormalCo", "MegaCo", "Temp-to-PermCo", "CoreCompCo-A", "CoreCompCo-B", "BlendedCo",
                 "NormalCo", "MegaCo", "Temp-to-PermCo", "CoreCompCo-A", "CoreCompCo-B", "BlendedCo",
                 "NormalCo", "MegaCo", "Temp-to-PermCo", "CoreCompCo-A", "CoreCompCo-B", "BlendedCo"),
    worker_type = c(rep("Permanent", 6), rep("IC", 6), rep("Temporary", 6)),
    num_employees = c(27, 25, 26, 3, 3, 24,
                     3, 1, 1, 7, 7, 3, 
                     3, 7, 6, 23, 23, 6),
    net_value_per_employee = c(6532, -26819, 42163, 6532, 6532, -24170,
                              22351, 10275, 80607, 20103, 33953, 54559,
                              -4779, -33592, 15473, -32147, -32147, 4130),
    total_net_value = c(176353, -670482, 1096232, 19595, 19595, -580081,
                       67054, 10274, 80607, 140722, 237671, 163676,
                       -14338, -235145, 92839, -739382, -739382, 24779),
    workforce_total = c(229068, -895352, 1269678, -579065, -481860, -391726,
                       229068, -895352, 1269678, -579065, -481860, -391726,
                       229068, -895352, 1269678, -579065, -481860, -391726)
  )
}

# Strategy descriptions from the paper
get_strategy_descriptions <- function() {
  list(
    "NormalCo" = list(
      title = "NormalCo: Baseline Strategy",
      description = "Uses contingent workers as needed to respond to fluctuations in staffing requirements, consistent with the core-periphery distinction. Wages, benefits, and training for permanent employees and contingent workers are average, and all ICs are required to have an Agent of Record (AOR). Performance, OCBs, and turnover are also average. This scenario represents a baseline condition.",
      characteristics = c("Average wages and benefits", "Standard performance levels", "Moderate turnover rates", "Uses AOR for all ICs", "Responds to demand fluctuations")
    ),
    "MegaCo" = list(
      title = "MegaCo: Low-Cost Strategy",
      description = "Pursues a low-cost HR strategy and uses contingent workers wherever possible to save money. Organizations that expect a ready supply of suitable employees avoid investing in training and retention. Wages, benefits, and training for all workers are low. Performance and OCBs are low, and turnover among all three employee groups is high.",
      characteristics = c("Low wages and minimal benefits", "Minimal training investment", "High turnover accepted as cost of business", "Low performance and OCBs", "Cost minimization focus")
    ),
    "Temp-to-PermCo" = list(
      title = "Temp-to-PermCo: Extended Screening Strategy",
      description = "Uses contingent employment as an extended screening or probationary process through which contingent workers may eventually become permanent employees. This approach is common in accounting and engineering where skilled workers are scarce. Wages, benefits, and training for permanent employees are high. Performance and OCBs are also high, and turnover is low.",
      characteristics = c("High investment in permanent employees", "Contingent work as probationary period", "High performance expectations", "Low turnover rates", "Extended screening process")
    ),
    "CoreCompCo-A" = list(
      title = "CoreCompCo-A: Direct IC Core Competency Strategy",
      description = "Staff noncore roles predominantly with contingent workers (direct ICs). Core employees' job security is protected by the presence of contingent workers, who will be the first to be let go if financial exigencies demand it. Permanent employees have average conditions, while contingent workers have lower wages, training, performance, and OCBs with high turnover.",
      characteristics = c("Core-periphery workforce model", "Direct hire independent contractors", "Job security for core employees", "Lower investment in contingent workers", "High contingent worker turnover")
    ),
    "CoreCompCo-B" = list(
      title = "CoreCompCo-B: Agent IC Core Competency Strategy", 
      description = "Same as CoreCompCo-A but requires all contractors to be hired through an Agent of Record (AOR). This directly illustrates the financial implications of AOR arrangements compared to direct hiring of independent contractors.",
      characteristics = c("Core-periphery workforce model", "Agent of Record for all ICs", "Compliance and administrative oversight", "Higher IC costs due to AOR fees", "Same performance patterns as CoreCompCo-A")
    ),
    "BlendedCo" = list(
      title = "BlendedCo: No Strategy Approach",
      description = "Companies that do not have an a priori strategy for using contingent workers, and use a combination of contingent workers and permanent employees working together. An unintended consequence is that permanent employees' perceptions of job security are reduced, leading to lower OCBs and performance levels. Wages, benefits, and training for all workers are average.",
      characteristics = c("No clear contingent worker strategy", "Mixed workforce without planning", "Reduced permanent employee job security", "Lower OCBs due to uncertainty", "Average compensation across all workers")
    )
  )
}

# Create Figure 1 from Fisher & Connelly (2017)
create_fc2017_figure1 <- function() {
  # Define the components (repositioned for wider layout)
  framework_data <- data.frame(
    component = c("HR Strategy", "Worker Type", "Service Costs", "Service Value", "Net Value"),
    x = c(3.5, 2, 6, 6, 9.5),
    y = c(5.5, 3, 4.5, 1.5, 3),
    width = c(2.2, 1.8, 2.0, 2.0, 1.8),
    height = c(1.4, 1.2, 1.4, 1.4, 1.0)
  )
  
  # Create detailed text for each component
  hr_strategy_text <- "**HR Strategy**<br>• Low cost<br>• Core-periphery<br>• Temp-to-perm"
  
  worker_type_text <- "**Worker Type**<br>• Permanent<br>• Contingent (temporary,<br>&nbsp;&nbsp;independent contractor)"
  
  service_costs_text <- "**Service Costs**<br>• Wages and benefits<br>• Transaction costs<br>• Turnover"
  
  service_value_text <- "**Service Value**<br>• Task performance<br>• OCBs"
  
  net_value_text <- "**Net Value**"
  
  # Create the base plot
  p <- ggplot() +
    # Draw arrows FIRST (so they appear behind boxes)
    
    # Main relationship arrows (starting from behind boxes)
    # Worker Type to Service Costs
    geom_segment(aes(x = 2.9, y = 3.4, xend = 5.0, yend = 4.2), 
                 arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2, color = "darkblue") +
    
    # Worker Type to Service Value  
    geom_segment(aes(x = 2.9, y = 2.6, xend = 5.0, yend = 1.8), 
                 arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2, color = "darkblue") +
    
    # HR Strategy moderation arrows (pointing to the relationship arrows)
    # HR Strategy moderates Worker Type -> Service Costs relationship
    geom_curve(aes(x = 3.5, y = 4.8, xend = 4.0, yend = 3.9), 
               arrow = arrow(length = unit(0.25, "cm")), linewidth = 1, 
               color = "red", curvature = -0.3) +
    
    # HR Strategy moderates Worker Type -> Service Value relationship  
    geom_curve(aes(x = 3.5, y = 4.8, xend = 4.0, yend = 2.1), 
               arrow = arrow(length = unit(0.25, "cm")), linewidth = 1, 
               color = "red", curvature = 0.3) +
    
    # Service Costs to Service Value (feedback loop)
    geom_segment(aes(x = 6, y = 3.8, xend = 6, yend = 2.2), 
                 arrow = arrow(length = unit(0.3, "cm")), linewidth = 1, color = "darkred") +
    
    # Service Costs to Net Value
    geom_segment(aes(x = 7.0, y = 4.2, xend = 8.5, yend = 3.4), 
                 arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2, color = "darkgreen") +
    
    # Service Value to Net Value
    geom_segment(aes(x = 7.0, y = 1.8, xend = 8.5, yend = 2.6), 
                 arrow = arrow(length = unit(0.3, "cm")), linewidth = 1.2, color = "darkgreen") +
    
    # Add rectangles for each component (drawn AFTER arrows so they appear on top)
    geom_rect(data = framework_data, 
              aes(xmin = x - width/2, xmax = x + width/2,
                  ymin = y - height/2, ymax = y + height/2),
              fill = "lightblue", color = "black", alpha = 0.9, linewidth = 0.8) +
    
    # Add text labels
    annotate("richtext", x = 3.5, y = 5.5, label = hr_strategy_text, 
             hjust = 0.5, vjust = 0.5, size = 3.5, 
             fill = NA, label.color = NA) +
    
    annotate("richtext", x = 2, y = 3, label = worker_type_text, 
             hjust = 0.5, vjust = 0.5, size = 3.5, 
             fill = NA, label.color = NA) +
    
    annotate("richtext", x = 6, y = 4.5, label = service_costs_text, 
             hjust = 0.5, vjust = 0.5, size = 3.5,
             fill = NA, label.color = NA) +
    
    annotate("richtext", x = 6, y = 1.5, label = service_value_text, 
             hjust = 0.5, vjust = 0.5, size = 3.5,
             fill = NA, label.color = NA) +
    
    annotate("richtext", x = 9.5, y = 3, label = net_value_text, 
             hjust = 0.5, vjust = 0.5, size = 4,
             fill = NA, label.color = NA) +
    
    # Add annotations for different types of relationships
    annotate("text", x = 6.3, y = 3, label = "Costs can affect\nperformance", 
             size = 2.8, color = "darkred", hjust = 0) +
    
    annotate("text", x = 1.2, y = 4.5, label = "HR Strategy\nmoderates\nrelationships", 
             size = 3, color = "red", hjust = 0.5, fontface = "italic") +
    
    # Add title
    labs(title = "Figure 1: A Framework of Contingent Worker Costs and Benefits",
         subtitle = "From Fisher & Connelly (2017) - HR Strategy as Moderator") +
    
    # Remove axes and grid
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Set coordinate limits (even wider for better spacing)
    xlim(0, 11) +
    ylim(0.5, 6.5)
  
  return(p)
}

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Contingent Workers Business Case"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Interactive Analysis", tabName = "analysis", icon = icon("calculator")),
      menuItem("Six Strategy Analysis", tabName = "strategies", icon = icon("chess")),
      menuItem("Custom Scenario Builder", tabName = "scenarios", icon = icon("sliders-h")),
      menuItem("Business Case Report", tabName = "report", icon = icon("file-alt")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .info-box {
          background: white;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .metric-box {
          text-align: center;
          background: white;
          border-radius: 5px;
          padding: 20px;
          margin: 10px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .positive { color: #28a745; font-weight: bold; }
        .negative { color: #dc3545; font-weight: bold; }
        .neutral { color: #6c757d; font-weight: bold; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "About This Analysis", width = 8, status = "primary",
            HTML("
            <h3>Fisher & Connelly (2017): Lower Cost or Just Lower Value?</h3>
            <p><strong>Research Question:</strong> When do contingent workers provide better value than permanent employees?</p>
            
            <h4>Key Concepts:</h4>
            <ul>
              <li><strong>Contingent Workers:</strong> Independent contractors, temporary workers, and agency workers</li>
              <li><strong>Service Value:</strong> The economic value generated by an employee's work</li>
              <li><strong>Service Costs:</strong> Direct salary, benefits, training, and turnover costs</li>
              <li><strong>Net Value:</strong> Service Value minus Service Costs</li>
            </ul>
            
            <h4>Worker Types Analyzed:</h4>
            <ul>
              <li><strong>Permanent Employees:</strong> Traditional full-time employees with benefits</li>
              <li><strong>IC Direct:</strong> Independent contractors hired directly</li>
              <li><strong>IC Agent of Record:</strong> Independent contractors through agencies</li>
              <li><strong>Temporary Workers:</strong> Short-term employees through agencies</li>
            </ul>
            ")
          ),
          box(
            title = "Conceptual Framework", width = 6, status = "info",
            plotOutput("figure1_plot", height = "500px", width = "100%"),
            br(),
            p("This framework shows how HR strategy moderates the relationships between worker types and both service costs and service value, 
              which together determine the net value to the organization.", 
              style = "font-size: 0.9em; color: #666;")
          )
        ),
        
        fluidRow(
          valueBox(
            value = "$27,176",
            subtitle = "Highest Net Value: IC Direct",
            icon = icon("trophy"),
            color = "green",
            width = 3
          ),
          valueBox(
            value = "-$5,093",
            subtitle = "Lowest Net Value: Temporary",
            icon = icon("exclamation-triangle"),
            color = "red",
            width = 3
          ),
          valueBox(
            value = "40%",
            subtitle = "Turnover Impact on IC Direct",
            icon = icon("chart-line"),
            color = "blue",
            width = 3
          ),
          valueBox(
            value = "127%",
            subtitle = "Turnover Impact on Temporary",
            icon = icon("chart-line"),
            color = "orange",
            width = 3
          )
        )
      ),
      
      # Interactive Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Cost and Value Analysis", width = 8, status = "primary",
            plotlyOutput("main_plot", height = "400px")
          ),
          box(
            title = "Key Metrics", width = 4, status = "info",
            tableOutput("metrics_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Breakdown", width = 12, status = "primary",
            DT::dataTableOutput("detailed_table")
          )
        )
      ),
      
      # Six Strategy Analysis Tab
      tabItem(tabName = "strategies",
        fluidRow(
          box(
            title = "Fisher & Connelly (2017) Six Organizational Strategies", width = 12, status = "primary",
            HTML("
            <p>Fisher & Connelly analyzed six different organizational strategies for using contingent workers. 
            Each strategy represents a different approach to workforce management with distinct implications for costs and value.</p>
            ")
          )
        ),
        
        fluidRow(
          box(
            title = "Strategy Selection", width = 4, status = "info",
            selectInput("strategy_choice", "Choose Strategy to Analyze:",
                       choices = list(
                         "1. NormalCo (Baseline)" = "NormalCo",
                         "2. MegaCo (Low-Cost)" = "MegaCo", 
                         "3. Temp-to-PermCo (Extended Screening)" = "Temp-to-PermCo",
                         "4a. CoreCompCo-A (Direct ICs)" = "CoreCompCo-A",
                         "4b. CoreCompCo-B (Agent ICs)" = "CoreCompCo-B",
                         "5. BlendedCo (No Strategy)" = "BlendedCo"
                       ),
                       selected = "NormalCo"),
            br(),
            h5("Compare All Strategies:"),
            actionButton("show_all_strategies", "View All Six Strategies", 
                        class = "btn-success", style = "width: 100%;")
          ),
          
          box(
            title = "Strategy Description", width = 8, status = "primary",
            htmlOutput("strategy_description")
          )
        ),
        
        fluidRow(
          box(
            title = "Strategy Results", width = 8, status = "success",
            conditionalPanel(
              condition = "input.show_all_strategies % 2 == 0",
              plotlyOutput("strategy_plot", height = "400px")
            ),
            conditionalPanel(
              condition = "input.show_all_strategies % 2 == 1",
              plotlyOutput("all_strategies_plot", height = "400px")
            )
          ),
          box(
            title = "Key Metrics", width = 4, status = "info",
            conditionalPanel(
              condition = "input.show_all_strategies % 2 == 0",
              tableOutput("strategy_metrics")
            ),
            conditionalPanel(
              condition = "input.show_all_strategies % 2 == 1",
              tableOutput("all_strategies_summary")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Strategic Insights", width = 12, status = "warning",
            htmlOutput("strategy_insights")
          )
        )
      ),
      
      # Custom Scenario Builder Tab
      tabItem(tabName = "scenarios",
        fluidRow(
          box(
            title = "Scenario Parameters", width = 4, status = "primary",
            h4("Adjust Key Variables"),
            
            sliderInput("temp_wage_multiplier", 
                       "Temporary Worker Wage (% of Permanent)", 
                       min = 60, max = 120, value = 84, step = 5),
            
            sliderInput("ic_wage_multiplier", 
                       "IC Worker Wage (% of Permanent)", 
                       min = 100, max = 150, value = 127, step = 5),
            
            sliderInput("turnover_rate", 
                       "Annual Turnover Rate (%)", 
                       min = 10, max = 50, value = 30, step = 5),
            
            sliderInput("temp_benefits_pct", 
                       "Temporary Benefits (% of Wage)", 
                       min = 0, max = 30, value = 27, step = 5),
            
            sliderInput("ic_benefits_pct", 
                       "IC Benefits (% of Wage)", 
                       min = 0, max = 10, value = 5, step = 1),
            
            actionButton("reset_defaults", "Reset to Paper Values", 
                        class = "btn-warning")
          ),
          
          box(
            title = "Scenario Results", width = 8, status = "success",
            plotlyOutput("scenario_plot", height = "400px"),
            br(),
            htmlOutput("scenario_interpretation")
          )
        )
      ),
      
      # Business Case Report Tab
      tabItem(tabName = "report",
        fluidRow(
          box(
            title = "Report Generator", width = 12, status = "primary",
            fluidRow(
              column(6,
                h4("Generate Custom Report"),
                p("Create a downloadable PDF report with your current scenario settings and analysis."),
                br(),
                downloadButton("download_report", "Download PDF Report", 
                             class = "btn-primary btn-lg"),
                br(), br(),
                p(em("Note: Report generation may take 30-60 seconds depending on your system."))
              ),
              column(6,
                h4("Report Contents"),
                tags$ul(
                  tags$li("Executive summary with key findings"),
                  tags$li("Your custom scenario parameters and results"),
                  tags$li("Comparative analysis across worker types"), 
                  tags$li("Strategic recommendations"),
                  tags$li("Implementation guidelines"),
                  tags$li("Charts and data tables")
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Executive Summary", width = 12, status = "primary",
            htmlOutput("executive_summary")
          )
        ),
        
        fluidRow(
          box(
            title = "Recommendations", width = 6, status = "success",
            htmlOutput("recommendations")
          ),
          box(
            title = "Risk Factors", width = 6, status = "warning",
            htmlOutput("risk_factors")
          )
        ),
        
        fluidRow(
          box(
            title = "Implementation Guidelines", width = 12, status = "info",
            htmlOutput("implementation_guide")
          )
        )
      ),
      
      # References Tab
      tabItem(tabName = "references",
        fluidRow(
          box(
            title = "Primary Reference", width = 12, status = "primary",
            HTML("
            <h4>Fisher, S. L., & Connelly, C. E. (2017). Lower Cost or Just Lower Value? Modeling the Organizational Costs and Benefits of Contingent Work. <em>Academy of Management Discoveries, 3</em>(2), 165–186.</h4>
            <p><strong>DOI:</strong> <a href='https://doi.org/10.5465/amd.2015.0119' target='_blank'>https://doi.org/10.5465/amd.2015.0119</a></p>
            
            <h5>Abstract:</h5>
            <p style='text-align: justify; margin: 15px 0;'>
            Although many managers assume that the use of contingent workers helps organizations lower their costs, it is unclear if these anticipated savings actually materialize once these workers' productivity and indirect costs are taken into account. The purpose of this paper is to identify the conditions under which contingent workers may (or may not) be a cost-effective solution for organizations. We develop a theoretical framework of the financial costs and benefits of three different contingent work arrangements, taking into account direct and indirect costs as well as the value of both task performance and organizational citizenship behaviors. This framework suggests that costs associated with lower performance and higher turnover substantially reduce the overall value of temporary agency workers. We then use a simulation approach with six scenarios representing different organizational strategies to examine how organizational circumstances may further affect the likelihood that the use of contingent workers actually represents a significant cost savings. Our results suggest that although temporary workers were less cost-effective (and independent contractors were more cost-effective) in each scenario, the cost-effectiveness of each worker type also varies depending on the strategy, with the 'Temp-to-Perm' approach being most cost-effective overall.
            </p>
            ")
          )
        ),
        
        fluidRow(
          box(
            title = "Related Resources", width = 6, status = "info",
            HTML("
            <h5>Key Concepts Referenced:</h5>
            <ul>
              <li><strong>Utility Analysis:</strong> Brogden, H. E. (1949). When testing pays off. <em>Personnel Psychology, 2</em>(2), 171-183.</li>
              <li><strong>Service Value Framework:</strong> Schmidt, F. L., & Hunter, J. E. (1998). The validity and utility of selection methods in personnel psychology. <em>Psychological Bulletin, 124</em>(2), 262-274.</li>
              <li><strong>Contingent Work Theory:</strong> Connelly, C. E., & Gallagher, D. G. (2004). Emerging trends in contingent work research. <em>Journal of Management, 30</em>(6), 959-983.</li>
            </ul>
            
            <h5>Additional Reading:</h5>
            <ul>
              <li>Davis-Blake, A., & Broschak, J. P. (2009). Outsourcing and the changing nature of work. <em>Annual Review of Sociology, 35</em>, 321-340.</li>
              <li>Kalleberg, A. L. (2000). Nonstandard employment relations: Part-time, temporary and contract work. <em>Annual Review of Sociology, 26</em>(1), 341-365.</li>
            </ul>
            ")
          ),
          box(
            title = "App Information", width = 6, status = "success",
            HTML("
            <h5>Educational Tool Details:</h5>
            <p><strong>Purpose:</strong> Interactive exploration of contingent worker business cases</p>
            <p><strong>Target Audience:</strong> Students, researchers, and practitioners in HR and organizational management</p>
            <p><strong>Version:</strong> 1.0</p>
            <p><strong>Last Updated:</strong> December 2024</p>
            
            <h5>Source Code & Documentation:</h5>
            <p><strong>GitHub Repository:</strong><br>
            <a href='#' target='_blank' style='color: #666; font-style: italic;'>
            [GitHub link to be added when published online]
            </a></p>
            
            <p><strong>Citation for this Tool:</strong><br>
            <em>Contingent Workers Business Case Analysis Tool</em> (2024). Interactive educational application based on Fisher & Connelly (2017). [GitHub URL to be added].</p>
            
            <h5>Technical Requirements:</h5>
            <ul>
              <li>R (version 4.0+)</li>
              <li>Shiny framework</li>
              <li>Modern web browser</li>
              <li>LaTeX for PDF reports (optional)</li>
            </ul>
            ")
          )
        ),
        
        fluidRow(
          box(
            title = "Acknowledgments", width = 12, status = "warning",
            HTML("
            <p><strong>Original Research:</strong> This application is based entirely on the work of Sandra L. Fisher (Clarkson University) and Catherine E. Connelly (McMaster University). All data, frameworks, and core findings are from their 2017 Academy of Management Discoveries publication.</p>
            
            <p><strong>Educational Implementation:</strong> This interactive tool was developed to enhance student learning and practical application of utility analysis concepts. The app reproduces and extends the original analysis for educational purposes.</p>
            
            <p><strong>Data Verification:</strong> All calculations and parameter values have been verified against the original publication to ensure accuracy and reproducibility.</p>
            
            <p><strong>AI Development Assistance:</strong> This educational app was developed with assistance from AI language models and modern development tools:</p>
            <ul>
              <li><strong>AI Assistants:</strong> Claude (Anthropic) for code development, data analysis, and educational content design</li>
              <li><strong>Development Environment:</strong> Cursor AI-powered code editor for enhanced productivity and code quality</li>
              <li><strong>Human Oversight:</strong> All AI-generated content was reviewed, validated, and refined by human researchers</li>
              <li><strong>Data Verification:</strong> Reproduction analyses and calculations were independently verified against the original publication</li>
            </ul>
            <p><em>AI tools enhanced development efficiency while maintaining academic rigor and accuracy in reproducing the original research findings.</em></p>
            
            <p><strong>Disclaimer:</strong> This tool is for educational purposes only. Business decisions should consider additional factors beyond those modeled in this analysis.</p>
            ")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load the baseline data from reproduction
  baseline_data <- reactive({
    # This uses the data from the reproduction analysis
    load_reproduction_data()
  })
  
  # Render Figure 1
  output$figure1_plot <- renderPlot({
    create_fc2017_figure1()
  })
  
  # Calculate scenario data based on user inputs
  scenario_data <- reactive({
    # Base permanent employee salary
    perm_salary <- 47295
    
    # Calculate adjusted wages
    temp_wage <- perm_salary * (input$temp_wage_multiplier / 100)
    ic_wage <- perm_salary * (input$ic_wage_multiplier / 100)
    
    # Calculate service values (using paper's multiplier approach)
    perm_value <- 82955  # From paper
    temp_value <- temp_wage * 1.754  # Paper's service value multiplier
    ic_value <- ic_wage * 1.754
    
    # Calculate benefits
    perm_benefits <- 13999  # From paper
    temp_benefits <- temp_wage * (input$temp_benefits_pct / 100)
    ic_benefits <- ic_wage * (input$ic_benefits_pct / 100)
    
    # Training costs (simplified)
    training_costs <- c(1887, 201, 201, 172)
    
    # Calculate turnover costs
    turnover_multiplier <- input$turnover_rate / 100
    perm_turnover <- perm_salary * turnover_multiplier
    ic_turnover <- ic_wage * turnover_multiplier * 0.3  # Adjusted for IC structure
    temp_turnover <- temp_wage * turnover_multiplier * 2  # Higher for temporary
    
    # Build scenario results
    tibble(
      worker_type = c("Permanent", "IC Direct", "IC Agent", "Temporary"),
      service_value = c(perm_value, ic_value, ic_value, temp_value),
      wage_costs = c(perm_salary, ic_wage, ic_wage, temp_wage),
      benefits_costs = c(perm_benefits, ic_benefits, ic_benefits * 1.05, temp_benefits),
      training_costs = training_costs,
      turnover_costs = c(perm_turnover, ic_turnover, ic_turnover * 1.1, temp_turnover),
      total_costs = wage_costs + benefits_costs + training_costs + turnover_costs,
      net_value = service_value - total_costs
    )
  })
  
  # Main plot for analysis tab
  output$main_plot <- renderPlotly({
    data <- baseline_data() %>%
      mutate(
        worker_type = case_when(
          worker_type == "Permanent employees" ~ "Permanent",
          worker_type == "IC, direct" ~ "IC Direct",
          worker_type == "IC, AOR" ~ "IC Agent",
          worker_type == "Temporary workers" ~ "Temporary"
        )
      ) %>%
      select(worker_type, service_value, total_service_costs_with_to, net_value_with_to)
    
    p <- data %>%
      ggplot(aes(x = reorder(worker_type, net_value_with_to))) +
      geom_col(aes(y = service_value/1000), fill = "lightblue", alpha = 0.7, width = 0.6) +
      geom_col(aes(y = total_service_costs_with_to/1000), fill = "lightcoral", alpha = 0.7, width = 0.4) +
      geom_point(aes(y = net_value_with_to/1000), size = 4, color = "darkgreen") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      coord_flip() +
      scale_y_continuous(labels = label_dollar(suffix = "K")) +
      labs(
        title = "Service Value vs. Service Costs by Worker Type",
        subtitle = "Points show net value; bars show value (blue) and costs (red)",
        x = "Worker Type",
        y = "Annual Amount (Thousands)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Metrics table
  output$metrics_table <- renderTable({
    baseline_data() %>%
      mutate(
        worker_type = case_when(
          worker_type == "Permanent employees" ~ "Permanent",
          worker_type == "IC, direct" ~ "IC Direct",
          worker_type == "IC, AOR" ~ "IC Agent",
          worker_type == "Temporary workers" ~ "Temporary"
        )
      ) %>%
      select(
        "Worker Type" = worker_type,
        "Net Value" = net_value_with_to
      ) %>%
      mutate(
        "Net Value" = paste0("$", format(`Net Value`, big.mark = ","))
      )
  }, striped = TRUE, hover = TRUE)
  
  # Detailed table
  output$detailed_table <- DT::renderDataTable({
    baseline_data() %>%
      mutate(
        worker_type = case_when(
          worker_type == "Permanent employees" ~ "Permanent",
          worker_type == "IC, direct" ~ "IC Direct",
          worker_type == "IC, AOR" ~ "IC Agent",
          worker_type == "Temporary workers" ~ "Temporary"
        )
      ) %>%
      select(
        "Worker Type" = worker_type,
        "Service Value" = service_value,
        "Service Costs" = service_costs_no_to,
        "Turnover Costs" = turnover_costs,
        "Total Costs" = total_service_costs_with_to,
        "Net Value" = net_value_with_to
      ) %>%
      mutate(across(where(is.numeric), ~ paste0("$", format(.x, big.mark = ","))))
  }, options = list(scrollX = TRUE, pageLength = 10))
  
  # Scenario plot
  output$scenario_plot <- renderPlotly({
    data <- scenario_data()
    
    p <- data %>%
      ggplot(aes(x = reorder(worker_type, net_value), y = net_value/1000)) +
      geom_col(aes(fill = ifelse(net_value > 0, "Positive", "Negative")), 
               alpha = 0.8, width = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("Positive" = "darkgreen", "Negative" = "darkred")) +
      scale_y_continuous(labels = label_dollar(suffix = "K")) +
      coord_flip() +
      labs(
        title = "Net Value by Worker Type (Scenario Analysis)",
        x = "Worker Type",
        y = "Annual Net Value (Thousands)",
        fill = "Net Value"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Scenario interpretation
  output$scenario_interpretation <- renderUI({
    data <- scenario_data()
    best_worker <- data$worker_type[which.max(data$net_value)]
    worst_worker <- data$worker_type[which.min(data$net_value)]
    best_value <- max(data$net_value)
    worst_value <- min(data$net_value)
    
    HTML(paste0(
      "<div class='info-box'>",
      "<h4>Scenario Results:</h4>",
      "<p><strong>Best Option:</strong> ", best_worker, 
      " with <span class='positive'>$", format(round(best_value), big.mark = ","), "</span> annual net value</p>",
      "<p><strong>Worst Option:</strong> ", worst_worker, 
      " with <span class='", ifelse(worst_value < 0, "negative", "positive"), "'>$", 
      format(round(worst_value), big.mark = ","), "</span> annual net value</p>",
      "<p><strong>Value Difference:</strong> $", format(round(best_value - worst_value), big.mark = ","), 
      " between best and worst options</p>",
      "</div>"
    ))
  })
  
  # Reset defaults
  observeEvent(input$reset_defaults, {
    updateSliderInput(session, "temp_wage_multiplier", value = 84)
    updateSliderInput(session, "ic_wage_multiplier", value = 127)
    updateSliderInput(session, "turnover_rate", value = 30)
    updateSliderInput(session, "temp_benefits_pct", value = 27)
    updateSliderInput(session, "ic_benefits_pct", value = 5)
  })
  
  # Strategy analysis outputs
  output$strategy_description <- renderUI({
    strategy <- input$strategy_choice
    descriptions <- get_strategy_descriptions()
    info <- descriptions[[strategy]]
    
    HTML(paste0(
      "<h4>", info$title, "</h4>",
      "<p>", info$description, "</p>",
      "<h5>Key Characteristics:</h5>",
      "<ul>",
      paste0("<li>", info$characteristics, "</li>", collapse = ""),
      "</ul>"
    ))
  })
  
  output$strategy_plot <- renderPlotly({
    strategy <- input$strategy_choice
    data <- get_strategy_data() %>%
      filter(strategy == !!strategy) %>%
      filter(!is.na(net_value_per_employee))
    
    p <- data %>%
      ggplot(aes(x = worker_type, y = net_value_per_employee/1000, fill = worker_type)) +
      geom_col(alpha = 0.8, width = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("Permanent" = "steelblue", "IC" = "darkgreen", "Temporary" = "orange")) +
      scale_y_continuous(labels = label_dollar(suffix = "K")) +
      labs(
        title = paste("Net Value per Employee:", strategy),
        x = "Worker Type",
        y = "Net Value (Thousands)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$strategy_metrics <- renderTable({
    strategy <- input$strategy_choice
    data <- get_strategy_data() %>%
      filter(strategy == !!strategy) %>%
      filter(!is.na(net_value_per_employee))
    
    data %>%
      select(
        "Worker Type" = worker_type,
        "# Employees" = num_employees,
        "Net Value per Employee" = net_value_per_employee
      ) %>%
      mutate(
        "Net Value per Employee" = paste0("$", format(`Net Value per Employee`, big.mark = ","))
      )
  }, striped = TRUE)
  
  output$all_strategies_plot <- renderPlotly({
    data <- get_strategy_data() %>%
      filter(!is.na(workforce_total)) %>%
      group_by(strategy) %>%
      slice(1) %>%
      ungroup()
    
    p <- data %>%
      ggplot(aes(x = reorder(strategy, workforce_total), y = workforce_total/1000)) +
      geom_col(aes(fill = ifelse(workforce_total > 0, "Positive", "Negative")), alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("Positive" = "darkgreen", "Negative" = "darkred")) +
      scale_y_continuous(labels = label_dollar(suffix = "K")) +
      coord_flip() +
      labs(
        title = "Total Workforce Net Value by Strategy",
        subtitle = "For 33-person help desk team",
        x = "Strategy",
        y = "Total Net Value (Thousands)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$all_strategies_summary <- renderTable({
    data <- get_strategy_data() %>%
      filter(!is.na(workforce_total)) %>%
      group_by(strategy) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(desc(workforce_total))
    
    data %>%
      select(
        "Strategy" = strategy,
        "Total Net Value" = workforce_total
      ) %>%
      mutate(
        "Total Net Value" = paste0("$", format(`Total Net Value`, big.mark = ","))
      )
  }, striped = TRUE)
  
  output$strategy_insights <- renderUI({
    if(input$show_all_strategies %% 2 == 0) {
      # Single strategy insights
      strategy <- input$strategy_choice
      data <- get_strategy_data() %>%
        filter(strategy == !!strategy) %>%
        filter(!is.na(workforce_total)) %>%
        slice(1)
      
      total_value <- data$workforce_total
      
      HTML(paste0(
        "<h4>Strategic Analysis: ", strategy, "</h4>",
        "<p><strong>Total Workforce Value:</strong> $", format(total_value, big.mark = ","), "</p>",
        if(total_value > 0) {
          "<p class='positive'>✓ This strategy creates positive value for the organization.</p>"
        } else {
          "<p class='negative'>⚠ This strategy destroys organizational value.</p>"
        },
        "<p>Use the comparison view to see how this strategy ranks against all others.</p>"
      ))
    } else {
      # All strategies insights
      HTML("
      <h4>Key Strategic Insights from Fisher & Connelly (2017):</h4>
      <ul>
        <li><strong>Temp-to-PermCo</strong> provides the highest value ($1.27M) through strategic use of contingent work as extended screening</li>
        <li><strong>NormalCo</strong> (baseline) generates moderate positive value ($229K) with balanced approach</li>
        <li><strong>BlendedCo</strong> shows negative value (-$392K) due to lack of strategic planning</li>
        <li><strong>CoreCompCo strategies</strong> have severe negative impacts due to heavy reliance on low-performing temporary workers</li>
        <li><strong>MegaCo</strong> demonstrates that pure cost-cutting strategies backfire (-$895K)</li>
      </ul>
      <p><strong>Conclusion:</strong> Strategic alignment between worker types and organizational goals is crucial for value creation.</p>
      ")
    }
  })
  
  # Executive summary
  output$executive_summary <- renderUI({
    HTML("
    <div class='info-box'>
      <h4>Key Findings from Fisher & Connelly (2017)</h4>
      <p>This analysis demonstrates that <strong>not all contingent workers are created equal</strong> from a financial perspective:</p>
      <ul>
        <li><strong>Independent contractors (direct hire)</strong> provide the highest net value at $27,176 annually</li>
        <li><strong>Temporary workers</strong> actually create negative value (-$5,093) when turnover costs are included</li>
        <li><strong>Turnover costs are critical</strong> - they can reduce net value by 40-127% depending on worker type</li>
        <li><strong>Service value differences</strong> exist, but cost management is equally important</li>
      </ul>
      <p><strong>Bottom Line:</strong> Organizations should be selective about when and how they use contingent workers, 
      focusing on IC direct arrangements while being cautious about temporary workers.</p>
    </div>
    ")
  })
  
  # Recommendations
  output$recommendations <- renderUI({
    HTML("
    <div class='info-box'>
      <h4>Strategic Recommendations</h4>
      <ol>
        <li><strong>Prioritize IC Direct Arrangements:</strong> Highest ROI with proper contractor management</li>
        <li><strong>Minimize Temporary Worker Usage:</strong> Only for short-term, low-skill needs</li>
        <li><strong>Focus on Turnover Reduction:</strong> Implement retention strategies for all worker types</li>
        <li><strong>Evaluate True Costs:</strong> Include all hidden costs in contingent worker decisions</li>
        <li><strong>Match Worker Type to Need:</strong> Don't use one-size-fits-all approaches</li>
      </ol>
    </div>
    ")
  })
  
  # Risk factors
  output$risk_factors <- renderUI({
    HTML("
    <div class='info-box'>
      <h4>Key Risk Factors</h4>
      <ul>
        <li><strong>Legal Compliance:</strong> IC classification requirements vary by jurisdiction</li>
        <li><strong>Turnover Variability:</strong> Actual turnover may differ significantly from estimates</li>
        <li><strong>Hidden Costs:</strong> Training, coordination, and management overhead</li>
        <li><strong>Quality Consistency:</strong> Performance variation across worker types</li>
        <li><strong>Market Conditions:</strong> Labor availability and wage inflation</li>
      </ul>
    </div>
    ")
  })
  
  # Implementation guide
  output$implementation_guide <- renderUI({
    HTML("
    <div class='info-box'>
      <h4>Implementation Guidelines</h4>
      <h5>Before Implementing:</h5>
      <ul>
        <li>Conduct legal review of contractor classifications</li>
        <li>Assess current turnover costs and patterns</li>
        <li>Evaluate management capabilities for different worker types</li>
        <li>Consider long-term strategic workforce needs</li>
      </ul>
      
      <h5>Best Practices:</h5>
      <ul>
        <li>Start with pilot programs for new worker arrangements</li>
        <li>Develop clear performance metrics and monitoring systems</li>
        <li>Create standardized onboarding for each worker type</li>
        <li>Regularly review and update cost assumptions</li>
        <li>Build contingent worker management expertise</li>
      </ul>
    </div>
    ")
  })
  
  # Report generation
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Contingent_Workers_Analysis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Get current scenario data
      current_scenario <- scenario_data()
      current_baseline <- baseline_data()
      
      # Write Rmd content
      rmd_content <- paste0('
---
title: "Contingent Workers Business Case Analysis"
subtitle: "Based on Fisher & Connelly (2017)"
date: "', format(Sys.Date(), "%B %d, %Y"), '"
output: 
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
geometry: margin=0.75in
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 5)
library(ggplot2)
library(dplyr)
library(scales)
library(knitr)
library(tidyr)

# Recreate data
baseline_data <- tibble::tibble(
  worker_type = c("Permanent employees", "IC, direct", "IC, AOR", "Temporary workers"),
  service_value = c(82955, 105768, 105768, 69683),
  service_costs_no_to = c(63181, 60502, 63518, 50969),
  net_value_no_to = c(19774, 45266, 42250, 18714),
  turnover_costs = c(13243, 18090, 19899, 23837),
  total_service_costs_with_to = c(76424, 78592, 83417, 74806),
  net_value_with_to = c(6532, 27176, 22351, -5093)
)

scenario_data <- tibble::tibble(
  worker_type = c("Permanent", "IC Direct", "IC Agent", "Temporary"),
  service_value = c(', paste(current_scenario$service_value, collapse = ", "), '),
  wage_costs = c(', paste(current_scenario$wage_costs, collapse = ", "), '),
  benefits_costs = c(', paste(current_scenario$benefits_costs, collapse = ", "), '),
  training_costs = c(', paste(current_scenario$training_costs, collapse = ", "), '),
  turnover_costs = c(', paste(current_scenario$turnover_costs, collapse = ", "), '),
  total_costs = c(', paste(current_scenario$total_costs, collapse = ", "), '),
  net_value = c(', paste(current_scenario$net_value, collapse = ", "), ')
)

# Parameters used
temp_wage_mult <- ', input$temp_wage_multiplier, '
ic_wage_mult <- ', input$ic_wage_multiplier, '
turnover_rate <- ', input$turnover_rate, '
temp_benefits <- ', input$temp_benefits_pct, '
ic_benefits <- ', input$ic_benefits_pct, '
```

# Executive Summary

This report analyzes the business case for different types of contingent workers based on the methodology from Fisher & Connelly (2017). The analysis compares permanent employees, independent contractors (direct and agent-hired), and temporary workers across multiple cost and value dimensions.

## Key Findings

Based on your scenario parameters:

```{r key_findings}
# Ensure scenario_data exists and has valid data
if(nrow(scenario_data) > 0 && !all(is.na(scenario_data$net_value))) {
  best_worker <- scenario_data$worker_type[which.max(scenario_data$net_value)]
  worst_worker <- scenario_data$worker_type[which.min(scenario_data$net_value)]
  best_value <- max(scenario_data$net_value, na.rm = TRUE)
  worst_value <- min(scenario_data$net_value, na.rm = TRUE)
  
  cat("• **Highest Net Value:** ", best_worker, " at $", format(round(best_value), big.mark = ","), " annually\\n")
  cat("• **Lowest Net Value:** ", worst_worker, " at $", format(round(worst_value), big.mark = ","), " annually\\n")
  cat("• **Value Difference:** $", format(round(best_value - worst_value), big.mark = ","), " between best and worst options\\n")
  if(abs(worst_value) > 0) {
    cat("• **ROI Impact:** Using optimal worker type provides ", round((best_value - worst_value)/abs(worst_value)*100, 1), "% better returns\\n")
  }
} else {
  cat("• Data processing error - please check scenario parameters\\n")
}
```

# Scenario Parameters

Your analysis used the following parameters:

- **Temporary Worker Wages:** ', input$temp_wage_multiplier, '% of permanent employee wages
- **IC Worker Wages:** ', input$ic_wage_multiplier, '% of permanent employee wages  
- **Annual Turnover Rate:** ', input$turnover_rate, '%
- **Temporary Worker Benefits:** ', input$temp_benefits_pct, '% of wages
- **IC Worker Benefits:** ', input$ic_benefits_pct, '% of wages

# Comparative Analysis

## Net Value Comparison

```{r net_value_chart, fig.cap="Annual Net Value by Worker Type"}
scenario_data %>%
  ggplot(aes(x = reorder(worker_type, net_value), y = net_value/1000)) +
  geom_col(aes(fill = ifelse(net_value > 0, "Positive", "Negative")), alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Positive" = "darkgreen", "Negative" = "darkred")) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  coord_flip() +
  labs(title = "Annual Net Value by Worker Type",
       x = "Worker Type", y = "Net Value (Thousands USD)") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Cost Breakdown Analysis

```{r cost_table}
cost_summary <- scenario_data %>%
  select(worker_type, wage_costs, benefits_costs, training_costs, turnover_costs, total_costs) %>%
  mutate(across(where(is.numeric), ~ paste0("$", format(.x, big.mark = ","))))

kable(cost_summary, 
      col.names = c("Worker Type", "Wages", "Benefits", "Training", "Turnover", "Total Costs"),
      caption = "Annual Cost Breakdown by Worker Type")
```

# Strategic Recommendations

## Primary Recommendations

1. **Optimize Worker Mix:** Based on your scenario, prioritize the highest-value worker arrangements for maximum value creation.

2. **Cost Management:** Focus on turnover reduction strategies, as turnover costs significantly impact all worker types.

3. **Strategic Implementation:** 
   - Start with pilot programs for new arrangements
   - Develop clear performance metrics
   - Regular cost assumption reviews

## Risk Mitigation

- **Legal Compliance:** Ensure proper worker classification
- **Quality Assurance:** Maintain performance standards across worker types  
- **Cost Monitoring:** Track actual vs. projected costs regularly

# Implementation Guidelines

## Phase 1: Assessment (Months 1-2)
- Legal review of worker classifications
- Current cost analysis and benchmarking
- Management capability assessment

## Phase 2: Pilot Program (Months 3-6)  
- Small-scale implementation of optimal worker arrangements
- Performance and cost tracking
- Process refinement

## Phase 3: Scale-Up (Months 7+)
- Gradual expansion based on pilot results
- Full implementation of cost management strategies
- Ongoing optimization

# Appendix: Methodology

This analysis is based on the utility analysis framework from Fisher & Connelly (2017), which compares:

- **Service Value:** Economic value generated by workers
- **Service Costs:** All costs including wages, benefits, training, and turnover
- **Net Value:** Service Value minus Service Costs

The model accounts for turnover costs, which can dramatically impact the business case for different worker arrangements.

## References and Acknowledgments

**Primary Citation:**
Fisher, S. L., & Connelly, C. E. (2017). Lower cost or just lower value? Modeling the organizational costs and benefits of contingent work. *Academy of Management Discoveries*, 3(2), 165-186.

**AI Development Acknowledgments:**
This educational tool was developed with assistance from AI language models and modern development tools:
- **AI Assistant:** Claude (Anthropic) for code development, data analysis, and educational content design
- **Development Environment:** Cursor AI-powered code editor for enhanced productivity and code quality
- **Human Oversight:** All AI-generated content was reviewed, validated, and refined by human researchers
- **Data Verification:** Reproduction analyses and calculations were independently verified against the original publication

AI tools enhanced development efficiency while maintaining strict academic rigor and accuracy in reproducing the original research findings.

**Educational Use Disclaimer:**
This tool is for educational purposes only. All rights to the original research remain with the authors and publishers. Business decisions should consider additional factors beyond those modeled in this analysis.

---

*Report generated on ', format(Sys.Date(), "%B %d, %Y"), ' using scenario parameters specified above.*
')
      
      writeLines(rmd_content, temp_rmd)
      
      # Render PDF
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
