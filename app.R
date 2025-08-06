#Load Libraries
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

#Define Custom Functions

#Expectancy Function
Expectancyfunc <- function(Validity, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut){
  n <- 1000
  mean <- c(0,0)
  lower <- c(PredLowerCut, CritLowerCut)
  upper <- c(PredUpperCut, CritUpperCut)
  corr <- diag(2)
  corr[lower.tri(corr)] <- Validity
  corr[upper.tri(corr)] <- Validity
  jtprob <- pmvnorm(lower, upper, mean, corr, algorithm = Miwa(steps=128))
  xprob <- pnorm(PredUpperCut, mean = 0, sd = 1) - pnorm(PredLowerCut, mean = 0, sd = 1)
  expectancy <- jtprob/xprob
  return(expectancy[1])
}

# Function for formatting axis labels
dollar_format2 <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", round(x/1e6, 1), "M"), 
         ifelse(abs(x) >= 1e3, paste0("$", round(x/1e3, 1), "K"), 
                paste0("$", round(x, 1))))
}

#References
reference_content <- HTML(
  ' <div style="padding-left: 30px; text-indent: -30px;">
    <p>Alexander III, L., Mulfinger, E., & Oswald, F. L. (2019). <i>Investing in People Online</i> (Version 2.0) [Software], Rice University, Houston, Texas. Available from <a  href="https://orgtools.shinyapps.io/IIP3/">https://orgtools.shinyapps.io/IIP3/</a><p>
    <p>Avolio, B. J., Avey, J. B., & Quisenberry, D. (2010). Estimating return on leadership development investment. <i>The Leadership Quarterly, 21</i>(4), 633–644. <a href="https://doi.org/10.1016/j.leaqua.2010.06.006">https://doi.org/10.1016/j.leaqua.2010.06.006</a><p>
    <p>Avolio, B. J., Hannah, S. T., Reichard, R. J., Chan, A., & Walumbwa, F. O. (2009). A meta-analytic review of leadership intervention research: Effects on leader behaviors, attitudes, and effectiveness. <i>The Leadership Quarterly, 20</i>(5), 764–784. <a href="https://doi.org/10.1016/j.leaqua.2009.06.006">https://doi.org/10.1016/j.leaqua.2009.06.006</a><p>
    <p>Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. <i>Journal of Applied Psychology, 83</i>(1), 84–96. <a href="https://doi.org/10.1037/0021-9010.83.1.84">https://doi.org/10.1037/0021-9010.83.1.84</a><p>
    <p>Cascio, W. F., Boudreau, J. W., & Fink, A. A. (2019). <i>Investing in people: financial impact of human resource initiatives</i> (3rd ed.). Society for Human Resource Management.<p>
    <p>Cucina, J., Berger, J., & Busciglio, H. (2017). Communicating Criterion-Related Validity Using Expectancy Charts: A New Approach. <i>Personnel Assessment and Decisions, 3</i>(1). <a href="https://doi.org/10.25035/pad.2017.001">https://doi.org/10.25035/pad.2017.001</a><p>
    <p>Latham, G. P., & Whyte, G. (1994). The Futility Of Utility Analysis. <i>Personnel Psychology, 47</i>(1), 31–46. <a href="https://doi.org/10.1111/j.1744-6570.1994.tb02408.x">https://doi.org/10.1111/j.1744-6570.1994.tb02408.x</a><p>
    <p>Magnusson, K. (2023, June 9). A Causal Inference Perspective on Therapist Effects. <a href="https://doi.org/10.31234/osf.io/f7mvz">https://doi.org/10.31234/osf.io/f7mvz</a><p>
    <p>Schmidt, F. L. (2012). The economic value of goal setting to employers. In E. A. Locke & G. P. Latham (Eds.), <i>New developments in goal setting and task performance</i> (pp. 16–20). Routledge/Taylor & Francis Group. <a href="https://doi.org/10.4324/9780203082744">https://doi.org/10.4324/9780203082744</a><p>
    <p>Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. <i>Journal of Management, 26</i>(2), 281–299. <a href="https://doi.org/10.1177/014920630002600206">https://doi.org/10.1177/014920630002600206</a><p>
    <p>Sturman, M. C. (2012). Employee value: Combining utility analysis with strategic human resource management research to yield strong theory. In N. Schmitt (Ed.), <i>The Oxford handbook of personnel assessment and selection</i> (pp. [insert page numbers if available]). Oxford University Press. <a href="https://doi.org/10.1093/oxfordhb/9780199732579.013.0033">https://doi.org/10.1093/oxfordhb/9780199732579.013.0033</a><p>
    <p>Schmidt, F. L. (2013). The economic value of goal setting to employers. In E. A. Locke & G. P. Latham (Eds.), <i>New Developments in Goal Setting and Task Performance</i> (pp. 16–20). Routledge/Taylor & Francis Group.</p>
    </div>'
)

# Opening UI
opening_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Utility Analysis+ App"),
  mainPanel(
    div(
      h4(HTML("<b>Introduction</b>")),
      p("Welcome to the Utility Analysis+ App (UA+). This tool is designed to translate a complex and sophisticated method—utility analysis—into an accessible format for both pedagogical use and practical application. Its core aim is to help estimate the return on investment (ROI) or economic value of employee performance."),
      p("You can explore the app using the navigation tabs above:"),
      tags$ul(
        tags$li("The Staffing Utility, Comp & Ben Utility, and Training Utility tabs offer tools to calculate the financial impact of various HR interventions."),
        tags$li("These tools also generate reports and visualizations to assist in communicating results to key stakeholders."),
        tags$li(HTML('Research shows that the presentation format significantly affects manager acceptance of utility analysis results <a href="https://doi.org/10.1177/014920630002600206">(Sturman, 2000)</a>.'))
      ),
      p("This app supports evidence-based decision-making by delivering clear, practical, and educational outputs."),
      br(),
      h4(HTML("<b>How Utility Analysis (UA) Works (Based on <a href='https://store.shrm.org/Investing-in-People-Financial-Impact-of-Human-Resource-Initiatives-Third-Edition' target='_blank'>Cascio et al., 2019</a>)</b>")),
      p("Utility analysis estimates ROI for HR initiatives such as staffing, training, development, and compensation by considering:"),
      tags$ul(
        tags$li(HTML("<b>Quantity:</b> Number of affected employees and duration of the intervention.")),
        tags$li(HTML("<b>Quality:</b> Effectiveness of the intervention and the value of improved performance.")),
        tags$li(HTML("<b>Cost:</b> Financial investment required to implement the program."))
      ),
      p("The app operationalizes these principles to deliver actionable and educational insights. Given that utility analysis can be applied to evaluate the economic impact of interventions across every part of the human resource budget, this tool provides published examples focused on different components of that budget, including staffing, training, and compensation."),
      br(),
      h4(HTML("<b>Strategically Managing the Human Resource Budget Using UA</b>")),
      p(HTML("Understanding the human resource budget (HRB) as a finite resource is crucial for strategic HR decision-making. All firms must optimize the allocation of this budget across various functions like recruitment (R), training (T), compensation (C), and administration (A) to effectively execute their strategy.")),
      p(HTML("The core insight from <a href='https://www.amazon.com/Financial-Analysis-HR-Managers-paperback/dp/0133925420' target='_blank'>Director (2012)</a> is that money can be moved around the budget to accomplish a firm's goals. For example if unanticipated staffing expenses arise, then money must be pulled from the compensation, training, and administrative portions of the HR budget to fund unanticipated staffing expenses. This underscores how evidence-based methods, such as utility analysis, can inform more effective and efficient allocations of the firm's budget, thereby helping the HR team contribute to the firm's strategic goals.")),
      br(),
      h4(HTML("\U0001F4C2 Code Repository:")),
      p(HTML("Access the app's code on GitHub:<br><a href='https://github.com/utilityanalysis/webApp'>https://github.com/utilityanalysis/webApp</a>"))
    )
  )
)

#Reference UI
reference_ui <- fluidPage(
  titlePanel("References"),
  mainPanel(
    reference_content
  )
)

#Glossary UI
glossary_ui <- fluidPage(
  tags$style(HTML(".size {font-size: 16px}")),
  titlePanel("Glossary"),
  mainPanel(
    p(HTML("<strong>Break-Even SDy</strong> - Minimum possible value of SDy to recoup losses from conducting a staffing/training program."), class = "size"),
    p(HTML("<strong>Cohen's D</strong> - A type of effect size. This value represents the standardized difference in production between a trained and untrained group."), class = "size"),
    p(HTML("<strong>Discount Rate</strong> - Rate of accrual of monetary investments from production revenues."), class = "size"),
    p(HTML("<strong>Effect Size</strong> - Standard Deviation of the performance difference between a treated group and an untreated group."), class = "size"),
    p(HTML("<strong>Expectancy</strong> - A measure of the ability of a test to predict outcomes. For example, a test with high validity has a high expectancy of predicting high performance based on a high test score."), class = "size"),
    p(HTML("<strong>SDp</strong> - Standard deviation of work output among employees as a percentage of mean output."), class = "size"),
    p(HTML("<strong>SDy</strong> - Monetary value of job performance. This value indicates the difference between an average worker (50th percentile) and a worker with high production (85th percentile). Methods for measuring SDy include the 40% rule, Global Estimation, and CREPID among others."), class = "size"),
    p(HTML("<strong>Selection ratio</strong> - Ratio of the number of employees to be hired from an applicant pool."), class = "size"),
    p(HTML("<strong>Tax Rate</strong> - Company tax rate."), class = "size"),
    p(HTML("<strong>Utility</strong> - A measure of the return on investment in workers. Utility compares the quality and quantity of a treated group of employees to the cost of the treatment. This returns an estimation of the economic value that these employees add to the company."), class = "size"),
    p(HTML("<strong>Validity</strong> - Correlation between selection test performance and actual job performance. A validity of 0 indicates no relationship, while a validity of 1.00 indicates a perfect relationship. Validity will almost always be below .50 (Carson et. al. (1998))."), class = "size"),
    p(HTML("<strong>Variable Costs</strong> - Costs that rise with greater production. For example, material costs, incentives, commission, variable overhead, etc."), class = "size")
  )
)

#Staffing UI
main_ui <- dashboardPage(
  
  dashboardHeader(title = "Staffing Utility"),
  
  #sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Staffing Utility", tabName = "staffing_utility", icon = icon("file-alt")),
      menuItem("Effect Size (Expectancy Chart)", tabName = "expectancy", icon = icon("dashboard")),
      menuItem("Utility Analysis (UA)", tabName = "utility_adjustments", icon = icon("sliders-h")),
      menuItem("Monte Carlo Analysis of UA", tabName = "monte", icon = icon("magnifying-glass-chart"))
    )
  ),
  
  #Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .custom-split-layout {
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .custom-split-layout > * {
        margin-right: 10px; /* Adjust the margin as needed */
        }
        body, .content-wrapper, .main-sidebar, .right-side {
          height: 100vh !important;
          overflow-y: auto;
        }
      "))
    ),
    
    #Tab 1
    tabItems(
      tabItem(tabName = "staffing_utility",
        fluidPage(
          titlePanel("Staffing Utility"),
          mainPanel(
            p(HTML("This is the staffing utility section of the UA+ app. Staffing utility can be described as a return on investment on improved staffing procedures. It measures this return on investment by comparing the quantity and quality of workers chosen by the staffing procedure to the cost of the procedure. On the left panel, there is a tab for expectancy. Expectancy shows the relationship between predictor scores (in this case validity), and criterion scores such as job performance (<a href='https://doi.org/10.25035/pad.2017.001' target='_blank'>Cucina et al., 2017</a>). The Expectancy tab produces an expectancy chart based on the work of Cucina et al. This chart compares the expected job performance of hires based on their score from staffing procedures. The default values provided are from Latham & Whyte (<a href='https://doi.org/10.1111/j.1744-6570.1994.tb02408.x' target='_blank'>Latham & Whyte, 1994</a>).")),
            br(),
            p(HTML("The Utility Output tab is where values for utility are displayed. Along with these values is a plain text description modeled after Carson et al.'s descriptions of utility that were found to be effective when presented to managers (<a href='https://doi.org/10.1037/0021-9010.83.1.84' target='_blank'>Carson et al., 1998</a>). This tab also includes utility adjustments for economic factors and workflows. Economic factor defaults provided are from Sturman (<a href='https://doi.org/10.1177/014920630002600206' target='_blank'>Sturman, 2000</a>) while workflow defaults are from Cascio et al. (<a href='https://www.worldcat.org/title/1105756172' target='_blank'>Cascio et al., 2019</a>)."))
                  
          )
        )
      ),
      #Tab 2
      tabItem(tabName = "expectancy",
        fluidPage(
          useShinyjs(),
          titlePanel("Expectancy of High Job Performance"),
          sidebarLayout(
            sidebarPanel(
              numericInput("rxy1", "Validity of old procedure(s)/program(s):", 0.1, min = -1, max = 1, step = 0.01),
              numericInput("rxy2", "Validity of new procedure(s)/program(s):", 0.5, min = -1, max = 1, step = 0.01),
              actionButton("go", "Generate Chart")
            ),
            mainPanel(
              plotOutput("utility_graph"),
              textOutput("expectancy_text"),
              downloadButton("download_plot", "Export PDF")
            )
          )
        )
      ),
      
      #Tab 3
      tabItem(tabName = "utility_adjustments",
        fluidPage(
          useShinyjs(),
          titlePanel("Predicting Returns on Improved Staffing Procedures"),
          sidebarLayout(
            sidebarPanel(
              tabsetPanel(
                id = "utilityPages",
                tabPanel("Page 1",
                  HTML('<h3><b>Unadjusted Utility</b></h3>'),
                  numericInput("n", "Number of hires:", 470, min = 1, step = 1),
                  numericInput("sdy", "SD of performance in monetary units:", 16290, min = 0.01, step = 1),
                  numericInput("sr", "Selection ratio:", 0.33, min = 0, max = 1, step = 0.01),
                  numericInput("cost1", "Cost per applicant of old procedure:", 200, min = 0.01, step = 1),
                  numericInput("cost2", "Cost per applicant of new procedure:", 304.33, min = 0.01, step = 1),
                  numericInput("period", "Anticipated tenure of selected employees:", 18, min = 0.01, step = 1),
                  actionButton("go", "Compute Utility")
                ),
                tabPanel("Page 2",
                  HTML('<h3><b>Capital Budgeting Considerations</b></h3>'),
                  numericInput("vcost", "Variable Costs (%):", 35, min = 0, max = 100, step = 1),
                  numericInput("tax", "Tax Rate (%):", 63, min = 0, max = 100, step = 1),
                  numericInput("drate", "Discount Rate (%):", 11, min = 0, max = 100, step = 1),
                  actionButton("go2", "Compute Financially Adjusted Utility")
                ),
                tabPanel("Page 3",
                  HTML('<h3><b>Employee Flows</b></h3>'),
                  numericInput("pyears" , "Program Length" , 15, min = 1, step = 1),
                  numericInput("nadd" , "Employees Added per Year:" , 470, min = 0, step = 1),
                  numericInput("nsub" , "Employees Lost per Year(After Tenure):", 470, min = 0, step = 1),
                  actionButton("go_flows", "Account for Flows")
                )
              )
            ),
            mainPanel(
                    # Outputs for Page 1
              uiOutput("unadjusted_utility_label"),
              uiOutput("unadjusted_utility_value"),
              uiOutput("unadjusted_utility_explanation"),
              uiOutput("unadjusted_utility_math"),
                    # Outputs for Page 2
              uiOutput("adjusted_utility_label"),
              uiOutput("adjusted_utility_value"),
              uiOutput("adjusted_utility_explanation"),
              uiOutput("adjusted_utility_math"),
                    uiOutput("final_adjusted_utility_output"), # New output for final adjusted value
                    # Outputs for Page 3   
              uiOutput("flows_utility_label"),
              uiOutput("flows_utility_value"),
              uiOutput("flows_utility_explanation"),
              uiOutput("flows_utility_math"),
              downloadButton("download_pdf", "Export Report")
            )
          )
      
      ),
      
      tabItem(tabName = "monte",
          titlePanel("Monte Carlo Analysis"),
          sidebarPanel(
            width = 5,
            tabsetPanel(
              id = "tabs",
              tabPanel(
                "Page 1",
                fluidRow(
                  splitLayout(
                    numericInput("t1", "Min Tenure", 1, min = 1),
                    numericInput("t2", "Max Tenure", 10, min = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("n1", "Min Hires", 1, min = 1),
                    numericInput("n2", "Max Hires", 1100, min = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("sel1", "Min Applicants/Hire", 1, min = 1),
                    numericInput("sel2", "Max Applicants/Hire", 50, min = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("r1", "Min Validity", .1, min = -1, max = 1),
                    numericInput("r2", "Max Validity", .77, min = -1, max = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("sdy1", "Min SDy", 4000, min = 0),
                    numericInput("sdy2", "Max SDy", 40000, min = 0),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("costm1", "Min Cost/Applicant", 0, min = 0),
                    numericInput("costm2", "Max Cost/Applicant", 1100, min = 0),
                    class = "custom-split-layout"
                  )
                )
              ),
              tabPanel(
                "Page 2",
                fluidRow(
                  splitLayout(
                    numericInput("i1", "Min Discount Rate", .01, min = 0),
                    numericInput("i2", "Max Discount Rate", .11, min = 0),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("vc1", "Min Variable Costs", .02, min = 0),
                    numericInput("vc2", "Max Variable Costs", .35, min = 0),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("tax1", "Min Tax Rate", .30, min = 0),
                    numericInput("tax2", "Max Tax Rate", .63, min = 0),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("ir1", "Min Validity Change", .05, min = -1, max = 1),
                    numericInput("ir2", "Max Validity Change", .38, min = -1, max = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("pa1", "Min Initial Acceptance", .2, min = 0, max = 1),
                    numericInput("pa2", "Max Initial Acceptance", .7, min = 0, max = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("bxy1", "Min Acceptance-Productivy Correlation", -.5, min = -1, max = 1),
                    numericInput("bxy2", "Max Acceptance-Productivy Correlation", 0, min = -1, max = 1),
                    class = "custom-split-layout"
                  )
                )
              ),
              tabPanel(
                "Page 3",
                fluidRow(
                  splitLayout(
                    numericInput("stab1", "Min Performance Stability", .5, min = 0, max = 1),
                    numericInput("stab2", "Max Performance Stability", 1, min = 0, max = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("rc1", "Min Cutoff Score", -2),
                    numericInput("rc2", "Max Cutoff Score", 0),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("pto1", "Min Turnover Probability", 0, min = 0, max = 1),
                    numericInput("pto2", "Max Turnover Probability", 0.33, min = 0, max = 1),
                    class = "custom-split-layout"
                  )
                ),
                fluidRow(
                  splitLayout(
                    numericInput("corrto1", "Min Turnover-Performance Correlation", 0, min = -1, max = 1),
                    numericInput("corrto2", "Max Turnover-Performance Correlation", .5, min = -1, max = 1),
                    class = "custom-split-layout"
                  )
                ),
                numericInput("seed", "Set Seed for Reproducibility", 12845, min = 1),
                actionButton("monteGo", "Run Monte Carlo Analysis")
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              textOutput("medianmonte"),
              textOutput("negmonte"),
              box(
                title = "Density Plot",
                status = "primary",
                solidHeader = TRUE,
                width = 14,
                height = 6,
                plotlyOutput("density")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 9,
                      title= "Adjustment Effects",
                tableOutput("sturman2"),
                      status = "primary" 
                    )
              )
          )
      )
    ),

#Training UI
training_ui <- dashboardPage(
  dashboardHeader(title = "Training Utility"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarTrain",
      selected = "training_utility",
      menuItem("Training Utility", tabName = "training_utility", icon = icon("file-lines")),
      menuItem("Effect Size (BESD)", tabName = "effect_size", icon = icon("chart-simple")),
      menuItem("Utility Outputs", tabName = "utility_outputs", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "training_utility",
              fluidPage(
                titlePanel("Training Utility"),
                mainPanel(
                  p("This is the training utility section of the UA+ app. Training utility can be described as a return on investment in the development of workers. This return on development investment (RODI) is calculated using the effect size of the training process. In the panel on the left, there is a tab for effect size that gives a descriptive analysis of the effect size that you choose to input. This consists of a modified graph from Alexander et al's Investing in People Online app, as well as a modified version of Magnusson's description of effect size. This tool is useful for displaying how effect size relates to the productivity of trained versus untrained workers."),
                br(),
                  p("Treated employees are those who have participated in a particular program or intervention."),
                  p("The Utility Outputs tab displays the results of utility analysis as well as a plain text description. This utility analysis takes into account the quality and quantity of the training program and workers and compares this value to the cost of the program. It additionally takes into account economic factors such as variable costs, taxes, and investments.")
                )
              )
      ),
      tabItem(tabName = "effect_size",
              
              fluidPage(
                useShinyjs(),
                titlePanel("Effect Size Description"),
                sidebarLayout(
                  sidebarPanel(
                    id = "sidebar",
                    HTML('<h3>Effect Size Inputs</h3>'),
                    selectInput(
                      "effect_size_dropdown",
                      label = HTML('<b>Effect Size Dropdown Options</b>'),
                      choices = list(
                        "Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)" = 0.35,
                        "Leadership development: Average estimate (Avoilio et al., 2009)" = 0.65,
                        "Leadership development: Intensive program (3+ days) (Avoilio et al., 2009)" = 1.00,
                        "Leadership development: Upper bound (Avoilio et al., 2009)" = 1.37,
                        "Goal setting (Schmidt, 2013)" = 0.46
                      ),
                      selected = 0.35
                    ),
                    numericInput("dTrain2_1", "Input the Effect Size (Cohen's d) Manually", 0),
                    br(),
                    actionButton("goEffect", "Generate Chart")
                    
                  ),
                  mainPanel(
                    
                    plotOutput("training_graph_1"),
                    br(),
                    textOutput("effectText"),
                    downloadButton("effect_download", "Download PDF")
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
                      id = "pageTabs",
                      tabPanel("Page 1",
                               HTML('<h3>Utility Inputs (Page 1)</h3>'),
                               selectInput(
                                 "effect_size_dropdown_utility",
                                 label = HTML('<b>Effect Size Dropdown Options</b>'),
                                 choices = list(
                                   "Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)" = 0.35,
                                   "Leadership development: Average estimate (Avoilio et al., 2009)" = 0.65,
                                   "Leadership development: Intensive program (3+ days) (Avoilio et al., 2009)" = 1.00,
                                   "Leadership development: Upper bound (Avoilio et al., 2009)" = 1.37,
                                   "Goal setting (Schmidt, 2013)" = 0.46
                                 ),
                                 selected = 0.35
                               ),
                               numericInput("dTrain2", "Input the Effect Size (Cohen's d) Manually", 0),
                               numericInput("nTrain", "Number of Employees Trained", 0, min = 0, step = 1),
                               numericInput("tTrain", "Training Effect Duration", 0, min = 0),
                               numericInput("sdyTrain", "SD of Performance in Monetary Units", 0),
                               numericInput("sdP", "SD of Work Output as % of Mean Output", 20, min = 0, max = 100),
                               numericInput("costTrain2", "Cost per Employee of Procedure", 0, min = 0),
                               br(),
                               actionButton("goUn", "Compute Utility(No Adjustments)")
                      ),
                      tabPanel("Page 2",
                               HTML('<h3>Utility Inputs (Page 2)</h3>'),
                               HTML('<h4>Economic Factors</h4>'),
                               numericInput("vrateTrain", "Variable Costs (%)", 35, min = 0, max = 100),
                               numericInput("taxTrain", "Tax Rate (%)", 63, min = 0, max = 100),
                               numericInput("discTrain", "Discount Rate (%)", 11, min = 0, max = 100),
                               HTML('<h4>Employee Flows</h4>'),
                               numericInput("lengthTrain", "Program Length", 15, min = 0),
                               numericInput("addTrain", "Employees Trained per Year", 618, min = 0),
                               numericInput("subTrain", "Loss of Trained Employees per Year (After Effect Duration)", 618, min = 0),
                               actionButton("go4", "Compute Utility")
                      )
                    )
                  ),
                  mainPanel(
                    uiOutput("h3"),
                    textOutput("training_utilityUn"),
                    textOutput("training_utilityOpCost"),
                    textOutput("break_even_train"),
                    uiOutput("h4"),
                    textOutput("training_utility"),
                    textOutput("training_utility_per_employee"),
                    textOutput("training_utility_per_employee_per_year"),
                    br(),
                    textOutput("trainingText1"),
                    br(),
                    textOutput("trainingText2"),
                    br(),
                    textOutput("trainingText3"),
                    downloadButton("training_download", "Export Report")
                  )
                )
              )
      )
    )
  )
),

#Server
main_server <- function(input, output, session) {
  # ... (rest of the code remains unchanged)
}

# Run the application
shinyApp(ui = main_ui, server = main_server)

#Server
main_server <- function(input, output, session) {
  # Utility formatting function available everywhere in server
  format_utility <- function(x) {
    paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
  }
  #global variables
  utilityUn <- reactiveVal()
  utilityUnPer <- reactiveVal()
  utilityUnPerYear <- reactiveVal()
  utilityAdj <- reactiveVal()
  utilityAdjPer <- reactiveVal()
  utilityAdjPerYear <- reactiveVal()
  staffSDy <- reactiveVal()
  costAdj <- reactiveVal()
  elo <- reactiveVal()
  elmo <- reactiveVal()
  eumo <- reactiveVal()
  eto <- reactiveVal()
  eln <- reactiveVal()
  elmn <- reactiveVal()
  eumn <- reactiveVal()
  etn <- reactiveVal()
  bh <- reactiveVal()
  gh <- reactiveVal()
  expPro <- reactiveVal()
  expPro2 <- reactiveVal()
  expDiff <- reactiveVal()
  expDiff2 <- reactiveVal()
  selectedPage <- reactiveVal(1)
  
  #Toggles
  observe({ 
    #Staffing Toggle 1
    toggleState(
      id = "go",
      condition = 
        input$n > 0 &
        input$sdy >= 0 &
        -1 <= input$rxy1  &
        1 >= input$rxy1 &
        -1 <= input$rxy2  &
        1 >= input$rxy2 &
        input$sr > 0 &
        input$cost1 >= 0 &
        input$cost2 >= 0 &
        input$period > 0
    )
    #Staffing Toggle 2
    toggleState(
      id = "go2",
      condition =
        input$vcost >= 0 &
        input$vcost <= 100 &
        input$tax >= 0 &
        input$tax <= 100 &
        input$drate >= 0 &
        input$drate <= 100 &
        input$pyears >0 &
        input$nadd >= 0 &
        input$nsub >= 0
    )
    #Training Toggle 1
    toggleState(
      id = "goEffect",
      condition =
        input$nTrain_1 > 0 &
        input$tTrain_1 > 0 &
        input$sdyTrain_1 > 0 &
        input$costTrain2_1 >= 0
    )
    toggleState(
      id = "monteGo",
      condition =
        input$t1 >= 1 &
        input$t2 >= 1 &
        input$n1 >= 1 &
        input$n2 >= 1 &
        input$sel1 >= 1 &
        input$sel2 >= 1 &
        input$r1 >= -1 &
        input$r1 <= 1 &
        input$r2 >= -1 &
        input$r2 <= 1 &
        input$sdy1 >= 0 &
        input$sdy2 >= 0 &
        input$costm1 >= 0 &
        input$costm2 >= 0 &
        input$i1 >= 0 &
        input$i2 >= 0 &
        input$vc1 >= 0 &
        input$vc2 >= 0 &
        input$tax1 >= 0 &
        input$tax2 >= 0 &
        input$ir1 >= -1 &
        input$ir1 <= 1 &
        input$ir2 >= -1 &
        input$ir2 <= 1 &
        input$pa1 >= 0 &
        input$pa1 <= 1 &
        input$pa2 >= 0 &
        input$pa2 <= 1 &
        input$bxy1 >= -1 &
        input$bxy1 <= 1 &
        input$bxy2 >= -1 &
        input$bxy2 <= 1 &
        input$stab1 >= 0 &
        input$stab1 <= 1 &
        input$stab2 >= 0 &
        input$stab2 <= 1 &
        input$pto1 >= 0 &
        input$pto1 <= 1 &
        input$pto2 >= 0 &
        input$pto2 <= 1 &
        input$corrto1 >= -1 &
        input$corrto1 <= 1 &
        input$corrto2 >= -1 &
        input$corrto2 <= 1
    )
    
    if (!isTruthy(input$go)) {
      disable("download_plot")
    }
    if (!isTruthy(input$go2)) {
      disable("download_pdf")
    }
    if (!isTruthy(input$goEffect)) {
      disable("effect_download")
    }
    if (!isTruthy(input$go4)) {
      disable("training_download")
    }
    if (!isTruthy(input$goSchmidt)) {
      disable("sdP")
    }
  })
  #Staffing Utility  
  observeEvent(input$go, {
    enable("download_pdf")
    n <- input$n
    period <- input$period
    # Utility Calculation (Unadjusted)
    utility_value <- utilityBcg(
      n = (input$n),
      sdy = input$sdy,
      rxy = (input$rxy2 - input$rxy1),
      uxs = ux(input$sr),
      sr = input$sr,
      pux = NULL,
      cost = ((input$cost2 - input$cost1) * (input$n / input$sr)),
      period = input$period
    )
    # Compute Break Even SDy
    breakEvenSDy <- (
      ((input$cost2)*(input$n/input$sr))/(input$n*(input$rxy2)*input$period*ux(input$sr))
    )
    
    # Compute old top expectancy
    expectancyTopOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+.67 , Inf, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute old upper middle expectancy
    expectancyUMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #Compute old lower middle expectancy
    expectancyLMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #compute old lower expectancy
    expectancyLowOld <- 100*round(Expectancyfunc(input$rxy1, -Inf , ux(input$sr)-.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute new top expectancy
    expectancyTopNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+.67 , Inf, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    # Compute new upper middle expectancy
    expectancyUMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #Compute new lower middle expectancy
    expectancyLMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #compute new lower expectancy
    expectancyLowNew <- 100*round(Expectancyfunc(input$rxy2, -Inf , ux(input$sr)-.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #Calculate increase in good hires/ decrease in bad hires
    badHire <- round(((expectancyLowOld-expectancyLowNew)/expectancyLowOld)*100, 0)
    goodHire <- round(((expectancyTopNew-expectancyTopOld)/expectancyTopOld)*100, 0)
    bh(badHire)
    gh(goodHire)
    
    #assign local variables to global variables
    elo(expectancyLowOld)
    elmo(expectancyLMOld)
    eumo(expectancyUMOld)
    eto(expectancyTopOld)
    eln(expectancyLowNew)
    elmn(expectancyLMNew)
    eumn(expectancyUMNew)
    etn(expectancyTopNew)
    
    # Calculate the per-hire utility
    per_hire_utility <- utility_value / n
    
    # Calculate the per-hire per-year utility
    per_year_utility <- per_hire_utility / period
    
    # Format
    formatted_total_utility <- format_utility(utility_value)
    utilityUn(formatted_total_utility)
    formatted_per_hire_utility <- format_utility(per_hire_utility)
    utilityUnPer(formatted_per_hire_utility)
    formatted_per_year_utility <- format_utility(per_year_utility)
    utilityUnPerYear(formatted_per_year_utility)
    formatted_breakEvenSDy <- format_utility(breakEvenSDy)
    staffSDy(formatted_breakEvenSDy)
    
    #If else for procedure with better results
    exp_diff <- if (expectancyTopNew > expectancyTopOld) {
      expectancyTopNew - expectancyTopOld
    }
    else {
      expectancyTopOld - expectancyTopNew
    }
    expDiff(exp_diff)
    exp_diff2 <- if (expectancyLowNew > expectancyLowOld) {
      expectancyTopNew - expectancyTopOld
    }
    else {
      expectancyLowOld - expectancyLowNew
    }
    expDiff2(exp_diff2)
    exp_procedure <- if (expectancyTopNew > expectancyTopOld) {
      "new procedure"
    }
    else {
      "old procedure"
    }
    expPro(exp_procedure)
    exp_procedure2 <- if (expectancyLowNew > expectancyLowOld) {
      "increases"
    }
    else {
      "decreases"
    }
    expPro2 <- exp_procedure2
    
    #expectancy chart
    
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),  # Specify the order of levels
        Probability = c(
          expectancyLowOld, expectancyLowNew,
          expectancyLMOld, expectancyLMNew,
          expectancyUMOld, expectancyUMNew,
          expectancyTopOld, expectancyTopNew
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      improvement <- goodHire  # percent improvement
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "Quartile Score on Selection Procedure(s)/Program(s)",
          y = "Probability of High Job Performance",
          fill = "Procedure(s)/Program(s)",
          title = "Figure 1. Comparing old and new procedure(s)/program(s)",
          subtitle = paste0("The new procedure(s)/program(s) improves prediction of high performance by ", improvement, "%.")
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#6BAED6", "New" = "#2171B5")) +
        theme_minimal() +  
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
          plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 10)),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = .5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          axis.line.x = element_line(size = 1, colour = 'black'),
          axis.line.y = element_line(size = 1, colour = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 6, position = position_dodge(width = 0.9)))
    })
    output$utility_graph <- renderPlot({
      plot_data()
    })
    
    #Plain Text
    output$expectancy_text <- renderText({
      paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", goodHire, "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", badHire, "% [(",elo(), " - ",eln(), ")/", elo() ,"].")
    })
    
    #Export PDF
    output$download_plot <- downloadHandler(
      filename = function() {
        "expectancy.pdf"  
      },
      
      content = function(file) {
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", goodHire, "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", badHire, "% [(",elo(), " - ",eln(), ")/", elo() ,"]."), width = 90, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 14
            ),
            plot.margin = margin(1,1,1,1, "in")
          )
        
        
        ggsave(
          file,
          plot,
          device = "pdf",
          width = 10.625,  
          height = 13.75,  
          units = "in",  
          dpi = 300  
        )
      }
    )
    
    # Unadjusted utility outputs
    output$unadjusted_utility_label <- renderUI({
      HTML("<b>Unadjusted Utility Estimate</b>")
    })
    output$unadjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityUn(), "</span>"))
    })
    output$unadjusted_utility_explanation <- renderUI({
      HTML(
        "The unadjusted utility estimate represents the total net financial benefit of using the new staffing procedure, before accounting for taxes, variable costs, or other economic adjustments. It is calculated as the increase in value from improved selection minus the additional costs of the new procedure. This value helps you understand the raw financial impact of switching to the new procedure, and is useful for comparing the basic ROI of different staffing strategies before considering real-world financial factors."
      )
    })
    # Underlying Math with Unicode/plaintext formatting
    n <- input$n
    sdy <- input$sdy
    rxy1 <- input$rxy1
    rxy2 <- input$rxy2
    sr <- input$sr
    cost1 <- input$cost1
    cost2 <- input$cost2
    period <- input$period
    delta_rxy <- rxy2 - rxy1
    uxs <- ux(sr)
    cost_diff <- (cost2 - cost1) * (n / sr)
    final_utility <- n * sdy * delta_rxy * uxs * period - cost_diff
    utility_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Utility = n × sdy × (rxy₂ - rxy₁) × ux(sr) × period - (cost₂ - cost₁) × (n / sr)",
      "\n\nWhere:",
      "n = ", n, ", sdy = ", sdy, ", rxy₂ = ", rxy2, ", rxy₁ = ", rxy1, ", ux(sr) = ", round(uxs, 4), ", period = ", period, ", cost₂ = ", cost2, ", cost₁ = ", cost1, ", sr = ", sr,
      "\n\nStep-by-step:",
      "\n1. Difference in validity: rxy₂ - rxy₁ = ", delta_rxy,
      "\n2. Cost difference: (cost₂ - cost₁) × (n / sr) = ", round(cost_diff, 2),
      "\n3. Utility = ", n, " × ", sdy, " × ", delta_rxy, " × ", round(uxs, 4), " × ", period, " - ", round(cost_diff, 2),
      "\n4. Final Utility = ", format_utility(final_utility),
      "</pre>"
    )
    output$unadjusted_utility_math <- renderUI({
      HTML(utility_formula)
    })
  })
  
  #Utility Tab
  observeEvent(input$go2,{
    enable("download_pdf")
    varCosts <- -input$vcost/100
    tax <- input$tax
    disc <- input$drate
    costOrd <- input$cost1
    costAc <- input$cost2
    validOrd <- input$rxy1
    validAc <- input$rxy2
    SDjp <- input$sdy
    sr1  <- input$sr
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- input$period
    last <- input$pyears
    add <- input$nadd
    subt <- input$nsub
    
    discProp <- disc / 100
    valid <- validAc - validOrd
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
    numyr <- tenure1 + last
    
    nk <- 0
    totDelta <- 0
    totDelta1 <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    adjusted_utility <- totDelta
    adjusted_utility_value <- adjusted_utility
    adjusted_utility_perHire <- totDelta / (last*add)
    adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
    
    # Format the utility values
    formatted_adjusted_utility <- format_utility(adjusted_utility)
    formatted_adjusted_per_hire_utility <- format_utility(adjusted_utility_perHire)
    formatted_adjusted_per_year_utility <- format_utility(adjusted_utility_perHire_perYear)
    formatted_cost_adjustment <- format_utility(totDelta1/last)
    
    output$h4 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')
    })
    
    output$adjusted_utility <- renderText({
      paste("Failing to use this program will have total opportunity costs to the company of:", 
            formatted_adjusted_utility)
    })
    
    output$adjusted_utility_perHire <- renderText({
      paste("Failing to use this program will have opportunity costs to the company of:", 
            formatted_adjusted_per_hire_utility,
            "per hire.")
    })
    
    output$adjusted_utility_perHire_perYear <- renderText({
      paste("Failing to use this program will have opportunity costs to the company of:", 
            formatted_adjusted_per_year_utility,
            "per hire per year they stay.")
    })
    
    output$cost_adjustment <- renderText({
      paste("The cost adjustment factor is:", formatted_cost_adjustment)
    })
    
    # Display
    
    output$h1 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')
    })
    
    output$h2 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')
    })
    
    output$total_utility <- renderText({
      paste("The opportunity costs of failing to use the program are:", 
            utilityUn())
    })
    output$break_even_SDy <- renderText({
      paste("The break-even value for Standard Deviation of the new procedure is:", 
            staffSDy())
    })
    
    #If Else for larger utility
    lowHigh <- if(input$rxy1 > input$rxy2) {
      "old"
    }
    else{
      "new"
    }
    ls <- if(input$rxy1 > input$rxy2) {
      "smaller"
    }
    else{
      "larger"
    }
    #Plain text
    staff1 <- paste0("In order to assess the effectiveness of new staffing procedures, we have conducted an analysis comparing the quality and quantity of new hires to the cost of the selection processes. As part of this analysis, we have found the validity for each of the staffing procedures. Our research indicates that the new procedure will have an operational validity of ", input$rxy2, ", which is ", ls, " than the operational validity of ", input$rxy1, " defining the old procedure. This means that the ", lowHigh, " procedure more accurately avoids bad hires.")
    staff2 <- paste0("Our cost analysis found that costs associated with administering the new procedure are $", input$cost2, " per applicant, in comparison to the cost of the old procedure which is $", input$cost1, " per applicant. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of $", input$sdy, ". The first is a variable cost adjustment and refers to costs that increase with hiring higher quality talent, such as compensation enhancements. These are expected to offset opportunity costs associated with this program by ", input$vcost, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$tax, "%. Lastly, the cash value of increased performance over time must be discounted to the present to approximate the net present value of the program. The discount rate applied to this program is ", input$drate, "%. Since we will hire ", input$nadd, " new employees from a large selection pool each year, and the new procedure is expected to be used for at least ", input$pyears," years, implementing the new program is expected to cost our company ", costAdj(), " per year.")
    staff3 <- paste0("We found that opportunity cost is also affected by employee flows. When employees selected through the program enter and exit our company, the costs and benefits associated with the program change over time. Our research shows that employees selected using this program are expected to stay with the company an average of ", input$period, " years. This means that when we do not hire using this procedure, the opportunity cost associated with each employee is ", utilityAdjPer(), " or ", utilityAdjPerYear(), " per year they stay. Considering this along with all previous factors, our analysis has found that failing to implement this program will result in a total opportunity cost of ", utilityAdj(), ".")
    
    s1wrap <- strwrap(staff1, width = 97, indent = 8, simplify = TRUE)
    s1wrap <- paste(s1wrap, collapse = "\n")
    s2wrap <- strwrap(staff2, width = 97, indent = 8, simplify = TRUE)
    s2wrap <- paste(s2wrap, collapse = "\n")
    s3wrap <- strwrap(staff3, width = 97, indent = 8, simplify = TRUE)
    s3wrap <- paste(s3wrap, collapse = "\n")
    
    output$plainText1 <- renderText({
      staff1
    })
    output$plainText2 <- renderText({
      staff2
    })
    output$plainText3 <- renderText({
      staff3
    })
    
    #assign global variables to local variables
    elo1 <- elo()
    eln1 <- eln()
    elmo1 <- elmo()
    elmn1 <- elmn()
    eumo1 <- eumo()
    eumn1 <- eumn()
    eto1 <- eto()
    etn1 <- etn()
    
    #create expectancy chart used in pdf export
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),
        Probability = c(
          elo1, eln1,
          elmo1, elmn1,
          eumo1, eumn1,
          eto1, etn1
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      title_text <- paste0("Using the ", expPro(), " improves the probability of acquiring a high performer by ", gh(), " percent.")
      wrapped_title <- strwrap(title_text, width = 100, simplify = TRUE)
      wrapped_title <- paste(wrapped_title, collapse = "\n")
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "Quartile Score on Selection Procedure(s)/Program(s)",
          y = "Probability of High Job Performance",
          fill = "Procedure(s)/Program(s)"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#6BAED6", "New" = "#2171B5")) +
        theme_minimal() +  # Customize the plot theme (optional)
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 12, lineheight = 1.2),
          axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = .5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          axis.line.x = element_line(size = 1, colour = 'black'),
          axis.line.y = element_line(size = 1, colour = 'black'),
          panel.grid.minor = element_blank()
        ) +
        ggtitle(paste0("Figure 1\n", wrapped_title))
      
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 6, position = position_dodge(width = 0.9)))
    })
    
    #pdf export
    output$download_pdf <- downloadHandler(
      filename = function() {
        "staffing_report.pdf"
      },
      
      content = function(file) {
        
        title <- textGrob(
          "Staffing Report",
          x = 0.5, y = unit(0.5, "in"),
          gp = gpar(fontsize = 20),
        )
        
        par1 <- textGrob(
          s1wrap,
          x = unit(1, "in"), y = 0.62,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        par2 <- textGrob(
          s2wrap,
          x = unit(1, "in"), y = -0.02,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        par3 <- textGrob(
          s3wrap,
          x = unit(1, "in"), y = -0.76,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", gh(), "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", bh(), "% [(",elo(), " - ",eln(), ")/", elo() ,"]."), width = 100, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        # Create the ggplot2 plot with a caption
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 12
            ),
            plot.margin = margin(2.25,1,1,1, "in")
          )
        
        plot_text <- arrangeGrob(title, par1, par2, par3, plot, nrow= 5, heights = unit(c(1, 1, 1, 1, 5), "null"))
        
        ggsave(
          file,
          plot_text,
          device = "pdf",
          width = 10.625,  
          height = 13.75,  
          units = "in",  
          dpi = 300  
        )
      }
    )
    
    # Underlying Math for Adjusted Utility
    vcost <- input$vcost
    tax <- input$tax
    drate <- input$drate
    # Calculate unadjusted utility for expanded formula
    unadjusted_utility <- input$n * input$sdy * (input$rxy2 - input$rxy1) * ux(input$sr) * input$period - (input$cost2 - input$cost1) * (input$n / input$sr)
    # Calculate totDelta for adjusted utility
varCosts <- -vcost/100
taxProp <- tax/100
discProp <- drate/100
discRat <- 1/(1 + discProp)
valid <- input$rxy2 - input$rxy1
ord <- ux(input$sr)
SDjp <- input$sdy
sr1 <- input$sr
tenure1 <- input$period
last <- input$pyears
add <- input$nadd
subt <- input$nsub
costOrd <- input$cost1
costAc <- input$cost2
ordsr1 <- dnorm(qnorm(1-sr1),0,1)
ck <- add * ((costAc - costOrd) / sr1)
numyr <- tenure1 + last

nk <- 0
totDelta <- 0
totDelta1 <- 0

for (i in 1:numyr) {
  if (i > tenure1) {nk <- nk - subt}
  if (i <= last) {nk <- nk + add}
  if (i > last) {ck <- 0}
  if (nk >= 0) {
    delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
    delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
  }
  delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
  totDelta1 <- totDelta1 + delta2 + delta3
  delta <- delta1 - delta2
  totDelta <- totDelta + delta
}
    
    adjusted_utility_value <- totDelta
    adjusted_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Adjusted Utility = Utility × (1 - vcost/100) × (1 - tax/100) × (1 / (1 + drate/100))",
      "\n\nWhere:",
      "Utility = ", format_utility(unadjusted_utility), ", vcost = ", vcost, "%, tax = ", tax, "%, drate = ", drate, "%",
      "\n\nStep-by-step:",
      "\n1. Start with unadjusted utility: ", format_utility(unadjusted_utility),
      "\n2. Apply variable cost adjustment: (1 - ", vcost, "/100) = ", round(1 - vcost/100, 4),
      "\n3. Apply tax adjustment: (1 - ", tax, "/100) = ", round(1 - tax/100, 4),
      "\n4. Apply discount rate: (1 / (1 + ", drate, "/100)) = ", round(1 / (1 + drate/100), 4),
      "\n5. Adjusted Utility = ", format_utility(unadjusted_utility), " × ", round(1 - vcost/100, 4), " × ", round(1 - tax/100, 4), " × ", round(1 / (1 + drate/100), 4),
      "\n6. Final Adjusted Utility = ", format_utility(adjusted_utility_value),
      "</pre>"
    )
    output$adjusted_utility_math <- renderUI({
      if (!is.null(adjusted_utility_value)) {
        HTML(adjusted_formula)
      } else {
        NULL
      }
    })
  })
  
###Monte Carlo###
observeEvent(input$monteGo, {
  showModal(modalDialog("Please wait while the analysis completes.",
                        easyClose = FALSE,
                        footer = NULL))
  set.seed(input$seed)
  ##### Step 1: Build basic utility formula parameters (adjustment 0 in Sturman table 1) #####
  # Define the population of parameters to randomly sample from
  t = runif(10000, input$t1, input$t2)                               # expected tenure of staying in years
  n = runif(10000, log(input$n1), log(input$n2))       # Create a sequence from 1 to 1100 in increments of 1 for n         # 1-50 applicants per hire
  select = runif(10000, input$sel1, input$sel2)          # for example, from 0.01 to 1 in increments of 0.01
  sdy = runif(10000, input$sdy1, input$sdy2)            # standard deviation of job performance value
  if(input$costm1 == 0){
    cost = runif(10000, 0, log(input$costm2))}
  else{
    cost = runif(10000, log(input$costm1), log(input$costm2))
  }    # cost per applicant of operating the new selection program
  r1 = runif(10000, input$r1, input$r2)            # operational validity of the new selection program
  utility_data <- data.frame(
    t = t,
    n = exp(n),
    select = 1/select,
    sdy = sdy,
    cost = exp(cost),
    r1 = r1
  )
  
  
  utility_data$unadjusted_cost <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    cost <- as.numeric(row["cost"])
    select <- as.numeric(row["select"])
    return(as.numeric(round(n * cost / select, 0)))
  })
  
  utility_data$unadjusted_utility <- apply(utility_data, 1, function(row){
    t <- as.numeric(row["t"])
    n <- as.numeric(row["n"])
    select <- as.numeric(row["select"])
    r1 <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    unadjusted_cost <- as.numeric(row["unadjusted_cost"])
    return(as.numeric((t * n * iopsych::ux(select) * r1 * sdy) - unadjusted_cost))
  })
  
  # Create a data frame for the plot
  unadjusted_utility_df <- data.frame(Unadjusted_Utility_Total = utility_data$unadjusted_utility)
  
  # Calculate density
  density_df_un <- data.frame(x = density(unadjusted_utility_df$Unadjusted_Utility_Total)$x,
                           y = density(unadjusted_utility_df$Unadjusted_Utility_Total)$y)

  density_df_un$group <- "Line 1"
  
  ##### Step 2: Account for # Economic adjustments (adjustment 1 in Sturman table 1).  #####
  # Variables for adjustment
  i = runif(10000, input$i1, input$i2)          # discount rate
  tax = runif(10000, input$tax1, input$tax2)        # marginal tax rate
  vc = runif(10000, input$vc1, input$vc2)          # variable costs
  
  # Place these values into the dataset that we built. Sample random values for each row in the dataset
  utility_data$i <- i
  utility_data$tax <- tax
  utility_data$vc <- vc
  
  # build adjustment formula for econ
  adjusted_utility_econ <- function(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t) {
    return ((n*x2*r1*uxs*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
  }
  
  # compute adjusted utility for each row
  utility_data$adjusted_utility_econ <- apply(utility_data, 1, function(row) {
    n <- as.numeric(row["n"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    t <- as.numeric(row["t"])
    r1 <- as.numeric(row["r1"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    uxs = iopsych::ux(sr)
    return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
  })
  
  # Create a data frame for the plot
 adjusted_utility_df_econ <- data.frame(Adjusted_Utility_Total_econ = utility_data$adjusted_utility_econ)
  
  # Calculate density
  density_df_econ <- data.frame(x = density(adjusted_utility_df_econ$Adjusted_Utility_Total_econ)$x,
                              y = density(adjusted_utility_df_econ$Adjusted_Utility_Total_econ)$y)
  
  density_df_econ$group <- "Line 2"
  
  #compute effect
  utility_data$percent_decrease_econ <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  # View the first few rows of the updated dataset
  mean.econ <- mean(utility_data$percent_decrease_econ)
  median.econ <- median(utility_data$percent_decrease_econ)
  
  ##### Step 3: Account for # Multiple Devices (adjustment 5 in Sturman table 1).  #####
  
  incremental_r = runif(10000, input$ir1, input$ir2)
  utility_data$incremental_r <- incremental_r
  utility <- function(n, t, r, sdy, sr, cost) {
    return(n*t*r*iopsych::ux(sr)*sdy-round((n/sr)*cost, 0))
  }
  
  #compute adjusted utility for multiple devices
  utility_data$adjusted_utility_mult <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    return(as.numeric(utility(n, t, r, sdy, sr, cost)))
  })
  
  #compute effect
  utility_data$percent_decrease_mult <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_mult"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.mult <- mean(utility_data$percent_decrease_mult)
  median.mult <- median(utility_data$percent_decrease_mult)
  
  ##### Step 4: Account for # Deviations from Top-Down Hiring (adjustment 6 in Sturman table 1).  #####
  pa <- runif(10000, input$pa1, input$pa2) # initial acceptance rate
  bxy <- runif(10000, input$bxy1, input$bxy2) # correlation between acceptance and performance
  utility_data$initial_accept <- pa
  utility_data$perf_corr <- bxy
  utility.topdown <- function(n, t, r, pa, bxy, sdy, sr, cost) {
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
    return(n*t*r*z*sdy-round((n/sr)*cost, 0))
  }
  
  #compute adjusted utility for deviations from top down hiring
  
  utility_data$adjusted_utility_topdown <-  apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    return(as.numeric(utility.topdown(n, t, r, pa, bxy, sdy, sr, cost)))
  })
  
  #compute effect
  utility_data$decrease_topdown <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_topdown"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.topdown <- mean(utility_data$decrease_topdown)
  median.topdown <- median(utility_data$decrease_topdown)
  
  ##### Step 5: Account for # Probationary Period (adjustment 4 in Sturman table 1).  #####
  
  rc <- runif(10000, input$rc1, input$rc2) # cutoff score
  utility_data$cutoff_score <- rc
  
  utility.probation <- function(n, t, r, sr, sdy, rc, cost) {
    xc <- qnorm(1-sr)
    PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
    PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
    PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
    mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
    sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
    so <- pnorm(rc, lower.tail = FALSE)
    mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
    return((n*sdy*r*ux(sr)+(t-1)*n*sp*sdy*mur.xcrc-(n/sr)*cost)-((t-1)*n*sdy*so*mur.rc))
  }
  
  utility_data$adjusted_utility_probation <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    rc <- as.numeric(row["cutoff_score"])
    return(as.numeric(utility.probation(n,t,r,sr,sdy,rc,cost)))
  })
  
  #compute effect
  utility_data$decrease_probation <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_probation"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.probation <- mean(utility_data$decrease_probation)
  median.probation <- median(utility_data$decrease_probation)
  
  ##### Step 6: Account for # Employee Flows (adjustment 2 in Sturman table 1).  #####
  pto <- runif(10000, input$pto1, input$pto2) # turnover probability
  corrto <- runif(10000, input$corrto1, input$corrto2) # turnover performance correlation
  
  utility_data$turnover_probability <- pto
  utility_data$performance_correlation_turnover <- corrto
  
  utility.flow <- function(n, t, r, sdy, pto, corrto, sr, cost){
    num_years <- seq(1, t)
    n_added <- c(rep(n, 1), rep(0, t - 1))
    n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
    
    #Financially Adjusted Costs of Program in period (year)
    c_adj <- (n_added / sr) * cost
    unadjusted_utility <- n_cum * r * zbarx * sdy
    
    adjusted_utility <- unadjusted_utility - c_adj
    adjusted_utility <- sum(round(adjusted_utility, 0))
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_flow <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    return(as.numeric(utility.flow(n, t, r, sdy, pto, corrto, sr, cost)))
  })
  
  #compute effect
  utility_data$decrease_flow <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_flow"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.flow <- mean(utility_data$decrease_flow)
  median.flow <- median(utility_data$decrease_flow)
  
  ##### Step 7: Account for # Temporal Validity (adjustment 2 in Sturman table 1). #####
  
  stab <- runif(10000, input$stab1, input$stab2) #stability of performance
  utility_data$performance_stability <- stab
  
  utility_data$adjusted_utility_temp <- apply(utility_data, 1, function(row){
    t <- as.numeric(row["t"])
    n <- as.numeric(row["n"])
    select <- as.numeric(row["select"])
    r1 <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    unadjusted_cost <- as.numeric(row["unadjusted_cost"])
    stab <- as.numeric(row["performance_stability"])
    return(as.numeric((t * n * stab * ux(select) * r1 * sdy) - unadjusted_cost))
  })
  
  #compute effect
  utility_data$decrease_temp <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_temp"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.temp <- mean(utility_data$decrease_temp)
  median.temp <- median(utility_data$decrease_temp)
  
  #economic variables + multiple devices
  
  utility_data$adjusted_utility_econ_mult <- apply(utility_data, 1, function(row) {
    n <- as.numeric(row["n"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    i <- as.numeric(row["i"])
    t <- as.numeric(row["t"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    r1 <- as.numeric(row["incremental_r"])
    x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    uxs = ux(sr)
    return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
  })
  
  # Create a data frame for the plot
  adjusted_utility_df_econmult <- data.frame(Adjusted_Utility_Total_econmult = utility_data$adjusted_utility_econ_mult)
  
  # Calculate density
  density_df_econmult <- data.frame(x = density(adjusted_utility_df_econmult$Adjusted_Utility_Total_econmult)$x,
                                y = density(adjusted_utility_df_econmult$Adjusted_Utility_Total_econmult)$y)
  
  density_df_econmult$group <- "Line 3"
  
  #compute effect
  utility_data$decrease_econmult <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over1 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_mult"])/abs(row["adjusted_utility_econ"])
    return(as.numeric(100*change))
  })
  
  mean.econmult <- mean(utility_data$decrease_econmult)
  median.econmult <- median(utility_data$decrease_econmult)
  mean.over1 <- mean(utility_data$decrease_over1)
  median.over1 <- median(utility_data$decrease_over1)
  
  #economic variables + temporal validity
  
  utility_data$adjusted_utility_econ_tv <- apply(utility_data, 1, function(row) {
    n <- as.numeric(row["n"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    i <- as.numeric(row["i"])
    t <- as.numeric(row["t"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    r1 <- as.numeric(row["r1"])
    x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    stab = as.numeric(row["performance_stability"])
    uxs = ux(sr)*stab
    return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
  })
  
  #compute effect
  utility_data$decrease_econtv <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_tv"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over1.2 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_tv"])/abs(row["adjusted_utility_econ"])
    return(as.numeric(100*change))
  })
  
  mean.econtv <- mean(utility_data$decrease_econtv)
  median.econtv <- median(utility_data$decrease_econtv)
  mean.over1.2 <- mean(utility_data$decrease_over1.2)
  median.over1.2 <- median(utility_data$decrease_over1.2)
  
  #economic variables + top down hiring
  
  utility.econtd <- function(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc){
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
    x2 <- (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    return ((n*x2*r*z*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
  }
  
  utility_data$adjusted_utility_econ_td <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    return(utility.econtd(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc))
  })
  
  #compute effect
  utility_data$decrease_econ_td <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_td"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over1.3 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_td"])/abs(row["adjusted_utility_econ"])
    return(as.numeric(100*change))
  })
  
  mean.econtd <- mean(utility_data$decrease_econ_td)
  median.econtd <- median(utility_data$decrease_econ_td)
  mean.over1.3 <- mean(utility_data$decrease_over1.3)
  median.over1.3 <- median(utility_data$decrease_over1.3)
  
  #economic variables + probation
  
  utility.econ.probation <- function(n, t, r, sr, sdy, rc, cost, i, vc, tax) {
    xc <- qnorm(1-sr)
    PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
    PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
    PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
    mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
    sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
    so <- pnorm(rc, lower.tail = FALSE)
    mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
    x1 <- 1/(1+i)
    x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
    return((n*x1*sdy*r*ux(sr)*(1-tax)*(1-vc)+x2*n*sp*sdy*mur.xcrc*(1-vc)*(1-tax)-(n/sr)*cost*(1-tax))-(x2*n*sdy*so*mur.rc*(1-vc)*(1-tax)))
  }
  
  utility_data$adjusted_utility_econ_prob <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    rc <- as.numeric(row["cutoff_score"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    return(as.numeric(utility.econ.probation(n,t,r,sr,sdy,rc,cost, i, vc, tax)))
  })
  
  #compute effect
  utility_data$decrease_econ_prob <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_prob"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over1.4 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_prob"])/abs(row["adjusted_utility_econ"])
    return(as.numeric(100*change))
  })
  
  mean.econprob <- mean(utility_data$decrease_econ_prob)
  median.econprob <- median(utility_data$decrease_econ_prob)
  mean.over1.4 <- mean(utility_data$decrease_over1.4)
  median.over1.4 <- median(utility_data$decrease_over1.4)
  
  #economic variables + employee flows
  
  utility.econ.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax){
    num_years <- seq(1, t)
    n_added <- c(rep(n, 1), rep(0, t - 1))
    n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
    discount_factor <- 1/(1 + i)^num_years
    
    #Financially Adjusted Costs of Program in period (year)
    c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
    unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
    
    adjusted_utility <- unadjusted_utility - c_adj
    adjusted_utility <- sum(round(adjusted_utility, 0))
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_econ_flow <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    return(as.numeric(utility.econ.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax)))
  })
  
  #compute effect
  utility_data$decrease_econ_flow <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_flow"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over1.5 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_flow"])/abs(row["adjusted_utility_econ"])
    return(as.numeric(100*change))
  })
  
  mean.econflow <- mean(utility_data$decrease_econ_flow)
  median.econflow <- median(utility_data$decrease_econ_flow)
  mean.over1.5 <- mean(utility_data$decrease_over1.5)
  median.over1.5 <- median(utility_data$decrease_over1.5)
  
  #economic variables + multiple devices + temporal validity
  
  utility_data$adjusted_utility_econ_mult_tv <- apply(utility_data, 1, function(row) {
    n <- as.numeric(row["n"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    i <- as.numeric(row["i"])
    t <- as.numeric(row["t"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    r1 <- as.numeric(row["incremental_r"])
    x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    stab = as.numeric(row["performance_stability"])
    uxs = ux(sr)*stab
    return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
  })
  
  # Create a data frame for the plot
  adjusted_utility_df_econmulttv <- data.frame(Adjusted_Utility_Total_econmulttv = utility_data$adjusted_utility_econ_mult_tv)
  
  # Calculate density
  density_df_econmulttv <- data.frame(x = density(adjusted_utility_df_econmulttv$Adjusted_Utility_Total_econmulttv)$x,
                                    y = density(adjusted_utility_df_econmulttv$Adjusted_Utility_Total_econmulttv)$y)
  
  density_df_econmulttv$group <- "Line 4"
  
  #compute effect
  utility_data$decrease_econmulttv <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over2 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_tv"])/abs(row["adjusted_utility_econ_mult"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttv <- mean(utility_data$decrease_econmulttv)
  median.econmulttv <- median(utility_data$decrease_econmulttv)
  mean.over2 <- mean(utility_data$decrease_over2)
  median.over2 <- median(utility_data$decrease_over2)
  
  #economic variables + multiple devices + top down
  
  utility_data$adjusted_utility_econ_mult_td <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    return(utility.econtd(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc))
  })
  
  #compute effect
  utility_data$decrease_econmulttd <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_td"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over2.1 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_td"])/abs(row["adjusted_utility_econ_mult"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttd <- mean(utility_data$decrease_econmulttd)
  median.econmulttd <- median(utility_data$decrease_econmulttd)
  mean.over2.1 <- mean(utility_data$decrease_over2.1)
  median.over2.1 <- median(utility_data$decrease_over2.1)
  
  #economic variables + multiple devices + probation
  
  utility_data$adjusted_utility_econ_mult_prob <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    rc <- as.numeric(row["cutoff_score"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    return(as.numeric(utility.econ.probation(n,t,r,sr,sdy,rc,cost, i, vc, tax)))
  })
  
  #compute effect
  utility_data$decrease_econmultprob <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_prob"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over2.2 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_prob"])/abs(row["adjusted_utility_econ_mult"])
    return(as.numeric(100*change))
  })
  
  mean.econmultprob <- mean(utility_data$decrease_econmultprob)
  median.econmultprob <- median(utility_data$decrease_econmultprob)
  mean.over2.2 <- mean(utility_data$decrease_over2.2)
  median.over2.2 <- median(utility_data$decrease_over2.2)
  
  #economic variables + multiple devices + employee flows
  
  utility.econ.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax){
    num_years <- seq(1, t)
    n_added <- c(rep(n, 1), rep(0, t - 1))
    n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
    discount_factor <- 1/(1 + i)^num_years
    
    #Financially Adjusted Costs of Program in period (year)
    c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
    unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
    
    adjusted_utility <- unadjusted_utility - c_adj
    adjusted_utility <- sum(round(adjusted_utility, 0))
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_econ_mult_flow <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    return(as.numeric(utility.econ.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax)))
  })
  
  #compute effect
  utility_data$decrease_econmultflow <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_flow"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over2.3 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_flow"])/abs(row["adjusted_utility_econ_mult"])
    return(as.numeric(100*change))
  })
  
  mean.econmultflow <- mean(utility_data$decrease_econmultflow)
  median.econmultflow <- median(utility_data$decrease_econmultflow)
  mean.over2.3 <- mean(utility_data$decrease_over2.3)
  median.over2.3 <- median(utility_data$decrease_over2.3)
  
  #economic variables + multiple devices + temporal validity + top down
  
  utility.econtdtv <- function(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab){
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
    x2 <- (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    return ((n*x2*r*stab*z*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
  }
  
  utility_data$adjusted_utility_econ_mult_tv_td <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    stab <- as.numeric(row["performance_stability"])
    return(utility.econtdtv(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab))
  })
  
  # Create a data frame for the plot
  adjusted_utility_df_econmulttvtd <- data.frame(Adjusted_Utility_Total_econmulttvtd = utility_data$adjusted_utility_econ_mult_tv_td)
  
  # Calculate density
  density_df_econmulttvtd <- data.frame(x = density(adjusted_utility_df_econmulttvtd$Adjusted_Utility_Total_econmulttvtd)$x,
                                      y = density(adjusted_utility_df_econmulttvtd$Adjusted_Utility_Total_econmulttvtd)$y)
  
  density_df_econmulttvtd$group <- "Line 5"
  
  #compute effect
  utility_data$decrease_econmulttvtd <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over3 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_td"])/abs(row["adjusted_utility_econ_mult_tv"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttvtd <- mean(utility_data$decrease_econmulttvtd)
  median.econmulttvtd <- median(utility_data$decrease_econmulttvtd)
  mean.over3 <- mean(utility_data$decrease_over3)
  median.over3 <- median(utility_data$decrease_over3)
  
  #economic variables + multiple devices + temporal validity + prob
  
  utility.econ.tv.probation <- function(n, t, r, sr, sdy, rc, cost, i, vc, tax, stab) {
    xc <- qnorm(1-sr)
    PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
    PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
    PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
    mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
    sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
    so <- pnorm(rc, lower.tail = FALSE)
    mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
    x1 <- 1/(1+i)
    x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
    return((n*x1*stab*sdy*r*ux(sr)*(1-tax)*(1-vc)+x2*n*sp*stab*sdy*mur.xcrc*(1-vc)*(1-tax)-(n/sr)*cost*(1-tax))-(x2*n*stab*sdy*so*mur.rc*(1-vc)*(1-tax)))
  }
  
  utility_data$adjusted_utility_econ_mult_tv_prob <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    rc <- as.numeric(row["cutoff_score"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    stab <- as.numeric(row["performance_stability"])
    return(as.numeric(utility.econ.tv.probation(n, t, r, sr, sdy, rc, cost, i, vc, tax, stab)))
  })
  
  #compute effect
  utility_data$decrease_econmulttvprob <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_prob"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over3.1 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_prob"])/abs(row["adjusted_utility_econ_mult_tv"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttvprob <- mean(utility_data$decrease_econmulttvprob)
  median.econmulttvprob <- median(utility_data$decrease_econmulttvprob)
  mean.over3.1 <- mean(utility_data$decrease_over3.1)
  median.over3.1 <- median(utility_data$decrease_over3.1)
  
  #economic variables + multiple devices + temporal validity + flows
  
  utility.econ.tv.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab){
    num_years <- seq(1, t)
    n_added <- c(rep(n, 1), rep(0, t - 1))
    n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
    discount_factor <- 1/(1 + i)^num_years
    
    #Financially Adjusted Costs of Program in period (year)
    c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
    unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
    
    adjusted_utility <- stab*unadjusted_utility - c_adj
    adjusted_utility <- sum(round(adjusted_utility, 0))
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_econ_mult_tv_flow <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    stab <- as.numeric(row["performance_stability"])
    return(as.numeric(utility.econ.tv.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab)))
  })
  
  #compute effect
  utility_data$decrease_econmulttvflow <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_flow"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over3.2 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_flow"])/abs(row["adjusted_utility_econ_mult_tv"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttvflow <- mean(utility_data$decrease_econmulttvflow)
  median.econmulttvflow <- median(utility_data$decrease_econmulttvflow)
  mean.over3.2 <- mean(utility_data$decrease_over3.2)
  median.over3.2 <- median(utility_data$decrease_over3.2)
  
    #economic variables + multiple devices + temporal validity + top down
  
  utility.econ.mult.tv.td.prob <- function(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab){
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    xc <- qnorm(1-sr)
    xc.pa <- qnorm(1-pa)
    xc.p2 <- qnorm(1-p2)
    PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
    PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
    PHI1.1.pa <- (xc.pa-r*rc)/sqrt(1-r^2)
    PHI1.2.pa <- (rc-r*xc.pa)/sqrt(1-r^2)
    PHI1.1.p2 <- (xc.p2-r*rc)/sqrt(1-r^2)
    PHI1.2.p2 <- (rc-r*xc.p2)/sqrt(1-r^2)
    PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
    mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
    mur.xcrc.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))/pmvnorm(c(xc.pa, rc), Inf, corr = PHI2.corr)
    mur.xcrc.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))/pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)
    sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
    sp.p2 <- pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)/pnorm(xc.p2, lower.tail = FALSE)
    so <- pnorm(rc, lower.tail = FALSE)
    mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
    z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
    z.mod <- sp*(pa*mur.xcrc+pa*bxy*ux(pa)*r)+sp.p2*((p2/sr)*mur.xcrc.p2-mur.xcrc)
    x1 <- 1/(1+i)
    x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
    return((n*x1*stab*sdy*r*z*(1-tax)*(1-vc)+x2*n*sdy*stab*z.mod*(1-tax)*(1-vc)-(n/sr)*cost*(1-tax))-(x2*n*sdy*so*stab*mur.rc*(1-tax)*(1-vc)))
  }
  
  utility_data$adjusted_utility_econ_mult_tv_td_prob <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    rc <- as.numeric(row["cutoff_score"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    stab <- as.numeric(row["performance_stability"])
    return(as.numeric(utility.econ.mult.tv.td.prob(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab)))
  })
  
  # Create a data frame for the plot
  adjusted_utility_df_econmulttvtdprob <- data.frame(Adjusted_Utility_Total_econmulttvtdprob = utility_data$adjusted_utility_econ_mult_tv_td_prob)
  
  # Calculate density
  density_df_econmulttvtdprob <- data.frame(x = density(adjusted_utility_df_econmulttvtdprob$Adjusted_Utility_Total_econmulttvtdprob)$x,
                                        y = density(adjusted_utility_df_econmulttvtdprob$Adjusted_Utility_Total_econmulttvtdprob)$y)
  
  density_df_econmulttvtdprob$group <- "Line 6"
  
  #compute effect
  utility_data$decrease_econmulttvtdprob <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td_prob"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over4 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult_tv_td"]-row["adjusted_utility_econ_mult_tv_td_prob"])/abs(row["adjusted_utility_econ_mult_tv_td"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttvtdprob <- mean(utility_data$decrease_econmulttvtdprob)
  median.econmulttvtdprob <- median(utility_data$decrease_econmulttvtdprob)
  mean.over4 <- mean(utility_data$decrease_over4)
  median.over4 <- median(utility_data$decrease_over4)
  
  #economic variables + multiple devices + temporal validity + top down + flow
  
  utility.econ.tv.td.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy){
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    num_years <- seq(1, t)
    n_added <- c(rep(n, 1), rep(0, t - 1))
    n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep((pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n, 1), rep(((pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n)+corrto*ux(1-pto), t-1))
    discount_factor <- 1/(1 + i)^num_years
    
    #Financially Adjusted Costs of Program in period (year)
    c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
    unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
    
    adjusted_utility <- stab*unadjusted_utility - c_adj
    adjusted_utility <- sum(round(adjusted_utility, 0))
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_econ_mult_tv_td_flow <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    stab <- as.numeric(row["performance_stability"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    return(as.numeric(utility.econ.tv.td.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy)))
  })
  
  #compute effect
  utility_data$decrease_econmulttvtdflow <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td_flow"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over4.1 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult_tv_td"]-row["adjusted_utility_econ_mult_tv_td_flow"])/abs(row["adjusted_utility_econ_mult_tv_td"])
    return(as.numeric(100*change))
  })
  
  mean.econmulttvtdflow <- mean(utility_data$decrease_econmulttvtdflow)
  median.econmulttvtdflow <- median(utility_data$decrease_econmulttvtdflow)
  mean.over4.1 <- mean(utility_data$decrease_over4.1)
  median.over4.1 <- median(utility_data$decrease_over4.1)
  
  # final adjustment
  
  utility.complete <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc){
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    xc <- qnorm(1-sr)
    xc.pa <- qnorm(1-pa)
    xc.p2 <- qnorm(1-p2)
    PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
    PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
    PHI1.1.pa <- (xc.pa-r*rc)/sqrt(1-r^2)
    PHI1.2.pa <- (rc-r*xc.pa)/sqrt(1-r^2)
    PHI1.1.p2 <- (xc.p2-r*rc)/sqrt(1-r^2)
    PHI1.2.p2 <- (rc-r*xc.p2)/sqrt(1-r^2)
    PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
    mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
    mur.xcrc.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))/pmvnorm(c(xc.pa, rc), Inf, corr = PHI2.corr)
    mur.xcrc.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))/pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)
    sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
    sp.p2 <- pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)/pnorm(xc.p2, lower.tail = FALSE)
    so <- pnorm(rc, lower.tail = FALSE)
    mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
    z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
    z.mod <- sp*(pa*mur.xcrc+pa*bxy*ux(pa)*r)+sp.p2*((p2/sr)*mur.xcrc.p2-mur.xcrc)
    x1 <- 1/(1+i)
    #Financially Adjusted Costs of Program in period (year)
    a <- n*x1*stab*sdy*r*z*(1-tax)*(1-vc)
    c <- (n/sr)*cost*(1-tax)
    if (t>1){
      num_years <- seq(1, t-1)
      n_added <- c(rep(n, 1), rep(0, t - 2))
      n_turn <- c(rep((n*pto)/t, t-1))
      n_cum <- cumsum(n_added - n_turn)
      zbarx <- c(rep((z.mod+corrto*ux(1-pto)*sp), t-1))
      discount_factor <- 1/(1 + i)^(num_years+1)
      b <- stab*n_cum * discount_factor * zbarx * sdy * (1 - vc) * (1 - tax)
      utility_0 <- stab * n_cum * discount_factor * so * (mur.rc+corrto*ux(1-pto)) * sdy * (1 - vc) * (1 - tax)
    }
    else{
      b <- 0
      utility_0 <- 0
    }
    adjusted_utility <- a+sum(b)-c-sum(utility_0)
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_final <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    stab <- as.numeric(row["performance_stability"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    rc <- as.numeric(row["cutoff_score"])
    return(as.numeric(utility.complete(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc)))
  })
  
  # Create a data frame for the plot
  adjusted_utility_df_final <- data.frame(Adjusted_Utility_Total_final = utility_data$adjusted_utility_final)
  
  # Calculate density
  density_df_final <- data.frame(x = density(adjusted_utility_df_final$Adjusted_Utility_Total_final)$x,
                                            y = density(adjusted_utility_df_final$Adjusted_Utility_Total_final)$y)
  
  density_df_final$group <- "Line 7"
  
  #compute effect
  utility_data$decrease_final <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_final"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  utility_data$decrease_over5 <- apply(utility_data, 1, function(row){
    change <- (row["adjusted_utility_econ_mult_tv_td_prob"]-row["adjusted_utility_final"])/abs(row["adjusted_utility_econ_mult_tv_td_prob"])
    return(as.numeric(100*change))
  })
  
  mean.final <- mean(utility_data$decrease_final)
  median.final <- median(utility_data$decrease_final)
  mean.over5 <- mean(utility_data$decrease_over5)
  median.over5 <- median(utility_data$decrease_over5)
  
  # Effect Table
  table_2 <- data.frame(
    net_effect_1 = c(paste0(round(-median.econ, 0), "%"), paste0(round(-mean.econ, 0), "%"), paste0(round(-median.mult, 0), "%"), paste0(round(-mean.mult, 0), "%"), paste0(round(-median.temp, 0), "%"), paste0(-round(mean.temp, 0), "%"),  paste0(round(-median.topdown, 0), "%"),
                     paste0(round(-mean.topdown, 0), "%"), paste0(round(-median.probation, 0), "%"), paste0(round(-mean.probation, 0), "%"), paste0(round(-median.flow, 0), "%"),  paste0(round(-mean.flow, 0), "%")),
    net_effect_2a = c("", "", paste0(round(-median.econmult, 0), "%"), paste0(round(-mean.econmult, 0), "%"), paste0(round(-median.econtv, 0), "%"), paste0(round(-mean.econtv, 0), "%"), paste0(round(-median.econtd, 0), "%"), paste0(round(-mean.econtd, 0), "%"), paste0(round(-median.econprob, 0), "%"), paste0(round(-mean.econprob, 0), "%"), paste0(round(-median.econflow, 0), "%"), paste0(round(-mean.econflow, 0), "%")),
    over_prev_2b =c("", "", paste0(round(-median.over1, 0), "%"), paste0(round(-mean.over1, 0), "%"), paste0(round(-median.over1.2, 0), "%"), paste0(round(-mean.over1.2, 0), "%"), paste0(round(-median.over1.3, 0), "%"), paste0(round(-mean.over1.3, 0), "%"), paste0(round(-median.over1.4, 0), "%"), paste0(round(-mean.over1.4, 0), "%"), paste0(round(-median.over1.5, 0), "%"), paste0(round(-mean.over1.5, 0), "%")),
    net_effect_3a = c("", "", "", "", paste0(round(-median.econmulttv, 0), "%"), paste0(round(-mean.econmulttv, 0), "%"), paste0(round(-median.econmulttd, 0), "%"), paste0(round(-mean.econmulttd, 0), "%"), paste0(round(-median.econmultprob, 0), "%"), paste0(round(-mean.econmultprob, 0), "%"), paste0(round(-median.econmultflow, 0), "%"), paste0(round(-mean.econmultflow, 0), "%")),
    over_prev_3b =c("", "", "", "", paste0(round(-median.over2, 0), "%"), paste0(round(-mean.over2, 0), "%"), paste0(round(-median.over2.1, 0), "%"), paste0(round(-mean.over2.1, 0), "%"), paste0(round(-median.over2.2, 0), "%"), paste0(round(-mean.over2.2, 0), "%"), paste0(round(-median.over2.3, 0), "%"), paste0(round(-mean.over2.3, 0), "%")),
    net_effect_4a = c("", "", "", "", "", "", paste0(round(-median.econmulttvtd, 0), "%"), paste0(round(-mean.econmulttvtd, 0), "%"), paste0(round(-median.econmulttvprob, 0), "%"), paste0(round(-mean.econmulttvprob, 0), "%"), paste0(round(-median.econmulttvflow, 0), "%"), paste0(round(-mean.econmulttvflow, 0), "%")),
    over_prev_4b =c("", "", "", "", "", "", paste0(round(-median.over3, 0), "%"), paste0(round(-mean.over3, 0), "%"), paste0(round(-median.over3.1, 0), "%"), paste0(round(-mean.over3.1, 0), "%"), paste0(round(-median.over3.2, 0), "%"), paste0(round(-mean.over3.2, 0), "%")),
    net_effect_5a = c("", "", "", "", "", "", "", "", paste0(round(-median.econmulttvtdprob, 0), "%"), paste0(round(-mean.econmulttvtdprob, 0), "%"), paste0(round(-median.econmulttvtdflow, 0), "%"), paste0(round(-mean.econmulttvtdflow, 0), "%")),
    over_prev_5b =c("", "", "", "", "", "", "", "", paste0(round(-median.over4, 0), "%"), paste0(round(-mean.over4, 0), "%"), paste0(round(-median.over4.1, 0), "%"), paste0(round(-mean.over4.1, 0), "%")),
    net_effect_6a = c("", "", "", "", "", "", "", "", "", "", paste0(round(-median.final, 0), "%"), paste0(round(-mean.final, 0), "%")),
    over_prev_6b =c("", "", "", "", "", "", "", "", "", "", paste0(round(-median.over5, 0), "%"), paste0(round(-mean.over5, 0), "%"))
  )
  rownames(table_2) <- c("Economic Variables Median", "Economic Variables Mean", "Multiple Devices Median", "Multiple Devices Mean", "Temporal Validity Median", "Temporal Validity Mean", "Deviations from Top Down Hiring Median", "Deviations from Top Down Hiring Mean", "Probationary Period Median", "Probationary Period Mean", "Employee Flows Median", "Employee Flows Mean")
  colnames(table_2) <- c("Net Effect1", "Net Effect2a", "Over Previous Modification2b", "Net Effect3a", "Over Previous Modification3b", "Net Effect4a", "Over Previous Modification4b", "Net Effect5a", "Over Previous Modification5b", "Net Effect6a", "Over Previous Modification6b")
  output$sturman2 <- renderTable(table_2, rownames = TRUE, width = 1000)
  
  combined_df <- density_df_final
  combined_df$Fill <- ifelse(combined_df$x < 0, "Negative", "Positive")
  
  density_plot <- ggplot(combined_df, aes(x = x, y = y)) + 
    geom_area(aes(fill = Fill), alpha=0.4) +
    scale_fill_manual(values = c("Negative" = "red", "Positive" = "green")) +
    scale_x_continuous(labels = dollar_format2, breaks = extended_breaks(6)) +
    labs(title = "",
         x = "Return on Investment Total", 
         y = "") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0),
          plot.subtitle = element_text(hjust = 0, color = "black"),
          plot.background = element_rect(fill = "white", linetype = "blank"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 17),
          axis.title = element_text(size =18)) +
    guides(fill="none")  # Remove legend
  
  output$density <- renderPlotly({
    ggplotly(density_plot)
  })
  
  # Calculate percentage of estimates that are negative
  pct_negative <- round(sum(utility_data$adjusted_utility_final < 0) / length(utility_data$adjusted_utility_final) * 100, 2)
  
  # Calculate average utility estimate
  avg_utility_estimate <- round(median(utility_data$adjusted_utility_final), 2)
  
  # get the positive value
  avg_positive_utility <- median(utility_data[utility_data$adjusted_utility_final > 0, "adjusted_utility_final"])
  medianform <- dollar_format2(avg_utility_estimate)
  
  output$medianmonte <- renderText(paste0("The median return on investment for these parameters is ", medianform, "."))
  output$negmonte <- renderText(paste0(pct_negative, "% of return on investment outcomes were negative."))
  
  removeModal()
}
  
#Training Utility
  
  #toggles for Training/RODI
  observeEvent(input$selectPage1, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 1")
  })
  
  observeEvent(input$selectPage2, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 2")
  })
  
  observe({
    dt <- input$dTrain2_1
    # Get the label for the selected effect size
    effect_choices <- list(
      "Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)" = 0.35,
      "Leadership development: Average estimate (Avoilio et al., 2009)" = 0.65,
      "Leadership development: Intensive program (3+ days) (Avoilio et al., 2009)" = 1.00,
      "Leadership development: Upper bound (Avoilio et al., 2009)" = 1.37,
      "Goal setting (Schmidt, 2013)" = 0.46
    )
    # Find the label corresponding to the selected value
    selected_label <- names(effect_choices)[which(as.numeric(effect_choices) == as.numeric(input$effect_size_dropdown))][1]
    # Extract only the intervention type (e.g., 'Goal setting' or 'Leadership development')
    intervention <- trimws(sub("[:(].*", "", selected_label))
    # Calculate BESD values
    r <- dt / sqrt(dt^2 + 4)
    success_rate_with <- 0.5 + (r / 2)
    success_rate_without <- 0.5 - (r / 2)
    besd_data <- data.frame(
      Group = c("Untreated", "Treated"),
      SuccessRate = c(success_rate_without, success_rate_with)
    )
    besd_data$Group <- factor(besd_data$Group, levels = c("Untreated", "Treated"))
    diff_percent <- round((success_rate_with - success_rate_without) * 100, 1)
    # Extract modifier from the dropdown label (e.g., 'weak estimate', 'average estimate', etc.)
    # The label is the full dropdown label, e.g., 'Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)'
    # We'll extract the part before the first parenthesis as the intervention, and the part in parentheses as the modifier
    label_parts <- strsplit(selected_label, ": ")[[1]]
    intervention_type <- label_parts[1]
    # Extract modifier (e.g., 'Lower bound (e.g., minimal short program) (Avoilio et al., 2009)')
    modifier <- if (length(label_parts) > 1) label_parts[2] else ""
    # Clean up modifier: take the first part before citation, and trim, then lowercase
    modifier_clean <- tolower(trimws(sub("\\(.*", "", modifier)))
    # Compose subtitle
    subtitle_text <- paste0(intervention_type, if (nzchar(modifier_clean)) paste0(" (", modifier_clean, ")") else "", ": Success rate improves by ", diff_percent, "%.")
    # BESD bar plot
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
    output$training_graph_1 <- renderPlot(gg)
      output$effectText <- renderText({
      paste0("The above chart shows the Binomial Effect Size Display (BESD) for the training program. With an effect size of ", dt, ", the success rate for the treated group is ", round(success_rate_with*100,1), "% compared to ", round(success_rate_without*100,1), "% for the untreated group.")
    })
      output$effect_download <- downloadHandler(
        filename = function() {
        "binomial_effect_size_display.pdf"
        },
        content = function(file) {
        capWrap <- strwrap(paste0("The above chart shows the Binomial Effect Size Display (BESD) for the training program. With an effect size of ", dt, ", the success rate for the treated group is ", round(success_rate_with*100,1), "% compared to ", round(success_rate_without*100,1), "% for the untreated group."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          plot <- gg +
            labs(
              caption = capWrap
          )
          ggsave(
            file,
            plot,
            device = "pdf",
            width = 10.625,  
            height = 6.875,  
            units = "in",  
            dpi = 300  
          )
        }
      )
  })
  
  #Training Utility Output Tab
  
  observeEvent(input$goUn,{
    # compute unadjusted training utility
    utilityTrainUn <- input$nTrain*input$dTrain2*input$sdyTrain*input$tTrain-input$nTrain*input$costTrain2
    # unadjusted break even sdy
    breakEven <- (input$nTrain*input$costTrain2)/(input$nTrain*input$tTrain*input$dTrain2)
    
    # Format utility values with appropriate scaling
    format_utility <- function(x) {
      paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
    }
    
    formatted_breakEvenTrain <- format_utility(breakEven)
    formatted_utilityTrainUn <- format_utility(utilityTrainUn)
    
    output$h3 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')
    })
    
    output$training_utilityUn <- renderText({
      paste("The opportunity costs of failing to use the program are:", formatted_utilityTrainUn)
    })
    
    output$break_even_train <- renderText({
      paste("The break even value of SDy is:", formatted_breakEvenTrain)
    })
    # Add context paragraph
    context_text <- paste0(
      "This analysis estimates the economic value generated by the training intervention. The unadjusted utility represents the total net benefit (in dollars) of the program, calculated as the increase in performance value minus the cost of the intervention. The opportunity cost is the value lost if the program is not implemented. These values help decision-makers understand the financial impact of training investments, and can be compared to the cost of alternative interventions or left unused."
    )
    output$trainingText1 <- renderText({ context_text })
    output$trainingText2 <- renderText({ "" })
    output$trainingText3 <- renderText({ "" })
  })
  
  # Add the new output to the UI (mainPanel) for utility_outputs tab
  # (Add textOutput("training_utilityOpCost") below training_utilityUn)

  observeEvent(input$go4,{
    enable("training_download")
    dt <- input$dTrain2
    is_schmidt <- !is.null(input$effect_size_dropdown_utility) && abs(as.numeric(input$effect_size_dropdown_utility) - 0.46) < 1e-6
    if (is_schmidt) {
      N <- input$nTrain
      T <- input$tTrain
      SDy <- input$sdyTrain
      d <- dt
      C <- input$costTrain2
      dollar_value <- N * T * SDy * d - N * C
      per_employee <- (T * SDy * d) - C
      per_year <- (SDy * d) - (C / T)
      percent_output_gain <- round(d * (input$sdP / 100) * 100, 1)
      formatted_total <- paste0("$", formatC(round(dollar_value), format = "f", big.mark = ",", digits = 0))
      formatted_per_employee <- paste0("$", formatC(round(per_employee), format = "f", big.mark = ",", digits = 0))
      formatted_per_year <- paste0("$", formatC(round(per_year), format = "f", big.mark = ",", digits = 0))
      formatted_C <- paste0("$", formatC(round(C), format = "f", big.mark = ",", digits = 0))
      formatted_SDy <- paste0("$", formatC(round(SDy), format = "f", big.mark = ",", digits = 0))
      annual_cost <- N * C
      formatted_annual_cost <- paste0("$", formatC(round(annual_cost), format = "f", big.mark = ",", digits = 0))
      output$h4 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Goal Setting Utility (Schmidt, 2013)</span>')})
      output$training_utilityUn <- renderText({ NULL })
      output$training_utilityOpCost <- renderText({ NULL })
      output$break_even_train <- renderText({ NULL })
      output$training_utility <- renderText({
        paste("Estimated Dollar Value of Goal Setting Intervention (Schmidt, 2013):", formatted_total)
      })
      output$training_utility_per_employee <- renderText({
        paste("Estimated Value per Employee:", formatted_per_employee)
      })
      output$training_utility_per_employee_per_year <- renderText({
        paste("Estimated Value per Employee per Year:", formatted_per_year)
      })
      output$trainingText1 <- renderText({
        untreated_below <- round(pnorm(0, mean = d, sd = 1) * 100, 0)
        paste0(
          "We have conducted an analysis in order to assess the utility of a goal setting intervention. The program consisted of ", N, " employees participating in a goal setting program. Research showed that ", untreated_below, "% of employees who did not participate in the intervention were assessed to have lower job performance than those who did participate."
        )
      })
      output$trainingText2 <- renderText({
        paste0(
          "Our cost analysis found that costs associated with the goal setting intervention are ", formatted_C, " per employee. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of ", formatted_SDy, ". The first is a variable cost adjustment and refers to costs that increase with higher job performance, such as compensation enhancements. These are expected to offset utility associated with goal setting by ", input$vrateTrain, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$taxTrain, "%. Lastly, the cash value of increased performance over time must be discounted to the present to approximate the net present value of the intervention. The discount rate applied to this intervention is ", input$discTrain, "%. Since ", N, " employees will participate each year, and the intervention is expected to be used for at least ", input$lengthTrain, " years, implementing the goal setting intervention is expected to cost our company ", formatted_annual_cost, " per year."
        )
      })
      output$trainingText3 <- renderText({
        paste0(
          "We found that utility is also affected by employee flows. When employees that participate in the intervention lose the effects from goal setting, the costs and benefits associated with the intervention change over time. Our research shows that employees that participated in the intervention are expected to be affected for an average of ", T, " years. This means that when employees do not participate in this intervention, the utility loss associated with each employee is ", formatted_per_employee, " or ", formatted_per_year, " per year. Considering this along with all previous factors, our analysis has found that failing to implement this intervention will result in a total utility loss of ", formatted_total, "."
        )
      })
      output$training_download <- downloadHandler(
        filename = function() {
          "goal_setting_utility_schmidt.pdf"
        },
        content = function(file) {
          capWrap <- strwrap(paste0(
            "Estimated Dollar Value of Goal Setting Intervention (Schmidt, 2013): ", formatted_total, "\n",
            "Formula: (T)(N)(SDy)(d) - (N)(C)\n",
            "Where: T = ", T, " years, N = ", N, ", SDy = ", formatted_SDy, ", d = ", d, ", C = ", formatted_C, ".\n",
            "Percent output gain: ", percent_output_gain, "%."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          plot <- ggplot() + theme_void() +
            labs(title = "Goal Setting Utility (Schmidt, 2013)", caption = capWrap)
          ggsave(
            file,
            plot,
            device = "pdf",
            width = 10.625,  
            height = 6.875,  
            units = "in",  
            dpi = 300  
          )
        }
      )
      return()
    }
    # Otherwise, use the existing logic for RODI/other interventions
    # ... existing code ...
  })
  
  # Add an observer to update dTrain2_1 when the dropdown changes
  observeEvent(input$effect_size_dropdown, {
    updateNumericInput(session, "dTrain2_1", value = as.numeric(input$effect_size_dropdown))
  })
  
  # Add an observer in the server to update dTrain2 when the dropdown changes
  observeEvent(input$effect_size_dropdown_utility, {
    updateNumericInput(session, "dTrain2", value = as.numeric(input$effect_size_dropdown_utility))
  })

  # Automatically set default values for goal setting when selected in Utility Outputs tab
  observeEvent(input$effect_size_dropdown_utility, {
    if (!is.null(input$effect_size_dropdown_utility) && abs(as.numeric(input$effect_size_dropdown_utility) - 0.46) < 1e-6) {
      updateNumericInput(session, "tTrain", value = 5)
      updateNumericInput(session, "nTrain", value = 35)
      updateNumericInput(session, "costTrain2", value = 200)
      updateNumericInput(session, "sdyTrain", value = 20000)
    }
  })

  # Render the Goal Setting diagram
  output$goal_setting_diagram <- renderGrViz({
    grViz("
      digraph path_diagram {
        graph [rankdir = LR]
        node [shape = box, style = filled, fillcolor = lightblue]

        GoalSetting [label = 'Goal Setting']
        Performance [label = 'Job Performance']
        Profits [label = 'Profits']

        GoalSetting -> Performance [label = '+']
        Performance -> Profits [label = '+']
      }
    ")
  })

  # Render the Utility diagram
  output$utility_diagram <- renderGrViz({
    grViz("
      digraph path_diagram {
        graph [layout = dot, rankdir = LR]

        node [
          shape = box,
          style = filled,
          fillcolor = lightblue,
          fontname = Helvetica,
          fontcolor = black,
          width = 2,
          fixedsize = true
        ]

        Quantity [label = 'Quantity']
        Quality [label = 'Quality']
        Cost [label = 'Cost']
        Utility [label = 'Utility']

        Quantity -> Utility
        Quality  -> Utility
        Cost     -> Utility
      }
    ")
  })

  # In the server logic, after computing and formatting the unadjusted utility, add:
  output$unadjusted_utility_value <- renderUI({
    HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityUn(), "</span>"))
  })
  output$unadjusted_utility_explanation <- renderUI({
    HTML(
      "The unadjusted utility estimate represents the total net financial benefit of using the new staffing procedure, before accounting for taxes, variable costs, or other economic adjustments. It is calculated as the increase in value from improved selection minus the additional costs of the new procedure. This value helps you understand the raw financial impact of switching to the new procedure, and is useful for comparing the basic ROI of different staffing strategies before considering real-world financial factors."
    )
  })

  # Add observers to clear outputs when switching pages
  observeEvent(input$utilityPages, {
    if (input$utilityPages == "Page 1") {
      output$adjusted_utility_label <- renderUI({ NULL })
      output$adjusted_utility_value <- renderUI({ NULL })
      output$adjusted_utility_explanation <- renderUI({ NULL })
      output$flows_utility_label <- renderUI({ NULL })
      output$flows_utility_value <- renderUI({ NULL })
      output$flows_utility_explanation <- renderUI({ NULL })
    } else if (input$utilityPages == "Page 2") {
      output$unadjusted_utility_label <- renderUI({ NULL })
      output$unadjusted_utility_value <- renderUI({ NULL })
      output$unadjusted_utility_explanation <- renderUI({ NULL })
      output$flows_utility_label <- renderUI({ NULL })
      output$flows_utility_value <- renderUI({ NULL })
      output$flows_utility_explanation <- renderUI({ NULL })
    } else if (input$utilityPages == "Page 3") {
      output$unadjusted_utility_label <- renderUI({ NULL })
      output$unadjusted_utility_value <- renderUI({ NULL })
      output$unadjusted_utility_explanation <- renderUI({ NULL })
      output$adjusted_utility_label <- renderUI({ NULL })
      output$adjusted_utility_value <- renderUI({ NULL })
      output$adjusted_utility_explanation <- renderUI({ NULL })
    }
  })

  # Page 1: Compute Utility
  observeEvent(input$go, {
    enable("download_pdf")
    output$unadjusted_utility_label <- renderUI({
      HTML("<b>Unadjusted Utility Estimate</b>")
    })
    output$unadjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityUn(), "</span>"))
    })
    output$unadjusted_utility_explanation <- renderUI({
      HTML(
        "The unadjusted utility estimate represents the total net financial benefit of using the new staffing procedure, before accounting for taxes, variable costs, or other economic adjustments. It is calculated as the increase in value from improved selection minus the additional costs of the new procedure. This value helps you understand the raw financial impact of switching to the new procedure, and is useful for comparing the basic ROI of different staffing strategies before considering real-world financial factors."
      )
    })
    # Underlying Math with Unicode/plaintext formatting
    n <- input$n
    sdy <- input$sdy
    rxy1 <- input$rxy1
    rxy2 <- input$rxy2
    sr <- input$sr
    cost1 <- input$cost1
    cost2 <- input$cost2
    period <- input$period
    delta_rxy <- rxy2 - rxy1
    uxs <- ux(sr)
    cost_diff <- (cost2 - cost1) * (n / sr)
    final_utility <- n * sdy * delta_rxy * uxs * period - cost_diff
    utility_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Utility = n × sdy × (rxy₂ - rxy₁) × ux(sr) × period - (cost₂ - cost₁) × (n / sr)",
      "\n\nWhere:",
      "n = ", n, ", sdy = ", sdy, ", rxy₂ = ", rxy2, ", rxy₁ = ", rxy1, ", ux(sr) = ", round(uxs, 4), ", period = ", period, ", cost₂ = ", cost2, ", cost₁ = ", cost1, ", sr = ", sr,
      "\n\nStep-by-step:",
      "\n1. Difference in validity: rxy₂ - rxy₁ = ", delta_rxy,
      "\n2. Cost difference: (cost₂ - cost₁) × (n / sr) = ", round(cost_diff, 2),
      "\n3. Utility = ", n, " × ", sdy, " × ", delta_rxy, " × ", round(uxs, 4), " × ", period, " - ", round(cost_diff, 2),
      "\n4. Final Utility = ", format_utility(final_utility),
      "</pre>"
    )
    output$unadjusted_utility_math <- renderUI({
      HTML(utility_formula)
    })
    # Clear other outputs
    output$adjusted_utility_label <- renderUI({ NULL })
    output$adjusted_utility_value <- renderUI({ NULL })
    output$adjusted_utility_explanation <- renderUI({ NULL })
    output$adjusted_utility_math <- renderUI({ NULL })
    output$flows_utility_label <- renderUI({ NULL })
    output$flows_utility_value <- renderUI({ NULL })
    output$flows_utility_explanation <- renderUI({ NULL })
    output$flows_utility_math <- renderUI({ NULL })
  })

  # Page 2: Compute Adjustments
  observeEvent(input$go2, {
    enable("download_pdf")
    output$adjusted_utility_label <- renderUI({
      HTML("<b>Adjusted Utility Estimate</b>")
    })
    output$adjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityAdj(), "</span>"))
    })
    output$adjusted_utility_explanation <- renderUI({
      HTML(
        "The adjusted utility estimate accounts for real-world financial factors such as variable costs, taxes, and discount rates. This value provides a more realistic estimate of the net benefit of the staffing procedure, reflecting the true economic impact after these adjustments. Use this value for more accurate budgeting and investment decisions."
      )
    })
    # Underlying Math for Adjusted Utility with Unicode/plaintext formatting
    vcost <- input$vcost
    tax <- input$tax
    drate <- input$drate
    # Calculate unadjusted utility for expanded formula
     # Calculate totDelta for adjusted utility
    # Convert percentages to proportions
    varCosts <- -input$vcost/100
    taxProp <- input$tax/100
    discProp <- input$drate/100
    discRat <- 1/(1 + discProp)

    # Calculate other parameters (using inputs from Page 1 and Page 2)
    valid <- input$rxy2 - input$rxy1
    ord <- ux(input$sr)
    SDjp <- input$sdy
    sr1 <- input$sr
    tenure1 <- input$period
    last <- input$pyears
    add <- input$nadd
    subt <- input$nsub
    costOrd <- input$cost1
    costAc <- input$cost2
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    ck <- add * ((costAc - costOrd) / sr1)
    numyr <- tenure1 + last
    
    # Initialize variables
    nk <- 0
    totDelta <- 0
    totDelta1 <- 0
    
    # Calculate adjusted utility within the loop
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    unadjusted_utility <- input$n * input$sdy * (input$rxy2 - input$rxy1) * ux(input$sr) * input$period - (input$cost2 - input$cost1) * (input$n / input$sr)
    adjusted_utility_value <- totDelta
    adjusted_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Adjusted Utility = Utility × (1 - vcost/100) × (1 - tax/100) × (1 / (1 + drate/100))",
      "\n\nWhere:",
      "Utility = ", format_utility(unadjusted_utility), ", vcost = ", vcost, "%, tax = ", tax, "%, drate = ", drate, "%",
      "\n\nStep-by-step:",
      "\n1. Start with unadjusted utility: ", format_utility(unadjusted_utility),
      "\n2. Apply variable cost adjustment: (1 - ", vcost, "/100) = ", round(1 - vcost/100, 4),
      "\n3. Apply tax adjustment: (1 - ", tax, "/100) = ", round(1 - tax/100, 4),
      "\n4. Apply discount rate: (1 / (1 + ", drate, "/100)) = ", round(1 / (1 + drate/100), 4),
      "\n5. Adjusted Utility = ", format_utility(unadjusted_utility), " × ", round(1 - vcost/100, 4), " × ", round(1 - tax/100, 4), " × ", round(1 / (1 + drate/100), 4),
      "\n6. Final Adjusted Utility = ", format_utility(adjusted_utility_value),
      "</pre>"
    )
    output$adjusted_utility_math <- renderUI({
      if (!is.null(adjusted_utility_value)) {
        HTML(adjusted_formula)
      } else {
        NULL
      }
    })
    # Clear other outputs
    output$unadjusted_utility_label <- renderUI({ NULL })
    output$unadjusted_utility_value <- renderUI({ NULL })
    output$unadjusted_utility_explanation <- renderUI({ NULL })
    output$unadjusted_utility_math <- renderUI({ NULL })
    output$flows_utility_label <- renderUI({ NULL })
    output$flows_utility_value <- renderUI({ NULL })
    output$flows_utility_explanation <- renderUI({ NULL })
    output$flows_utility_math <- renderUI({ NULL })
  })

  # Page 3: Account for Flows
  observeEvent(input$go_flows, {
    enable("download_pdf")
    final_utility <- utilityAdjPerYear() # Replace with your actual calculation for flows if needed
    output$flows_utility_label <- renderUI({
      HTML("<b>Final Adjusted Utility Estimate (with Employee Flows)</b>")
    })
    output$flows_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", final_utility, "</span>"))
    })
    output$flows_utility_explanation <- renderUI({
      HTML(
        "This final adjusted utility estimate incorporates the effects of employee flows, such as hiring and turnover, in addition to all previous financial adjustments. It provides the most comprehensive estimate of the net benefit of the staffing procedure, reflecting both economic factors and workforce dynamics. Use this value for strategic planning and to understand the long-term impact of staffing decisions on your organization."
      )
    })
    # Underlying Math for Employee Flows with Unicode/plaintext formatting
    pyears <- input$pyears
    nadd <- input$nadd
    nsub <- input$nsub
    flows_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Final Utility = Adjusted Utility × (Employee Flows Adjustment)",
      "\n\nWhere:",
      "Program Length = ", pyears, ", Employees Added per Year = ", nadd, ", Employees Lost per Year (After Tenure) = ", nsub,
      "\n\nStep-by-step:",
      "\n1. Net change in employees per year: Employees Added per Year - Employees Lost per Year",
      "\n2. Adjust utility for employee inflows and outflows over the program length.",
      "</pre>"
    )
    output$flows_utility_math <- renderUI({
      HTML(flows_formula)
    })
    # Clear other outputs
    output$unadjusted_utility_label <- renderUI({ NULL })
    output$unadjusted_utility_value <- renderUI({ NULL })
    output$unadjusted_utility_explanation <- renderUI({ NULL })
    output$unadjusted_utility_math <- renderUI({ NULL })
    output$adjusted_utility_label <- renderUI({ NULL })
    output$adjusted_utility_value <- renderUI({ NULL })
    output$adjusted_utility_explanation <- renderUI({ NULL })
    output$adjusted_utility_math <- renderUI({ NULL })
  })
}

# Navbar
ui <- navbarPage(
  "UA+",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Comp & Ben Utility", fluidPage(h4(HTML("<b>Compensation & Benefits Utility</b>")), p("This section will provide tools and reports for analyzing the utility of compensation and benefits interventions. (Coming soon.)"))),
  tabPanel("Training Utility", training_ui),
  tabPanel("Glossary", glossary_ui),
  tabPanel("References", reference_ui)
),

shinyApp(ui, main_server)

#Expectancy Function
Expectancyfunc <- function(Validity, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut){
  n <- 1000
  mean <- c(0,0)
  lower <- c(PredLowerCut, CritLowerCut)
  upper <- c(PredUpperCut, CritUpperCut)
  corr <- diag(2)
  corr[lower.tri(corr)] <- Validity
  corr[upper.tri(corr)] <- Validity
  jtprob <- pmvnorm(lower, upper, mean, corr, algorithm = Miwa(steps=128))
  xprob <- pnorm(PredUpperCut, mean = 0, sd = 1) - pnorm(PredLowerCut, mean = 0, sd = 1)
  expectancy <- jtprob/xprob
  return(expectancy[1])
}

# Function for formatting axis labels
dollar_format2 <- function(x) {
  ifelse(abs(x) >= 1e6, paste0("$", round(x/1e6, 1), "M"), 
         ifelse(abs(x) >= 1e3, paste0("$", round(x/1e3, 1), "K"), 
                paste0("$", round(x, 1))))
}

#References
reference_content <- HTML(
  ' <div style="padding-left: 30px; text-indent: -30px;">
    <p>Alexander III, L., Mulfinger, E., & Oswald, F. L. (2019). <i>Investing in People Online</i> (Version 2.0) [Software], Rice University, Houston, Texas. Available from <a  href="https://orgtools.shinyapps.io/IIP3/">https://orgtools.shinyapps.io/IIP3/</a><p>
    <p>Avolio, B. J., Avey, J. B., & Quisenberry, D. (2010). Estimating return on leadership development investment. <i>The Leadership Quarterly, 21</i>(4), 633–644. <a href="https://doi.org/10.1016/j.leaqua.2010.06.006">https://doi.org/10.1016/j.leaqua.2010.06.006</a><p>
    <p>Avolio, B. J., Hannah, S. T., Reichard, R. J., Chan, A., & Walumbwa, F. O. (2009). A meta-analytic review of leadership intervention research: Effects on leader behaviors, attitudes, and effectiveness. <i>The Leadership Quarterly, 20</i>(5), 764–784. <a href="https://doi.org/10.1016/j.leaqua.2009.06.006">https://doi.org/10.1016/j.leaqua.2009.06.006</a><p>
    <p>Carson, K. P., Becker, J. S., & Henderson, J. A. (1998). Is utility really futile? A failure to replicate and an extension. <i>Journal of Applied Psychology, 83</i>(1), 84–96. <a href="https://doi.org/10.1037/0021-9010.83.1.84">https://doi.org/10.1037/0021-9010.83.1.84</a><p>
    <p>Cascio, W. F., Boudreau, J. W., & Fink, A. A. (2019). <i>Investing in people: financial impact of human resource initiatives</i> (3rd ed.). Society for Human Resource Management.<p>
    <p>Cucina, J., Berger, J., & Busciglio, H. (2017). Communicating Criterion-Related Validity Using Expectancy Charts: A New Approach. <i>Personnel Assessment and Decisions, 3</i>(1). <a href="https://doi.org/10.25035/pad.2017.001">https://doi.org/10.25035/pad.2017.001</a><p>
    <p>Latham, G. P., & Whyte, G. (1994). The Futility Of Utility Analysis. <i>Personnel Psychology, 47</i>(1), 31–46. <a href="https://doi.org/10.1111/j.1744-6570.1994.tb02408.x">https://doi.org/10.1111/j.1744-6570.1994.tb02408.x</a><p>
    <p>Magnusson, K. (2023, June 9). A Causal Inference Perspective on Therapist Effects. <a href="https://doi.org/10.31234/osf.io/f7mvz">https://doi.org/10.31234/osf.io/f7mvz</a><p>
    <p>Schmidt, F. L. (2012). The economic value of goal setting to employers. In E. A. Locke & G. P. Latham (Eds.), <i>New developments in goal setting and task performance</i> (pp. 16–20). Routledge/Taylor & Francis Group. <a href="https://doi.org/10.4324/9780203082744">https://doi.org/10.4324/9780203082744</a><p>
    <p>Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. <i>Journal of Management, 26</i>(2), 281–299. <a href="https://doi.org/10.1177/014920630002600206">https://doi.org/10.1177/014920630002600206</a><p>
    <p>Sturman, M. C. (2012). Employee value: Combining utility analysis with strategic human resource management research to yield strong theory. In N. Schmitt (Ed.), <i>The Oxford handbook of personnel assessment and selection</i> (pp. [insert page numbers if available]). Oxford University Press. <a href="https://doi.org/10.1093/oxfordhb/9780199732579.013.0033">https://doi.org/10.1093/oxfordhb/9780199732579.013.0033</a><p>
    <p>Schmidt, F. L. (2013). The economic value of goal setting to employers. In E. A. Locke & G. P. Latham (Eds.), <i>New Developments in Goal Setting and Task Performance</i> (pp. 16–20). Routledge/Taylor & Francis Group.</p>
    </div>'
)

# Opening UI
opening_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Utility Analysis+ App"),
  mainPanel(
    div(
      h4(HTML("<b>Introduction</b>")),
      p("Welcome to the Utility Analysis+ App (UA+). This tool is designed to translate a complex and sophisticated method—utility analysis—into an accessible format for both pedagogical use and practical application. Its core aim is to help estimate the return on investment (ROI) or economic value of employee performance."),
      p("You can explore the app using the navigation tabs above:"),
      tags$ul(
        tags$li("The Staffing Utility, Comp & Ben Utility, and Training Utility tabs offer tools to calculate the financial impact of various HR interventions."),
        tags$li("These tools also generate reports and visualizations to assist in communicating results to key stakeholders."),
        tags$li(HTML('Research shows that the presentation format significantly affects manager acceptance of utility analysis results <a href="https://doi.org/10.1177/014920630002600206">(Sturman, 2000)</a>.'))
      ),
      p("This app supports evidence-based decision-making by delivering clear, practical, and educational outputs."),
      br(),
      h4(HTML("<b>How Utility Analysis (UA) Works (Based on <a href='https://store.shrm.org/Investing-in-People-Financial-Impact-of-Human-Resource-Initiatives-Third-Edition' target='_blank'>Cascio et al., 2019</a>)</b>")),
      p("Utility analysis estimates ROI for HR initiatives such as staffing, training, development, and compensation by considering:"),
      tags$ul(
        tags$li(HTML("<b>Quantity:</b> Number of affected employees and duration of the intervention.")),
        tags$li(HTML("<b>Quality:</b> Effectiveness of the intervention and the value of improved performance.")),
        tags$li(HTML("<b>Cost:</b> Financial investment required to implement the program."))
      ),
      p("The app operationalizes these principles to deliver actionable and educational insights. Given that utility analysis can be applied to evaluate the economic impact of interventions across every part of the human resource budget, this tool provides published examples focused on different components of that budget, including staffing, training, and compensation."),
      br(),
      h4(HTML("<b>Strategically Managing the Human Resource Budget Using UA</b>")),
      p(HTML("Understanding the human resource budget (HRB) as a finite resource is crucial for strategic HR decision-making. All firms must optimize the allocation of this budget across various functions like recruitment (R), training (T), compensation (C), and administration (A) to effectively execute their strategy.")),
      p(HTML("The core insight from <a href='https://www.amazon.com/Financial-Analysis-HR-Managers-paperback/dp/0133925420' target='_blank'>Director (2012)</a> is that money can be moved around the budget to accomplish a firm's goals. For example if unanticipated staffing expenses arise, then money must be pulled from the compensation, training, and administrative portions of the HR budget to fund unanticipated staffing expenses. This underscores how evidence-based methods, such as utility analysis, can inform more effective and efficient allocations of the firm's budget, thereby helping the HR team contribute to the firm's strategic goals.")),
      br(),
      h4(HTML("\U0001F4C2 Code Repository:")),
      p(HTML("Access the app's code on GitHub:<br><a href='https://github.com/utilityanalysis/webApp'>https://github.com/utilityanalysis/webApp</a>"))
    )
  )
)

#Reference UI
reference_ui <- fluidPage(
  titlePanel("References"),
  mainPanel(
    reference_content
  )
)

#Glossary UI
glossary_ui <- fluidPage(
  tags$style(HTML(".size {font-size: 16px}")),
  titlePanel("Glossary"),
  mainPanel(
    p(HTML("<strong>Break-Even SDy</strong> - Minimum possible value of SDy to recoup losses from conducting a staffing/training program."), class = "size"),
    p(HTML("<strong>Cohen's D</strong> - A type of effect size. This value represents the standardized difference in production between a trained and untrained group."), class = "size"),
    p(HTML("<strong>Discount Rate</strong> - Rate of accrual of monetary investments from production revenues."), class = "size"),
    p(HTML("<strong>Effect Size</strong> - Standard Deviation of the performance difference between a treated group and an untreated group."), class = "size"),
    p(HTML("<strong>Expectancy</strong> - A measure of the ability of a test to predict outcomes. For example, a test with high validity has a high expectancy of predicting high performance based on a high test score."), class = "size"),
    p(HTML("<strong>SDp</strong> - Standard deviation of work output among employees as a percentage of mean output."), class = "size"),
    p(HTML("<strong>SDy</strong> - Monetary value of job performance. This value indicates the difference between an average worker (50th percentile) and a worker with high production (85th percentile). Methods for measuring SDy include the 40% rule, Global Estimation, and CREPID among others."), class = "size"),
    p(HTML("<strong>Selection ratio</strong> - Ratio of the number of employees to be hired from an applicant pool."), class = "size"),
    p(HTML("<strong>Tax Rate</strong> - Company tax rate."), class = "size"),
    p(HTML("<strong>Utility</strong> - A measure of the return on investment in workers. Utility compares the quality and quantity of a treated group of employees to the cost of the treatment. This returns an estimation of the economic value that these employees add to the company."), class = "size"),
    p(HTML("<strong>Validity</strong> - Correlation between selection test performance and actual job performance. A validity of 0 indicates no relationship, while a validity of 1.00 indicates a perfect relationship. Validity will almost always be below .50 (Carson et. al. (1998))."), class = "size"),
    p(HTML("<strong>Variable Costs</strong> - Costs that rise with greater production. For example, material costs, incentives, commission, variable overhead, etc."), class = "size")
  )
)



#Training UI
training_ui <- dashboardPage(
  dashboardHeader(title = "Training Utility"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarTrain",
      selected = "training_utility",
      menuItem("Training Utility", tabName = "training_utility", icon = icon("file-lines")),
      menuItem("Effect Size (BESD)", tabName = "effect_size", icon = icon("chart-simple")),
      menuItem("Utility Outputs", tabName = "utility_outputs", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "training_utility",
              fluidPage(
                titlePanel("Training Utility"),
                mainPanel(
                  p("This is the training utility section of the UA+ app. Training utility can be described as a return on investment in the development of workers. This return on development investment (RODI) is calculated using the effect size of the training process. In the panel on the left, there is a tab for effect size that gives a descriptive analysis of the effect size that you choose to input. This consists of a modified graph from Alexander et al's Investing in People Online app, as well as a modified version of Magnusson's description of effect size. This tool is useful for displaying how effect size relates to the productivity of trained versus untrained workers."),
                br(),
                  p("Treated employees are those who have participated in a particular program or intervention."),
                  p("The Utility Outputs tab displays the results of utility analysis as well as a plain text description. This utility analysis takes into account the quality and quantity of the training program and workers and compares this value to the cost of the program. It additionally takes into account economic factors such as variable costs, taxes, and investments.")
                )
              )
      ),
      tabItem(tabName = "effect_size",
              
              fluidPage(
                useShinyjs(),
                titlePanel("Effect Size Description"),
                sidebarLayout(
                  sidebarPanel(
                    id = "sidebar",
                    HTML('<h3>Effect Size Inputs</h3>'),
                    selectInput(
                      "effect_size_dropdown",
                      label = HTML('<b>Effect Size Dropdown Options</b>'),
                      choices = list(
                        "Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)" = 0.35,
                        "Leadership development: Average estimate (Avoilio et al., 2009)" = 0.65,
                        "Leadership development: Intensive program (3+ days) (Avoilio et al., 2009)" = 1.00,
                        "Leadership development: Upper bound (Avoilio et al., 2009)" = 1.37,
                        "Goal setting (Schmidt, 2013)" = 0.46
                      ),
                      selected = 0.35
                    ),
                    numericInput("dTrain2_1", "Input the Effect Size (Cohen's d) Manually", 0),
                    br(),
                    actionButton("goEffect", "Generate Chart")
                    
                  ),
                  mainPanel(
                    
                    plotOutput("training_graph_1"),
                    br(),
                    textOutput("effectText"),
                    downloadButton("effect_download", "Download PDF")
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
                      id = "pageTabs",
                      tabPanel("Page 1",
                               HTML('<h3>Utility Inputs (Page 1)</h3>'),
                               selectInput(
                                 "effect_size_dropdown_utility",
                                 label = HTML('<b>Effect Size Dropdown Options</b>'),
                                 choices = list(
                                   "Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)" = 0.35,
                                   "Leadership development: Average estimate (Avoilio et al., 2009)" = 0.65,
                                   "Leadership development: Intensive program (3+ days) (Avoilio et al., 2009)" = 1.00,
                                   "Leadership development: Upper bound (Avoilio et al., 2009)" = 1.37,
                                   "Goal setting (Schmidt, 2013)" = 0.46
                                 ),
                                 selected = 0.35
                               ),
                               numericInput("dTrain2", "Input the Effect Size (Cohen's d) Manually", 0),
                               numericInput("nTrain", "Number of Employees Trained", 0, min = 0, step = 1),
                               numericInput("tTrain", "Training Effect Duration", 0, min = 0),
                               numericInput("sdyTrain", "SD of Performance in Monetary Units", 0),
                               numericInput("sdP", "SD of Work Output as % of Mean Output", 20, min = 0, max = 100),
                               numericInput("costTrain2", "Cost per Employee of Procedure", 0, min = 0),
                               br(),
                               actionButton("goUn", "Compute Utility(No Adjustments)")
                      ),
                      tabPanel("Page 2",
                               HTML('<h3>Utility Inputs (Page 2)</h3>'),
                               HTML('<h4>Economic Factors</h4>'),
                               numericInput("vrateTrain", "Variable Costs (%)", 35, min = 0, max = 100),
                               numericInput("taxTrain", "Tax Rate (%)", 63, min = 0, max = 100),
                               numericInput("discTrain", "Discount Rate (%)", 11, min = 0, max = 100),
                               HTML('<h4>Employee Flows</h4>'),
                               numericInput("lengthTrain", "Program Length", 15, min = 0),
                               numericInput("addTrain", "Employees Trained per Year", 618, min = 0),
                               numericInput("subTrain", "Loss of Trained Employees per Year (After Effect Duration)", 618, min = 0),
                               actionButton("go4", "Compute Utility")
                      )
                    )
                  ),
                  mainPanel(
                    uiOutput("h3"),
                    textOutput("training_utilityUn"),
                    textOutput("training_utilityOpCost"),
                    textOutput("break_even_train"),
                    uiOutput("h4"),
                    textOutput("training_utility"),
                    textOutput("training_utility_per_employee"),
                    textOutput("training_utility_per_employee_per_year"),
                    br(),
                    textOutput("trainingText1"),
                    br(),
                    textOutput("trainingText2"),
                    br(),
                    textOutput("trainingText3"),
                    downloadButton("training_download", "Export Report")
                  )
                )
              )
      )
    )
  )
)

#Server
main_server <- function(input, output, session) {
  # Utility formatting function available everywhere in server
  format_utility <- function(x) {
    paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
  }
  #global variables
  utilityUn <- reactiveVal()
  utilityUnPer <- reactiveVal()
  utilityUnPerYear <- reactiveVal()
  utilityAdj <- reactiveVal()
  utilityAdjPer <- reactiveVal()
  utilityAdjPerYear <- reactiveVal()
  staffSDy <- reactiveVal()
  costAdj <- reactiveVal()
  elo <- reactiveVal()
  elmo <- reactiveVal()
  eumo <- reactiveVal()
  eto <- reactiveVal()
  eln <- reactiveVal()
  elmn <- reactiveVal()
  eumn <- reactiveVal()
  etn <- reactiveVal()
  bh <- reactiveVal()
  gh <- reactiveVal()
  expPro <- reactiveVal()
  expPro2 <- reactiveVal()
  expDiff <- reactiveVal()
  expDiff2 <- reactiveVal()
  selectedPage <- reactiveVal(1)
  
  #Toggles
  observe({ 
    #Staffing Toggle 1
    toggleState(
      id = "go",
      condition = 
        input$n > 0 &
        input$sdy >= 0 &
        -1 <= input$rxy1  &
        1 >= input$rxy1 &
        -1 <= input$rxy2  &
        1 >= input$rxy2 &
        input$sr > 0 &
        input$cost1 >= 0 &
        input$cost2 >= 0 &
        input$period > 0
    )
    #Staffing Toggle 2
    toggleState(
      id = "go2",
      condition =
        input$vcost >= 0 &
        input$vcost <= 100 &
        input$tax >= 0 &
        input$tax <= 100 &
        input$drate >= 0 &
        input$drate <= 100 &
        input$pyears >0 &
        input$nadd >= 0 &
        input$nsub >= 0
    )
    #Training Toggle 1
    toggleState(
      id = "goEffect",
      condition =
        input$nTrain_1 > 0 &
        input$tTrain_1 > 0 &
        input$sdyTrain_1 > 0 &
        input$costTrain2_1 >= 0
    )
    toggleState(
      id = "monteGo",
      condition =
        input$t1 >= 1 &
        input$t2 >= 1 &
        input$n1 >= 1 &
        input$n2 >= 1 &
        input$sel1 >= 1 &
        input$sel2 >= 1 &
        input$r1 >= -1 &
        input$r1 <= 1 &
        input$r2 >= -1 &
        input$r2 <= 1 &
        input$sdy1 >= 0 &
        input$sdy2 >= 0 &
        input$costm1 >= 0 &
        input$costm2 >= 0 &
        input$i1 >= 0 &
        input$i2 >= 0 &
        input$vc1 >= 0 &
        input$vc2 >= 0 &
        input$tax1 >= 0 &
        input$tax2 >= 0 &
        input$ir1 >= -1 &
        input$ir1 <= 1 &
        input$ir2 >= -1 &
        input$ir2 <= 1 &
        input$pa1 >= 0 &
        input$pa1 <= 1 &
        input$pa2 >= 0 &
        input$pa2 <= 1 &
        input$bxy1 >= -1 &
        input$bxy1 <= 1 &
        input$bxy2 >= -1 &
        input$bxy2 <= 1 &
        input$stab1 >= 0 &
        input$stab1 <= 1 &
        input$stab2 >= 0 &
        input$stab2 <= 1 &
        input$pto1 >= 0 &
        input$pto1 <= 1 &
        input$pto2 >= 0 &
        input$pto2 <= 1 &
        input$corrto1 >= -1 &
        input$corrto1 <= 1 &
        input$corrto2 >= -1 &
        input$corrto2 <= 1
    )
    
    if (!isTruthy(input$go)) {
      disable("download_plot")
    }
    if (!isTruthy(input$go2)) {
      disable("download_pdf")
    }
    if (!isTruthy(input$goEffect)) {
      disable("effect_download")
    }
    if (!isTruthy(input$go4)) {
      disable("training_download")
    }
    if (!isTruthy(input$goSchmidt)) {
      disable("sdP")
    }
  })
  #Staffing Utility  
  observeEvent(input$go, {
    enable("download_pdf")
    n <- input$n
    period <- input$period
    # Utility Calculation (Unadjusted)
    utility_value <- utilityBcg(
      n = (input$n),
      sdy = input$sdy,
      rxy = (input$rxy2 - input$rxy1),
      uxs = ux(input$sr),
      sr = input$sr,
      pux = NULL,
      cost = ((input$cost2 - input$cost1) * (input$n / input$sr)),
      period = input$period
    )
    # Compute Break Even SDy
    breakEvenSDy <- (
      ((input$cost2)*(input$n/input$sr))/(input$n*(input$rxy2)*input$period*ux(input$sr))
    )
    
    # Compute old top expectancy
    expectancyTopOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+.67 , Inf, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute old upper middle expectancy
    expectancyUMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #Compute old lower middle expectancy
    expectancyLMOld <- 100*round(Expectancyfunc(input$rxy1, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    #compute old lower expectancy
    expectancyLowOld <- 100*round(Expectancyfunc(input$rxy1, -Inf , ux(input$sr)-.67, input$rxy1*(ux(input$sr))+.67, Inf),2)
    
    # Compute new top expectancy
    expectancyTopNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+.67 , Inf, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    # Compute new upper middle expectancy
    expectancyUMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)+0 , ux(input$sr)+.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #Compute new lower middle expectancy
    expectancyLMNew <- 100*round(Expectancyfunc(input$rxy2, ux(input$sr)-.67 , ux(input$sr)+0, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #compute new lower expectancy
    expectancyLowNew <- 100*round(Expectancyfunc(input$rxy2, -Inf , ux(input$sr)-.67, input$rxy2*(ux(input$sr))+.67, Inf),2)
    
    #Calculate increase in good hires/ decrease in bad hires
    badHire <- round(((expectancyLowOld-expectancyLowNew)/expectancyLowOld)*100, 0)
    goodHire <- round(((expectancyTopNew-expectancyTopOld)/expectancyTopOld)*100, 0)
    bh(badHire)
    gh(goodHire)
    
    #assign local variables to global variables
    elo(expectancyLowOld)
    elmo(expectancyLMOld)
    eumo(expectancyUMOld)
    eto(expectancyTopOld)
    eln(expectancyLowNew)
    elmn(expectancyLMNew)
    eumn(expectancyUMNew)
    etn(expectancyTopNew)
    
    # Calculate the per-hire utility
    per_hire_utility <- utility_value / n
    
    # Calculate the per-hire per-year utility
    per_year_utility <- per_hire_utility / period
    
    # Format
    formatted_total_utility <- format_utility(utility_value)
    utilityUn(formatted_total_utility)
    formatted_per_hire_utility <- format_utility(per_hire_utility)
    utilityUnPer(formatted_per_hire_utility)
    formatted_per_year_utility <- format_utility(per_year_utility)
    utilityUnPerYear(formatted_per_year_utility)
    formatted_breakEvenSDy <- format_utility(breakEvenSDy)
    staffSDy(formatted_breakEvenSDy)
    
    #If else for procedure with better results
    exp_diff <- if (expectancyTopNew > expectancyTopOld) {
      expectancyTopNew - expectancyTopOld
    }
    else {
      expectancyTopOld - expectancyTopNew
    }
    expDiff(exp_diff)
    exp_diff2 <- if (expectancyLowNew > expectancyLowOld) {
      expectancyTopNew - expectancyTopOl
    }
    else {
      expectancyLowOld - expectancyLowNew
    }
    expDiff2(exp_diff2)
    exp_procedure <- if (expectancyTopNew > expectancyTopOld) {
      "new procedure"
    }
    else {
      "old procedure"
    }
    expPro(exp_procedure)
    exp_procedure2 <- if (expectancyLowNew > expectancyLowOld) {
      "increases"
    }
    else {
      "decreases"
    }
    expPro2 <- exp_procedure2
    
    #expectancy chart
    
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),  # Specify the order of levels
        Probability = c(
          expectancyLowOld, expectancyLowNew,
          expectancyLMOld, expectancyLMNew,
          expectancyUMOld, expectancyUMNew,
          expectancyTopOld, expectancyTopNew
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      improvement <- goodHire  # percent improvement
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "Quartile Score on Selection Procedure(s)/Program(s)",
          y = "Probability of High Job Performance",
          fill = "Procedure(s)/Program(s)",
          title = "Figure 1. Comparing old and new procedure(s)/program(s)",
          subtitle = paste0("The new procedure(s)/program(s) improves prediction of high performance by ", improvement, "%.")
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#6BAED6", "New" = "#2171B5")) +
        theme_minimal() +  
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16, lineheight = 1.2),
          plot.subtitle = element_text(hjust = 0, size = 14, margin = margin(b = 10)),
          axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = .5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          axis.line.x = element_line(size = 1, colour = 'black'),
          axis.line.y = element_line(size = 1, colour = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 6, position = position_dodge(width = 0.9)))
    })
    output$utility_graph <- renderPlot({
      plot_data()
    })
    
    #Plain Text
    output$expectancy_text <- renderText({
      paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", goodHire, "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", badHire, "% [(",elo(), " - ",eln(), ")/", elo() ,"].")
    })
    
    #Export PDF
    output$download_plot <- downloadHandler(
      filename = function() {
        "expectancy.pdf"  
      },
      
      content = function(file) {
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", goodHire, "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", badHire, "% [(",elo(), " - ",eln(), ")/", elo() ,"]."), width = 90, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 14
            ),
            plot.margin = margin(1,1,1,1, "in")
          )
        
        
        ggsave(
          file,
          plot,
          device = "pdf",
          width = 10.625,  
          height = 13.75,  
          units = "in",  
          dpi = 300  
        )
      }
    )
    
    # Unadjusted utility outputs
    output$unadjusted_utility_label <- renderUI({
      HTML("<b>Unadjusted Utility Estimate</b>")
    })
    output$unadjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityUn(), "</span>"))
    })
    output$unadjusted_utility_explanation <- renderUI({
      HTML(
        "The unadjusted utility estimate represents the total net financial benefit of using the new staffing procedure, before accounting for taxes, variable costs, or other economic adjustments. It is calculated as the increase in value from improved selection minus the additional costs of the new procedure. This value helps you understand the raw financial impact of switching to the new procedure, and is useful for comparing the basic ROI of different staffing strategies before considering real-world financial factors."
      )
    })
    # Underlying Math with Unicode/plaintext formatting
    n <- input$n
    sdy <- input$sdy
    rxy1 <- input$rxy1
    rxy2 <- input$rxy2
    sr <- input$sr
    cost1 <- input$cost1
    cost2 <- input$cost2
    period <- input$period
    delta_rxy <- rxy2 - rxy1
    uxs <- ux(sr)
    cost_diff <- (cost2 - cost1) * (n / sr)
    final_utility <- n * sdy * delta_rxy * uxs * period - cost_diff
    utility_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Utility = n × sdy × (rxy₂ - rxy₁) × ux(sr) × period - (cost₂ - cost₁) × (n / sr)",
      "\n\nWhere:",
      "n = ", n, ", sdy = ", sdy, ", rxy₂ = ", rxy2, ", rxy₁ = ", rxy1, ", ux(sr) = ", round(uxs, 4), ", period = ", period, ", cost₂ = ", cost2, ", cost₁ = ", cost1, ", sr = ", sr,
      "\n\nStep-by-step:",
      "\n1. Difference in validity: rxy₂ - rxy₁ = ", delta_rxy,
      "\n2. Cost difference: (cost₂ - cost₁) × (n / sr) = ", round(cost_diff, 2),
      "\n3. Utility = ", n, " × ", sdy, " × ", delta_rxy, " × ", round(uxs, 4), " × ", period, " - ", round(cost_diff, 2),
      "\n4. Final Utility = ", format_utility(final_utility),
      "</pre>"
    )
    output$unadjusted_utility_math <- renderUI({
      HTML(utility_formula)
    })
  })
  
  #Utility Tab
  observeEvent(input$go2,{
    enable("download_pdf")
    varCosts <- -input$vcost/100
    tax <- input$tax
    disc <- input$drate
    costOrd <- input$cost1
    costAc <- input$cost2
    validOrd <- input$rxy1
    validAc <- input$rxy2
    SDjp <- input$sdy
    sr1  <- input$sr
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- input$period
    last <- input$pyears
    add <- input$nadd
    subt <- input$nsub
    
    discProp <- disc / 100
    valid <- validAc - validOrd
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
    numyr <- tenure1 + last
    
    nk <- 0
    totDelta <- 0
    totDelta1 <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    adjusted_utility <- totDelta
    adjusted_utility_value <- adjusted_utility
    adjusted_utility_perHire <- totDelta / (last*add)
    adjusted_utility_perHire_perYear <- (totDelta / (last * add * tenure1))
    
    # Format the utility values
    formatted_adjusted_utility <- format_utility(adjusted_utility)
    formatted_adjusted_per_hire_utility <- format_utility(adjusted_utility_perHire)
    formatted_adjusted_per_year_utility <- format_utility(adjusted_utility_perHire_perYear)
    formatted_cost_adjustment <- format_utility(totDelta1/last)
    
    output$h4 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')
    })
    
    output$adjusted_utility <- renderText({
      paste("Failing to use this program will have total opportunity costs to the company of:", 
            formatted_adjusted_utility)
    })
    
    output$adjusted_utility_perHire <- renderText({
      paste("Failing to use this program will have opportunity costs to the company of:", 
            formatted_adjusted_per_hire_utility,
            "per hire.")
    })
    
    output$adjusted_utility_perHire_perYear <- renderText({
      paste("Failing to use this program will have opportunity costs to the company of:", 
            formatted_adjusted_per_year_utility,
            "per hire per year they stay.")
    })
    
    output$cost_adjustment <- renderText({
      paste("The cost adjustment factor is:", formatted_cost_adjustment)
    })
    
    # Display
    
    output$h1 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')
    })
    
    output$h2 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Adjusted Values</span>')
    })
    
    output$total_utility <- renderText({
      paste("The opportunity costs of failing to use the program are:", 
            utilityUn())
    })
    output$break_even_SDy <- renderText({
      paste("The break-even value for Standard Deviation of the new procedure is:", 
            staffSDy())
    })
    
    #If Else for larger utility
    lowHigh <- if(input$rxy1 > input$rxy2) {
      "old"
    }
    else{
      "new"
    }
    ls <- if(input$rxy1 > input$rxy2) {
      "smaller"
    }
    else{
      "larger"
    }
    #Plain text
    staff1 <- paste0("In order to assess the effectiveness of new staffing procedures, we have conducted an analysis comparing the quality and quantity of new hires to the cost of the selection processes. As part of this analysis, we have found the validity for each of the staffing procedures. Our research indicates that the new procedure will have an operational validity of ", input$rxy2, ", which is ", ls, " than the operational validity of ", input$rxy1, " defining the old procedure. This means that the ", lowHigh, " procedure more accurately avoids bad hires.")
    staff2 <- paste0("Our cost analysis found that costs associated with administering the new procedure are $", input$cost2, " per applicant, in comparison to the cost of the old procedure which is $", input$cost1, " per applicant. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of $", input$sdy, ". The first is a variable cost adjustment and refers to costs that increase with hiring higher quality talent, such as compensation enhancements. These are expected to offset opportunity costs associated with this program by ", input$vcost, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$tax, "%. Lastly, the cash value of increased performance over time must be discounted to the present to approximate the net present value of the program. The discount rate applied to this program is ", input$drate, "%. Since we will hire ", input$nadd, " new employees from a large selection pool each year, and the new procedure is expected to be used for at least ", input$pyears," years, implementing the new program is expected to cost our company ", costAdj(), " per year.")
    staff3 <- paste0("We found that opportunity cost is also affected by employee flows. When employees selected through the program enter and exit our company, the costs and benefits associated with the program change over time. Our research shows that employees selected using this program are expected to stay with the company an average of ", input$period, " years. This means that when we do not hire using this procedure, the opportunity cost associated with each employee is ", utilityAdjPer(), " or ", utilityAdjPerYear(), " per year they stay. Considering this along with all previous factors, our analysis has found that failing to implement this program will result in a total opportunity cost of ", utilityAdj(), ".")
    
    s1wrap <- strwrap(staff1, width = 97, indent = 8, simplify = TRUE)
    s1wrap <- paste(s1wrap, collapse = "\n")
    s2wrap <- strwrap(staff2, width = 97, indent = 8, simplify = TRUE)
    s2wrap <- paste(s2wrap, collapse = "\n")
    s3wrap <- strwrap(staff3, width = 97, indent = 8, simplify = TRUE)
    s3wrap <- paste(s3wrap, collapse = "\n")
    
    output$plainText1 <- renderText({
      staff1
    })
    output$plainText2 <- renderText({
      staff2
    })
    output$plainText3 <- renderText({
      staff3
    })
    
    #assign global variables to local variables
    elo1 <- elo()
    eln1 <- eln()
    elmo1 <- elmo()
    elmn1 <- elmn()
    eumo1 <- eumo()
    eumn1 <- eumn()
    eto1 <- eto()
    etn1 <- etn()
    
    #create expectancy chart used in pdf export
    plot_data <- reactive({
      bar_data <- data.frame(
        Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
        Procedure = factor(c("Old", "New"), levels = c("Old", "New")),
        Probability = c(
          elo1, eln1,
          elmo1, elmn1,
          eumo1, eumn1,
          eto1, etn1
        )
      )
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      title_text <- paste0("Using the ", expPro(), " improves the probability of acquiring a high performer by ", gh(), " percent.")
      wrapped_title <- strwrap(title_text, width = 100, simplify = TRUE)
      wrapped_title <- paste(wrapped_title, collapse = "\n")
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "Quartile Score on Selection Procedure(s)/Program(s)",
          y = "Probability of High Job Performance",
          fill = "Procedure(s)/Program(s)"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#6BAED6", "New" = "#2171B5")) +
        theme_minimal() +  # Customize the plot theme (optional)
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 12, lineheight = 1.2),
          axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = .5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          axis.line.x = element_line(size = 1, colour = 'black'),
          axis.line.y = element_line(size = 1, colour = 'black'),
          panel.grid.minor = element_blank()
        ) +
        ggtitle(paste0("Figure 1\n", wrapped_title))
      
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 6, position = position_dodge(width = 0.9)))
    })
    
    #pdf export
    output$download_pdf <- downloadHandler(
      filename = function() {
        "staffing_report.pdf"
      },
      
      content = function(file) {
        
        title <- textGrob(
          "Staffing Report",
          x = 0.5, y = unit(0.5, "in"),
          gp = gpar(fontsize = 20),
        )
        
        par1 <- textGrob(
          s1wrap,
          x = unit(1, "in"), y = 0.62,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        par2 <- textGrob(
          s2wrap,
          x = unit(1, "in"), y = -0.02,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        par3 <- textGrob(
          s3wrap,
          x = unit(1, "in"), y = -0.76,
          gp = gpar(fontsize = 14),
          hjust = 0
        )
        
        capWrap <- strwrap(paste0("The above chart compares the expectancy of the new staffing procedure to the old staffing procedure. Expectancy is a 
             measure of the ability of a test to predict outcomes. Using the new procedure improves the chance that we acquire a high 
             performer by ", gh(), "% [(",etn(), " - ",eto(), ")/", eto() ,"] and avoid a bad hire by ", bh(), "% [(",elo(), " - ",eln(), ")/", elo() ,"]."), width = 100, simplify = TRUE)
        capWrap <- paste(capWrap, collapse = "\n")
        # Create the ggplot2 plot with a caption
        plot <- plot_data() +
          labs(
            caption = capWrap
          ) +
          theme(
            plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 10, unit = "pt"),
              size = 12
            ),
            plot.margin = margin(2.25,1,1,1, "in")
          )
        
        plot_text <- arrangeGrob(title, par1, par2, par3, plot, nrow= 5, heights = unit(c(1, 1, 1, 1, 5), "null"))
        
        ggsave(
          file,
          plot_text,
          device = "pdf",
          width = 10.625,  
          height = 13.75,  
          units = "in",  
          dpi = 300  
        )
      }
    )
    
    # Underlying Math for Adjusted Utility
    vcost <- input$vcost
    tax <- input$tax
    drate <- input$drate
    # Compute the adjusted utility value as in the main calculation
    adjusted_utility_value <- adjusted_utility
    adjusted_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Adjusted Utility = Utility × (1 - vcost/100) × (1 - tax/100) × (1 / (1 + drate/100))",
      "\n\nWhere:",
      "vcost = ", vcost, "%", ", tax = ", tax, "%", ", drate = ", drate, "%",
      "\n\nStep-by-step:",
      "\n1. Apply variable cost adjustment: (1 - vcost/100)",
      "\n2. Apply tax adjustment: (1 - tax/100)",
      "\n3. Apply discount rate: (1 / (1 + drate/100))",
      "\n4. Final Adjusted Utility = ", format_utility(adjusted_utility_value),
      "</pre>"
    )
    output$adjusted_utility_math <- renderUI({
      if (!is.null(adjusted_utility_value)) {
        HTML(adjusted_formula)
      } else {
        NULL
      }
    })
  })
  
###Monte Carlo###
observeEvent(input$monteGo, {
  showModal(modalDialog("Please wait while the analysis completes.",
                        easyClose = FALSE,
                        footer = NULL))
  set.seed(input$seed)
  ##### Step 1: Build basic utility formula parameters (adjustment 0 in Sturman table 1) #####
  # Define the population of parameters to randomly sample from
  t = runif(10000, input$t1, input$t2)                               # expected tenure of staying in years
  n = runif(10000, log(input$n1), log(input$n2))       # Create a sequence from 1 to 1100 in increments of 1 for n         # 1-50 applicants per hire
  select = runif(10000, input$sel1, input$sel2)          # for example, from 0.01 to 1 in increments of 0.01
  sdy = runif(10000, input$sdy1, input$sdy2)            # standard deviation of job performance value
  if(input$costm1 == 0){
    cost = runif(10000, 0, log(input$costm2))}
  else{
    cost = runif(10000, log(input$costm1), log(input$costm2))
  }    # cost per applicant of operating the new selection program
  r1 = runif(10000, input$r1, input$r2)            # operational validity of the new selection program
  utility_data <- data.frame(
    t = t,
    n = exp(n),
    select = 1/select,
    sdy = sdy,
    cost = exp(cost),
    r1 = r1
  )
  
  
  utility_data$unadjusted_cost <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    cost <- as.numeric(row["cost"])
    select <- as.numeric(row["select"])
    return(as.numeric(round(n * cost / select, 0)))
  })
  
  utility_data$unadjusted_utility <- apply(utility_data, 1, function(row){
    t <- as.numeric(row["t"])
    n <- as.numeric(row["n"])
    select <- as.numeric(row["select"])
    r1 <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    unadjusted_cost <- as.numeric(row["unadjusted_cost"])
    return(as.numeric((t * n * iopsych::ux(select) * r1 * sdy) - unadjusted_cost))
  })
  
  # Create a data frame for the plot
  unadjusted_utility_df <- data.frame(Unadjusted_Utility_Total = utility_data$unadjusted_utility)
  
  # Calculate density
  density_df_un <- data.frame(x = density(unadjusted_utility_df$Unadjusted_Utility_Total)$x,
                           y = density(unadjusted_utility_df$Unadjusted_Utility_Total)$y)

  density_df_un$group <- "Line 1"
  
  ##### Step 2: Account for # Economic adjustments (adjustment 1 in Sturman table 1).  #####
  # Variables for adjustment
  i = runif(10000, input$i1, input$i2)          # discount rate
  tax = runif(10000, input$tax1, input$tax2)        # marginal tax rate
  vc = runif(10000, input$vc1, input$vc2)          # variable costs
  
  # Place these values into the dataset that we built. Sample random values for each row in the dataset
  utility_data$i <- i
  utility_data$tax <- tax
  utility_data$vc <- vc
  
  # build adjustment formula for econ
  adjusted_utility_econ <- function(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t) {
    return ((n*x2*r1*uxs*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
  }
  
  # compute adjusted utility for each row
  utility_data$adjusted_utility_econ <- apply(utility_data, 1, function(row) {
    n <- as.numeric(row["n"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    t <- as.numeric(row["t"])
    r1 <- as.numeric(row["r1"])
    i <- as.numeric(row["i"])
    tax <- as.numeric(row["tax"])
    vc <- as.numeric(row["vc"])
    x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
    uxs = iopsych::ux(sr)
    return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
  })
  
  # Create a data frame for the plot
 adjusted_utility_df_econ <- data.frame(Adjusted_Utility_Total_econ = utility_data$adjusted_utility_econ)
  
  # Calculate density
  density_df_econ <- data.frame(x = density(adjusted_utility_df_econ$Adjusted_Utility_Total_econ)$x,
                              y = density(adjusted_utility_df_econ$Adjusted_Utility_Total_econ)$y)
  
  density_df_econ$group <- "Line 2"
  
  #compute effect
  utility_data$percent_decrease_econ <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_econ"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  # View the first few rows of the updated dataset
  mean.econ <- mean(utility_data$percent_decrease_econ)
  median.econ <- median(utility_data$percent_decrease_econ)
  
  ##### Step 3: Account for # Multiple Devices (adjustment 5 in Sturman table 1).  #####
  
  incremental_r = runif(10000, input$ir1, input$ir2)
  utility_data$incremental_r <- incremental_r
  utility <- function(n, t, r, sdy, sr, cost) {
    return(n*t*r*iopsych::ux(sr)*sdy-round((n/sr)*cost, 0))
  }
  
  #compute adjusted utility for multiple devices
  utility_data$adjusted_utility_mult <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["incremental_r"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    return(as.numeric(utility(n, t, r, sdy, sr, cost)))
  })
  
  #compute effect
  utility_data$percent_decrease_mult <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_mult"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.mult <- mean(utility_data$percent_decrease_mult)
  median.mult <- median(utility_data$percent_decrease_mult)
  
  ##### Step 4: Account for # Deviations from Top-Down Hiring (adjustment 6 in Sturman table 1).  #####
  pa <- runif(10000, input$pa1, input$pa2) # initial acceptance rate
  bxy <- runif(10000, input$bxy1, input$bxy2) # correlation between acceptance and performance
  utility_data$initial_accept <- pa
  utility_data$perf_corr <- bxy
  utility.topdown <- function(n, t, r, pa, bxy, sdy, sr, cost) {
    p2 <- ((n-(pa*n))/(n/sr))+sr
    if (p2 > 1){
      p2 <- 1
    }
    z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
    return(n*t*r*z*sdy-round((n/sr)*cost, 0))
  }
  
  #compute adjusted utility for deviations from top down hiring
  
  utility_data$adjusted_utility_topdown <-  apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    pa <- as.numeric(row["initial_accept"])
    bxy <- as.numeric(row["perf_corr"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    return(as.numeric(utility.topdown(n, t, r, pa, bxy, sdy, sr, cost)))
  })
  
  #compute effect
  utility_data$decrease_topdown <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_topdown"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.topdown <- mean(utility_data$decrease_topdown)
  median.topdown <- median(utility_data$decrease_topdown)
  
  ##### Step 5: Account for # Probationary Period (adjustment 4 in Sturman table 1).  #####
  
  rc <- runif(10000, input$rc1, input$rc2) # cutoff score
  utility_data$cutoff_score <- rc
  
  utility.probation <- function(n, t, r, sr, sdy, rc, cost) {
    xc <- qnorm(1-sr)
    PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
    PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
    PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
    mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
    sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
    so <- pnorm(rc, lower.tail = FALSE)
    mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
    return((n*sdy*r*ux(sr)+(t-1)*n*sp*sdy*mur.xcrc-(n/sr)*cost)-((t-1)*n*sdy*so*mur.rc))
  }
  
  utility_data$adjusted_utility_probation <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    rc <- as.numeric(row["cutoff_score"])
    return(as.numeric(utility.probation(n,t,r,sr,sdy,rc,cost)))
  })
  
  #compute effect
  utility_data$decrease_probation <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_probation"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.probation <- mean(utility_data$decrease_probation)
  median.probation <- median(utility_data$decrease_probation)
  
  ##### Step 6: Account for # Employee Flows (adjustment 2 in Sturman table 1).  #####
  pto <- runif(10000, input$pto1, input$pto2) # turnover probability
  corrto <- runif(10000, input$corrto1, input$corrto2) # turnover performance correlation
  
  utility_data$turnover_probability <- pto
  utility_data$performance_correlation_turnover <- corrto
  
  utility.flow <- function(n, t, r, sdy, pto, corrto, sr, cost){
    num_years <- seq(1, t)
    n_added <- c(rep(n, 1), rep(0, t - 1))
    n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
    n_cum <- cumsum(n_added - n_turn)
    zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
    
    #Financially Adjusted Costs of Program in period (year)
    c_adj <- (n_added / sr) * cost
    unadjusted_utility <- n_cum * r * zbarx * sdy
    
    adjusted_utility <- unadjusted_utility - c_adj
    adjusted_utility <- sum(round(adjusted_utility, 0))
    return(adjusted_utility)
  }
  
  utility_data$adjusted_utility_flow <- apply(utility_data, 1, function(row){
    n <- as.numeric(row["n"])
    t <- as.numeric(row["t"])
    r <- as.numeric(row["r1"])
    sdy <- as.numeric(row["sdy"])
    sr <- as.numeric(row["select"])
    cost <- as.numeric(row["cost"])
    pto <- as.numeric(row["turnover_probability"])
    corrto <- as.numeric(row["performance_correlation_turnover"])
    return(as.numeric(utility.flow(n, t, r, sdy, pto, corrto, sr, cost)))
  })
  
  #compute effect
  utility_data$decrease_flow <- apply(utility_data, 1, function(row){
    change <- (row["unadjusted_utility"]-row["adjusted_utility_flow"])/abs(row["unadjusted_utility"])
    return(as.numeric(100*change))
  })
  
  mean.flow <- mean(utility_data$decrease_flow)
  median.flow <- median(utility_data$decrease_flow)
    
    ##### Step 7: Account for # Temporal Validity (adjustment 2 in Sturman table 1). #####
    
    stab <- runif(10000, input$stab1, input$stab2) #stability of performance
    utility_data$performance_stability <- stab
    
    utility_data$adjusted_utility_temp <- apply(utility_data, 1, function(row){
      t <- as.numeric(row["t"])
      n <- as.numeric(row["n"])
      select <- as.numeric(row["select"])
      r1 <- as.numeric(row["r1"])
      sdy <- as.numeric(row["sdy"])
      unadjusted_cost <- as.numeric(row["unadjusted_cost"])
      stab <- as.numeric(row["performance_stability"])
      return(as.numeric((t * n * stab * ux(select) * r1 * sdy) - unadjusted_cost))
    })
    
    #compute effect
    utility_data$decrease_temp <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_temp"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    mean.temp <- mean(utility_data$decrease_temp)
    median.temp <- median(utility_data$decrease_temp)
    
    #economic variables + multiple devices
    
    utility_data$adjusted_utility_econ_mult <- apply(utility_data, 1, function(row) {
      n <- as.numeric(row["n"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      i <- as.numeric(row["i"])
      t <- as.numeric(row["t"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      r1 <- as.numeric(row["incremental_r"])
      x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
      uxs = ux(sr)
      return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
    })
    
    # Create a data frame for the plot
    adjusted_utility_df_econmult <- data.frame(Adjusted_Utility_Total_econmult = utility_data$adjusted_utility_econ_mult)
    
    # Calculate density
    density_df_econmult <- data.frame(x = density(adjusted_utility_df_econmult$Adjusted_Utility_Total_econmult)$x,
                                      y = density(adjusted_utility_df_econmult$Adjusted_Utility_Total_econmult)$y)
    
    density_df_econmult$group <- "Line 3"
    
    #compute effect
    utility_data$decrease_econmult <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over1 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_mult"])/abs(row["adjusted_utility_econ"])
      return(as.numeric(100*change))
    })
    
    mean.econmult <- mean(utility_data$decrease_econmult)
    median.econmult <- median(utility_data$decrease_econmult)
    mean.over1 <- mean(utility_data$decrease_over1)
    median.over1 <- median(utility_data$decrease_over1)
    
    #economic variables + temporal validity
    
    utility_data$adjusted_utility_econ_tv <- apply(utility_data, 1, function(row) {
      n <- as.numeric(row["n"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      i <- as.numeric(row["i"])
      t <- as.numeric(row["t"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      r1 <- as.numeric(row["r1"])
      x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
      stab = as.numeric(row["performance_stability"])
      uxs = ux(sr)*stab
      return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
    })
    
    #compute effect
    utility_data$decrease_econtv <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_tv"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over1.2 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_tv"])/abs(row["adjusted_utility_econ"])
      return(as.numeric(100*change))
    })
    
    mean.econtv <- mean(utility_data$decrease_econtv)
    median.econtv <- median(utility_data$decrease_econtv)
    mean.over1.2 <- mean(utility_data$decrease_over1.2)
    median.over1.2 <- median(utility_data$decrease_over1.2)
    
    #economic variables + top down hiring
    
    utility.econtd <- function(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc){
      p2 <- ((n-(pa*n))/(n/sr))+sr
      if (p2 > 1){
        p2 <- 1
      }
      z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
      x2 <- (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
      return ((n*x2*r*z*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
    }
    
    utility_data$adjusted_utility_econ_td <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["r1"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      pa <- as.numeric(row["initial_accept"])
      bxy <- as.numeric(row["perf_corr"])
      return(utility.econtd(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc))
    })
    
    #compute effect
    utility_data$decrease_econ_td <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_td"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over1.3 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_td"])/abs(row["adjusted_utility_econ"])
      return(as.numeric(100*change))
    })
    
    mean.econtd <- mean(utility_data$decrease_econ_td)
    median.econtd <- median(utility_data$decrease_econ_td)
    mean.over1.3 <- mean(utility_data$decrease_over1.3)
    median.over1.3 <- median(utility_data$decrease_over1.3)
    
    #economic variables + probation
    
    utility.econ.probation <- function(n, t, r, sr, sdy, rc, cost, i, vc, tax) {
      xc <- qnorm(1-sr)
      PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
      PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
      PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
      mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
      sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
      so <- pnorm(rc, lower.tail = FALSE)
      mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
      x1 <- 1/(1+i)
      x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
      return((n*x1*sdy*r*ux(sr)*(1-tax)*(1-vc)+x2*n*sp*sdy*mur.xcrc*(1-vc)*(1-tax)-(n/sr)*cost*(1-tax))-(x2*n*sdy*so*mur.rc*(1-vc)*(1-tax)))
    }
    
    utility_data$adjusted_utility_econ_prob <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["r1"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      rc <- as.numeric(row["cutoff_score"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      return(as.numeric(utility.econ.probation(n,t,r,sr,sdy,rc,cost, i, vc, tax)))
    })
    
    #compute effect
    utility_data$decrease_econ_prob <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_prob"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over1.4 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_prob"])/abs(row["adjusted_utility_econ"])
      return(as.numeric(100*change))
    })
  
  mean.econprob <- mean(utility_data$decrease_econ_prob)
    median.econprob <- median(utility_data$decrease_econ_prob)
    mean.over1.4 <- mean(utility_data$decrease_over1.4)
    median.over1.4 <- median(utility_data$decrease_over1.4)
    
    #economic variables + employee flows
    
    utility.econ.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax){
      num_years <- seq(1, t)
      n_added <- c(rep(n, 1), rep(0, t - 1))
      n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
      n_cum <- cumsum(n_added - n_turn)
      zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
      discount_factor <- 1/(1 + i)^num_years
      
      #Financially Adjusted Costs of Program in period (year)
      c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
      unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
      
      adjusted_utility <- unadjusted_utility - c_adj
      adjusted_utility <- sum(round(adjusted_utility, 0))
      return(adjusted_utility)
    }
    
    utility_data$adjusted_utility_econ_flow <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["r1"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      pto <- as.numeric(row["turnover_probability"])
      corrto <- as.numeric(row["performance_correlation_turnover"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      return(as.numeric(utility.econ.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax)))
    })
    
    #compute effect
    utility_data$decrease_econ_flow <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_flow"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over1.5 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ"]-row["adjusted_utility_econ_flow"])/abs(row["adjusted_utility_econ"])
      return(as.numeric(100*change))
    })
    
    mean.econflow <- mean(utility_data$decrease_econ_flow)
    median.econflow <- median(utility_data$decrease_econ_flow)
    mean.over1.5 <- mean(utility_data$decrease_over1.5)
    median.over1.5 <- median(utility_data$decrease_over1.5)
    
    #economic variables + multiple devices + temporal validity
    
    utility_data$adjusted_utility_econ_mult_tv <- apply(utility_data, 1, function(row) {
      n <- as.numeric(row["n"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      i <- as.numeric(row["i"])
      t <- as.numeric(row["t"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      r1 <- as.numeric(row["incremental_r"])
      x2 = (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
      stab = as.numeric(row["performance_stability"])
      uxs = ux(sr)*stab
      return(as.numeric(adjusted_utility_econ(n, x2, r1, uxs, sdy, vc, tax, sr, cost, i, t)))
    })
    
    # Create a data frame for the plot
    adjusted_utility_df_econmulttv <- data.frame(Adjusted_Utility_Total_econmulttv = utility_data$adjusted_utility_econ_mult_tv)
    
    # Calculate density
    density_df_econmulttv <- data.frame(x = density(adjusted_utility_df_econmulttv$Adjusted_Utility_Total_econmulttv)$x,
                                        y = density(adjusted_utility_df_econmulttv$Adjusted_Utility_Total_econmulttv)$y)
    
    density_df_econmulttv$group <- "Line 4"
    
    #compute effect
    utility_data$decrease_econmulttv <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over2 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_tv"])/abs(row["adjusted_utility_econ_mult"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttv <- mean(utility_data$decrease_econmulttv)
    median.econmulttv <- median(utility_data$decrease_econmulttv)
    mean.over2 <- mean(utility_data$decrease_over2)
    median.over2 <- median(utility_data$decrease_over2)
    
    #economic variables + multiple devices + top down
    
    utility_data$adjusted_utility_econ_mult_td <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      pa <- as.numeric(row["initial_accept"])
      bxy <- as.numeric(row["perf_corr"])
      return(utility.econtd(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc))
    })
    
    #compute effect
    utility_data$decrease_econmulttd <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_td"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over2.1 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_td"])/abs(row["adjusted_utility_econ_mult"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttd <- mean(utility_data$decrease_econmulttd)
    median.econmulttd <- median(utility_data$decrease_econmulttd)
    mean.over2.1 <- mean(utility_data$decrease_over2.1)
    median.over2.1 <- median(utility_data$decrease_over2.1)
    
    #economic variables + multiple devices + probation
    
    utility_data$adjusted_utility_econ_mult_prob <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      rc <- as.numeric(row["cutoff_score"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      return(as.numeric(utility.econ.probation(n,t,r,sr,sdy,rc,cost, i, vc, tax)))
    })
    
    #compute effect
    utility_data$decrease_econmultprob <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_prob"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over2.2 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_prob"])/abs(row["adjusted_utility_econ_mult"])
      return(as.numeric(100*change))
    })
    
    mean.econmultprob <- mean(utility_data$decrease_econmultprob)
    median.econmultprob <- median(utility_data$decrease_econmultprob)
    mean.over2.2 <- mean(utility_data$decrease_over2.2)
    median.over2.2 <- median(utility_data$decrease_over2.2)
    
    #economic variables + multiple devices + employee flows
    
    utility.econ.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax){
      num_years <- seq(1, t)
      n_added <- c(rep(n, 1), rep(0, t - 1))
      n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
      n_cum <- cumsum(n_added - n_turn)
      zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
      discount_factor <- 1/(1 + i)^num_years
      
      #Financially Adjusted Costs of Program in period (year)
      c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
      unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
      
      adjusted_utility <- unadjusted_utility - c_adj
      adjusted_utility <- sum(round(adjusted_utility, 0))
      return(adjusted_utility)
    }
    
    utility_data$adjusted_utility_econ_mult_flow <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      pto <- as.numeric(row["turnover_probability"])
      corrto <- as.numeric(row["performance_correlation_turnover"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      return(as.numeric(utility.econ.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax)))
    })
    
    #compute effect
    utility_data$decrease_econmultflow <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_flow"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over2.3 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult"]-row["adjusted_utility_econ_mult_flow"])/abs(row["adjusted_utility_econ_mult"])
      return(as.numeric(100*change))
    })
    
    mean.econmultflow <- mean(utility_data$decrease_econmultflow)
    median.econmultflow <- median(utility_data$decrease_econmultflow)
    mean.over2.3 <- mean(utility_data$decrease_over2.3)
    median.over2.3 <- median(utility_data$decrease_over2.3)
    
    #economic variables + multiple devices + temporal validity + top down
    
    utility.econtdtv <- function(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab){
      p2 <- ((n-(pa*n))/(n/sr))+sr
      if (p2 > 1){
        p2 <- 1
      }
      z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
      x2 <- (1/(1+i)*(1-(1/(1+i)^t)))/(1-(1/(1+i)))
      return ((n*x2*r*stab*z*sdy*(1-vc)*(1-tax))-((n/sr)*cost*(1-tax)))
    }
    
    utility_data$adjusted_utility_econ_mult_tv_td <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      pa <- as.numeric(row["initial_accept"])
      bxy <- as.numeric(row["perf_corr"])
      stab <- as.numeric(row["performance_stability"])
      return(utility.econtdtv(n, t, r, sdy, sr, cost, pa, bxy, i, tax, vc, stab))
    })
    
    # Create a data frame for the plot
    adjusted_utility_df_econmulttvtd <- data.frame(Adjusted_Utility_Total_econmulttvtd = utility_data$adjusted_utility_econ_mult_tv_td)
    
    # Calculate density
    density_df_econmulttvtd <- data.frame(x = density(adjusted_utility_df_econmulttvtd$Adjusted_Utility_Total_econmulttvtd)$x,
                                          y = density(adjusted_utility_df_econmulttvtd$Adjusted_Utility_Total_econmulttvtd)$y)
    
    density_df_econmulttvtd$group <- "Line 5"
    
    #compute effect
    utility_data$decrease_econmulttvtd <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over3 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_td"])/abs(row["adjusted_utility_econ_mult_tv"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttvtd <- mean(utility_data$decrease_econmulttvtd)
    median.econmulttvtd <- median(utility_data$decrease_econmulttvtd)
    mean.over3 <- mean(utility_data$decrease_over3)
    median.over3 <- median(utility_data$decrease_over3)
    
    #economic variables + multiple devices + temporal validity + prob
    
    utility.econ.tv.probation <- function(n, t, r, sr, sdy, rc, cost, i, vc, tax, stab) {
      xc <- qnorm(1-sr)
      PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
      PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
      PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
      mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
      sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
      so <- pnorm(rc, lower.tail = FALSE)
      mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
      x1 <- 1/(1+i)
      x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
      return((n*x1*stab*sdy*r*ux(sr)*(1-tax)*(1-vc)+x2*n*sp*stab*sdy*mur.xcrc*(1-vc)*(1-tax)-(n/sr)*cost*(1-tax))-(x2*n*stab*sdy*so*mur.rc*(1-vc)*(1-tax)))
    }
    
    utility_data$adjusted_utility_econ_mult_tv_prob <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      rc <- as.numeric(row["cutoff_score"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      stab <- as.numeric(row["performance_stability"])
      return(as.numeric(utility.econ.tv.probation(n, t, r, sr, sdy, rc, cost, i, vc, tax, stab)))
    })
    
    #compute effect
    utility_data$decrease_econmulttvprob <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_prob"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over3.1 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_prob"])/abs(row["adjusted_utility_econ_mult_tv"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttvprob <- mean(utility_data$decrease_econmulttvprob)
    median.econmulttvprob <- median(utility_data$decrease_econmulttvprob)
    mean.over3.1 <- mean(utility_data$decrease_over3.1)
    median.over3.1 <- median(utility_data$decrease_over3.1)
    
    #economic variables + multiple devices + temporal validity + flows
    
    utility.econ.tv.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab){
      num_years <- seq(1, t)
      n_added <- c(rep(n, 1), rep(0, t - 1))
      n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
      n_cum <- cumsum(n_added - n_turn)
      zbarx <- c(rep(ux(sr), 1), rep(ux(sr)+corrto*ux(1-pto), t-1))
      discount_factor <- 1/(1 + i)^num_years
      
      #Financially Adjusted Costs of Program in period (year)
      c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
      unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
      
      adjusted_utility <- stab*unadjusted_utility - c_adj
      adjusted_utility <- sum(round(adjusted_utility, 0))
      return(adjusted_utility)
    }
    
    utility_data$adjusted_utility_econ_mult_tv_flow <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      pto <- as.numeric(row["turnover_probability"])
      corrto <- as.numeric(row["performance_correlation_turnover"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      stab <- as.numeric(row["performance_stability"])
      return(as.numeric(utility.econ.tv.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab)))
    })
    
    #compute effect
    utility_data$decrease_econmulttvflow <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_flow"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over3.2 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult_tv"]-row["adjusted_utility_econ_mult_tv_flow"])/abs(row["adjusted_utility_econ_mult_tv"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttvflow <- mean(utility_data$decrease_econmulttvflow)
    median.econmulttvflow <- median(utility_data$decrease_econmulttvflow)
    mean.over3.2 <- mean(utility_data$decrease_over3.2)
    median.over3.2 <- median(utility_data$decrease_over3.2)
    
    #economic variables + multiple devices + temporal validity + top down
    
    utility.econ.mult.tv.td.prob <- function(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab){
      p2 <- ((n-(pa*n))/(n/sr))+sr
      if (p2 > 1){
        p2 <- 1
      }
      xc <- qnorm(1-sr)
      xc.pa <- qnorm(1-pa)
      xc.p2 <- qnorm(1-p2)
      PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
      PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
      PHI1.1.pa <- (xc.pa-r*rc)/sqrt(1-r^2)
      PHI1.2.pa <- (rc-r*xc.pa)/sqrt(1-r^2)
      PHI1.1.p2 <- (xc.p2-r*rc)/sqrt(1-r^2)
      PHI1.2.p2 <- (rc-r*xc.p2)/sqrt(1-r^2)
      PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
      mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
      mur.xcrc.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))/pmvnorm(c(xc.pa, rc), Inf, corr = PHI2.corr)
      mur.xcrc.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))/pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)
      sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
      sp.p2 <- pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)/pnorm(xc.p2, lower.tail = FALSE)
      so <- pnorm(rc, lower.tail = FALSE)
      mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
      z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
      z.mod <- sp*(pa*mur.xcrc+pa*bxy*ux(pa)*r)+sp.p2*((p2/sr)*mur.xcrc.p2-mur.xcrc)
      x1 <- 1/(1+i)
      x2 <- (1/(1+i)*(1-(1/(1+i)^(t-1))))/(1-(1/(1+i)))
      return((n*x1*stab*sdy*r*z*(1-tax)*(1-vc)+x2*n*sdy*stab*z.mod*(1-tax)*(1-vc)-(n/sr)*cost*(1-tax))-(x2*n*sdy*so*stab*mur.rc*(1-tax)*(1-vc)))
    }
    
    utility_data$adjusted_utility_econ_mult_tv_td_prob <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      pa <- as.numeric(row["initial_accept"])
      bxy <- as.numeric(row["perf_corr"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      rc <- as.numeric(row["cutoff_score"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      stab <- as.numeric(row["performance_stability"])
      return(as.numeric(utility.econ.mult.tv.td.prob(n, t, r, pa, bxy, rc, sdy, sr, cost, i, tax, vc, stab)))
    })
    
    # Create a data frame for the plot
    adjusted_utility_df_econmulttvtdprob <- data.frame(Adjusted_Utility_Total_econmulttvtdprob = utility_data$adjusted_utility_econ_mult_tv_td_prob)
    
    # Calculate density
    density_df_econmulttvtdprob <- data.frame(x = density(adjusted_utility_df_econmulttvtdprob$Adjusted_Utility_Total_econmulttvtdprob)$x,
                                              y = density(adjusted_utility_df_econmulttvtdprob$Adjusted_Utility_Total_econmulttvtdprob)$y)
    
    density_df_econmulttvtdprob$group <- "Line 6"
    
    #compute effect
    utility_data$decrease_econmulttvtdprob <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td_prob"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over4 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult_tv_td"]-row["adjusted_utility_econ_mult_tv_td_prob"])/abs(row["adjusted_utility_econ_mult_tv_td"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttvtdprob <- mean(utility_data$decrease_econmulttvtdprob)
    median.econmulttvtdprob <- median(utility_data$decrease_econmulttvtdprob)
    mean.over4 <- mean(utility_data$decrease_over4)
    median.over4 <- median(utility_data$decrease_over4)
    
    #economic variables + multiple devices + temporal validity + top down + flow
    
    utility.econ.tv.td.flow <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy){
      p2 <- ((n-(pa*n))/(n/sr))+sr
      if (p2 > 1){
        p2 <- 1
      }
      num_years <- seq(1, t)
      n_added <- c(rep(n, 1), rep(0, t - 1))
      n_turn <- c(rep(0, 1), rep((n*pto)/t, t-1))
      n_cum <- cumsum(n_added - n_turn)
      zbarx <- c(rep((pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n, 1), rep(((pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n)+corrto*ux(1-pto), t-1))
      discount_factor <- 1/(1 + i)^num_years
      
      #Financially Adjusted Costs of Program in period (year)
      c_adj <- (n_added / sr) * cost * (1 - tax) * (1/(1 + i)^(num_years - 1))
      unadjusted_utility <- n_cum * discount_factor * r * zbarx * sdy * (1 - vc) * (1 - tax)
      
      adjusted_utility <- stab*unadjusted_utility - c_adj
      adjusted_utility <- sum(round(adjusted_utility, 0))
      return(adjusted_utility)
    }
    
    utility_data$adjusted_utility_econ_mult_tv_td_flow <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      pto <- as.numeric(row["turnover_probability"])
      corrto <- as.numeric(row["performance_correlation_turnover"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      stab <- as.numeric(row["performance_stability"])
      pa <- as.numeric(row["initial_accept"])
      bxy <- as.numeric(row["perf_corr"])
      return(as.numeric(utility.econ.tv.td.flow(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy)))
    })
    
    #compute effect
    utility_data$decrease_econmulttvtdflow <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_econ_mult_tv_td_flow"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over4.1 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult_tv_td"]-row["adjusted_utility_econ_mult_tv_td_flow"])/abs(row["adjusted_utility_econ_mult_tv_td"])
      return(as.numeric(100*change))
    })
    
    mean.econmulttvtdflow <- mean(utility_data$decrease_econmulttvtdflow)
    median.econmulttvtdflow <- median(utility_data$decrease_econmulttvtdflow)
    mean.over4.1 <- mean(utility_data$decrease_over4.1)
    median.over4.1 <- median(utility_data$decrease_over4.1)
    
    # final adjustment
    
    utility.complete <- function(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc){
      p2 <- ((n-(pa*n))/(n/sr))+sr
      if (p2 > 1){
        p2 <- 1
      }
      xc <- qnorm(1-sr)
      xc.pa <- qnorm(1-pa)
      xc.p2 <- qnorm(1-p2)
      PHI1.1 <- (xc-r*rc)/sqrt(1-r^2)
      PHI1.2 <- (rc-r*xc)/sqrt(1-r^2)
      PHI1.1.pa <- (xc.pa-r*rc)/sqrt(1-r^2)
      PHI1.2.pa <- (rc-r*xc.pa)/sqrt(1-r^2)
      PHI1.1.p2 <- (xc.p2-r*rc)/sqrt(1-r^2)
      PHI1.2.p2 <- (rc-r*xc.p2)/sqrt(1-r^2)
      PHI2.corr <- matrix(c(1, r, r, 1), nrow = 2)
      mur.xcrc <- (dnorm(rc)*pnorm(PHI1.1, lower.tail = FALSE)+r*dnorm(xc)*pnorm(PHI1.2, lower.tail = FALSE))/pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)
      mur.xcrc.pa <- (dnorm(rc)*pnorm(PHI1.1.pa, lower.tail = FALSE)+r*dnorm(xc.pa)*pnorm(PHI1.2.pa, lower.tail = FALSE))/pmvnorm(c(xc.pa, rc), Inf, corr = PHI2.corr)
      mur.xcrc.p2 <- (dnorm(rc)*pnorm(PHI1.1.p2, lower.tail = FALSE)+r*dnorm(xc.p2)*pnorm(PHI1.2.p2, lower.tail = FALSE))/pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)
      sp <- pmvnorm(c(xc, rc), Inf, corr = PHI2.corr)/pnorm(xc, lower.tail = FALSE)
      sp.p2 <- pmvnorm(c(xc.p2, rc), Inf, corr = PHI2.corr)/pnorm(xc.p2, lower.tail = FALSE)
      so <- pnorm(rc, lower.tail = FALSE)
      mur.rc <- dnorm(rc)/pnorm(rc, lower.tail = FALSE)
      z <- (pa*n*ux(sr)+n*(bxy)*dnorm(qnorm(1-pa))+(n/sr)*(dnorm(qnorm(1 - p2))-(dnorm(qnorm(1 - sr)))))/n
      z.mod <- sp*(pa*mur.xcrc+pa*bxy*ux(pa)*r)+sp.p2*((p2/sr)*mur.xcrc.p2-mur.xcrc)
      x1 <- 1/(1+i)
      #Financially Adjusted Costs of Program in period (year)
      a <- n*x1*stab*sdy*r*z*(1-tax)*(1-vc)
      c <- (n/sr)*cost*(1-tax)
      if (t>1){
        num_years <- seq(1, t-1)
        n_added <- c(rep(n, 1), rep(0, t - 2))
        n_turn <- c(rep((n*pto)/t, t-1))
        n_cum <- cumsum(n_added - n_turn)
        zbarx <- c(rep((z.mod+corrto*ux(1-pto)*sp), t-1))
        discount_factor <- 1/(1 + i)^(num_years+1)
        b <- stab*n_cum * discount_factor * zbarx * sdy * (1 - vc) * (1 - tax)
        utility_0 <- stab * n_cum * discount_factor * so * (mur.rc+corrto*ux(1-pto)) * sdy * (1 - vc) * (1 - tax)
      }
      else{
        b <- 0
        utility_0 <- 0
      }
      adjusted_utility <- a+sum(b)-c-sum(utility_0)
      return(adjusted_utility)
    }
    
    utility_data$adjusted_utility_final <- apply(utility_data, 1, function(row){
      n <- as.numeric(row["n"])
      t <- as.numeric(row["t"])
      r <- as.numeric(row["incremental_r"])
      sdy <- as.numeric(row["sdy"])
      sr <- as.numeric(row["select"])
      cost <- as.numeric(row["cost"])
      pto <- as.numeric(row["turnover_probability"])
      corrto <- as.numeric(row["performance_correlation_turnover"])
      i <- as.numeric(row["i"])
      tax <- as.numeric(row["tax"])
      vc <- as.numeric(row["vc"])
      stab <- as.numeric(row["performance_stability"])
      pa <- as.numeric(row["initial_accept"])
      bxy <- as.numeric(row["perf_corr"])
      rc <- as.numeric(row["cutoff_score"])
      return(as.numeric(utility.complete(n, t, r, sdy, pto, corrto, sr, cost, i, vc, tax, stab, pa, bxy, rc)))
    })
    
    # Create a data frame for the plot
    adjusted_utility_df_final <- data.frame(Adjusted_Utility_Total_final = utility_data$adjusted_utility_final)
    
    # Calculate density
    density_df_final <- data.frame(x = density(adjusted_utility_df_final$Adjusted_Utility_Total_final)$x,
                                   y = density(adjusted_utility_df_final$Adjusted_Utility_Total_final)$y)
    
    density_df_final$group <- "Line 7"
    
    #compute effect
    utility_data$decrease_final <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_final"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_final <- apply(utility_data, 1, function(row){
      change <- (row["unadjusted_utility"]-row["adjusted_utility_final"])/abs(row["unadjusted_utility"])
      return(as.numeric(100*change))
    })
    
    utility_data$decrease_over5 <- apply(utility_data, 1, function(row){
      change <- (row["adjusted_utility_econ_mult_tv_td_prob"]-row["adjusted_utility_final"])/abs(row["adjusted_utility_econ_mult_tv_td_prob"])
      return(as.numeric(100*change))
    })
    
    # Effect Table
    table_2 <- data.frame(
      net_effect_1 = c(paste0(round(-median.econ, 0), "%"), paste0(round(-mean.econ, 0), "%"), paste0(round(-median.mult, 0), "%"), paste0(round(-mean.mult, 0), "%"), paste0(round(-median.temp, 0), "%"), paste0(-round(mean.temp, 0), "%"),  paste0(round(-median.topdown, 0), "%"),
                       paste0(round(-mean.topdown, 0), "%"), paste0(round(-median.probation, 0), "%"), paste0(round(-mean.probation, 0), "%"), paste0(round(-median.flow, 0), "%"),  paste0(round(-mean.flow, 0), "%")),
      net_effect_2a = c("", "", paste0(round(-median.econmult, 0), "%"), paste0(round(-mean.econmult, 0), "%"), paste0(round(-median.econtv, 0), "%"), paste0(round(-mean.econtv, 0), "%"), paste0(round(-median.econtd, 0), "%"), paste0(round(-mean.econtd, 0), "%"), paste0(round(-median.econprob, 0), "%"), paste0(round(-mean.econprob, 0), "%"), paste0(round(-median.econflow, 0), "%"), paste0(round(-mean.econflow, 0), "%")),
      over_prev_2b =c("", "", paste0(round(-median.over1, 0), "%"), paste0(round(-mean.over1, 0), "%"), paste0(round(-median.over1.2, 0), "%"), paste0(round(-mean.over1.2, 0), "%"), paste0(round(-median.over1.3, 0), "%"), paste0(round(-mean.over1.3, 0), "%"), paste0(round(-median.over1.4, 0), "%"), paste0(round(-mean.over1.4, 0), "%"), paste0(round(-median.over1.5, 0), "%"), paste0(round(-mean.over1.5, 0), "%")),
      net_effect_3a = c("", "", "", "", paste0(round(-median.econmulttv, 0), "%"), paste0(round(-mean.econmulttv, 0), "%"), paste0(round(-median.econmulttd, 0), "%"), paste0(round(-mean.econmulttd, 0), "%"), paste0(round(-median.econmultprob, 0), "%"), paste0(round(-mean.econmultprob, 0), "%"), paste0(round(-median.econmultflow, 0), "%"), paste0(round(-mean.econmultflow, 0), "%")),
      over_prev_3b =c("", "", "", "", paste0(round(-median.over2, 0), "%"), paste0(round(-mean.over2, 0), "%"), paste0(round(-median.over2.1, 0), "%"), paste0(round(-mean.over2.1, 0), "%"), paste0(round(-median.over2.2, 0), "%"), paste0(round(-mean.over2.2, 0), "%"), paste0(round(-median.over2.3, 0), "%"), paste0(round(-mean.over2.3, 0), "%")),
      net_effect_4a = c("", "", "", "", "", "", paste0(round(-median.econmulttvtd, 0), "%"), paste0(round(-mean.econmulttvtd, 0), "%"), paste0(round(-median.econmulttvprob, 0), "%"), paste0(round(-mean.econmulttvprob, 0), "%"), paste0(round(-median.econmulttvflow, 0), "%"), paste0(round(-mean.econmulttvflow, 0), "%")),
      over_prev_4b =c("", "", "", "", "", "", paste0(round(-median.over3, 0), "%"), paste0(round(-mean.over3, 0), "%"), paste0(round(-median.over3.1, 0), "%"), paste0(round(-mean.over3.1, 0), "%"), paste0(round(-median.over3.2, 0), "%"), paste0(round(-mean.over3.2, 0), "%")),
      net_effect_5a = c("", "", "", "", "", "", "", "", paste0(round(-median.econmulttvtdprob, 0), "%"), paste0(round(-mean.econmulttvtdprob, 0), "%"), paste0(round(-median.econmulttvtdflow, 0), "%"), paste0(round(-mean.econmulttvtdflow, 0), "%")),
      over_prev_5b =c("", "", "", "", "", "", "", "", paste0(round(-median.over4, 0), "%"), paste0(round(-mean.over4, 0), "%"), paste0(round(-median.over4.1, 0), "%"), paste0(round(-mean.over4.1, 0), "%")),
      net_effect_6a = c("", "", "", "", "", "", "", "", "", "", paste0(round(-median.final, 0), "%"), paste0(round(-mean.final, 0), "%")),
      over_prev_6b =c("", "", "", "", "", "", "", "", "", "", paste0(round(-median.over5, 0), "%"), paste0(round(-mean.over5, 0), "%"))
    )
    rownames(table_2) <- c("Economic Variables Median", "Economic Variables Mean", "Multiple Devices Median", "Multiple Devices Mean", "Temporal Validity Median", "Temporal Validity Mean", "Deviations from Top Down Hiring Median", "Deviations from Top Down Hiring Mean", "Probationary Period Median", "Probationary Period Mean", "Employee Flows Median", "Employee Flows Mean")
    colnames(table_2) <- c("Net Effect1", "Net Effect2a", "Over Previous Modification2b", "Net Effect3a", "Over Previous Modification3b", "Net Effect4a", "Over Previous Modification4b", "Net Effect5a", "Over Previous Modification5b", "Net Effect6a", "Over Previous Modification6b")
    output$sturman2 <- renderTable(table_2, rownames = TRUE, width = 1000)
    
    combined_df <- density_df_final
    combined_df$Fill <- ifelse(combined_df$x < 0, "Negative", "Positive")
    
    density_plot <- ggplot(combined_df, aes(x = x, y = y)) + 
      geom_area(aes(fill = Fill), alpha=0.4) +
      scale_fill_manual(values = c("Negative" = "red", "Positive" = "green")) +
      scale_x_continuous(labels = dollar_format2, breaks = extended_breaks(6)) +
      labs(title = "",
           x = "Return on Investment Total", 
           y = "") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0),
            plot.subtitle = element_text(hjust = 0, color = "black"),
            plot.background = element_rect(fill = "white", linetype = "blank"),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 17),
            axis.title = element_text(size =18)) +
      guides(fill="none")  # Remove legend
    
    output$density <- renderPlotly({
      ggplotly(density_plot)
    })
    
    # Calculate percentage of estimates that are negative
    pct_negative <- round(sum(utility_data$adjusted_utility_final < 0) / length(utility_data$adjusted_utility_final) * 100, 2)
    
    # Calculate average utility estimate
    avg_utility_estimate <- round(median(utility_data$adjusted_utility_final), 2)
    
    # get the positive value
    avg_positive_utility <- median(utility_data[utility_data$adjusted_utility_final > 0, "adjusted_utility_final"])
    medianform <- dollar_format2(avg_utility_estimate)
    
    output$medianmonte <- renderText(paste0("The median return on investment for these parameters is ", medianform, "."))
    output$negmonte <- renderText(paste0(pct_negative, "% of return on investment outcomes were negative."))
    
    removeModal()
  })
  
  #Training Utility
  
  #toggles for Training/RODI
  observeEvent(input$selectPage1, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 1")
  })
  
  observeEvent(input$selectPage2, {
    updateTabsetPanel(session, "pageTabs", selected = "Page 2")
  })
  
  observe({
    dt <- input$dTrain2_1
    # Get the label for the selected effect size
    effect_choices <- list(
      "Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)" = 0.35,
      "Leadership development: Average estimate (Avoilio et al., 2009)" = 0.65,
      "Leadership development: Intensive program (3+ days) (Avoilio et al., 2009)" = 1.00,
      "Leadership development: Upper bound (Avoilio et al., 2009)" = 1.37,
      "Goal setting (Schmidt, 2013)" = 0.46
    )
    # Find the label corresponding to the selected value
    selected_label <- names(effect_choices)[which(as.numeric(effect_choices) == as.numeric(input$effect_size_dropdown))][1]
    # Extract only the intervention type (e.g., 'Goal setting' or 'Leadership development')
    intervention <- trimws(sub("[:(].*", "", selected_label))
    # Calculate BESD values
    r <- dt / sqrt(dt^2 + 4)
    success_rate_with <- 0.5 + (r / 2)
    success_rate_without <- 0.5 - (r / 2)
    besd_data <- data.frame(
      Group = c("Untreated", "Treated"),
      SuccessRate = c(success_rate_without, success_rate_with)
    )
    besd_data$Group <- factor(besd_data$Group, levels = c("Untreated", "Treated"))
    diff_percent <- round((success_rate_with - success_rate_without) * 100, 1)
    # Extract modifier from the dropdown label (e.g., 'weak estimate', 'average estimate', etc.)
    # The label is the full dropdown label, e.g., 'Leadership development: Lower bound (e.g., minimal short program) (Avoilio et al., 2009)'
    # We'll extract the part before the first parenthesis as the intervention, and the part in parentheses as the modifier
    label_parts <- strsplit(selected_label, ": ")[[1]]
    intervention_type <- label_parts[1]
    # Extract modifier (e.g., 'Lower bound (e.g., minimal short program) (Avoilio et al., 2009)')
    modifier <- if (length(label_parts) > 1) label_parts[2] else ""
    # Clean up modifier: take the first part before citation, and trim, then lowercase
    modifier_clean <- tolower(trimws(sub("\\(.*", "", modifier)))
    # Compose subtitle
    subtitle_text <- paste0(intervention_type, if (nzchar(modifier_clean)) paste0(" (", modifier_clean, ")") else "", ": Success rate improves by ", diff_percent, "%.")
    # BESD bar plot
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
    output$training_graph_1 <- renderPlot(gg)
    output$effectText <- renderText({
      paste0("The above chart shows the Binomial Effect Size Display (BESD) for the training program. With an effect size of ", dt, ", the success rate for the treated group is ", round(success_rate_with*100,1), "% compared to ", round(success_rate_without*100,1), "% for the untreated group.")
    })
    output$effect_download <- downloadHandler(
        filename = function() {
        "binomial_effect_size_display.pdf"
        },
        content = function(file) {
        capWrap <- strwrap(paste0("The above chart shows the Binomial Effect Size Display (BESD) for the training program. With an effect size of ", dt, ", the success rate for the treated group is ", round(success_rate_with*100,1), "% compared to ", round(success_rate_without*100,1), "% for the untreated group."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          plot <- gg +
            labs(
              caption = capWrap
          )
        ggsave(
          file,
          plot,
          device = "pdf",
          width = 10.625,  
          height = 6.875,  
          units = "in",  
          dpi = 300  
        )
      }
    )
  })
  
  #Training Utility Output Tab
  
  observeEvent(input$goUn,{
    # compute unadjusted training utility
    utilityTrainUn <- input$nTrain*input$dTrain2*input$sdyTrain*input$tTrain-input$nTrain*input$costTrain2
    # unadjusted break even sdy
    breakEven <- (input$nTrain*input$costTrain2)/(input$nTrain*input$tTrain*input$dTrain2)
    
    # Format utility values with appropriate scaling
    format_utility <- function(x) {
      paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
    }
    
    formatted_breakEvenTrain <- format_utility(breakEven)
    formatted_utilityTrainUn <- format_utility(utilityTrainUn)
    
    output$h3 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Unadjusted Values</span>')
    })
    
    output$training_utilityUn <- renderText({
      paste("The opportunity costs of failing to use the program are:", formatted_utilityTrainUn)
    })
    
    output$break_even_train <- renderText({
      paste("The break even value of SDy is:", formatted_breakEvenTrain)
    })
    # Add context paragraph
    context_text <- paste0(
      "This analysis estimates the economic value generated by the training intervention. The unadjusted utility represents the total net benefit (in dollars) of the program, calculated as the increase in performance value minus the cost of the intervention. The opportunity cost is the value lost if the program is not implemented. These values help decision-makers understand the financial impact of training investments, and can be compared to the cost of alternative interventions or left unused."
    )
    output$trainingText1 <- renderText({ context_text })
    output$trainingText2 <- renderText({ "" })
    output$trainingText3 <- renderText({ "" })
  })
  
  # Add the new output to the UI (mainPanel) for utility_outputs tab
  # (Add textOutput("training_utilityOpCost") below training_utilityUn)

  observeEvent(input$go4,{
    enable("training_download")
    dt <- input$dTrain2
    is_schmidt <- !is.null(input$effect_size_dropdown_utility) && abs(as.numeric(input$effect_size_dropdown_utility) - 0.46) < 1e-6
    if (is_schmidt) {
      N <- input$nTrain
      T <- input$tTrain
      SDy <- input$sdyTrain
      d <- dt
      C <- input$costTrain2
      dollar_value <- N * T * SDy * d - N * C
      per_employee <- (T * SDy * d) - C
      per_year <- (SDy * d) - (C / T)
      percent_output_gain <- round(d * (input$sdP / 100) * 100, 1)
      formatted_total <- paste0("$", formatC(round(dollar_value), format = "f", big.mark = ",", digits = 0))
      formatted_per_employee <- paste0("$", formatC(round(per_employee), format = "f", big.mark = ",", digits = 0))
      formatted_per_year <- paste0("$", formatC(round(per_year), format = "f", big.mark = ",", digits = 0))
      formatted_C <- paste0("$", formatC(round(C), format = "f", big.mark = ",", digits = 0))
      formatted_SDy <- paste0("$", formatC(round(SDy), format = "f", big.mark = ",", digits = 0))
      annual_cost <- N * C
      formatted_annual_cost <- paste0("$", formatC(round(annual_cost), format = "f", big.mark = ",", digits = 0))
      output$h4 <- renderUI({HTML('<span style="font-size: 14px; font-weight: bold;">Goal Setting Utility (Schmidt, 2013)</span>')})
      output$training_utilityUn <- renderText({ NULL })
      output$training_utilityOpCost <- renderText({ NULL })
      output$break_even_train <- renderText({ NULL })
      output$training_utility <- renderText({
        paste("Estimated Dollar Value of Goal Setting Intervention (Schmidt, 2013):", formatted_total)
      })
      output$training_utility_per_employee <- renderText({
        paste("Estimated Value per Employee:", formatted_per_employee)
      })
      output$training_utility_per_employee_per_year <- renderText({
        paste("Estimated Value per Employee per Year:", formatted_per_year)
      })
      output$trainingText1 <- renderText({
        untreated_below <- round(pnorm(0, mean = d, sd = 1) * 100, 0)
        paste0(
          "We have conducted an analysis in order to assess the utility of a goal setting intervention. The program consisted of ", N, " employees participating in a goal setting program. Research showed that ", untreated_below, "% of employees who did not participate in the intervention were assessed to have lower job performance than those who did participate."
        )
      })
      output$trainingText2 <- renderText({
        paste0(
          "Our cost analysis found that costs associated with the goal setting intervention are ", formatted_C, " per employee. To make this cost estimate more accurate, we've applied three financial adjustments to our SDy figure of ", formatted_SDy, ". The first is a variable cost adjustment and refers to costs that increase with higher job performance, such as compensation enhancements. These are expected to offset utility associated with goal setting by ", input$vrateTrain, "%. Second, as higher performers should increase profits, returns should be taxed at the company's effective tax rate, which is ", input$taxTrain, "%. Lastly, the cash value of increased performance over time must be discounted to the present to approximate the net present value of the intervention. The discount rate applied to this intervention is ", input$discTrain, "%. Since ", N, " employees will participate each year, and the intervention is expected to be used for at least ", input$lengthTrain, " years, implementing the goal setting intervention is expected to cost our company ", formatted_annual_cost, " per year."
        )
      })
      output$trainingText3 <- renderText({
        paste0(
          "We found that utility is also affected by employee flows. When employees that participate in the intervention lose the effects from goal setting, the costs and benefits associated with the intervention change over time. Our research shows that employees that participated in the intervention are expected to be affected for an average of ", T, " years. This means that when employees do not participate in this intervention, the utility loss associated with each employee is ", formatted_per_employee, " or ", formatted_per_year, " per year. Considering this along with all previous factors, our analysis has found that failing to implement this intervention will result in a total utility loss of ", formatted_total, "."
        )
      })
      output$training_download <- downloadHandler(
        filename = function() {
          "goal_setting_utility_schmidt.pdf"
        },
        content = function(file) {
          capWrap <- strwrap(paste0(
            "Estimated Dollar Value of Goal Setting Intervention (Schmidt, 2013): ", formatted_total, "\n",
            "Formula: (T)(N)(SDy)(d) - (N)(C)\n",
            "Where: T = ", T, " years, N = ", N, ", SDy = ", formatted_SDy, ", d = ", d, ", C = ", formatted_C, ".\n",
            "Percent output gain: ", percent_output_gain, "%."), width = 100, simplify = TRUE)
          capWrap <- paste(capWrap, collapse = "\n")
          plot <- ggplot() + theme_void() +
            labs(title = "Goal Setting Utility (Schmidt, 2013)", caption = capWrap)
          ggsave(
            file,
            plot,
            device = "pdf",
            width = 10.625,  
            height = 6.875,
            units = "in",  
            dpi = 300  
          )
        }
      )
      return()
    }
    # Otherwise, use the existing logic for RODI/other interventions
    # ... existing code ...
  })
  
  # Add an observer to update dTrain2_1 when the dropdown changes
  observeEvent(input$effect_size_dropdown, {
    updateNumericInput(session, "dTrain2_1", value = as.numeric(input$effect_size_dropdown))
  })
  
  # Add an observer in the server to update dTrain2 when the dropdown changes
  observeEvent(input$effect_size_dropdown_utility, {
    updateNumericInput(session, "dTrain2", value = as.numeric(input$effect_size_dropdown_utility))
  })

  # Automatically set default values for goal setting when selected in Utility Outputs tab
  observeEvent(input$effect_size_dropdown_utility, {
    if (!is.null(input$effect_size_dropdown_utility) && abs(as.numeric(input$effect_size_dropdown_utility) - 0.46) < 1e-6) {
      updateNumericInput(session, "tTrain", value = 5)
      updateNumericInput(session, "nTrain", value = 35)
      updateNumericInput(session, "costTrain2", value = 200)
      updateNumericInput(session, "sdyTrain", value = 20000)
    }
  })

  # Render the Goal Setting diagram
  output$goal_setting_diagram <- renderGrViz({
    grViz("
      digraph path_diagram {
        graph [rankdir = LR]
        node [shape = box, style = filled, fillcolor = lightblue]

        GoalSetting [label = 'Goal Setting']
        Performance [label = 'Job Performance']
        Profits [label = 'Profits']

        GoalSetting -> Performance [label = '+']
        Performance -> Profits [label = '+']
      }
    ")
  })

  # Render the Utility diagram
  output$utility_diagram <- renderGrViz({
    grViz("
      digraph path_diagram {
        graph [layout = dot, rankdir = LR]

        node [
          shape = box,
          style = filled,
          fillcolor = lightblue,
          fontname = Helvetica,
          fontcolor = black,
          width = 2,
          fixedsize = true
        ]

        Quantity [label = 'Quantity']
        Quality [label = 'Quality']
        Cost [label = 'Cost']
        Utility [label = 'Utility']

        Quantity -> Utility
        Quality  -> Utility
        Cost     -> Utility
      }
    ")
  })

  # In the server logic, after computing and formatting the unadjusted utility, add:
  output$unadjusted_utility_value <- renderUI({
    HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityUn(), "</span>"))
  })
  output$unadjusted_utility_explanation <- renderUI({
    HTML(
      "The unadjusted utility estimate represents the total net financial benefit of using the new staffing procedure, before accounting for taxes, variable costs, or other economic adjustments. It is calculated as the increase in value from improved selection minus the additional costs of the new procedure. This value helps you understand the raw financial impact of switching to the new procedure, and is useful for comparing the basic ROI of different staffing strategies before considering real-world financial factors."
    )
  })

  # Add observers to clear outputs when switching pages
  observeEvent(input$utilityPages, {
    if (input$utilityPages == "Page 1") {
      # Clear outputs from Page 2 and Page 3 when navigating to Page 1
      output$adjusted_utility_label <- renderUI({ NULL })
      output$adjusted_utility_value <- renderUI({ NULL })
      output$adjusted_utility_explanation <- renderUI({ NULL })
      output$adjusted_utility_math <- renderUI({ NULL }) # Clear adjusted math block
      output$final_adjusted_utility_output <- renderUI({ NULL }) # Clear the new output

      output$flows_utility_label <- renderUI({ NULL })
      output$flows_utility_value <- renderUI({ NULL })
      output$flows_utility_explanation <- renderUI({ NULL })
      output$flows_utility_math <- renderUI({ NULL }) # Clear flows math block

    } else if (input$utilityPages == "Page 2") {
      # Clear outputs from Page 1 and Page 3 when navigating to Page 2
      output$unadjusted_utility_label <- renderUI({ NULL })
      output$unadjusted_utility_value <- renderUI({ NULL })
      output$unadjusted_utility_explanation <- renderUI({ NULL })
      output$unadjusted_utility_math <- renderUI({ NULL }) # Clear unadjusted math block

      output$flows_utility_label <- renderUI({ NULL })
      output$flows_utility_value <- renderUI({ NULL })
      output$flows_utility_explanation <- renderUI({ NULL })
      output$flows_utility_math <- renderUI({ NULL }) # Clear flows math block

    } else if (input$utilityPages == "Page 3") {
      # Clear outputs from Page 1 and Page 2 when navigating to Page 3
      output$unadjusted_utility_label <- renderUI({ NULL })
      output$unadjusted_utility_value <- renderUI({ NULL })
      output$unadjusted_utility_explanation <- renderUI({ NULL })
      output$unadjusted_utility_math <- renderUI({ NULL }) # Clear unadjusted math block

      output$adjusted_utility_label <- renderUI({ NULL })
      output$adjusted_utility_value <- renderUI({ NULL })
      output$adjusted_utility_explanation <- renderUI({ NULL })
      output$adjusted_utility_math <- renderUI({ NULL }) # Clear adjusted math block
      output$final_adjusted_utility_output <- renderUI({ NULL }) # Clear the new output
    }
  })

  # Page 1: Compute Utility
  observeEvent(input$go, {
    enable("download_pdf")
    output$unadjusted_utility_label <- renderUI({
      HTML("<b>Unadjusted Utility Estimate</b>")
    })
    output$unadjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", utilityUn(), "</span>"))
    })
    output$unadjusted_utility_explanation <- renderUI({
      HTML(
        "The unadjusted utility estimate represents the total net financial benefit of using the new staffing procedure, before accounting for taxes, variable costs, or other economic adjustments. It is calculated as the increase in value from improved selection minus the additional costs of the new procedure. This value helps you understand the raw financial impact of switching to the new procedure, and is useful for comparing the basic ROI of different staffing strategies before considering real-world financial factors."
      )
    })
    # Underlying Math with Unicode/plaintext formatting
    n <- input$n
    sdy <- input$sdy
    rxy1 <- input$rxy1
    rxy2 <- input$rxy2
    sr <- input$sr
    cost1 <- input$cost1
    cost2 <- input$cost2
    period <- input$period
    delta_rxy <- rxy2 - rxy1
    uxs <- ux(sr)
    cost_diff <- (cost2 - cost1) * (n / sr)
    final_utility <- n * sdy * delta_rxy * uxs * period - cost_diff
    utility_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Utility = n × sdy × (rxy₂ - rxy₁) × ux(sr) × period - (cost₂ - cost₁) × (n / sr)",
      "\n\nWhere:",
      "n = ", n, ", sdy = ", sdy, ", rxy₂ = ", rxy2, ", rxy₁ = ", rxy1, ", ux(sr) = ", round(uxs, 4), ", period = ", period, ", cost₂ = ", cost2, ", cost₁ = ", cost1, ", sr = ", sr,
      "\n\nStep-by-step:",
      "\n1. Difference in validity: rxy₂ - rxy₁ = ", delta_rxy,
      "\n2. Cost difference: (cost₂ - cost₁) × (n / sr) = ", round(cost_diff, 2),
      "\n3. Utility = ", n, " × ", sdy, " × ", delta_rxy, " × ", round(uxs, 4), " × ", period, " - ", round(cost_diff, 2),
      "\n4. Final Utility = ", format_utility(final_utility),
      "</pre>"
    )
    output$unadjusted_utility_math <- renderUI({
      HTML(utility_formula)
    })
    # Clear other outputs
    output$adjusted_utility_label <- renderUI({ NULL })
    output$adjusted_utility_value <- renderUI({ NULL })
    output$adjusted_utility_explanation <- renderUI({ NULL })
    output$adjusted_utility_math <- renderUI({ NULL })
    output$flows_utility_label <- renderUI({ NULL })
    output$flows_utility_value <- renderUI({ NULL })
    output$flows_utility_explanation <- renderUI({ NULL })
    output$flows_utility_math <- renderUI({ NULL })
  })

  # Page 2: Compute Adjustments
  observeEvent(input$go2, {
    enable("download_pdf")
    output$adjusted_utility_label <- renderUI({
      HTML("<b>Adjusted Utility Estimate</b>")
    })
    # Now using the format_utility function on the calculated value
    output$adjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", format_utility(adjusted_utility_value), "</span>"))
    })
    output$adjusted_utility_explanation <- renderUI({
      HTML(
        "The adjusted utility estimate accounts for real-world financial factors such as variable costs, taxes, and discount rates. This value provides a more realistic estimate of the net benefit of the staffing procedure, reflecting the true economic impact after these adjustments. Use this value for more accurate budgeting and investment decisions."
      )
    })

    # Calculate totDelta for adjusted utility (Moved here to be in scope)
    # Convert percentages to proportions
    varCosts <- -input$vcost/100
    taxProp <- input$tax/100
    discProp <- input$drate/100
    discRat <- 1/(1 + discProp)

    # Calculate other parameters (using inputs from Page 1 and Page 2)
    valid <- input$rxy2 - input$rxy1
    ord <- ux(input$sr)
    SDjp <- input$sdy
    sr1 <- input$sr
    tenure1 <- input$period
    last <- input$pyears
    add <- input$nadd
    subt <- input$nsub
    costOrd <- input$cost1
    costAc <- input$cost2
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    ck <- add * ((costAc - costOrd) / sr1)
    numyr <- tenure1 + last

    # Initialize variables
    nk <- 0
    totDelta <- 0
    totDelta1 <- 0

    # Calculate adjusted utility within the loop
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((discRat^i) * valid * ord * SDjp * (-varCosts) * (taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }

    # Compute the adjusted utility value - using the calculated totDelta
    adjusted_utility_value <- totDelta

    # Underlying Math for Adjusted Utility with Unicode/plaintext formatting
    adjusted_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Adjusted Utility = Utility × (1 - vcost/100) × (1 - tax/100) × (1 / (1 + drate/100))  (This is a simplified representation; the actual calculation involves a loop accounting for period, program length, and employee flows)", # Updated formula text for clarity
      "\\n\\nWhere:",
      "vcost = ", vcost, "%, tax = ", tax, "%, drate = ", drate, "%",
      # We won't list all individual parameters here, as the formula is simplified representation of the loop
      "\\n\\nStep-by-step (based on the detailed calculation loop):",
      "\\n1. Adjust initial utility for variable costs and taxes over time.",
      "\\n2. Account for discount rate each year.",
      "\\n3. Incorporate costs and benefits associated with employee flows (additions and subtractions) over the program length.",
      "\\n4. Sum up the discounted net benefits over the program duration.",
      "\\n5. Final Adjusted Utility = ", format_utility(adjusted_utility_value), # Using the calculated and formatted value
      "</pre>"
    )
    output$adjusted_utility_math <- renderUI({
      if (!is.null(adjusted_utility_value)) {
        HTML(adjusted_formula)
      } else {
        NULL
      }
    })

    # Render the final adjusted utility value below the main estimate
    output$final_adjusted_utility_output <- renderUI({
      if (!is.null(adjusted_utility_value)) {
        HTML(paste0("<b>Final Adjusted Utility = </b>", format_utility(adjusted_utility_value)))
      } else {
        NULL
      }
    })


    # Clear other outputs from Page 1 and Page 3
    output$unadjusted_utility_label <- renderUI({ NULL })
    output$unadjusted_utility_value <- renderUI({ NULL })
    output$unadjusted_utility_explanation <- renderUI({ NULL })
    output$unadjusted_utility_math <- renderUI({ NULL })
    output$flows_utility_label <- renderUI({ NULL })
    output$flows_utility_value <- renderUI({ NULL })
    output$flows_utility_explanation <- renderUI({ NULL })
    output$flows_utility_math <- renderUI({ NULL })
  })

  # Page 3: Account for Flows
  observeEvent(input$go_flows, {
    enable("download_pdf")
    final_utility <- utilityAdjPerYear() # Replace with your actual calculation for flows if needed
    output$flows_utility_label <- renderUI({
      HTML("<b>Final Adjusted Utility Estimate (with Employee Flows)</b>")
    })
    output$flows_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", final_utility, "</span>"))
    })
    output$flows_utility_explanation <- renderUI({
      HTML(
        "This final adjusted utility estimate incorporates the effects of employee flows, such as hiring and turnover, in addition to all previous financial adjustments. It provides the most comprehensive estimate of the net benefit of the staffing procedure, reflecting both economic factors and workforce dynamics. Use this value for strategic planning and to understand the long-term impact of staffing decisions on your organization."
      )
    })
    # Underlying Math for Employee Flows with Unicode/plaintext formatting
    pyears <- input$pyears
    nadd <- input$nadd
    nsub <- input$nsub
    flows_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>Formula:",
      "Final Utility = Adjusted Utility × (Employee Flows Adjustment)",
      "\n\nWhere:",
      "Program Length = ", pyears, ", Employees Added per Year = ", nadd, ", Employees Lost per Year (After Tenure) = ", nsub,
      "\n\nStep-by-step:",
      "\n1. Net change in employees per year: Employees Added per Year - Employees Lost per Year",
      "\n2. Adjust utility for employee inflows and outflows over the program length.",
      "</pre>"
    )
    output$flows_utility_math <- renderUI({
      HTML(flows_formula)
    })
    # Clear other outputs
    output$unadjusted_utility_label <- renderUI({ NULL })
    output$unadjusted_utility_value <- renderUI({ NULL })
    output$unadjusted_utility_explanation <- renderUI({ NULL })
    output$unadjusted_utility_math <- renderUI({ NULL })
    output$adjusted_utility_label <- renderUI({ NULL })
    output$adjusted_utility_value <- renderUI({ NULL })
    output$adjusted_utility_explanation <- renderUI({ NULL })
    output$adjusted_utility_math <- renderUI({ NULL })
  })
}

# Navbar
ui <- navbarPage(
  "UA+",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Comp & Ben Utility", fluidPage(h4(HTML("<b>Compensation & Benefits Utility</b>")), p("This section will provide tools and reports for analyzing the utility of compensation and benefits interventions. (Coming soon.)"))),
  tabPanel("Training Utility", training_ui),
  tabPanel("Glossary", glossary_ui),
  tabPanel("References", reference_ui)
)

shinyApp(ui, main_server)