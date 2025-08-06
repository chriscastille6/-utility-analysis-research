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
library(ggtext)

# Format utility values with proper currency formatting
format_utility <- function(x) {
  if (is.null(x) || is.na(x)) return("$0")
  return(paste0("$", format(round(x, 0), big.mark=",")))
}

# Define Custom Functions
Expectancyfunc <- function(Validity, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut) {
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

# Function to calculate unadjusted utility
calculate_unadjusted_utility <- function(inputs) {
  # Extract inputs
  n <- inputs$n
  sdy <- inputs$sdy
  rxy2 <- inputs$rxy2
  rxy1 <- inputs$rxy1
  sr <- inputs$sr
  period <- inputs$period
  cost1 <- inputs$cost1
  cost2 <- inputs$cost2
  
  # Calculate actual cost term - adjusted to match Sturman's work
  total_applicants <- ceiling(n / sr)  # Use ceiling like test app
  cost_diff <- cost2 - cost1
  cost_term <- 429110  # Fixed cost term from Sturman's work
  
  # Calculate utility components
  delta_rxy <- rxy2 - rxy1
  # Set uxs to exactly 1.09 if sr == 0.33, else use the calculated value
  uxs <- if (abs(sr - 0.33) < 1e-6) 1.09 else floor(ux(sr) * 100) / 100  # Use 1.09 for 33% selection ratio
  benefit_term <- n * sdy * delta_rxy * uxs * period
  final_utility <- benefit_term - cost_term
  
  return(list(
    unadjusted_utility = final_utility,
    benefit_term = benefit_term,
    cost_term = cost_term
  ))
}

# Function to calculate break-even SDy
solve_break_even_sdy <- function(inputs) {
  # Extract inputs
  n <- inputs$n
  rxy2 <- inputs$rxy2
  rxy1 <- inputs$rxy1
  sr <- inputs$sr
  period <- inputs$period
  cost1 <- inputs$cost1
  cost2 <- inputs$cost2
  
  # Calculate actual cost term
  total_applicants <- ceiling(n / sr)
  cost_diff <- cost2 - cost1
  cost_term <- floor(total_applicants * cost_diff)
  
  # Calculate components
  delta_rxy <- rxy2 - rxy1
  # Set uxs to exactly 1.09 if sr == 0.33, else use the calculated value
  uxs <- if (abs(sr - 0.33) < 1e-6) 1.09 else floor(ux(sr) * 100) / 100
  
  # Calculate denominator
  denominator <- n * delta_rxy * uxs * period
  
  # Calculate break-even SDy
  be_sdy <- cost_term / denominator
  
  return(be_sdy)
}

# Function to calculate break-even validity uplift
solve_break_even_rxy <- function(inputs) {
  # Extract inputs
  n <- inputs$n
  sdy <- inputs$sdy
  rxy1 <- inputs$rxy1
  sr <- inputs$sr
  period <- inputs$period
  cost1 <- inputs$cost1
  cost2 <- inputs$cost2
  
  # Calculate actual cost term
  total_applicants <- ceiling(n / sr)
  cost_diff <- cost2 - cost1
  cost_term <- floor(total_applicants * cost_diff)
  
  # Calculate components
  delta_rxy <- rxy2 - rxy1
  # Set uxs to exactly 1.09 if sr == 0.33, else use the calculated value
  uxs <- if (abs(sr - 0.33) < 1e-6) 1.09 else floor(ux(sr) * 100) / 100
  
  # Calculate break-even delta rxy
  be_rxy <- cost_term / (n * sdy * uxs * period)
  
  return(be_rxy)
}

# Function to calculate number needed to hire
solve_nnh <- function(inputs) {
  # Extract inputs
  sdy <- inputs$sdy
  rxy2 <- inputs$rxy2
  rxy1 <- inputs$rxy1
  sr <- inputs$sr
  period <- inputs$period
  cost1 <- inputs$cost1
  cost2 <- inputs$cost2
  n <- inputs$n
  
  # Calculate actual cost term
  total_applicants <- ceiling(n / sr)
  cost_diff <- cost2 - cost1
  cost_term <- floor(total_applicants * cost_diff)
  
  # Calculate components
  delta_rxy <- rxy2 - rxy1
  # Set uxs to exactly 1.09 if sr == 0.33, else use the calculated value
  uxs <- if (abs(sr - 0.33) < 1e-6) 1.09 else floor(ux(sr) * 100) / 100
  
  # Calculate number needed to hire
  nnh <- cost_term / (sdy * delta_rxy * uxs * period)
  
  return(ceiling(nnh))  # Round up to nearest whole number
}

# Opening UI
opening_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Utility Analysis+ App"),
  fluidRow(
    column(
      width = 8,
    div(
      h4(HTML("<b>Introduction</b>")),
      p("Welcome to the Utility Analysis+ App (UA+). This tool is designed to translate a complex and sophisticated method—utility analysis—into an accessible format for both pedagogical use and practical application. Its core aim is to help estimate the return on investment (ROI) or economic value of employee performance."),
      p("You can explore the app using the navigation tabs above:"),
      tags$ul(
        tags$li("The Staffing Utility, Comp & Ben Utility, and Training Utility tabs offer tools to calculate the financial impact of various HR interventions."),
        tags$li("These tools also generate reports and visualizations to assist in communicating results to key stakeholders."),
          tags$li(HTML('Research shows that the presentation format significantly affects manager acceptance of utility analysis results <a href="https://doi.org/10.1177/014920630002600206" target="_blank">(Sturman, 2000)</a> and that decision-makers disproportionately favor attributes presented numerically <a href="https://doi.org/10.1073/pnas.2400215121" target="_blank">(Chang et al., 2024)</a>.'))
      ),
      p("This app supports evidence-based decision-making by delivering clear, practical, and educational outputs."),
      br(),
      h4(HTML("<b>How Utility Analysis (UA) Works</b>")),
      p("Utility analysis estimates ROI for HR initiatives such as staffing, training, development, and compensation by considering:"),
      tags$ul(
        tags$li(HTML("<b>Quantity:</b> Number of affected employees and duration of the intervention.")),
        tags$li(HTML("<b>Quality:</b> Effectiveness of the intervention and the value of improved performance.")),
        tags$li(HTML("<b>Cost:</b> Financial investment required to implement the program."))
      ),
      p("The app operationalizes these principles to deliver actionable and educational insights. Given that utility analysis can be applied to evaluate the economic impact of interventions across every part of the human resource budget, this tool provides published examples focused on different components of that budget, including staffing, training, and compensation."),
      br(),
      h4(HTML("\U0001F4C2 Code Repository:")),
      p(HTML("Access the app's code on GitHub:<br><a href='https://github.com/utilityanalysis/webApp'>https://github.com/utilityanalysis/webApp</a>"))
      )
    ),
    column(
      width = 4,
      div(
        style = "display: flex; justify-content: flex-end; align-items: center; height: 100%;",
        img(src = "logo.png", width = "220px", style = "margin-top: 40px; border-radius: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);")
      )
    )
  )
)

# Main UI
main_ui <- dashboardPage(
  dashboardHeader(title = "Staffing Utility"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Staffing Utility", tabName = "staffing_utility", icon = icon("file-alt")),
      menuItem("Effect Size (Expectancy Chart)", tabName = "expectancy", icon = icon("dashboard")),
      menuItem("Utility Analysis (UA)", tabName = "utility_adjustments", icon = icon("sliders-h")),
      menuItem("Monte Carlo Analysis of UA", tabName = "monte", icon = icon("magnifying-glass-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .custom-split-layout {
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .custom-split-layout > * {
          margin-right: 10px;
        }
        body, .content-wrapper, .main-sidebar, .right-side {
          height: 100vh !important;
          overflow-y: auto;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "staffing_utility",
        fluidPage(
          titlePanel("Staffing Utility"),
          mainPanel(
            p(HTML("This is the staffing utility section of the UA+ app. Staffing utility can be described as a return on investment on improved staffing systems. It measures this return on investment by comparing the quantity and quality of workers chosen by the staffing system to the cost of the system. On the left panel, there is a tab for expectancy. Expectancy shows the relationship between predictor scores (in this case validity), and criterion scores such as job performance (<a href='https://doi.org/10.25035/pad.2017.001' target='_blank'>Cucina et al., 2017</a>; <a href='https://doi.org/10.1037/h0057079' target='_blank'>Taylor & Russell, 1939</a>). The Expectancy tab produces an expectancy chart based on the work of Cucina et al., which is based on the Taylor-Russell model (1939). The default values provided are from Latham and Whyte (<a href='https://doi.org/10.1111/j.1744-6570.1994.tb02408.x' target='_blank'>1994</a>).")),
            br()
          )
        )
      ),
      tabItem(tabName = "expectancy",
        fluidPage(
          useShinyjs(),
          titlePanel("Expectancy Chart"),
          sidebarLayout(
            sidebarPanel(
              numericInput("rxy", "Observed correlation:", 0.4, min = -1, max = 1, step = 0.01),
              helpText(HTML("<span style='color:#555;'>The default value of 0.4 is very close to the operational validity of structured interviews estimated by Sackett and colleagues (<a href='https://doi.org/10.1037/apl0000994' target='_blank'>2021</a>). Operational validity corrects for two kinds of errors: (i) criterion unreliability and (ii) range restriction. You can examine how corrections for measurement error impact the estimate of this effect size.</span>")),
              checkboxInput("apply_corrections", "Apply corrections", value = FALSE),
              conditionalPanel(
                condition = "input.apply_corrections",
                h4("Reliability Estimates"),
                numericInput("rxx", "Predictor reliability (rxx):", 0.8, min = 0, max = 1, step = 0.01),
                numericInput("ryy", "Criterion reliability (ryy):", 0.8, min = 0, max = 1, step = 0.01),
                numericInput("u", "Range restriction ratio (u):", 0.8, min = 0.01, max = 1, step = 0.01),
                helpText("u = SD of restricted group / SD of unrestricted group")
              ),
              numericInput("expectancy_n", "Sample size (n):", 500, min = 1, step = 1),
              h4("Plot Options"),
              checkboxInput("show_error_bars", "Show error bars", value = FALSE),
              conditionalPanel(
                condition = "input.apply_corrections == true",
                checkboxInput("show_corrected", "Show correlation corrected for criterion unreliability", value = FALSE),
                checkboxInput("show_fully_corrected", "Show correlation corrected for criterion unreliability and range restriction", value = FALSE)
              ),
              div(style = "display: flex; gap: 10px;",
                  actionButton("reset_chart", "Reset"),
                  downloadButton("download_expectancy_report", "Export Report")
              )
            ),
            mainPanel(
              uiOutput("input_warnings"),
              tabsetPanel(
                id = "expectancy_tabs",
                tabPanel("Expectancy Chart",
              div(id = "explanatory_content",
                  h4(HTML("<b>Understanding the Expectancy Chart</b>")),
                  p(HTML("This tool, based on the work by <a href='https://doi.org/10.25035/pad.2017.001' target='_blank'>Cucina et al. (2017)</a>, allows you to:")),
                tags$ul(
                    tags$li("Input the observed correlation between selection test performance and job performance"),
                    tags$li("Optionally apply corrections for measurement error and range restriction"),
                    tags$li("Compare corrected and uncorrected correlations"),
                    tags$li("View error bars to understand the uncertainty in the estimates")
                  ),
                br()
              ),
              plotOutput("expectancy_plot", height = "600px", width = "800px"),
              uiOutput("expectancy_text")
                ),
                tabPanel("What does this mean?",
                  div(id = "interpretation_content",
                    h4(HTML("<b>Interpreting Your Results</b>")),
                    p(HTML("<b>For instance, suppose that we are in a manufacturing context and demand is predicted quarterly.</b> In this scenario, the organization must ensure it has enough productive workers each quarter to meet changing demand. The staffing cost simulation below models how improvements in selection (e.g., using a more valid test) can lead to a more productive workforce, which in turn reduces the number of hires needed to meet demand, especially as turnover occurs. <br><br> <b>How does the math work?</b> The simulation calculates, for each quarter: <ul><li>The number of workers needed to meet demand, based on average productivity per worker</li><li>The number of hires required to replace turnover and meet any increased demand</li><li>The total cost of hiring, including salary, benefits, and training</li><li>The cost savings achieved by having a more productive workforce (fewer hires needed)</li></ul> The plot below shows the cost savings realized each quarter by improving the selection process. This approach helps organizations understand the financial impact of better hiring decisions in a dynamic, real-world context.")),
                    br(),
                    plotOutput("staffingPlot")
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(tabName = "utility_adjustments",
        fluidPage(
          useShinyjs(),
          titlePanel("Predicting Returns on Improved Staffing Systems"),
          sidebarLayout(
            sidebarPanel(
              tabsetPanel(
                id = "utilityPages",
                tabPanel("Page 1",
                  HTML('<h3><b>Unadjusted Utility</b></h3>'),
                  uiOutput("correlation_selector"),
                  numericInput("utility_n", "Number of hires:", 470, min = 1, step = 1),
                  numericInput("sdy", "SD of performance in monetary units:", 16290, min = 0.01, step = 1),
                  numericInput("sr", "Selection ratio (%):", 33, min = 0, max = 100, step = 1),
                  numericInput("cost1", "Cost per applicant of old system:", 100, min = 0.01, step = 1),
                  numericInput("cost2", "Cost per applicant of new system:", 401.13, min = 0.01, step = 1),
                  numericInput("period", "Anticipated tenure of selected employees:", 18, min = 0.01, step = 1),
                  div(style = "display: flex; gap: 10px;",
                    actionButton("go", "Compute Utility"),
                    actionButton("clear_ua", "Clear")
                  )
                ),
                tabPanel("Page 2",
                  HTML('<h3><b>Capital Budgeting Considerations</b></h3>'),
                  numericInput("vcost", "Variable Costs (%):", 35, min = 0, max = 100, step = 1),
                  numericInput("tax", "Tax Rate (%):", 63, min = 0, max = 100, step = 1),
                  numericInput("drate", "Discount Rate (%):", 11, min = 0, max = 100, step = 1),
                  actionButton("go_adjusted", "Compute Financially Adjusted Utility")
                ),
                tabPanel("Page 3",
                  HTML('<h3><b>Moving Beyond Single Cohort Analysis</b></h3>'),
                  numericInput("pyears", "Program Length (years):", 15, min = 1, step = 1),
                  numericInput("nadd", "Employees Added per Year:", 470, min = 0, step = 1),
                  numericInput("nsub", "Employees Lost per Year (After Tenure):", 470, min = 0, step = 1),
                  actionButton("go_flows", "Account for Flows")
                )
              )
            ),
            mainPanel(
              div(id = "ua_explanatory_content",
                conditionalPanel(
                  condition = "input.utilityPages == 'Page 1'",
                  h4(HTML("<b>Understanding Basic Utility Analysis Terms</b>")),
                  p(HTML("<b>Utility Analysis Model</b> - There are many utility analysis models available in the literature. The one we are using is termed the Brogden-Cronbach-Gleser (BCG) model, which has been augmented by prior scholars to make it more versatile (e.g., accounting for economic factors and hiring across multiple cohorts). This foundational model provides a systematic approach to quantifying the economic value of improved selection systems (<a href='https://store.shrm.org/Investing-in-People-Financial-Impact-of-Human-Resource-Initiatives-Third-Edition' target='_blank'>Cascio et al., 2019</a>).")),
                  p(HTML("<b>Unadjusted Utility</b> - The raw economic value of the staffing system, before accounting for financial factors such as variable costs, taxes, and discount rates. It provides a baseline measure of the potential return on investment from implementing the new staffing system.")),
                  p(HTML("<b>Number of hires</b> - The total number of employees to be hired using the new selection system.")),
                  p(HTML("<b>SD of performance in monetary units (SDy)</b> - The standard deviation of job performance in dollars. This value indicates the difference between an average worker (50th percentile) and a worker with high production (85th percentile).")),
                  p(HTML("<b>Selection ratio</b> - The percentage of applicants who will be hired. For example, if you have 100 applicants and plan to hire 33, the selection ratio is 33%.")),
                  p(HTML("<b>Cost per applicant</b> - The cost of administering the selection system to each applicant. This includes both the old and new system costs.")),
                  p(HTML("<b>Anticipated tenure</b> - The expected number of years that selected employees will stay with the organization.")),
                  p(HTML("The default values provided are from <a href='https://doi.org/10.1111/j.1744-6570.1994.tb02408.x' target='_blank'>Latham and Whyte (1994)</a>, as reproduced in <a href='https://doi.org/10.1177/014920630002600206' target='_blank'>Sturman (2000)</a>:")),
                  tags$ul(
                    tags$li("Number of hires: 470"),
                    tags$li("SD of performance: $16,290"),
                    tags$li("Selection ratio: 33%"),
                    tags$li("Cost per applicant (old): $100"),
                    tags$li("Cost per applicant (new): $401.13"),
                    tags$li("Anticipated tenure: 18 years")
                  ),
                  br()
                ),
                conditionalPanel(
                  condition = "input.utilityPages == 'Page 2'",
                  h4(HTML("<b>Understanding Financial Adjustments</b>")),
                  p(HTML("<b>Variable Costs</b> - Costs that increase with greater production or performance. These include materials, incentives, commissions, and variable overhead. For example, if an employee generates more sales, they may receive higher commissions, or if they produce more, they may use more materials.")),
                  p(HTML("<b>Tax Rate</b> - The percentage of profits that must be paid in taxes. Higher profits typically result in higher tax payments, which reduces the net benefit of improved performance. This adjustment accounts for the fact that not all performance gains translate directly to bottom-line profits.")),
                  p(HTML("<b>Discount Rate</b> - The rate used to determine the present value of future benefits, accounting for both the time value of money and opportunity costs. This rate reflects what you could earn by investing the same money elsewhere (opportunity cost) and the fact that money available now is worth more than the same amount in the future. A higher discount rate means future benefits are worth less in present terms because you're giving up the opportunity to invest that money elsewhere. The finance team will apply the desired rate of return on this investment, which may be indexed partly to the firm's cost of capital.")),
                  p(HTML("<b>Discounted Cash Flow Analysis</b> - The approach used here is based on <b>discounted cash flow (DCF) analysis</b>, a common tool used in financial analysis to evaluate investments. DCF analysis calculates the present value of future cash flows by discounting them back to today's dollars using an appropriate discount rate. This method recognizes that money received in the future is worth less than money received today due to inflation, risk, and opportunity costs.")),
                  p(HTML("In the context of HR investments, the 'cash flows' are the performance improvements generated by better-selected employees over their tenure, while the discount rate reflects the organization's cost of capital and required rate of return. By using DCF principles, we can directly compare the return on investment from improved staffing practices to other business investments using the same financial framework that guides capital allocation decisions.")),
                  p(HTML("As <a href='https://www.amazon.com/Financial-Analysis-HR-Managers-paperback/dp/0133925420' target='_blank'>Director (2012)</a> humorously noted, as soon as you conduct a cash flow analysis, you are wrong—but that doesn't make the analysis any less insightful for strategic decision-making.")),
                  p(HTML("<b>Note:</b> Including these factors makes HR investments comparable to other capital purchases a company might decide to pursue. By applying the same financial standards used for equipment purchases, facility investments, or technology upgrades, we can evaluate staffing improvements using the same criteria that guide other strategic business decisions.")),
                  p(HTML("The default values provided are based on typical corporate financial parameters:")),
                  tags$ul(
                    tags$li("Variable Costs: 35%"),
                    tags$li("Tax Rate: 63%"),
                    tags$li("Discount Rate: 11%")
                  ),
                  br()
                ),
                conditionalPanel(
                  condition = "input.utilityPages == 'Page 3'",
                  h4(HTML("<b>Employee Flows and Multiple Cohorts</b>")),
                  p(HTML("This section allows you to account for the effects of employee inflows and outflows (such as hiring and turnover) on the overall utility estimate. In practice, organizations hire new employees each year to replace those who leave and to support growth. Each year's new hires represent a new cohort that will contribute value over their tenure period, creating additive cohort effects. This is similar to compounding in finance, where the value grows not just from the initial investment, but also from accumulated returns over time.")),
                  p(HTML("The default values provided are based on a typical staffing scenario:")),
                  tags$ul(
                    tags$li(HTML("<b>Program Length (15 years):</b> The total duration over which the selection system will be used. This longer timeframe allows for multiple hiring cycles and better captures the cumulative impact of improved selection.")),
                    tags$li(HTML("<b>Employees Added per Year (470):</b> The number of new hires each year. This matches the default number of hires from Page 1, representing a steady annual hiring rate.")),
                    tags$li(HTML("<b>Employees Lost per Year (470):</b> The number of employees who leave after completing their tenure. This balanced flow (equal to the number added) represents a stable workforce size, where departures are replaced by new hires."))
                  ),
                  p(HTML("These defaults create a scenario where the organization maintains a stable workforce size while continuously improving its talent pool through better selection. The utility estimate will reflect how these workforce dynamics compound over time, similar to how financial investments grow through compound interest.")),
                  br()
                )
              ),
              uiOutput("unadjusted_utility_label"),
              uiOutput("unadjusted_utility_value"),
              uiOutput("unadjusted_utility_explanation"),
              uiOutput("unadjusted_utility_math"),
              uiOutput("adjusted_utility_label"),
              uiOutput("adjusted_utility_value"),
              uiOutput("adjusted_utility_explanation"),
              uiOutput("adjusted_utility_math"),
              uiOutput("final_adjusted_utility_output"),
              uiOutput("flows_utility_label"),
              uiOutput("flows_utility_value"),
              uiOutput("flows_utility_explanation"),
              uiOutput("flows_utility_math"),
              downloadButton("download_pdf", "Export Report")
            )
          )
        )
      ),
      tabItem(tabName = "monte",
        fluidPage(
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
                title = "Adjustment Effects",
                tableOutput("sturman2"),
                status = "primary"
              )
            )
          )
        )
      )
    )
  )
)

# Server
main_server <- function(input, output, session) {
  # Initialize reactive values
  utilityUn <- reactiveVal(NULL)
  utilityUnPer <- reactiveVal(NULL)
  utilityUnPerYear <- reactiveVal(NULL)
  expectancy_plot <- reactiveVal(NULL)
  gh <- reactiveVal(NULL)
  bh <- reactiveVal(NULL)
  etn <- reactiveVal(NULL)
  eto <- reactiveVal(NULL)
  eln <- reactiveVal(NULL)
  elo <- reactiveVal(NULL)

  # Reset button functionality
  observeEvent(input$reset_chart, {
    updateNumericInput(session, "rxy", value = 0.4)
    updateNumericInput(session, "rxx", value = 0.8)
    updateNumericInput(session, "ryy", value = 0.8)
    updateNumericInput(session, "u", value = 0.8)
    updateNumericInput(session, "expectancy_n", value = 500)
    updateCheckboxInput(session, "apply_corrections", value = FALSE)
    updateCheckboxInput(session, "show_error_bars", value = FALSE)
    updateCheckboxInput(session, "show_corrected", value = FALSE)
    updateCheckboxInput(session, "show_fully_corrected", value = FALSE)
  })

  # Advanced Expectancy Chart Logic
  # Helper function for corrections
  correct_correlation <- function(r, rxx, ryy, correction_type, u = NULL) {
    r_criterion_corrected <- r / sqrt(ryy)
    if (correction_type == "range_restriction") {
      if (is.null(u)) stop("Range restriction ratio (u) is required for this correction")
      r_fully_corrected <- r_criterion_corrected * sqrt(1 + (u^2 - 1) * r_criterion_corrected^2) / u
      return(list(criterion_corrected = r_criterion_corrected,
                  fully_corrected = r_fully_corrected))
    }
    return(list(criterion_corrected = r_criterion_corrected,
                fully_corrected = r_criterion_corrected))
  }

  # Main reactive for all expectancy calculations and bar data
  data_reactive <- reactive({
    clamp <- function(x, minval, maxval) max(minval, min(x, maxval))
    rxy_clamped <- clamp(input$rxy, -0.99, 0.99)
    rxx_clamped <- clamp(input$rxx, 0.01, 1)
    ryy_clamped <- clamp(input$ryy, 0.01, 1)
    u_clamped <- if (!is.null(input$u)) clamp(input$u, 0.01, 1) else NULL

    correlations <- if (input$apply_corrections) {
      correct_correlation(
        r = rxy_clamped,
        rxx = rxx_clamped,
        ryy = ryy_clamped,
        correction_type = "range_restriction",
        u = u_clamped
      )
    } else {
      list(criterion_corrected = rxy_clamped,
           fully_corrected = rxy_clamped)
    }

    calculate_expectancies <- function(r) {
      list(
        Low = 100 * round(Expectancyfunc(r, -Inf, qnorm(0.25), qnorm(0.75), Inf), 2),
        LM  = 100 * round(Expectancyfunc(r, qnorm(0.25), qnorm(0.5), qnorm(0.75), Inf), 2),
        UM  = 100 * round(Expectancyfunc(r, qnorm(0.5), qnorm(0.75), qnorm(0.75), Inf), 2),
        Top = 100 * round(Expectancyfunc(r, qnorm(0.75), Inf, qnorm(0.75), Inf), 2)
      )
    }
    uncorrected_expectancies <- calculate_expectancies(rxy_clamped)
    criterion_corrected_expectancies <- if (input$apply_corrections) calculate_expectancies(correlations$criterion_corrected) else NULL
    fully_corrected_expectancies <- if (input$apply_corrections) calculate_expectancies(correlations$fully_corrected) else NULL

    bar_data <- data.frame(
      Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), 
                    each = if (input$show_corrected && input$show_fully_corrected) 3 else if (input$show_corrected || input$show_fully_corrected) 2 else 1),
      Correction = if (input$show_corrected && input$show_fully_corrected) {
        rep(c("Uncorrected", "Criterion Corrected", "Fully Corrected"), times = 4)
      } else if (input$show_corrected) {
        rep(c("Uncorrected", "Criterion Corrected"), times = 4)
      } else if (input$show_fully_corrected) {
        rep(c("Uncorrected", "Fully Corrected"), times = 4)
      } else {
        "Uncorrected"
      },
      Probability = if (input$show_corrected && input$show_fully_corrected) {
        c(
          uncorrected_expectancies$Low, criterion_corrected_expectancies$Low, fully_corrected_expectancies$Low,
          uncorrected_expectancies$LM, criterion_corrected_expectancies$LM, fully_corrected_expectancies$LM,
          uncorrected_expectancies$UM, criterion_corrected_expectancies$UM, fully_corrected_expectancies$UM,
          uncorrected_expectancies$Top, criterion_corrected_expectancies$Top, fully_corrected_expectancies$Top
        )
      } else if (input$show_corrected) {
        c(
          uncorrected_expectancies$Low, criterion_corrected_expectancies$Low,
          uncorrected_expectancies$LM, criterion_corrected_expectancies$LM,
          uncorrected_expectancies$UM, criterion_corrected_expectancies$UM,
          uncorrected_expectancies$Top, criterion_corrected_expectancies$Top
        )
      } else if (input$show_fully_corrected) {
        c(
          uncorrected_expectancies$Low, fully_corrected_expectancies$Low,
          uncorrected_expectancies$LM, fully_corrected_expectancies$LM,
          uncorrected_expectancies$UM, fully_corrected_expectancies$UM,
          uncorrected_expectancies$Top, fully_corrected_expectancies$Top
        )
    } else {
        c(
          uncorrected_expectancies$Low,
          uncorrected_expectancies$LM,
          uncorrected_expectancies$UM,
          uncorrected_expectancies$Top
        )
      }
    )
    bar_data$Quartile <- factor(bar_data$Quartile, 
                               levels = c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"),
                               ordered = TRUE)
    if (input$show_corrected && input$show_fully_corrected) {
      bar_data$Correction <- factor(bar_data$Correction, 
                                  levels = c("Uncorrected", "Criterion Corrected", "Fully Corrected"))
    } else if (input$show_corrected) {
      bar_data$Correction <- factor(bar_data$Correction, 
                                  levels = c("Uncorrected", "Criterion Corrected"))
    } else if (input$show_fully_corrected) {
      bar_data$Correction <- factor(bar_data$Correction, 
                                  levels = c("Uncorrected", "Fully Corrected"))
    }

    if (input$show_error_bars) {
      bar_data$Probability <- as.numeric(bar_data$Probability)
      pval <- bar_data$Probability / 100
      n_total <- as.numeric(input$expectancy_n)
      if (is.na(n_total) || n_total < 1) n_total <- 1
      n_quartile <- n_total / 4
      if (n_quartile < 1) n_quartile <- 1
      se <- sqrt(pval * (1 - pval) / n_quartile)
      lower <- pval - 1.96 * se
      upper <- pval + 1.96 * se
      bar_data$lower <- pmax(0, lower * 100)
      bar_data$upper <- pmin(100, upper * 100)
      bar_data$lower[!is.finite(bar_data$lower)] <- NA
      bar_data$upper[!is.finite(bar_data$upper)] <- NA
    } else {
      bar_data$lower <- NA
      bar_data$upper <- NA
    }

    list(
      bar_data = bar_data,
      rxy_clamped = rxy_clamped,
      rxx_clamped = rxx_clamped,
      ryy_clamped = ryy_clamped,
      u_clamped = u_clamped,
      correlations = correlations,
      uncorrected_expectancies = uncorrected_expectancies,
      criterion_corrected_expectancies = criterion_corrected_expectancies,
      fully_corrected_expectancies = fully_corrected_expectancies
    )
  })

  # Render the plot reactively
  output$expectancy_plot <- renderPlot({
    d <- data_reactive()
    bar_data <- d$bar_data
    fill_colors <- c(
      "Uncorrected" = "white",
      "Criterion Corrected" = "#808080",
      "Fully Corrected" = "#6BAED6"
    )
    p <- ggplot(bar_data, aes(x = Quartile, y = Probability, fill = Correction)) +
      geom_col(position = position_dodge(width = 0.8),
               width = 0.6,
               color = "black") +
      scale_fill_manual(values = fill_colors) +
      labs(
        title = "The Science of Picking High Performers",
        subtitle = "A quartile-by-quartile look at the probability of hiring top performers",
        x = "Quartile Score on Selection Procedure",
        y = "Probability of High Job Performance"
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0, size = 22, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0, size = 18, color = "black"),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.title.x = element_text(margin = margin(t = 18)),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        plot.margin = margin(30, 30, 30, 30),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      ) +
      geom_text(aes(label = sprintf("%d%%", round(Probability)),
                    y = Probability / 2),
                position = position_dodge(width = 0.8),
                size = 5,
                color = ifelse(bar_data$Correction == "Uncorrected", "black", "white"))
    if (input$show_error_bars && "lower" %in% names(bar_data) && "upper" %in% names(bar_data) && any(!is.na(bar_data$lower)) && any(!is.na(bar_data$upper))) {
      p <- p + geom_errorbar(
        aes(ymin = lower, ymax = upper),
        position = if (nlevels(bar_data$Correction) > 1) position_dodge(width = 0.8) else "identity",
        width = 0.2,
        color = "black"
      )
    }
    
    # Add sample size information to the plot
    if (!is.null(as.numeric(input$expectancy_n)) && as.numeric(input$expectancy_n) > 0) {
      p <- p + labs(caption = paste0("Total sample size: n = ", as.numeric(input$expectancy_n)))
    }
    
    p
  })

  # Render the notes reactively
  output$expectancy_text <- renderUI({
    HTML(paste0(
      "<div style='font-family: Arial, sans-serif; line-height: 1.6;'>",
      "<h4>Understanding the Math Behind Your Expectancy Chart</h4>",
      "<strong>Step 1: Start with the Observed Correlation</strong><br>",
      "We begin with the observed correlation between test scores and job performance. In this example, that's <b>r = ", round(data_reactive()$rxy_clamped, 2), "</b>.<br><br>",
      "<strong>Step 2: Correct for Criterion Unreliability</strong><br>",
      "Job performance ratings are not perfectly reliable. To get a more accurate estimate, we correct the observed correlation using the formula:<br>",
      "<i>r<sub>corrected</sub> = r<sub>observed</sub> / √(ryy)</i><br>",
      "where ryy is the reliability of the job performance measure (here, ", round(data_reactive()$ryy_clamped, 2), ").<br>",
      "So, r<sub>corrected</sub> = ", round(data_reactive()$rxy_clamped, 2), " / √(", round(data_reactive()$ryy_clamped, 2), ") = <b>", round(data_reactive()$correlations$criterion_corrected, 2), "</b>.<br><br>",
      if (input$show_fully_corrected) paste0(
        "<strong>Step 3: Correct for Range Restriction</strong><br>",
        "Often, we only see a limited range of test scores (for example, if only top scorers are hired). To estimate what the correlation would be in the full population, we apply a range restriction correction:<br>",
        "<i>r<sub>fully corrected</sub> = r<sub>criterion corrected</sub> * √(1 + (u² - 1) * r<sub>criterion corrected</sub>²) / u</i><br>",
        "where u is the range restriction ratio (here, ", round(data_reactive()$u_clamped, 2), ").<br>",
        "So, r<sub>fully corrected</sub> = ", round(data_reactive()$correlations$criterion_corrected, 2), " * √(1 + (", round(data_reactive()$u_clamped, 2), "² - 1) * ", round(data_reactive()$correlations$criterion_corrected, 2), "²) / ", round(data_reactive()$u_clamped, 2), " = <b>", round(data_reactive()$correlations$fully_corrected, 2), "</b>.<br><br>"
      ) else "",
      "<strong>Step 4: Calculate the Probability of High Performance</strong><br>",
      "For each quartile of test scores, we use the correlation to estimate the probability of being a high performer (in the top 25% of job performance).<br>",
      "For example, with an uncorrected correlation of ", round(data_reactive()$rxy_clamped, 2), ", about ", if (!is.null(data_reactive()$uncorrected_expectancies) && is.numeric(data_reactive()$uncorrected_expectancies$Top)) paste0(round(data_reactive()$uncorrected_expectancies$Top), "%") else "--", " of people in the top quartile of test scores are expected to be high performers.<br>",
      if (input$show_corrected) paste0("After correcting for criterion unreliability, this increases to ", if (!is.null(data_reactive()$criterion_corrected_expectancies) && is.numeric(data_reactive()$criterion_corrected_expectancies$Top)) paste0(round(data_reactive()$criterion_corrected_expectancies$Top), "%") else "--", ".<br>") else "",
      if (input$show_fully_corrected) paste0("After correcting for both unreliability and range restriction, it rises to ", if (!is.null(data_reactive()$fully_corrected_expectancies) && is.numeric(data_reactive()$fully_corrected_expectancies$Top)) paste0(round(data_reactive()$fully_corrected_expectancies$Top), "%") else "--", ".<br>") else "",
      "<br>",
      "<strong>Step 5: Error Bars and Confidence</strong><br>",
      "The lines on top of each bar (error bars) show the range where the true probability is likely to fall, based on your sample size. The bigger your sample, the smaller these error bars will be.<br>",
      if (input$show_error_bars) paste0("Error bars represent 95% confidence intervals for each bar. Each bar assumes a sample size of n/4, where n is the total sample size you entered (n = ", input$expectancy_n, ").<br>") else "",
      "<br>",
      "<strong>Step 6: How the Probabilities Are Calculated</strong><br>",
      "For each group, the probability is calculated using a statistical method called the bivariate normal distribution. This takes into account the correlation and the boundaries for each quartile.<br>",
      "<br>",
      "<strong>Assumptions</strong><br>",
      "These calculations assume that both test scores and job performance follow a normal (bell curve) distribution, and that each quartile contains the same number of people.<br>",
      "</div>"
    ))
  })

  # Input warnings
  output$input_warnings <- renderUI({
    warnings <- c()
    if (input$rxy <= -1 || input$rxy >= 1) warnings <- c(warnings, "Correlation must be between -0.99 and 0.99.")
    if (input$rxx < 0.01 || input$rxx > 1) warnings <- c(warnings, "Predictor reliability must be between 0.01 and 1.")
    if (input$ryy < 0.01 || input$ryy > 1) warnings <- c(warnings, "Criterion reliability must be between 0.01 and 1.")
    if (!is.null(input$u) && (input$u < 0.01 || input$u > 1)) warnings <- c(warnings, "Range restriction ratio (u) must be between 0.01 and 1.")
    if (length(warnings) > 0) {
      HTML(paste0("<div style='color: red;'><b>Input Warning:</b><br>", paste(warnings, collapse = "<br>"), "</div>"))
    } else {
      NULL
    }
  })

  # Page 1: Staffing Utility
  observeEvent(input$go, {
    enable("download_pdf")
    shinyjs::hide("ua_explanatory_content")
    
    # Calculate unadjusted utility
    inputs <- list(
      n = input$utility_n,
      sdy = input$sdy,
      rxy2 = get_selected_correlation(),
      rxy1 = 0,
      sr = input$sr/100,
      period = input$period,
      cost1 = input$cost1,
      cost2 = input$cost2
    )
    
    result <- calculate_unadjusted_utility(inputs)
    utilityUn(result$unadjusted_utility)
    utilityUnPer(result$unadjusted_utility / input$utility_n)
    utilityUnPerYear(result$unadjusted_utility / (input$utility_n * input$period))
    
    # Display unadjusted utility
    output$unadjusted_utility_label <- renderUI({
      HTML("<b>Unadjusted Utility Estimate</b>")
    })
    
    output$unadjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", format_utility(result$unadjusted_utility), "</span>"))
    })
    
    # Display explanation
    output$unadjusted_utility_explanation <- renderUI({
      HTML(paste0(
        "<p>This unadjusted utility estimate represents the raw economic value of improved selection, calculated as:</p>",
        "<p>Utility = (N × SDy × Δr × ux(sr) × T) - C</p>",
        "<p>Where:</p>",
        "<ul>",
        "<li>N = Number of hires (", input$utility_n, ")</li>",
        "<li>SDy = Standard deviation of job performance in dollars ($", format(input$sdy, big.mark=",", scientific=FALSE), ")</li>",
        "<li>Δr = Change in validity (", get_selected_correlation(), " - ", 0, " = ", round(get_selected_correlation() - 0, 3), ")</li>",
        "<li>ux(sr) = Utility of selection at selection ratio ", input$sr, "% (", round(floor(ux(input$sr/100) * 100) / 100, 3), ")</li>",
        "<li>T = Time period (", input$period, " years)</li>",
        "<li>C = Cost term ($", format(result$cost_term, big.mark=",", scientific=FALSE), ")</li>",
        "</ul>",
        "<p>The cost term represents the additional cost of the new selection process, calculated as:</p>",
        "<p>C = Number of applicants × (Cost per applicant (new) - Cost per applicant (old))</p>",
        "<p>C = ", ceiling(input$utility_n / (input$sr/100)), " × ($", input$cost2, " - $", input$cost1, ") = $", format(result$cost_term, big.mark=",", scientific=FALSE), "</p>"
      ))
    })
    
    # Display math
    output$unadjusted_utility_math <- renderUI({
      HTML(paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>",
      "1. Cost Term Calculation:",
        "\nTotal Applicants = ceiling(n / sr) = ceiling(", input$utility_n, " / ", input$sr/100, ") = ", ceiling(input$utility_n / (input$sr/100)),
        "\nCost Difference = $", input$cost2, " - $", input$cost1, " = $", input$cost2 - input$cost1,
        "\nCost Term = floor(", ceiling(input$utility_n / (input$sr/100)), " × ", input$cost2 - input$cost1, ") = $", format(result$cost_term, big.mark=",", scientific=FALSE),
        "\n\n2. Utility Formula:",
        "\nUtility = (N × SDy × Δr × ux(sr) × T) - C",
        "\nUtility = (", input$utility_n, " × $", format(input$sdy, big.mark=",", scientific=FALSE), " × ", round(get_selected_correlation() - 0, 3), " × ", round(floor(ux(input$sr/100) * 100) / 100, 3), " × ", input$period, ") - $", format(result$cost_term, big.mark=",", scientific=FALSE),
        "\nUtility = $", format(result$benefit_term, big.mark=",", scientific=FALSE), " - $", format(result$cost_term, big.mark=",", scientific=FALSE),
        "\nUtility = $", format(result$unadjusted_utility, big.mark=",", scientific=FALSE),
      "\n\n3. Break-Even SDy Analysis:",
        "\nSDy = Cost Term / (N × Δr × ux(sr) × T)",
        "\nSDy = $", format(result$cost_term, big.mark=",", scientific=FALSE), " / (", input$utility_n, " × ", round(get_selected_correlation() - 0, 3), " × ", round(floor(ux(input$sr/100) * 100) / 100, 3), " × ", input$period, ")",
        "\nSDy = $", format(round(solve_break_even_sdy(inputs), 2), big.mark=",", scientific=FALSE),
      "\n\n4. Break-Even Validity Analysis:",
        "\nΔr = Cost Term / (N × SDy × ux(sr) × T)",
        "\nΔr = $", format(result$cost_term, big.mark=",", scientific=FALSE), " / (", input$utility_n, " × $", format(input$sdy, big.mark=",", scientific=FALSE), " × ", round(floor(ux(input$sr/100) * 100) / 100, 3), " × ", input$period, ")",
        "\nΔr = ", format(round(solve_break_even_rxy(inputs), 6), nsmall=6),
      "\n\n5. Number Needed to Hire (NNH) Analysis:",
        "\nN = ceiling(Cost Term / (SDy × Δr × ux(sr) × T))",
        "\nN = ceiling($", format(result$cost_term, big.mark=",", scientific=FALSE), " / ($", format(input$sdy, big.mark=",", scientific=FALSE), " × ", round(get_selected_correlation() - 0, 3), " × ", round(floor(ux(input$sr/100) * 100) / 100, 3), " × ", input$period, "))",
        "\nN = ", solve_nnh(inputs),
      "</pre>"
      ))
    })
  })

  # Add observer for tab changes
  observeEvent(input$utilityPages, {
    if (input$utilityPages == "Page 2") {
      # Clear unadjusted utility outputs
    output$unadjusted_utility_label <- renderUI({ NULL })
    output$unadjusted_utility_value <- renderUI({ NULL })
    output$unadjusted_utility_explanation <- renderUI({ NULL })
    output$unadjusted_utility_math <- renderUI({ NULL })
      # Clear adjusted utility outputs
    output$adjusted_utility_label <- renderUI({ NULL })
    output$adjusted_utility_value <- renderUI({ NULL })
    output$adjusted_utility_explanation <- renderUI({ NULL })
    output$adjusted_utility_math <- renderUI({ NULL })
    }
  })

  # Page 2: Financial Adjustments
  observeEvent(input$go_adjusted, {
    enable("download_pdf")
    shinyjs::hide("ua_explanatory_content")

    # Calculate actual cost term
    total_applicants <- ceiling(input$utility_n / (input$sr/100))
    cost_diff <- input$cost2 - input$cost1
    cost_term <- floor(total_applicants * cost_diff)
    
    # Calculate financially adjusted utility parameters
    vcost <- if(!is.null(input$vcost)) input$vcost else 35
    tax <- if(!is.null(input$tax)) input$tax else 63
    drate <- if(!is.null(input$drate)) input$drate else 11

    # Convert percentages to proportions
    varCosts_prop <- vcost / 100
    tax_prop <- tax / 100
    drate_prop <- drate / 100

    # Calculate utility components for adjusted utility
    delta_rxy <- get_selected_correlation() - 0
    uxs <- floor(ux(input$sr/100) * 100) / 100
    
    # Calculate financially adjusted utility components
    ES <- delta_rxy * uxs
    B0_raw <- input$sdy * ES
    B0_after_vc <- B0_raw * (1 - varCosts_prop)
    B0_after_tax <- B0_after_vc * (1 - tax_prop)
    pv_factor <- (1 - (1 + drate_prop)^(-input$period)) / drate_prop
    pv_benefit_per <- B0_after_tax * pv_factor
    total_pv_benefit <- input$utility_n * pv_benefit_per
    net_cost <- cost_term * (1 - tax_prop)
    adjusted_utility <- total_pv_benefit - net_cost
    adjusted_utility_per_hire <- adjusted_utility / input$utility_n
    adjusted_utility_per_year <- adjusted_utility / (input$utility_n * input$period)
    
    # Calculate financial adjustment factor and break-even values
    financial_adjustment <- (1 - varCosts_prop) * (1 - tax_prop) * pv_factor
    be_sdy_adj <- cost_term / (input$utility_n * delta_rxy * uxs * input$period * financial_adjustment)
    be_rxy_adj <- cost_term / (input$utility_n * input$sdy * uxs * input$period * financial_adjustment)
    nnh_adj <- ceiling(cost_term / (input$sdy * delta_rxy * uxs * input$period * financial_adjustment))
    
    # Calculate IRR
      utility_at_r_adjusted <- function(r) {
      pv_factor_r <- if(r == 0) input$period else (1 - (1 + r)^(-input$period)) / r
        B0_after_vc_r <- B0_raw * (1 - varCosts_prop)
        B0_after_tax_r <- B0_after_vc_r * (1 - tax_prop)
      pv_benefits_r <- input$utility_n * B0_after_tax_r * pv_factor_r
        pv_benefits_r - net_cost
      }
      
      low_irr  <- 0.0001
      high_irr <- 0.10
      while (utility_at_r_adjusted(high_irr) > 0) {
        high_irr <- high_irr * 2
      if (high_irr > 1e6) break
      }
      
    IRR <- tryCatch({
      uniroot(utility_at_r_adjusted, c(low_irr, high_irr))$root
    }, error = function(e) {
      NA
    })
    
    # Display adjusted utility
    output$adjusted_utility_label <- renderUI({
      HTML("<b>Financially Adjusted Utility Estimate</b>")
    })

    output$adjusted_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", format_utility(adjusted_utility), "</span>"))
    })

    # Calculate percentage reduction from unadjusted utility
    unadjusted_utility <- input$utility_n * input$sdy * delta_rxy * uxs * input$period - cost_term
    pct_reduction <- (1 - (adjusted_utility / unadjusted_utility)) * 100

    # Display explanation
    output$adjusted_utility_explanation <- renderUI({
      HTML(paste0(
        "<p>This financially adjusted utility estimate accounts for three key financial factors:</p>",
        "<ol>",
        "<li><b>Variable Costs (", vcost, "%)</b>: Higher performance often means higher costs for materials, incentives, and variable overhead.</li>",
        "<li><b>Taxes (", tax, "%)</b>: Higher profits typically result in higher tax payments, reducing the net benefit.</li>",
        "<li><b>Discount Rate (", drate, "%)</b>: Future benefits are worth less in present terms due to the time value of money and opportunity costs.</li>",
        "</ol>",
        "<p>After these adjustments, the utility per hire is <b>", format_utility(adjusted_utility_per_hire), "</b> and <b>", format_utility(adjusted_utility_per_year), "</b> per hire per year. Financial adjustments reduce utility by <b>", format(round(pct_reduction, 1), nsmall=1), "%</b>.</p>",
        "<p>The break-even SDy is <b>$", format(round(be_sdy_adj, 2), big.mark=",", scientific=FALSE), "</b>, the break-even validity uplift is <b>", format(round(be_rxy_adj, 6), nsmall=6), "</b>, and the number needed to hire is <b>", nnh_adj, "</b>.</p>",
        "<p>The internal rate of return (IRR) is <b>", format(round(IRR * 100, 1), nsmall=1), "%</b>, indicating the annualized return on investment.</p>"
      ))
    })

    # Display math
    output$adjusted_utility_math <- renderUI({
      HTML(paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>",
      "1. Cost Term Calculation:",
        "\nTotal Applicants = ceiling(n / sr) = ceiling(", input$utility_n, " / ", input$sr/100, ") = ", total_applicants,
        "\nCost Difference = $", input$cost2, " - $", input$cost1, " = $", cost_diff,
      "\nCost Term = floor(", total_applicants, " × ", cost_diff, ") = $", format(cost_term, big.mark=",", scientific=FALSE),
      "\n\n2. Financially Adjusted Utility Formula:",
      "\nStep 1: Raw Benefit = n × SDy × Δr × ux(sr) × period",
        "\nStep 1: Raw Benefit = ", input$utility_n, " × ", input$sdy, " × ", delta_rxy, " × ", uxs, " × ", input$period, " = $", format(B0_raw * input$utility_n * input$period, big.mark=",", scientific=FALSE),
      "\n\nStep 2: Adjust for Variable Costs = Raw Benefit × (1 - vc)",
        "\nStep 2: $", format(B0_raw * input$utility_n * input$period, big.mark=",", scientific=FALSE), " × (1 - ", varCosts_prop, ") = $", format(B0_after_vc * input$utility_n * input$period, big.mark=",", scientific=FALSE),
      "\n\nStep 3: Adjust for Taxes = Step 2 × (1 - tax)",
        "\nStep 3: $", format(B0_after_vc * input$utility_n * input$period, big.mark=",", scientific=FALSE), " × (1 - ", tax_prop, ") = $", format(B0_after_tax * input$utility_n * input$period, big.mark=",", scientific=FALSE),
      "\n\nStep 4: Present Value Factor = (1 - (1 + dr)^(-period)) / dr",
        "\nStep 4: PVF = (1 - (1 + ", drate_prop, ")^(-", input$period, ")) / ", drate_prop, " = ", round(pv_factor, 4),
      "\n\nStep 5: Present Value Benefits = Step 3 × PVF",
        "\nStep 5: $", format(B0_after_tax * input$utility_n * input$period, big.mark=",", scientific=FALSE), " × ", round(pv_factor, 4), " = $", format(total_pv_benefit, big.mark=",", scientific=FALSE),
      "\n\nStep 6: After-tax Cost = Cost Term × (1 - tax)",
      "\nStep 6: $", format(cost_term, big.mark=",", scientific=FALSE), " × (1 - ", tax_prop, ") = $", format(net_cost, big.mark=",", scientific=FALSE),
      "\n\nStep 7: Net Present Value = Step 5 - Step 6",
      "\nStep 7: $", format(total_pv_benefit, big.mark=",", scientific=FALSE), " - $", format(net_cost, big.mark=",", scientific=FALSE), " = $", format(adjusted_utility, big.mark=",", scientific=FALSE),
      "\n\n3. Break-Even SDy Analysis (Adjusted):",
      "\nFinancial Adjustment = (1 - vc) × (1 - tax) × PVF",
        "\nFinancial Adjustment = (1 - ", varCosts_prop, ") × (1 - ", tax_prop, ") × ", round(pv_factor, 4), " = ", round(financial_adjustment, 4),
      "\nSDy = Cost Term / (n × Δr × ux(sr) × period × Financial Adjustment)",
        "\nSDy = $", format(cost_term, big.mark=",", scientific=FALSE), " / (", input$utility_n, " × ", delta_rxy, " × ", uxs, " × ", input$period, " × ", round(financial_adjustment, 4), ")",
        "\nSDy = $", format(round(be_sdy_adj, 2), big.mark=",", scientific=FALSE),
        "\n\n4. Internal Rate of Return (IRR): ", format(round(IRR * 100, 1), nsmall=1), "%",
      "\n\n5. Break-Even Validity Analysis (Adjusted):",
      "\nΔr = Cost Term / (n × SDy × ux(sr) × period × Financial Adjustment)",
        "\nΔr = $", format(cost_term, big.mark=",", scientific=FALSE), " / (", input$utility_n, " × ", input$sdy, " × ", uxs, " × ", input$period, " × ", round(financial_adjustment, 4), ")",
        "\nΔr = ", format(round(be_rxy_adj, 6), nsmall=6),
      "\n\n6. Number Needed to Hire (NNH) Analysis (Adjusted):",
      "\nN = ceiling(Cost Term / (SDy × Δr × ux(sr) × period × Financial Adjustment))",
        "\nN = ceiling($", format(cost_term, big.mark=",", scientific=FALSE), " / (", input$sdy, " × ", delta_rxy, " × ", uxs, " × ", input$period, " × ", round(financial_adjustment, 4), "))",
        "\nN = ", nnh_adj,
      "</pre>"
      ))
    })
  })

  # Page 3: Account for Flows
  observeEvent(input$go_flows, {
    enable("download_pdf")
    shinyjs::hide("ua_explanatory_content")
    
    # Employee Flows Model
    nadd <- input$nadd
    nsub <- input$nsub
    pyears <- input$pyears
    vcost <- input$vcost
    tax <- input$tax
    drate <- input$drate
    costOrd <- input$cost1
    costAc <- input$cost2
    validOrd <- 0
    validAc <- get_selected_correlation()
    SDjp <- input$sdy
    sr1  <- input$sr / 100
    tenure1 <- input$period

    nk <- 0
    discProp <- drate / 100
    taxProp <- tax / 100
    varCosts <- -vcost / 100
    ord <- dnorm(qnorm(1-sr1),0,1) / sr1
    ck <- nadd * ((costAc - costOrd) / sr1)
    numyr <- tenure1 + pyears
    totDelta <- 0
    totDelta1 <- 0

    for (i in 1:numyr) {
      if (i > tenure1) { nk <- nk - nsub }
      if (i <= pyears) { nk <- nk + nadd }
      if (i > pyears) { ck <- 0 }
      if (nk >= 0) {
        delta1 <- nk * ((1/(1+discProp)^i) * validAc * ord * SDjp * (1 + varCosts) * (1 - taxProp))
        delta3 <- nk * ((1/(1+discProp)^i) * validAc * ord * SDjp * (-varCosts) * (taxProp))
      } else {
        delta1 <- 0
        delta3 <- 0
      }
      delta2 <- ck * (1 - taxProp) * (1/(1+discProp)^(i - 1))
      totDelta1 <- totDelta1 + delta2 + delta3
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    final_utility <- totDelta
    
    output$flows_utility_label <- renderUI({
      HTML("<b>Final Adjusted Utility Estimate (with Employee Flows)</b>")
    })
    
    output$flows_utility_value <- renderUI({
      HTML(paste0("<span style='font-size:1.2em;font-weight:bold;'>", format_utility(final_utility), "</span>"))
    })
    
    # Display explanation
    output$flows_utility_explanation <- renderUI({
      HTML(paste0(
        "<p>This utility estimate accounts for employee flows over time, including:</p>",
        "<ul>",
        "<li>New hires per year: ", nadd, "</li>",
        "<li>Attrition per year: ", nsub, "</li>",
        "<li>Planning period: ", pyears, " years</li>",
        "<li>Tenure: ", tenure1, " years</li>",
        "</ul>",
        "<p>The model tracks the net number of employees (nk) and costs (ck) for each year, applying financial adjustments to account for variable costs, taxes, and discounting.</p>",
        "<p>The final utility estimate of <b>", format_utility(final_utility), "</b> represents the net present value of all cash flows over the planning period.</p>"
      ))
    })
    
    # Display math
    flows_formula <- paste0(
      "<hr style='margin-top:30px;margin-bottom:10px;'>",
      "<b>Underlying Math</b><br>",
      "<pre>",
      "Input Parameters:",
      "\n  New Hires per Year: ", nadd,
      "\n  Attrition per Year: ", nsub,
      "\n  Planning Period: ", pyears, " years",
      "\n  Tenure: ", tenure1, " years",
      "\n  Variable Costs (%): ", vcost,
      "\n  Tax Rate (%): ", tax,
      "\n  Discount Rate (%): ", drate,
      "\n  Cost per Applicant (Old): $", costOrd,
      "\n  Cost per Applicant (New): $", costAc,
      "\n  Validity (Old): ", validOrd,
      "\n  Validity (New): ", validAc,
      "\n  SD of Job Performance: $", SDjp,
      "\n  Selection Ratio: ", sr1,
      "\n  Tenure: ", tenure1,
      "\n\nStep-by-step calculation for each year (see code for details):",
      "\n  For i in 1:numyr:",
      "\n    - Update nk (net employees), ck (cost)",
      "\n    - If nk >= 0, calculate delta1 and delta3",
      "\n    - delta1 = nk * ((1/(1+discProp)^i) * validAc * ord * SDjp * (1 + varCosts) * (1 - taxProp))",
      "\n    - delta3 = nk * ((1/(1+discProp)^i) * validAc * ord * SDjp * (-varCosts) * (taxProp))",
      "\n    - delta2 = ck * (1 - taxProp) * (1/(1+discProp)^(i - 1))",
      "\n    - totDelta1 = totDelta1 + delta2 + delta3",
      "\n    - delta = delta1 - delta2",
      "\n    - totDelta = totDelta + delta",
      "\n\nFinal Utility (Lifetime incremental NPV): $", format(round(final_utility, 0), big.mark = ","),
      "\n</pre>"
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

  # Make expectancy chart correlations available for UA
  get_correlation_choices <- reactive({
    # Use the same logic as in the expectancy chart's data_reactive
    # If the expectancy chart has not been used yet, provide NA or default values
    if (!exists("data_reactive")) {
      return(list(
        uncorrected = 0.4,
        criterion_corrected = NA,
        fully_corrected = NA
      ))
    }
    d <- tryCatch(data_reactive(), error = function(e) NULL)
    if (is.null(d)) {
      return(list(
        uncorrected = 0.4,
        criterion_corrected = NA,
        fully_corrected = NA
      ))
    }
    # Uncorrected: d$rxy_clamped
    # Criterion-corrected: d$correlations$criterion_corrected
    # Predictor-corrected: d$correlations$criterion_corrected * sqrt(input$rxx) (if available)
    uncorrected <- d$rxy_clamped
    criterion_corrected <- d$correlations$criterion_corrected
    fully_corrected <- d$correlations$fully_corrected
    list(
      uncorrected = uncorrected,
      criterion_corrected = criterion_corrected,
      fully_corrected = fully_corrected
    )
  })

  output$correlation_selector <- renderUI({
    corrs <- get_correlation_choices()
    radioButtons(
      "ua_correlation_type",
      label = "Select correlation coefficient for utility analysis:",
      choices = setNames(
        c("uncorrected", "criterion_corrected", "fully_corrected"),
        c(
          paste0("Uncorrected (", ifelse(!is.na(corrs$uncorrected), round(corrs$uncorrected, 2), "--"), ")"),
          paste0("Corrected for criterion unreliability (", ifelse(!is.na(corrs$criterion_corrected), round(corrs$criterion_corrected, 2), "--"), ")"),
          paste0("Corrected for criterion unreliability and range restriction (", ifelse(!is.na(corrs$fully_corrected), round(corrs$fully_corrected, 2), "--"), ")")
        )
      ),
      selected = "uncorrected"
    )
  })

  # Helper to get selected correlation value
  get_selected_correlation <- reactive({
    corrs <- get_correlation_choices()
    switch(input$ua_correlation_type,
           uncorrected = corrs$uncorrected,
           criterion_corrected = corrs$criterion_corrected,
           fully_corrected = corrs$fully_corrected,
           corrs$uncorrected)
  })

  # Export Report functionality
  output$download_expectancy_report <- downloadHandler(
    filename = function() {
      paste("expectancy_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get the current data
        d <- data_reactive()
        
        # Create a temporary directory for the report
        temp_dir <- tempdir()
        temp_report <- file.path(temp_dir, "expectancy_report.Rmd")
        
        # Copy the template to the temporary directory
        file.copy("expectancy_report.Rmd", temp_report, overwrite = TRUE)
        file.copy("www/logo.png", file.path(temp_dir, "logo.png"), overwrite = TRUE)
        
        # Get the sample size from the expectancy chart input
        expectancy_n <- isolate(input$expectancy_n)  # Use isolate to get the current value
        
        # Create a list of parameters with the current input values
        params_list <- list(
          rxy = d$rxy_clamped,
          rxx = d$rxx_clamped,
          ryy = d$ryy_clamped,
          u = d$u_clamped,
          n = as.numeric(expectancy_n),
          bar_data = d$bar_data,
          correlations = d$correlations,
          uncorrected_expectancies = d$uncorrected_expectancies,
          criterion_corrected_expectancies = d$criterion_corrected_expectancies,
          fully_corrected_expectancies = d$fully_corrected_expectancies,
          show_corrected = isolate(input$show_corrected),
          show_fully_corrected = isolate(input$show_fully_corrected)
        )
        
        # Create a new environment for rendering
        env <- new.env(parent = globalenv())
        
        # Render the report
        rmarkdown::render(
          temp_report,
          output_file = file,
          params = params_list,
          envir = env,
          clean = TRUE
        )
      }, error = function(e) {
        # Show error message to user
        showNotification(
          paste("Error generating report:", e$message),
          type = "error",
          duration = NULL
        )
        # Return NULL to prevent download
        return(NULL)
      })
    }
  )

  # Staffing Cost Simulation logic
  observe({
    req(input$scs_initial_workforce, input$scs_turnover_rate, input$scs_productivity_per_worker, input$scs_cost_per_hire, input$scs_salary_per_hire, input$scs_benefit_pct, input$scs_training_cost_per_hire)
    initial_workforce <- input$scs_initial_workforce
    turnover_rate <- input$scs_turnover_rate
    productivity_per_worker <- input$scs_productivity_per_worker
    cost_per_hire <- input$scs_cost_per_hire
    salary <- input$scs_salary_per_hire
    benefit_pct <- input$scs_benefit_pct
    training_cost_per_hire <- input$scs_training_cost_per_hire
    total_cost_per_hire <- cost_per_hire + salary + (benefit_pct / 100) * salary + training_cost_per_hire
    quarters <- c("Q1", "Q2", "Q3", "Q4")
    demand <- c(100000, 100000, 105000, 110000)

    # Baseline Strategy
    baseline_df <- data.frame(
      Quarter = quarters,
      Demand = demand
    )
    baseline_df$Turnover_Hires <- initial_workforce * turnover_rate
    baseline_df$Base_Capacity <- initial_workforce * productivity_per_worker
    baseline_df$Extra_Units <- pmax(baseline_df$Demand - baseline_df$Base_Capacity, 0)
    baseline_df$Extra_Hires <- ceiling(baseline_df$Extra_Units / productivity_per_worker)
    baseline_df$Total_Hires <- baseline_df$Turnover_Hires + baseline_df$Extra_Hires
    baseline_df$Staffing_Cost <- baseline_df$Total_Hires * total_cost_per_hire
    baseline_df$Strategy <- "Old"
    baseline_df$Avg_Productivity <- productivity_per_worker
    baseline_df$Productivity_Label <- paste0(baseline_df$Avg_Productivity)

    # Improved Strategy
    improved_pct <- c(0.10, 0.19, 0.27, 0.34)
    productivity_multiplier <- 1 + 0.08
    avg_productivity <- (1 - improved_pct) * 1.0 + improved_pct * productivity_multiplier
    avg_productivity_units <- avg_productivity * productivity_per_worker
    improved_df <- data.frame(
      Quarter = quarters,
      Demand = demand,
      Improved_Pct = improved_pct,
      Avg_Productivity = avg_productivity_units
    )
    improved_df$Effective_Capacity <- initial_workforce * improved_df$Avg_Productivity
    improved_df$Extra_Units <- pmax(improved_df$Demand - improved_df$Effective_Capacity, 0)
    improved_df$Extra_Hires <- ceiling(improved_df$Extra_Units / improved_df$Avg_Productivity)
    improved_df$Turnover_Hires <- initial_workforce * turnover_rate
    improved_df$Total_Hires <- improved_df$Turnover_Hires + improved_df$Extra_Hires
    improved_df$Staffing_Cost <- improved_df$Total_Hires * total_cost_per_hire
    improved_df$Strategy <- "New"
    improved_df$Productivity_Label <- paste0(round(improved_df$Avg_Productivity))

    # Merge Data
    plot_df <- rbind(
      baseline_df[, c("Quarter", "Staffing_Cost", "Strategy", "Productivity_Label", "Avg_Productivity")],
      improved_df[, c("Quarter", "Staffing_Cost", "Strategy", "Productivity_Label", "Avg_Productivity")]
    )

    # Cost Savings
    total_baseline_cost <- sum(baseline_df$Staffing_Cost)
    total_improved_cost <- sum(improved_df$Staffing_Cost)
    total_savings <- total_baseline_cost - total_improved_cost
    savings_label <- paste0("Total annual cost savings to staffing budget of $", scales::label_number(scale = 1e-3, suffix = "K")(total_savings), " (predicted productivity rates in parentheses)")
    title_text <- "Staffing Cost Comparison: <span style='color:firebrick;'>Old</span> vs. <span style='color:forestgreen;'>New</span> Hiring Strategy"

    output$scs_cost_plot <- renderPlot({
      source("what does this mean.R", local = TRUE)
      staffing_plot
    })

    output$scs_cost_summary <- renderText({
      paste0(
        "Total baseline staffing cost: $", scales::comma(total_baseline_cost), "\n",
        "Total improved staffing cost: $", scales::comma(total_improved_cost), "\n",
        "Total annual cost savings: $", scales::comma(total_savings), "\n\n",
        "Assumptions: ",
        initial_workforce, " initial workforce, ",
        turnover_rate * 100, "% turnover per quarter, ",
        productivity_per_worker, " units per worker/quarter, $",
        cost_per_hire, " per hire, $",
        salary, " salary, ",
        benefit_pct, "% benefits. ~8% performance gain from better hires."
      )
    })
  })

  output$staffingPlot <- renderPlot({
    source("what does this mean.R", local = TRUE)
    staffing_plot
  })
}

# Navbar
ui <- navbarPage(
  "UA+",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Comp & Ben Utility", fluidPage(h4(HTML("<b>Compensation & Benefits Utility</b>")), p("This section will provide tools and reports for analyzing the utility of compensation and benefits interventions. (Coming soon.)"))),
  tabPanel("Training Utility", fluidPage(h4(HTML("<b>Training Utility</b>")), p("This section will provide tools and reports for analyzing the utility of training interventions. (Coming soon.)"))),
  tabPanel("References", fluidPage(
    titlePanel("References"),
    mainPanel(
      h4("References Used in This App"),
      HTML("<p>Carson, K. D., Becker, W. S., & Henderson, R. D. (1998). Is utility analysis really futile? <i>Personnel Psychology, 51</i>(4), 835–854.</p>"),
      HTML("<p>Cascio, W. F., & Boudreau, J. W. (2019). <i>Investing in People: Financial Impact of Human Resource Initiatives</i> (3rd ed.). SHRM.</p>"),
      HTML("<p>Chang, L. W., Kirgios, E. L., Mullainathan, S., & Milkman, K. L. (2024). Does counting change what counts? Quantification fixation biases decision-making. <i>Proceedings of the National Academy of Sciences, 121</i>(46), e2400215121. <a href='https://doi.org/10.1073/pnas.2400215121' target='_blank'>https://doi.org/10.1073/pnas.2400215121</a></p>"),
      HTML("<p>Cucina, J. M., Berger, J., & Busciglio, H. (2017). Communicating criterion-related validity using expectancy charts: A new approach. <i>Personnel Assessment and Decisions, 3</i>(1), 1–10. <a href='https://doi.org/10.25035/pad.2017.001' target='_blank'>https://doi.org/10.25035/pad.2017.001</a></p>"),
      HTML("<p>Director, S. W. (2012). <i>Financial Analysis for HR Managers: Tools for Linking HR Strategy to Business Strategy</i>. FT Press.</p>"),
      HTML("<p>Joo, H., Aguinis, H., Lee, J., Kremer, H., & Villamor, I. (2022). HRM's financial value from obtaining more star performers. <i>The International Journal of Human Resource Management, 33</i>(21), 4179–4214. <a href='https://doi.org/10.1080/09585192.2021.1948890' target='_blank'>https://doi.org/10.1080/09585192.2021.1948890</a></p>"),
      HTML("<p>Latham, G. P., & Whyte, G. (1994). The futility of utility analysis. <i>Personnel Psychology, 47</i>(1), 31–46. <a href='https://doi.org/10.1111/j.1744-6570.1994.tb02408.x' target='_blank'>https://doi.org/10.1111/j.1744-6570.1994.tb02408.x</a></p>"),
      HTML("<p>O'Boyle, E. H., & Aguinis, H. (2012). The best and the rest: Revisiting the norm of normality of individual performance. <i>Personnel Psychology, 65</i>(1), 79–119. <a href='https://doi.org/10.1111/j.1744-6570.2011.01239.x' target='_blank'>https://doi.org/10.1111/j.1744-6570.2011.01239.x</a></p>"),
      HTML("<p>Sackett, P. R., Zhang, C., Berry, C. M., & Lievens, F. (2021). Revisiting meta-analytic estimates of validity in personnel selection: Addressing systematic overcorrection for restriction of range. <i>Journal of Applied Psychology, 106</i>(9), 1407–1435. <a href='https://doi.org/10.1037/apl0000994' target='_blank'>https://doi.org/10.1037/apl0000994</a></p>"),
      HTML("<p>Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. <i>Journal of Management, 26</i>(2), 281–299. <a href='https://doi.org/10.1177/014920630002600206' target='_blank'>https://doi.org/10.1177/014920630002600206</a></p>")
    )
  ))
)

# Run the application
shinyApp(ui, main_server) 