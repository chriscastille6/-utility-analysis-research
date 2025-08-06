# expectancy_chart_module.R

# UI function for the expectancy chart module
expectancyChartUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    titlePanel("Advanced Expectancy Chart"),
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("rxy"), "Observed correlation:", 0.3, min = -1, max = 1, step = 0.01),
        checkboxInput(ns("apply_corrections"), "Apply corrections", value = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("apply_corrections")),
          h4("Reliability Estimates"),
          numericInput(ns("rxx"), "Predictor reliability (rxx):", 0.8, min = 0, max = 1, step = 0.01),
          numericInput(ns("ryy"), "Criterion reliability (ryy):", 0.8, min = 0, max = 1, step = 0.01),
          numericInput(ns("u"), "Range restriction ratio (u):", 0.8, min = 0.01, max = 1, step = 0.01),
          helpText("u = SD of restricted group / SD of unrestricted group")
        ),
        numericInput(ns("sr"), "Selection ratio (%):", 33, min = 0, max = 100, step = 1),
        numericInput(ns("n"), "Sample size (n):", 100, min = 1, step = 1),
        h4("Plot Options"),
        checkboxInput(ns("show_error_bars"), "Show error bars", value = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("apply_corrections")),
          checkboxInput(ns("show_corrected"), "Show correlation corrected for criterion unreliability", value = FALSE),
          checkboxInput(ns("show_fully_corrected"), "Show correlation corrected for criterion unreliability and range restriction", value = FALSE)
        ),
        div(style = "display: flex; gap: 10px;",
            actionButton(ns("generate_chart"), "Generate Chart"),
            actionButton(ns("clear_chart"), "Clear")
        )
      ),
      mainPanel(
        uiOutput(ns("input_warnings")),
        div(id = ns("explanatory_content"),
            h4(HTML("<b>Understanding the Expectancy Chart</b>")),
            p("This tool allows you to:"),
            tags$ul(
              tags$li("Input the observed correlation between selection test performance and job performance"),
              tags$li("Optionally apply corrections for measurement error and range restriction"),
              tags$li("Compare corrected and uncorrected correlations"),
              tags$li("View error bars to understand the uncertainty in the estimates")
            ),
            p("The chart shows the probability of high job performance across different performance quartiles."),
            br()
        ),
        plotOutput(ns("expectancy_plot"), height = "600px", width = "800px"),
        uiOutput(ns("expectancy_text"))
      )
    )
  )
}

# Server function for the expectancy chart module
expectancyChartServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # (Insert all server logic here, replacing input$... and output$... with input$... and output$...)
    # Use Expectancyfunc and correct_correlation from the global environment
    # ... (copy over the server logic from expectancy_chart_advanced.R, using ns for IDs)
  })
} 