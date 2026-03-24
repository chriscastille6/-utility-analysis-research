# Simulation UA portfolio — joint view of multiple HR interventions over time.
# Complements domain-specific calculators linked from each UA app.

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x

sim_other_apps_base <- function() {
  Sys.getenv("UA_APPS_BASE_URL", unset = "https://bayoupal.nicholls.edu/ua")
}

sim_spec_url <- function() {
  paste0(
    Sys.getenv(
      "SIMULATION_UA_GITHUB_BASE",
      unset = "https://github.com/chriscastille6/-utility-analysis-research/blob/main"
    ),
    "/docs/SIMULATION_UA_LENS_SPEC.md"
  )
}

#' Illustrative rows only — not tied to a real sim key; replace with your numbers.
example_portfolio_df <- function() {
  data.frame(
    practice = c(
      "[Example] Structured interviews + validated work sample",
      "[Example] Supervisor coaching program (ongoing)",
      "[Example] Merit budget / pay mix change"
    ),
    domain = c("Staffing/selection", "Training", "Wages/pay"),
    start_quarter = c(1L, 2L, 4L),
    annual_utility = c(185000, 72000, 210000),
    cost = c(48000, 28000, 35000),
    cost_type = c("once", "recur", "once"),
    stringsAsFactors = FALSE
  )
}

empty_portfolio_df <- function() {
  data.frame(
    practice = character(),
    domain = character(),
    start_quarter = integer(),
    annual_utility = numeric(),
    cost = numeric(),
    cost_type = character(),
    stringsAsFactors = FALSE
  )
}

ui <- dashboardPage(
  dashboardHeader(
    title = span(
      "Simulation UA portfolio ",
      tags$span(class = "label label-warning", style = "font-size:12px;vertical-align:middle;", "Under development")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Portfolio builder", tabName = "portfolio", icon = icon("layer-group")),
      menuItem("Links to UA tools", tabName = "links", icon = icon("link"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "portfolio",
        fluidRow(
          box(
            width = 12,
            title = tagList(
              "Joint utility across decisions ",
              tags$span(class = "label label-warning", "Under development")
            ),
            status = "primary",
            solidHeader = TRUE,
            htmlOutput("from_context"),
            p(
              "Add rows for practices or investments you are considering in the simulation.",
              "Use domain calculators (training, staffing, pay, etc.) to estimate annualized utility and cost, then record them here to see combined impact and timing across ",
              strong("eight quarters"),
              " (extend later as needed)."
            ),
            tags$div(
              class = "callout",
              style = "background:#fff8e6;border-left:4px solid #f0ad4e;padding:12px 14px;margin:12px 0;border-radius:4px;",
              tags$strong("Template workflow (suggested)"),
              tags$ol(
                tags$li("Pick one decision area (e.g., staffing), run the matching UA calculator, copy defensible annual utility and cost assumptions."),
                tags$li("Enter a ", tags$strong("start quarter"), " when the practice begins affecting outcomes in your sim story."),
                tags$li(tags$strong("One-time cost"), " hits in the start quarter; ", tags$strong("recurring cost"), " spreads per active quarter (simplified)."),
                tags$li("Repeat for other practices, then read the chart as a ", tags$em("relative"), " comparison — not a substitute for your sim’s official scoring.")
              )
            ),
            p(
              tags$em("This page loads with ", tags$strong("illustrative"), " rows (prefixed ", tags$code("[Example]"), "). Clear them when you enter your own numbers.")
            ),
            p(
              a(href = sim_spec_url(), "Simulation UA Lens design spec", target = "_blank")
            )
          )
        ),
        fluidRow(
          box(
            width = 4,
            title = "Add line item",
            status = "info",
            solidHeader = TRUE,
            textInput("pr_name", "Practice / intervention label", value = ""),
            selectInput(
              "pr_domain",
              "Domain",
              choices = c(
                "Staffing/selection", "Training", "Wages/pay", "Benefits",
                "Other HR", "Cross-domain"
              )
            ),
            numericInput("pr_start_q", "Start quarter (1-8)", value = 1, min = 1, max = 8, step = 1),
            numericInput("pr_delta_u", "Annual utility ($)", value = 0, step = 1000),
            numericInput("pr_cost", "One-time or annual cost ($)", value = 0, step = 1000),
            selectInput("pr_cost_type", "Cost timing", choices = c("One-time (Q1 of item)" = "once", "Each active quarter" = "recur")),
            actionButton("pr_add", "Add to portfolio", class = "btn-primary"),
            br(), br(),
            actionButton("pr_load_examples", "Reload example rows", class = "btn-warning"),
            br(), br(),
            actionButton("pr_clear", "Clear all rows", class = "btn-default"),
            br(), br(),
            downloadButton("dl_csv", "Download current table (CSV)"),
            br(), br(),
            downloadButton("dl_template", "Download empty CSV template")
          ),
          box(
            width = 8,
            title = "Portfolio table",
            status = "success",
            solidHeader = TRUE,
            DTOutput("pr_table")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Net by quarter (simple roll-up)",
            status = "warning",
            solidHeader = TRUE,
            plotOutput("pr_chart", height = "260px")
          ),
          box(
            width = 6,
            title = "Totals (illustrative)",
            status = "warning",
            solidHeader = TRUE,
            tableOutput("pr_totals")
          )
        )
      ),
      tabItem(
        tabName = "links",
        fluidRow(
          box(
            width = 12,
            title = "Open domain calculators",
            status = "primary",
            solidHeader = TRUE,
            tags$ul(
              tags$li(a(href = paste0(sim_other_apps_base(), "/training/"), "Training UA", target = "_blank")),
              tags$li(a(href = paste0(sim_other_apps_base(), "/staffing/"), "Staffing UA", target = "_blank")),
              tags$li(a(href = paste0(sim_other_apps_base(), "/job-crafting/"), "Job crafting UA", target = "_blank")),
              tags$li(a(href = paste0(sim_other_apps_base(), "/fisher-2017/"), "Contingent workers (Fisher & Connelly 2017)", target = "_blank")),
              tags$li(a(href = paste0(sim_other_apps_base(), "/fisher-2020/"), "Workers with disabilities (Fisher & Connelly 2020)", target = "_blank")),
              tags$li(a(href = paste0(sim_other_apps_base(), "/sturman/"), "Performance-based pay (Sturman)", target = "_blank"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(df = example_portfolio_df())

  output$from_context <- renderUI({
    qs <- isolate(session$clientData$url_search)
    q <- if (is.null(qs) || !nzchar(qs)) list() else shiny::parseQueryString(qs)
    if (!is.null(q$from) && nzchar(q$from)) {
      tags$p(tags$strong("Arrived from: "), tags$code(q$from))
    } else {
      NULL
    }
  })

  observeEvent(input$pr_add, {
    if (!nzchar(trimws(input$pr_name))) {
      showNotification("Enter a practice label.", type = "warning")
      return()
    }
    rv$df <- bind_rows(
      rv$df,
      data.frame(
        practice = input$pr_name,
        domain = input$pr_domain,
        start_quarter = as.integer(input$pr_start_q),
        annual_utility = input$pr_delta_u,
        cost = input$pr_cost,
        cost_type = input$pr_cost_type,
        stringsAsFactors = FALSE
      )
    )
  })

  observeEvent(input$pr_load_examples, {
    rv$df <- example_portfolio_df()
    showNotification("Loaded illustrative example rows.", type = "message")
  })

  observeEvent(input$pr_clear, {
    rv$df <- empty_portfolio_df()
    showNotification("Table cleared.", type = "message")
  })

  output$pr_table <- renderDT({
    datatable(rv$df, options = list(pageLength = 8), rownames = FALSE)
  })

  quarter_rollups <- reactive({
    df <- rv$df
    if (nrow(df) == 0) {
      return(data.frame(quarter = 1:8, net = 0))
    }
    net <- numeric(8)
    for (i in seq_len(nrow(df))) {
      sq <- max(1L, min(8L, as.integer(df$start_quarter[i])))
      u <- as.numeric(df$annual_utility[i]) / 4
      cst <- as.numeric(df$cost[i])
      ct <- df$cost_type[i]
      for (q in sq:8) {
        net[q] <- net[q] + u
        if (ct == "recur") {
          net[q] <- net[q] - cst / 4
        }
      }
      if (ct == "once") {
        net[sq] <- net[sq] - cst
      }
    }
    data.frame(quarter = 1:8, net = net)
  })

  output$pr_chart <- renderPlot({
    d <- quarter_rollups()
    ggplot(d, aes(x = quarter, y = net)) +
      geom_col(fill = "#3c8dbc") +
      labs(x = "Quarter", y = "Net (utility − cost, illustrative)") +
      theme_minimal()
  })

  output$pr_totals <- renderTable({
    d <- quarter_rollups()
    data.frame(
      Metric = c("Cumulative net (8Q)", "Final-quarter net"),
      Value = c(sum(d$net), d$net[8])
    )
  }, digits = 0)

  output$dl_csv <- downloadHandler(
    filename = function() paste0("simulation_ua_portfolio_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(rv$df, file, row.names = FALSE)
    }
  )

  output$dl_template <- downloadHandler(
    filename = function() "simulation_ua_portfolio_template.csv",
    content = function(file) {
      utils::write.csv(empty_portfolio_df(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
