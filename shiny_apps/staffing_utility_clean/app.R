# Comprehensive Staffing Utility Analysis App
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Staffing Utility Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Status", tabName = "status", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "status",
        fluidRow(
          box(width = 12, title = "Application Status", status = "warning", solidHeader = TRUE,
            h4("Under Maintenance"),
            p("The staffing utility analysis tool is being updated for improved compatibility."),
            p("Please check back soon for full functionality.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
