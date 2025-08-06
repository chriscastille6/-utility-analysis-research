library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Test Correlation Slider"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("correlation",
                  "Correlation (ρ):",
                  min = -1,
                  max = 1,
                  value = 0.3,
                  step = 0.01),
      
      numericInput("line_slope",
                   "Slope (m):",
                   value = 0.3,
                   min = -2,
                   max = 2,
                   step = 0.01),
      
      checkboxInput("show_best_fit",
                    "Show Your Line",
                    value = FALSE)
    ),
    
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Generate data
  plot_data <- reactive({
    set.seed(42)
    n <- 100
    z1 <- rnorm(n, 0, 1)
    z2 <- rnorm(n, 0, 1)
    
    rho <- input$correlation
    x <- z1
    y <- rho * z1 + sqrt(1 - rho^2) * z2
    
    data.frame(x = x, y = y)
  })
  
  output$scatter_plot <- renderPlotly({
    data <- plot_data()
    
    p <- plot_ly(
      data = data,
      x = ~x, 
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 6, color = 'steelblue', opacity = 0.7)
    ) %>%
      layout(
        title = paste0("Correlation = ", round(input$correlation, 2)),
        xaxis = list(title = "X", range = c(-3, 3)),
        yaxis = list(title = "Y", range = c(-3, 3))
      )
    
    # Add line if checkbox is checked
    if (input$show_best_fit) {
      x_range <- seq(-3, 3, length.out = 100)
      y_line <- input$line_slope * x_range
      
      p <- p %>% add_trace(
        x = x_range,
        y = y_line,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'red', width = 3),
        name = paste0('y = ', input$line_slope, 'x')
      )
    }
    
    p
  })
}

shinyApp(ui = ui, server = server) 