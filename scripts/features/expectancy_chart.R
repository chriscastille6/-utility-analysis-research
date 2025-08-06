# Load required libraries
library(shiny)
library(ggplot2)
library(mvtnorm)
library(shinyjs)

# Define the Expectancy function
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

# UI definition
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Expectancy Chart"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("rxy1", "Validity of old system:", 0.1, min = -1, max = 1, step = 0.01),
      numericInput("rxy2", "Validity of new system:", 0.5, min = -1, max = 1, step = 0.01),
      numericInput("sr", "Selection ratio (%):", 33, min = 0, max = 100, step = 1),
      div(style = "display: flex; gap: 10px;",
        actionButton("generate_chart", "Generate Chart"),
        actionButton("clear_chart", "Clear")
      )
    ),
    
    mainPanel(
      div(id = "explanatory_content",
        h4(HTML("<b>Understanding Validity</b>")),
        p(HTML("<b>Validity</b> is the correlation between selection test performance and actual job performance. It can range from -1.00 to 1.00, where:")),
        tags$ul(
          tags$li("-1.00 indicates a perfect negative relationship"),
          tags$li("0 indicates no relationship"),
          tags$li("1.00 indicates a perfect positive relationship")
        ),
        p(HTML("In practice, validity coefficients for selection procedures typically range from 0 to .50. For example, structured interviews have an estimated operational validity of .42 (<a href='https://doi.org/10.1037/apl0000994' target='_blank'>Sackett et al., 2021</a>).")),
        p("The chart generated will show how the new selection system compares to the old one in terms of predicting high job performance across different performance quartiles."),
        p("The default values provided are from Latham and Whyte (1994):"),
        tags$ul(
          tags$li("Old system validity: 0.10"),
          tags$li("New system validity: 0.50"),
          tags$li("Selection ratio: 33%")
        ),
        br()
      ),
      plotOutput("expectancy_plot"),
      textOutput("expectancy_text")
    )
  )
)

# Server definition
server <- function(input, output, session) {
  # Generate chart
  observeEvent(input$generate_chart, {
    # Hide explanatory content
    shinyjs::hide("explanatory_content")
    
    # Convert selection ratio to decimal
    sr_decimal <- input$sr / 100
    
    # Compute old top expectancy
    expectancyTopOld <- 100*round(Expectancyfunc(input$rxy1, qnorm(1-sr_decimal)+.67, Inf, input$rxy1*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    # Compute old upper middle expectancy
    expectancyUMOld <- 100*round(Expectancyfunc(input$rxy1, qnorm(1-sr_decimal)+0, qnorm(1-sr_decimal)+.67, input$rxy1*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    #Compute old lower middle expectancy
    expectancyLMOld <- 100*round(Expectancyfunc(input$rxy1, qnorm(1-sr_decimal)-.67, qnorm(1-sr_decimal)+0, input$rxy1*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    #compute old lower expectancy
    expectancyLowOld <- 100*round(Expectancyfunc(input$rxy1, -Inf, qnorm(1-sr_decimal)-.67, input$rxy1*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    # Compute new top expectancy
    expectancyTopNew <- 100*round(Expectancyfunc(input$rxy2, qnorm(1-sr_decimal)+.67, Inf, input$rxy2*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    # Compute new upper middle expectancy
    expectancyUMNew <- 100*round(Expectancyfunc(input$rxy2, qnorm(1-sr_decimal)+0, qnorm(1-sr_decimal)+.67, input$rxy2*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    #Compute new lower middle expectancy
    expectancyLMNew <- 100*round(Expectancyfunc(input$rxy2, qnorm(1-sr_decimal)-.67, qnorm(1-sr_decimal)+0, input$rxy2*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    #compute new lower expectancy
    expectancyLowNew <- 100*round(Expectancyfunc(input$rxy2, -Inf, qnorm(1-sr_decimal)-.67, input$rxy2*(qnorm(1-sr_decimal))+.67, Inf),2)
    
    #Calculate increase in good hires/ decrease in bad hires
    badHire <- round(((expectancyLowOld-expectancyLowNew)/expectancyLowOld)*100, 0)
    goodHire <- round(((expectancyTopNew-expectancyTopOld)/expectancyTopOld)*100, 0)
    
    # Create data frame for plotting
    bar_data <- data.frame(
      Quartile = rep(c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), each = 2),
      Procedure = rep(c("Old", "New"), times = 4),
      Probability = c(
        expectancyLowOld, expectancyLowNew,
        expectancyLMOld, expectancyLMNew,
        expectancyUMOld, expectancyUMNew,
        expectancyTopOld, expectancyTopNew
      )
    )
    
    # Set factor levels to ensure correct order
    bar_data$Procedure <- factor(bar_data$Procedure, levels = c("Old", "New"))
    bar_data$Quartile <- factor(bar_data$Quartile, 
                               levels = c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"),
                               ordered = TRUE)
    
    # Create the plot
    p <- ggplot(bar_data, aes(x = Quartile, y = Probability, fill = Procedure)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Expectancy Chart",
        subtitle = "Probability of High Job Performance by Selection System",
        x = "Quartile Score on Selection Procedure(s)/Program(s)",
        y = "Probability of High Job Performance",
        fill = "Procedure(s)/Program(s)"
      ) +
      scale_fill_manual(values = c("Old" = "#6BAED6", "New" = "#2171B5"), breaks = c("Old", "New")) +
      scale_x_discrete(limits = c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"), ordered = TRUE) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 12, lineheight = 1.2),
        plot.subtitle = element_text(hjust = 0, margin = margin(b = 10), size = 10),
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
      geom_text(aes(label = paste0(round(Probability, 1), "%")), vjust = -0.5, size = 6, position = position_dodge(width = 0.9))
    
    # Render the plot
    output$expectancy_plot <- renderPlot({
      p
    })
    
    # Update text output
    output$expectancy_text <- renderText({
      paste0("Using the new procedure improves the probability of acquiring a high performer by ", goodHire, "% and avoids bad hires by ", badHire, "%.")
    })
  })
  
  # Clear chart
  observeEvent(input$clear_chart, {
    output$expectancy_plot <- renderPlot(NULL)
    output$expectancy_text <- renderText("")
    shinyjs::show("explanatory_content")
  })
}

# Run the application
shinyApp(ui = ui, server = server) 