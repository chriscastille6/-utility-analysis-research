#Load Libraries
library(shiny)
library(iopsych)
library(scales)
library(shinyjs)
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(stringr)

#Define Custom Functions
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

# Define the UI for the opening webpage
opening_ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Opening Page"),
  mainPanel(
    h4("Introduction"),
    p("This is the opening page of the application. You can navigate using the above tabs.")
  )
)

#Define Reference UI
reference_ui <- fluidPage(
  titlePanel("References")
)

#Define Reference UI
glossary_ui <- fluidPage(
  titlePanel("Glossary")
)

# Define the App UI
main_ui <- fluidPage(
  useShinyjs(),
  
  # App title
  titlePanel("Predicting Returns on Improved Staffing Procedures"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of hires:", 100, min = 1, step = 1),
      numericInput("sdy", "SD of performance in monetary units:", 6000, min = 0.01, step = 1),
      numericInput("rxy1", "Validity of old procedure:", 0.1, min = -1, max = 1, step = 0.01),
      numericInput("rxy2", "Validity of new procedure:", 0.3, min = -1, max = 1, step = 0.01),
      numericInput("sr", "Selection ratio:", 0.25, min = 0, max = 1, step = 0.01),
      numericInput("cost1", "Cost per applicant of old procedure:", 100, min = 0.01, step = 1),
      numericInput("cost2", "Cost per applicant of new procedure:", 200, min = 0.01, step = 1),
      numericInput("period", "Anticipated tenure of selected employees:", 3, min = 0.01, step = 1),
      
      # Button
      actionButton("go", "Compute Utility")
    ),
    
    # Display the results
    mainPanel(
      textOutput("total_utility"),
      textOutput("per_hire_utility"),
      textOutput("per_year_utility"),
      textOutput("break_even_SDy"),
      
      # Add additional space between text outputs and bar graph
      br(),  # Add a one-line gap
      
      # Bar graph
      plotOutput("utility_graph"),
      
      #Download Button
      downloadButton("download_plot", "Download Chart")
    )
  )
)


# Define the server logic
main_server <- function(input, output) {
  observe({ 
    #Only allow calculations for certain parameters
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
        input$cost1 > 0 &
        input$cost2 >= 0 &
        input$period > 0
    )
    if (!isTruthy(input$go)) {
      disable("download_plot")
    }
  })
  
  
  observeEvent(input$go, {
    
    enable("download_plot")
    
    # Compute the utility
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
    
    # Calculate the per-hire utility
    per_hire_utility <- utility_value / input$n
    
    # Calculate the per-hire per-year utility
    per_year_utility <- per_hire_utility / input$period
    
    # Format the total utility value as a dollar amount
    formatted_total_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(utility_value, 2))
    
    # Format the per-hire utility value as a dollar amount
    formatted_per_hire_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(per_hire_utility, 2))
    
    # Format the per-hire per-year utility value as a dollar amount
    formatted_per_year_utility <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(per_year_utility, 2))
    
    # Format the break even value as a dollar amount
    formatted_breakEvenSDy <- label_dollar(scale = .001, prefix= "~$", suffix = "K")(signif(breakEvenSDy, 2))
    
    # Display the formatted total utility value
    output$total_utility <- renderText({
      paste("The total predicted returns (unadjusted) on improved staffing are:", 
            formatted_total_utility)
    })
    
    # Display the formatted per-hire utility value
    output$per_hire_utility <- renderText({
      paste("The predicted returns (unadjusted) on improved staffing per hire are:", 
            formatted_per_hire_utility)
    })
    
    # Display the formatted per-hire per-year utility value
    output$per_year_utility <- renderText({
      paste("The predicted returns (unadjusted) on improved staffing per hire per year are:", 
            formatted_per_year_utility)
    })
    
    # Display the formatted break even value
    output$break_even_SDy <- renderText({
      paste("The break-even value for Standard Deviation of the new procedure is:", 
            formatted_breakEvenSDy)
    })
    
    # Create an expectancy chart
    
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
      
      # Wrap the axis labels using str_wrap
      bar_data$Quartile <- str_wrap(bar_data$Quartile, width = 10)  # Adjust the width as needed
      
      # Determine the maximum y-value you want to display (1.2 times the maximum of both sets)
      max_y_value <- 1.2 * max(max(bar_data$Probability))
      
      # Create a ggplot object
      p <- ggplot(bar_data, aes(x = reorder(Quartile, Probability), y = Probability, fill = Procedure)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          x = "", 
          y = "Probability of High Job Performance",
          fill = "Procedure"
        ) +
        ylim(0, max_y_value) +  # Set the y-axis limits
        scale_fill_manual(values = c("Old" = "#FF9999", "New" = "#9999FF")) +  # Define lighter colors
        theme_minimal() +  # Customize the plot theme (optional)
        theme(
          plot.title = element_text(hjust = 0, margin = margin(b = 10), size = 16),  # Align title to the left and add margin
          axis.text = element_text(size = 14),  # Increase the font size of axis labels (adjust the size as needed)
          axis.title.y = element_text(size = 14),  # Adjust y-axis title size
          axis.text.x = element_text(angle = 0, hjust = .5)  # Rotate x-axis labels for better readability
        ) +
        ggtitle("Figure 1\nExpectancy of High Job Performance")  # Set the title
      
      # Add data labels centered above the bars
      return(p + geom_text(aes(label = paste0(Probability, "%")), vjust = -0.5, size = 4, position = position_dodge(width = 0.9)))
    })
    
    output$utility_graph <- renderPlot({
      plot_data()
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        "bar_plot.jpg"  # Set the filename for the downloaded file
      },
      
      content = function(file) {
        # Save the ggplot2 plot as an image file (jpg)
        ggsave(file, plot = plot_data(), device = "jpg", width = 8)
      },
      
      contentType = ("image/jpg")
    )
    
  })
  
}


# Create the opening page and main app within a navbarPage
ui <- navbarPage(
  "App Navigation",
  tabPanel("Opening Page", opening_ui),
  tabPanel("Staffing Utility", main_ui),
  tabPanel("Glossary", glossary_ui),
  tabPanel("References", reference_ui)
)
  
# Create a Shiny app
shinyApp(ui, main_server)