# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggforce)  # For geom_circle in Venn diagram
library(rmarkdown)
library(tinytex)
library(patchwork)  # For combining plots

# Helper functions
Expectancyfunc <- function(r, PredLowerCut, PredUpperCut, CritLowerCut, CritUpperCut) {
  # Calculate the probability of being in the criterion range given the predictor range
  # using the bivariate normal distribution
  
  # Define the bivariate normal density function
  bivariate_normal <- function(x, y, r) {
    z <- (x^2 + y^2 - 2*r*x*y) / (2*(1-r^2))
    exp(-z) / (2*pi*sqrt(1-r^2))
  }
  
  # Define the integration limits
  x_limits <- c(PredLowerCut, PredUpperCut)
  y_limits <- c(CritLowerCut, CritUpperCut)
  
  # Perform numerical integration
  integrate2d <- function(f, x_limits, y_limits, r) {
    # Use a simple numerical integration method
    n <- 100  # Number of points for integration
    
    # Handle infinite bounds by using reasonable finite limits
    x_min <- if(is.infinite(x_limits[1])) -6 else x_limits[1]
    x_max <- if(is.infinite(x_limits[2])) 6 else x_limits[2]
    y_min <- if(is.infinite(y_limits[1])) -6 else y_limits[1]
    y_max <- if(is.infinite(y_limits[2])) 6 else y_limits[2]
    
    x <- seq(x_min, x_max, length.out = n)
    y <- seq(y_min, y_max, length.out = n)
    
    # Create a grid of points
    grid <- expand.grid(x = x, y = y)
    
    # Calculate the function values at each point
    z <- mapply(function(x, y) f(x, y, r), grid$x, grid$y)
    
    # Calculate the area of each grid cell
    dx <- (x_max - x_min) / (n-1)
    dy <- (y_max - y_min) / (n-1)
    
    # Sum up the volumes
    sum(z) * dx * dy
  }
  
  # Calculate the probability
  prob <- integrate2d(bivariate_normal, x_limits, y_limits, r)
  
  # Calculate the marginal probability of being in the predictor range
  marginal_prob <- pnorm(PredUpperCut) - pnorm(PredLowerCut)
  
  # Return the conditional probability
  prob / marginal_prob
}

# Create CSS file for styling
writeLines('
body {
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
  line-height: 1.6;
  color: #333;
}

h1, h2, h3, h4 {
  color: #2c3e50;
  margin-top: 1.5em;
}

.nav-tabs {
  margin-bottom: 20px;
}

.tab-content {
  padding: 20px;
  border: 1px solid #ddd;
  border-top: none;
  border-radius: 0 0 4px 4px;
}

.plot {
  margin: 20px 0;
  border: 1px solid #eee;
  border-radius: 4px;
  padding: 10px;
}

.reference {
  margin-top: 40px;
  padding-top: 20px;
  border-top: 1px solid #eee;
  font-size: 0.9em;
  color: #666;
}
', "styles.css")

# Helper functions
generate_correlated_data <- function(r, n = 1000) {
  # Handle perfect correlations
  if (abs(r) == 1) {
    x <- rnorm(n)
    y <- r * x  # For r = 1, y = x; for r = -1, y = -x
    return(data.frame(x = x, y = y))
  }
  
  # For all correlations, use a consistent approach
  x <- rnorm(n)
  # Scale the noise based on the correlation
  noise_scale <- sqrt(1 - r^2)
  y <- r * x + rnorm(n, sd = noise_scale)
  
  # Convert to data frame
  data.frame(x = x, y = y)
}

create_venn <- function(r) {
  # Calculate shared variance
  shared_var <- r^2 * 100
  
  # Calculate circle positions and sizes based on correlation
  # When r = 1, circles should completely overlap
  # When r = 0, circles should not overlap
  # When r = -1, circles should be completely separate
  circle_distance <- 2 * (1 - abs(r))  # Distance between circle centers
  
  # Create Venn diagram data
  venn_data <- data.frame(
    x = c(-circle_distance/2, circle_distance/2),  # Position circles based on correlation
    y = c(0, 0),
    r = c(1, 1),
    label = c("X", "Y"),
    fill = c("#6BAED6", "#FF9999")  # Different colors for each circle
  )
  
  # Create plot
  venn <- ggplot(venn_data, aes(x0 = x, y0 = y, r = r, label = label, fill = fill)) +
    geom_circle(alpha = 0.3) +
    geom_text(aes(x = x, y = y + 1.2), size = 5) +
    annotate("text", x = 0, y = 0, 
             label = sprintf("%.1f%%", shared_var),
             size = 6) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14)) +
    labs(title = "Shared Variance") +
    scale_fill_identity()  # Use the fill colors directly
  
  return(venn)
}

# UI
ui <- fluidPage(
  titlePanel("The Correlator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rxy1", "Set the Correlation Coefficient Yourself:", 0.44, min = -1, max = 1, step = 0.01),
      textInput("x_var", "X Variable Title:", "Attendance"),
      textInput("y_var", "Y Variable Title:", "Grades"),
      hr(),
      h4("Scatter Plot Options"),
      checkboxInput("show_regression", "Show Regression Line", value = TRUE),
      checkboxInput("show_residuals", "Show Residuals", value = FALSE),
      checkboxInput("show_ellipses", "Show Probability Ellipses", value = FALSE),
      checkboxInput("show_density", "Show Density Plots", value = FALSE),
      actionButton("generate", "Generate Chart"),
      downloadButton("download_pdf", "Export Report"),
      hr(),
      h4("Guess the Effect Size"),
      p("Based on Lakens (2024) and Meyer et al. (2001)"),
      actionButton("play_meyer", "New Correlation"),
      numericInput("guess_meyer", "Your Guess for r:", value = 0, min = -1, max = 1, step = 0.01),
      div(style = "display: flex; gap: 10px;",
        actionButton("submit_meyer", "Check Guess"),
        actionButton("clear_game", "Clear Game")
      ),
      verbatimTextOutput("meyer_feedback"),
      br(),
      textOutput("meyer_context")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Basic Visuals", 
                 fluidRow(
                   column(6, plotlyOutput("scatter_plot")),
                   column(6, plotOutput("venn_diagram"))
                 ),
                 fluidRow(
                   column(12, 
                          div(style = "text-align: center; margin-top: 20px;",
                              h4("Correlation Formula"),
                              p("r = ", textOutput("correlation_formula", inline = TRUE)),
                              p("Shared Variance = ", textOutput("shared_variance", inline = TRUE), "%")
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          div(style = "text-align: center; margin-top: 20px;",
                              h4("Guess History"),
                              plotOutput("guess_history")
                          )
                   )
                 )
        ),
        tabPanel("Expectancy Chart", plotOutput("expectancy_plot")),
        tabPanel("References",
          br(),
          tags$div(class = "reference",
            h4("Sources"),
            tags$div(
              tags$p(
                "Credé, M., Roch, S. G., & Kieszczynka, U. M. (2010). Class attendance in college: A meta-analytic review of the relationship of class attendance with grades and student characteristics. ", 
                tags$i("Review of Educational Research"), 
                ", 80(2), 272–295. https://doi.org/10.3102/0034654310362998"
              ),
              tags$p(
                "Lakens, D. (2025). Guess the correlation [Shiny app]. LinkedIn. Retrieved May 20, 2025, from https://lnkd.in/eRTbeWdu"
              ),
              tags$p(
                "Lakens, D. (2025). Guess the correlation data-collecting version [Shiny app]. Eindhoven University of Technology. Retrieved from https://shiny.ieis.tue.nl/guesscorrelation/"
              ),
              tags$p(
                "Magnusson, K. (2023). Interpreting Correlations: An interactive visualization (Version 0.7.1) [Web App]. R Psychologist. Retrieved from https://rpsychologist.com/correlation/"
              ),
              tags$p(
                "Meyer, G. J., Finn, S. E., Eyde, L. D., Kay, G. G., Moreland, K. L., Dies, R. R., Eisman, E. J., Kubiszyn, T. W., & Reed, G. M. (2001). Psychological Testing and Psychological Assessment. ", 
                tags$i("American Psychologist"), 
                ", 128–165."
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Meyer correlations and their descriptions
  meyer_df <- data.frame(
    r = c(
      0.00, 0.02, 0.03, 0.03, 0.04, 0.05, 0.06, 0.07,
      0.11, 0.17, 0.17, 0.20, 0.25, 0.26, 0.27, 0.30,
      0.34, 0.38, 0.40, 0.55, 0.60, 0.67
    ),
    description = c(
      "effect of sugar consumption on behavior and cognitive processes in children",
      "aspirin and reduced risk of death by heart attack",
      "antihypertensive medication and reduced risk of stroke",
      "chemotherapy and breast cancer survival",
      "post-MI cardiac rehabilitation and reduced cardiovascular deaths",
      "alendronate and reduction in postmenopausal fractures",
      "MLB batting skill and hit success on a given at-bat",
      "aspirin + heparin vs. aspirin alone for unstable angina (MI or death)",
      "antihistamine use and reduced sneezing/runny nose",
      "extent of brain‐tissue destruction and impaired learning in monkeys",
      "prominent movie critics' reviews and box‐office success",
      "employment interviews and prediction of job success",
      "Viagra and its unwanted side-effects",
      "gender and weight for U.S. adults",
      "psychological therapy under real-world conditions",
      "sleeping pills and short-term insomnia improvement",
      "elevation above sea level and U.S. daily temperature",
      "Viagra and improved male sexual functioning",
      "contiguous natural environments and center‐habitat species density",
      "gender and arm strength",
      "latitude and U.S. daily temperature",
      "gender and height for U.S. adults"
    ),
    stringsAsFactors = FALSE
  )
  
  # Store the current index
  current_index <- reactiveVal(NULL)
  
  # Store the current data
  current_data <- reactiveVal(NULL)
  
  # Store guess history
  guess_history <- reactiveVal(data.frame(
    trial = integer(),
    guess = numeric(),
    true_r = numeric(),
    error = numeric()
  ))
  
  # When the user clicks "New Meyer Correlation"
  observeEvent(input$play_meyer, {
    current_index(sample(nrow(meyer_df), 1))
    # Generate new data
    current_data(generate_correlated_data(meyer_df$r[current_index()], n = 1000))
    
    # Clear the variable labels
    updateTextInput(session, "x_var", value = "")
    updateTextInput(session, "y_var", value = "")
    
    # Clear the context and feedback when generating new correlation
    output$meyer_context <- renderText({ "" })
    output$meyer_feedback <- renderText({ "" })
  })
  
  # When the user clicks "Clear Game"
  observeEvent(input$clear_game, {
    # Reset current index
    current_index(NULL)
    
    # Clear guess history
    guess_history(data.frame(
      trial = integer(),
      guess = numeric(),
      true_r = numeric(),
      error = numeric()
    ))
    
    # Reset variable labels to default
    updateTextInput(session, "x_var", value = "Attendance")
    updateTextInput(session, "y_var", value = "Grades")
    
    # Generate default data
    current_data(generate_correlated_data(input$rxy1, n = 1000))
    
    # Clear feedback and context
    output$meyer_feedback <- renderText({ "" })
    output$meyer_context <- renderText({ "" })
  })
  
  # When they submit a guess, compute and show feedback
  observeEvent(input$submit_meyer, {
    req(current_index())
    guess <- input$guess_meyer
    truth <- meyer_df$r[current_index()]
    error <- guess - truth  # Changed to signed error
    
    # Extract variables from description
    description <- meyer_df$description[current_index()]
    # Split on " and " to get the two variables
    vars <- strsplit(description, " and ")[[1]]
    # Clean up the variables (remove any leading/trailing whitespace)
    if (length(vars) == 2) {
      x_var <- trimws(vars[1])
      y_var <- trimws(vars[2])
    } else {
      # If we can't split on " and ", use the whole description as x_var
      # and "Outcome" as y_var
      x_var <- description
      y_var <- "Outcome"
    }
    
    # Update the input fields with the current variables
    updateTextInput(session, "x_var", value = x_var)
    updateTextInput(session, "y_var", value = y_var)
    
    # Update guess history
    new_guess <- data.frame(
      trial = nrow(guess_history()) + 1,
      guess = guess,
      true_r = truth,
      error = error
    )
    guess_history(rbind(guess_history(), new_guess))
    
    # Show feedback
    output$meyer_feedback <- renderText({
      sprintf(
        "True r = %.2f\nYour guess = %.2f\nAbsolute error = %.2f",
        truth, guess, abs(error)
      )
    })
    
    # Show the context after the guess
    output$meyer_context <- renderText({
      paste0(
        "This scatterplot shows the relationship between ",
        meyer_df$description[current_index()],
        " (true r = ", sprintf("%.2f", meyer_df$r[current_index()]), "). ",
        "Correlations taken from Meyer et al. (2001)."
      )
    })
  })
  
  # Initialize default data
  observe({
    if (is.null(current_data())) {
      current_data(generate_correlated_data(input$rxy1, n = 1000))
    }
  })
  
  # Render the scatter plot
  output$scatter_plot <- renderPlotly({
    req(current_data())
    df <- current_data()
    
    # Create base plot
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(alpha = 0.6, color = "#6BAED6") +
      labs(
        x = input$x_var,
        y = input$y_var,
        title = if (!is.null(current_index())) {
          if (nrow(guess_history()) > 0) {
            paste0("Guess the Correlation (r = ", sprintf("%.2f", meyer_df$r[current_index()]), ")")
          } else {
            "Guess the Correlation"
          }
        } else {
          if(input$x_var == "Attendance" && input$y_var == "Grades") {
            "Correlation Scatter Plot (Credé et al., 2010)"
          } else {
            "Correlation Scatter Plot"
          }
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)
      )
    
    # Add regression line if selected
    if (input$show_regression) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#FF9999")
    }
    
    # Add residuals if selected
    if (input$show_residuals) {
      model <- lm(y ~ x, data = df)
      df <- df %>%
        mutate(predicted = predict(model))
      p <- p + geom_segment(
        data = df,
        aes(x = x, y = y, xend = x, yend = predicted),
        color = "red",
        alpha = 0.3,
        linetype = "dashed"
      )
    }
    
    # Add probability ellipses if selected
    if (input$show_ellipses) {
      p <- p + stat_ellipse(
        type = "norm",
        level = 0.95,
        color = "darkgreen",
        linetype = "dashed"
      )
    }
    
    # Add density plots if selected
    if (input$show_density) {
      # Calculate density for x and y
      x_density <- density(df$x)
      y_density <- density(df$y)
      
      # Scale the density plots to fit nicely in the margins
      x_scale <- 0.3 * diff(range(df$y))
      y_scale <- 0.3 * diff(range(df$x))
      
      # Add x-axis density at the top
      p <- p + geom_line(
        data = data.frame(x = x_density$x, y = max(df$y) - x_density$y * x_scale),
        aes(x = x, y = y),
        color = "#6BAED6",
        alpha = 0.5
      )
      
      # Add y-axis density on the right
      p <- p + geom_line(
        data = data.frame(x = max(df$x) - y_density$y * y_scale, y = y_density$x),
        aes(x = x, y = y),
        color = "#6BAED6",
        alpha = 0.5
      )
    }
    
    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Generate and display Venn diagram
  output$venn_diagram <- renderPlot({
    # Use the current correlation from either the game or manual input
    current_r <- if (!is.null(current_index())) {
      meyer_df$r[current_index()]
    } else {
      input$rxy1
    }
    create_venn(current_r)
  })
  
  # Display correlation formula and shared variance
  output$correlation_formula <- renderText({
    # Use the current correlation from either the game or manual input
    current_r <- if (!is.null(current_index())) {
      meyer_df$r[current_index()]
    } else {
      input$rxy1
    }
    sprintf("%.2f", current_r)
  })
  
  output$shared_variance <- renderText({
    # Use the current correlation from either the game or manual input
    current_r <- if (!is.null(current_index())) {
      meyer_df$r[current_index()]
    } else {
      input$rxy1
    }
    sprintf("%.1f", current_r^2 * 100)
  })
  
  # Render guess history plot
  output$guess_history <- renderPlot({
    req(nrow(guess_history()) > 0)
    
    ggplot(guess_history(), aes(x = trial, y = error)) +
      geom_line(color = "#6BAED6", size = 1) +
      geom_point(color = "#2171B5", size = 3) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +
      labs(
        x = "Trial Number",
        y = "Guess - True Correlation",
        title = "Guess Accuracy Over Time"
      ) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90")
      )
  })
  
  # Reactive values to store expectancy results
  expectancy_results <- reactiveVal()
  
  # Reactive data generation
  scatter_data <- reactive({
    generate_correlated_data(input$rxy1)
  })
  
  # Calculate regression statistics
  regression_stats <- reactive({
    data <- scatter_data()
    model <- lm(y ~ x, data = data)
    coef <- coef(model)
    means <- colMeans(data)
    sds <- apply(data, 2, sd)
    
    list(
      intercept = coef[1],
      slope = coef[2],
      mean_x = means["x"],
      mean_y = means["y"],
      sd_x = sds["x"],
      sd_y = sds["y"]
    )
  })
  
  # Output regression statistics
  output$regression_equation <- renderText({
    stats <- regression_stats()
    sprintf("y = %.2f + %.2fx", stats$intercept, stats$slope)
  })

  output$mean_y <- renderText({
    sprintf("%.2f", regression_stats()$mean_y)
  })
  
  output$mean_x <- renderText({
    sprintf("%.2f", regression_stats()$mean_x)
  })
  
  output$sd_y <- renderText({
    sprintf("%.2f", regression_stats()$sd_y)
  })
  
  output$sd_x <- renderText({
    sprintf("%.2f", regression_stats()$sd_x)
  })
  
  # Generate chart
  observeEvent(input$generate, {
    # Calculate expectancies
    r_target <- input$rxy1
    print("Correlation coefficient from input:")
    print(r_target)
    
    quartile_zs <- c(-Inf, qnorm(c(0.25, 0.5, 0.75)), Inf)
    crit_cut <- qnorm(0.75)
    
    expectancies <- sapply(1:4, function(i) {
      Expectancyfunc(r_target,
                     PredLowerCut = quartile_zs[i],
                     PredUpperCut = quartile_zs[i+1],
                     CritLowerCut = crit_cut,
                     CritUpperCut = Inf)
    })
    
    # Store expectancies in correct order
    expectancyLowOld <- 100 * expectancies[1]  # Bottom 25%
    expectancyLMOld <- 100 * expectancies[2]   # Lower Middle 25%
    expectancyUMOld <- 100 * expectancies[3]   # Upper Middle 25%
    expectancyTopOld <- 100 * expectancies[4]  # Top 25%
    
    # Create plot data with ordered quartiles
    bar_data <- data.frame(
      Quartile = factor(
        c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%"),
        levels = c("Bottom 25%", "Lower Middle 25%", "Upper Middle 25%", "Top 25%")
      ),
      Probability = c(
        expectancyLowOld,  # Bottom 25%
        expectancyLMOld,   # Lower Middle 25%
        expectancyUMOld,   # Upper Middle 25%
        expectancyTopOld   # Top 25%
      )
    )
    
    # Create the app version with large text
    p_app <- ggplot(bar_data, aes(x = Quartile, y = Probability)) +
      geom_bar(stat = "identity", fill = "#6BAED6") +
      geom_text(aes(label = sprintf("%.1f%%", Probability)), 
                position = position_stack(vjust = 0.5),
                color = "white", size = 6) +
      labs(
        title = "Expectancy Chart",
        subtitle = paste("Probability of High", input$y_var, "by", input$x_var, "Quartiles"),
        x = paste(input$x_var, "Quartiles"), 
        y = paste("Probability of High", input$y_var)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        axis.line = element_line(color = "black", linewidth = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 100))
    
    # Create PDF version with smaller text
    p_pdf <- ggplot(bar_data, aes(x = Quartile, y = Probability)) +
      geom_bar(stat = "identity", fill = "#6BAED6") +
      geom_text(aes(label = sprintf("%.1f%%", Probability)), 
                position = position_stack(vjust = 0.5),
                color = "white", size = 3.5) +
      labs(
        title = "Expectancy Chart",
        subtitle = paste("Probability of High", input$y_var, "by", input$x_var, "Quartiles"),
        x = paste(input$x_var, "Quartiles"), 
        y = paste("Probability of High", input$y_var)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, size = 10),
        plot.subtitle = element_text(hjust = 0, size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        axis.line = element_line(color = "black", linewidth = 0.5)
      ) +
      scale_y_continuous(limits = c(0, 100))
    
    # Save the PDF version
    ggsave("expectancy_chart.png", p_pdf, width = 5, height = 3.5, dpi = 300)
    
    # Display the app version
    output$expectancy_plot <- renderPlot(p_app, width = 600, height = 400)
    
    # Store results
    results <- list(
      r_value = as.numeric(input$rxy1),  # Ensure it's numeric
      expectancyLowOld = expectancyLowOld,  # Bottom 25%
      expectancyLMOld = expectancyLMOld,    # Lower Middle 25%
      expectancyUMOld = expectancyUMOld,    # Upper Middle 25%
      expectancyTopOld = expectancyTopOld,  # Top 25%
      x_var = input$x_var,
      y_var = input$y_var,
      # Add regression statistics
      intercept = regression_stats()$intercept,
      slope = regression_stats()$slope,
      mean_x = regression_stats()$mean_x,
      mean_y = regression_stats()$mean_y,
      sd_x = regression_stats()$sd_x,
      sd_y = regression_stats()$sd_y
    )
    
    print("Results being passed to template:")
    print(results$r_value)
    
    expectancy_results(results)
  })
  
  # Download PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("correlation_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      tryCatch({
        # Create a temporary directory
        temp_dir <- tempdir()
        temp_file <- file.path(temp_dir, "temp_report.pdf")
        
        # Check if tinytex is installed
        if (!requireNamespace("tinytex", quietly = TRUE)) {
          stop("tinytex package is required for PDF generation. Please install it using: install.packages('tinytex')")
        }
        
        # Check if tinytex is properly installed
        if (!tinytex::is_tinytex()) {
          stop("TinyTeX is not installed. Please run: tinytex::install_tinytex()")
        }
        
        # Get the current results
        results <- expectancy_results()
        if (is.null(results)) {
          stop("Please generate the chart first by clicking the 'Generate Chart' button")
        }
        
        # Print debug information
        print("Correlation coefficient being passed:")
        print(input$rxy1)
        
        # Render the R Markdown template to the temporary file
        result <- rmarkdown::render(
          "utility_report.Rmd",
          output_file = temp_file,
          output_format = "pdf_document",
          params = list(
            r_value = as.numeric(input$rxy1),  # Pass correlation coefficient directly
            expectancyLowOld = results$expectancyLowOld,
            expectancyLMOld = results$expectancyLMOld,
            expectancyUMOld = results$expectancyUMOld,
            expectancyTopOld = results$expectancyTopOld,
            x_var = results$x_var,
            y_var = results$y_var,
            intercept = results$intercept,
            slope = results$slope,
            mean_x = results$mean_x,
            mean_y = results$mean_y,
            sd_x = results$sd_x,
            sd_y = results$sd_y
          ),
          envir = new.env(parent = globalenv()),
          quiet = FALSE  # Show compilation messages
        )
        
        # Copy the temporary file to the requested file
        if (file.exists(temp_file)) {
          file.copy(temp_file, file, overwrite = TRUE)
        } else {
          stop("PDF file was not created. Check the compilation messages above.")
        }
      }, error = function(e) {
        # Show detailed error message
        showNotification(
          paste("Error generating PDF:", e$message, "\nPlease ensure you have LaTeX installed."),
          type = "error",
          duration = NULL
        )
        # Print the full error for debugging
        print(e)
        # Return NULL to indicate failure
        return(NULL)
      })
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
