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

# Function to calculate corrected correlation
correct_correlation <- function(r, rxx, ryy, correction_type, u = NULL) {
  # First correct for criterion unreliability only
  r_criterion_corrected <- r / sqrt(ryy)
  
  # Then apply range restriction correction if requested
  if (correction_type == "range_restriction") {
    if (is.null(u)) stop("Range restriction ratio (u) is required for this correction")
    r_fully_corrected <- r_criterion_corrected * sqrt(1 + (u^2 - 1) * r_criterion_corrected^2) / u
    return(list(criterion_corrected = r_criterion_corrected,
                fully_corrected = r_fully_corrected))
  }
  
  return(list(criterion_corrected = r_criterion_corrected,
              fully_corrected = r_criterion_corrected))
}

# UI definition
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Advanced Expectancy Chart"),
  
  sidebarLayout(
    sidebarPanel(
      # Correlation inputs
      numericInput("rxy", "Observed correlation:", 0.3, min = -1, max = 1, step = 0.01),
      
      # Correction options
      checkboxInput("apply_corrections", "Apply corrections", value = FALSE),
      
      # Conditional inputs for corrections
      conditionalPanel(
        condition = "input.apply_corrections",
        h4("Reliability Estimates"),
        numericInput("rxx", "Predictor reliability (rxx):", 0.8, min = 0, max = 1, step = 0.01),
        numericInput("ryy", "Criterion reliability (ryy):", 0.8, min = 0, max = 1, step = 0.01),
        numericInput("u", "Range restriction ratio (u):", 0.8, min = 0.01, max = 1, step = 0.01),
        helpText("u = SD of restricted group / SD of unrestricted group")
      ),
      
      # Sample size
      numericInput("n", "Sample size (n):", 100, min = 1, step = 1),
      
      # Plot options
      h4("Plot Options"),
      checkboxInput("show_error_bars", "Show error bars", value = FALSE),
      conditionalPanel(
        condition = "input.apply_corrections == true",
        checkboxInput("show_corrected", "Show correlation corrected for criterion unreliability", value = FALSE),
        checkboxInput("show_fully_corrected", "Show correlation corrected for criterion unreliability and range restriction", value = FALSE)
      ),
      
      # Action buttons
      div(style = "display: flex; gap: 10px;",
          actionButton("generate_chart", "Generate Chart"),
          actionButton("clear_chart", "Clear")
      )
    ),
    
    mainPanel(
      uiOutput("input_warnings"),
      div(id = "explanatory_content",
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
      plotOutput("expectancy_plot", height = "600px", width = "800px"),
      uiOutput("expectancy_text")
    )
  )
)

# Server definition
server <- function(input, output, session) {
  # Main reactive for all expectancy calculations and bar data
  data_reactive <- reactive({
    # Clamp values
    clamp <- function(x, minval, maxval) max(minval, min(x, maxval))
    rxy_clamped <- clamp(input$rxy, -0.99, 0.99)
    rxx_clamped <- clamp(input$rxx, 0.01, 1)
    ryy_clamped <- clamp(input$ryy, 0.01, 1)
    u_clamped <- if (!is.null(input$u)) clamp(input$u, 0.01, 1) else NULL

    # Calculate corrected correlations
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

    # Calculate expectancies
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

    # Bar data logic (as previously implemented)
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

    # Error bars
    if (input$show_error_bars) {
      bar_data$Probability <- as.numeric(bar_data$Probability)
      pval <- bar_data$Probability / 100
      n_total <- as.numeric(input$n)
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
        title = "Expectancy Chart",
        subtitle = if (input$show_corrected && input$show_fully_corrected) {
          paste("Probability of High Job Performance\n",
                "Uncorrected r =", round(d$rxy_clamped, 2), "\n",
                "Criterion Corrected r =", round(d$correlations$criterion_corrected, 2), "\n",
                "Fully Corrected r =", round(d$correlations$fully_corrected, 2))
        } else if (input$show_corrected) {
          paste("Probability of High Job Performance\n",
                "Uncorrected r =", round(d$rxy_clamped, 2), "\n",
                "Criterion Corrected r =", round(d$correlations$criterion_corrected, 2))
        } else if (input$show_fully_corrected) {
          paste("Probability of High Job Performance\n",
                "Uncorrected r =", round(d$rxy_clamped, 2), "\n",
                "Fully Corrected r =", round(d$correlations$fully_corrected, 2))
        } else {
          paste("Probability of High Job Performance (r =", round(d$rxy_clamped, 2), ")")
        },
        x = "Quartile Score on Selection Procedure",
        y = "Probability of High Job Performance"
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0, size = 16, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0, size = 14, color = "black"),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
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
    p
  })

  # Render the notes reactively (no change needed, just use data_reactive() for values)
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
      if (input$show_error_bars) paste0("Error bars represent 95% confidence intervals for each bar. Each bar assumes a sample size of n/4, where n is the total sample size you entered (n = ", input$n, ").<br>") else "",
      "<br>",
      
      "<strong>Step 6: How the Probabilities Are Calculated</strong><br>",
      "For each group, the probability is calculated using a statistical method called the bivariate normal distribution. This takes into account the correlation and the boundaries for each quartile.<br>",
      "<br>",
      "<strong>Assumptions</strong><br>",
      "These calculations assume that both test scores and job performance follow a normal (bell curve) distribution, and that each quartile contains the same number of people.<br>",
      "<br>For more details, see: Cucina, J. M., Berger, J., & Busciglio, H. (2017). Communicating criterion-related validity using expectancy charts: A new approach. <span style='font-style: italic;'>Personnel Assessment and Decisions, 3</span>(1), 1–10. <a href='https://doi.org/10.25035/pad.2017.001' target='_blank'>https://doi.org/10.25035/pad.2017.001</a>",
      "</div>"
    ))
  })

  # Clear chart
  observeEvent(input$clear_chart, {
    output$expectancy_plot <- renderPlot(NULL)
    output$expectancy_text <- renderUI(NULL)
    shinyjs::show("explanatory_content")
  })

  # Add warning messages to the UI
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
}

# Run the application
shinyApp(ui = ui, server = server) 