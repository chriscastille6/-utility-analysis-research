library(shiny)
library(ggplot2)
library(scales)
library(ggtext)
library(dplyr)
library(gridExtra)
# Add statistical testing libraries
library(nortest)  # For Anderson-Darling and other normality tests
library(poweRlaw) # For power law fitting and testing

# Function to generate power law distributed data with target mean
generate_power_law <- function(n, alpha = 2.5, xmin = 100, target_mean = NULL) {
  # Generate power law distributed data using inverse transform sampling
  u <- runif(n)
  x <- xmin * (1 - u)^(-1/(alpha - 1))
  
  # If target mean is specified, adjust the data to match it
  if (!is.null(target_mean)) {
    current_mean <- mean(x)
    # Scale the data to match target mean
    x <- x * (target_mean / current_mean)
  }
  
  return(x)
}

# Function to calculate theoretical mean of power law distribution
power_law_theoretical_mean <- function(alpha, xmin) {
  if (alpha <= 2) {
    return(Inf)  # Mean is infinite for alpha <= 2
  } else {
    return((alpha - 1) / (alpha - 2) * xmin)
  }
}

# Function to generate normal distributed data (like in main app)
generate_normal <- function(n, mean = 200, sd = 40) {
  return(rnorm(n, mean = mean, sd = sd))
}

ui <- fluidPage(
  titlePanel("Productivity Distribution Comparison: Normal vs Power Law"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Distribution Parameters"),
      
      # Sample size
      numericInput("sample_size", 
                   "Sample Size:", 
                   value = 500, 
                   min = 100, 
                   max = 5000, 
                   step = 100),
      
      # Distribution choice
      radioButtons("distribution", 
                   "Choose Distribution:",
                   choices = list(
                     "Normal Distribution (Current App)" = "normal",
                     "Power Law Distribution" = "power_law",
                     "Both (Side by Side)" = "both"
                   ),
                   selected = "both"),
      
      # Normal distribution parameters
      conditionalPanel(
        condition = "input.distribution == 'normal' || input.distribution == 'both'",
        h5("Normal Distribution Parameters"),
        numericInput("normal_mean", 
                     "Mean:", 
                     value = 200, 
                     min = 100, 
                     max = 300),
        numericInput("normal_sd", 
                     "Standard Deviation:", 
                     value = 40, 
                     min = 10, 
                     max = 100)
      ),
      
      # Power law parameters
      conditionalPanel(
        condition = "input.distribution == 'power_law' || input.distribution == 'both'",
        h5("Power Law Parameters"),
        numericInput("alpha", 
                     "Alpha (shape parameter):", 
                     value = 2.5, 
                     min = 1.1, 
                     max = 5.0, 
                     step = 0.1),
        numericInput("xmin", 
                     "Minimum value (xmin):", 
                     value = 100, 
                     min = 50, 
                     max = 200)
      ),
      
      # Generate new sample button
      actionButton("generate", "Generate New Sample", 
                   style = "margin-top: 20px; width: 100%;"),
      
      # Fixed seed option
      checkboxInput("fixed_seed", "Use Fixed Seed (reproducible)", value = TRUE),
      
      conditionalPanel(
        condition = "input.fixed_seed == true",
        numericInput("seed_value", "Seed Value:", value = 789, min = 1)
      )
    ),
    
    mainPanel(
      # Distribution plots
      plotOutput("distribution_plot", height = "500px"),
      
      # Summary statistics
      h4("Summary Statistics"),
      tableOutput("summary_stats"),
      
      # Explanation text
      h4("Understanding the Distributions"),
      htmlOutput("explanation"),
      
      # Statistical tests
      h4("Statistical Tests"),
      htmlOutput("statistical_tests")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data generation
  productivity_data <- reactive({
    # Use button click or initial load to trigger generation
    input$generate
    
    # Set seed if requested
    if (input$fixed_seed) {
      set.seed(input$seed_value)
    }
    
    n <- input$sample_size
    target_mean <- input$normal_mean  # Use normal mean as target for both distributions
    
    if (input$distribution == "normal") {
      data.frame(
        productivity = generate_normal(n, input$normal_mean, input$normal_sd),
        distribution = "Normal"
      )
    } else if (input$distribution == "power_law") {
      data.frame(
        productivity = generate_power_law(n, input$alpha, input$xmin, target_mean),
        distribution = "Power Law"
      )
    } else { # both
      normal_data <- generate_normal(n, input$normal_mean, input$normal_sd)
      power_law_data <- generate_power_law(n, input$alpha, input$xmin, target_mean)
      
      rbind(
        data.frame(productivity = normal_data, distribution = "Normal"),
        data.frame(productivity = power_law_data, distribution = "Power Law")
      )
    }
  })
  
  # Statistical tests
  statistical_tests <- reactive({
    data <- productivity_data()
    
    if (input$distribution == "both") {
      normal_data <- data[data$distribution == "Normal", "productivity"]
      power_law_data <- data[data$distribution == "Power Law", "productivity"]
      
      # Tests for normal data - keep normality tests for normal distribution
      normal_tests <- list(
        shapiro = if(length(normal_data) <= 5000) shapiro.test(normal_data) else NULL,
        kolmogorov_smirnov = ks.test(normal_data, "pnorm", mean = mean(normal_data), sd = sd(normal_data))
      )
      
      # Power law specific tests - focus on confirming power law behavior
      power_law_tests <- list()
      
      # Fit power law distribution using Clauset-Shalizi-Newman method
      pl_fit <- try({
        pl_obj <- displ$new(power_law_data)
        pl_obj$setXmin(estimate_xmin(pl_obj))
        pl_obj
      }, silent = TRUE)
      
      if (!inherits(pl_fit, "try-error")) {
        # 1. Clauset-Shalizi-Newman goodness-of-fit test
        power_law_tests$csn_test <- try({
          bootstrap_p(pl_fit, no_of_sims = 100, threads = 1)
        }, silent = TRUE)
        
        # 2. Compare power law to alternative distributions
        # Fit alternative distributions for comparison
        exp_fit <- try({
          exp_obj <- disexp$new(power_law_data)
          exp_obj$setXmin(pl_fit$getXmin())
          exp_obj$setPars(estimate_pars(exp_obj))
          exp_obj
        }, silent = TRUE)
        
        ln_fit <- try({
          ln_obj <- dislnorm$new(power_law_data)
          ln_obj$setXmin(pl_fit$getXmin())
          ln_obj$setPars(estimate_pars(ln_obj))
          ln_obj
        }, silent = TRUE)
        
        # Likelihood ratio tests
        if (!inherits(exp_fit, "try-error")) {
          power_law_tests$lr_vs_exponential <- try({
            compare_distributions(pl_fit, exp_fit)
          }, silent = TRUE)
        }
        
        if (!inherits(ln_fit, "try-error")) {
          power_law_tests$lr_vs_lognormal <- try({
            compare_distributions(pl_fit, ln_fit)
          }, silent = TRUE)
        }
        
        # 3. Power law parameter estimates
        power_law_tests$alpha_estimate <- pl_fit$getPars()
        power_law_tests$xmin_estimate <- pl_fit$getXmin()
        
        # 4. Kolmogorov-Smirnov test specifically for power law fit
        power_law_tests$ks_powerlaw <- try({
          # Generate theoretical power law data with same parameters
          theoretical_data <- rplcon(length(power_law_data), 
                                   xmin = pl_fit$getXmin(), 
                                   alpha = pl_fit$getPars())
          ks.test(power_law_data, theoretical_data)
        }, silent = TRUE)
      }
      
      power_law_tests$pl_fit <- pl_fit
      
      return(list(normal = normal_tests, power_law = power_law_tests))
      
    } else {
      # Single distribution tests
      prod_data <- data$productivity
      
      if (input$distribution == "normal") {
        # Normal distribution tests
        tests <- list(
          shapiro = if(length(prod_data) <= 5000) shapiro.test(prod_data) else NULL,
          kolmogorov_smirnov = ks.test(prod_data, "pnorm", mean = mean(prod_data), sd = sd(prod_data))
        )
      } else {
        # Power law distribution tests
        tests <- list()
        
        # Fit power law distribution
        pl_fit <- try({
          pl_obj <- displ$new(prod_data)
          pl_obj$setXmin(estimate_xmin(pl_obj))
          pl_obj
        }, silent = TRUE)
        
        if (!inherits(pl_fit, "try-error")) {
          # Clauset-Shalizi-Newman goodness-of-fit test
          tests$csn_test <- try({
            bootstrap_p(pl_fit, no_of_sims = 100, threads = 1)
          }, silent = TRUE)
          
          # Compare to alternative distributions
          exp_fit <- try({
            exp_obj <- disexp$new(prod_data)
            exp_obj$setXmin(pl_fit$getXmin())
            exp_obj$setPars(estimate_pars(exp_obj))
            exp_obj
          }, silent = TRUE)
          
          ln_fit <- try({
            ln_obj <- dislnorm$new(prod_data)
            ln_obj$setXmin(pl_fit$getXmin())
            ln_obj$setPars(estimate_pars(ln_obj))
            ln_obj
          }, silent = TRUE)
          
          # Likelihood ratio tests
          if (!inherits(exp_fit, "try-error")) {
            tests$lr_vs_exponential <- try({
              compare_distributions(pl_fit, exp_fit)
            }, silent = TRUE)
          }
          
          if (!inherits(ln_fit, "try-error")) {
            tests$lr_vs_lognormal <- try({
              compare_distributions(pl_fit, ln_fit)
            }, silent = TRUE)
          }
          
          # Parameter estimates
          tests$alpha_estimate <- pl_fit$getPars()
          tests$xmin_estimate <- pl_fit$getXmin()
          
          # KS test for power law fit
          tests$ks_powerlaw <- try({
            theoretical_data <- rplcon(length(prod_data), 
                                     xmin = pl_fit$getXmin(), 
                                     alpha = pl_fit$getPars())
            ks.test(prod_data, theoretical_data)
          }, silent = TRUE)
        }
        
        tests$pl_fit <- pl_fit
      }
      
      return(tests)
    }
  })
  
  # Generate plots
  output$distribution_plot <- renderPlot({
    data <- productivity_data()
    
    if (input$distribution == "both") {
      # Side by side histograms
      p1 <- ggplot(data[data$distribution == "Normal", ], aes(x = productivity)) +
        geom_histogram(bins = 30, fill = "lightblue", color = "white", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(productivity)), color = "red", linetype = "dashed", linewidth = 1) +
        labs(title = "Normal Distribution", 
             x = "Productivity (units)", 
             y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
      
      p2 <- ggplot(data[data$distribution == "Power Law", ], aes(x = productivity)) +
        geom_histogram(bins = 30, fill = "lightcoral", color = "white", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(productivity)), color = "red", linetype = "dashed", linewidth = 1) +
        labs(title = "Power Law Distribution", 
             x = "Productivity (units)", 
             y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
      
      # Combine plots
      grid.arrange(p1, p2, ncol = 2)
      
    } else {
      # Single histogram
      fill_color <- if (input$distribution == "normal") "lightblue" else "lightcoral"
      
      ggplot(data, aes(x = productivity)) +
        geom_histogram(bins = 30, fill = fill_color, color = "white", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(productivity)), color = "red", linetype = "dashed", linewidth = 1) +
        geom_vline(aes(xintercept = median(productivity)), color = "blue", linetype = "dashed", linewidth = 1) +
        labs(title = paste(unique(data$distribution), "Distribution"), 
             x = "Productivity (units)", 
             y = "Count",
             subtitle = "Red line = Mean, Blue line = Median") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", size = 14),
              plot.subtitle = element_text(size = 10))
    }
  })
  
  # Summary statistics table
  output$summary_stats <- renderTable({
    data <- productivity_data()
    target_mean <- input$normal_mean
    
    if (input$distribution == "both") {
      # Calculate stats for each distribution
      normal_stats <- data[data$distribution == "Normal", "productivity"]
      power_law_stats <- data[data$distribution == "Power Law", "productivity"]
      
      # Calculate theoretical power law mean
      theoretical_mean <- power_law_theoretical_mean(input$alpha, input$xmin)
      theoretical_mean_display <- if (is.infinite(theoretical_mean)) {
        "∞ (α ≤ 2)"
      } else {
        round(theoretical_mean, 1)
      }
      
      stats_df <- data.frame(
        Statistic = c("Target Mean", "Empirical Mean", "Theoretical Mean (Power Law)", "Median", "Standard Deviation", "Minimum", "Maximum", 
                     "90th Percentile", "95th Percentile", "99th Percentile"),
        Normal = c(
          round(target_mean, 1),
          round(mean(normal_stats), 1),
          "N/A",
          round(median(normal_stats), 1),
          round(sd(normal_stats), 1),
          round(min(normal_stats), 1),
          round(max(normal_stats), 1),
          round(quantile(normal_stats, 0.90), 1),
          round(quantile(normal_stats, 0.95), 1),
          round(quantile(normal_stats, 0.99), 1)
        ),
        Power_Law = c(
          round(target_mean, 1),
          round(mean(power_law_stats), 1),
          theoretical_mean_display,
          round(median(power_law_stats), 1),
          round(sd(power_law_stats), 1),
          round(min(power_law_stats), 1),
          round(max(power_law_stats), 1),
          round(quantile(power_law_stats, 0.90), 1),
          round(quantile(power_law_stats, 0.95), 1),
          round(quantile(power_law_stats, 0.99), 1)
        )
      )
      
      return(stats_df)
      
    } else {
      # Single distribution stats
      prod_data <- data$productivity
      
      # For power law, show theoretical mean
      if (input$distribution == "power_law") {
        theoretical_mean <- power_law_theoretical_mean(input$alpha, input$xmin)
        theoretical_mean_display <- if (is.infinite(theoretical_mean)) {
          "∞ (α ≤ 2)"
        } else {
          round(theoretical_mean, 1)
        }
        
        data.frame(
          Statistic = c("Target Mean", "Empirical Mean", "Theoretical Mean", "Median", "Standard Deviation", "Minimum", "Maximum", 
                       "90th Percentile", "95th Percentile", "99th Percentile"),
          Value = c(
            round(target_mean, 1),
            round(mean(prod_data), 1),
            theoretical_mean_display,
            round(median(prod_data), 1),
            round(sd(prod_data), 1),
            round(min(prod_data), 1),
            round(max(prod_data), 1),
            round(quantile(prod_data, 0.90), 1),
            round(quantile(prod_data, 0.95), 1),
            round(quantile(prod_data, 0.99), 1)
          )
        )
      } else {
        data.frame(
          Statistic = c("Target Mean", "Empirical Mean", "Median", "Standard Deviation", "Minimum", "Maximum", 
                       "90th Percentile", "95th Percentile", "99th Percentile"),
          Value = c(
            round(target_mean, 1),
            round(mean(prod_data), 1),
            round(median(prod_data), 1),
            round(sd(prod_data), 1),
            round(min(prod_data), 1),
            round(max(prod_data), 1),
            round(quantile(prod_data, 0.90), 1),
            round(quantile(prod_data, 0.95), 1),
            round(quantile(prod_data, 0.99), 1)
          )
        )
      }
    }
  }, striped = TRUE, hover = TRUE)
  
  # Statistical tests output
  output$statistical_tests <- renderUI({
    tests <- statistical_tests()
    
    if (input$distribution == "both") {
      # Format results for both distributions
      normal_html <- "<h5><b>Normal Distribution Tests:</b></h5>"
      power_law_html <- "<h5><b>Power Law Distribution Tests:</b></h5>"
      
      # Normal distribution tests
      if (!is.null(tests$normal$shapiro)) {
        normal_html <- paste0(normal_html, 
          "<b>Shapiro-Wilk Test for Normality:</b><br>",
          "W = ", round(tests$normal$shapiro$statistic, 4), 
          ", p-value = ", format.pval(tests$normal$shapiro$p.value, digits = 4),
          " (", ifelse(tests$normal$shapiro$p.value > 0.05, "Normal", "Not Normal"), ")<br><br>")
      } else {
        normal_html <- paste0(normal_html, "<b>Shapiro-Wilk Test:</b> Sample too large (n > 5000)<br><br>")
      }
      
      normal_html <- paste0(normal_html,
        "<b>Kolmogorov-Smirnov Test vs Normal:</b><br>",
        "D = ", round(tests$normal$kolmogorov_smirnov$statistic, 4),
        ", p-value = ", format.pval(tests$normal$kolmogorov_smirnov$p.value, digits = 4),
        " (", ifelse(tests$normal$kolmogorov_smirnov$p.value > 0.05, "Normal", "Not Normal"), ")<br>")
      
      # Power law distribution tests
      if (!is.null(tests$power_law$csn_test)) {
        power_law_html <- paste0(power_law_html,
          "<b>Clauset-Shalizi-Newman Goodness-of-Fit Test:</b><br>",
          "p-value = ", format.pval(tests$power_law$csn_test$p, digits = 4),
          " (", ifelse(tests$power_law$csn_test$p > 0.05, "Power Law", "Not Power Law"), ")<br><br>")
      } else {
        power_law_html <- paste0(power_law_html, "<b>Clauset-Shalizi-Newman Test:</b> Test failed<br><br>")
      }
      
      # Add likelihood ratio tests and parameter estimates
      if (!is.null(tests$power_law$alpha_estimate)) {
        power_law_html <- paste0(power_law_html,
          "<b>Power Law Parameter Estimates:</b><br>",
          "α = ", round(tests$power_law$alpha_estimate, 3), "<br>",
          "xmin = ", round(tests$power_law$xmin_estimate, 1), "<br><br>")
      }
      
      if (!is.null(tests$power_law$lr_vs_exponential) && !inherits(tests$power_law$lr_vs_exponential, "try-error")) {
        power_law_html <- paste0(power_law_html,
          "<b>Likelihood Ratio Tests:</b><br>",
          "Power Law vs Exponential: p = ", format.pval(tests$power_law$lr_vs_exponential$p, digits = 4), "<br>")
      }
      
      if (!is.null(tests$power_law$lr_vs_lognormal) && !inherits(tests$power_law$lr_vs_lognormal, "try-error")) {
        power_law_html <- paste0(power_law_html,
          "Power Law vs Log-Normal: p = ", format.pval(tests$power_law$lr_vs_lognormal$p, digits = 4), "<br><br>")
      }
      
      HTML(paste0(normal_html, "<hr>", power_law_html))
      
    } else {
      # Single distribution
      html_output <- "<h5><b>Statistical Tests:</b></h5>"
      
      if (input$distribution == "normal") {
        # Normal distribution tests
        if (!is.null(tests$shapiro)) {
          html_output <- paste0(html_output,
            "<b>Shapiro-Wilk Test for Normality:</b><br>",
            "W = ", round(tests$shapiro$statistic, 4),
            ", p-value = ", format.pval(tests$shapiro$p.value, digits = 4),
            " (", ifelse(tests$shapiro$p.value > 0.05, "Normal", "Not Normal"), ")<br><br>")
        } else {
          html_output <- paste0(html_output, "<b>Shapiro-Wilk Test:</b> Sample too large (n > 5000)<br><br>")
        }
        
        html_output <- paste0(html_output,
          "<b>Kolmogorov-Smirnov Test vs Normal:</b><br>",
          "D = ", round(tests$kolmogorov_smirnov$statistic, 4),
          ", p-value = ", format.pval(tests$kolmogorov_smirnov$p.value, digits = 4),
          " (", ifelse(tests$kolmogorov_smirnov$p.value > 0.05, "Normal", "Not Normal"), ")<br><br>")
        
      } else {
        # Power law distribution tests
        if (!is.null(tests$csn_test) && !inherits(tests$csn_test, "try-error")) {
          html_output <- paste0(html_output,
            "<b>Clauset-Shalizi-Newman Goodness-of-Fit Test:</b><br>",
            "p-value = ", format.pval(tests$csn_test$p, digits = 4),
            " (", ifelse(tests$csn_test$p > 0.05, "Power Law", "Not Power Law"), ")<br><br>")
        }
        
        if (!is.null(tests$alpha_estimate)) {
          html_output <- paste0(html_output,
            "<b>Power Law Parameter Estimates:</b><br>",
            "α = ", round(tests$alpha_estimate, 3), "<br>",
            "xmin = ", round(tests$xmin_estimate, 1), "<br><br>")
        }
        
        if (!is.null(tests$lr_vs_exponential) && !inherits(tests$lr_vs_exponential, "try-error")) {
          html_output <- paste0(html_output,
            "<b>Likelihood Ratio Tests:</b><br>",
            "Power Law vs Exponential: p = ", format.pval(tests$lr_vs_exponential$p, digits = 4), "<br>")
        }
        
        if (!is.null(tests$lr_vs_lognormal) && !inherits(tests$lr_vs_lognormal, "try-error")) {
          html_output <- paste0(html_output,
            "Power Law vs Log-Normal: p = ", format.pval(tests$lr_vs_lognormal$p, digits = 4), "<br><br>")
        }
      }
      
      HTML(html_output)
    }
  })
  
  # Explanation text
  output$explanation <- renderUI({
    HTML(paste0(
      "<b>Normal Distribution (Used in Main App):</b><br>",
      "• Bell-shaped curve with most values clustered around the mean<br>",
      "• Symmetric distribution - equal probability of values above and below the mean<br>",
      "• Standard deviation determines the spread of the distribution<br>",
      "• About 68% of values fall within 1 standard deviation of the mean<br>",
      "• About 95% of values fall within 2 standard deviations of the mean<br>",
      "• Extreme values (very high or very low performers) are relatively rare<br><br>",
      
      "<b>Power Law Distribution:</b><br>",
      "• Heavy-tailed distribution with a long right tail<br>",
      "• Small number of individuals have extremely high productivity<br>",
      "• Many individuals have relatively low productivity<br>",
      "• Also known as 'Pareto distribution' or '80-20 rule'<br>",
      "• Common in many real-world phenomena (wealth, city sizes, etc.)<br>",
      "• Alpha parameter controls the shape: lower alpha = heavier tail<br>",
      "• Mean is often much higher than median due to extreme values<br><br>",
      
      "<b>Statistical Tests Explained:</b><br>",
      "• <b>Shapiro-Wilk Test:</b> Tests if data comes from a normal distribution<br>",
      "  - Null hypothesis: Data is normally distributed<br>",
      "  - p > 0.05: Fail to reject (data appears normal)<br>",
      "  - p ≤ 0.05: Reject null (data is not normal)<br>",
      "  - Only works for samples ≤ 5000<br><br>",
      "• <b>Kolmogorov-Smirnov Test:</b> Compares sample to theoretical normal<br>",
      "  - Tests maximum difference between empirical and theoretical CDF<br>",
      "  - Same interpretation as other normality tests<br><br>",
      "• <b>Clauset-Shalizi-Newman Test:</b> Specifically tests for power law behavior<br>",
      "  - Uses bootstrap resampling to test goodness-of-fit to power law<br>",
      "  - p > 0.05: Power law is plausible<br>",
      "  - p ≤ 0.05: Reject power law hypothesis<br>",
      "  - Gold standard for testing power law distributions<br><br>",
      "• <b>Likelihood Ratio Tests:</b> Compare power law to alternative distributions<br>",
      "  - Tests whether power law fits better than exponential or log-normal<br>",
      "  - p < 0.05: Power law significantly better than alternative<br>",
      "  - p > 0.05: No significant difference between distributions<br>",
      "  - Helps distinguish true power laws from similar distributions<br><br>",
      
      "<b>Implications for HR and Selection:</b><br>",
      "• <b>Normal Distribution Assumption:</b> Traditional HR practices assume normal distribution<br>",
      "  - Selection systems designed to improve average performance<br>",
      "  - Performance management focuses on moving people toward the mean<br>",
      "  - Compensation systems often assume bell curve performance<br><br>",
      "• <b>Power Law Reality:</b> Some argue productivity follows power law<br>",
      "  - A few 'star performers' contribute disproportionately to organizational success<br>",
      "  - Selection should focus on identifying potential stars<br>",
      "  - Retention of top performers becomes critical<br>",
      "  - Different compensation strategies may be needed<br><br>",
      
      "<b>Research Evidence:</b><br>",
      "• Hunter, Schmidt & Judiesch (1990) found that for complex jobs, productivity distributions have larger standard deviations<br>",
      "• Some studies suggest power law distributions in software development and sales<br>",
      "• The debate continues in organizational psychology about which distribution better represents reality<br>",
      "• The choice of distribution assumption has significant implications for HR strategy<br><br>",
      
      "<b>Interpreting the Test Results:</b><br>",
      "• <b>For Normal Data:</b> Expect normality tests to have p > 0.05<br>",
      "• <b>For Power Law Data:</b> Expect normality tests to have p < 0.05 (rejecting normality)<br>",
      "  and Clauset-Shalizi-Newman test to have p > 0.05 (supporting power law)<br>",
      "• <b>Likelihood Ratios:</b> p < 0.05 means power law fits significantly better<br>",
      "• <b>Parameter Estimates:</b> α (alpha) controls the tail heaviness<br>",
      "  - Lower α = heavier tail (more extreme values)<br>",
      "  - Higher α = lighter tail (more like exponential)<br>",
      "• <b>Sample Size Matters:</b> Larger samples make it easier to detect deviations<br>",
      "• <b>Perfect Fit Rare:</b> Real data rarely fits theoretical distributions perfectly<br><br>",
      
      "<b>Try This:</b><br>",
      "• Generate samples and observe how test results change with sample size<br>",
      "• Adjust the alpha parameter for power law (try 1.5, 2.0, 3.0) and see how tests respond<br>",
      "• Compare test results between normal and power law distributions<br>",
      "• Notice how bootstrap p-values change when you generate new samples<br>",
      "• Consider: What would these test results mean for your organization's selection strategy?"
    ))
  })
}

shinyApp(ui, server) 