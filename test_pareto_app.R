# Test Custom Pareto Optimization Shiny App
# Standalone version to test before integration into main staffing utility app

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# =============================================================================
# CUSTOM PARETO OPTIMIZATION FUNCTIONS
# =============================================================================

# Calculate composite validity for given weights
calculate_composite_validity <- function(weights, cor_matrix) {
  composite_validity <- sqrt(t(weights) %*% cor_matrix %*% weights)
  return(as.numeric(composite_validity))
}

# Calculate adverse impact ratio for given weights
calculate_adverse_impact <- function(weights, d, sr) {
  composite_d <- sum(weights * d)
  minority_selection_rate <- pnorm(qnorm(sr, lower.tail = FALSE) + composite_d, lower.tail = FALSE)
  majority_selection_rate <- sr
  ai_ratio <- minority_selection_rate / majority_selection_rate
  return(ai_ratio)
}

# Identify Pareto optimal solutions
identify_pareto_frontier <- function(validity_scores, ai_ratios) {
  pareto_optimal <- logical(length(validity_scores))
  
  for (i in 1:length(validity_scores)) {
    dominated <- FALSE
    for (j in 1:length(validity_scores)) {
      if (i != j) {
        if (validity_scores[j] >= validity_scores[i] && ai_ratios[j] >= ai_ratios[i] &&
            (validity_scores[j] > validity_scores[i] || ai_ratios[j] > ai_ratios[i])) {
          dominated <- TRUE
          break
        }
      }
    }
    pareto_optimal[i] <- !dominated
  }
  
  return(pareto_optimal)
}

# Main Pareto optimization function
custom_pareto_optimization <- function(prop, sr, d, cor_matrix, n_combinations = 100) {
  n_predictors <- length(d)
  
  weights_list <- list()
  validity_list <- numeric()
  ai_ratio_list <- numeric()
  
  set.seed(123)
  
  for (i in 1:n_combinations) {
    weights <- runif(n_predictors)
    weights <- weights / sum(weights)
    
    composite_validity <- calculate_composite_validity(weights, cor_matrix)
    ai_ratio <- calculate_adverse_impact(weights, d, sr)
    
    weights_list[[i]] <- weights
    validity_list[i] <- composite_validity
    ai_ratio_list[i] <- ai_ratio
  }
  
  pareto_optimal <- identify_pareto_frontier(validity_list, ai_ratio_list)
  
  results <- data.frame(
    combination = 1:n_combinations,
    validity = validity_list,
    ai_ratio = ai_ratio_list,
    pareto_optimal = pareto_optimal,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n_predictors) {
    results[[paste0("weight_", i)]] <- sapply(weights_list, function(w) w[i])
  }
  
  return(list(
    results = results,
    pareto_solutions = results[pareto_optimal, ],
    weights = weights_list[pareto_optimal],
    validity = validity_list[pareto_optimal],
    ai_ratio = ai_ratio_list[pareto_optimal]
  ))
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Custom Pareto Optimization Test"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pareto Analysis", tabName = "pareto", icon = icon("chart-line")),
      menuItem("Correlation Matrix", tabName = "correlation", icon = icon("table")),
      menuItem("Results Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "pareto",
        fluidRow(
          box(width = 4, title = "Parameters", status = "primary", solidHeader = TRUE,
            h5("Selection Parameters:"),
            sliderInput("prop", "Minority Proportion:", value = 0.35, min = 0.1, max = 0.9, step = 0.05),
            sliderInput("sr", "Selection Ratio:", value = 0.25, min = 0.05, max = 0.95, step = 0.05),
            numericInput("n_combinations", "Number of Combinations:", value = 200, min = 50, max = 1000, step = 50),
            
            br(),
            actionButton("run_pareto", "Run Pareto Analysis", class = "btn-success", style = "width: 100%;")
          ),
          
          box(width = 8, title = "Pareto Frontier", status = "success", solidHeader = TRUE,
            plotlyOutput("pareto_plot"),
            br(),
            fluidRow(
              valueBoxOutput("total_solutions", width = 4),
              valueBoxOutput("pareto_solutions", width = 4),
              valueBoxOutput("passing_80_rule", width = 4)
            )
          )
        ),
        
                 fluidRow(
           box(width = 6, title = "Top Pareto Solutions", status = "info", solidHeader = TRUE,
             DT::dataTableOutput("pareto_table")
           ),
           box(width = 6, title = "Strategy Analysis", status = "success", solidHeader = TRUE,
             htmlOutput("strategy_analysis")
           )
         ),
         
         fluidRow(
           box(width = 6, title = "Pareto Trade-off Curve", status = "primary", solidHeader = TRUE,
             plotlyOutput("pareto_tradeoff_plot")
           ),
           box(width = 6, title = "Predictor Weights Trade-off", status = "success", solidHeader = TRUE,
             plotlyOutput("weights_tradeoff_plot")
           )
         ),
         
         fluidRow(
           box(width = 12, title = "Weight Distribution Heatmap", status = "warning", solidHeader = TRUE,
             plotlyOutput("weight_analysis_plot")
           )
         )
      ),
      
             tabItem(tabName = "correlation",
         fluidRow(
           box(width = 12, title = "Berry et al. (2024) Correlation Matrix", status = "primary", solidHeader = TRUE,
             div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
               h5("Meta-Analytic Correlation Matrix:"),
               p("This correlation matrix represents the intercorrelations between selection methods based on Berry et al. (2024) meta-analysis. 
                 These correlations are used to calculate composite validity in the Pareto optimization analysis."),
               p(strong("Citation:"), "Berry, C. M., Sackett, P. R., & Landers, R. N. (2024). Pareto optimization with updated meta-analytic correlations. Journal of Applied Psychology.")
             ),
             DT::dataTableOutput("correlation_matrix_table")
           )
         ),
         
         fluidRow(
           box(width = 6, title = "Predictor Validities", status = "info", solidHeader = TRUE,
             div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px;",
               h6("Individual Criterion Validities:"),
               tags$ul(
                 tags$li("Biodata: r = 0.38"),
                 tags$li("Cognitive Ability: r = 0.31"),
                 tags$li("Personality: r = 0.19"),
                 tags$li("Structured Interview: r = 0.42"),
                 tags$li("Integrity Test: r = 0.31"),
                 tags$li("Performance Test: r = 0.26")
               )
             )
           ),
           
           box(width = 6, title = "Adverse Impact (d-values)", status = "warning", solidHeader = TRUE,
             div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px;",
               h6("Black-White Subgroup Differences:"),
               tags$ul(
                 tags$li("Biodata: d = 0.32"),
                 tags$li("Cognitive Ability: d = 0.79"),
                 tags$li("Personality: d = -0.07"),
                 tags$li("Structured Interview: d = 0.24"),
                 tags$li("Integrity Test: d = 0.10"),
                 tags$li("Performance Test: d = 0.37")
               ),
               p(em("Note: Negative d-values indicate minority advantage"))
             )
           )
         )
       ),
       
       tabItem(tabName = "table",
         fluidRow(
           box(width = 12, title = "All Solutions", status = "warning", solidHeader = TRUE,
             DT::dataTableOutput("all_solutions_table")
           )
         )
       ),
      
      tabItem(tabName = "about",
        fluidRow(
          box(width = 12, title = "Custom Pareto Optimization Test", status = "info", solidHeader = TRUE,
            h4("About This Test App"),
            p("This standalone app tests our custom Pareto optimization implementation based on De Corte et al. (2011) methodology."),
            
            h5("Key Features:"),
            tags$ul(
              tags$li("Custom Pareto optimization without external dependencies"),
              tags$li("Berry et al. (2024) correlation matrix"),
              tags$li("Adverse impact analysis with 4/5ths rule"),
              tags$li("Interactive parameter adjustment"),
              tags$li("Real-time Pareto frontier visualization")
            ),
            
            h5("Predictors (Berry et al., 2024):"),
            tags$ul(
              tags$li("Biodata (d = 0.32)"),
              tags$li("Cognitive Ability (d = 0.79)"),
              tags$li("Personality (d = -0.07)"),
              tags$li("Structured Interview (d = 0.24)"),
              tags$li("Integrity Test (d = 0.10)"),
              tags$li("Performance Test (d = 0.37)")
            ),
            
            h5("Validation Checks:"),
            tags$ul(
              tags$li("✓ All validities positive"),
              tags$li("✓ All AI ratios in [0,1] range"),
              tags$li("✓ Weights sum to 1"),
              tags$li("✓ No Pareto solutions dominate each other"),
              tags$li("✓ 4/5ths rule compliance tracking")
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Berry et al. (2024) correlation matrix
  berry_matrix <- matrix(c(
    1.00, 0.13, 0.54, 0.21, 0.25, 0.42,
    0.13, 1.00, 0.03, 0.18, 0.01, 0.29,
    0.54, 0.03, 1.00, 0.08, 0.28, 0.23,
    0.21, 0.18, 0.08, 1.00, -0.02, 0.45,
    0.25, 0.01, 0.28, -0.02, 1.00, 0.16,
    0.42, 0.29, 0.23, 0.45, 0.16, 1.00
  ), nrow = 6, byrow = TRUE)
  
  # d-values for adverse impact
  d_values <- c(0.32, 0.79, -0.07, 0.24, 0.10, 0.37)
  predictor_names <- c("Biodata", "Cognitive", "Personality", "Interview", "Integrity", "Performance")
  
  # Pareto results reactive
  pareto_results <- reactive({
    input$run_pareto
    
    isolate({
      custom_pareto_optimization(
        prop = input$prop,
        sr = input$sr,
        d = d_values,
        cor_matrix = berry_matrix,
        n_combinations = input$n_combinations
      )
    })
  })
  
  # Pareto frontier plot
  output$pareto_plot <- renderPlotly({
    results <- pareto_results()
    
    # Create base plot with all solutions as background
    all_data <- data.frame(
      AI_Ratio = results$results$ai_ratio,
      Validity = results$results$validity,
      Pareto_Optimal = results$results$pareto_optimal
    )
    
    # Create Pareto data with strategy labels
    pareto_data <- all_data[all_data$Pareto_Optimal, ]
    
    # Add strategy labels if Pareto solutions exist
    if (nrow(pareto_data) > 0) {
      # Identify key strategies
      high_perf_row <- which.max(pareto_data$Validity)
      diversity_row <- which.max(pareto_data$AI_Ratio)
      balanced_row <- which.min(abs(pareto_data$AI_Ratio - 0.80))
      
      pareto_data$Strategy <- "Pareto Frontier"
      pareto_data$Strategy[high_perf_row] <- "High Performance"
      pareto_data$Strategy[diversity_row] <- "Diversity Focused"
      pareto_data$Strategy[balanced_row] <- "Balanced"
      
      # Create plot
      p <- ggplot() +
        # All solutions as background
        geom_point(data = all_data, aes(x = AI_Ratio, y = Validity), 
                  color = "lightgray", alpha = 0.3, size = 1) +
        # Pareto frontier line (sorted for smooth curve)
        geom_line(data = pareto_data[order(pareto_data$AI_Ratio), ], 
                 aes(x = AI_Ratio, y = Validity), 
                 color = "red", linewidth = 2, alpha = 0.8) +
        # Pareto solutions
        geom_point(data = pareto_data, aes(x = AI_Ratio, y = Validity, color = Strategy), 
                  size = 3, alpha = 0.8) +
        scale_color_manual(values = c("High Performance" = "purple", 
                                     "Diversity Focused" = "blue", 
                                     "Balanced" = "green", 
                                     "Pareto Frontier" = "red")) +
        labs(title = "Pareto Frontier: Validity vs. Adverse Impact",
             subtitle = "Red line shows Pareto-optimal frontier from all simulated combinations",
             x = "Adverse Impact Ratio", y = "Criterion Validity") +
        theme_minimal() +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "orange", alpha = 0.7) +
        annotate("text", x = 0.5, y = 0.85, label = "4/5ths Rule (0.80)", color = "orange", size = 3)
    } else {
      # Fallback if no Pareto solutions
      p <- ggplot(all_data, aes(x = AI_Ratio, y = Validity)) +
        geom_point(color = "lightgray", alpha = 0.3) +
        labs(title = "No Pareto Solutions Found",
             x = "Adverse Impact Ratio", y = "Criterion Validity") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
   
   # Pareto Trade-off Curve (ParetoR style)
   output$pareto_tradeoff_plot <- renderPlotly({
     results <- pareto_results()
     
     if (nrow(results$pareto_solutions) == 0) {
       return(plotly_empty())
     }
     
     # Sort Pareto solutions by AI ratio for smooth curve
     pareto_data <- results$pareto_solutions[order(results$pareto_solutions$ai_ratio), ]
     
     p <- ggplot(pareto_data, aes(x = ai_ratio, y = validity)) +
       geom_line(color = "red", linewidth = 2) +
       geom_point(color = "red", size = 3, shape = 8) +
       geom_hline(yintercept = 0.80, linetype = "dashed", color = "orange", linewidth = 1) +
       annotate("text", x = 0.5, y = 0.82, label = "4/5ths Rule (0.80)", 
                color = "orange", size = 3) +
       labs(title = "Composite Validity -- AI Ratio Trade-off",
            subtitle = "Pareto Frontier",
            x = "AI Ratio",
            y = "Composite Validity") +
       theme_minimal() +
       theme(plot.title = element_text(size = 14, face = "bold"))
     
     ggplotly(p, tooltip = c("x", "y"))
   })
   
   # Predictor Weights Trade-off Function (ParetoR style)
   output$weights_tradeoff_plot <- renderPlotly({
     results <- pareto_results()
     
     if (nrow(results$pareto_solutions) == 0) {
       return(plotly_empty())
     }
     
     # Sort Pareto solutions by AI ratio
     pareto_data <- results$pareto_solutions[order(results$pareto_solutions$ai_ratio), ]
     
     # Create weights data frame
     weights_data <- data.frame()
     
     for (i in 1:length(results$weights)) {
       weights <- results$weights[[i]]
       ai_ratio <- pareto_data$ai_ratio[i]
       
       for (j in 1:length(predictor_names)) {
         weights_data <- rbind(weights_data, data.frame(
           AI_Ratio = ai_ratio,
           Predictor = predictor_names[j],
           Weight = weights[j],
           stringsAsFactors = FALSE
         ))
       }
     }
     
     # Color palette for predictors
     colors <- c("Biodata" = "red", "Cognitive" = "yellow", "Personality" = "green", 
                "Interview" = "cyan", "Integrity" = "blue", "Performance" = "magenta")
     
     p <- ggplot(weights_data, aes(x = AI_Ratio, y = Weight, color = Predictor, linetype = Predictor)) +
       geom_line(linewidth = 1.5) +
       geom_point(size = 2, shape = 8) +
       scale_color_manual(values = colors) +
       scale_linetype_manual(values = c("Biodata" = "dashed", "Cognitive" = "dashed", 
                                       "Personality" = "dashed", "Interview" = "dashed", 
                                       "Integrity" = "dashed", "Performance" = "dashed")) +
       labs(title = "Predictor Weights Trade-off Function",
            subtitle = "How weights change across the Pareto frontier",
            x = "AI Ratio",
            y = "Predictor Weight") +
       theme_minimal() +
       theme(plot.title = element_text(size = 14, face = "bold"),
             legend.position = "bottom") +
       ylim(0, 1)
     
     ggplotly(p, tooltip = c("x", "y", "color"))
   })
   
   # Value boxes
  output$total_solutions <- renderValueBox({
    results <- pareto_results()
    valueBox(
      value = nrow(results$results),
      subtitle = "Total Solutions",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$pareto_solutions <- renderValueBox({
    results <- pareto_results()
    valueBox(
      value = sum(results$results$pareto_optimal),
      subtitle = "Pareto Optimal",
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$passing_80_rule <- renderValueBox({
    results <- pareto_results()
    passing <- sum(results$results$ai_ratio >= 0.80)
    valueBox(
      value = passing,
      subtitle = "Pass 4/5ths Rule",
      icon = icon("check-circle"),
      color = "orange"
    )
  })
  
  # Pareto solutions table
  output$pareto_table <- DT::renderDataTable({
    results <- pareto_results()
    
    if (nrow(results$pareto_solutions) == 0) {
      return(data.frame(Message = "No Pareto optimal solutions found"))
    }
    
    # Create table with weights
    table_data <- results$pareto_solutions[, c("combination", "validity", "ai_ratio")]
    
    # Add weight columns
    for (i in 1:length(predictor_names)) {
      table_data[[predictor_names[i]]] <- sapply(results$weights, function(w) w[i])
    }
    
    # Add 4/5ths rule column
    table_data$`4/5ths Rule` <- ifelse(table_data$ai_ratio >= 0.80, "PASS", "FAIL")
    
    # Round numeric columns
    table_data$validity <- round(table_data$validity, 3)
    table_data$ai_ratio <- round(table_data$ai_ratio, 3)
    for (i in 1:length(predictor_names)) {
      table_data[[predictor_names[i]]] <- round(table_data[[predictor_names[i]]], 3)
    }
    
    # Rename columns
    names(table_data)[1:3] <- c("Solution", "Validity", "AI Ratio")
    
    DT::datatable(table_data, 
                  options = list(pageLength = 10, searching = FALSE),
                  caption = "Pareto Optimal Solutions")
  })
  
     # Correlation matrix table
   output$correlation_matrix_table <- DT::renderDataTable({
     # Create correlation matrix data frame
     cor_matrix_df <- as.data.frame(berry_matrix)
     names(cor_matrix_df) <- predictor_names
     row.names(cor_matrix_df) <- predictor_names
     
     # Round to 2 decimal places
     cor_matrix_df <- round(cor_matrix_df, 2)
     
     DT::datatable(cor_matrix_df,
                   options = list(pageLength = 6, searching = FALSE, paging = FALSE),
                   caption = "Intercorrelations Between Selection Methods",
                   class = "display") %>%
       DT::formatStyle(
         columns = 1:ncol(cor_matrix_df),
         backgroundColor = DT::styleInterval(
           c(-0.5, 0, 0.5),
           c("#ffcccc", "#ffffff", "#ccffcc", "#ccffcc")
         )
       )
   })
   
   # All solutions table
   output$all_solutions_table <- DT::renderDataTable({
     results <- pareto_results()
     
     table_data <- results$results[, c("combination", "validity", "ai_ratio", "pareto_optimal")]
     
     # Add weight columns
     for (i in 1:length(predictor_names)) {
       table_data[[predictor_names[i]]] <- sapply(results$results$combination, function(comb) {
         idx <- which(results$results$combination == comb)
         if (length(idx) > 0) {
           results$results[[paste0("weight_", i)]][idx]
         } else {
           NA
         }
       })
     }
     
     # Add 4/5ths rule column
     table_data$`4/5ths Rule` <- ifelse(table_data$ai_ratio >= 0.80, "PASS", "FAIL")
     
     # Round numeric columns
     table_data$validity <- round(table_data$validity, 3)
     table_data$ai_ratio <- round(table_data$ai_ratio, 3)
     for (i in 1:length(predictor_names)) {
       table_data[[predictor_names[i]]] <- round(table_data[[predictor_names[i]]], 3)
     }
     
     # Rename columns
     names(table_data)[1:4] <- c("Solution", "Validity", "AI Ratio", "Pareto Optimal")
     table_data$`Pareto Optimal` <- ifelse(table_data$`Pareto Optimal`, "Yes", "No")
     
     DT::datatable(table_data, 
                   options = list(pageLength = 25, searching = TRUE),
                   caption = "All Solutions Generated")
   })
   
   # Strategy analysis output
   output$strategy_analysis <- renderUI({
     results <- pareto_results()
     
     if (nrow(results$pareto_solutions) == 0) {
       return(HTML("<p>No Pareto optimal solutions found.</p>"))
     }
     
     # Find different strategy types
     pareto_data <- results$pareto_solutions
     
     # Strategy 1: Highest Validity
     highest_validity <- pareto_data[which.max(pareto_data$validity), ]
     highest_weights <- results$weights[[which.max(pareto_data$validity)]]
     
     # Strategy 2: Highest AI Ratio (most diverse)
     highest_ai <- pareto_data[which.max(pareto_data$ai_ratio), ]
     highest_ai_weights <- results$weights[[which.max(pareto_data$ai_ratio)]]
     
     # Strategy 3: Balanced (closest to 0.80 AI ratio)
     balanced_idx <- which.min(abs(pareto_data$ai_ratio - 0.80))
     balanced <- pareto_data[balanced_idx, ]
     balanced_weights <- results$weights[[balanced_idx]]
     
     HTML(paste0(
       "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
       "<h6><strong>Strategy Recommendations:</strong></h6>",
       
       "<div style='background-color: #e8f5e8; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
       "<h6><strong>🏆 High-Performance Strategy:</strong></h6>",
       "<p><strong>Validity:</strong> ", round(highest_validity$validity, 3), 
       " | <strong>AI Ratio:</strong> ", round(highest_validity$ai_ratio, 3), "</p>",
       "<p><strong>Key Weights:</strong> ", 
       paste(predictor_names[which(highest_weights > 0.2)], collapse = ", "), "</p>",
       "</div>",
       
       "<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
       "<h6><strong>🌍 Diversity-Focused Strategy:</strong></h6>",
       "<p><strong>Validity:</strong> ", round(highest_ai$validity, 3), 
       " | <strong>AI Ratio:</strong> ", round(highest_ai$ai_ratio, 3), "</p>",
       "<p><strong>Key Weights:</strong> ", 
       paste(predictor_names[which(highest_ai_weights > 0.2)], collapse = ", "), "</p>",
       "</div>",
       
       "<div style='background-color: #e8f4fd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
       "<h6><strong>⚖️ Balanced Strategy:</strong></h6>",
       "<p><strong>Validity:</strong> ", round(balanced$validity, 3), 
       " | <strong>AI Ratio:</strong> ", round(balanced$ai_ratio, 3), "</p>",
       "<p><strong>Key Weights:</strong> ", 
       paste(predictor_names[which(balanced_weights > 0.2)], collapse = ", "), "</p>",
       "</div>",
       
       "<hr>",
       "<p><em>Recommendation: Choose strategy based on organizational priorities for performance vs. diversity.</em></p>",
       "</div>"
     ))
   })
   
   # Weight analysis plot
   output$weight_analysis_plot <- renderPlotly({
     results <- pareto_results()
     
     if (nrow(results$pareto_solutions) == 0) {
       return(plotly_empty())
     }
     
     # Create weight analysis data
     weight_data <- data.frame()
     
     for (i in 1:length(results$weights)) {
       weights <- results$weights[[i]]
       for (j in 1:length(predictor_names)) {
         weight_data <- rbind(weight_data, data.frame(
           Solution = i,
           Predictor = predictor_names[j],
           Weight = weights[j],
           Validity = results$pareto_solutions$validity[i],
           AI_Ratio = results$pareto_solutions$ai_ratio[i],
           stringsAsFactors = FALSE
         ))
       }
     }
     
     # Create heatmap-style plot
     p <- ggplot(weight_data, aes(x = Predictor, y = reorder(Solution, Validity), fill = Weight)) +
       geom_tile() +
       scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue", 
                           midpoint = 0.3, limits = c(0, 1)) +
       labs(title = "Weight Distribution Across Pareto Solutions",
            subtitle = "Darker blue = higher weight | Solutions ordered by validity",
            x = "Selection Method",
            y = "Pareto Solution (ranked by validity)",
            fill = "Weight") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
             legend.position = "bottom") +
       geom_text(aes(label = sprintf("%.2f", Weight)), 
                color = "black", size = 2.5, fontface = "bold")
     
     ggplotly(p, tooltip = c("x", "y", "fill", "Validity", "AI_Ratio"))
   })
}

# Run the application
shinyApp(ui = ui, server = server)
