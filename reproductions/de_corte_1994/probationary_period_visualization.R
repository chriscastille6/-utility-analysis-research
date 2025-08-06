# Probationary Period Visualization
# This script creates visualizations to explain how De Corte's model works

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Set theme
theme_set(theme_bw() +
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 14, face = "bold"),
                  legend.text = element_text(size = 10),
                  panel.background = element_rect(fill = "white"),
                  plot.background = element_rect(fill = "white")))

# ============================================================================
# VISUALIZATION 1: TWO-STAGE DECISION PROCESS
# ============================================================================

# Create data for the two-stage process
stage_data <- data.frame(
  Stage = c("Selection", "Selection", "Retention", "Retention"),
  Outcome = c("Hired", "Not Hired", "Pass Probation", "Fail Probation"),
  Count = c(17, 119, 14.5, 2.5),
  Color = c("blue", "gray", "green", "red")
)

# Create the two-stage process diagram
p1 <- ggplot(stage_data, aes(x = Stage, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Hired" = "lightblue", "Not Hired" = "lightgray", 
                               "Pass Probation" = "lightgreen", "Fail Probation" = "lightcoral")) +
  labs(title = "Two-Stage Decision Process",
       subtitle = "Selection → Probation → Retention",
       x = "Decision Stage",
       y = "Number of Employees",
       fill = "Outcome") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Save the plot
ggsave("two_stage_process.png", p1, width = 8, height = 6, dpi = 300)

# ============================================================================
# VISUALIZATION 2: UTILITY COMPONENTS OVER TIME
# ============================================================================

# Create data for utility over time
time_data <- data.frame(
  Period = 1:11,
  All_Selectees = rep(17, 11),
  Retained_Employees = c(17, rep(14.5, 10)),
  Utility_Component = c("Initial", rep("Retention", 10))
)

# Create the utility over time plot
p2 <- ggplot(time_data, aes(x = Period)) +
  geom_line(aes(y = All_Selectees, color = "All Selectees"), size = 1.5) +
  geom_line(aes(y = Retained_Employees, color = "Retained Employees"), size = 1.5) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  annotate("text", x = 1.5, y = 18, label = "End of\nProbation", color = "red") +
  scale_color_manual(values = c("All Selectees" = "blue", "Retained Employees" = "green")) +
  labs(title = "Utility Components Over Time",
       subtitle = "Period 1: Probation (All Selectees) | Periods 2-11: Retention (Only Successful)",
       x = "Time Period",
       y = "Number of Employees Contributing to Utility",
       color = "Employee Group") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

# Save the plot
ggsave("utility_over_time.png", p2, width = 10, height = 6, dpi = 300)

# ============================================================================
# VISUALIZATION 3: SUCCESS RATIO COMPARISON
# ============================================================================

# Create data for success ratio comparison
success_data <- data.frame(
  Method = c("Random Selection", "Predictor Selection"),
  Success_Ratio = c(0.663, 0.853),
  Employees_Hired = c(17, 17),
  Employees_Retained = c(17 * 0.663, 17 * 0.853),
  Employees_Lost = c(17 * (1 - 0.663), 17 * (1 - 0.853))
)

# Reshape data for plotting
success_long <- success_data %>%
  select(Method, Employees_Retained, Employees_Lost) %>%
  gather(key = "Outcome", value = "Count", -Method)

# Create the success ratio comparison plot
p3 <- ggplot(success_long, aes(x = Method, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Employees_Retained" = "lightgreen", "Employees_Lost" = "lightcoral")) +
  labs(title = "Success Ratio Comparison",
       subtitle = "Impact of Selection Method on Employee Retention",
       x = "Selection Method",
       y = "Number of Employees",
       fill = "Outcome") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  geom_text(aes(label = round(Count, 1)), position = position_stack(vjust = 0.5))

# Save the plot
ggsave("success_ratio_comparison.png", p3, width = 8, height = 6, dpi = 300)

# ============================================================================
# VISUALIZATION 4: UTILITY BREAKDOWN
# ============================================================================

# Create data for utility breakdown
utility_data <- data.frame(
  Component = c("Initial Period\n(All Selectees)", "Retention Period\n(Successful Only)", "Training Costs", "Predictor Costs"),
  Value = c(17 * (28430 - 22500), 10 * 17 * 0.853 * (29947 - 22500), -17 * 10000, -136 * 200),
  Type = c("Revenue", "Revenue", "Cost", "Cost")
)

# Create the utility breakdown plot
p4 <- ggplot(utility_data, aes(x = Component, y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Revenue" = "lightgreen", "Cost" = "lightcoral")) +
  labs(title = "Utility Breakdown",
       subtitle = "Components of Total Utility with Predictor Selection",
       x = "Utility Component",
       y = "Dollar Value ($)",
       fill = "Type") +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  geom_text(aes(label = paste0("$", format(round(Value), big.mark=","))), 
            position = position_stack(vjust = 0.5))

# Save the plot
ggsave("utility_breakdown.png", p4, width = 10, height = 6, dpi = 300)

# ============================================================================
# VISUALIZATION 5: CONDITIONAL EXPECTATIONS
# ============================================================================

# Create data for conditional expectations
conditional_data <- data.frame(
  Group = c("All Selectees\nμ_y(x_c)", "Successful Selectees\nμ_y(x_c,r_c)", "Random Successful\nμ_y(r_c)"),
  Performance = c(28430, 29947, 28279),
  Type = c("Predictor Selection", "Predictor Selection", "Random Selection")
)

# Create the conditional expectations plot
p5 <- ggplot(conditional_data, aes(x = Group, y = Performance, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Predictor Selection" = "lightblue", "Random Selection" = "lightgray")) +
  labs(title = "Conditional Performance Expectations",
       subtitle = "Average Performance Payoff by Employee Group",
       x = "Employee Group",
       y = "Performance Payoff ($)",
       fill = "Selection Method") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  geom_text(aes(label = paste0("$", format(Performance, big.mark=","))), 
            position = position_stack(vjust = 0.5))

# Save the plot
ggsave("conditional_expectations.png", p5, width = 8, height = 6, dpi = 300)

# ============================================================================
# CREATE SUMMARY TABLE
# ============================================================================

# Create a summary table of key concepts
concept_summary <- data.frame(
  Concept = c("Two-Stage Process", "Success Ratio", "Conditional Expectations", 
              "Time Structure", "Cost Accounting"),
  Description = c("Selection → Probation → Retention", 
                  "Proportion of selectees who pass probation",
                  "Performance expectations given selection and retention",
                  "Period 1: All selectees; Periods 2-T: Only retained",
                  "Training, separation, and recruitment costs"),
  Impact = c("Introduces retention uncertainty", 
             "Determines effective workforce size",
             "Accounts for predictor-performance correlation",
             "Amplifies retention effects over time",
             "Provides economic realism")
)

# Save summary
write.csv(concept_summary, "probationary_concepts_summary.csv", row.names = FALSE)

cat("=== PROBATIONARY PERIOD VISUALIZATIONS COMPLETE ===\n")
cat("Created 5 visualization files:\n")
cat("1. two_stage_process.png - Two-stage decision process\n")
cat("2. utility_over_time.png - Utility components over time\n")
cat("3. success_ratio_comparison.png - Success ratio comparison\n")
cat("4. utility_breakdown.png - Utility breakdown by component\n")
cat("5. conditional_expectations.png - Conditional performance expectations\n")
cat("6. probationary_concepts_summary.csv - Summary of key concepts\n") 