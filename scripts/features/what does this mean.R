# Load required libraries
library(ggplot2)
library(scales)
library(ggtext)

# Parameters
initial_workforce <- 500
turnover_rate <- 0.10
productivity_per_worker <- 200
cost_per_hire <- 2000
salary <- 32000  # New: salary per hire
benefit_pct <- 20  # New: benefits as % of salary
training_cost_per_hire <- 200  # New: training cost per hire
# Calculate total cost per hire
total_cost_per_hire <- cost_per_hire + salary + (benefit_pct / 100) * salary + training_cost_per_hire
quarters <- c("Q1", "Q2", "Q3", "Q4")
demand <- c(100000, 100000, 105000, 110000)

# ---- Baseline Strategy ----
baseline_df <- data.frame(
  Quarter = quarters,
  Demand = demand
)

# Calculate baseline strategy
baseline_df$Turnover_Hires <- initial_workforce * turnover_rate
baseline_df$Base_Capacity <- initial_workforce * productivity_per_worker
baseline_df$Extra_Units <- pmax(baseline_df$Demand - baseline_df$Base_Capacity, 0)
baseline_df$Extra_Hires <- ceiling(baseline_df$Extra_Units / productivity_per_worker)
baseline_df$Total_Hires <- baseline_df$Turnover_Hires + baseline_df$Extra_Hires
baseline_df$Staffing_Cost <- baseline_df$Total_Hires * total_cost_per_hire
baseline_df$Strategy <- "Current"
baseline_df$Avg_Productivity <- productivity_per_worker
baseline_df$Productivity_Label <- paste0(baseline_df$Avg_Productivity)

# ---- Improved Strategy ----
improved_pct <- c(0.10, 0.19, 0.27, 0.34)
productivity_multiplier <- 1 + 0.08

# Calculate average productivity for each quarter
avg_productivity <- (1 - improved_pct) * 1.0 + improved_pct * productivity_multiplier
avg_productivity_units <- avg_productivity * productivity_per_worker

improved_df <- data.frame(
  Quarter = quarters,
  Demand = demand,
  Improved_Pct = improved_pct,
  Avg_Productivity = avg_productivity_units
)

# Calculate required workforce size based on productivity
improved_df$Required_Workforce <- ceiling(improved_df$Demand / improved_df$Avg_Productivity)

# Calculate workforce changes and hiring needs
current_workforce <- initial_workforce
current_high_quality_pct <- 0  # Track percentage of high-quality employees

# Create detailed savings breakdown
savings_breakdown <- data.frame(
  Quarter = quarters,
  Hires_Avoided = 0,
  Staffing_Cost_Saved = 0,
  Salary_Cost_Saved = 0,
  Benefits_Cost_Saved = 0,
  Training_Cost_Saved = 0,
  Total_Savings = 0
)

for (i in 1:nrow(improved_df)) {
  # Calculate turnover for current quarter
  turnover <- current_workforce * turnover_rate

  # Calculate the workforce after turnover
  workforce_after_turnover <- current_workforce - turnover

  # Calculate how many hires are needed to reach required workforce
  hires_needed <- ceiling(max(0, improved_df$Required_Workforce[i] - workforce_after_turnover))

  # Only hire up to the number of people lost to turnover (can't hire negative or more than turnover)
  hires_to_make <- min(turnover, hires_needed)

  # Total hires needed = hires_to_make
  improved_df$Total_Hires[i] <- hires_to_make

  # Calculate staffing cost: all hires use total_cost_per_hire
  improved_df$Staffing_Cost[i] <- hires_to_make * total_cost_per_hire

  # Calculate savings breakdown
  baseline_hires <- baseline_df$Total_Hires[i]
  hires_avoided <- baseline_hires - improved_df$Total_Hires[i]

  # Debug printing for each quarter
  cat(sprintf("\n%s Detailed Calculations (UPDATED):\n", quarters[i]))
  cat("Baseline Hires:", baseline_hires, "\n")
  cat("Improved Hires:", improved_df$Total_Hires[i], "\n")
  cat("Current Workforce:", current_workforce, "\n")
  cat("Required Workforce:", improved_df$Required_Workforce[i], "\n")
  cat("Turnover:", turnover, "\n")
  cat("Workforce After Turnover:", workforce_after_turnover, "\n")
  cat("Hires Needed:", hires_needed, "\n")
  cat("Hires To Make:", hires_to_make, "\n")
  cat("Improved Productivity:", improved_df$Avg_Productivity[i], "\n")
  cat("Baseline Productivity:", productivity_per_worker, "\n")

  savings_breakdown$Hires_Avoided[i] <- hires_avoided
  savings_breakdown$Staffing_Cost_Saved[i] <- hires_avoided * cost_per_hire
  savings_breakdown$Salary_Cost_Saved[i] <- hires_avoided * (salary / 4)
  savings_breakdown$Benefits_Cost_Saved[i] <- hires_avoided * ((salary * benefit_pct / 100) / 4)
  savings_breakdown$Training_Cost_Saved[i] <- hires_avoided * training_cost_per_hire
  savings_breakdown$Total_Savings[i] <- savings_breakdown$Staffing_Cost_Saved[i] + 
                                      savings_breakdown$Salary_Cost_Saved[i] + 
                                      savings_breakdown$Benefits_Cost_Saved[i] + 
                                      savings_breakdown$Training_Cost_Saved[i]

  # Print quarterly breakdown
  cat(sprintf("\nQuarter %s Breakdown (UPDATED):\n", quarters[i]))
  cat(sprintf("Hires Avoided: %.0f\n", hires_avoided))
  cat(sprintf("Staffing Cost Saved: $%.0f\n", savings_breakdown$Staffing_Cost_Saved[i]))
  cat(sprintf("Salary Cost Saved: $%.0f\n", savings_breakdown$Salary_Cost_Saved[i]))
  cat(sprintf("Benefits Cost Saved: $%.0f\n", savings_breakdown$Benefits_Cost_Saved[i]))
  cat(sprintf("Training Cost Saved: $%.0f\n", savings_breakdown$Training_Cost_Saved[i]))
  cat(sprintf("Total Savings: $%.0f\n", savings_breakdown$Total_Savings[i]))

  # Update current workforce for next quarter
  current_workforce <- workforce_after_turnover + hires_to_make

  # Update percentage of high-quality employees
  # New hires are all high-quality, and we lose some through turnover
  current_high_quality_pct <- (current_high_quality_pct * (current_workforce - hires_to_make) + 
                             hires_to_make) / current_workforce
}

improved_df$Strategy <- "Proposed"
improved_df$Productivity_Label <- paste0(round(improved_df$Avg_Productivity))

# ---- Merge Data ----
plot_df <- data.frame(
  Quarter = quarters,
  Current_Cost = baseline_df$Staffing_Cost,
  Proposed_Cost = improved_df$Staffing_Cost
)
plot_df$Cost_Savings <- plot_df$Current_Cost - plot_df$Proposed_Cost

# Print annual totals
cat("\nAnnual Totals:\n")
cat(sprintf("Total Hires Avoided: %.0f\n", sum(savings_breakdown$Hires_Avoided)))
cat(sprintf("Total Staffing Cost Saved: $%.0f\n", sum(savings_breakdown$Staffing_Cost_Saved)))
cat(sprintf("Total Salary Cost Saved: $%.0f\n", sum(savings_breakdown$Salary_Cost_Saved)))
cat(sprintf("Total Benefits Cost Saved: $%.0f\n", sum(savings_breakdown$Benefits_Cost_Saved)))
cat(sprintf("Total Training Cost Saved: $%.0f\n", sum(savings_breakdown$Training_Cost_Saved)))
cat(sprintf("Total Annual Savings: $%.0f\n", sum(savings_breakdown$Total_Savings)))

# ---- Cost Savings ----
total_baseline_cost <- sum(baseline_df$Staffing_Cost)
total_improved_cost <- sum(improved_df$Staffing_Cost)
total_savings <- total_baseline_cost - total_improved_cost

# ---- Title with Color-Coded Strategy ----
title_text <- "How Enhanced Staffing Delivers Value to the Firm"

# ---- Subtitle ----
total_savings_millions <- total_savings / 1e6
savings_label <- paste0(
  "The new system delivers more productive workers (predicted productivity in parentheses), delivering $",
  formatC(total_savings_millions, format = "f", digits = 3),
  "M in cost savings"
)

# ---- Note ----
note_text <- paste0(
  "We are assuming ", turnover_rate * 100, "% quarterly turnover, which may go down overtime by increasing the level of person-job fit via enhanced staffing. ",
  "\nCost savings measured against current trend of 200 units produced on average per worker per quarter. It comprises of selection costs ($2000 per hire),\ncompensation costs ($32k salary, 20% benefits), training costs ($200 per hire). ",
  "We are also assuming that no part-time hires are\nused to meet production demands."
)

# ---- Final Plot ----
staffing_plot <- ggplot(plot_df, aes(x = Quarter, y = Cost_Savings)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  # Add productivity labels above dollar values
  geom_text(
    data = data.frame(
      Quarter = quarters,
      Productivity = paste0("(", round(improved_df$Avg_Productivity), ")"),
      Cost_Savings = plot_df$Cost_Savings
    ),
    aes(label = Productivity),
    vjust = -2.5,
    size = 4.5,
    color = "forestgreen"
  ) +
  # Add dollar values with M or K formatting
  geom_text(
    aes(label = ifelse(abs(Cost_Savings) >= 1e6,
                       paste0("$", formatC(Cost_Savings / 1e6, format = "f", digits = 3), "M"),
                       paste0("$", formatC(Cost_Savings / 1e3, format = "f", digits = 1), "K"))),
    vjust = -0.5,
    size = 4.5
  ) +
  scale_y_continuous(
    labels = function(x) ifelse(abs(x) >= 1e6,
                                paste0("$", formatC(x / 1e6, format = "f", digits = 3), "M"),
                                paste0("$", formatC(x / 1e3, format = "f", digits = 1), "K")),
    expand = expansion(mult = c(0.05, 0.20))
  ) +
  labs(
    title = title_text,
    subtitle = savings_label,
    x = "Quarter",
    y = "Cost Savings Relative to Current Staffing Strategy",
    caption = note_text
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    plot.caption = element_text(hjust = 0),
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

staffing_plot
