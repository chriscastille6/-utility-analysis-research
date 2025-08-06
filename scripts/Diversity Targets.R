# Required Libraries
library(MASS)  # For mvrnorm function
library(ggplot2)
library(ggrepel)

# Constants
current_minority_proportion <- 0.10  # Starting with 10% minorities
current_workforce_size <- 600       # Initial workforce size
target_minority_ratio <- 0.35        # Target 35% minority ratio
quarterly_turnover_rate <- 0.10      # 10% turnover rate
number_of_new_hires <- 150           # Number of new hires each quarter
years <- 8                           # Total number of years
quarters <- years * 4                # Total number of quarters

# Adjusting the expected proportion of minorities in new hires
# Increase this value to reach the target faster
expected_proportion_minorities_new_hires <- 0.40  # 40% minorities in new hires

# Data storage
years_passed <- numeric(quarters)
cumulative_hires <- numeric(quarters)
minority_proportions <- numeric(quarters)

# Simulation loop with corrected cumulative hires calculation
for (i in 1:quarters) {
  # Calculate turnover
  minorities_leaving <- round(current_minority_proportion * current_workforce_size * quarterly_turnover_rate)
  non_minorities_leaving <- round((1 - current_minority_proportion) * current_workforce_size * quarterly_turnover_rate)
  
  # Update workforce after turnover
  current_workforce_size <- current_workforce_size - minorities_leaving - non_minorities_leaving
  current_minority_count <- current_minority_proportion * current_workforce_size
  
  # Add new hires
  current_workforce_size <- current_workforce_size + number_of_new_hires
  current_minority_count <- current_minority_count + (expected_proportion_minorities_new_hires * number_of_new_hires)
  
  # Update minority proportion
  current_minority_proportion <- current_minority_count / current_workforce_size
  
  # Correctly calculating cumulative hires
  cumulative_hires[i] <- number_of_new_hires * i
  
  # Store data
  years_passed[i] <- i / 4
  minority_proportions[i] <- current_minority_proportion * 100
}

# Creating a DataFrame
data <- data.frame(
  Year = years_passed,
  Cumulative_Hires = cumulative_hires,
  Percent_Minorities = minority_proportions
)

# Plotting
max_hires <- max(data$Cumulative_Hires)  # Get the maximum value of Cumulative_Hires

# Find the quarter when minority proportion reaches 35%
target_minority_proportion <- 35
target_reached_quarter <- which(minority_proportions >= target_minority_proportion)[1]
target_reached_year <- years_passed[target_reached_quarter]

# Preparing data for plotting
data$Scaled_Percent_Minorities <- data$Percent_Minorities * max(data$Cumulative_Hires) / 100

# Plotting with demarcation for when 35% is reached
plot <- ggplot(data, aes(x = Year)) +
  geom_line(aes(y = Cumulative_Hires, color = "Cumulative Hires")) +
  geom_line(aes(y = Scaled_Percent_Minorities, color = "Percent Minorities")) +
  geom_vline(xintercept = target_reached_year, linetype = "dashed", color = "green") +
  geom_text(aes(x = target_reached_year, y = max(data$Cumulative_Hires), label = paste("35% at Year", target_reached_year)), hjust = -0.1) +
  scale_color_manual(values = c("Cumulative Hires" = "blue", "Percent Minorities" = "red")) +
  labs(x = "Year", y = "Cumulative Hires") +
  scale_y_continuous(name = "Cumulative Hires", sec.axis = sec_axis(~ . * 100 / max(data$Cumulative_Hires), name = "Percent Minorities")) +
  ggtitle("Workforce Diversity Over Time") +
  theme_minimal()+
  guides(color = guide_legend(title = NULL))

# Print the plot
print(plot)

# Print the year when 35% minority representation is achieved
print(paste("35% minority representation is achieved in year:", target_reached_year))