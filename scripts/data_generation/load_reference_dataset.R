# Load and Verify Sturman (2000) Reference Dataset
# This script loads the saved dataset and displays key information

library(dplyr)

cat("Loading Sturman (2000) Reference Dataset...\n")
cat("===========================================\n")

# Find the most recent dataset file
dataset_files <- list.files(pattern = "sturman_reference_dataset_.*\\.rds$")
if(length(dataset_files) == 0) {
  stop("No reference dataset files found. Run generate_reference_dataset.R first.")
}

# Use the most recent file
latest_file <- dataset_files[length(dataset_files)]
cat("Loading:", latest_file, "\n")

# Load the dataset
data <- readRDS(latest_file)

cat("Dataset loaded successfully!\n\n")

# Display basic information
cat("DATASET INFORMATION\n")
cat("===================\n")
cat("Number of observations:", nrow(data), "\n")
cat("Number of variables:", ncol(data), "\n")
cat("Memory size:", format(object.size(data), units = "MB"), "\n\n")

# Display variable names
cat("VARIABLES\n")
cat("=========\n")
cat("Parameters:", paste(names(data)[1:16], collapse = ", "), "\n")
cat("Utility estimates:", paste(names(data)[grep("utility_", names(data))], collapse = ", "), "\n")
cat("Percentage changes:", paste(names(data)[grep("pct_change_", names(data))], collapse = ", "), "\n\n")

# Key results
cat("KEY RESULTS\n")
cat("===========\n")
cat("Economic adjustment median reduction:", round(median(data$pct_change_economic, na.rm = TRUE), 1), "%\n")
cat("Multiple devices median reduction:", round(median(data$pct_change_multiple, na.rm = TRUE), 1), "%\n")
cat("All adjustments combined - negative cases:", round(sum(data$utility_all < 0, na.rm = TRUE) / nrow(data) * 100, 1), "%\n")

# Basic utility statistics
cat("\nBASIC UTILITY STATISTICS\n")
cat("========================\n")
basic_stats <- summary(data$utility_basic)
print(basic_stats)

cat("\nECONOMIC ADJUSTMENT STATISTICS\n")
cat("==============================\n")
econ_stats <- summary(data$utility_economic)
print(econ_stats)

cat("\nALL ADJUSTMENTS STATISTICS\n")
cat("==========================\n")
all_stats <- summary(data$utility_all)
print(all_stats)

cat("\nDataset ready for analysis!\n")
cat("Use: data <- readRDS('", latest_file, "')\n", sep = "") 