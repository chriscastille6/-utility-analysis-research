# De Corte (1994) Paper Verification Comparison
# This script compares our reproduction results with the original paper's reported values

# Load required libraries
library(dplyr)
library(knitr)
library(kableExtra)

# ============================================================================
# ORIGINAL PAPER VALUES (from De Corte, 1994)
# ============================================================================

# Parameter values from the original paper
original_params <- data.frame(
  Parameter = c("N (Number of hires)", 
                "n (Applicant pool size)",
                "μ_s (Average service cost)",
                "Time (Average time periods)",
                "S_p (Success ratio predictor)",
                "μ_y (Average performance payoff)",
                "C_t (Training cost per hire)",
                "C_p (Cost per candidate)",
                "C_s (Separation cost)",
                "ρ_yR (Correlation payoff-performance)",
                "σ_y (Performance standard deviation)",
                "ρ (Predictor validity)"),
  Original_Value = c(17, 136, 22500, 11, 0.853, 25000, 10000, 200, 1000, 0.85, 7000, 0.35),
  Units = c("employees", "applicants", "$", "periods", "ratio", "$", "$", "$", "$", "correlation", "$", "correlation")
)

# Key calculated values from the original paper
original_calculations <- data.frame(
  Calculation = c("Critical predictor score (x_c)",
                  "Critical performance score (r_c)",
                  "Selection ratio (N/n)",
                  "Success ratio random (S_0)",
                  "μ_y(x_c) - Average payoff of selectees",
                  "μ_y(x_c,r_c) - Average payoff of successful selectees",
                  "μ_y(r_c) - Average payoff of random successful",
                  "Utility with predictor (U_p)",
                  "Utility with random selection (U_0)",
                  "Utility difference (ΔU)",
                  "Fixed quota utility difference",
                  "Fixed quota utility difference (corrected)"),
  Original_Value = c(1.15, -0.42, 0.125, 0.663, 28430, 29947, 28279, 983955, 523635, 460320, 344774, 298538),
  Units = c("z-score", "z-score", "ratio", "ratio", "$", "$", "$", "$", "$", "$", "$", "$")
)

# ============================================================================
# OUR REPRODUCTION VALUES
# ============================================================================

# Load our reproduction results
our_results <- read.csv("de_corte_1994_results.csv")

# Extract our calculated values
our_calculations <- data.frame(
  Calculation = c("Critical predictor score (x_c)",
                  "Critical performance score (r_c)",
                  "Selection ratio (N/n)",
                  "Success ratio random (S_0)",
                  "μ_y(x_c) - Average payoff of selectees",
                  "μ_y(x_c,r_c) - Average payoff of successful selectees",
                  "μ_y(r_c) - Average payoff of random successful",
                  "Utility with predictor (U_p)",
                  "Utility with random selection (U_0)",
                  "Utility difference (ΔU)",
                  "Fixed quota utility difference",
                  "Fixed quota utility difference (corrected)"),
  Our_Value = c(our_results$Value[1], our_results$Value[2], our_results$Value[3], 
                our_results$Value[5], NA, NA, NA, our_results$Value[6], 
                our_results$Value[7], our_results$Value[8], our_results$Value[9], 
                our_results$Value[10])
)

# ============================================================================
# COMPARISON ANALYSIS
# ============================================================================

# Merge original and our values for comparison
comparison <- merge(original_calculations, our_calculations, by = "Calculation")

# Calculate differences and percentage differences
comparison$Difference <- comparison$Our_Value - comparison$Original_Value
comparison$Percent_Difference <- (comparison$Difference / comparison$Original_Value) * 100

# Round values for display
comparison$Original_Value_Rounded <- round(comparison$Original_Value, 2)
comparison$Our_Value_Rounded <- round(comparison$Our_Value, 2)
comparison$Difference_Rounded <- round(comparison$Difference, 2)
comparison$Percent_Difference_Rounded <- round(comparison$Percent_Difference, 2)

# ============================================================================
# CREATE COMPARISON TABLES
# ============================================================================

# Parameter comparison table
cat("=== PARAMETER VALUES COMPARISON ===\n")
print(original_params)

# Results comparison table
cat("\n=== CALCULATED VALUES COMPARISON ===\n")
comparison_display <- comparison %>%
  select(Calculation, Original_Value_Rounded, Our_Value_Rounded, 
         Difference_Rounded, Percent_Difference_Rounded) %>%
  filter(!is.na(Our_Value_Rounded))

# Display comparison table
kable(comparison_display, 
      col.names = c("Calculation", "Original", "Our Value", "Difference", "% Diff"),
      caption = "Comparison of Reproduction Results with Original Paper") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# ============================================================================
# VERIFICATION STATUS
# ============================================================================

# Check which values are within acceptable tolerance
comparison$Verification_Status <- "Needs Review"
comparison$Verification_Status[abs(comparison$Percent_Difference_Rounded) <= 1] <- "✅ Verified"
comparison$Verification_Status[abs(comparison$Percent_Difference_Rounded) <= 5 & 
                              abs(comparison$Percent_Difference_Rounded) > 1] <- "⚠️ Minor Difference"
comparison$Verification_Status[abs(comparison$Percent_Difference_Rounded) > 5] <- "❌ Significant Difference"

# Summary of verification
verification_summary <- comparison %>%
  group_by(Verification_Status) %>%
  summarise(Count = n(), .groups = 'drop')

cat("\n=== VERIFICATION SUMMARY ===\n")
print(verification_summary)

# ============================================================================
# DETAILED ANALYSIS OF DIFFERENCES
# ============================================================================

cat("\n=== DETAILED ANALYSIS ===\n")

# Check critical values
critical_values <- comparison[comparison$Calculation %in% 
                             c("Critical predictor score (x_c)", "Critical performance score (r_c)"), ]
cat("\nCritical Values:\n")
print(critical_values[, c("Calculation", "Original_Value_Rounded", "Our_Value_Rounded", 
                         "Difference_Rounded", "Percent_Difference_Rounded", "Verification_Status")])

# Check utility values
utility_values <- comparison[comparison$Calculation %in% 
                            c("Utility with predictor (U_p)", "Utility with random selection (U_0)", 
                              "Utility difference (ΔU)"), ]
cat("\nUtility Values:\n")
print(utility_values[, c("Calculation", "Original_Value_Rounded", "Our_Value_Rounded", 
                        "Difference_Rounded", "Percent_Difference_Rounded", "Verification_Status")])

# ============================================================================
# SAVE COMPARISON RESULTS
# ============================================================================

# Save detailed comparison
write.csv(comparison, "paper_verification_comparison.csv", row.names = FALSE)

# Save verification summary
write.csv(verification_summary, "verification_summary.csv", row.names = FALSE)

# Create a summary report
summary_report <- data.frame(
  Metric = c("Total Calculations Compared",
             "Successfully Verified (≤1% difference)",
             "Minor Differences (1-5% difference)", 
             "Significant Differences (>5% difference)",
             "Overall Verification Rate"),
  Value = c(nrow(comparison),
            sum(comparison$Verification_Status == "✅ Verified"),
            sum(comparison$Verification_Status == "⚠️ Minor Difference"),
            sum(comparison$Verification_Status == "❌ Significant Difference"),
            paste0(round((sum(comparison$Verification_Status == "✅ Verified") / nrow(comparison)) * 100, 1), "%"))
)

cat("\n=== SUMMARY REPORT ===\n")
print(summary_report)

# Save summary report
write.csv(summary_report, "verification_summary_report.csv", row.names = FALSE)

cat("\n=== VERIFICATION COMPLETE ===\n")
cat("Detailed results saved to: paper_verification_comparison.csv\n")
cat("Summary saved to: verification_summary_report.csv\n") 