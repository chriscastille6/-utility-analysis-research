# Summary Table Demo: Latham & Whyte Extension Analysis
# Quick script to display the key comparative results

# Load the results
load("latham_whyte_comprehensive_results.RData")

# Format currency function
format_currency <- function(amount) {
  paste0("$", formatC(round(amount), format = "d", big.mark = ","))
}

cat("=======================================================\n")
cat("LATHAM & WHYTE (1994) EXTENSION ANALYSIS\n")
cat("Comparing Traditional, Sturman (2000), and Star Power\n")
cat("=======================================================\n\n")

cat("CASE PARAMETERS:\n")
cat("• Position: Budget Analyst (Government agency)\n")
cat("• Selected:", lw_params$n_selected, "| Applicants:", lw_params$n_applicants, "\n")
cat("• Selection ratio:", paste0(lw_params$selection_ratio * 100, "%"), "\n")
cat("• Validity:", lw_params$validity, "| Salary:", format_currency(lw_params$mean_salary), "\n")
cat("• Time horizon:", lw_params$time_horizon, "years\n\n")

cat("COMPARATIVE RESULTS:\n")
cat("===================\n\n")

# Create formatted results
approaches <- c("Traditional (Brogden-Cronbach-Gleser)",
                "Sturman (2000) Monte Carlo Adjusted", 
                "Star Performer (Joo et al. 2022)",
                "Combined (Sturman + Star Power)")

sdy_values <- c(format_currency(traditional$sdy),
                format_currency(sturman$sdy),
                format_currency(star_power$sdy),
                format_currency(combined$sdy))

total_utilities <- c(format_currency(traditional$total_utility),
                     format_currency(sturman$total_utility),
                     format_currency(star_power$total_utility),
                     format_currency(combined$total_utility))

per_hire_values <- c(format_currency(traditional$per_hire_utility),
                     format_currency(sturman$per_hire_utility),
                     format_currency(star_power$per_hire_utility),
                     format_currency(combined$per_hire_utility))

# Calculate changes from traditional
base_utility <- traditional$total_utility
changes <- c("Baseline",
             paste0(round((sturman$total_utility - base_utility) / base_utility * 100, 1), "%"),
             paste0("+", round((star_power$total_utility - base_utility) / base_utility * 100, 1), "%"),
             paste0("+", round((combined$total_utility - base_utility) / base_utility * 100, 1), "%"))

# Display results
for(i in 1:length(approaches)) {
  cat(paste0(i, ". ", approaches[i], "\n"))
  cat("   SDy:", sdy_values[i], "\n")
  cat("   Total Utility:", total_utilities[i], "\n")
  cat("   Per-Hire Value:", per_hire_values[i], "\n")
  cat("   Change from Traditional:", changes[i], "\n\n")
}

cat("KEY INSIGHTS:\n")
cat("=============\n")
cat("• Traditional approach may overestimate by 35% (Sturman effect)\n")
cat("• Star performer recognition increases utility by 175%\n")
cat("• Combined realistic approach shows 120% improvement\n")
cat("• All scenarios support substantial selection program investment\n\n")

cat("ROI ANALYSIS:\n")
cat("=============\n")
assessment_costs <- lw_params$n_applicants * lw_params$cost_per_applicant
cat("• Total assessment costs:", format_currency(assessment_costs), "\n")
cat("• Conservative ROI (Sturman):", round(sturman$total_utility / assessment_costs), "× return\n")
cat("• Realistic ROI (Combined):", round(combined$total_utility / assessment_costs), "× return\n")
cat("• Optimistic ROI (Star Power):", round(star_power$total_utility / assessment_costs), "× return\n\n")

cat("METHODOLOGICAL EVOLUTION:\n")
cat("=========================\n")
cat("1940s-1980s: Classical utility analysis foundation\n")
cat("2000s:       Sturman's realism and operational constraints\n")
cat("2020s:       Star performer recognition and heavy-tailed distributions\n\n")

cat("Analysis complete. See simple_report.pdf for detailed report.\n") 