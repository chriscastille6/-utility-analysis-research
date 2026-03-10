# Employee Benefits Utility Analysis Demonstration
# Practical Application of Gregory (1981) Methodology to Employee Benefits
# 
# This script demonstrates how to apply Gregory's risk analysis approach
# to evaluate different employee benefits funding mechanisms

# Source the benefits utility analysis framework
source("employee_benefits_utility_analysis.R")

# =============================================================================
# DEMONSTRATION 1: HEALTH INSURANCE FUNDING DECISION
# =============================================================================

cat("=== DEMONSTRATION 1: HEALTH INSURANCE FUNDING DECISION ===\n")
cat("Adapting Gregory (1981) Healthcare Benefits Analysis\n\n")

# Scenario: 500-employee organization deciding on health insurance funding
health_insurance_params <- list(
  n_employees = 500,
  benefit_type = "health_insurance",
  funding_mechanism = "fully_insured"  # Will be varied in comparison
)

# Compare different funding mechanisms (similar to Gregory's ERP vs. self-funded)
funding_mechanisms <- c("fully_insured", "self_funded", "partially_self_funded", "stop_loss_insurance")

cat("Comparing funding mechanisms for 500 employees:\n")
cat("- Fully Insured: Traditional insurance with predictable costs\n")
cat("- Self Funded: Direct payment with higher risk but lower base cost\n")
cat("- Partially Self Funded: Hybrid approach with moderate risk\n")
cat("- Stop Loss Insurance: Self-funded with catastrophic protection\n\n")

# Calculate comparison
health_comparison <- compare_benefits_funding(health_insurance_params, funding_mechanisms, risk_aversion = 0.001)

# Display results
cat("HEALTH INSURANCE FUNDING COMPARISON:\n")
cat("=====================================\n")
print(health_comparison$comparison_summary)

# Identify optimal choice
optimal_choice <- health_comparison$comparison_summary[which.min(health_comparison$comparison_summary$rank), ]
cat("\nOPTIMAL CHOICE:", optimal_choice$funding_mechanism, "\n")
cat("Expected Annual Cost: $", format(round(optimal_choice$expected_cost), big.mark = ","), "\n")
cat("Risk Premium: $", format(round(optimal_choice$risk_premium), big.mark = ","), "\n")
cat("Certainty Equivalent: $", format(round(optimal_choice$certainty_equivalent), big.mark = ","), "\n\n")

# =============================================================================
# DEMONSTRATION 2: RISK SENSITIVITY ANALYSIS
# =============================================================================

cat("=== DEMONSTRATION 2: RISK SENSITIVITY ANALYSIS ===\n")
cat("How risk aversion affects funding mechanism choice\n\n")

# Analyze how different risk aversion levels affect the decision
risk_sensitivity <- benefits_risk_sensitivity(health_insurance_params, funding_mechanisms)

# Find crossover points where preferences change
cat("Risk Sensitivity Analysis Results:\n")
cat("Risk Aversion Range: 0.000 to 0.010\n")
cat("(Higher values = more risk averse)\n\n")

# Show how preferences change with risk aversion
risk_levels <- c(0.001, 0.003, 0.005, 0.007, 0.010)
cat("Funding Mechanism Rankings by Risk Aversion Level:\n")
cat("==================================================\n")

for (risk_level in risk_levels) {
  subset_data <- risk_sensitivity[risk_sensitivity$risk_aversion == risk_level, ]
  ranked_data <- subset_data[order(subset_data$expected_utility, decreasing = TRUE), ]
  
  cat("Risk Aversion =", risk_level, ":\n")
  for (i in 1:nrow(ranked_data)) {
    cat("  ", i, ".", ranked_data$funding_mechanism[i], 
        " (Utility =", round(ranked_data$expected_utility[i], 6), ")\n")
  }
  cat("\n")
}

# =============================================================================
# DEMONSTRATION 3: BENEFITS PORTFOLIO OPTIMIZATION
# =============================================================================

cat("=== DEMONSTRATION 3: BENEFITS PORTFOLIO OPTIMIZATION ===\n")
cat("Optimizing multiple benefits under budget constraints\n\n")

# Scenario: 1000-employee organization with $5M benefits budget
benefit_types <- c("health_insurance", "dental_insurance", "retirement_401k", "wellness_program")

cat("Benefits Portfolio Optimization:\n")
cat("- Organization Size: 1,000 employees\n")
cat("- Budget Constraint: $5,000,000\n")
cat("- Benefit Types:", paste(benefit_types, collapse = ", "), "\n\n")

# Optimize portfolio
portfolio_optimization <- optimize_benefits_portfolio(
  employee_count = 1000,
  benefit_types = benefit_types,
  budget_constraint = 5000000,
  risk_aversion = 0.001
)

# Display top recommendations
cat("TOP 10 BENEFITS PORTFOLIO OPTIONS:\n")
cat("===================================\n")
top_options <- head(portfolio_optimization$portfolio_options[order(portfolio_optimization$portfolio_options$rank), ], 10)

for (i in 1:nrow(top_options)) {
  option <- top_options[i, ]
  cat(i, ".", option$benefit_type, "(", option$funding_mechanism, ")\n")
  cat("   Expected Cost: $", format(round(option$expected_cost), big.mark = ","), "\n")
  cat("   Risk Premium: $", format(round(option$risk_premium), big.mark = ","), "\n")
  cat("   Utility Score:", round(option$utility_score, 6), "\n\n")
}

# =============================================================================
# DEMONSTRATION 4: COMPARISON WITH TRADITIONAL UTILITY ANALYSIS
# =============================================================================

cat("=== DEMONSTRATION 4: COMPARISON WITH TRADITIONAL UTILITY ANALYSIS ===\n")
cat("Gregory's Risk-Aware Approach vs. Traditional Cost-Benefit Analysis\n\n")

# Traditional approach: Simple cost comparison
traditional_analysis <- function(benefits_params, funding_mechanisms) {
  results <- data.frame(
    funding_mechanism = funding_mechanisms,
    expected_cost = numeric(length(funding_mechanisms)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(funding_mechanisms)) {
    params <- benefits_params
    params$funding_mechanism <- funding_mechanisms[i]
    result <- calculate_benefits_utility(params, risk_aversion = 0.001)
    results$expected_cost[i] <- result$expected_cost
  }
  
  results$rank <- rank(results$expected_cost)  # Lower cost = better rank
  return(results)
}

# Compare approaches
traditional_results <- traditional_analysis(health_insurance_params, funding_mechanisms)
gregory_results <- health_comparison$comparison_summary

cat("TRADITIONAL COST-BASED RANKING:\n")
cat("===============================\n")
for (i in 1:nrow(traditional_results)) {
  option <- traditional_results[i, ]
  cat(option$rank, ".", option$funding_mechanism, 
      " - $", format(round(option$expected_cost), big.mark = ","), "\n")
}

cat("\nGREGORY'S RISK-AWARE RANKING:\n")
cat("==============================\n")
for (i in 1:nrow(gregory_results)) {
  option <- gregory_results[i, ]
  cat(option$rank, ".", option$funding_mechanism, 
      " - $", format(round(option$expected_cost), big.mark = ","), 
      " (Risk Premium: $", format(round(option$risk_premium), big.mark = ","), ")\n")
}

# Identify differences
cat("\nKEY DIFFERENCES:\n")
cat("=================\n")
cat("Traditional approach focuses only on expected costs\n")
cat("Gregory's approach considers both cost and risk preferences\n")
cat("Risk-aware approach may recommend higher-cost, lower-risk options\n")
cat("This is particularly important for risk-averse organizations\n\n")

# =============================================================================
# DEMONSTRATION 5: PRACTICAL IMPLEMENTATION GUIDANCE
# =============================================================================

cat("=== DEMONSTRATION 5: PRACTICAL IMPLEMENTATION GUIDANCE ===\n")
cat("How to apply Gregory's methodology in practice\n\n")

cat("STEP 1: ASSESS ORGANIZATIONAL RISK AVERSION\n")
cat("===========================================\n")
cat("- Use risk tolerance surveys or historical decision patterns\n")
cat("- Consider organization size, financial stability, and industry\n")
cat("- Typical risk aversion ranges: 0.0005 (low) to 0.005 (high)\n\n")

cat("STEP 2: IDENTIFY BENEFITS AND FUNDING OPTIONS\n")
cat("=============================================\n")
cat("- List all employee benefits under consideration\n")
cat("- Identify available funding mechanisms for each benefit\n")
cat("- Gather cost data and variability estimates\n\n")

cat("STEP 3: CALCULATE UTILITY FOR EACH OPTION\n")
cat("=========================================\n")
cat("- Use exponential utility function with organizational risk aversion\n")
cat("- Account for cost variability and funding mechanism risk reduction\n")
cat("- Calculate certainty equivalents and risk premiums\n\n")

cat("STEP 4: COMPARE AND RANK OPTIONS\n")
cat("================================\n")
cat("- Rank options by expected utility (not just expected cost)\n")
cat("- Consider risk sensitivity across different risk aversion levels\n")
cat("- Identify robust choices that perform well across scenarios\n\n")

cat("STEP 5: IMPLEMENT AND MONITOR\n")
cat("=============================\n")
cat("- Implement chosen benefits portfolio\n")
cat("- Monitor actual costs vs. predictions\n")
cat("- Update risk aversion estimates based on experience\n")
cat("- Re-evaluate periodically as conditions change\n\n")

# =============================================================================
# SUMMARY AND INSIGHTS
# =============================================================================

cat("=== SUMMARY: GREGORY (1981) ADAPTATION TO EMPLOYEE BENEFITS ===\n\n")

cat("KEY INSIGHTS FROM GREGORY'S METHODOLOGY:\n")
cat("========================================\n")
cat("1. Risk matters: Organizations should consider risk preferences, not just costs\n")
cat("2. Funding mechanisms differ in risk characteristics, not just cost levels\n")
cat("3. Expected utility theory provides a rigorous framework for risk-aware decisions\n")
cat("4. Risk sensitivity analysis helps identify robust choices across scenarios\n")
cat("5. Certainty equivalents provide intuitive risk-adjusted cost measures\n\n")

cat("PRACTICAL APPLICATIONS:\n")
cat("=======================\n")
cat("- Health insurance funding decisions (fully insured vs. self-funded)\n")
cat("- Retirement plan design (defined benefit vs. defined contribution)\n")
cat("- Benefits portfolio optimization under budget constraints\n")
cat("- Risk management in benefits administration\n")
cat("- Strategic benefits planning for different organizational risk profiles\n\n")

cat("INTEGRATION WITH EXISTING UTILITY ANALYSIS:\n")
cat("===========================================\n")
cat("- Combines with BCG utility analysis for comprehensive HR decision-making\n")
cat("- Incorporates Sturman (2000) economic adjustments for realistic estimates\n")
cat("- Provides risk-aware framework for benefits decisions\n")
cat("- Enables portfolio optimization across multiple HR interventions\n\n")

cat("This framework extends Gregory's pioneering work in healthcare benefits\n")
cat("to provide a comprehensive approach for evaluating employee benefits\n")
cat("that accounts for both expected costs and organizational risk preferences.\n")
