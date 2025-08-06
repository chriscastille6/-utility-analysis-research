# Update Existing Pareto Optimization with Berry et al. (2024) Findings
# This script provides the updated correlation matrix and parameters to replace
# the Roth et al. (2011) values in our existing Pareto optimization code

# Berry et al. (2024) Updated Meta-Analytic Correlation Matrix
berry_2024_matrix <- matrix(c(
  1.00, 0.13, 0.54, 0.21, 0.25, 0.42,
  0.13, 1.00, 0.03, 0.18, 0.01, 0.29,
  0.54, 0.03, 1.00, 0.08, 0.28, 0.23,
  0.21, 0.18, 0.08, 1.00, -0.02, 0.45,
  0.25, 0.01, 0.28, -0.02, 1.00, 0.16,
  0.42, 0.29, 0.23, 0.45, 0.16, 1.00
), nrow=6, byrow=TRUE)

# Variable names
var_names <- c("Biodata", "GMA", "Conscientiousness", "Structured_Interview", "Integrity", "SJT")
colnames(berry_2024_matrix) <- rownames(berry_2024_matrix) <- var_names

# Updated validities (Berry et al., 2024)
berry_2024_validities <- c(0.38, 0.31, 0.19, 0.42, 0.31, 0.26)
names(berry_2024_validities) <- var_names

# Updated Black-White d-values (Berry et al., 2024)
berry_2024_d_values <- c(0.32, 0.79, -0.07, 0.24, 0.10, 0.37)
names(berry_2024_d_values) <- var_names

# Function to generate updated Pareto optimization code
generate_updated_code <- function() {
  cat("# UPDATED PARETO OPTIMIZATION CODE USING BERRY ET AL. (2024) MATRIX\n\n")
  
  cat("# Berry et al. (2024) correlation matrix\n")
  cat("berry_cor_matrix <- matrix(c(\n")
  for(i in 1:nrow(berry_2024_matrix)) {
    cat("  ", paste(berry_2024_matrix[i,], collapse = ", "), ",\n")
  }
  cat("), nrow=6, byrow=TRUE)\n\n")
  
  cat("# Variable names\n")
  cat("var_names <- c(", paste(paste0('"', var_names, '"'), collapse = ", "), ")\n")
  cat("colnames(berry_cor_matrix) <- rownames(berry_cor_matrix) <- var_names\n\n")
  
  cat("# Updated validities (Berry et al., 2024)\n")
  cat("validities <- c(", paste(berry_2024_validities, collapse = ", "), ")\n")
  cat("names(validities) <- var_names\n\n")
  
  cat("# Updated Black-White d-values (Berry et al., 2024)\n")
  cat("d_values <- c(", paste(berry_2024_d_values, collapse = ", "), ")\n")
  cat("names(d_values) <- var_names\n\n")
  
  cat("# Key changes from Roth et al. (2011):\n")
  cat("# - GMA validity: .52 -> .31\n")
  cat("# - Structured Interview validity: .48 -> .42\n")
  cat("# - Biodata validity: .32 -> .38\n")
  cat("# - Added SJT with validity .26\n")
  cat("# - Updated all intercorrelations\n")
  cat("# - Updated Black-White d-values\n\n")
}

# Function to compare with Roth et al. (2011)
compare_matrices <- function() {
  cat("=== COMPARISON: BERRY ET AL. (2024) vs ROTH ET AL. (2011) ===\n\n")
  
  # Roth et al. (2011) values (approximate)
  roth_validities <- c(0.32, 0.52, 0.22, 0.48, 0.42, NA)
  roth_d_values <- c(0.57, 0.72, 0.06, 0.32, 0.04, NA)
  
  comparison <- data.frame(
    Method = var_names,
    Berry_Validity = berry_2024_validities,
    Roth_Validity = roth_validities,
    Validity_Change = berry_2024_validities - roth_validities,
    Berry_d = berry_2024_d_values,
    Roth_d = roth_d_values,
    d_Change = berry_2024_d_values - roth_d_values
  )
  
  print(comparison)
  
  cat("\nKey Changes:\n")
  cat("1. GMA validity reduced by .21 (from .52 to .31)\n")
  cat("2. Structured Interview validity reduced by .06 (from .48 to .42)\n")
  cat("3. Biodata validity increased by .06 (from .32 to .38)\n")
  cat("4. SJT added with validity .26\n")
  cat("5. GMA d-value increased by .07 (from .72 to .79)\n")
}

# Function to provide recommendations
provide_recommendations <- function() {
  cat("\n=== RECOMMENDATIONS FOR UPDATING EXISTING CODE ===\n\n")
  
  cat("1. REPLACE CORRELATION MATRIX:\n")
  cat("   - Update all Pareto optimization scripts\n")
  cat("   - Replace Roth et al. (2011) matrix with Berry et al. (2024)\n")
  cat("   - Update variable names to include SJT\n\n")
  
  cat("2. UPDATE VALIDITY COEFFICIENTS:\n")
  cat("   - GMA: .52 -> .31 (40% reduction)\n")
  cat("   - Structured Interview: .48 -> .42 (13% reduction)\n")
  cat("   - Biodata: .32 -> .38 (19% increase)\n")
  cat("   - Add SJT: .26\n\n")
  
  cat("3. UPDATE DIVERSITY TARGETS:\n")
  cat("   - Higher d-values may require adjusted diversity goals\n")
  cat("   - Consider GMA exclusion as viable strategy\n")
  cat("   - Focus on structured interviews and biodata\n\n")
  
  cat("4. REVISE UTILITY CALCULATIONS:\n")
  cat("   - Update all utility analysis with new validities\n")
  cat("   - Recalculate expected returns on selection investments\n")
  cat("   - Adjust cost-benefit analyses\n\n")
  
  cat("5. UPDATE DOCUMENTATION:\n")
  cat("   - Revise all references to Roth et al. (2011)\n")
  cat("   - Update validity expectations in reports\n")
  cat("   - Modify training materials and examples\n\n")
}

# Run the functions
cat("Berry et al. (2024) Pareto Optimization Update Script\n")
cat("=====================================================\n\n")

generate_updated_code()
compare_matrices()
provide_recommendations()

# Save the updated matrix and parameters
save(berry_2024_matrix, berry_2024_validities, berry_2024_d_values, var_names,
     file = "berry_2024_updated_parameters.RData")

cat("\n=== UPDATED PARAMETERS SAVED ===\n")
cat("File: berry_2024_updated_parameters.RData\n")
cat("Contains: berry_2024_matrix, berry_2024_validities, berry_2024_d_values, var_names\n\n")

cat("=== NEXT STEPS ===\n")
cat("1. Review the generated code above\n")
cat("2. Update your existing Pareto optimization scripts\n")
cat("3. Test with the new parameters\n")
cat("4. Update documentation and training materials\n")
cat("5. Consider the implications for your utility analysis work\n") 