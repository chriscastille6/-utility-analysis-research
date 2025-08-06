# File Organization Summary

## Overview
The project files have been organized into a logical directory structure to improve maintainability and navigation. All functionality has been preserved with updated file paths.

## Directory Structure

### Root Directory
**Main Application Files:**
- `app.R` - Main Shiny application
- `app_backup.R` - Backup of main application
- `sturman_2000_monte_carlo.R` - Core Monte Carlo simulation module

**Data Files:**
- `sturman_reference_dataset_20250620_200631.csv` - Reference dataset (CSV format)
- `sturman_reference_dataset_20250620_200631.rds` - Reference dataset (RDS format)
- `sturman_dataset_metadata_20250620_200631.json` - Dataset metadata
- `sturman_summary_stats_20250620_200631.csv` - Summary statistics
- `sturman_usefulness_stats_20250620_200631.csv` - Usefulness analysis statistics

**Documentation:**
- `README.md` - Project overview and instructions
- `STANDARDIZATION_ACTION_PLAN.md` - Standardization roadmap
- `STURMAN_REPLICATION_SUMMARY.md` - Replication analysis summary
- `PERFORMANCE_BASED_TURNOVER_MODEL.md` - Turnover model documentation

### `/scripts/` Directory

#### `/scripts/utilities/`
**Core Utility Functions:**
- `sturman_utility_functions.R` - Standardized utility calculation functions

#### `/scripts/sturman_analysis/`
**Sturman (2000) Analysis Files:**
- `sturman_general_usefulness.R` - General usefulness analysis implementation
- `verify_sturman_results.R` - Results verification against Sturman targets
- `sturman_analysis_critique.R` - Critical analysis of Sturman methodology
- `sturman_theory_testing.R` - Theory testing framework
- `sturman_seed_testing.R` - Seed sensitivity analysis
- `sturman_table1_analysis.R` - Table 1 parameter analysis
- `sturman_exact_table1.R` - Exact Table 1 replication
- `sturman_complete_tables.R` - Complete table generation
- `sturman_dual_strategy.R` - Dual strategy implementation
- `sturman_comprehensive_test.R` - Comprehensive testing suite
- `final_sturman_implementation.R` - Final implementation version
- `parameter_optimization.R` - Parameter optimization analysis
- `verify_sturman_formula.R` - Formula verification

#### `/scripts/testing/`
**Testing and Validation Files:**
- `test_boudreau_exact_formulas.R` - Boudreau formula testing
- `test_parameter_correlations.R` - Parameter correlation analysis
- `test_all_2decimal_rounding.R` - Rounding precision testing
- `test_standardized_vs_current.R` - Standardization comparison
- `test_backup_approach.R` - Backup approach testing
- `test_lw_adjustments.R` - Latham & Whyte adjustments testing
- `test_parameter_rounding_precision.R` - Parameter rounding precision
- `test_10k_with_rounding.R` - 10K simulation with rounding
- `test_ux_rounding.R` - UX rounding testing
- `test_seeds_sturman.R` - Seed testing for Sturman analysis
- `test_cumulative_approach.R` - Cumulative approach testing
- `test_monte_carlo.R` - Monte Carlo testing
- `create_hybrid_table2.R` - Hybrid table creation
- `generate_table2.R` - Table 2 generation

#### `/scripts/data_generation/`
**Data Generation and Processing:**
- `generate_reference_dataset.R` - Reference dataset generation
- `load_reference_dataset.R` - Dataset loading utilities
- `extract_pdfs.R` - PDF text extraction
- `extract_tables.R` - Table extraction from PDFs
- `extract_tables_simple.R` - Simplified table extraction
- `integrate_monte_carlo.R` - Monte Carlo integration

#### `/scripts/features/`
**Feature Development and Experimental Code:**
- Various experimental and feature development files (existing structure preserved)

### Other Directories
- `/articles/` - Research papers and extracted text
- `/reports/` - Generated reports and documentation
- `/versions/` - Version history and backups
- `/plots/` - Generated visualizations
- `/logs/` - Application logs
- `/www/` - Web assets (logos, CSS)
- `/manuscripts/` - Academic manuscripts

## Path Updates
All source statements have been updated to reflect the new file locations:

- Files in `/scripts/testing/` use: `source("../utilities/sturman_utility_functions.R")`
- Files in `/scripts/sturman_analysis/` use: `source("../utilities/sturman_utility_functions.R")`
- Files in root directory use: `source("scripts/utilities/sturman_utility_functions.R")`

## Benefits of New Organization

1. **Logical Grouping**: Related files are grouped together by function
2. **Easier Navigation**: Clear directory structure makes finding files intuitive
3. **Maintainability**: Easier to maintain and update related functionality
4. **Scalability**: Structure supports future expansion
5. **Clarity**: Purpose of each file is clear from its location

## Functionality Verification
✅ All file paths have been updated and tested
✅ Main application (`sturman_2000_monte_carlo.R`) loads successfully
✅ Testing files execute correctly from their new locations
✅ All source dependencies are properly linked

## Usage Notes
- Main application files remain in root directory for easy access
- Use relative paths when sourcing files to maintain portability
- All functionality preserved - no breaking changes introduced 