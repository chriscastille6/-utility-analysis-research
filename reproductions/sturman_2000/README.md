# STURMAN (2000) REPLICATION STUDY
## "Implications of utility analysis adjustments for estimates of human resource intervention value"

### OVERVIEW
This folder contains the complete replication of Sturman (2000)'s Monte Carlo simulation study examining the usefulness of utility analysis adjustments.

### KEY FILES

#### Main Replication
- **sturman_2000_replication_report.pdf** - Final comprehensive report
- **sturman_2000_replication_report.Rmd** - Report source code
- **STURMAN_REPLICATION_SUMMARY.md** - Executive summary of findings

#### Core Analysis Files  
- **final_comprehensive_analysis.R** - Main replication analysis
- **save_final_results.R** - Results compilation and export
- **median_vs_mean_analysis.R** - Statistical methodology validation

#### Specialized Analyses
- **amplification_factor_analysis.R** - Effect size amplification testing
- **effect_size_clarification.R** - Parameter range optimization
- **logarithmic_effect_size_test.R** - Distribution methodology testing

#### Methodology Testing
- **test_sturman_usefulness_method.R** - Usefulness analysis validation
- **test_sturman_table2_values.R** - Table 2 replication attempt
- **sturman_usefulness_walkthrough.R** - Step-by-step methodology

#### Data Files
- **sturman_monte_carlo_dataset_final.csv/.rds** - Final simulation results
- **sturman_replication_comparison_final.csv** - Comparison with original
- **sturman_summary_statistics_final.csv** - Summary statistics
- **sturman_usefulness_analysis_final.csv** - Usefulness rankings

#### Supporting Files
- **replication_metadata_final.json** - Analysis metadata
- **references.bib** - Bibliography for citations

### REPLICATION STATUS: ✅ SUCCESSFUL

**Key Achievement**: Successfully validated Sturman's usefulness analysis methodology while identifying important methodological insights about parameter ranges and effect size calculations.

### MAIN FINDINGS
1. **Usefulness Rankings Confirmed**: Economic adjustments > Multiple devices > Other adjustments
2. **Methodology Validated**: Darlington (1968) multiple regression approach successfully applied
3. **Parameter Sensitivity**: Results highly dependent on parameter range assumptions
4. **Effect Size Insights**: Median vs mean considerations important for skewed distributions

### USAGE
1. Start with STURMAN_REPLICATION_SUMMARY.md for overview
2. Review sturman_2000_replication_report.pdf for complete analysis
3. Run final_comprehensive_analysis.R to reproduce main results
4. Explore specialized analyses for methodological insights

### CITATIONS
Primary study: Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. Journal of Management, 26(2), 281-299.
