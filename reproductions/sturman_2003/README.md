# STURMAN ET AL. (2003) REPLICATION STUDY
## "Is it worth it to win the Talent War? Evaluating the Utility of Performance-Based Pay"

### OVERVIEW
This folder contains the complete replication of Sturman et al. (2003)'s utility analysis of performance-based pay strategies using the Boudreau & Berger (1985) employee movement framework.

### KEY FILES

#### Main Replication
- **sturman_2003_replication_report.pdf** - Final comprehensive report
- **sturman_2003_replication_report.Rmd** - Report source code
- **STURMAN_2003_REPLICATION_SUMMARY.md** - Executive summary of findings

#### Core Analysis
- **sturman_2003_replication_analysis.R** - Complete replication code
- **sturman_2003_results.RData** - Analysis results and data

#### Supporting Files
- **references.bib** - Bibliography for citations

### REPLICATION STATUS: ✅ SUCCESSFUL

**Key Achievement**: Successfully validated the Boudreau & Berger (1985) framework application to pay-for-performance strategies, confirming that performance-based pay can be financially attractive when service value is properly considered.

### MAIN FINDINGS

#### Cost Analysis
- **Movement Costs**: Strategy 2 (performance bonuses) had lowest costs ($142M)
- **Service Costs**: Strategy 3 (pure performance-based) had lowest costs ($1.49B)

#### Net Utility Results (90% SDy - Realistic Scenario)
- **Strategy 1** (across-the-board): -$1.127B (baseline)
- **Strategy 2** (performance bonuses): -$1.116B (+$10.5M advantage)
- **Strategy 3** (pure performance): -$1.120B (+$6.7M advantage)

#### Strategic Insights
1. Traditional cost analysis insufficient - would reject all pay-for-performance
2. Performance-based pay becomes attractive when service value considered
3. SDy assumptions critical - results highly sensitive to performance variability
4. Strategy 2 (hybrid approach) most robust across all scenarios

### METHODOLOGY REPLICATED
- **Framework**: Boudreau & Berger (1985) employee movement utility
- **Data Source**: Trevor et al. (1997) turnover probabilities
- **Service Value**: Schmidt & Hunter (1983) approach
- **Analysis Period**: 4 years (2004-2007)
- **SDy Scenarios**: 30%, 60%, 90% of salary

### USAGE
1. Start with STURMAN_2003_REPLICATION_SUMMARY.md for overview
2. Review sturman_2003_replication_report.pdf for complete analysis
3. Run sturman_2003_replication_analysis.R to reproduce results
4. Load sturman_2003_results.RData for detailed data analysis

### CITATIONS
Primary study: Sturman, M. C., Trevor, C. O., Boudreau, J. W., & Gerhart, B. (2003). Is it worth it to win the talent war? Evaluating the utility of performance-based pay. Personnel Psychology, 56(4), 997-1035.
