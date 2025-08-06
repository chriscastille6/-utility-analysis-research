# Extensions: Advanced Utility Analysis Applications

This folder contains extensions to core utility analysis findings, demonstrating how recent methodological advances enhance classic case studies.

## Overview

The extensions take established utility analysis cases and apply modern insights from:
- **Sturman (2000)**: Monte Carlo adjustments for realistic operational constraints
- **Joo et al. (2022)**: Star performer recognition in heavy-tailed performance distributions
- **Combined approaches**: Balanced integration of both methodological advances

## Current Extensions

### Latham & Whyte (1994) Comprehensive Analysis

**Files:**
- `latham_whyte_comprehensive_analysis.R` - Main analysis script
- `simple_report.Rmd` - R Markdown source for PDF report
- `simple_report.pdf` - Final comprehensive report
- `latham_whyte_comprehensive_results.RData` - Analysis results data

**Analysis Overview:**

The classic Latham & Whyte budget analyst selection case is re-examined using four methodological approaches:

1. **Traditional Utility Analysis**
   - Uses Brogden-Cronbach-Gleser model
   - SDy = 40% of mean salary
   - Assumes normal performance distribution
   - **Result:** $112,258,934 total utility

2. **Sturman (2000) Monte Carlo Adjustments**
   - Applies 35% reduction for operational realities
   - Accounts for range restriction and validity shrinkage
   - **Result:** $72,968,307 total utility (-35%)

3. **Star Performer Approach (Joo et al. 2022)**
   - Uses SDy = 1.1 × mean salary (2.75× multiplier)
   - Recognizes heavy-tailed performance distributions
   - **Result:** $308,928,370 total utility (+175%)

4. **Combined Realistic Approach**
   - Integrates Sturman caution with star performer recognition
   - Applies 20% reduction to star estimates
   - **Result:** $247,142,696 total utility (+120%)

### Key Insights

**Methodological Evolution:**
- Traditional approaches may overestimate utility by ~35%
- Star performer recognition can increase utility estimates by ~175%
- Combined approaches suggest ~120% improvement over traditional is realistic

**Strategic Implications:**
- All scenarios support substantial investment in selection procedures
- Star performer identification dramatically increases ROI
- Even conservative estimates provide compelling business cases
- ROI ranges from 591× (conservative) to 1,999× (realistic)

**Research Progression:**
- 1940s-1980s: Classical utility analysis foundation
- 2000s: Sturman's realism and operational constraints
- 2020s: Star performer recognition and heavy-tailed distributions

## Usage

### Running the Analysis

```r
# From the extensions directory
source("latham_whyte_comprehensive_analysis.R")

# This will:
# 1. Load required functions from ../scripts/utilities/
# 2. Calculate all four approaches
# 3. Generate summary tables and statistics
# 4. Save results to latham_whyte_comprehensive_results.RData
```

### Generating the Report

```r
# Compile the PDF report
rmarkdown::render("simple_report.Rmd")

# This creates simple_report.pdf with:
# - Executive summary
# - Comparative analysis
# - Visual comparisons
# - Strategic implications
# - Detailed conclusions
```

## Technical Details

### Case Parameters (Latham & Whyte 1994)
- **Position:** Budget Analyst
- **Organization:** Government agency
- **Candidates selected:** 618
- **Total applicants:** 12,360
- **Selection ratio:** 5%
- **Test validity:** 0.76
- **Mean salary:** $29,000 (1980s dollars)
- **Cost per applicant:** $10
- **Time horizon:** 10 years

### SDy Calculations
- **Traditional:** 0.40 × $29,000 = $11,600
- **Star Performer:** 1.1 × $29,000 = $31,900 (2.75× multiplier)

### Adjustment Factors
- **Sturman reduction:** 35% (based on Monte Carlo findings)
- **Combined reduction:** 20% (balanced approach)

## Future Extensions

Potential future extensions could include:

1. **Schmidt & Hunter (1998) Meta-Analysis Updates**
   - Apply updated validity coefficients
   - Modern selection procedure combinations

2. **O'Boyle & Aguinis (2012) Performance Distribution Models**
   - Paretian vs. normal distribution comparisons
   - Different star performer thresholds

3. **Boudreau & Berger (1985) Employee Flow Models**
   - Turnover and promotion effects
   - Multi-period utility analysis

4. **Modern Technology Integration**
   - AI-enhanced selection procedures
   - Validity generalization updates

## Dependencies

- `scripts/utilities/star_performer_functions.R`
- R packages: `dplyr`, `ggplot2`, `knitr`, `rmarkdown`
- LaTeX distribution for PDF generation

## References

- Joo, H., Aguinis, H., & Bradley, K. J. (2022). HRM's role in the financial value of firms obtaining more stars. *The International Journal of Human Resource Management*, 33(1), 4173-4216.
- Latham, G. P., & Whyte, G. (1994). The futility of utility analysis. *Personnel Psychology*, 47(1), 31-46.
- Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. *Journal of Management*, 26(2), 281-299.
- Sturman, M. C., Côté, S., & Mangum, T. W. (2023). Getting more from stars: A commentary on Joo, Aguinis, and Bradley (2022). *The International Journal of Human Resource Management*, 34(14), 2747-2760. 