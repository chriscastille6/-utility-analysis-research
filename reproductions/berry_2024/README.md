# Berry et al. (2024) Reproducibility Project

## Study Overview

**Citation:** Berry, C. M., Lievens, F., Zhang, C., & Sackett, P. R. (2024). Insights from an updated personnel selection meta-analytic matrix: Revisiting general mental ability tests' role in the validity–diversity trade-off. *Journal of Applied Psychology, 109*(10), 1611-1634.

**Key Findings:**
- GMA tests are no longer the strongest predictor of job performance (validity reduced from .52 to .31)
- Excluding GMA tests from selection batteries has minimal impact on validity
- The validity-diversity trade-off is less severe than previously thought
- Structured interviews and biodata emerge as the strongest predictors

## Replication Goals

This project aims to reproduce key insights from Berry et al. (2024) using our existing Pareto optimization codebase:

1. **Reproduce the updated meta-analytic correlation matrix** (Table 1)
2. **Verify the dominance analysis results** (Table 2) 
3. **Test the multiple correlation comparisons** (Table 3)
4. **Implement Pareto optimization analyses** to verify the validity-diversity trade-off findings
5. **Compare results with our existing Pareto optimization implementations**

## Key Tables to Reproduce

- **Table 1:** Updated meta-analytic correlation matrix with validities and Black-White d-values
- **Table 2:** Dominance analysis comparing bivariate validities to multiple regression results
- **Table 3:** Multiple correlations for all possible predictor combinations
- **Tables 4-8:** Pareto optimization results for validity-diversity trade-offs

## Methodology

The study uses:
- Updated meta-analytic correlation matrix based on Sackett et al. (2022) validity estimates
- Dominance analysis to assess relative importance of predictors
- Pareto optimization to examine validity-diversity trade-offs
- Multiple regression analyses for all possible predictor combinations

## Files Structure

- `berry_2024_analysis.R` - Main analysis script
- `berry_2024_report.Rmd` - R Markdown report
- `data/` - Data files and correlation matrices
- `results/` - Output files and results
- `figures/` - Generated plots and visualizations

## Dependencies

- R packages: `psych`, `lavaan`, `MASS`, `ggplot2`, `ParetoR`, `dominanceanalysis`
- Existing Pareto optimization code from the main codebase 