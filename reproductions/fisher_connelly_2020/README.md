# Fisher & Connelly (2021) Reproducibility Project
## Utility of Workers with Disabilities

### Citation
Fisher, S., & Connelly, C. E. (2020). Building the “Business Case” for Hiring People with Disabilities: A Financial Cost-Benefit Analysis Methodology and Example. Canadian Journal of Disability Studies, 9(4), 71–88. https://doi.org/10.15353/cjds.v9i4.669

### Overview
This project reproduces Fisher & Connelly (2021) on the economic utility implications of employing workers with disabilities. We will rebuild the model, replicate key results, and test sensitivity to core assumptions.

### Research Questions
- What is the net utility impact of hiring workers with disabilities under various accommodations and productivity assumptions?
- How do accommodation costs, turnover, absenteeism, and productivity distributions affect ROI?
- Under which conditions do inclusive hiring strategies yield positive economic returns?

### Methods (planned)
- Utility modeling consistent with Brogden–Cronbach–Gleser and economic adjustments where applicable
- Parameterization from paper; sensitivity analyses across stated ranges
- Scenario analysis with accommodation cost tiers and productivity assumptions

### Files
- `fisher_connelly_2021_analysis.R` — main analysis
- `fisher_connelly_2021_report.Rmd` — reproducibility report
- `data/` — digitized inputs
- `figures/` — plots
- `references.bib` — citations

### Data Sources
- PDF text extracted to: `articles/extracted_text/2021; Fisher & Connelly, Utility of disabled workers.txt`
- Original PDF: `articles/2021; Fisher & Connelly, Utility of disabled workers.pdf`

### Status
- [x] Project scaffolding
- [ ] Extract and encode parameters from paper
- [ ] Implement baseline utility model
- [ ] Reproduce key tables/figures
- [ ] Sensitivity analysis
- [ ] Report and summary

### Next Steps
1. Extract parameter values from the paper and digitize any tables
2. Implement baseline model reflecting accommodation costs and productivity assumptions
3. Run sensitivity analyses (accommodation cost levels, turnover, productivity)
4. Recreate tables/figures and validate claims
5. Finalize report and document limitations

---
Last updated: `r Sys.Date()` 