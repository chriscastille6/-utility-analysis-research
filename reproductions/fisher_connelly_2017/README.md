# Fisher & Connelly (2017) Reproducibility Project
## Utility of Contingent (Part-Time) Workers

### Citation
Fisher, S. L., & Connelly, C. E. (2017). Lower Cost or Just Lower Value? Modeling the Organizational Costs and Benefits of Contingent Work. Academy of Management Discoveries, 3(2), 165–186. https://doi.org/10.5465/amd.2015.0119

### Overview
This project reproduces Fisher & Connelly (2017) on the economic utility of contingent/part-time workers. We will rebuild their analytic framework, recreate key tables/figures, and validate core quantitative claims.

### Research Questions
- What is the net utility of contingent workers under varying assumptions (wages, productivity, turnover, benefits)?
- How do key parameters (e.g., hours, benefits eligibility, productivity differentials) affect ROI vs. full-time employees?
- Under what organizational conditions are contingent workers economically advantageous?

### Methods (planned)
- Brogden–Cronbach–Gleser style utility modeling adapted to contingent vs. full-time contexts
- Parameterization from article text/tables; sensitivity analysis across plausible ranges
- Scenario analysis: baseline, optimistic, conservative; sector-specific if reported

### Files
- `fisher_connelly_2017_analysis.R` — main analysis
- `fisher_connelly_2017_report.Rmd` — reproducibility report
- `data/` — any digitized tables/inputs
- `figures/` — generated plots
- `references.bib` — citations

### Data Sources
- PDF text extracted to: `articles/extracted_text/2017; Fisher & Connelly, utility of contingents.txt`
- Original PDF: `articles/2017; Fisher & Connelly, utility of contingents.pdf`

### Status
- [x] Project scaffolding
- [ ] Extract and encode parameter values from paper
- [ ] Implement baseline utility model
- [ ] Reproduce key tables/figures
- [ ] Sensitivity analysis
- [ ] Report and summary

### Next Steps
1. Parse the extracted text for parameter values; verify against any tables/appendix
2. Implement baseline utility comparison (contingent vs full-time)
3. Add sensitivity analyses (productivity differential, benefit costs, turnover)
4. Recreate figures/tables; verify numeric claims
5. Write report and document assumptions

### Notes
- Handle file names with spaces/semicolons carefully in R (use quoted paths)
- If the paper defines distinct worker categories (e.g., independent contractors vs agency temps), model separately

---
Last updated: `r Sys.Date()` 