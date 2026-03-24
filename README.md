# Utility Analysis Research

A comprehensive R-based research platform for workforce diversity and productivity analysis using Pareto optimization techniques and Sturman (2000) Monte Carlo utility analysis.

## 🚀 Quick Start

**Shiny Apps:** Launch `app.R` for interactive utility analysis with expectancy charts and Monte Carlo simulation.

**Sturman Monte Carlo:** Run `scripts/scripts/sturman_2000_monte_carlo.R` for specialized utility analysis simulation.

## 📁 Repository Structure

```
📦 Utility Analysis Research/
├── 📄 app.R                           # 🎯 MAIN APPLICATION - Full Shiny app
├── 📄 app_backup.R                    # Backup of main application
├── 📄 scripts/scripts/sturman_2000_monte_carlo.R      # 🎲 Sturman Monte Carlo simulation
├── 📄 Utility Analysis Research.Rproj # R Project file
├── 📄 styles.css                      # App styling
├── 📄 logo.png                        # Project logo
├── 📄 README.md                       # This documentation
│
├── 📁 data/                           # 📊 DATA FILES
│   └── 📁 sturman_reference/          # Sturman reference datasets
│       ├── 📄 sturman_reference_dataset_20250620_200631.csv
│       ├── 📄 sturman_reference_dataset_20250620_200631.rds
│       ├── 📄 sturman_dataset_metadata_20250620_200631.json
│       ├── 📄 sturman_summary_stats_20250620_200631.csv
│       └── 📄 sturman_usefulness_stats_20250620_200631.csv
│
├── 📁 docs/                           # 📚 DOCUMENTATION
│   ├── 📄 FILE_ORGANIZATION_SUMMARY.md # File organization guide
│   ├── 📄 STANDARDIZATION_ACTION_PLAN.md # Standardization roadmap
│   ├── 📄 STURMAN_REPLICATION_SUMMARY.md # Sturman replication analysis
│   ├── 📄 PERFORMANCE_BASED_TURNOVER_MODEL.md # Turnover model docs
│   └── 📄 SIMULATION_UA_LENS_SPEC.md # Simulation UA Lens / tool-generator design spec
│
├── 📁 scripts/                        # 🔧 ANALYSIS SCRIPTS
│   ├── 📁 utilities/                  # Core utility functions
│   │   └── 📄 sturman_utility_functions.R # Standardized utility calculations
│   │
│   ├── 📁 sturman_analysis/           # Sturman (2000) analysis files
│   │   ├── 📄 sturman_general_usefulness.R # General usefulness analysis
│   │   ├── 📄 verify_sturman_results.R # Results verification
│   │   ├── 📄 sturman_analysis_critique.R # Critical analysis
│   │   ├── 📄 sturman_theory_testing.R # Theory testing framework
│   │   ├── 📄 sturman_seed_testing.R # Seed sensitivity analysis
│   │   ├── 📄 sturman_table1_analysis.R # Table 1 parameter analysis
│   │   ├── 📄 sturman_exact_table1.R # Exact Table 1 replication
│   │   ├── 📄 sturman_complete_tables.R # Complete table generation
│   │   ├── 📄 sturman_dual_strategy.R # Dual strategy implementation
│   │   ├── 📄 sturman_comprehensive_test.R # Comprehensive testing
│   │   ├── 📄 final_sturman_implementation.R # Final implementation
│   │   ├── 📄 parameter_optimization.R # Parameter optimization
│   │   └── 📄 verify_sturman_formula.R # Formula verification
│   │
│   ├── 📁 testing/                    # Testing and validation files
│   │   ├── 📄 test_boudreau_exact_formulas.R # Boudreau formula testing
│   │   ├── 📄 test_parameter_correlations.R # Parameter correlation analysis
│   │   ├── 📄 test_all_2decimal_rounding.R # Rounding precision testing
│   │   ├── 📄 test_standardized_vs_current.R # Standardization comparison
│   │   ├── 📄 test_backup_approach.R # Backup approach testing
│   │   ├── 📄 test_lw_adjustments.R # Latham & Whyte adjustments
│   │   ├── 📄 test_parameter_rounding_precision.R # Parameter rounding
│   │   ├── 📄 test_10k_with_rounding.R # 10K simulation with rounding
│   │   ├── 📄 test_ux_rounding.R # UX rounding testing
│   │   ├── 📄 test_seeds_sturman.R # Seed testing
│   │   ├── 📄 test_cumulative_approach.R # Cumulative approach
│   │   ├── 📄 test_monte_carlo.R # Monte Carlo testing
│   │   ├── 📄 create_hybrid_table2.R # Hybrid table creation
│   │   └── 📄 generate_table2.R # Table 2 generation
│   │
│   ├── 📁 data_generation/            # Data processing files
│   │   ├── 📄 generate_reference_dataset.R # Reference dataset generation
│   │   ├── 📄 load_reference_dataset.R # Dataset loading utilities
│   │   ├── 📄 extract_pdfs.R # PDF text extraction
│   │   ├── 📄 extract_tables.R # Table extraction from PDFs
│   │   ├── 📄 extract_tables_simple.R # Simplified table extraction
│   │   └── 📄 integrate_monte_carlo.R # Monte Carlo integration
│   │
│   ├── 📁 features/                   # 🔧 FEATURE ANALYSIS SCRIPTS
│   │   ├── 📄 pareto optimization.R   # 🌟 Main Pareto analysis
│   │   ├── 📄 what does this mean v2.0.R # Advanced analysis interpretation
│   │   ├── 📄 what does this mean.R   # Basic analysis interpretation
│   │   ├── 📄 causal diagram.R        # Causal relationship modeling
│   │   ├── 📄 expectancy_chart_advanced.R # Advanced expectancy tables
│   │   ├── 📄 expectancy_chart_module.R # Modular expectancy functions
│   │   ├── 📄 expectancy chart cucina.R # Cucina-based expectancy analysis
│   │   ├── 📄 expectancy_chart.R      # Standard expectancy charts
│   │   ├── 📄 test_utilityB.R         # Utility function testing
│   │   ├── 📄 test_app.R              # App functionality testing
│   │   ├── 📄 utility_calculation.R   # Core utility calculations
│   │   ├── 📄 Sturman (2000)_8_23.R   # Sturman research implementation
│   │   ├── 📄 Shiny App.R             # Shiny app development
│   │   └── 📄 Sturman (2000).R        # Original Sturman analysis
│   │
│   └── 📄 [Other legacy scripts]      # Original analysis scripts
│
├── 📁 manuscripts/                    # 📝 ACADEMIC MANUSCRIPTS & PROPOSALS
│   └── 📄 SIOP Proposal '24.docx      # 2024 SIOP Conference proposal
│
├── 📁 versions/                       # 📱 APP VERSION HISTORY
│   ├── 📄 app_2023-09-25.R           # App version from September 2023
│   ├── 📄 app_online_2024-05-19.R    # Online deployment version
│   └── 📄 app_v03_2024-06-17.R       # Version 3 (June 2024)
│
├── 📁 reports/                        # 📊 REPORTS & DOCUMENTATION
│   ├── 📄 expectancy_report.Rmd       # Expectancy analysis report
│   ├── 📄 app_utility_report.Rmd      # App utility documentation
│   ├── 📄 app_expectancy_report.Rmd   # App expectancy features
│   ├── 📄 utility_report.Rmd          # Utility analysis report
│   └── 📄 header.tex                  # LaTeX header for reports
│
├── 📁 articles/                       # 📚 RESEARCH PAPERS
│   └── 📁 extracted_text/             # Extracted text from PDFs
│
├── 📁 plots/                          # 📈 GENERATED PLOTS & FIGURES
│   ├── 📄 Fig1.png                    # Figure 1
│   ├── 📄 Fig2.png                    # Figure 2
│   ├── 📄 expectancy_chart.png        # Expectancy visualization
│   └── 📄 Rplots.pdf                  # R-generated plots
│
├── 📁 www/                            # 🌐 WEB ASSETS
│   └── 📄 logo.png                    # Web logo
│
└── 📁 logs/                           # 📝 SYSTEM LOGS
    └── 📄 [Various log files]
```

## 🔑 Key Features

### 🎯 Main Applications

**Primary App (`app.R`):**
- Complete Shiny web application
- Interactive workforce diversity analysis
- Real-time Pareto optimization
- Expectancy table generation
- Utility analysis tools

**Sturman Monte Carlo (`scripts/scripts/sturman_2000_monte_carlo.R`):**
- Specialized utility analysis simulation
- Replication of Sturman (2000) methodology
- Monte Carlo parameter sensitivity analysis
- Economic adjustment modeling

### 🧮 Sturman Analysis Suite (`scripts/sturman_analysis/`)

**Core Analysis:**
- `sturman_general_usefulness.R` - General usefulness analysis implementation
- `verify_sturman_results.R` - Results verification against Sturman targets
- `sturman_theory_testing.R` - Comprehensive theory testing framework

**Advanced Analysis:**
- `sturman_analysis_critique.R` - Critical analysis of methodology
- `parameter_optimization.R` - Parameter optimization techniques
- `sturman_comprehensive_test.R` - Full testing suite

### 🔬 Testing Framework (`scripts/testing/`)

**Validation Testing:**
- `test_boudreau_exact_formulas.R` - Boudreau formula accuracy testing
- `test_parameter_correlations.R` - Parameter correlation impact analysis
- `test_all_2decimal_rounding.R` - Rounding precision validation

**Implementation Testing:**
- `test_standardized_vs_current.R` - Standardization comparison
- `test_backup_approach.R` - Alternative approach validation

### 🌟 Feature Scripts (`scripts/features/`)

**Pareto Optimization:**
- `pareto optimization.R` - **Main analysis script with dual matrix support**
  - Roth et al. (2011) - 5 predictors
  - Berry et al. (2024) - 6 predictors including SJT
  - Easy matrix switching
  - Comprehensive workforce evolution modeling

**Expectancy Analysis:**
- `expectancy_chart_advanced.R` - Advanced expectancy tables
- `expectancy_chart.R` - Standard expectancy visualizations
- `expectancy chart cucina.R` - Cucina methodology implementation

### 📊 Data Management (`data/`)

**Sturman Reference Data:**
- Complete 10,000-sample Monte Carlo datasets
- Metadata and summary statistics
- Both CSV and RDS formats for flexibility

### 📚 Documentation (`docs/`)

**Implementation Guides:**
- `FILE_ORGANIZATION_SUMMARY.md` - Complete file organization guide
- `STANDARDIZATION_ACTION_PLAN.md` - Standardization roadmap
- `STURMAN_REPLICATION_SUMMARY.md` - Detailed replication analysis

## 🛠 Technical Requirements

- R 4.0+
- Required packages: `ParetoR`, `shiny`, `ggplot2`, `dplyr`, `psych`, `lavaan`, `MASS`, `mvtnorm`, `ggrepel`, `iopsych`, `shinydashboard`, `DT`, `shinyjs`

## 📚 Research Foundation

This platform implements methods from:
- **Sturman (2000)** - Monte Carlo utility analysis framework
- **Boudreau (1983)** - Economic utility adjustments
- **Roth et al. (2011)** - Selection procedure correlations
- **Berry et al. (2024)** - Updated meta-analytic correlations with SJT
- **De Corte et al. (1994)** - Pareto optimization in selection

## 🎯 Usage

### Quick Start Options:

1. **Sturman Analysis:** Run `scripts/scripts/sturman_2000_monte_carlo.R` for utility analysis
2. **Full Platform:** Launch `app.R` for complete interactive analysis
3. **Specific Testing:** Run individual scripts in `scripts/testing/`
4. **Feature Analysis:** Execute scripts in `scripts/features/`

### Working with Organized Structure:

- **All utility functions** are centralized in `scripts/utilities/`
- **Analysis scripts** automatically source the correct utility functions
- **Data files** are organized in `data/` with clear naming conventions
- **Documentation** is centralized in `docs/` for easy reference

## 📈 Output

- Interactive visualizations
- Pareto-optimal hiring strategies
- Workforce evolution projections
- Cost-benefit analyses
- Monte Carlo simulation results
- Publication-ready plots (saved to `plots/`)

## 🔄 File Organization Benefits

- **Logical grouping** of related functionality
- **Easy navigation** with clear directory structure
- **Preserved functionality** - all scripts work as before
- **Scalable structure** for future development
- **Clear separation** of data, code, documentation, and output 