# AGENTS.md

## Cursor Cloud specific instructions

### Project overview

This is an R-based research platform for Utility Analysis in I/O Psychology. It consists of multiple standalone Shiny web apps and R analysis scripts. There is no package manager lockfile; dependencies are R packages installed via `install.packages()`.

### Running Shiny apps

Individual apps are launched with `shiny::runApp()`. The main apps are:

| App | Command |
|-----|---------|
| Job Crafting Utility | `Rscript -e 'shiny::runApp("shiny_apps/job_crafting/app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Training Utility | `Rscript -e 'shiny::runApp("training_utility_app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Staffing Utility (fixed) | `Rscript -e 'shiny::runApp("staffing_utility_app_fixed.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Fisher-Connelly 2017 | `Rscript -e 'shiny::runApp("fisher_connelly_2017_app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Fisher-Connelly 2020 | `Rscript -e 'shiny::runApp("fisher_connelly_2020_app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Sturman Monte Carlo | `Rscript -e 'shiny::runApp("scripts/sturman_2000_monte_carlo.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |

### Known issues

- The main `app.R` (and `app_backup.R`) contain duplicate code sections that cause R parse errors. The standalone apps in `shiny_apps/` and root-level `*_app.R` files work correctly.

### Running analysis scripts

Analysis scripts in `scripts/` and `reproductions/` are standalone R scripts executed with `Rscript`:

```
Rscript scripts/utilities/sturman_utility_functions.R
Rscript reproductions/sturman_2000/sturman_usefulness_walkthrough.R
```

The `scripts/utilities/sturman_utility_functions.R` file includes a `validate_implementation()` function that can be used as a basic test.

### System dependencies for R packages

The following system libraries are required for compiling R packages from source:
`libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libfontconfig1-dev`, `libharfbuzz-dev`, `libfribidi-dev`, `libfreetype6-dev`, `libpng-dev`, `libtiff5-dev`, `libjpeg-dev`, `librsvg2-dev`, `libv8-dev`, `libpoppler-cpp-dev`, `cmake`, `pandoc`.

### No formal lint/test framework

This project does not use a formal R linting tool (e.g., `lintr`) or testing framework (e.g., `testthat`). Code validation is done by sourcing scripts and running Shiny apps manually.
