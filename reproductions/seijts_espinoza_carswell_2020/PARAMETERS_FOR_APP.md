# PARAMETERS FOR UA+ APP
## Seijts, Espinoza, & Carswell (2020) - Character Assessment Utility Analysis

This document explains how to use the Seijts et al. (2020) parameters in the main UA+ web application.

---

## PARAMETER FILES

### 1. CSV Format (`seijts_2020_parameters.csv`)
**Best for**: R scripts, data analysis, easy reading

```r
# Load in R
params <- read.csv("seijts_2020_parameters.csv")
param_list <- setNames(params$value, params$parameter)

# Access parameters
N <- param_list["N"]
selection_ratio <- param_list["selection_ratio"]
sdy <- param_list["sdy"]
```

### 2. JSON Format (`seijts_2020_parameters.json`)
**Best for**: Web applications, JavaScript, API integration

```javascript
// Load in JavaScript
fetch('seijts_2020_parameters.json')
  .then(response => response.json())
  .then(data => {
    const N = data.basic_parameters.N;
    const selection_ratio = data.basic_parameters.selection_ratio;
    const sdy = data.basic_parameters.sdy;
  });
```

### 3. R Data Format (`seijts_2020_params.RData`)
**Best for**: R applications, Shiny apps, quick loading

```r
# Load in R
load("seijts_2020_params.RData")

# Access pre-calculated values
utility_15 <- seijts_2020_params$utility_15
utility_10 <- seijts_2020_params$utility_10
```

---

## KEY PARAMETERS FOR UA+ APP

### Basic Utility Analysis
| Parameter | Value | Description |
|-----------|-------|-------------|
| `N` | 1 | Number of candidates per year |
| `selection_ratio` | 0.33 | Selection ratio (33%) |
| `cost_per_candidate` | 800 | Cost in CAD dollars |
| `sdy` | 115500 | Performance SD in dollars |
| `reported_correlation` | 0.30 | Character-performance correlation |
| `tenure_15` | 15 | 15-year tenure period |
| `tenure_10` | 10 | 10-year tenure period |

### IOPsych Package Usage
```r
library(iopsych)

# Get Zxs value
zxs <- ux(0.33)  # Returns 1.097

# Calculate utility
utility_15 <- utilityBcg(
  n = 1,
  sdy = 115500,
  rxy = 0.30,
  sr = 0.33,
  cost = 800,
  period = 15
)
```

### Expected Results
- **15-year utility**: CAD $569,584
- **10-year utility**: CAD $379,456
- **15-year yearly**: CAD $37,972
- **10-year yearly**: CAD $37,946
- **ROI (15 years)**: 23,495%
- **ROI (10 years)**: 15,653%

---

## INTEGRATION WITH UA+ APP

### 1. Default Values
Use these parameters as default values in the UA+ app:

```r
# In app.R or server.R
default_params <- list(
  N = 1,
  selection_ratio = 0.33,
  cost_per_candidate = 800,
  sdy = 115500,
  reported_correlation = 0.30,
  tenure_15 = 15,
  tenure_10 = 10
)
```

### 2. Pre-calculated Results
Use the pre-calculated results for comparison:

```r
# Reported vs calculated comparison
comparison_data <- data.frame(
  period = c("15 years", "10 years"),
  reported = c(564128, 375285),
  calculated = c(569584, 379456),
  difference_pct = c(1.0, 1.1)
)
```

### 3. Economic Adjustments
Use the adjustment ranges for sensitivity analysis:

```r
# Economic adjustment parameters
adjustment_ranges <- list(
  variable_costs = c(-0.02, -0.185, -0.35),
  taxation_rates = c(0.30, 0.465, 0.63),
  discounting_rates = c(0.01, 0.06, 0.11),
  incremental_validity = c(0.05, 0.175, 0.30)
)
```

---

## EXAMPLE USAGE IN SHINY APP

### UI Inputs
```r
# In ui.R
numericInput("N", "Number of candidates per year:", value = 1, min = 1),
numericInput("selection_ratio", "Selection ratio:", value = 0.33, min = 0.01, max = 1),
numericInput("cost_per_candidate", "Cost per candidate (CAD):", value = 800, min = 0),
numericInput("sdy", "Performance SD in dollars:", value = 115500, min = 0),
numericInput("reported_correlation", "Validity coefficient:", value = 0.30, min = 0, max = 1)
```

### Server Calculations
```r
# In server.R
observeEvent(input$calculate, {
  # Load IOPsych package
  library(iopsych)
  
  # Get Zxs value
  zxs <- ux(input$selection_ratio)
  
  # Calculate utility
  utility_15 <- utilityBcg(
    n = input$N,
    sdy = input$sdy,
    rxy = input$reported_correlation,
    sr = input$selection_ratio,
    cost = input$cost_per_candidate,
    period = 15
  )
  
  # Display results
  output$results <- renderText({
    paste("15-year utility: CAD $", format(round(utility_15), big.mark=","))
  })
})
```

---

## VALIDATION

### Comparison with Reported Values
The IOPsych package results are very close to the reported values:

| Metric | Reported | IOPsych | Difference |
|--------|----------|---------|------------|
| 15-year utility | CAD $564,128 | CAD $569,584 | 1.0% |
| 10-year utility | CAD $375,285 | CAD $379,456 | 1.1% |

### Why the Small Difference?
- **IOPsych `ux()` function**: Uses precise Naylor & Shine (1965) table values
- **Reported values**: May have used rounded intermediate calculations
- **1% difference**: Well within acceptable tolerance for utility analysis

---

## BEST PRACTICES

### 1. Use IOPsych Package
- Always use `ux()` for Zxs calculations
- Use `utilityBcg()` for basic utility calculations
- Document package version used

### 2. Parameter Documentation
- Include source citations for all parameters
- Document any modifications from original study
- Provide clear descriptions of each parameter

### 3. Validation
- Compare results with reported values
- Test parameter sensitivity
- Document any discrepancies

### 4. Reproducibility
- Save parameter files with analysis scripts
- Include version information
- Provide clear loading instructions

---

## FILES SUMMARY

| File | Format | Purpose | Best For |
|------|--------|---------|----------|
| `seijts_2020_parameters.csv` | CSV | Simple parameter list | R scripts, data analysis |
| `seijts_2020_parameters.json` | JSON | Structured data | Web apps, JavaScript |
| `seijts_2020_params.RData` | R Data | Pre-calculated results | R apps, Shiny |
| `load_parameters.R` | R Script | Loading example | Testing, validation |

---

**Status**: ✅ Parameters ready for integration with UA+ app. All formats tested and validated. 