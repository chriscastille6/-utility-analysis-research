# How De Corte's Model Accounts for Probationary Periods

## The Core Innovation

De Corte's (1994) model introduces **probationary periods** into utility analysis, recognizing that not all selected employees will be retained. This fundamentally changes how we calculate the economic value of selection methods.

## The Two-Stage Process

### Stage 1: Selection
- **N employees are hired** from n applicants using a predictor
- **All N employees** contribute to utility during the probationary period
- **Training costs** are incurred for all hires

### Stage 2: Retention
- **After probation**, only employees who meet performance standards are retained
- **S_p × N employees** pass probation and continue contributing to utility
- **(1 - S_p) × N employees** fail probation and are dismissed

## Key Mathematical Components

### 1. Success Ratio (S_p)
- **Definition**: Proportion of selectees who pass probation
- **Example**: S_p = 0.853 means 85.3% of selected employees are retained
- **Impact**: Determines how many employees contribute to long-term utility

### 2. Conditional Performance Expectations
- **μ_y(x_c)**: Average performance of all selectees during probation
- **μ_y(x_c,r_c)**: Average performance of employees who pass probation
- **μ_y(r_c)**: Average performance if selection were random

### 3. Time Period Structure
- **Period 1**: All N selectees contribute (probationary period)
- **Periods 2-T**: Only S_p × N retained employees contribute

## The Utility Formula

$$U_p = N \times (\mu_y(x_c) - \mu_s) + (T-1) \times N \times S_p \times (\mu_y(x_c,r_c) - \mu_s) - N \times C_t - n \times C_p$$

### Breaking It Down:

1. **Initial Period Utility**: $N \times (\mu_y(x_c) - \mu_s)$
   - All selectees during probation

2. **Retention Period Utility**: $(T-1) \times N \times S_p \times (\mu_y(x_c,r_c) - \mu_s)$
   - Only successful employees over remaining periods

3. **Costs**: $-N \times C_t - n \times C_p$
   - Training costs for all hires
   - Predictor costs for all applicants

## Why This Matters

### 1. Realistic Cost Accounting
- **Training costs** are incurred for employees who don't stay
- **Separation costs** reduce overall utility
- **Recruitment costs** vary by selection method

### 2. Retention Uncertainty
- **Not all selected employees contribute equally** to long-term value
- **Success ratios significantly impact** overall utility
- **Predictor validity affects both selection and retention**

### 3. Time Horizon Effects
- **Longer time horizons** increase the value of retention
- **The (T-1) multiplier** amplifies retention effects
- **Organizations with longer tenures** benefit more from good selection

## Example from De Corte (1994)

| Metric | Value |
|--------|-------|
| Employees Hired | 17 |
| Success Ratio | 85.3% |
| Employees Retained | 14.5 |
| Utility with Predictor | $983,201 |
| Utility with Random Selection | $523,635 |
| **Utility Difference** | **$459,566** |

## Key Insights

### 1. Probationary Periods Reduce Utility
- Training costs for employees who don't stay
- Success ratios significantly impact overall utility
- Not all selected employees contribute to long-term value

### 2. Predictor Validity Matters More
- Higher validity improves both selection and retention
- Small improvements in validity can have large utility impacts
- The correlation between predictor and performance affects retention rates

### 3. Economic Realism
- The model accounts for real organizational costs
- Separation costs reduce utility differences
- Recruitment costs vary by selection method

## Practical Implications

### For Organizations
- **When to use probationary periods**: High training costs, uncertain performance
- **How long probation should be**: Balance between information gain and costs
- **Selection method choice**: Consider both selection and retention effects

### For Utility Analysis
- **Include probationary periods** in utility calculations
- **Account for separation costs** realistically
- **Model retention probabilities** based on predictor validity
- **Consider time horizons** in utility estimates

## Comparison with Traditional Models

### Traditional Brogden Model
- Assumes all selected employees remain on the job
- No separation costs
- No retention uncertainty

### De Corte's Enhanced Model
- Realistic probationary periods
- Separation cost accounting
- Retention probability modeling
- Two-stage decision framework

## Conclusion

De Corte's model provides a **realistic framework** for utility analysis that accounts for the common organizational practice of probationary periods. The key insight is that **probationary periods fundamentally change the economics of selection** by introducing retention uncertainty and requiring consideration of both selection and retention effects in utility calculations.

**Key Takeaway**: Probationary periods don't just add complexity—they fundamentally change the utility calculus by introducing retention uncertainty and requiring consideration of both selection accuracy and retention probability in decision-making.

---

**Visualizations Created:**
- Two-stage decision process
- Utility components over time
- Success ratio comparison
- Utility breakdown by component
- Conditional performance expectations

**Files:**
- `probationary_period_explanation.Rmd` - Detailed explanation
- `probationary_period_visualization.R` - Visualization script
- `probationary_concepts_summary.csv` - Key concepts table 