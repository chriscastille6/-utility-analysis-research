# Compensation Analysis Lab

An interactive educational web application designed to help students learn market data analysis for compensation decisions. This tool demonstrates how HR professionals use compensation survey data to make informed pay decisions through survey weighting, aging factors, and pay policy implementation.

## 🎯 Learning Objectives

Students will learn to:
- **Weight compensation surveys** based on quality metrics and sample sizes
- **Apply aging factors** to account for data currency and salary inflation
- **Analyze pay policy impacts** (lead, match, lag strategies) on compensation costs
- **Create competitive analysis** and salary structures based on market data
- **Make data-driven recommendations** for compensation strategy

## 🏗️ Application Structure

### Educational Modules

1. **Market Data Explorer** - Examine raw compensation survey data across 18 job positions from 5 different survey sources
2. **Survey Weighting** - Learn how different weighting methods affect market rate calculations
3. **Aging Analysis** - Understand how data currency impacts market competitiveness
4. **Pay Policy Analysis** - Compare lead, match, and lag compensation strategies
5. **Competitive Analysis** - Multi-position market positioning analysis
6. **Salary Structure Builder** - Create systematic pay ranges based on market analysis

### Sample Data

The application includes realistic sample data featuring:
- **18 job positions** across 6 job families (Engineering, Sales, Marketing, HR, Finance, Operations)
- **5 survey sources** with varying quality ratings and sample sizes
- **3 job levels** (Individual Contributor, Manager, Director)
- **90 total data points** for comprehensive analysis

## 📊 Key Features

### Market Data Process
Following the compensation management flowchart methodology:

1. **Gather Survey Data** - Collect salary information from multiple sources
2. **Weight Sources** - Apply weights based on survey quality and relevance
3. **Age Data** - Adjust for time elapsed using merit budget estimates
4. **Apply Pay Policy** - Implement organizational compensation strategy
5. **Create Structure** - Develop salary ranges and competitive positioning

### Interactive Analysis Tools

- **Real-time calculations** with instant visual feedback
- **Comparative analysis** across different methodological approaches
- **Sensitivity analysis** to understand parameter impacts
- **Professional reporting** capabilities for business recommendations

## 🚀 Getting Started

### Prerequisites

Required R packages:
```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly", "ggplot2",
  "dplyr", "tidyr", "purrr", "stringr", "scales",
  "rmarkdown", "knitr"
))
```

### Running the Application

1. **Local Development:**
   ```r
   # Navigate to the app directory
   setwd("path/to/compensation_analysis")
   
   # Run the application
   shiny::runApp("app.R")
   ```

2. **Educational Use:**
   - Open in RStudio for step-by-step learning
   - Use with guided exercises for hands-on practice
   - Integrate with compensation management coursework

## 💼 Business Applications

### Real-World Use Cases

- **Annual Salary Reviews** - Setting competitive pay levels for existing roles
- **New Position Pricing** - Determining market-competitive offers for new hires
- **Budget Planning** - Projecting compensation costs under different strategies
- **Retention Strategy** - Understanding competitive positioning to reduce turnover
- **M&A Analysis** - Harmonizing pay structures across merged organizations

### Professional Skills Development

Students gain practical experience with:
- Market data interpretation and analysis
- Statistical weighting methodologies
- Time-series adjustment techniques
- Strategic compensation planning
- Data-driven decision making

## 📚 Educational Framework

### Pedagogical Approach

The application follows a progressive learning model:

1. **Exploration** - Students first examine raw market data to understand variability
2. **Methodology** - Learn systematic approaches to data processing and weighting
3. **Analysis** - Apply aging factors and policy decisions to see impacts
4. **Synthesis** - Create comprehensive compensation structures and recommendations
5. **Evaluation** - Generate professional reports and business cases

### Assessment Integration

Perfect for:
- **Hands-on exercises** with real-world scenarios
- **Case study analysis** using actual compensation challenges
- **Group projects** involving competitive analysis
- **Individual assignments** on methodology and recommendations

## 🔬 Methodology

### Survey Weighting Methods

- **Equal Weight** - All surveys weighted equally (baseline comparison)
- **Sample Size** - Weight proportional to number of participants
- **Quality Rating** - Weight based on survey methodology rigor
- **Combined** - Balanced approach using both quality and sample size

### Aging Factor Calculations

```
Aged_Salary = Original_Salary × (1 + Merit_Budget)^(Months_Elapsed / 12)
```

### Pay Policy Implementation

- **Market Matching** - Target 50th percentile positioning
- **Market Leading** - 5-15% above market for talent attraction
- **Market Lagging** - 5-10% below market for cost control

## 📈 Advanced Features

### Data Validation
- Automatic quality checks for data consistency
- Sample size adequacy warnings
- Currency assessment and recommendations

### Scenario Analysis
- Multiple weighting method comparisons
- Pay policy cost impact calculations
- Sensitivity analysis for key parameters

### Professional Reporting
- Comprehensive analysis reports
- Executive summary generation
- Data export capabilities for further analysis

## 🎓 Course Integration

### Suggested Course Topics

- **Compensation Management** - Core methodology and practice
- **HR Analytics** - Data-driven decision making in HR
- **Organizational Psychology** - Pay equity and motivation theory
- **Business Strategy** - Talent management and competitive positioning

### Learning Assessments

- Pre/post knowledge checks on compensation concepts
- Practical exercises with guided solutions
- Capstone projects involving real organizational scenarios
- Peer review of compensation recommendations

## 🔧 Technical Details

### Application Architecture

Built using the Shiny framework with:
- **Reactive programming** for responsive user interactions
- **Modular design** for maintainable code structure
- **Professional UI** using shinydashboard components
- **Interactive visualizations** via plotly integration

### Data Management

- Simulated realistic compensation data with controlled variability
- Reproducible data generation for consistent educational experiences
- Comprehensive metadata for survey source characteristics
- Export capabilities for extended analysis

## 📝 License

This educational tool is part of the Utility Analysis Research project and is designed for academic and professional development purposes.

## 🤝 Contributing

This application was developed as part of a comprehensive utility analysis research platform. Contributions and suggestions for educational enhancements are welcome.

## 📞 Support

For questions about implementation or educational use, please refer to the broader Utility Analysis Research documentation or contact the development team.

---

*Developed as part of the Utility Analysis Research project to advance evidence-based decision making in human resources and organizational psychology.*


