# Fisher & Connelly Educational Web Apps

This directory contains two interactive Shiny web applications designed to help students learn about utility analysis through the work of Fisher & Connelly.

## Apps Included

### 1. Fisher & Connelly (2017) - Contingent Workers Business Case
**File:** `fisher_connelly_2017_app.R`

**Purpose:** Interactive tool for learning how to build a business case for using contingent workers (independent contractors, temporary workers) selectively.

**Key Learning Objectives:**
- Understand the different types of contingent workers and their cost structures
- Learn how turnover costs dramatically impact net value calculations
- Explore scenarios where contingent workers provide superior ROI
- Practice building business cases with real data

**App Features:**
- **Overview Tab:** Introduction to concepts, key findings, and Figure 1 from the original paper
- **Interactive Analysis Tab:** Visualizations of costs, values, and net outcomes
- **Six Strategy Analysis Tab:** Explore the six organizational strategies analyzed by Fisher & Connelly
- **Custom Scenario Builder Tab:** Adjust key parameters to see impact on business case
- **Business Case Report Tab:** Executive summary and implementation guidelines with PDF generation
- **References Tab:** Complete citations, related resources, and app information

### 2. Fisher & Connelly (2020) - Disability Employment Business Case
**File:** `fisher_connelly_2020_app.R`

**Purpose:** Interactive tool for learning how to build the financial business case for employing workers with disabilities.

**Key Learning Objectives:**
- Understand the comprehensive cost-benefit analysis of disability employment
- Learn about accommodation costs vs. performance benefits
- Explore how to measure both tangible and intangible value
- Practice creating evidence-based business cases for inclusive hiring

**App Features:**
- **Overview Tab:** Study background and key findings ($15,483 annual advantage)
- **Interactive Analysis Tab:** Cost breakdowns, performance impacts, and comparisons
- **Accommodation Builder Tab:** Customize accommodation scenarios and ROI analysis
- **Business Case Report Tab:** Implementation guides and strategic recommendations
- **References Tab:** Complete citations, disability employment resources, and app information

## How to Run the Apps

### Option 1: Run Directly in R/RStudio
```r
# For the 2017 Contingent Workers app:
shiny::runApp("fisher_connelly_2017_app.R")

# For the 2020 Disability Employment app:
shiny::runApp("fisher_connelly_2020_app.R")
```

### Option 2: Run from Command Line
```bash
# Navigate to the project directory
cd "/path/to/Utility Analysis Research"

# Run the 2017 app
R -e "shiny::runApp('fisher_connelly_2017_app.R')"

# Run the 2020 app
R -e "shiny::runApp('fisher_connelly_2020_app.R')"
```

## Required R Packages

Both apps require the following R packages:
```r
install.packages(c(
  "shiny",
  "shinydashboard", 
  "ggplot2",
  "dplyr",
  "scales",
  "DT",
  "plotly",
  "tidyr",
  "rmarkdown",
  "knitr",
  "ggtext",
  "grid",
  "gridExtra"
))
```

**Note:** PDF report generation requires a LaTeX installation (e.g., TinyTeX, MiKTeX, or MacTeX). Install TinyTeX with:
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Educational Use Cases

### For Students:
1. **Homework Assignments:** Use the scenario builders to explore "what-if" analyses
2. **Class Discussions:** Compare findings across different parameter assumptions  
3. **Research Projects:** Use as a starting point for extending the analyses
4. **Case Study Analysis:** Practice interpreting business cases with real data

### For Instructors:
1. **Live Demonstrations:** Show utility analysis concepts interactively
2. **Assignment Creation:** Have students explore specific scenarios and report findings
3. **Concept Reinforcement:** Visual learning of complex financial calculations
4. **Real-World Applications:** Connect academic concepts to practical business decisions

## Key Features for Learning

### Interactive Elements:
- **Slider Controls:** Adjust wages, turnover rates, accommodation costs
- **Real-Time Updates:** See immediate impact of parameter changes
- **Visual Feedback:** Charts and graphs update dynamically
- **Scenario Comparison:** Compare different worker types or conditions
- **PDF Report Generation:** Download customized business case reports with current scenarios

### Educational Scaffolding:
- **Progressive Disclosure:** Information builds from overview to detailed analysis
- **Guided Interpretation:** Built-in explanations of findings and implications
- **Business Context:** Practical applications and implementation guidelines
- **Evidence-Based:** All calculations verified against original research

## Data Sources

Both apps use data directly extracted and verified from the original Fisher & Connelly publications:

- **Fisher & Connelly (2017):** "Lower Cost or Just Lower Value? Modeling the Organizational Costs and Benefits of Contingent Work"
- **Fisher & Connelly (2020):** "Building the 'Business Case' for Hiring People with Disabilities: A Financial Cost-Benefit Analysis Methodology and Example"

The reproduction analyses in the `reproductions/` folder verify that our calculations match the original papers.

### Figure Integration

The **Fisher & Connelly (2017) app** includes an enhanced interactive reproduction of **Figure 1** from the original paper: "A Framework of Contingent Worker Costs and Benefits." This conceptual framework diagram shows:

- How **HR Strategy** acts as a **moderator** of the relationships between worker types and outcomes
- How **Worker Type** influences both **Service Costs** and **Service Value**
- The components of Service Costs (wages/benefits, transaction costs, turnover)
- The components of Service Value (task performance, OCBs)
- The feedback loop where **Service Costs can affect Service Value** (performance)
- How these combine to determine **Net Value**

The figure features improved spacing, color-coded relationships, and moderation arrows that demonstrate how organizational strategy shapes the effectiveness of different worker arrangements. The figure is displayed in the Overview tab and provides students with a comprehensive visual understanding of the theoretical framework underlying the analysis.

## App Structure

Both apps follow a consistent structure for ease of use:

1. **Overview Tab:** Study background and key concepts
2. **Interactive Analysis Tab:** Core data exploration and visualization  
3. **Scenario/Parameter Tab:** Customizable analysis with user inputs
4. **Business Case Tab:** Practical implementation guidance
5. **References Tab:** Complete citations, related resources, and technical information

## Technical Notes

- Apps are self-contained with embedded data (no external dependencies)
- Data loading functions include fallback values if reproduction files are missing
- UI responsive design works on desktop and tablet screens
- All calculations are transparent and match original research

## Report Generation Features

Both apps now include comprehensive PDF report generation with professional business case documents featuring:
- **Executive summaries** with key financial findings
- **Customized analysis** based on user's scenario parameters
- **Charts and visualizations** of ROI and cost-benefit analysis
- **Strategic recommendations** for implementation
- **Professional formatting** suitable for business presentations

### Citations and Acknowledgments in PDF Reports

Each generated PDF report includes:
- **Primary research citations** with full academic references
- **Supporting literature** (including Consensus.app systematic review for 2020 app)
- **AI development acknowledgments** detailing the use of Claude (Anthropic) and Cursor
- **Human oversight disclosure** emphasizing validation and quality control
- **Educational use disclaimers** and legal information
- **Transparency statements** about methodology and data verification

The reports demonstrate **best practices for AI transparency** in academic and professional contexts, providing a model for ethical AI-assisted tool development.

### Report Contents:
- **Executive Summary:** Key findings and recommendations based on current scenario
- **Parameter Documentation:** All user-selected inputs and assumptions
- **Financial Analysis:** Charts, tables, and ROI calculations
- **Strategic Recommendations:** Customized implementation guidance
- **Risk Assessment:** Identification and mitigation strategies
- **Implementation Checklist:** Step-by-step action items

### Report Benefits:
- **Presentation Ready:** Professional formatting for management presentations
- **Documentation:** Permanent record of analysis assumptions and results
- **Comparison:** Save multiple scenarios for side-by-side analysis
- **Assignment Submission:** Students can submit comprehensive analyses
- **Business Cases:** Ready-to-use documents for organizational decision-making

### Usage Notes:
- Reports reflect current scenario parameters at time of generation
- PDF generation may take 30-60 seconds depending on system performance
- Requires LaTeX installation for PDF rendering
- Reports are fully customized with user's specific inputs and calculations

## References and Citations

Both apps include comprehensive **References tabs** that provide:

### Academic Information:
- **Complete citations** for the primary research papers with DOI links
- **Abstracts** from the original publications
- **Related research** and foundational works in utility analysis and employment studies
- **Additional reading** suggestions for deeper exploration

### App Documentation:
- **Technical requirements** and version information
- **Citation guidelines** for referencing the educational tools
- **GitHub repository placeholder** (to be populated when published online)
- **Proper attribution** to the original researchers

### Legal and Ethical Information:
- **Acknowledgments** to original authors and institutions
- **Educational use disclaimers** and limitations
- **Accessibility statements** (especially for the disability employment app)
- **Implementation guidance** for practical applications

### For Instructors:
- Use the References tabs to help students understand **academic standards**
- Demonstrate **proper citation practices** in applied research
- Show connections between **theoretical frameworks** and practical applications
- Provide **additional resources** for extended learning
- **PDF reports include comprehensive citations** and AI development acknowledgments
- **Model transparency** in AI-assisted academic tool development

## Future Enhancements

Potential additions for enhanced learning:
- Comparison tools between the two studies
- Additional sensitivity analysis options
- Integration with other utility analysis frameworks
- Multi-scenario comparison reports

## AI Development Acknowledgments

These educational applications were developed with assistance from AI language models and modern development tools:

- **AI Assistants:** Claude (Anthropic) for code development, data analysis, educational content design, and documentation
- **Development Environment:** Cursor AI-powered code editor for enhanced productivity, code quality, and debugging
- **Human Oversight:** All AI-generated content was reviewed, validated, and refined by human researchers
- **Data Verification:** Reproduction analyses and calculations were independently verified against original publications
- **Academic Rigor:** AI tools enhanced development efficiency while maintaining strict adherence to research accuracy

The use of AI tools enabled rapid development of comprehensive educational resources while ensuring all reproduced findings match the original research papers exactly.

## Support

For technical issues or educational questions about these apps, refer to:
- Original research papers for methodological details
- Reproduction analyses in `reproductions/` folders for calculation verification
- Main utility analysis documentation in the project

---

**Created:** January 2025  
**Based on:** Fisher & Connelly (2017, 2020) research  
**Purpose:** Educational utility analysis learning tools
