/**
 * Guided Scenarios & Tutorials
 *
 * Each scenario reproduces a specific illustration from the R codebase
 * with exact parameters, expected results, and a step-by-step tutorial
 * explaining the methodology and interpretation.
 */

import type { InterventionParams } from "../models/utility-engine";

export interface TutorialStep {
  title: string;
  content: string;
  highlight?: string; // parameter name to highlight in the UI
}

export interface ExpectedResult {
  label: string;
  value: string;
  explanation: string;
}

export interface GuidedScenario {
  id: string;
  title: string;
  subtitle: string;
  category: string;
  difficulty: "beginner" | "intermediate" | "advanced";
  estimatedMinutes: number;
  icon: string;
  color: string;

  // Narrative context
  narrative: string;
  learningObjectives: string[];

  // Pre-configured interventions
  interventions: InterventionParams[];
  overlapFactor?: number;

  // Step-by-step tutorial
  steps: TutorialStep[];

  // Expected results for verification
  expectedResults: ExpectedResult[];

  // Key takeaways
  takeaways: string[];

  // Source citation
  citation: string;
  rAppSource: string;
}

export const GUIDED_SCENARIOS: GuidedScenario[] = [
  // ═══════════════════════════════════════════════════════════════════════════
  // 1. STAFFING: STRUCTURED INTERVIEWS
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "staffing-structured-interviews",
    title: "Improving the Hiring Process",
    subtitle: "The business case for structured interviews",
    category: "Selection & Staffing",
    difficulty: "beginner",
    estimatedMinutes: 10,
    icon: "🎯",
    color: "#2563eb",
    narrative:
      "Your organization hires 100 new employees per year for a role paying $50,000. Currently, hiring managers use unstructured interviews (validity ≈ 0.20). You're evaluating whether to implement structured, behaviorally-anchored interviews (validity ≈ 0.51). The selection ratio is 0.30 — for every 3 applicants, 1 is hired. The structured interview costs $150 more per applicant to administer.",
    learningObjectives: [
      "Understand the Brogden-Cronbach-Gleser (BCG) utility model",
      "See how validity improvements translate to dollar value",
      "Learn what selection ratio and SDy mean in practical terms",
      "Interpret break-even analysis for a selection intervention",
    ],
    interventions: [
      {
        id: "scenario-si-1",
        type: "selection",
        label: "Structured Interviews",
        numEmployees: 100,
        avgSalary: 50000,
        sdyPercent: 40,
        timePeriodYears: 3,
        costPerEmployee: 150,
        fixedCost: 5000,
        validityCoefficient: 0.51,
        currentValidity: 0.20,
        selectionRatio: 0.30,
        applyEconomicAdjustments: false,
      },
    ],
    steps: [
      {
        title: "Understanding the Scenario",
        content:
          "You have 100 positions to fill. With a selection ratio of 0.30, you're screening about 333 applicants to fill those 100 spots. The key question: is the structured interview worth the extra $150/applicant?",
      },
      {
        title: "What Is SDy?",
        content:
          'SDy is the standard deviation of job performance in dollar terms. The "40% rule" (Schmidt & Hunter, 1983) estimates SDy at 40% of salary. For a $50,000 job, SDy ≈ $20,000. This means top performers produce roughly $20,000 more value per year than average performers.',
        highlight: "sdyPercent",
      },
      {
        title: "What Is Validity?",
        content:
          "Validity (rxy) is the correlation between your selection tool's scores and actual job performance. Unstructured interviews have validity ≈ 0.20 (barely better than random). Structured interviews reach ≈ 0.51 (Sackett et al., 2022). The incremental validity is 0.51 − 0.20 = 0.31.",
        highlight: "validityCoefficient",
      },
      {
        title: "What Is the Selection Ratio?",
        content:
          "Selection ratio = hired ÷ applicants. At 0.30, you hire the top 30%. Lower ratios mean more selectivity and higher utility — you're picking from a stronger pool. The formula uses λ(ϕ), the mean standardized score of selectees.",
        highlight: "selectionRatio",
      },
      {
        title: "The BCG Formula",
        content:
          "ΔU = N × t × Δr × SDy × λ(ϕ) − C\n\nwhere:\n• N = 100 hires\n• t = 3 years average tenure\n• Δr = 0.31 (validity improvement)\n• SDy = $20,000\n• λ(ϕ) = 1.167 (for SR = 0.30)\n• C = 333 × $150 + $5,000 setup = $54,950",
      },
      {
        title: "Click Calculate ROI",
        content:
          "Click 'Calculate ROI →' to see the results. Look at the net benefit, ROI ratio, and especially the break-even effect size — it tells you how wrong the research estimates would have to be before this investment stops paying off.",
      },
    ],
    expectedResults: [
      {
        label: "Gross Benefit",
        value: "~$2.2M",
        explanation:
          "Over 3 years, the improved hiring generates approximately $2.2M in additional performance value.",
      },
      {
        label: "ROI",
        value: "~38:1",
        explanation:
          "Every dollar spent on structured interviews returns about $38 in performance gains.",
      },
      {
        label: "Break-even validity",
        value: "~0.008",
        explanation:
          "Even if the true incremental validity were only 0.008 (essentially zero), you'd still break even. The investment is extremely robust.",
      },
    ],
    takeaways: [
      "Structured interviews are one of the highest-ROI HR interventions available",
      "The BCG model shows that even modest validity improvements create large value when applied to many hires over time",
      "Break-even analysis is a powerful tool for communicating with skeptical stakeholders — it shows how much the research would have to be wrong",
    ],
    citation: "Sackett et al. (2022); Schmidt & Hunter (1998); Brogden (1949); Cronbach & Gleser (1965)",
    rAppSource: "app.R, staffing_utility_app_fixed.R",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 2. TRAINING: TECHNICAL VS MANAGERIAL (MORROW ET AL.)
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "training-morrow-comparison",
    title: "Are All Training Programs Equal?",
    subtitle: "Morrow et al. (1997): Technical vs. managerial training ROI",
    category: "Training & Development",
    difficulty: "beginner",
    estimatedMinutes: 12,
    icon: "📚",
    color: "#059669",
    narrative:
      "Morrow et al. (1997) conducted a landmark study comparing the utility of different training programs. They found dramatic differences: technical/sales training (d = 0.64) yielded far higher returns than managerial training (d = 0.31). You're an HR director with a budget to train 200 employees. Which program should you invest in?",
    learningObjectives: [
      "Compare ROI across training types using the same utility framework",
      "Understand how effect size (Cohen's d) drives training utility",
      "Learn break-even analysis for training investments",
      "See why not all training dollars are created equal",
    ],
    interventions: [
      {
        id: "scenario-morrow-tech",
        type: "training",
        label: "Technical/Sales Training",
        numEmployees: 200,
        avgSalary: 75000,
        sdyPercent: 40,
        timePeriodYears: 2,
        costPerEmployee: 2000,
        fixedCost: 10000,
        effectSize: 0.64,
      },
      {
        id: "scenario-morrow-mgmt",
        type: "training",
        label: "Managerial Training",
        numEmployees: 200,
        avgSalary: 75000,
        sdyPercent: 40,
        timePeriodYears: 2,
        costPerEmployee: 2500,
        fixedCost: 10000,
        effectSize: 0.31,
      },
    ],
    overlapFactor: 0.25,
    steps: [
      {
        title: "The Research Question",
        content:
          "Morrow, Jarrett, & Rupinski (1997) asked a simple but powerful question: do all training programs deliver the same return? They studied 18 training programs across one organization and found the answer was a resounding 'no.'",
      },
      {
        title: "Effect Size: The Key Differentiator",
        content:
          "Cohen's d measures the standardized effect of training. Technical/sales training (d = 0.64) means trained employees perform 0.64 standard deviations better. Managerial training (d = 0.31) is half that effect. In dollar terms, this difference is enormous.",
        highlight: "effectSize",
      },
      {
        title: "Two Interventions, Same Organization",
        content:
          "Both programs train 200 employees earning $75,000 (SDy = $30,000). Technical training costs $2,000/person; managerial training costs $2,500/person (more expensive AND less effective).",
      },
      {
        title: "Training Utility Formula",
        content:
          "ΔU = N × t × d × SDy − C\n\nTechnical: 200 × 2 × 0.64 × $30,000 − $410,000 = $7.27M\nManagerial: 200 × 2 × 0.31 × $30,000 − $510,000 = $3.21M\n\nThe technical program generates more than twice the net benefit at a lower cost.",
      },
      {
        title: "Compare the Results",
        content:
          "Click 'Calculate ROI →' to see both programs side by side. Pay attention to the contribution chart — it shows how much each program contributes to total value. Also check each program's break-even effect size.",
      },
      {
        title: "Overlap Between Programs",
        content:
          "If you run both programs, their effects partially overlap (both improve performance). The overlap slider adjusts for this shared variance. At 25% overlap, the combined benefit is less than the simple sum — but still very positive.",
      },
    ],
    expectedResults: [
      {
        label: "Technical/Sales Net Benefit",
        value: "~$7.3M",
        explanation:
          "High effect size (d = 0.64) combined with lower cost makes this a powerhouse investment.",
      },
      {
        label: "Managerial Net Benefit",
        value: "~$3.2M",
        explanation: "Still positive, but less than half the return of technical training.",
      },
      {
        label: "Break-even d (Technical)",
        value: "~0.034",
        explanation:
          "Technical training breaks even at d = 0.034 — the effect could be 95% smaller than estimated and still pay for itself.",
      },
      {
        label: "Break-even d (Managerial)",
        value: "~0.085",
        explanation:
          "Managerial training needs a slightly larger effect to justify its higher cost, but still very robust.",
      },
    ],
    takeaways: [
      "Training programs differ dramatically in ROI — effect size matters more than cost",
      "Technical/sales training consistently shows the highest returns in meta-analyses",
      "Even managerial training with a modest effect size (d = 0.31) generates millions in value",
      "Break-even analysis shows both programs are robust: they'd need to be near-zero effective to not pay off",
    ],
    citation: "Morrow, C. C., Jarrett, M. Q., & Rupinski, M. T. (1997). An investigation of the effect and economic utility of corporate-wide training. Personnel Psychology, 50(1), 91–119.",
    rAppSource: "training_utility_app.R (Morrow et al. tab)",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 3. JOB CRAFTING: HEALTHCARE
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "job-crafting-healthcare",
    title: "Job Crafting in Healthcare",
    subtitle: "Low-cost, high-impact employee empowerment",
    category: "Job Design & Engagement",
    difficulty: "beginner",
    estimatedMinutes: 8,
    icon: "🔧",
    color: "#7c3aed",
    narrative:
      "A hospital system employs 100 healthcare workers (average salary $50,000). Job crafting interventions — workshops where employees reshape their tasks, relationships, and perceptions of work — cost only $40 per employee. Oprea et al. (2019) meta-analyzed 14 studies and found a task performance effect of g = 0.47 in healthcare settings.",
    learningObjectives: [
      "Understand job crafting as an HR intervention",
      "See how low-cost interventions can produce outsized returns",
      "Compare effect sizes across industries",
      "Learn to interpret meta-analytic confidence intervals",
    ],
    interventions: [
      {
        id: "scenario-jc-1",
        type: "job_crafting",
        label: "Job Crafting (Healthcare)",
        numEmployees: 100,
        avgSalary: 50000,
        sdyPercent: 40,
        timePeriodYears: 1,
        costPerEmployee: 40,
        fixedCost: 2000,
        effectSize: 0.47,
      },
    ],
    steps: [
      {
        title: "What Is Job Crafting?",
        content:
          "Job crafting is when employees proactively reshape their work. It includes three dimensions:\n\n• Structural crafting — changing the number, scope, or type of tasks\n• Social crafting — changing interactions with others\n• Cognitive crafting — changing how they perceive and find meaning in work\n\nUnlike top-down job redesign, job crafting is employee-driven.",
      },
      {
        title: "The Meta-Analytic Evidence",
        content:
          "Oprea et al. (2019) meta-analyzed 14 studies with 1,204 participants. Key findings:\n\n• Overall job crafting effect: g = 0.26\n• Work engagement effect: g = 0.31\n• Task performance in healthcare: g = 0.47\n\nHealthcare shows the strongest evidence — possibly because healthcare workers have more autonomy to craft their roles.",
      },
      {
        title: "The Cost Advantage",
        content:
          "At $40 per employee plus a $2,000 setup cost, this is one of the cheapest interventions available. Compare: assessment centers cost $2,500/person, leadership development costs $3,000/person. Job crafting workshops are 60-75x cheaper.",
        highlight: "costPerEmployee",
      },
      {
        title: "Duration Factor",
        content:
          "The original research used relatively short interventions (about 3 months). We conservatively set duration to 1 year to see annualized returns. Even with this conservative timeframe, the ROI is extraordinary.",
        highlight: "timePeriodYears",
      },
      {
        title: "Run the Analysis",
        content:
          "Click 'Calculate ROI' and note three things:\n\n1. The net benefit per employee — how much value each person adds\n2. The ROI ratio — likely one of the highest you'll see\n3. The break-even effect size — how robust this estimate is",
      },
    ],
    expectedResults: [
      {
        label: "Net Benefit",
        value: "~$934K",
        explanation:
          "100 employees generating ~$9,340 each in additional value from a $40 investment.",
      },
      {
        label: "ROI",
        value: "~156:1",
        explanation:
          "For every $1 spent on job crafting workshops, the organization gains ~$156. This extreme ROI is driven by the very low cost.",
      },
      {
        label: "Break-even effect size",
        value: "~0.003",
        explanation:
          "The intervention would have to produce virtually zero effect to not pay for itself. The cost is so low that any positive effect is worthwhile.",
      },
    ],
    takeaways: [
      "Job crafting is a 'best-kept secret' in HR — extremely low cost, strong evidence base",
      "Healthcare settings show the strongest effects, but all industries show positive results",
      "The break-even analysis makes this a near-zero-risk investment",
      "Annual value increase of ~$9,400 per employee from a $40 intervention",
    ],
    citation: "Oprea, B. T., Barzin, L., Vîrgă, D., Iliescu, D., & Rusu, A. (2019). Effectiveness of job crafting interventions. European Journal of Work and Organizational Psychology, 28(6), 723–741.",
    rAppSource: "job_crafting_utility_app.R, shiny_apps/job_crafting/app.R",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 4. GOAL SETTING
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "goal-setting-program",
    title: "The Power of Goal Setting",
    subtitle: "One of psychology's most robust findings, applied to the bottom line",
    category: "Performance Management",
    difficulty: "beginner",
    estimatedMinutes: 8,
    icon: "🎯",
    color: "#d97706",
    narrative:
      "You're implementing a systematic goal-setting program (SMART goals, quarterly check-ins) for 300 employees earning an average of $55,000. Locke & Latham's goal-setting theory, validated across 600+ studies, shows a robust effect of d = 0.46. The implementation costs about $200 per employee (training managers, setting up the system) plus $5,000 in setup costs.",
    learningObjectives: [
      "Understand why goal setting is considered psychology's most validated finding",
      "See how a low-cost organizational intervention generates high returns",
      "Compare goal setting ROI to other HR interventions",
    ],
    interventions: [
      {
        id: "scenario-gs-1",
        type: "goal_setting",
        label: "Goal Setting Program",
        numEmployees: 300,
        avgSalary: 55000,
        sdyPercent: 40,
        timePeriodYears: 2,
        costPerEmployee: 200,
        fixedCost: 5000,
        effectSize: 0.46,
      },
    ],
    steps: [
      {
        title: "Goal Setting Theory",
        content:
          "Locke & Latham (2002) synthesized over 600 studies spanning 35 years. The key finding: specific, difficult goals lead to higher performance than vague goals ('do your best') or no goals. The meta-analytic effect is d = 0.46 — a medium effect that translates to substantial economic value.",
      },
      {
        title: "Why Is This So Cost-Effective?",
        content:
          "Goal setting doesn't require hiring external trainers, purchasing expensive technology, or taking employees off the job for weeks. The main cost is manager training ($200/person) and system setup ($5,000). Compare this to assessment centers ($2,500/person) or leadership development ($3,000/person).",
        highlight: "costPerEmployee",
      },
      {
        title: "The Parameters",
        content:
          "• 300 employees affected\n• Average salary $55,000 → SDy = $22,000 (at 40%)\n• Effect size d = 0.46 (medium, well-validated)\n• Benefits last 2 years (conservative — goals are ongoing)\n• Total cost: 300 × $200 + $5,000 = $65,000",
      },
      {
        title: "Calculate and Interpret",
        content:
          "Run the analysis and focus on the ROI ratio. Goal setting typically produces one of the highest ROIs because the cost is so low relative to the effect. Schmidt (2013) estimated the economic value of goal setting to employers in the billions annually.",
      },
    ],
    expectedResults: [
      {
        label: "Net Benefit",
        value: "~$6.0M",
        explanation:
          "300 employees × 2 years × 0.46 × $22,000 − $65,000 in costs.",
      },
      {
        label: "ROI",
        value: "~94:1",
        explanation: "Extremely high ROI due to the combination of a meaningful effect size and very low implementation cost.",
      },
    ],
    takeaways: [
      "Goal setting is arguably the most cost-effective HR intervention available",
      "The d = 0.46 effect has been replicated across hundreds of studies — it's one of psychology's most robust findings",
      "Implementation costs are minimal, making the ROI extraordinary",
      "If your organization doesn't have a systematic goal-setting program, this should be a top priority",
    ],
    citation: "Locke, E. A., & Latham, G. P. (2002). Building a practically useful theory of goal setting. American Psychologist, 57(9), 705–717; Schmidt, F. L. (2013). The economic value of goal setting.",
    rAppSource: "training_utility_app.R (Goal Setting option)",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 5. DISABILITY EMPLOYMENT
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "disability-employment",
    title: "The Business Case for Disability Employment",
    subtitle: "Fisher & Connelly (2020): Why hiring workers with disabilities pays off",
    category: "Diversity & Inclusion",
    difficulty: "intermediate",
    estimatedMinutes: 12,
    icon: "♿",
    color: "#0891b2",
    narrative:
      "Fisher & Connelly (2020) built a utility analysis framework specifically for disability employment. They found that workers with disabilities often have a higher net service value ($21,949) than workers without disabilities ($6,466) — a $15,483 annual advantage per employee. This is driven by lower turnover (0% vs 16.6%), lower wages (78% of non-disabled wages), and strong performance (1.28× multiplier).",
    learningObjectives: [
      "Understand how turnover costs affect total workforce value",
      "See how accommodation costs compare to turnover savings",
      "Learn the Fisher & Connelly (2020) service value framework",
      "Build a data-driven business case for disability inclusion",
    ],
    interventions: [
      {
        id: "scenario-dis-1",
        type: "custom",
        label: "Disability Employment Program",
        numEmployees: 100,
        avgSalary: 25300,
        sdyPercent: 40,
        timePeriodYears: 3,
        costPerEmployee: 214,
        fixedCost: 30000,
        effectSize: 0.45,
      },
    ],
    steps: [
      {
        title: "The Overlooked Workforce",
        content:
          "Workers with disabilities are an underutilized talent pool. Fisher & Connelly (2020) challenged the assumption that accommodations are expensive and performance is lower. Their utility analysis told a very different story.",
      },
      {
        title: "The Net Value Framework",
        content:
          "Service Value = the dollar value of what an employee produces.\nService Cost = wages + benefits + behavioral costs + turnover costs + accommodations.\nNet Value = Service Value − Service Cost.\n\nWorkers with disabilities: Net Value = $21,949\nWorkers without disabilities: Net Value = $6,466\nAdvantage: $15,483/year per employee.",
      },
      {
        title: "Why the Advantage?",
        content:
          "Three factors drive the advantage:\n\n1. Lower turnover (0% vs 16.6%) — turnover costs ~$4,195/year for non-disabled workers\n2. Lower wage costs (78% of non-disabled wages) — market reality\n3. Accommodation costs are modest ($214/year annual, $300 one-time)\n\nThe turnover savings alone more than offset accommodation costs.",
      },
      {
        title: "The Parameters We're Using",
        content:
          "We model this as a custom intervention:\n• 100 employees in the program\n• Average salary: $25,300 (wage ratio from Fisher & Connelly)\n• Annual accommodation cost: $214/employee\n• Setup cost: $30,000 (program development, manager training)\n• Effect size: d = 0.45 (captures the combined net value advantage)\n• Duration: 3 years (conservative — turnover savings compound)",
      },
      {
        title: "Calculate and Reflect",
        content:
          "Run the analysis. The key insight is that disability employment isn't a cost center — it's a profit center. Lower turnover, competitive performance, and modest accommodation costs combine to create genuine economic value.",
      },
    ],
    expectedResults: [
      {
        label: "Net Benefit",
        value: "~$1.3M",
        explanation:
          "100 employees over 3 years generating substantial value from reduced turnover and competitive performance.",
      },
      {
        label: "Annual accommodation cost",
        value: "$214/person",
        explanation:
          "Far less than turnover cost savings of ~$4,195/person/year for non-disabled workers.",
      },
    ],
    takeaways: [
      "Disability employment is an economic opportunity, not just a social responsibility",
      "Turnover cost savings are the single biggest driver of the financial advantage",
      "Annual accommodation costs ($214) are a fraction of turnover costs ($4,195)",
      "This analysis gives HR a data-driven argument for disability inclusion programs",
    ],
    citation: "Fisher, S. L., & Connelly, C. E. (2020). Building the business case for hiring people with disabilities. In S. L. Fisher (Ed.), Research in Personnel and Human Resources Management.",
    rAppSource: "fisher_connelly_2020_app.R",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 6. ECONOMIC ADJUSTMENTS (STURMAN 2000)
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "sturman-economic-adjustments",
    title: "Do Economic Adjustments Change the Story?",
    subtitle: "Sturman (2000): Variable costs, taxes, and discounting",
    category: "Advanced Topics",
    difficulty: "advanced",
    estimatedMinutes: 15,
    icon: "📉",
    color: "#dc2626",
    narrative:
      "Sturman (2000) argued that standard utility analysis overestimates returns by ignoring real-world economic factors: variable costs (10-30%), corporate taxes (20-40%), and time-value discounting (5-15%). When these adjustments are applied, do HR interventions still show positive returns? We'll test this with a staffing scenario using the most conservative assumptions.",
    learningObjectives: [
      "Understand Sturman's five economic adjustment factors",
      "See how variable costs, taxes, and discounting reduce raw utility estimates",
      "Learn why adjusted utility is more credible to finance professionals",
      "Discover that even with heavy adjustments, good interventions remain worthwhile",
    ],
    interventions: [
      {
        id: "scenario-sturman-1",
        type: "selection",
        label: "Staffing (Unadjusted)",
        numEmployees: 100,
        avgSalary: 50000,
        sdyPercent: 40,
        timePeriodYears: 5,
        costPerEmployee: 300,
        fixedCost: 5000,
        validityCoefficient: 0.50,
        currentValidity: 0.10,
        selectionRatio: 0.33,
        applyEconomicAdjustments: false,
      },
      {
        id: "scenario-sturman-2",
        type: "selection",
        label: "Staffing (Sturman Adjusted)",
        numEmployees: 100,
        avgSalary: 50000,
        sdyPercent: 40,
        timePeriodYears: 5,
        costPerEmployee: 300,
        fixedCost: 5000,
        validityCoefficient: 0.50,
        currentValidity: 0.10,
        selectionRatio: 0.33,
        applyEconomicAdjustments: true,
        variableCostPercent: 20,
        taxRate: 35,
        discountRate: 10,
      },
    ],
    overlapFactor: 0.0,
    steps: [
      {
        title: "The Overestimation Problem",
        content:
          "Critics like Latham & Whyte (1994) found that managers rejected utility analysis results because the numbers seemed 'too good to be true.' Sturman (2000) showed they had a point — raw utility estimates ignore real economic factors.",
      },
      {
        title: "Variable Costs",
        content:
          "Not all of a performance increase translates to profit. If a salesperson sells 20% more, the company also incurs costs for the extra product (materials, shipping, etc.). Variable costs typically consume 10-30% of incremental revenue.",
        highlight: "variableCostPercent",
      },
      {
        title: "Tax Rate",
        content:
          "Increased profit is taxed. At a 35% corporate tax rate, $1.00 of additional performance value yields only $0.65 in after-tax benefit.",
        highlight: "taxRate",
      },
      {
        title: "Discount Rate",
        content:
          "A dollar today is worth more than a dollar next year. A 10% discount rate means Year 2 benefits are worth 91¢, Year 3 benefits are worth 83¢, etc. This is standard corporate finance — your CFO will expect to see this.",
        highlight: "discountRate",
      },
      {
        title: "Side-by-Side Comparison",
        content:
          "We've set up two identical staffing interventions — one unadjusted, one with Sturman adjustments. Calculate both to see the difference. The adjusted version is more conservative and more credible to business leaders.",
      },
      {
        title: "Interpret the Reduction",
        content:
          "The adjusted utility will be substantially lower than the unadjusted figure — typically 30-60% lower. But here's the key: even the adjusted figure is almost certainly still positive. Sturman's message wasn't that HR interventions don't work — it's that we should present more realistic numbers.",
      },
    ],
    expectedResults: [
      {
        label: "Unadjusted Net Benefit",
        value: "~$3.6M",
        explanation: "Raw BCG model without any economic adjustments over 5 years.",
      },
      {
        label: "Adjusted Net Benefit",
        value: "~$2.0M",
        explanation:
          "After applying 20% variable costs, 35% taxes, and 10% discount rate. Still strongly positive.",
      },
      {
        label: "Reduction",
        value: "~40-45%",
        explanation:
          "Economic adjustments reduce the estimate substantially, but the intervention remains highly worthwhile.",
      },
    ],
    takeaways: [
      "Economic adjustments reduce utility estimates by 30-60% — but the bottom line usually stays positive",
      "Presenting adjusted figures increases credibility with finance professionals and leadership",
      "Variable costs, taxes, and discounting are standard corporate finance concepts — using them shows sophistication",
      "Sturman's contribution was making utility analysis more realistic, not debunking it",
    ],
    citation: "Sturman, M. C. (2000). Implications of utility analysis adjustments for estimates of human resource intervention value. Journal of Management, 26(2), 281–299.",
    rAppSource: "app.R, scripts/utilities/sturman_utility_functions.R, scripts/sturman_2000_monte_carlo.R",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 7. COMBINED: THE HR SYSTEM
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "combined-hr-system",
    title: "Building a Complete HR System",
    subtitle: "What happens when you stack selection, training, and goal setting?",
    category: "Systems Thinking",
    difficulty: "advanced",
    estimatedMinutes: 15,
    icon: "🏗️",
    color: "#6d28d9",
    narrative:
      "Most organizations don't implement just one HR intervention — they build systems. What if you improved selection (structured interviews), added training (technical skills), AND implemented goal setting? This scenario models the combined effect of three evidence-based interventions for 200 employees, accounting for the overlap between them.",
    learningObjectives: [
      "Understand how multiple HR interventions interact",
      "Learn about the overlap/diminishing returns problem",
      "See how a system of interventions creates compounding value",
      "Practice interpreting combined utility with realistic assumptions",
    ],
    interventions: [
      {
        id: "scenario-system-select",
        type: "selection",
        label: "Structured Interviews",
        numEmployees: 200,
        avgSalary: 60000,
        sdyPercent: 40,
        timePeriodYears: 3,
        costPerEmployee: 150,
        fixedCost: 8000,
        validityCoefficient: 0.51,
        currentValidity: 0.20,
        selectionRatio: 0.25,
      },
      {
        id: "scenario-system-train",
        type: "training",
        label: "Technical Skills Training",
        numEmployees: 200,
        avgSalary: 60000,
        sdyPercent: 40,
        timePeriodYears: 2,
        costPerEmployee: 1500,
        fixedCost: 15000,
        effectSize: 0.50,
      },
      {
        id: "scenario-system-goals",
        type: "goal_setting",
        label: "Goal Setting Program",
        numEmployees: 200,
        avgSalary: 60000,
        sdyPercent: 40,
        timePeriodYears: 2,
        costPerEmployee: 150,
        fixedCost: 3000,
        effectSize: 0.46,
      },
    ],
    overlapFactor: 0.20,
    steps: [
      {
        title: "From Interventions to Systems",
        content:
          "Real HR doesn't happen one intervention at a time. Effective organizations build integrated talent management systems. The question: when you stack three interventions, is the combined value the sum of parts, or less?",
      },
      {
        title: "The Overlap Problem",
        content:
          "When selection and training both improve performance, they partly target the same variance. If you hire smarter people, they may benefit less from training (or more — it depends). The overlap factor (set to 20%) accounts for this shared variance.",
      },
      {
        title: "Three Interventions",
        content:
          "1. Structured Interviews (r = 0.51, replacing r = 0.20) — better hiring\n2. Technical Skills Training (d = 0.50) — improve existing employees\n3. Goal Setting (d = 0.46) — sustained performance management\n\nAll applied to the same 200 employees earning $60,000.",
      },
      {
        title: "The Combination Formula",
        content:
          "Combined effect: d_combined = √(Σdᵢ² + 2Σρᵢⱼdᵢdⱼ)\n\nWith 20% overlap, the combined effect is less than the sum of individual effects but greater than any single intervention. This is the multivariate generalization of the utility model.",
      },
      {
        title: "Calculate the System",
        content:
          "Run the analysis and examine:\n• Individual vs. combined contributions\n• The overlap reduction (how much less than the naive sum)\n• Each intervention's break-even point\n• Whether any individual intervention isn't worth it on its own",
      },
      {
        title: "What to Present to Leadership",
        content:
          "When presenting, lead with the combined figure and the overlap note. This shows you've thought about realistic interactions. Then show individual contributions to let leaders see which investments drive the most value.",
      },
    ],
    expectedResults: [
      {
        label: "Combined Net Benefit",
        value: "~$13-15M",
        explanation:
          "Three interventions together generate substantially more than any single one, even after overlap adjustment.",
      },
      {
        label: "Overlap Reduction",
        value: "~$2-4M",
        explanation:
          "The combined benefit is several million less than the naive sum, reflecting realistic shared variance in performance improvement.",
      },
      {
        label: "System ROI",
        value: "~20-30:1",
        explanation:
          "Even accounting for all costs and overlap, the system generates 20-30 times its total investment.",
      },
    ],
    takeaways: [
      "Combining interventions creates more value than any single intervention — but less than the naive sum",
      "The overlap factor is an honest acknowledgment that performance is a single construct targeted from multiple angles",
      "A system of three modest interventions can generate tens of millions in value",
      "Presenting the overlap adjustment increases credibility — it shows you've accounted for realistic interactions",
    ],
    citation: "Integration of Sackett et al. (2022), Morrow et al. (1997), and Locke & Latham (2002)",
    rAppSource: "New capability in UA+ (combines models from multiple R apps)",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 8. CONTINGENT WORKERS
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "contingent-workforce",
    title: "Should You Use Part-Time or Contract Workers?",
    subtitle: "Fisher & Connelly (2017): The hidden costs of contingent labor",
    category: "Workforce Strategy",
    difficulty: "intermediate",
    estimatedMinutes: 12,
    icon: "👥",
    color: "#059669",
    narrative:
      "Your organization is considering using temporary workers or independent contractors instead of permanent hires. Fisher & Connelly (2017) built a utility model showing that the answer depends heavily on turnover rates and HR strategy. In their analysis, permanent employees (net value: $6,532) often outperformed temporaries (net value: −$5,093) once turnover costs were included.",
    learningObjectives: [
      "Understand the full cost of contingent labor beyond wages",
      "See how turnover costs dramatically affect worker value calculations",
      "Compare different HR strategies for workforce composition",
      "Build a data-driven case for workforce mix decisions",
    ],
    interventions: [
      {
        id: "scenario-cw-perm",
        type: "custom",
        label: "Invest in Permanent Staff",
        numEmployees: 30,
        avgSalary: 47295,
        sdyPercent: 40,
        timePeriodYears: 3,
        costPerEmployee: 500,
        fixedCost: 10000,
        effectSize: 0.35,
      },
    ],
    steps: [
      {
        title: "The Contingent Workforce Dilemma",
        content:
          "Many organizations use temporary workers to save money. But Fisher & Connelly (2017) showed that apparent savings in wages and benefits can be wiped out by higher turnover costs, lower service value, and coordination overhead.",
      },
      {
        title: "The Net Value Framework",
        content:
          "Net Value = Service Value − Service Costs\n\nService Value: how much value the worker produces\nService Costs: wages + benefits + behavioral costs + turnover costs\n\nPermanent employees: Net Value = $6,532/year\nIC (direct): Net Value = $27,176/year\nIC (agency): Net Value = $22,351/year\nTemporary: Net Value = −$5,093/year (negative!)",
      },
      {
        title: "Why Temporaries Can Lose Money",
        content:
          "Temporary workers have 30% turnover. Each turnover event costs roughly $23,837 in recruitment, training, and lost productivity. This single factor swings their net value from positive to negative, despite lower wages (84% of permanent).",
      },
      {
        title: "The Strategy That Works Best",
        content:
          "Fisher & Connelly found that 'Temp-to-Perm' strategies (hire as temp, convert top performers to permanent) generated the highest total workforce value ($1.27M for 33 workers). The worst strategy: heavy reliance on temporaries without a conversion path.",
      },
      {
        title: "Running This Scenario",
        content:
          "We model an investment in stabilizing permanent staff — reducing turnover through better selection and onboarding. This costs $500/employee but the performance improvement (d = 0.35) plus reduced turnover generates substantial returns over 3 years.",
      },
    ],
    expectedResults: [
      {
        label: "Net Benefit of Investing in Permanent Staff",
        value: "~$340K",
        explanation:
          "Investing in 30 permanent employees generates substantial returns when you account for avoided turnover costs.",
      },
      {
        label: "Permanent Net Value (reference)",
        value: "$6,532/year",
        explanation:
          "From Fisher & Connelly (2017) — permanent employees generate positive net value after all costs.",
      },
      {
        label: "Temporary Net Value (reference)",
        value: "−$5,093/year",
        explanation:
          "Temporary workers actually cost money when you include turnover costs.",
      },
    ],
    takeaways: [
      "Cheap labor isn't always cheap — turnover costs can make temporary workers a net loss",
      "Temp-to-perm strategies balance flexibility with the benefits of permanent employment",
      "Investing in permanent employee retention has compounding returns over time",
      "Always include turnover costs in workforce mix decisions — they change the calculus dramatically",
    ],
    citation: "Fisher, S. L., & Connelly, C. E. (2017). Lower cost or just lower value? Modeling the organizational costs and benefits of contingent work. Academy of Management Discoveries, 3(2), 165–186.",
    rAppSource: "fisher_connelly_2017_app.R",
  },

  // ═══════════════════════════════════════════════════════════════════════════
  // 9. TRAINING ROI CALCULATOR
  // ═══════════════════════════════════════════════════════════════════════════
  {
    id: "training-roi-calculator",
    title: "Is Your Training Program Worth It?",
    subtitle: "A general-purpose ROI calculator for any training intervention",
    category: "Training & Development",
    difficulty: "beginner",
    estimatedMinutes: 8,
    icon: "🧮",
    color: "#2563eb",
    narrative:
      "You're designing a leadership development program for 75 participants. The program costs $40,000 to develop, $800/participant for delivery, $150/participant for materials, and $600/participant in opportunity cost (time away from work). The expected effect is d = 0.40 (typical for training), benefits last 2.5 years with 15% annual decay. Average salary is $70,000.",
    learningObjectives: [
      "Break down the full cost of a training program",
      "Understand benefit decay over time",
      "Calculate whether a specific training investment is worthwhile",
    ],
    interventions: [
      {
        id: "scenario-roi-1",
        type: "training",
        label: "Leadership Development Program",
        numEmployees: 75,
        avgSalary: 70000,
        sdyPercent: 40,
        timePeriodYears: 2,
        costPerEmployee: 1550,
        fixedCost: 40000,
        effectSize: 0.40,
      },
    ],
    steps: [
      {
        title: "Itemizing the Costs",
        content:
          "Total cost per participant:\n• Delivery: $800\n• Materials: $150\n• Opportunity cost: $600\n• Total per person: $1,550\n\nPlus $40,000 in fixed development costs.\nGrand total: 75 × $1,550 + $40,000 = $156,250",
      },
      {
        title: "The Effect Size",
        content:
          "d = 0.40 is a typical training effect. It means trained employees perform 0.40 standard deviations better than untrained ones. For a $70,000 salary (SDy = $28,000), this translates to $11,200 more value per employee per year.",
        highlight: "effectSize",
      },
      {
        title: "Benefit Duration",
        content:
          "We conservatively estimate benefits last 2 years. Some training effects persist longer, some fade. The original R app included a 15% annual decay option — for this simplified version, we use a shorter duration as a conservative proxy.",
        highlight: "timePeriodYears",
      },
      {
        title: "Calculate and Evaluate",
        content:
          "Run the analysis. Ask yourself:\n• Is the ROI high enough to justify the investment?\n• How does the break-even effect size compare to the expected d = 0.40?\n• Would you recommend this program to leadership?",
      },
    ],
    expectedResults: [
      {
        label: "Net Benefit",
        value: "~$1.5M",
        explanation: "75 participants × 2 years × 0.40 × $28,000 − $156,250 in costs.",
      },
      {
        label: "ROI",
        value: "~11:1",
        explanation: "Every dollar invested returns about $11 in performance value.",
      },
    ],
    takeaways: [
      "Most well-designed training programs generate positive ROI — the question is how much",
      "Opportunity costs (time away from work) are a real cost often overlooked in training budgets",
      "Even conservative estimates usually show training is worthwhile if the effect size exceeds d = 0.05-0.10",
    ],
    citation: "Based on training_utility_app.R ROI Calculator tab parameters",
    rAppSource: "training_utility_app.R (ROI Calculator tab)",
  },
];

export function getScenariosByCategory(): Record<string, GuidedScenario[]> {
  const cats: Record<string, GuidedScenario[]> = {};
  for (const s of GUIDED_SCENARIOS) {
    if (!cats[s.category]) cats[s.category] = [];
    cats[s.category].push(s);
  }
  return cats;
}

export function getScenarioById(id: string): GuidedScenario | undefined {
  return GUIDED_SCENARIOS.find((s) => s.id === id);
}
