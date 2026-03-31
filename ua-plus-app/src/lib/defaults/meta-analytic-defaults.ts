/**
 * Meta-Analytic Defaults Database
 *
 * Evidence-based default values drawn from the I/O psychology literature.
 * Each default includes the source citation so the AI can explain provenance.
 *
 * Sources are drawn from the original R codebase's embedded research data
 * and the meta-analytic literature it references.
 */

import type { InterventionType, InterventionParams } from "../models/utility-engine";

export interface InterventionTemplate {
  type: InterventionType;
  label: string;
  description: string;
  category: string;
  icon: string;

  defaults: Partial<InterventionParams>;

  // Meta-analytic evidence
  evidence: {
    effectSize: number;
    effectSizeLabel: string;
    confidenceInterval?: [number, number];
    kStudies?: number;
    nParticipants?: number;
    source: string;
    year: number;
    notes?: string;
  };

  // What this looks like in business language
  businessExample: string;
  // Typical parameters
  typicalSalaryRange: [number, number];
  typicalCostRange: [number, number];
}

export const INTERVENTION_TEMPLATES: InterventionTemplate[] = [
  // ─── Selection / Staffing ───────────────────────────────────────────────
  {
    type: "selection",
    label: "Structured Interviews",
    description: "Replace unstructured interviews with structured, behaviorally-anchored interviews",
    category: "Selection & Staffing",
    icon: "UserCheck",
    defaults: {
      validityCoefficient: 0.51,
      currentValidity: 0.20,
      selectionRatio: 0.30,
      costPerEmployee: 150,
      fixedCost: 5000,
      sdyPercent: 40,
      timePeriodYears: 3,
    },
    evidence: {
      effectSize: 0.51,
      effectSizeLabel: "Validity coefficient (r)",
      kStudies: 100,
      source: "Schmidt & Hunter (1998); Sackett et al. (2022)",
      year: 2022,
      notes: "Structured interviews show substantially higher validity than unstructured (r = .20)",
    },
    businessExample: "Standardize your interview process with job-related questions, rating scales, and interviewer training",
    typicalSalaryRange: [40000, 120000],
    typicalCostRange: [50, 500],
  },
  {
    type: "selection",
    label: "Cognitive Ability Testing",
    description: "Add a validated cognitive ability test to the selection battery",
    category: "Selection & Staffing",
    icon: "Brain",
    defaults: {
      validityCoefficient: 0.51,
      currentValidity: 0.0,
      selectionRatio: 0.30,
      costPerEmployee: 50,
      fixedCost: 2000,
      sdyPercent: 40,
      timePeriodYears: 3,
    },
    evidence: {
      effectSize: 0.51,
      effectSizeLabel: "Validity coefficient (r)",
      kStudies: 500,
      nParticipants: 100000,
      source: "Schmidt & Hunter (1998); Sackett et al. (2022)",
      year: 2022,
      notes: "The single best predictor of job performance across occupations",
    },
    businessExample: "Administer a 30-minute ability test to all applicants before interviews",
    typicalSalaryRange: [30000, 150000],
    typicalCostRange: [20, 200],
  },
  {
    type: "selection",
    label: "Work Sample Tests",
    description: "Administer job-relevant work sample or simulation exercises",
    category: "Selection & Staffing",
    icon: "ClipboardCheck",
    defaults: {
      validityCoefficient: 0.33,
      currentValidity: 0.0,
      selectionRatio: 0.30,
      costPerEmployee: 200,
      fixedCost: 10000,
      sdyPercent: 40,
      timePeriodYears: 3,
    },
    evidence: {
      effectSize: 0.33,
      effectSizeLabel: "Validity coefficient (r)",
      source: "Sackett et al. (2022)",
      year: 2022,
    },
    businessExample: "Have applicants complete a realistic job task or in-basket exercise",
    typicalSalaryRange: [35000, 100000],
    typicalCostRange: [100, 500],
  },
  {
    type: "selection",
    label: "Assessment Center",
    description: "Multi-method, multi-assessor evaluation for managerial positions",
    category: "Selection & Staffing",
    icon: "Users",
    defaults: {
      validityCoefficient: 0.36,
      currentValidity: 0.0,
      selectionRatio: 0.20,
      costPerEmployee: 2500,
      fixedCost: 25000,
      sdyPercent: 40,
      timePeriodYears: 5,
    },
    evidence: {
      effectSize: 0.36,
      effectSizeLabel: "Validity coefficient (r)",
      source: "Sackett et al. (2022); Arthur et al. (2003)",
      year: 2022,
    },
    businessExample: "Run a 1-2 day assessment center with simulations, role-plays, and group exercises for leadership candidates",
    typicalSalaryRange: [60000, 200000],
    typicalCostRange: [1000, 5000],
  },

  // ─── Training & Development ─────────────────────────────────────────────
  {
    type: "training",
    label: "Technical/Sales Training",
    description: "Job-specific technical skills or sales methodology training",
    category: "Training & Development",
    icon: "GraduationCap",
    defaults: {
      effectSize: 0.64,
      costPerEmployee: 1500,
      fixedCost: 5000,
      sdyPercent: 40,
      timePeriodYears: 2,
    },
    evidence: {
      effectSize: 0.64,
      effectSizeLabel: "Cohen's d",
      source: "Morrow et al. (1997); Arthur et al. (2003)",
      year: 1997,
      notes: "Highest ROI among training types. Large effect on job performance.",
    },
    businessExample: "Implement a structured sales methodology or technical certification program",
    typicalSalaryRange: [35000, 100000],
    typicalCostRange: [500, 5000],
  },
  {
    type: "training",
    label: "Managerial Training",
    description: "General management skills development program",
    category: "Training & Development",
    icon: "Briefcase",
    defaults: {
      effectSize: 0.31,
      costPerEmployee: 2500,
      fixedCost: 10000,
      sdyPercent: 40,
      timePeriodYears: 2,
    },
    evidence: {
      effectSize: 0.31,
      effectSizeLabel: "Cohen's d",
      source: "Morrow et al. (1997)",
      year: 1997,
      notes: "Small-medium effect. Often struggles to break even due to higher cost.",
    },
    businessExample: "General management development program for mid-level managers",
    typicalSalaryRange: [60000, 150000],
    typicalCostRange: [1000, 10000],
  },
  {
    type: "leadership_development",
    label: "Leadership Development",
    description: "Targeted leadership competency development program",
    category: "Training & Development",
    icon: "Award",
    defaults: {
      effectSize: 0.35,
      costPerEmployee: 3000,
      fixedCost: 15000,
      sdyPercent: 40,
      timePeriodYears: 3,
    },
    evidence: {
      effectSize: 0.35,
      effectSizeLabel: "Cohen's d",
      source: "Avolio et al. (2009); Lacerenza et al. (2017)",
      year: 2017,
      kStudies: 335,
      notes: "Effect varies by content (higher for knowledge/expertise outcomes).",
    },
    businessExample: "Structured leadership program with coaching, 360 feedback, and action learning",
    typicalSalaryRange: [80000, 200000],
    typicalCostRange: [2000, 15000],
  },

  // ─── Goal Setting & Performance Management ──────────────────────────────
  {
    type: "goal_setting",
    label: "Goal Setting Program",
    description: "Implement systematic goal setting (SMART goals, OKRs, etc.)",
    category: "Performance Management",
    icon: "Target",
    defaults: {
      effectSize: 0.46,
      costPerEmployee: 200,
      fixedCost: 5000,
      sdyPercent: 40,
      timePeriodYears: 2,
    },
    evidence: {
      effectSize: 0.46,
      effectSizeLabel: "Cohen's d",
      source: "Locke & Latham (2002); Schmidt (2013)",
      year: 2013,
      kStudies: 600,
      notes: "One of the most robust findings in organizational psychology. Very high ROI due to low cost.",
    },
    businessExample: "Train managers on goal-setting theory, implement quarterly goal cycles with specific, measurable targets",
    typicalSalaryRange: [30000, 120000],
    typicalCostRange: [50, 500],
  },

  // ─── Job Crafting ───────────────────────────────────────────────────────
  {
    type: "job_crafting",
    label: "Job Crafting Intervention",
    description: "Enable employees to reshape their job tasks, relationships, and perceptions",
    category: "Job Design & Engagement",
    icon: "Wrench",
    defaults: {
      effectSize: 0.47,
      costPerEmployee: 40,
      fixedCost: 2000,
      sdyPercent: 40,
      timePeriodYears: 1,
    },
    evidence: {
      effectSize: 0.47,
      effectSizeLabel: "Hedges' g (task performance, healthcare)",
      kStudies: 14,
      nParticipants: 1204,
      source: "Oprea et al. (2019)",
      year: 2019,
      confidenceInterval: [0.26, 0.68],
      notes: "Effect varies by industry. Healthcare shows strongest evidence. Overall g = 0.26 across contexts.",
    },
    businessExample: "Workshops helping employees identify strengths and redesign task boundaries, relationships, and meaning",
    typicalSalaryRange: [30000, 80000],
    typicalCostRange: [20, 200],
  },

  // ─── Compensation ───────────────────────────────────────────────────────
  {
    type: "compensation",
    label: "Pay-for-Performance System",
    description: "Link compensation to individual or team performance metrics",
    category: "Compensation & Benefits",
    icon: "DollarSign",
    defaults: {
      effectSize: 0.32,
      costPerEmployee: 0,
      fixedCost: 20000,
      sdyPercent: 40,
      timePeriodYears: 3,
    },
    evidence: {
      effectSize: 0.32,
      effectSizeLabel: "Cohen's d",
      source: "Cerasoli et al. (2014); Garbers & Konradt (2014)",
      year: 2014,
      notes: "Effect depends on task type. Stronger for quantity-based tasks. Cost = incremental comp budget.",
    },
    businessExample: "Redesign compensation to include meaningful performance-based bonuses or merit increases",
    typicalSalaryRange: [40000, 150000],
    typicalCostRange: [0, 5000],
  },

  // ─── Custom ─────────────────────────────────────────────────────────────
  {
    type: "custom",
    label: "Custom Intervention",
    description: "Define your own intervention with custom parameters",
    category: "Custom",
    icon: "Settings",
    defaults: {
      effectSize: 0.30,
      costPerEmployee: 500,
      fixedCost: 5000,
      sdyPercent: 40,
      timePeriodYears: 2,
    },
    evidence: {
      effectSize: 0.30,
      effectSizeLabel: "Cohen's d",
      source: "User-specified",
      year: 2024,
    },
    businessExample: "Any HR intervention — enter your own parameters and evidence",
    typicalSalaryRange: [30000, 200000],
    typicalCostRange: [0, 10000],
  },
];

/** Lookup by type */
export function getTemplateByType(type: InterventionType): InterventionTemplate | undefined {
  return INTERVENTION_TEMPLATES.find((t) => t.type === type);
}

/** Get templates by category */
export function getTemplatesByCategory(): Record<string, InterventionTemplate[]> {
  const categories: Record<string, InterventionTemplate[]> = {};
  for (const t of INTERVENTION_TEMPLATES) {
    if (!categories[t.category]) categories[t.category] = [];
    categories[t.category].push(t);
  }
  return categories;
}

/** SDy estimation guidelines */
export const SDY_GUIDELINES = {
  "40%": {
    label: "Standard Estimate (40% of salary)",
    description: "Schmidt & Hunter (1983) — widely used default",
    source: "Schmidt, F. L., & Hunter, J. E. (1983)",
  },
  "20%": {
    label: "Conservative (20% of salary)",
    description: "Lower bound, appropriate for highly standardized jobs",
    source: "Boudreau (1983)",
  },
  "60%": {
    label: "High Variability (60% of salary)",
    description: "Appropriate for complex jobs with high performance variability (executives, sales)",
    source: "Hunter et al. (1990)",
  },
};

/** Domain overlap defaults for combination analysis */
export const OVERLAP_DEFAULTS: Record<string, Record<string, number>> = {
  selection:           { selection: 0.50, training: 0.15, job_crafting: 0.10, goal_setting: 0.10, compensation: 0.10, leadership_development: 0.15 },
  training:            { selection: 0.15, training: 0.50, job_crafting: 0.20, goal_setting: 0.20, compensation: 0.15, leadership_development: 0.40 },
  job_crafting:        { selection: 0.10, training: 0.20, job_crafting: 0.50, goal_setting: 0.25, compensation: 0.15, leadership_development: 0.20 },
  goal_setting:        { selection: 0.10, training: 0.20, job_crafting: 0.25, goal_setting: 0.50, compensation: 0.20, leadership_development: 0.25 },
  compensation:        { selection: 0.10, training: 0.15, job_crafting: 0.15, goal_setting: 0.20, compensation: 0.50, leadership_development: 0.15 },
  leadership_development: { selection: 0.15, training: 0.40, job_crafting: 0.20, goal_setting: 0.25, compensation: 0.15, leadership_development: 0.50 },
};
