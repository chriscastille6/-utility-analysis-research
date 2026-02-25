/**
 * Unified Utility Analysis Computation Engine
 *
 * Ports the validated R models from the Utility Analysis Research codebase
 * into TypeScript. Each model corresponds to its R counterpart:
 *
 *   BCG Model          → app.R, staffing_utility_app_fixed.R
 *   Training Utility    → training_utility_app.R
 *   Job Crafting        → job_crafting_utility_app.R
 *   Contingent Workers  → fisher_connelly_2017_app.R
 *   Sturman Adjustments → scripts/utilities/sturman_utility_functions.R
 *   Combination Model   → new (multivariate framework)
 */

import { ux } from "../stats";

// ─── Types ──────────────────────────────────────────────────────────────────

export type InterventionType =
  | "selection"
  | "training"
  | "job_crafting"
  | "contingent_workforce"
  | "compensation"
  | "leadership_development"
  | "goal_setting"
  | "custom";

export interface InterventionParams {
  id: string;
  type: InterventionType;
  label: string;

  // Common parameters
  numEmployees: number;
  avgSalary: number;
  sdyPercent: number; // SDy as % of salary (typically 40%)
  timePeriodYears: number;
  costPerEmployee: number;
  fixedCost: number;

  // Selection-specific
  selectionRatio?: number;  // proportion selected (0-1)
  validityCoefficient?: number; // rxy
  currentValidity?: number; // existing system validity (for incremental)

  // Training/intervention-specific
  effectSize?: number; // Cohen's d

  // Contingent workforce-specific
  workerType?: "permanent" | "contractor_direct" | "contractor_agency" | "temporary";
  wageMultiplier?: number;
  benefitsPercent?: number;
  turnoverRate?: number;

  // Economic adjustments (Sturman 2000)
  applyEconomicAdjustments?: boolean;
  variableCostPercent?: number;
  taxRate?: number;
  discountRate?: number;
}

export interface UtilityResult {
  interventionId: string;
  interventionLabel: string;
  interventionType: InterventionType;

  grossBenefit: number;
  totalCost: number;
  netBenefit: number;
  netBenefitPerEmployee: number;
  roi: number; // as ratio (e.g., 5.0 = 5:1)
  roiPercent: number;
  breakEvenEffectSize: number;
  breakEvenCost: number;

  // Per-year breakdown
  yearlyBreakdown: { year: number; benefit: number; cost: number; net: number; cumulative: number }[];

  // Sensitivity data
  sensitivityToEffectSize: { effectSize: number; net: number }[];
  sensitivityToSDy: { sdyPercent: number; net: number }[];

  // For display
  sdy: number;
  effectSizeUsed: number;
  formulaDescription: string;
}

export interface CombinedResult {
  individualResults: UtilityResult[];
  combinedNetBenefit: number;
  combinedROI: number;
  combinedGross: number;
  combinedCost: number;
  overlapFactor: number;
  independentTotal: number; // sum without overlap adjustment
  overlapReduction: number; // how much was reduced
  interventionContributions: { label: string; contribution: number; percent: number }[];
}

// ─── Core Model: Brogden-Cronbach-Gleser ────────────────────────────────────

function computeSelectionUtility(p: InterventionParams): UtilityResult {
  const N = p.numEmployees;
  const sdy = p.avgSalary * (p.sdyPercent / 100);
  const sr = p.selectionRatio ?? 0.5;
  const rxy = p.validityCoefficient ?? 0.3;
  const t = p.timePeriodYears;
  const costPerHire = p.costPerEmployee;
  const applicants = Math.ceil(N / sr);

  const uxVal = ux(sr);

  // Incremental validity if current system exists
  const rCurrent = p.currentValidity ?? 0;
  const effectiveR = rxy - rCurrent;

  let grossBenefit = N * t * effectiveR * sdy * uxVal;
  const totalCost = applicants * costPerHire + p.fixedCost;

  // Sturman economic adjustments
  if (p.applyEconomicAdjustments) {
    const vc = p.variableCostPercent ?? 10;
    const tax = p.taxRate ?? 30;
    const disc = p.discountRate ?? 8;

    let adjusted = 0;
    for (let yr = 0; yr < t; yr++) {
      const yearBenefit = N * effectiveR * sdy * uxVal;
      const discountFactor = Math.pow(1 / (1 + disc / 100), yr);
      adjusted += yearBenefit * (1 + vc / 100) * (1 - tax / 100) * discountFactor;
    }
    grossBenefit = adjusted;
  }

  const netBenefit = grossBenefit - totalCost;

  // Break-even: what effect size (rxy) makes net = 0?
  const breakEvenR = totalCost / (N * t * sdy * uxVal);
  const breakEvenCost = grossBenefit;

  // Yearly breakdown
  const yearlyBreakdown = [];
  let cumulative = -totalCost;
  for (let yr = 1; yr <= Math.max(t, 5); yr++) {
    const yrBenefit = yr <= t ? N * effectiveR * sdy * uxVal : 0;
    const yrCost = yr === 1 ? totalCost : 0;
    cumulative += yrBenefit - yrCost;
    yearlyBreakdown.push({ year: yr, benefit: yrBenefit, cost: yrCost, net: yrBenefit - yrCost, cumulative });
  }

  // Sensitivity analysis
  const sensitivityToEffectSize = [];
  for (let d = 0.05; d <= 0.80; d += 0.05) {
    sensitivityToEffectSize.push({
      effectSize: d,
      net: N * t * d * sdy * uxVal - totalCost,
    });
  }

  const sensitivityToSDy = [];
  for (let pct = 20; pct <= 80; pct += 5) {
    const sdyTest = p.avgSalary * (pct / 100);
    sensitivityToSDy.push({
      sdyPercent: pct,
      net: N * t * effectiveR * sdyTest * uxVal - totalCost,
    });
  }

  return {
    interventionId: p.id,
    interventionLabel: p.label,
    interventionType: p.type,
    grossBenefit,
    totalCost,
    netBenefit,
    netBenefitPerEmployee: netBenefit / N,
    roi: totalCost > 0 ? grossBenefit / totalCost : Infinity,
    roiPercent: totalCost > 0 ? ((grossBenefit - totalCost) / totalCost) * 100 : Infinity,
    breakEvenEffectSize: breakEvenR,
    breakEvenCost: breakEvenCost,
    yearlyBreakdown,
    sensitivityToEffectSize,
    sensitivityToSDy,
    sdy,
    effectSizeUsed: effectiveR,
    formulaDescription: `ΔU = N × t × r_xy × SDy × λ(ϕ) − C = ${N} × ${t} × ${effectiveR.toFixed(2)} × $${sdy.toLocaleString()} × ${uxVal.toFixed(3)} − $${totalCost.toLocaleString()}`,
  };
}

// ─── Core Model: Training / Intervention Utility ────────────────────────────

function computeTrainingUtility(p: InterventionParams): UtilityResult {
  const N = p.numEmployees;
  const sdy = p.avgSalary * (p.sdyPercent / 100);
  const d = p.effectSize ?? 0.4;
  const t = p.timePeriodYears;
  const totalCost = N * p.costPerEmployee + p.fixedCost;

  let grossBenefit = N * t * d * sdy;

  if (p.applyEconomicAdjustments) {
    const vc = p.variableCostPercent ?? 10;
    const tax = p.taxRate ?? 30;
    const disc = p.discountRate ?? 8;
    let adjusted = 0;
    for (let yr = 0; yr < t; yr++) {
      const discountFactor = Math.pow(1 / (1 + disc / 100), yr);
      adjusted += N * d * sdy * (1 + vc / 100) * (1 - tax / 100) * discountFactor;
    }
    grossBenefit = adjusted;
  }

  const netBenefit = grossBenefit - totalCost;
  const breakEvenD = totalCost / (N * t * sdy);
  const breakEvenCost = grossBenefit;

  const yearlyBreakdown = [];
  let cumulative = -totalCost;
  for (let yr = 1; yr <= Math.max(t, 5); yr++) {
    const yrBenefit = yr <= t ? N * d * sdy : 0;
    const yrCost = yr === 1 ? totalCost : 0;
    cumulative += yrBenefit - yrCost;
    yearlyBreakdown.push({ year: yr, benefit: yrBenefit, cost: yrCost, net: yrBenefit - yrCost, cumulative });
  }

  const sensitivityToEffectSize = [];
  for (let es = 0.05; es <= 1.2; es += 0.05) {
    sensitivityToEffectSize.push({ effectSize: es, net: N * t * es * sdy - totalCost });
  }

  const sensitivityToSDy = [];
  for (let pct = 20; pct <= 80; pct += 5) {
    const sdyTest = p.avgSalary * (pct / 100);
    sensitivityToSDy.push({ sdyPercent: pct, net: N * t * d * sdyTest - totalCost });
  }

  return {
    interventionId: p.id,
    interventionLabel: p.label,
    interventionType: p.type,
    grossBenefit,
    totalCost,
    netBenefit,
    netBenefitPerEmployee: netBenefit / N,
    roi: totalCost > 0 ? grossBenefit / totalCost : Infinity,
    roiPercent: totalCost > 0 ? ((grossBenefit - totalCost) / totalCost) * 100 : Infinity,
    breakEvenEffectSize: breakEvenD,
    breakEvenCost,
    yearlyBreakdown,
    sensitivityToEffectSize,
    sensitivityToSDy,
    sdy,
    effectSizeUsed: d,
    formulaDescription: `ΔU = N × t × d × SDy − C = ${N} × ${t} × ${d.toFixed(2)} × $${sdy.toLocaleString()} − $${totalCost.toLocaleString()}`,
  };
}

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function computeUtility(params: InterventionParams): UtilityResult {
  switch (params.type) {
    case "selection":
      return computeSelectionUtility(params);
    case "training":
    case "job_crafting":
    case "leadership_development":
    case "goal_setting":
    case "compensation":
    case "custom":
      return computeTrainingUtility(params);
    case "contingent_workforce":
      return computeTrainingUtility(params); // uses same d-based framework
    default:
      return computeTrainingUtility(params);
  }
}

// ─── Combination Engine ─────────────────────────────────────────────────────

/**
 * Combines multiple intervention utilities accounting for overlap.
 *
 * The key insight: when interventions target overlapping aspects of job
 * performance, their combined effect is less than the sum of parts.
 *
 * We use a configurable overlap factor (0 = independent, 1 = fully redundant).
 * For interventions in different domains (e.g., selection + training),
 * default overlap is low (~0.15). For same domain, it's higher (~0.50).
 *
 * The combined effect size uses the multivariate framework:
 *   d_combined = √(Σ dᵢ² + 2 × Σᵢ<ⱼ ρᵢⱼ × dᵢ × dⱼ)
 *
 * With overlap factor ω, the effective ρ between interventions is:
 *   ρᵢⱼ = ω  (user-adjustable, defaults based on domain similarity)
 *
 * This naturally produces diminishing returns for overlapping interventions.
 */
export function computeCombinedUtility(
  interventions: InterventionParams[],
  overlapFactor: number = 0.15
): CombinedResult {
  const individualResults = interventions.map(computeUtility);

  if (individualResults.length === 0) {
    return {
      individualResults: [],
      combinedNetBenefit: 0,
      combinedROI: 0,
      combinedGross: 0,
      combinedCost: 0,
      overlapFactor,
      independentTotal: 0,
      overlapReduction: 0,
      interventionContributions: [],
    };
  }

  if (individualResults.length === 1) {
    const r = individualResults[0];
    return {
      individualResults,
      combinedNetBenefit: r.netBenefit,
      combinedROI: r.roi,
      combinedGross: r.grossBenefit,
      combinedCost: r.totalCost,
      overlapFactor,
      independentTotal: r.netBenefit,
      overlapReduction: 0,
      interventionContributions: [
        { label: r.interventionLabel, contribution: r.netBenefit, percent: 100 },
      ],
    };
  }

  // Compute combined effect using multivariate formula
  const effectSizes = individualResults.map((r) => r.effectSizeUsed);

  // d²_combined = Σ dᵢ² + 2 × Σᵢ<ⱼ ρ × dᵢ × dⱼ
  let sumDSq = 0;
  let sumCross = 0;
  for (let i = 0; i < effectSizes.length; i++) {
    sumDSq += effectSizes[i] * effectSizes[i];
    for (let j = i + 1; j < effectSizes.length; j++) {
      sumCross += overlapFactor * effectSizes[i] * effectSizes[j];
    }
  }
  const dCombined = Math.sqrt(sumDSq + 2 * sumCross);

  // Independent total (naive sum)
  const independentGross = individualResults.reduce((s, r) => s + r.grossBenefit, 0);
  const totalCost = individualResults.reduce((s, r) => s + r.totalCost, 0);
  const independentTotal = independentGross - totalCost;

  // Combined gross uses the combined effect size
  const sumIndividualD = effectSizes.reduce((a, b) => a + b, 0);
  const combinedGross = independentGross * (dCombined / Math.max(sumIndividualD, 0.001));
  const combinedNetBenefit = combinedGross - totalCost;

  // Proportional contributions
  const totalIndividualNet = individualResults.reduce((s, r) => s + Math.max(r.netBenefit, 0), 0);
  const interventionContributions = individualResults.map((r) => ({
    label: r.interventionLabel,
    contribution: r.netBenefit,
    percent: totalIndividualNet > 0 ? (Math.max(r.netBenefit, 0) / totalIndividualNet) * 100 : 0,
  }));

  return {
    individualResults,
    combinedNetBenefit,
    combinedROI: totalCost > 0 ? combinedGross / totalCost : 0,
    combinedGross,
    combinedCost: totalCost,
    overlapFactor,
    independentTotal,
    overlapReduction: independentTotal - combinedNetBenefit,
    interventionContributions,
  };
}

// ─── Monte Carlo Simulation ─────────────────────────────────────────────────

export interface MonteCarloResult {
  mean: number;
  median: number;
  sd: number;
  p5: number;
  p25: number;
  p75: number;
  p95: number;
  histogram: { bin: number; count: number }[];
  positiveProb: number;
}

export function runMonteCarlo(
  params: InterventionParams,
  nSims: number = 5000,
  paramRanges?: {
    effectSizeRange?: [number, number];
    sdyRange?: [number, number];
    nRange?: [number, number];
  }
): MonteCarloResult {
  const results: number[] = [];
  const d = params.effectSize ?? params.validityCoefficient ?? 0.3;
  const sdy = params.avgSalary * (params.sdyPercent / 100);

  const dRange = paramRanges?.effectSizeRange ?? [d * 0.5, d * 1.5];
  const sdyRange = paramRanges?.sdyRange ?? [sdy * 0.6, sdy * 1.4];
  const nRange = paramRanges?.nRange ?? [params.numEmployees, params.numEmployees];

  for (let i = 0; i < nSims; i++) {
    const simD = dRange[0] + Math.random() * (dRange[1] - dRange[0]);
    const simSDy = sdyRange[0] + Math.random() * (sdyRange[1] - sdyRange[0]);
    const simN = Math.round(nRange[0] + Math.random() * (nRange[1] - nRange[0]));
    const simCost = simN * params.costPerEmployee + params.fixedCost;

    let gross: number;
    if (params.type === "selection") {
      const sr = params.selectionRatio ?? 0.5;
      gross = simN * params.timePeriodYears * simD * simSDy * ux(sr);
    } else {
      gross = simN * params.timePeriodYears * simD * simSDy;
    }
    results.push(gross - simCost);
  }

  results.sort((a, b) => a - b);
  const mean = results.reduce((a, b) => a + b, 0) / nSims;
  const median = results[Math.floor(nSims / 2)];
  const variance = results.reduce((s, v) => s + (v - mean) ** 2, 0) / nSims;
  const sd = Math.sqrt(variance);

  // Histogram
  const min = results[0];
  const max = results[results.length - 1];
  const nBins = 30;
  const binWidth = (max - min) / nBins || 1;
  const histogram: { bin: number; count: number }[] = [];
  for (let i = 0; i < nBins; i++) {
    const binStart = min + i * binWidth;
    histogram.push({ bin: binStart + binWidth / 2, count: 0 });
  }
  for (const v of results) {
    const idx = Math.min(Math.floor((v - min) / binWidth), nBins - 1);
    histogram[idx].count++;
  }

  return {
    mean,
    median,
    sd,
    p5: results[Math.floor(0.05 * nSims)],
    p25: results[Math.floor(0.25 * nSims)],
    p75: results[Math.floor(0.75 * nSims)],
    p95: results[Math.floor(0.95 * nSims)],
    histogram,
    positiveProb: results.filter((v) => v > 0).length / nSims * 100,
  };
}
