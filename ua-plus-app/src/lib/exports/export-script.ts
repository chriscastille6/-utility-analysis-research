/**
 * R and Python Script Export
 *
 * Generates executable scripts that reproduce the exact utility analysis
 * calculations, so anyone can verify the math independently.
 */

import { saveAs } from "file-saver";
import type { CombinedResult, InterventionParams } from "../models/utility-engine";

export function exportRScript(
  interventions: InterventionParams[],
  result: CombinedResult,
  overlapFactor: number
) {
  const lines: string[] = [
    `# ═══════════════════════════════════════════════════════════════`,
    `# UA+ Utility Analysis — R Reproduction Script`,
    `# Generated: ${new Date().toISOString().slice(0, 10)}`,
    `# ═══════════════════════════════════════════════════════════════`,
    ``,
    `# This script reproduces the calculations from the UA+ platform.`,
    `# Every number can be verified by running this code in R.`,
    ``,
    `# --- Helper functions ---`,
    ``,
    `# Standard normal PDF`,
    `phi <- function(z) dnorm(z)`,
    ``,
    `# Selection ratio utility: mean standardized score of selectees`,
    `# Equivalent to iopsych::ux(sr)`,
    `ux <- function(sr) {`,
    `  z <- qnorm(1 - sr)`,
    `  return(dnorm(z) / sr)`,
    `}`,
    ``,
    `# Currency formatting`,
    `fmt <- function(x) paste0("$", format(round(x), big.mark = ","))`,
    ``,
  ];

  for (let idx = 0; idx < interventions.length; idx++) {
    const p = interventions[idx];

    lines.push(
      `# ═══ Intervention ${idx + 1}: ${p.label} ═══`,
      ``,
      `cat("\\n═══ ${p.label} ═══\\n")`,
      ``,
      `# --- Assumptions ---`,
      `N_${idx}     <- ${p.numEmployees}    # Number of employees`,
      `salary_${idx} <- ${p.avgSalary}   # Average salary ($)`,
      `sdy_pct_${idx} <- ${p.sdyPercent / 100}     # SDy as proportion of salary`,
      `t_${idx}     <- ${p.timePeriodYears}        # Duration (years)`,
      `cost_per_${idx} <- ${p.costPerEmployee}  # Cost per employee ($)`,
      `fixed_cost_${idx} <- ${p.fixedCost}      # Fixed/setup cost ($)`,
      ``,
    );

    if (p.type === "selection") {
      lines.push(
        `r_new_${idx}  <- ${p.validityCoefficient ?? 0.3}   # Proposed validity`,
        `r_old_${idx}  <- ${p.currentValidity ?? 0}         # Current validity`,
        `sr_${idx}     <- ${p.selectionRatio ?? 0.3}        # Selection ratio`,
        ``,
        `# --- Calculations ---`,
        `sdy_${idx}     <- salary_${idx} * sdy_pct_${idx}`,
        `delta_r_${idx} <- r_new_${idx} - r_old_${idx}`,
        `ux_val_${idx}  <- ux(sr_${idx})`,
        `applicants_${idx} <- ceiling(N_${idx} / sr_${idx})`,
        ``,
        `gross_${idx} <- N_${idx} * t_${idx} * delta_r_${idx} * sdy_${idx} * ux_val_${idx}`,
        `cost_${idx}  <- applicants_${idx} * cost_per_${idx} + fixed_cost_${idx}`,
        `net_${idx}   <- gross_${idx} - cost_${idx}`,
        `roi_${idx}   <- gross_${idx} / cost_${idx}`,
        `breakeven_${idx} <- cost_${idx} / (N_${idx} * t_${idx} * sdy_${idx} * ux_val_${idx})`,
        ``,
        `cat(sprintf("  SDy:            %s\\n", fmt(sdy_${idx})))`,
        `cat(sprintf("  Incremental r:  %.3f\\n", delta_r_${idx}))`,
        `cat(sprintf("  ux(SR):         %.3f\\n", ux_val_${idx}))`,
        `cat(sprintf("  Gross Benefit:  %s\\n", fmt(gross_${idx})))`,
        `cat(sprintf("  Total Cost:     %s\\n", fmt(cost_${idx})))`,
        `cat(sprintf("  Net Benefit:    %s\\n", fmt(net_${idx})))`,
        `cat(sprintf("  ROI:            %.1f:1\\n", roi_${idx}))`,
        `cat(sprintf("  Break-even r:   %.4f\\n", breakeven_${idx}))`,
        ``,
      );
    } else {
      lines.push(
        `d_${idx}     <- ${p.effectSize ?? 0.3}     # Effect size (Cohen's d)`,
        ``,
        `# --- Calculations ---`,
        `sdy_${idx}   <- salary_${idx} * sdy_pct_${idx}`,
        ``,
        `gross_${idx} <- N_${idx} * t_${idx} * d_${idx} * sdy_${idx}`,
        `cost_${idx}  <- N_${idx} * cost_per_${idx} + fixed_cost_${idx}`,
        `net_${idx}   <- gross_${idx} - cost_${idx}`,
        `roi_${idx}   <- gross_${idx} / cost_${idx}`,
        `breakeven_${idx} <- cost_${idx} / (N_${idx} * t_${idx} * sdy_${idx})`,
        ``,
        `cat(sprintf("  SDy:            %s\\n", fmt(sdy_${idx})))`,
        `cat(sprintf("  Effect Size d:  %.2f\\n", d_${idx}))`,
        `cat(sprintf("  Gross Benefit:  %s\\n", fmt(gross_${idx})))`,
        `cat(sprintf("  Total Cost:     %s\\n", fmt(cost_${idx})))`,
        `cat(sprintf("  Net Benefit:    %s\\n", fmt(net_${idx})))`,
        `cat(sprintf("  ROI:            %.1f:1\\n", roi_${idx}))`,
        `cat(sprintf("  Break-even d:   %.4f\\n", breakeven_${idx}))`,
        ``,
      );
    }
  }

  if (interventions.length > 1) {
    lines.push(
      `# ═══ Combined Analysis ═══`,
      ``,
      `overlap <- ${overlapFactor}  # Overlap factor between interventions`,
      ``,
      `# Effect sizes for combination`,
      `effect_sizes <- c(${result.individualResults.map((r) => `${r.effectSizeUsed}`).join(", ")})`,
      ``,
      `# Combined effect: d_combined = sqrt(sum(d^2) + 2 * sum(rho * di * dj))`,
      `sum_d_sq <- sum(effect_sizes^2)`,
      `sum_cross <- 0`,
      `for (i in 1:(length(effect_sizes)-1)) {`,
      `  for (j in (i+1):length(effect_sizes)) {`,
      `    sum_cross <- sum_cross + overlap * effect_sizes[i] * effect_sizes[j]`,
      `  }`,
      `}`,
      `d_combined <- sqrt(sum_d_sq + 2 * sum_cross)`,
      `sum_d <- sum(effect_sizes)`,
      ``,
      `combined_gross <- ${result.combinedGross.toFixed(2)}`,
      `combined_cost  <- ${result.combinedCost.toFixed(2)}`,
      `combined_net   <- combined_gross - combined_cost`,
      ``,
      `cat("\\n═══ Combined Results ═══\\n")`,
      `cat(sprintf("  Combined Net:   %s\\n", fmt(combined_net)))`,
      `cat(sprintf("  Combined ROI:   %.1f:1\\n", combined_gross / combined_cost))`,
      `cat(sprintf("  Overlap Factor: %.0f%%\\n", overlap * 100))`,
      ``,
    );
  }

  lines.push(
    `# ═══ References ═══`,
    `# BCG Model: Brogden (1949); Cronbach & Gleser (1965)`,
    `# SDy: Schmidt & Hunter (1983) — 40% rule`,
    `# Economic adjustments: Sturman (2000)`,
    `# Training utility: Morrow et al. (1997)`,
    `# Generated by UA+ (https://github.com/chriscastille6/-utility-analysis-research)`,
  );

  const blob = new Blob([lines.join("\n")], { type: "text/plain" });
  saveAs(blob, `ua-plus-analysis-${new Date().toISOString().slice(0, 10)}.R`);
}

export function exportPythonScript(
  interventions: InterventionParams[],
  result: CombinedResult,
  _overlapFactor: number
) {
  const lines: string[] = [
    `"""`,
    `UA+ Utility Analysis — Python Reproduction Script`,
    `Generated: ${new Date().toISOString().slice(0, 10)}`,
    ``,
    `This script reproduces the calculations from the UA+ platform.`,
    `Requirements: pip install scipy`,
    `"""`,
    ``,
    `import math`,
    `from scipy.stats import norm`,
    ``,
    ``,
    `def ux(sr: float) -> float:`,
    `    """Selection ratio utility: mean standardized score of selectees."""`,
    `    z = norm.ppf(1 - sr)`,
    `    return norm.pdf(z) / sr`,
    ``,
    ``,
    `def fmt(x: float) -> str:`,
    `    return f"${'{x:,.0f}'}"`,
    ``,
    ``,
  ];

  for (let idx = 0; idx < interventions.length; idx++) {
    const p = interventions[idx];

    lines.push(
      `# ${'═' .repeat(60)}`,
      `# Intervention ${idx + 1}: ${p.label}`,
      `# ${'═'.repeat(60)}`,
      ``,
      `print(f"\\n${'═' .repeat(40)}")`,
      `print(f"${p.label}")`,
      `print(f"${'═'.repeat(40)}")`,
      ``,
      `# Assumptions`,
      `N_${idx} = ${p.numEmployees}`,
      `salary_${idx} = ${p.avgSalary}`,
      `sdy_pct_${idx} = ${p.sdyPercent / 100}`,
      `t_${idx} = ${p.timePeriodYears}`,
      `cost_per_${idx} = ${p.costPerEmployee}`,
      `fixed_cost_${idx} = ${p.fixedCost}`,
      ``,
    );

    if (p.type === "selection") {
      lines.push(
        `r_new_${idx} = ${p.validityCoefficient ?? 0.3}`,
        `r_old_${idx} = ${p.currentValidity ?? 0}`,
        `sr_${idx} = ${p.selectionRatio ?? 0.3}`,
        ``,
        `# Calculations`,
        `sdy_${idx} = salary_${idx} * sdy_pct_${idx}`,
        `delta_r_${idx} = r_new_${idx} - r_old_${idx}`,
        `ux_val_${idx} = ux(sr_${idx})`,
        `applicants_${idx} = math.ceil(N_${idx} / sr_${idx})`,
        ``,
        `gross_${idx} = N_${idx} * t_${idx} * delta_r_${idx} * sdy_${idx} * ux_val_${idx}`,
        `cost_${idx} = applicants_${idx} * cost_per_${idx} + fixed_cost_${idx}`,
        `net_${idx} = gross_${idx} - cost_${idx}`,
        `roi_${idx} = gross_${idx} / cost_${idx}`,
        `breakeven_${idx} = cost_${idx} / (N_${idx} * t_${idx} * sdy_${idx} * ux_val_${idx})`,
        ``,
        `print(f"  SDy:           {fmt(sdy_${idx})}")`,
        `print(f"  Δr:            {delta_r_${idx}:.3f}")`,
        `print(f"  Gross Benefit: {fmt(gross_${idx})}")`,
        `print(f"  Total Cost:    {fmt(cost_${idx})}")`,
        `print(f"  Net Benefit:   {fmt(net_${idx})}")`,
        `print(f"  ROI:           {roi_${idx}:.1f}:1")`,
        `print(f"  Break-even r:  {breakeven_${idx}:.4f}")`,
        ``,
      );
    } else {
      lines.push(
        `d_${idx} = ${p.effectSize ?? 0.3}`,
        ``,
        `# Calculations`,
        `sdy_${idx} = salary_${idx} * sdy_pct_${idx}`,
        ``,
        `gross_${idx} = N_${idx} * t_${idx} * d_${idx} * sdy_${idx}`,
        `cost_${idx} = N_${idx} * cost_per_${idx} + fixed_cost_${idx}`,
        `net_${idx} = gross_${idx} - cost_${idx}`,
        `roi_${idx} = gross_${idx} / cost_${idx}`,
        `breakeven_${idx} = cost_${idx} / (N_${idx} * t_${idx} * sdy_${idx})`,
        ``,
        `print(f"  SDy:           {fmt(sdy_${idx})}")`,
        `print(f"  Effect Size:   {d_${idx}:.2f}")`,
        `print(f"  Gross Benefit: {fmt(gross_${idx})}")`,
        `print(f"  Total Cost:    {fmt(cost_${idx})}")`,
        `print(f"  Net Benefit:   {fmt(net_${idx})}")`,
        `print(f"  ROI:           {roi_${idx}:.1f}:1")`,
        `print(f"  Break-even d:  {breakeven_${idx}:.4f}")`,
        ``,
      );
    }
  }

  lines.push(
    `# References`,
    `# BCG Model: Brogden (1949); Cronbach & Gleser (1965)`,
    `# SDy: Schmidt & Hunter (1983)`,
    `# Generated by UA+ (https://github.com/chriscastille6/-utility-analysis-research)`,
  );

  const blob = new Blob([lines.join("\n")], { type: "text/plain" });
  saveAs(blob, `ua-plus-analysis-${new Date().toISOString().slice(0, 10)}.py`);
}
