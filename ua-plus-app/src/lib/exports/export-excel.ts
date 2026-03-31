/**
 * Excel Export with Cell-Referenced Formulas
 *
 * Generates a .xlsx workbook where:
 * - "Assumptions" sheet holds all input parameters in named cells
 * - "Calculations" sheet references the assumptions via cell addresses
 * - "Results" sheet shows final outputs with formulas tracing back to inputs
 *
 * This lets anyone audit or modify assumptions and see results update.
 */

import ExcelJS from "exceljs";
import { saveAs } from "file-saver";
import type { CombinedResult } from "../models/utility-engine";
import type { InterventionParams } from "../models/utility-engine";

const HEADER_FILL: ExcelJS.FillPattern = { type: "pattern", pattern: "solid", fgColor: { argb: "FF2563EB" } };
const HEADER_FONT: Partial<ExcelJS.Font> = { bold: true, color: { argb: "FFFFFFFF" }, size: 11 };
const SECTION_FILL: ExcelJS.FillPattern = { type: "pattern", pattern: "solid", fgColor: { argb: "FFF1F5F9" } };
const CURRENCY_FMT = '$#,##0';
const PCT_FMT = '0.0%';

export async function exportToExcel(
  interventions: InterventionParams[],
  result: CombinedResult,
  overlapFactor: number
) {
  const wb = new ExcelJS.Workbook();
  wb.creator = "UA+ Utility Analysis Platform";
  wb.created = new Date();

  // ═══ Sheet 1: Assumptions ═══
  const assumptions = wb.addWorksheet("Assumptions", { properties: { tabColor: { argb: "FF2563EB" } } });
  assumptions.columns = [
    { header: "Parameter", key: "param", width: 30 },
    { header: "Value", key: "value", width: 18 },
    { header: "Unit", key: "unit", width: 14 },
    { header: "Source / Note", key: "note", width: 40 },
  ];
  styleHeaderRow(assumptions);

  let aRow = 2;
  const cellMap: Record<string, string> = {};

  for (let idx = 0; idx < interventions.length; idx++) {
    const p = interventions[idx];
    const prefix = `i${idx}`;

    assumptions.getRow(aRow).values = [`── ${p.label} ──`, "", "", ""];
    assumptions.getRow(aRow).font = { bold: true, size: 11 };
    assumptions.getRow(aRow).fill = SECTION_FILL;
    aRow++;

    const params: [string, string, number | string, string, string][] = [
      [`${prefix}_N`, "Number of Employees", p.numEmployees, "", ""],
      [`${prefix}_salary`, "Average Salary ($)", p.avgSalary, "$", ""],
      [`${prefix}_sdy_pct`, "SDy (% of salary)", p.sdyPercent / 100, "%", "Schmidt & Hunter (1983)"],
      [`${prefix}_t`, "Duration (years)", p.timePeriodYears, "years", ""],
      [`${prefix}_cost_per`, "Cost per Employee ($)", p.costPerEmployee, "$", ""],
      [`${prefix}_fixed_cost`, "Fixed / Setup Cost ($)", p.fixedCost, "$", ""],
    ];

    if (p.type === "selection") {
      params.push(
        [`${prefix}_r`, "Validity Coefficient (r)", p.validityCoefficient ?? 0, "", "Sackett et al. (2022)"],
        [`${prefix}_r_old`, "Current System Validity", p.currentValidity ?? 0, "", ""],
        [`${prefix}_sr`, "Selection Ratio", p.selectionRatio ?? 0.3, "", ""],
      );
    } else {
      params.push(
        [`${prefix}_d`, "Effect Size (d)", p.effectSize ?? 0, "", "Meta-analytic default"],
      );
    }

    if (p.applyEconomicAdjustments) {
      params.push(
        [`${prefix}_vc`, "Variable Cost (%)", (p.variableCostPercent ?? 10) / 100, "%", "Sturman (2000)"],
        [`${prefix}_tax`, "Tax Rate (%)", (p.taxRate ?? 30) / 100, "%", "Sturman (2000)"],
        [`${prefix}_disc`, "Discount Rate (%)", (p.discountRate ?? 8) / 100, "%", "Sturman (2000)"],
      );
    }

    for (const [key, label, value, unit, note] of params) {
      const row = assumptions.getRow(aRow);
      row.values = [label, value, unit, note];
      const cell = `B${aRow}`;
      cellMap[key] = cell;
      if (unit === "%" || unit === "%") {
        assumptions.getCell(cell).numFmt = PCT_FMT;
      } else if (unit === "$") {
        assumptions.getCell(cell).numFmt = CURRENCY_FMT;
      }
      aRow++;
    }
    aRow++;
  }

  // Global parameters
  assumptions.getRow(aRow).values = ["── Global Parameters ──", "", "", ""];
  assumptions.getRow(aRow).font = { bold: true, size: 11 };
  assumptions.getRow(aRow).fill = SECTION_FILL;
  aRow++;
  assumptions.getRow(aRow).values = ["Overlap Factor", overlapFactor, "%", "Cross-intervention shared variance"];
  cellMap["overlap"] = `B${aRow}`;
  assumptions.getCell(`B${aRow}`).numFmt = PCT_FMT;

  // ═══ Sheet 2: Calculations ═══
  const calcs = wb.addWorksheet("Calculations", { properties: { tabColor: { argb: "FF059669" } } });
  calcs.columns = [
    { header: "Intervention", key: "intervention", width: 28 },
    { header: "Step", key: "step", width: 35 },
    { header: "Formula (cell references)", key: "formula", width: 50 },
    { header: "Result", key: "result", width: 18 },
  ];
  styleHeaderRow(calcs);

  let cRow = 2;
  for (let idx = 0; idx < interventions.length; idx++) {
    const p = interventions[idx];
    const r = result.individualResults[idx];
    const prefix = `i${idx}`;
    const sdy = `Assumptions!${cellMap[`${prefix}_salary`]}*Assumptions!${cellMap[`${prefix}_sdy_pct`]}`;

    calcs.getRow(cRow).values = [p.label, "──────────", "", ""];
    calcs.getRow(cRow).font = { bold: true };
    calcs.getRow(cRow).fill = SECTION_FILL;
    cRow++;

    // SDy calculation
    calcs.getRow(cRow).values = [
      p.label,
      "SDy = Salary × SDy%",
      `=Assumptions!${cellMap[`${prefix}_salary`]}*Assumptions!${cellMap[`${prefix}_sdy_pct`]}`,
      "",
    ];
    calcs.getCell(`C${cRow}`).numFmt = CURRENCY_FMT;
    const sdyCell = `C${cRow}`;
    calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
    calcs.getCell(`D${cRow}`).numFmt = CURRENCY_FMT;
    cRow++;

    if (p.type === "selection") {
      // Total applicants
      calcs.getRow(cRow).values = [
        "",
        "Applicants = N / SR",
        `=Assumptions!${cellMap[`${prefix}_N`]}/Assumptions!${cellMap[`${prefix}_sr`]}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      const appCell = `C${cRow}`;
      cRow++;

      // Incremental validity
      calcs.getRow(cRow).values = [
        "",
        "Δr = r_new − r_old",
        `=Assumptions!${cellMap[`${prefix}_r`]}-Assumptions!${cellMap[`${prefix}_r_old`]}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      const drCell = `C${cRow}`;
      cRow++;

      // Gross benefit
      calcs.getRow(cRow).values = [
        "",
        "Gross = N × t × Δr × SDy × λ(ϕ)",
        r.grossBenefit,
        "",
      ];
      calcs.getCell(`C${cRow}`).numFmt = CURRENCY_FMT;
      calcs.getCell(`D${cRow}`).value = r.grossBenefit;
      calcs.getCell(`D${cRow}`).numFmt = CURRENCY_FMT;
      cRow++;

      // Total cost
      calcs.getRow(cRow).values = [
        "",
        "Total Cost = Applicants × Cost/Person + Fixed",
        `=${appCell}*Assumptions!${cellMap[`${prefix}_cost_per`]}+Assumptions!${cellMap[`${prefix}_fixed_cost`]}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      calcs.getCell(`D${cRow}`).numFmt = CURRENCY_FMT;
      cRow++;
    } else {
      // Gross benefit for training/intervention
      calcs.getRow(cRow).values = [
        "",
        "Gross = N × t × d × SDy",
        `=Assumptions!${cellMap[`${prefix}_N`]}*Assumptions!${cellMap[`${prefix}_t`]}*Assumptions!${cellMap[`${prefix}_d`]}*${sdyCell}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      calcs.getCell(`D${cRow}`).numFmt = CURRENCY_FMT;
      const grossCell = `D${cRow}`;
      cRow++;

      // Total cost
      calcs.getRow(cRow).values = [
        "",
        "Total Cost = N × Cost/Person + Fixed",
        `=Assumptions!${cellMap[`${prefix}_N`]}*Assumptions!${cellMap[`${prefix}_cost_per`]}+Assumptions!${cellMap[`${prefix}_fixed_cost`]}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      calcs.getCell(`D${cRow}`).numFmt = CURRENCY_FMT;
      const costCell = `D${cRow}`;
      cRow++;

      // Net benefit
      calcs.getRow(cRow).values = [
        "",
        "Net Benefit = Gross − Cost",
        `=${grossCell}-${costCell}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      calcs.getCell(`D${cRow}`).numFmt = CURRENCY_FMT;
      cRow++;

      // ROI
      calcs.getRow(cRow).values = [
        "",
        "ROI = Gross / Cost",
        `=${grossCell}/${costCell}`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      calcs.getCell(`D${cRow}`).numFmt = '0.0';
      cRow++;

      // Break-even d
      calcs.getRow(cRow).values = [
        "",
        "Break-even d = Cost / (N × t × SDy)",
        `=${costCell}/(Assumptions!${cellMap[`${prefix}_N`]}*Assumptions!${cellMap[`${prefix}_t`]}*${sdyCell})`,
        "",
      ];
      calcs.getCell(`D${cRow}`).value = { formula: `C${cRow}` } as ExcelJS.CellFormulaValue;
      calcs.getCell(`D${cRow}`).numFmt = '0.000';
      cRow++;
    }
    cRow++;
  }

  // ═══ Sheet 3: Results Summary ═══
  const results = wb.addWorksheet("Results Summary", { properties: { tabColor: { argb: "FFD97706" } } });
  results.columns = [
    { header: "Metric", key: "metric", width: 30 },
    { header: "Value", key: "value", width: 20 },
  ];
  styleHeaderRow(results);

  const summaryRows: [string, number | string, string][] = [
    ["Combined Net Benefit", result.combinedNetBenefit, CURRENCY_FMT],
    ["Combined Gross Benefit", result.combinedGross, CURRENCY_FMT],
    ["Total Investment", result.combinedCost, CURRENCY_FMT],
    ["ROI Ratio", `${result.combinedROI.toFixed(1)}:1`, ""],
    ["Overlap Factor", overlapFactor, PCT_FMT],
    ["", "", ""],
  ];
  for (const r of result.individualResults) {
    summaryRows.push(
      [`── ${r.interventionLabel} ──`, "", ""],
      ["  Gross Benefit", r.grossBenefit, CURRENCY_FMT],
      ["  Total Cost", r.totalCost, CURRENCY_FMT],
      ["  Net Benefit", r.netBenefit, CURRENCY_FMT],
      ["  Net per Employee", r.netBenefitPerEmployee, CURRENCY_FMT],
      ["  ROI %", r.roiPercent / 100, PCT_FMT],
      ["  Break-even Effect Size", r.breakEvenEffectSize, "0.000"],
      ["  Effect Size Used", r.effectSizeUsed, "0.00"],
      ["", "", ""],
    );
  }

  let rRow = 2;
  for (const [label, value, fmt] of summaryRows) {
    const row = results.getRow(rRow);
    row.values = [label, value];
    if (fmt) results.getCell(`B${rRow}`).numFmt = fmt;
    if (typeof label === "string" && label.startsWith("──")) {
      row.font = { bold: true };
      row.fill = SECTION_FILL;
    }
    rRow++;
  }

  // Citations
  results.getRow(rRow + 1).values = ["Methodology & Citations"];
  results.getRow(rRow + 1).font = { bold: true };
  results.getRow(rRow + 2).values = ["BCG Model: Brogden (1949); Cronbach & Gleser (1965)"];
  results.getRow(rRow + 3).values = ["SDy estimation: Schmidt & Hunter (1983)"];
  results.getRow(rRow + 4).values = ["Economic adjustments: Sturman (2000)"];
  results.getRow(rRow + 5).values = ["Generated by UA+ Utility Analysis Platform"];

  // Download
  const buffer = await wb.xlsx.writeBuffer();
  const blob = new Blob([buffer], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
  saveAs(blob, `ua-plus-analysis-${new Date().toISOString().slice(0, 10)}.xlsx`);
}

function styleHeaderRow(ws: ExcelJS.Worksheet) {
  const row = ws.getRow(1);
  row.fill = HEADER_FILL;
  row.font = HEADER_FONT;
  row.height = 24;
}
