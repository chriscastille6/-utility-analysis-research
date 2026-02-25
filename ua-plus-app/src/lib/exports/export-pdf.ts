/**
 * PDF Summary Export
 *
 * Generates a clean, professional PDF summary suitable for
 * presenting to leadership / stakeholders.
 */

import { jsPDF } from "jspdf";
import autoTable from "jspdf-autotable";
import type { CombinedResult, InterventionParams } from "../models/utility-engine";
import { formatCurrency } from "../stats";

export function exportToPDF(
  interventions: InterventionParams[],
  result: CombinedResult,
  overlapFactor: number
) {
  const doc = new jsPDF();
  const pageW = doc.internal.pageSize.getWidth();
  const margin = 20;
  let y = margin;

  // ─── Title ─────────────────────────────────────────────────
  doc.setFillColor(37, 99, 235);
  doc.rect(0, 0, pageW, 38, "F");
  doc.setTextColor(255, 255, 255);
  doc.setFontSize(20);
  doc.setFont("helvetica", "bold");
  doc.text("Utility Analysis Summary", margin, 18);
  doc.setFontSize(10);
  doc.setFont("helvetica", "normal");
  doc.text(`Generated ${new Date().toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric" })}  |  UA+ Platform`, margin, 30);
  y = 50;

  doc.setTextColor(15, 23, 42);

  // ─── Executive Summary ────────────────────────────────────
  doc.setFontSize(14);
  doc.setFont("helvetica", "bold");
  doc.text("Executive Summary", margin, y);
  y += 10;

  const summaryData = [
    ["Combined Net Benefit", formatCurrency(result.combinedNetBenefit)],
    ["Gross Benefit", formatCurrency(result.combinedGross)],
    ["Total Investment", formatCurrency(result.combinedCost)],
    ["Return on Investment", `${result.combinedROI.toFixed(1)}:1`],
    ["Interventions Analyzed", `${result.individualResults.length}`],
  ];

  autoTable(doc, {
    startY: y,
    head: [["Metric", "Value"]],
    body: summaryData,
    theme: "grid",
    headStyles: { fillColor: [37, 99, 235], fontSize: 10 },
    bodyStyles: { fontSize: 10 },
    columnStyles: { 1: { halign: "right", fontStyle: "bold" } },
    margin: { left: margin, right: margin },
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  y = ((doc as any).lastAutoTable?.finalY ?? y + 40) + 15;

  // Overlap note
  if (result.individualResults.length > 1 && result.overlapReduction > 0) {
    doc.setFontSize(9);
    doc.setFont("helvetica", "italic");
    doc.setTextColor(100, 100, 100);
    doc.text(
      `Note: Combined benefit reflects ${(overlapFactor * 100).toFixed(0)}% overlap adjustment (${formatCurrency(result.overlapReduction)} reduction from naive sum of ${formatCurrency(result.independentTotal)}).`,
      margin,
      y,
      { maxWidth: pageW - 2 * margin }
    );
    y += 12;
  }

  doc.setTextColor(15, 23, 42);

  // ─── Individual Interventions ─────────────────────────────
  doc.setFontSize(14);
  doc.setFont("helvetica", "bold");
  doc.text("Intervention Details", margin, y);
  y += 10;

  for (let i = 0; i < result.individualResults.length; i++) {
    const r = result.individualResults[i];
    const p = interventions[i];
    if (!p) continue;

    if (y > 240) {
      doc.addPage();
      y = margin;
    }

    const rows = [
      ["Employees", p.numEmployees.toString()],
      ["Average Salary", `$${p.avgSalary.toLocaleString()}`],
      ["SDy (% of salary)", `${p.sdyPercent}%`],
      ["Duration", `${p.timePeriodYears} year(s)`],
      [p.type === "selection" ? "Validity (r)" : "Effect Size (d)",
        (p.type === "selection" ? p.validityCoefficient ?? 0 : p.effectSize ?? 0).toFixed(2)],
      ["Cost per Employee", `$${p.costPerEmployee.toLocaleString()}`],
      ["Fixed Cost", `$${p.fixedCost.toLocaleString()}`],
      ["", ""],
      ["Gross Benefit", formatCurrency(r.grossBenefit)],
      ["Total Cost", formatCurrency(r.totalCost)],
      ["Net Benefit", formatCurrency(r.netBenefit)],
      ["Net per Employee", formatCurrency(r.netBenefitPerEmployee)],
      ["ROI", `${r.roiPercent.toFixed(0)}%`],
      ["Break-even Effect Size", r.breakEvenEffectSize.toFixed(4)],
    ];

    autoTable(doc, {
      startY: y,
      head: [[r.interventionLabel, ""]],
      body: rows,
      theme: "striped",
      headStyles: { fillColor: [5, 150, 105], fontSize: 10 },
      bodyStyles: { fontSize: 9 },
      columnStyles: { 1: { halign: "right" } },
      margin: { left: margin, right: margin },
    });

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    y = ((doc as any).lastAutoTable?.finalY ?? y + 70) + 10;
  }

  // ─── Methodology ──────────────────────────────────────────
  if (y > 230) {
    doc.addPage();
    y = margin;
  }

  doc.setFontSize(14);
  doc.setFont("helvetica", "bold");
  doc.text("Methodology & Formulas", margin, y);
  y += 10;

  doc.setFontSize(9);
  doc.setFont("helvetica", "normal");
  for (const r of result.individualResults) {
    doc.setFont("helvetica", "bold");
    doc.text(r.interventionLabel, margin, y);
    y += 5;
    doc.setFont("courier", "normal");
    doc.text(r.formulaDescription, margin, y, { maxWidth: pageW - 2 * margin });
    y += 10;
  }

  y += 5;
  doc.setFont("helvetica", "normal");
  doc.setFontSize(8);
  doc.setTextColor(100, 100, 100);
  const citations = [
    "BCG Model: Brogden (1949); Cronbach & Gleser (1965)",
    "SDy estimation: Schmidt & Hunter (1983) — 40% rule",
    "Economic adjustments: Sturman (2000); Boudreau (1983)",
    "Training utility: Morrow et al. (1997); Arthur et al. (2003)",
    "Job crafting: Oprea et al. (2019)",
  ];
  for (const c of citations) {
    doc.text(c, margin, y);
    y += 4;
  }

  // Footer
  const pages = doc.getNumberOfPages();
  for (let i = 1; i <= pages; i++) {
    doc.setPage(i);
    doc.setFontSize(8);
    doc.setTextColor(150, 150, 150);
    doc.text(
      `UA+ Utility Analysis Platform  |  Page ${i} of ${pages}`,
      pageW / 2,
      doc.internal.pageSize.getHeight() - 10,
      { align: "center" }
    );
  }

  doc.save(`ua-plus-summary-${new Date().toISOString().slice(0, 10)}.pdf`);
}
