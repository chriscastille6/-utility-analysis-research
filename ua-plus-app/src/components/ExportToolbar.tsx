"use client";

import { useState } from "react";
import type { CombinedResult, InterventionParams } from "@/lib/models/utility-engine";

interface Props {
  interventions: InterventionParams[];
  result: CombinedResult;
  overlapFactor: number;
}

export function ExportToolbar({ interventions, result, overlapFactor }: Props) {
  const [exporting, setExporting] = useState<string | null>(null);

  const handleExport = async (type: "excel" | "r" | "python" | "pdf") => {
    setExporting(type);
    try {
      if (type === "excel") {
        const { exportToExcel } = await import("@/lib/exports/export-excel");
        await exportToExcel(interventions, result, overlapFactor);
      } else if (type === "r") {
        const { exportRScript } = await import("@/lib/exports/export-script");
        exportRScript(interventions, result, overlapFactor);
      } else if (type === "python") {
        const { exportPythonScript } = await import("@/lib/exports/export-script");
        exportPythonScript(interventions, result, overlapFactor);
      } else if (type === "pdf") {
        const { exportToPDF } = await import("@/lib/exports/export-pdf");
        exportToPDF(interventions, result, overlapFactor);
      }
    } finally {
      setTimeout(() => setExporting(null), 800);
    }
  };

  return (
    <div className="card">
      <div className="flex items-center justify-between mb-3">
        <div>
          <h3 className="font-semibold text-sm">Export & Share</h3>
          <p className="text-xs text-[var(--muted)]">Download your analysis for verification, scripting, or stakeholder communication</p>
        </div>
      </div>
      <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
        <ExportButton
          icon={<SpreadsheetIcon />}
          label="Excel Workbook"
          description="Cell-referenced formulas for auditing assumptions"
          color="#059669"
          onClick={() => handleExport("excel")}
          loading={exporting === "excel"}
        />
        <ExportButton
          icon={<CodeIcon />}
          label="R Script"
          description="Reproducible R code with all calculations"
          color="#2563eb"
          onClick={() => handleExport("r")}
          loading={exporting === "r"}
        />
        <ExportButton
          icon={<PythonIcon />}
          label="Python Script"
          description="Reproducible Python code with scipy"
          color="#d97706"
          onClick={() => handleExport("python")}
          loading={exporting === "python"}
        />
        <ExportButton
          icon={<DocIcon />}
          label="PDF Summary"
          description="Stakeholder-ready executive summary"
          color="#dc2626"
          onClick={() => handleExport("pdf")}
          loading={exporting === "pdf"}
        />
      </div>
    </div>
  );
}

function ExportButton({
  icon,
  label,
  description,
  color,
  onClick,
  loading,
}: {
  icon: React.ReactNode;
  label: string;
  description: string;
  color: string;
  onClick: () => void;
  loading: boolean;
}) {
  return (
    <button
      onClick={onClick}
      disabled={loading}
      className="group relative flex flex-col items-center text-center gap-2 rounded-xl border border-[var(--border)] bg-white p-4 transition-all hover:shadow-md hover:border-current disabled:opacity-60"
      style={{ "--hover-color": color } as React.CSSProperties}
    >
      <div
        className="w-10 h-10 rounded-lg flex items-center justify-center transition-colors"
        style={{ backgroundColor: `${color}14`, color }}
      >
        {loading ? <Spinner /> : icon}
      </div>
      <div>
        <div className="text-sm font-medium">{label}</div>
        <div className="text-[10px] text-[var(--muted)] mt-0.5 leading-tight">{description}</div>
      </div>
    </button>
  );
}

function Spinner() {
  return (
    <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24" fill="none">
      <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
      <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z" />
    </svg>
  );
}

function SpreadsheetIcon() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z" />
      <polyline points="14 2 14 8 20 8" />
      <line x1="8" y1="13" x2="16" y2="13" />
      <line x1="8" y1="17" x2="16" y2="17" />
      <line x1="12" y1="9" x2="12" y2="21" />
    </svg>
  );
}

function CodeIcon() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <polyline points="16 18 22 12 16 6" />
      <polyline points="8 6 2 12 8 18" />
    </svg>
  );
}

function PythonIcon() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <polyline points="16 18 22 12 16 6" />
      <polyline points="8 6 2 12 8 18" />
      <line x1="12" y1="2" x2="12" y2="22" />
    </svg>
  );
}

function DocIcon() {
  return (
    <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
      <path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z" />
      <polyline points="14 2 14 8 20 8" />
      <line x1="16" y1="13" x2="8" y2="13" />
      <line x1="16" y1="17" x2="8" y2="17" />
      <line x1="10" y1="9" x2="8" y2="9" />
    </svg>
  );
}
