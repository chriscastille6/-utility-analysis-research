"use client";

import { useState } from "react";
import {
  BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer,
  LineChart, Line, Legend, AreaChart, Area, Cell,
} from "recharts";
import type { CombinedResult, UtilityResult, InterventionParams, MonteCarloResult } from "@/lib/models/utility-engine";
import { runMonteCarlo } from "@/lib/models/utility-engine";
import { formatCurrency } from "@/lib/stats";
import { ExportToolbar } from "./ExportToolbar";

interface Props {
  result: CombinedResult;
  overlapFactor: number;
  interventions: InterventionParams[];
}

export function ResultsDashboard({ result, overlapFactor, interventions }: Props) {
  const [activeView, setActiveView] = useState<"summary" | "individual" | "sensitivity" | "montecarlo">("summary");
  const [selectedIntervention, setSelectedIntervention] = useState(0);

  const currentResult: UtilityResult | undefined = result.individualResults[selectedIntervention];

  return (
    <div className="max-w-5xl mx-auto space-y-6">
      {/* Top-level KPIs */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <div className="stat-card success">
          <div className="text-2xl font-bold">{formatCurrency(result.combinedNetBenefit)}</div>
          <div className="text-sm opacity-80">Net Benefit</div>
        </div>
        <div className="stat-card">
          <div className="text-2xl font-bold">{formatCurrency(result.combinedGross)}</div>
          <div className="text-sm opacity-80">Gross Benefit</div>
        </div>
        <div className="stat-card warning">
          <div className="text-2xl font-bold">{formatCurrency(result.combinedCost)}</div>
          <div className="text-sm opacity-80">Total Investment</div>
        </div>
        <div className="stat-card">
          <div className="text-2xl font-bold">{result.combinedROI.toFixed(1)}:1</div>
          <div className="text-sm opacity-80">Return on Investment</div>
        </div>
      </div>

      {/* Overlap note */}
      {result.individualResults.length > 1 && result.overlapReduction > 0 && (
        <div className="card bg-amber-50 border-amber-200">
          <div className="flex items-start gap-2">
            <span className="text-amber-600 text-lg">⚠️</span>
            <div className="text-sm">
              <strong>Overlap adjustment applied.</strong> The combined benefit is{" "}
              {formatCurrency(result.overlapReduction)} less than the naive sum (
              {formatCurrency(result.independentTotal)}) because these interventions share{" "}
              {(overlapFactor * 100).toFixed(0)}% common variance in job performance.
            </div>
          </div>
        </div>
      )}

      {/* Sub-navigation */}
      <div className="flex gap-1 bg-[var(--surface)] rounded-lg p-1">
        {(["summary", "individual", "sensitivity", "montecarlo"] as const).map((view) => (
          <button
            key={view}
            onClick={() => setActiveView(view)}
            className={`flex-1 px-3 py-2 rounded-md text-sm font-medium transition-colors ${
              activeView === view
                ? "bg-white shadow-sm text-[var(--foreground)]"
                : "text-[var(--muted)] hover:text-[var(--foreground)]"
            }`}
          >
            {view === "summary" ? "Summary" : view === "individual" ? "By Intervention" : view === "sensitivity" ? "Sensitivity" : "Monte Carlo"}
          </button>
        ))}
      </div>

      {/* Views */}
      {activeView === "summary" && <SummaryView result={result} />}
      {activeView === "individual" && currentResult && (
        <IndividualView
          results={result.individualResults}
          selected={selectedIntervention}
          onSelect={setSelectedIntervention}
        />
      )}
      {activeView === "sensitivity" && currentResult && (
        <SensitivityView result={currentResult} results={result.individualResults} selected={selectedIntervention} onSelect={setSelectedIntervention} />
      )}
      {activeView === "montecarlo" && <MonteCarloView result={result} />}

      {/* Export Toolbar */}
      <ExportToolbar interventions={interventions} result={result} overlapFactor={overlapFactor} />

      {/* Formula & Citations */}
      <div className="card">
        <h3 className="font-semibold text-sm mb-3">Methodology & Citations</h3>
        {result.individualResults.map((r) => (
          <div key={r.interventionId} className="mb-3">
            <div className="text-sm font-medium">{r.interventionLabel}</div>
            <code className="text-xs text-[var(--muted)] block mt-1 bg-[var(--surface)] p-2 rounded">
              {r.formulaDescription}
            </code>
          </div>
        ))}
        <div className="text-xs text-[var(--muted)] mt-4 space-y-1">
          <p>• BCG Model: Brogden (1949); Cronbach & Gleser (1965)</p>
          <p>• SDy estimation: Schmidt & Hunter (1983); 40% rule</p>
          <p>• Economic adjustments: Sturman (2000); Boudreau (1983)</p>
          <p>• Training utility: Morrow et al. (1997); Arthur et al. (2003)</p>
          <p>• Job crafting: Oprea et al. (2019)</p>
        </div>
      </div>
    </div>
  );
}

// ─── Summary View ─────────────────────────────────────────────────────────

function SummaryView({ result }: { result: CombinedResult }) {
  const contribData = result.interventionContributions.map((c) => ({
    name: c.label.length > 20 ? c.label.slice(0, 18) + "…" : c.label,
    value: c.contribution,
    percent: c.percent,
  }));

  const colors = ["#2563eb", "#059669", "#d97706", "#dc2626", "#7c3aed", "#0891b2"];

  // Cumulative yearly data (use the first intervention's breakdown as proxy)
  const firstResult = result.individualResults[0];
  const yearlyData = firstResult?.yearlyBreakdown.map((y) => ({
    year: `Year ${y.year}`,
    benefit: y.benefit,
    cost: y.cost,
    cumulative: y.cumulative,
  }));

  return (
    <div className="space-y-6">
      {/* Contribution breakdown */}
      {result.individualResults.length > 1 && (
        <div className="card">
          <h3 className="font-semibold text-sm mb-4">Contribution by Intervention</h3>
          <div style={{ height: 250 }}>
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={contribData} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis type="number" tickFormatter={(v) => formatCurrency(v)} fontSize={11} />
                <YAxis type="category" dataKey="name" width={140} fontSize={11} />
                <Tooltip formatter={(v: number) => formatCurrency(v)} />
                <Bar dataKey="value" radius={[0, 6, 6, 0]}>
                  {contribData.map((_, i) => (
                    <Cell key={i} fill={colors[i % colors.length]} />
                  ))}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* Cumulative Value */}
      {yearlyData && (
        <div className="card">
          <h3 className="font-semibold text-sm mb-4">Cumulative Net Value Over Time</h3>
          <div style={{ height: 280 }}>
            <ResponsiveContainer width="100%" height="100%">
              <AreaChart data={yearlyData}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis dataKey="year" fontSize={11} />
                <YAxis tickFormatter={(v) => formatCurrency(v)} fontSize={11} />
                <Tooltip formatter={(v: number) => formatCurrency(v)} />
                <Area type="monotone" dataKey="cumulative" stroke="#2563eb" fill="#dbeafe" name="Cumulative Net" />
              </AreaChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* Break-even summary */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {result.individualResults.map((r) => (
          <div key={r.interventionId} className="card">
            <h4 className="font-medium text-sm">{r.interventionLabel}</h4>
            <div className="mt-2 space-y-2">
              <div className="flex justify-between text-sm">
                <span className="text-[var(--muted)]">Net per employee</span>
                <span className="font-semibold">{formatCurrency(r.netBenefitPerEmployee)}</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-[var(--muted)]">ROI</span>
                <span className="font-semibold">{r.roiPercent.toFixed(0)}%</span>
              </div>
              <div className="flex justify-between text-sm">
                <span className="text-[var(--muted)]">Break-even effect size</span>
                <span className="font-semibold">{r.breakEvenEffectSize.toFixed(3)}</span>
              </div>
              <div className="text-xs text-[var(--muted)] mt-1">
                Even if the true effect is only <strong>{(r.breakEvenEffectSize / r.effectSizeUsed * 100).toFixed(0)}%</strong> of the
                meta-analytic estimate, this intervention still breaks even.
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

// ─── Individual View ──────────────────────────────────────────────────────

function IndividualView({
  results,
  selected,
  onSelect,
}: {
  results: UtilityResult[];
  selected: number;
  onSelect: (i: number) => void;
}) {
  const r = results[selected];
  if (!r) return null;

  return (
    <div className="space-y-4">
      {results.length > 1 && (
        <div className="flex gap-2">
          {results.map((res, i) => (
            <button
              key={res.interventionId}
              onClick={() => onSelect(i)}
              className={`px-3 py-1.5 rounded-lg text-sm ${
                i === selected ? "bg-[var(--primary)] text-white" : "bg-[var(--surface)] text-[var(--foreground)]"
              }`}
            >
              {res.interventionLabel}
            </button>
          ))}
        </div>
      )}

      <div className="grid grid-cols-3 gap-4">
        <div className="stat-card success">
          <div className="text-xl font-bold">{formatCurrency(r.netBenefit)}</div>
          <div className="text-sm opacity-80">Net Benefit</div>
        </div>
        <div className="stat-card">
          <div className="text-xl font-bold">{r.roi.toFixed(1)}:1</div>
          <div className="text-sm opacity-80">ROI Ratio</div>
        </div>
        <div className="stat-card warning">
          <div className="text-xl font-bold">{formatCurrency(r.totalCost)}</div>
          <div className="text-sm opacity-80">Total Cost</div>
        </div>
      </div>

      <div className="card">
        <h3 className="font-semibold text-sm mb-4">Annual Benefit vs Cost</h3>
        <div style={{ height: 280 }}>
          <ResponsiveContainer width="100%" height="100%">
            <BarChart data={r.yearlyBreakdown}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis dataKey="year" tickFormatter={(v) => `Year ${v}`} fontSize={11} />
              <YAxis tickFormatter={(v) => formatCurrency(v)} fontSize={11} />
              <Tooltip formatter={(v: number) => formatCurrency(v)} />
              <Legend />
              <Bar dataKey="benefit" fill="#059669" name="Benefit" radius={[4, 4, 0, 0]} />
              <Bar dataKey="cost" fill="#dc2626" name="Cost" radius={[4, 4, 0, 0]} />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </div>
    </div>
  );
}

// ─── Sensitivity View ─────────────────────────────────────────────────────

function SensitivityView({ result, results, selected, onSelect }: { result: UtilityResult; results: UtilityResult[]; selected: number; onSelect: (i: number) => void }) {
  return (
    <div className="space-y-4">
      {results.length > 1 && (
        <div className="flex gap-2">
          {results.map((res, i) => (
            <button key={res.interventionId} onClick={() => onSelect(i)}
              className={`px-3 py-1.5 rounded-lg text-sm ${i === selected ? "bg-[var(--primary)] text-white" : "bg-[var(--surface)]"}`}>
              {res.interventionLabel}
            </button>
          ))}
        </div>
      )}

      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="card">
          <h3 className="font-semibold text-sm mb-4">Sensitivity to Effect Size</h3>
          <div style={{ height: 280 }}>
            <ResponsiveContainer width="100%" height="100%">
              <LineChart data={result.sensitivityToEffectSize}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis dataKey="effectSize" fontSize={11} label={{ value: "Effect Size", position: "insideBottom", offset: -5, fontSize: 11 }} />
                <YAxis tickFormatter={(v) => formatCurrency(v)} fontSize={11} />
                <Tooltip formatter={(v: number) => formatCurrency(v)} />
                <Line type="monotone" dataKey="net" stroke="#2563eb" strokeWidth={2} dot={false} name="Net Benefit" />
                <Line type="monotone" dataKey={() => 0} stroke="#dc2626" strokeDasharray="5 5" dot={false} name="Break-even" />
              </LineChart>
            </ResponsiveContainer>
          </div>
        </div>

        <div className="card">
          <h3 className="font-semibold text-sm mb-4">Sensitivity to SDy (% of Salary)</h3>
          <div style={{ height: 280 }}>
            <ResponsiveContainer width="100%" height="100%">
              <LineChart data={result.sensitivityToSDy}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis dataKey="sdyPercent" fontSize={11} label={{ value: "SDy (% of salary)", position: "insideBottom", offset: -5, fontSize: 11 }} />
                <YAxis tickFormatter={(v) => formatCurrency(v)} fontSize={11} />
                <Tooltip formatter={(v: number) => formatCurrency(v)} />
                <Line type="monotone" dataKey="net" stroke="#059669" strokeWidth={2} dot={false} name="Net Benefit" />
              </LineChart>
            </ResponsiveContainer>
          </div>
        </div>
      </div>
    </div>
  );
}

// ─── Monte Carlo View ─────────────────────────────────────────────────────

function MonteCarloView({ result }: { result: CombinedResult }) {
  const [nSims, setNSims] = useState(5000);
  const [mcResult, setMcResult] = useState<MonteCarloResult | null>(null);
  const [selectedIdx, setSelectedIdx] = useState(0);

  const intervention = result.individualResults[selectedIdx];
  if (!intervention) return null;

  const handleRun = () => {
    const params = {
      id: intervention.interventionId,
      type: intervention.interventionType,
      label: intervention.interventionLabel,
      numEmployees: 100,
      avgSalary: intervention.sdy / 0.4 || 50000,
      sdyPercent: 40,
      timePeriodYears: 2,
      costPerEmployee: intervention.totalCost / 100,
      fixedCost: 0,
      effectSize: intervention.effectSizeUsed,
      validityCoefficient: intervention.interventionType === "selection" ? intervention.effectSizeUsed : undefined,
      selectionRatio: 0.3,
    };
    const mc = runMonteCarlo(params, nSims);
    setMcResult(mc);
  };

  return (
    <div className="space-y-4">
      <div className="card">
        <h3 className="font-semibold text-sm mb-3">Monte Carlo Simulation</h3>
        <p className="text-xs text-[var(--muted)] mb-4">
          Randomly vary the effect size and SDy within plausible ranges to see the distribution
          of possible outcomes. Based on Sturman (2000) methodology.
        </p>
        <div className="flex items-end gap-4">
          {result.individualResults.length > 1 && (
            <div>
              <label className="text-xs text-[var(--muted)]">Intervention</label>
              <select
                className="input-field text-sm"
                value={selectedIdx}
                onChange={(e) => { setSelectedIdx(parseInt(e.target.value)); setMcResult(null); }}
              >
                {result.individualResults.map((r, i) => (
                  <option key={r.interventionId} value={i}>{r.interventionLabel}</option>
                ))}
              </select>
            </div>
          )}
          <div>
            <label className="text-xs text-[var(--muted)]">Simulations</label>
            <select className="input-field text-sm" value={nSims} onChange={(e) => setNSims(parseInt(e.target.value))}>
              <option value={1000}>1,000</option>
              <option value={5000}>5,000</option>
              <option value={10000}>10,000</option>
            </select>
          </div>
          <button onClick={handleRun} className="btn-primary text-sm">
            Run Simulation
          </button>
        </div>
      </div>

      {mcResult && (
        <>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            <div className="card text-center">
              <div className="text-xs text-[var(--muted)]">Mean Outcome</div>
              <div className="text-lg font-bold">{formatCurrency(mcResult.mean)}</div>
            </div>
            <div className="card text-center">
              <div className="text-xs text-[var(--muted)]">Median</div>
              <div className="text-lg font-bold">{formatCurrency(mcResult.median)}</div>
            </div>
            <div className="card text-center">
              <div className="text-xs text-[var(--muted)]">90% Range</div>
              <div className="text-lg font-bold text-xs">{formatCurrency(mcResult.p5)} – {formatCurrency(mcResult.p95)}</div>
            </div>
            <div className="card text-center">
              <div className="text-xs text-[var(--muted)]">P(Positive ROI)</div>
              <div className="text-lg font-bold text-green-600">{mcResult.positiveProb.toFixed(1)}%</div>
            </div>
          </div>

          <div className="card">
            <h3 className="font-semibold text-sm mb-4">Distribution of Outcomes ({nSims.toLocaleString()} simulations)</h3>
            <div style={{ height: 300 }}>
              <ResponsiveContainer width="100%" height="100%">
                <BarChart data={mcResult.histogram}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                  <XAxis dataKey="bin" tickFormatter={(v) => formatCurrency(v)} fontSize={10} />
                  <YAxis fontSize={11} />
                  <Tooltip
                    formatter={(v: number) => v}
                    labelFormatter={(v) => `~${formatCurrency(v as number)}`}
                  />
                  <Bar dataKey="count" name="Frequency" radius={[2, 2, 0, 0]}>
                    {mcResult.histogram.map((entry, i) => (
                      <Cell key={i} fill={entry.bin >= 0 ? "#059669" : "#dc2626"} />
                    ))}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </div>
          </div>
        </>
      )}
    </div>
  );
}
