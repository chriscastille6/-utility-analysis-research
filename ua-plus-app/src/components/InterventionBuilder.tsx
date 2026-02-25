"use client";

import { useState } from "react";
import {
  INTERVENTION_TEMPLATES,
  getTemplatesByCategory,
  type InterventionTemplate,
} from "@/lib/defaults/meta-analytic-defaults";
import type { InterventionParams } from "@/lib/models/utility-engine";

interface Props {
  interventions: InterventionParams[];
  overlapFactor: number;
  onAdd: (intervention: InterventionParams) => void;
  onRemove: (id: string) => void;
  onUpdate: (id: string, updates: Partial<InterventionParams>) => void;
  onOverlapChange: (factor: number) => void;
  onCalculate: () => void;
}

export function InterventionBuilder({
  interventions,
  overlapFactor,
  onAdd,
  onRemove,
  onUpdate,
  onOverlapChange,
  onCalculate,
}: Props) {
  const [showPicker, setShowPicker] = useState(interventions.length === 0);
  const [editingId, setEditingId] = useState<string | null>(null);
  const categories = getTemplatesByCategory();

  const handleSelectTemplate = (template: InterventionTemplate) => {
    const id = `int-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;
    const params: InterventionParams = {
      id,
      type: template.type,
      label: template.label,
      numEmployees: 100,
      avgSalary: 50000,
      sdyPercent: template.defaults.sdyPercent ?? 40,
      timePeriodYears: template.defaults.timePeriodYears ?? 2,
      costPerEmployee: template.defaults.costPerEmployee ?? 500,
      fixedCost: template.defaults.fixedCost ?? 5000,
      effectSize: template.defaults.effectSize,
      validityCoefficient: template.defaults.validityCoefficient,
      selectionRatio: template.defaults.selectionRatio,
      currentValidity: template.defaults.currentValidity,
      applyEconomicAdjustments: false,
      variableCostPercent: 10,
      taxRate: 30,
      discountRate: 8,
    };
    onAdd(params);
    setShowPicker(false);
    setEditingId(id);
  };

  return (
    <div className="max-w-4xl mx-auto space-y-6">
      {/* Current Interventions */}
      {interventions.length > 0 && (
        <div className="space-y-4">
          <div className="flex items-center justify-between">
            <h2 className="text-lg font-semibold">Your Interventions</h2>
            <div className="flex items-center gap-3">
              <button onClick={() => setShowPicker(true)} className="btn-outline text-sm">
                + Add Another
              </button>
              <button onClick={onCalculate} className="btn-primary text-sm">
                Calculate ROI →
              </button>
            </div>
          </div>

          {interventions.map((intervention) => (
            <InterventionCard
              key={intervention.id}
              intervention={intervention}
              isEditing={editingId === intervention.id}
              onToggleEdit={() => setEditingId(editingId === intervention.id ? null : intervention.id)}
              onUpdate={(updates) => onUpdate(intervention.id, updates)}
              onRemove={() => onRemove(intervention.id)}
              template={INTERVENTION_TEMPLATES.find((t) => t.type === intervention.type)}
            />
          ))}

          {/* Overlap control (only when multiple interventions) */}
          {interventions.length > 1 && (
            <div className="card">
              <h3 className="text-sm font-semibold mb-2">Intervention Overlap</h3>
              <p className="text-xs text-[var(--muted)] mb-3">
                When interventions target similar aspects of performance, their combined effect is less than the sum
                of parts. Adjust the overlap factor to reflect how much your interventions share common ground.
              </p>
              <div className="flex items-center gap-4">
                <span className="text-xs text-[var(--muted)]">Independent</span>
                <input
                  type="range"
                  min="0"
                  max="0.8"
                  step="0.05"
                  value={overlapFactor}
                  onChange={(e) => onOverlapChange(parseFloat(e.target.value))}
                  className="flex-1"
                />
                <span className="text-xs text-[var(--muted)]">High Overlap</span>
                <span className="text-sm font-medium w-12 text-right">{(overlapFactor * 100).toFixed(0)}%</span>
              </div>
            </div>
          )}
        </div>
      )}

      {/* Intervention Picker */}
      {showPicker && (
        <div className="space-y-6">
          {interventions.length === 0 && (
            <div className="text-center py-6">
              <h2 className="text-2xl font-bold mb-2">What HR decision are you evaluating?</h2>
              <p className="text-[var(--muted)] max-w-lg mx-auto">
                Select one or more interventions below. Each comes pre-loaded with meta-analytic defaults
                from the research literature — you can adjust any parameter to match your organization.
              </p>
            </div>
          )}

          {Object.entries(categories).map(([category, templates]) => (
            <div key={category}>
              <h3 className="text-sm font-semibold text-[var(--muted)] uppercase tracking-wide mb-3">
                {category}
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                {templates.map((template) => (
                  <button
                    key={template.label}
                    onClick={() => handleSelectTemplate(template)}
                    className="card card-hover text-left flex gap-3"
                  >
                    <div className="flex-1">
                      <div className="font-medium text-sm">{template.label}</div>
                      <div className="text-xs text-[var(--muted)] mt-1">{template.description}</div>
                      <div className="flex gap-2 mt-2">
                        <span className="badge badge-blue">
                          {template.evidence.effectSizeLabel}: {template.evidence.effectSize}
                        </span>
                        <span className="badge badge-green text-[10px]">
                          {template.evidence.source.split(";")[0]}
                        </span>
                      </div>
                    </div>
                  </button>
                ))}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

// ─── Individual Intervention Card with Inline Editing ─────────────────────

function InterventionCard({
  intervention,
  isEditing,
  onToggleEdit,
  onUpdate,
  onRemove,
  template,
}: {
  intervention: InterventionParams;
  isEditing: boolean;
  onToggleEdit: () => void;
  onUpdate: (updates: Partial<InterventionParams>) => void;
  onRemove: () => void;
  template?: InterventionTemplate;
}) {
  const isSelection = intervention.type === "selection";

  return (
    <div className="card">
      <div className="flex items-start justify-between">
        <div className="flex items-start gap-3">
          <div className="w-10 h-10 rounded-lg bg-[var(--surface)] flex items-center justify-center text-lg">
            {isSelection ? "🎯" : intervention.type === "training" ? "📚" : intervention.type === "job_crafting" ? "🔧" : intervention.type === "goal_setting" ? "🎯" : intervention.type === "leadership_development" ? "🏆" : "⚙️"}
          </div>
          <div>
            <h3 className="font-semibold text-sm">{intervention.label}</h3>
            {template && (
              <p className="text-xs text-[var(--muted)] mt-0.5">
                {template.evidence.source} • {template.evidence.effectSizeLabel} = {template.evidence.effectSize}
              </p>
            )}
          </div>
        </div>
        <div className="flex items-center gap-2">
          <button onClick={onToggleEdit} className="text-xs text-[var(--primary)] hover:underline">
            {isEditing ? "Done" : "Edit"}
          </button>
          <button onClick={onRemove} className="text-xs text-[var(--danger)] hover:underline">
            Remove
          </button>
        </div>
      </div>

      {/* Summary row */}
      <div className="flex flex-wrap gap-3 mt-3">
        <MiniStat label="Employees" value={intervention.numEmployees.toString()} />
        <MiniStat label="Avg Salary" value={`$${intervention.avgSalary.toLocaleString()}`} />
        <MiniStat
          label={isSelection ? "Validity (r)" : "Effect Size (d)"}
          value={(isSelection ? intervention.validityCoefficient ?? 0 : intervention.effectSize ?? 0).toFixed(2)}
        />
        <MiniStat label="Cost/Person" value={`$${intervention.costPerEmployee.toLocaleString()}`} />
        <MiniStat label="Duration" value={`${intervention.timePeriodYears}yr`} />
      </div>

      {/* Editing panel */}
      {isEditing && (
        <div className="mt-4 pt-4 border-t border-[var(--border)] grid grid-cols-2 md:grid-cols-3 gap-4">
          <Field label="Number of Employees" value={intervention.numEmployees}
            onChange={(v) => onUpdate({ numEmployees: v })} type="number" />
          <Field label="Average Salary ($)" value={intervention.avgSalary}
            onChange={(v) => onUpdate({ avgSalary: v })} type="number" />
          <Field label="SDy (% of salary)" value={intervention.sdyPercent}
            onChange={(v) => onUpdate({ sdyPercent: v })} type="number" min={10} max={80} />
          <Field label="Cost per Employee ($)" value={intervention.costPerEmployee}
            onChange={(v) => onUpdate({ costPerEmployee: v })} type="number" />
          <Field label="Fixed/Setup Cost ($)" value={intervention.fixedCost}
            onChange={(v) => onUpdate({ fixedCost: v })} type="number" />
          <Field label="Duration (years)" value={intervention.timePeriodYears}
            onChange={(v) => onUpdate({ timePeriodYears: v })} type="number" min={1} max={20} />

          {isSelection ? (
            <>
              <Field label="Validity Coefficient (r)" value={intervention.validityCoefficient ?? 0.3}
                onChange={(v) => onUpdate({ validityCoefficient: v })} type="number" min={0.01} max={0.99} step={0.01} />
              <Field label="Selection Ratio" value={intervention.selectionRatio ?? 0.3}
                onChange={(v) => onUpdate({ selectionRatio: v })} type="number" min={0.01} max={0.99} step={0.01} />
              <Field label="Current System Validity" value={intervention.currentValidity ?? 0}
                onChange={(v) => onUpdate({ currentValidity: v })} type="number" min={0} max={0.99} step={0.01} />
            </>
          ) : (
            <Field label="Effect Size (d)" value={intervention.effectSize ?? 0.3}
              onChange={(v) => onUpdate({ effectSize: v })} type="number" min={0.01} max={2.0} step={0.01} />
          )}

          {/* Economic adjustments toggle */}
          <div className="col-span-full">
            <label className="flex items-center gap-2 text-sm cursor-pointer">
              <input
                type="checkbox"
                checked={intervention.applyEconomicAdjustments ?? false}
                onChange={(e) => onUpdate({ applyEconomicAdjustments: e.target.checked })}
                className="rounded"
              />
              Apply economic adjustments (Sturman, 2000)
            </label>
          </div>
          {intervention.applyEconomicAdjustments && (
            <>
              <Field label="Variable Cost (%)" value={intervention.variableCostPercent ?? 10}
                onChange={(v) => onUpdate({ variableCostPercent: v })} type="number" min={0} max={50} />
              <Field label="Tax Rate (%)" value={intervention.taxRate ?? 30}
                onChange={(v) => onUpdate({ taxRate: v })} type="number" min={0} max={50} />
              <Field label="Discount Rate (%)" value={intervention.discountRate ?? 8}
                onChange={(v) => onUpdate({ discountRate: v })} type="number" min={0} max={20} />
            </>
          )}
        </div>
      )}
    </div>
  );
}

function MiniStat({ label, value }: { label: string; value: string }) {
  return (
    <div className="bg-[var(--surface)] rounded-lg px-3 py-1.5">
      <div className="text-[10px] text-[var(--muted)] uppercase tracking-wide">{label}</div>
      <div className="text-sm font-semibold">{value}</div>
    </div>
  );
}

function Field({
  label,
  value,
  onChange,
  type = "number",
  min,
  max,
  step,
}: {
  label: string;
  value: number;
  onChange: (v: number) => void;
  type?: string;
  min?: number;
  max?: number;
  step?: number;
}) {
  return (
    <div>
      <label className="block text-xs font-medium text-[var(--muted)] mb-1">{label}</label>
      <input
        type={type}
        className="input-field text-sm"
        value={value}
        min={min}
        max={max}
        step={step}
        onChange={(e) => onChange(parseFloat(e.target.value) || 0)}
      />
    </div>
  );
}
