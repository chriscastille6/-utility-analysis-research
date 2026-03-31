"use client";

import { useState } from "react";
import {
  getScenariosByCategory,
  type GuidedScenario,
} from "@/lib/defaults/guided-scenarios";
import type { InterventionParams } from "@/lib/models/utility-engine";

interface Props {
  onLoadScenario: (interventions: InterventionParams[], overlapFactor: number) => void;
}

export function GuidedScenarios({ onLoadScenario }: Props) {
  const [selectedScenario, setSelectedScenario] = useState<GuidedScenario | null>(null);
  const [currentStep, setCurrentStep] = useState(0);
  const [tutorialComplete, setTutorialComplete] = useState(false);
  const categories = getScenariosByCategory();

  const handleStartScenario = (scenario: GuidedScenario) => {
    setSelectedScenario(scenario);
    setCurrentStep(0);
    setTutorialComplete(false);
  };

  const handleBack = () => {
    setSelectedScenario(null);
    setCurrentStep(0);
    setTutorialComplete(false);
  };

  const handleNextStep = () => {
    if (!selectedScenario) return;
    if (currentStep < selectedScenario.steps.length - 1) {
      setCurrentStep((s) => s + 1);
    } else {
      setTutorialComplete(true);
    }
  };

  const handlePrevStep = () => {
    if (currentStep > 0) setCurrentStep((s) => s - 1);
  };

  const handleLoadAndCalculate = () => {
    if (!selectedScenario) return;
    onLoadScenario(
      selectedScenario.interventions,
      selectedScenario.overlapFactor ?? 0.15
    );
  };

  // ─── Tutorial View ──────────────────────────────────────────────────────
  if (selectedScenario) {
    const step = selectedScenario.steps[currentStep];
    const totalSteps = selectedScenario.steps.length;

    return (
      <div className="max-w-3xl mx-auto space-y-6">
        {/* Header */}
        <div className="flex items-center gap-3">
          <button onClick={handleBack} className="text-sm text-[var(--muted)] hover:text-[var(--foreground)]">
            ← Back to scenarios
          </button>
        </div>

        <div className="card" style={{ borderLeft: `4px solid ${selectedScenario.color}` }}>
          <div className="flex items-start gap-3 mb-4">
            <span className="text-3xl">{selectedScenario.icon}</span>
            <div>
              <h2 className="text-xl font-bold">{selectedScenario.title}</h2>
              <p className="text-sm text-[var(--muted)]">{selectedScenario.subtitle}</p>
              <div className="flex gap-2 mt-2">
                <span className={`badge ${selectedScenario.difficulty === "beginner" ? "badge-green" : selectedScenario.difficulty === "intermediate" ? "badge-amber" : "badge-blue"}`}>
                  {selectedScenario.difficulty}
                </span>
                <span className="badge badge-blue">~{selectedScenario.estimatedMinutes} min</span>
              </div>
            </div>
          </div>

          {/* Narrative */}
          <div className="bg-[var(--surface)] rounded-lg p-4 mb-4">
            <h3 className="text-sm font-semibold mb-2">The Scenario</h3>
            <p className="text-sm leading-relaxed">{selectedScenario.narrative}</p>
          </div>

          {/* Learning Objectives */}
          <div className="mb-4">
            <h3 className="text-sm font-semibold mb-2">What You&apos;ll Learn</h3>
            <ul className="space-y-1">
              {selectedScenario.learningObjectives.map((obj, i) => (
                <li key={i} className="text-sm text-[var(--muted)] flex items-start gap-2">
                  <span className="text-[var(--primary)] mt-0.5">✓</span> {obj}
                </li>
              ))}
            </ul>
          </div>
        </div>

        {/* Pre-configured Interventions */}
        <div className="card">
          <h3 className="text-sm font-semibold mb-3">Pre-Configured Parameters</h3>
          <div className="space-y-3">
            {selectedScenario.interventions.map((intervention) => (
              <div key={intervention.id} className="bg-[var(--surface)] rounded-lg p-3">
                <div className="font-medium text-sm mb-2">{intervention.label}</div>
                <div className="grid grid-cols-2 md:grid-cols-4 gap-2 text-xs">
                  <ParamPill label="Employees" value={intervention.numEmployees.toString()} />
                  <ParamPill label="Avg Salary" value={`$${intervention.avgSalary.toLocaleString()}`} />
                  <ParamPill label="SDy %" value={`${intervention.sdyPercent}%`} />
                  <ParamPill label="Duration" value={`${intervention.timePeriodYears}yr`} />
                  {intervention.effectSize !== undefined && (
                    <ParamPill label="Effect Size (d)" value={intervention.effectSize.toFixed(2)} />
                  )}
                  {intervention.validityCoefficient !== undefined && (
                    <ParamPill label="Validity (r)" value={intervention.validityCoefficient.toFixed(2)} />
                  )}
                  {intervention.currentValidity !== undefined && intervention.currentValidity > 0 && (
                    <ParamPill label="Current r" value={intervention.currentValidity.toFixed(2)} />
                  )}
                  {intervention.selectionRatio !== undefined && (
                    <ParamPill label="Selection Ratio" value={intervention.selectionRatio.toFixed(2)} />
                  )}
                  <ParamPill label="Cost/Person" value={`$${intervention.costPerEmployee.toLocaleString()}`} />
                  <ParamPill label="Fixed Cost" value={`$${intervention.fixedCost.toLocaleString()}`} />
                  {intervention.applyEconomicAdjustments && (
                    <>
                      <ParamPill label="Variable Cost" value={`${intervention.variableCostPercent}%`} />
                      <ParamPill label="Tax Rate" value={`${intervention.taxRate}%`} />
                      <ParamPill label="Discount Rate" value={`${intervention.discountRate}%`} />
                    </>
                  )}
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Step-by-Step Tutorial */}
        <div className="card">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-sm font-semibold">
              Tutorial: Step {currentStep + 1} of {totalSteps}
            </h3>
            <div className="flex gap-1">
              {selectedScenario.steps.map((_, i) => (
                <div
                  key={i}
                  className={`w-2 h-2 rounded-full transition-colors ${
                    i <= currentStep ? "bg-[var(--primary)]" : "bg-[var(--border)]"
                  }`}
                />
              ))}
            </div>
          </div>

          {/* Progress bar */}
          <div className="w-full h-1 bg-[var(--surface)] rounded-full mb-4">
            <div
              className="h-1 bg-[var(--primary)] rounded-full transition-all duration-300"
              style={{ width: `${((currentStep + 1) / totalSteps) * 100}%` }}
            />
          </div>

          <div className="mb-4">
            <h4 className="font-semibold mb-2">{step.title}</h4>
            <p className="text-sm leading-relaxed whitespace-pre-line">{step.content}</p>
            {step.highlight && (
              <div className="mt-3 bg-blue-50 border border-blue-200 rounded-lg px-3 py-2 text-xs text-blue-800">
                💡 This step focuses on the <strong>{step.highlight}</strong> parameter.
                You can adjust it in the Build Analysis view after loading.
              </div>
            )}
          </div>

          <div className="flex justify-between">
            <button
              onClick={handlePrevStep}
              disabled={currentStep === 0}
              className="btn-outline text-sm disabled:opacity-40"
            >
              ← Previous
            </button>
            <button onClick={handleNextStep} className="btn-primary text-sm">
              {currentStep < totalSteps - 1 ? "Next Step →" : "Complete Tutorial ✓"}
            </button>
          </div>
        </div>

        {/* Post-tutorial: Expected Results & Load Button */}
        {tutorialComplete && (
          <>
            <div className="card border-green-200 bg-green-50">
              <h3 className="text-sm font-semibold text-green-900 mb-3">
                ✅ Tutorial Complete — Expected Results
              </h3>
              <div className="space-y-3">
                {selectedScenario.expectedResults.map((result, i) => (
                  <div key={i} className="bg-white rounded-lg p-3 border border-green-200">
                    <div className="flex justify-between items-center mb-1">
                      <span className="text-sm font-medium">{result.label}</span>
                      <span className="text-sm font-bold text-green-700">{result.value}</span>
                    </div>
                    <p className="text-xs text-[var(--muted)]">{result.explanation}</p>
                  </div>
                ))}
              </div>
            </div>

            <div className="card">
              <h3 className="text-sm font-semibold mb-3">Key Takeaways</h3>
              <ul className="space-y-2">
                {selectedScenario.takeaways.map((t, i) => (
                  <li key={i} className="text-sm flex items-start gap-2">
                    <span className="text-[var(--primary)] font-bold mt-0.5">{i + 1}.</span>
                    {t}
                  </li>
                ))}
              </ul>
            </div>

            <div className="card bg-[var(--surface)]">
              <p className="text-xs text-[var(--muted)] mb-1">Source</p>
              <p className="text-sm italic">{selectedScenario.citation}</p>
              <p className="text-xs text-[var(--muted)] mt-1">R source: {selectedScenario.rAppSource}</p>
            </div>

            <button onClick={handleLoadAndCalculate} className="btn-primary w-full text-base py-3">
              Load This Scenario & Calculate ROI →
            </button>
          </>
        )}
      </div>
    );
  }

  // ─── Scenario Picker ────────────────────────────────────────────────────
  return (
    <div className="max-w-4xl mx-auto space-y-8">
      <div className="text-center py-4">
        <h2 className="text-2xl font-bold mb-2">Guided Scenarios & Tutorials</h2>
        <p className="text-[var(--muted)] max-w-2xl mx-auto">
          Each scenario reproduces a published illustration of utility analysis with exact
          parameters, a step-by-step tutorial, and expected results you can verify. Start
          with a beginner scenario and work your way up.
        </p>
      </div>

      {Object.entries(categories).map(([category, scenarios]) => (
        <div key={category}>
          <h3 className="text-sm font-semibold text-[var(--muted)] uppercase tracking-wide mb-3">
            {category}
          </h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
            {scenarios.map((scenario) => (
              <button
                key={scenario.id}
                onClick={() => handleStartScenario(scenario)}
                className="card card-hover text-left"
              >
                <div className="flex items-start gap-3">
                  <span className="text-2xl">{scenario.icon}</span>
                  <div className="flex-1">
                    <div className="font-semibold text-sm">{scenario.title}</div>
                    <div className="text-xs text-[var(--muted)] mt-0.5">{scenario.subtitle}</div>
                    <div className="flex gap-2 mt-2">
                      <span className={`badge ${scenario.difficulty === "beginner" ? "badge-green" : scenario.difficulty === "intermediate" ? "badge-amber" : "badge-blue"}`}>
                        {scenario.difficulty}
                      </span>
                      <span className="badge badge-blue">~{scenario.estimatedMinutes} min</span>
                      <span className="badge badge-blue">
                        {scenario.interventions.length} intervention{scenario.interventions.length > 1 ? "s" : ""}
                      </span>
                    </div>
                  </div>
                </div>
              </button>
            ))}
          </div>
        </div>
      ))}
    </div>
  );
}

function ParamPill({ label, value }: { label: string; value: string }) {
  return (
    <div className="bg-white rounded px-2 py-1 border border-[var(--border)]">
      <div className="text-[10px] text-[var(--muted)] uppercase">{label}</div>
      <div className="font-semibold text-xs">{value}</div>
    </div>
  );
}
