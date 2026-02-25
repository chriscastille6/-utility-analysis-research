"use client";

import { useState, useCallback } from "react";
import { InterventionBuilder } from "@/components/InterventionBuilder";
import { ResultsDashboard } from "@/components/ResultsDashboard";
import { ChatInterface } from "@/components/ChatInterface";
import { GuidedScenarios } from "@/components/GuidedScenarios";
import { Header } from "@/components/Header";
import type { InterventionParams, CombinedResult } from "@/lib/models/utility-engine";
import { computeCombinedUtility } from "@/lib/models/utility-engine";

export default function Home() {
  const [interventions, setInterventions] = useState<InterventionParams[]>([]);
  const [results, setResults] = useState<CombinedResult | null>(null);
  const [overlapFactor, setOverlapFactor] = useState(0.15);
  const [activeTab, setActiveTab] = useState<"scenarios" | "build" | "results">("scenarios");
  const [showChat, setShowChat] = useState(false);

  const handleCalculate = useCallback(() => {
    if (interventions.length === 0) return;
    const combined = computeCombinedUtility(interventions, overlapFactor);
    setResults(combined);
    setActiveTab("results");
  }, [interventions, overlapFactor]);

  const handleAddIntervention = useCallback((intervention: InterventionParams) => {
    setInterventions((prev) => [...prev, intervention]);
  }, []);

  const handleRemoveIntervention = useCallback((id: string) => {
    setInterventions((prev) => prev.filter((i) => i.id !== id));
    setResults(null);
  }, []);

  const handleUpdateIntervention = useCallback((id: string, updates: Partial<InterventionParams>) => {
    setInterventions((prev) =>
      prev.map((i) => (i.id === id ? { ...i, ...updates } : i))
    );
    setResults(null);
  }, []);

  const handleLoadScenario = useCallback(
    (scenarioInterventions: InterventionParams[], scenarioOverlap: number) => {
      setInterventions(scenarioInterventions);
      setOverlapFactor(scenarioOverlap);
      const combined = computeCombinedUtility(scenarioInterventions, scenarioOverlap);
      setResults(combined);
      setActiveTab("results");
    },
    []
  );

  const handleAIConfig = useCallback(
    (config: { interventions: Partial<InterventionParams>[]; overlapFactor?: number }) => {
      const newInterventions: InterventionParams[] = config.interventions.map((p, idx) => ({
        id: `ai-${Date.now()}-${idx}`,
        type: p.type ?? "custom",
        label: p.label ?? `Intervention ${idx + 1}`,
        numEmployees: p.numEmployees ?? 100,
        avgSalary: p.avgSalary ?? 50000,
        sdyPercent: p.sdyPercent ?? 40,
        timePeriodYears: p.timePeriodYears ?? 2,
        costPerEmployee: p.costPerEmployee ?? 500,
        fixedCost: p.fixedCost ?? 5000,
        effectSize: p.effectSize,
        validityCoefficient: p.validityCoefficient,
        selectionRatio: p.selectionRatio,
        currentValidity: p.currentValidity,
        ...p,
      }));
      setInterventions(newInterventions);
      if (config.overlapFactor !== undefined) setOverlapFactor(config.overlapFactor);
      const combined = computeCombinedUtility(newInterventions, config.overlapFactor ?? overlapFactor);
      setResults(combined);
      setActiveTab("results");
    },
    [overlapFactor]
  );

  return (
    <div className="min-h-screen flex flex-col">
      <Header onToggleChat={() => setShowChat(!showChat)} showChat={showChat} />

      <div className="flex flex-1 overflow-hidden">
        <main className="flex-1 flex flex-col overflow-hidden">
          {/* Tabs */}
          <div className="flex border-b border-[var(--border)] bg-white px-6">
            <button
              onClick={() => setActiveTab("scenarios")}
              className={`px-4 py-3 text-sm transition-colors ${
                activeTab === "scenarios" ? "tab-active" : "text-[var(--muted)] hover:text-[var(--foreground)]"
              }`}
            >
              Guided Scenarios
            </button>
            <button
              onClick={() => setActiveTab("build")}
              className={`px-4 py-3 text-sm transition-colors ${
                activeTab === "build" ? "tab-active" : "text-[var(--muted)] hover:text-[var(--foreground)]"
              }`}
            >
              Build Analysis
              {interventions.length > 0 && (
                <span className="ml-2 badge badge-blue">{interventions.length}</span>
              )}
            </button>
            <button
              onClick={() => setActiveTab("results")}
              className={`px-4 py-3 text-sm transition-colors ${
                activeTab === "results" ? "tab-active" : "text-[var(--muted)] hover:text-[var(--foreground)]"
              }`}
              disabled={!results}
            >
              Results & ROI
            </button>
          </div>

          <div className="flex-1 overflow-auto p-6">
            {activeTab === "scenarios" && (
              <GuidedScenarios onLoadScenario={handleLoadScenario} />
            )}
            {activeTab === "build" && (
              <InterventionBuilder
                interventions={interventions}
                overlapFactor={overlapFactor}
                onAdd={handleAddIntervention}
                onRemove={handleRemoveIntervention}
                onUpdate={handleUpdateIntervention}
                onOverlapChange={setOverlapFactor}
                onCalculate={handleCalculate}
              />
            )}
            {activeTab === "results" && results && (
              <ResultsDashboard result={results} overlapFactor={overlapFactor} />
            )}
          </div>
        </main>

        {showChat && (
          <aside className="w-[420px] border-l border-[var(--border)] bg-white flex flex-col">
            <ChatInterface onConfigGenerated={handleAIConfig} results={results} />
          </aside>
        )}
      </div>
    </div>
  );
}
