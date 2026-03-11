# Simulation UA Decision Lens - Technical Build Specification (v1)

## Objective

Build a simulation-facing decision-support layer that helps students evaluate whether assumptions produce favorable or unfavorable utility outcomes in narrow HR domains:

1. Training
2. Staffing
3. Contingent/labor mix
4. DEI/disability proxy

The tool must provide insight under assumptions, not prescriptive recommendations.

---

## Product Requirements (v1)

- Domain-specific "Simulation Decision Lens" for each domain.
- Single-quarter analysis by default.
- Optional 2-4 quarter carry-forward preview.
- Auto-import from simulation `.xlsx` report exports.
- Manual fallback entry for all fields.
- Common assumptions panel shared by all lenses.
- Scenario bands (`low`, `base`, `high`) and sensitivity.
- Explicit driver + fragility explanations ("what flips sign").
- Exportable scenario summaries.
- Usage telemetry to monitor student tool utilization.

---

## Source Data Contract (Simulation Exports)

Expected report batch location in repo:

- `reports/reports-ccastille-20260311122643/`

Expected files:

- `decisions-*.xlsx`
- `department-*.xlsx`
- `environment-*.xlsx`
- `rationale.xlsx`

Key fields available from exports include:

- Training category budgets (new hires, managers, safety, quality)
- Staffing projected shortages/required headcount
- Wages and benefits by level
- KPI panel (turnover, absenteeism, morale, grievances, accident rate, productivity, quality, unit labor cost)
- Special decision text for selection choices
- Women/RM composition and targets

---

## Architecture

## 1) High-level app modules

1. `mod_data_intake`
2. `mod_common_assumptions`
3. `mod_lens_training`
4. `mod_lens_staffing`
5. `mod_lens_contingent`
6. `mod_lens_dei_proxy`
7. `mod_bundle_forecaster`
8. `mod_compare_export`
9. `mod_usage_telemetry`

## 2) Shared service layer

- `R/sim_import_service.R`  
  Parse and normalize `.xlsx` report files.
- `R/sim_field_map.R`  
  Canonical field names and extraction map.
- `R/ua_common_calcs.R`  
  SDy and common savings/cost utilities.
- `R/ua_lens_training.R`
- `R/ua_lens_staffing.R`
- `R/ua_lens_contingent.R`
- `R/ua_lens_dei_proxy.R`
- `R/ua_bundle_engine.R`
- `R/ua_interpretation_service.R`  
  Non-prescriptive text generation.
- `R/usage_logger.R`

## 3) State model

Use a single top-level `reactiveValues()` container:

- `rv$raw_import`
- `rv$canon` (normalized data model)
- `rv$assumptions` (global + scenario bands)
- `rv$lens_results`
- `rv$bundle_results`
- `rv$telemetry`

---

## Canonical Data Model

## 1) Context

- `team_name`
- `quarter_index` (1..12)
- `year_index`
- `headcount_total`

## 2) Workforce and pay

- `job_level_counts`: `lvl5..lvl1`
- `job_level_annual_pay`: `lvl5..lvl1`
- `weighted_avg_annual_salary`
- `sdy_method`: `auto_40pct_weighted_salary | manual`
- `sdy_value`

## 3) KPI snapshot

- `turnover_rate`
- `absenteeism_days`
- `morale`
- `grievances`
- `accident_rate`
- `productivity`
- `quality_index`
- `unit_labor_cost`

## 4) Decision inputs

- Training budget fields by stream
- Program toggles + costs (including DEI)
- Staffing targets (women/RM), projected shortages
- Wage/benefit settings
- Special decisions (selection choices)

## 5) Scenario assumptions

For each `low/base/high`:

- `d_*` effect terms
- affected population terms
- turnover and absentee deltas
- optional domain-specific risk multipliers

---

## Import/Parsing Specification

## 1) Import API

`import_sim_reports(path, strict = FALSE) -> list(raw, canon, warnings, errors)`

Behavior:

- Detect file set availability.
- Parse all supported sheets.
- Normalize to canonical keys.
- Return warnings for missing fields and continue when possible.

## 2) Manual fallback

If import unavailable/incomplete:

- Populate missing required fields from manual form.
- Track source provenance per field (`imported` vs `manual`).

## 3) Validation rules

- `turnover_rate` in `[0,1]`
- non-negative costs/headcounts
- quarter consistency check across files
- SDy must be present before lens calculations

---

## Common Calculation Contracts

## 1) SDy

`calc_sdy_40pct_weighted_salary(job_level_counts, job_level_annual_pay) -> list(weighted_salary, sdy)`

## 2) Savings helpers

- `calc_turnover_savings(...)`
- `calc_absenteeism_savings(...)`
- `calc_break_even_effect(...)`

All helper functions must be deterministic and unit-testable.

---

## Lens Specifications

## A) Training Lens

### Function

`run_training_lens(canon, assumptions, horizon_qtrs = 1) -> training_result`

### Core equations (per scenario)

- `gross_utility = N_affected * SDy * d_training * T`
- `net_utility = gross_utility + turnover_savings + absenteeism_savings - training_spend_total`
- `break_even_d = ...`

### Outputs

- `net_utility_low/base/high`
- `favorability_base`
- decomposition table
- fragility thresholds

## B) Staffing Lens

### Function

`run_staffing_lens(canon, assumptions, horizon_qtrs = 1) -> staffing_result`

### Selection quality

- Infer from special-decision choices by default.
- Manual override optional.

### Core equations

- `selection_utility = hires * SDy * delta_rxy * Zx - selection_cost`
- `capacity_penalty = shortage_penalty_proxy`
- `net_staffing_utility = selection_utility - capacity_penalty + turnover_savings`

### Outputs

- net utility band
- contribution of quality vs capacity constraints
- fragility diagnostics

## C) Contingent/Labor-Mix Lens

### Function

`run_contingent_lens(canon, assumptions, horizon_qtrs = 1) -> contingent_result`

### Scenario modes

- baseline
- moderate contingent fill
- high contingent fill

### Core equations

- estimate net value by scenario using wage/benefit/turnover + coordination risk proxies

### Outputs

- side-by-side scenario utility table
- labor-cost and quality-risk deltas

## D) DEI / Disability-Proxy Lens

### Function

`run_dei_proxy_lens(canon, assumptions, horizon_qtrs = 1) -> dei_result`

### Core equations

- `proxy_utility = productivity_value + turnover_savings + absenteeism_savings + grievance/accident_savings - dei_cost`

### Outputs

- proxy utility band
- channel contribution chart
- explicit proxy disclaimer

---

## Bundle Forecaster (Phase 1.5)

## Function

`run_bundle_forecast(lens_results, bundle_assumptions, horizon_qtrs = 2:4) -> bundle_result`

Each lens must emit standardized deltas:

- `delta_productivity`
- `delta_quality`
- `delta_turnover`
- `delta_absenteeism`
- `cost_one_time`
- `cost_recurring`
- `net_utility_low/base/high`

Bundle engine aggregates these with optional interaction modifiers and discounting.

---

## UI Component Specification

## 1) Data Intake

- file selector for report batch path
- import status card (coverage %, warnings)
- field provenance badge

## 2) Common Assumptions

- SDy panel (auto/manual)
- cost conversion panel (editable defaults)
- scenario band editor (`low/base/high`)

## 3) Each Lens tab

- assumptions subsection
- result card (favorable/unfavorable)
- decomposition chart
- fragility/threshold table
- narrative interpretation (non-prescriptive)

## 4) Compare/Export

- compare up to 3 saved scenarios
- export summary (`pdf/html/csv/json`)

## 5) Usage log view

- run count per team/quarter
- assumption change count
- sensitivity usage indicator

---

## Interpretation Service Rules (Critical)

Output text MUST:

- avoid imperative language ("you should")
- use conditional language ("under these assumptions")
- surface uncertainty and fragility

Template example:

- "Under current assumptions, projected utility is favorable this quarter."
- "Result is most sensitive to [driver]."
- "If [threshold] is crossed, projected utility becomes unfavorable."

---

## Telemetry Specification

## Event types

- `lens_opened`
- `data_imported`
- `assumption_changed`
- `scenario_run`
- `scenario_compared`
- `export_generated`

## Event payload (minimum)

- timestamp
- team_name
- quarter_index
- lens_id
- event_type
- scenario_id/hash
- changed_fields_count (if applicable)

---

## Testing Plan

## 1) Unit tests (calculation layer)

- SDy auto calculations
- lens utility equations
- break-even logic
- fragility threshold correctness

## 2) Parser tests

- expected extraction from each report type
- missing file behavior and fallback
- quarter/team consistency checks

## 3) Integration tests

- import -> assumptions -> lens run -> export pipeline
- manual-only pipeline
- mixed import/manual pipeline

## 4) Acceptance checks

- Each lens runs with imported batch and returns a base result.
- Compare tab shows all four lens outputs.
- Bundle forecaster runs for 2-4 quarter horizon.

---

## Rollout Plan

## Phase 1

- Data Intake + Common Assumptions + Training Lens

## Phase 2

- Staffing Lens + DEI Proxy Lens

## Phase 3

- Contingent Lens + Compare/Export

## Phase 4

- Bundle Forecaster + telemetry dashboards

---

## Out of Scope for v1

- Full causal estimation from simulation internals
- Hard recommendations/decision automation
- Grading workflow automation

---

## Deliverables

1. New modular service files in `R/`
2. Lens UI modules
3. Parser and field-map utilities
4. Usage telemetry logger
5. Unit and integration tests
6. User-facing "Simulation Decision Lens" docs

