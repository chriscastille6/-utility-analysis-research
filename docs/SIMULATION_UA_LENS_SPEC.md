# Simulation UA Lens / tool generator — design specification (v1 draft)

This document captures locked design decisions for a **Utility Analysis (UA) Lens** aligned with a course simulation that exports round reports as Excel files. It is intended for the **UA tool generator agent** and for implementers building parsers, projection logic, and per-domain views.

## Purpose and scope

- **Audience:** Students using UA reasoning over simulation outcomes.
- **Alignment with scoring:** Outputs should be consistent with the **simulation answer key** where the key defines acceptable **ranges** (not necessarily point estimates). The Lens is **not** a shortcut to graded “correct” answers; it structures transparent assumptions and math.
- **v1 non-goals:** Do **not** build UI that compares student outputs to keyed **bands** or publishes unpublished threshold tables. (Earlier exploration of band display is **out of scope** for v1.)

## Repo anchors (sample data and context)

| Resource | Path |
|----------|------|
| Sample round exports (Excel) | [`reports/reports-ccastille-20260311122643/`](../reports/reports-ccastille-20260311122643/) |
| Batch README (file families) | [`reports/reports-ccastille-20260311122643/README.md`](../reports/reports-ccastille-20260311122643/README.md) |
| Simulation incidents (PDF) | [`reports/reports-ccastille-20260311122643/Incidents_for_the_Simulation.pdf`](../reports/reports-ccastille-20260311122643/Incidents_for_the_Simulation.pdf) |
| Pointer / cloud sync note | [`reports/REPORT_BATCH_CCASTILLE_20260311.md`](../reports/REPORT_BATCH_CCASTILLE_20260311.md) |

Export families in the sample batch:

- **decisions-** (6): benefits, programs, review, staffing, training, wages  
- **department-** (7): compensation, dashboard, demographics, development, production, relations, staffing_report  
- **environment-** (4): iperformance, iproduction, itraining, iwages  
- **rationale.xlsx**

## Data ingestion

- **Mechanism:** **Automatic parsing** of official round `.xlsx` exports (not manual-only entry as the primary path).
- **Contract:** Document **sheet names, required columns, and types** per export type; version parsers when the vendor changes layout (`export_schema_version` or equivalent).
- **Errors:** Fail loudly on missing required columns; avoid silent mis-mapping.

## Projection horizon and parameters

- **Horizon:** Prioritize a **long-term view: next 8 quarters** (forward projection is first-class, not an afterthought).
- **Parameters:** Provide **defaults** (from course/sim documentation) and allow students to **adjust** parameters on the **main** path (not relegated to a hidden “advanced” mode).
- **Formula governance:** Core translation rules should follow **official simulation/course definitions** where they exist; student-adjustable knobs are for transparent sensitivity and planning, not smuggling unstated shortcuts.

## Upload and baseline behavior

Students may:

- **Re-upload** a new export each simulation quarter, **or**
- Work from **current** exports and rely on **forward projection** (defaults for the next 8 quarters).

When a **new** quarterly export is uploaded:

- **Default:** Treat it as the new **baseline** and **recompute** the forward **8-quarter** path from that file.
- **Optional:** **Save / duplicate scenario** before upload so prior plans can be compared.

**Parameter reset (hybrid):**

- **Refresh** any field that is **observed or implied by the new export** from that file.
- **Persist** **projection-only** controls and any **non-file** levers until the student changes them.

## Per-domain design rules

### Disability / inclusion (v1)

- There are **no explicit disability hiring/accommodation fields** in the current export batch.
- **Approach:** Include a **proxy lane** using available indicators (e.g., **DEI**, **morale**, **absenteeism**, **turnover** as present in exports).
- **Labeling:** Mark clearly as **indirect / proxy** — not a substitute for explicit disability-specific metrics.

### Staffing — selection quality

- **Both:** **Infer** selection quality (or related constructs) from **encoded** special decisions (e.g., structured interview, performance testing) when the export supports it.
- Always allow **manual override** (and sensible defaults when inference is missing).

## Per-domain Lens outline (for field + formula expansion)

Implementers should instantiate the following sections; each needs **inputs** (from parse + overrides), **UA logic** (per intervention domain), and **outputs** (tables/charts + 8-quarter trajectory where applicable).

| Domain group | Source files (prefix) | Notes |
|--------------|-------------------------|--------|
| Decisions | `decisions-benefits`, `decisions-programs`, `decisions-review`, `decisions-staffing`, `decisions-training`, `decisions-wages` | Cross-link staffing decisions with selection-quality rules above |
| Department | `department-*` | Tie department metrics to decision levers where the sim links them |
| Environment | `environment-*` | Context for productivity, training, wages, performance |
| Rationale | `rationale.xlsx` | Narrative / justification hooks for UA narrative outputs |
| Incidents | `Incidents_for_the_Simulation.pdf` | Scenario narrative; not a numeric parse target unless later structured |

## Open implementation items

1. **Parser schema:** Concrete column map per `.xlsx` type from the sample batch (and versioning strategy).
2. **Formula constants:** Single authoritative source (instructor manual, sim guide, or course spreadsheet) for default monetary and productivity translations used in 8-quarter projection.
3. **Selection vocabulary:** Map sim-specific codes or labels for interventions to inference rules for selection quality.
4. **Testing:** Golden-file tests on sample exports after each parser version bump.

## Related UA research in this repository

- Application suites and methodology: [`Utility_Analysis_Apps_README.md`](../Utility_Analysis_Apps_README.md), [`Fisher_Connelly_Apps_README.md`](../Fisher_Connelly_Apps_README.md)
- Deployment of existing Shiny UA apps (separate from this spec): [`deploy/bayoupal/README.md`](../deploy/bayoupal/README.md)

---

*Draft v1 — update as parser contracts and sim documentation are finalized.*
