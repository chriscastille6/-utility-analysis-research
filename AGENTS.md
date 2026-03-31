# AGENTS.md

## Cursor Cloud specific instructions

### Project overview

This repository contains two codebases:

1. **R Shiny apps** — the original research platform with standalone utility analysis apps
2. **UA+ web app** (`ua-plus-app/`) — a Next.js platform that unifies all models with guided tutorials, exports, and optional AI chat

### R Shiny apps

Individual apps are launched with `shiny::runApp()`. The main working apps:

| App | Command |
|-----|---------|
| Job Crafting Utility | `Rscript -e 'shiny::runApp("shiny_apps/job_crafting/app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Training Utility | `Rscript -e 'shiny::runApp("training_utility_app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Staffing Utility | `Rscript -e 'shiny::runApp("staffing_utility_app_fixed.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Fisher-Connelly 2017 | `Rscript -e 'shiny::runApp("fisher_connelly_2017_app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Fisher-Connelly 2020 | `Rscript -e 'shiny::runApp("fisher_connelly_2020_app.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |
| Sturman Monte Carlo | `Rscript -e 'shiny::runApp("scripts/sturman_2000_monte_carlo.R", host="0.0.0.0", port=3838, launch.browser=FALSE)'` |

**Known issue:** `app.R` and `app_backup.R` have duplicate code sections causing R parse errors. All standalone `*_app.R` and `shiny_apps/*/app.R` files work correctly.

**R analysis scripts** in `scripts/` and `reproductions/` run with `Rscript`. The `scripts/utilities/sturman_utility_functions.R` file has a `validate_implementation()` function for basic verification.

**System deps for R packages:** `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libfontconfig1-dev`, `libharfbuzz-dev`, `libfribidi-dev`, `libfreetype6-dev`, `libpng-dev`, `libtiff5-dev`, `libjpeg-dev`, `librsvg2-dev`, `libv8-dev`, `libpoppler-cpp-dev`, `cmake`, `pandoc`.

No formal lint/test framework for R; validation is done by sourcing scripts and running apps.

### UA+ Web App (`ua-plus-app/`)

Next.js 15 + TypeScript + Tailwind CSS. Unifies all utility analysis models from the R codebase into a single platform.

| Task | Command |
|------|---------|
| Install deps | `cd ua-plus-app && npm install` |
| Dev server | `cd ua-plus-app && npm run dev` (port 3000) |
| Build | `cd ua-plus-app && npm run build` |
| Lint | `cd ua-plus-app && npx eslint src/` |

**Key features:**
- 9 guided scenarios with step-by-step tutorials reproducing published R app illustrations
- Computation engine: BCG, training utility, Sturman (2000) adjustments, combination model
- Meta-analytic defaults from Schmidt & Hunter, Sackett et al., Morrow et al., Oprea et al.
- Export: Excel (cell-referenced formulas), R script, Python script, PDF summary
- AI chat via Gemini free tier (optional; guided wizard works without it)

**AI chat setup (optional):**
1. Get a free API key at https://aistudio.google.com/apikey (no credit card)
2. Create `ua-plus-app/.env` with `GOOGLE_GENERATIVE_AI_API_KEY=your-key`
3. The app works fully without AI — the guided wizard and all exports are standalone

**Gotcha:** The AI SDK uses `toTextStreamResponse()` (v6 API). If you see streaming issues, check `ai` and `@ai-sdk/google` package versions are compatible.
