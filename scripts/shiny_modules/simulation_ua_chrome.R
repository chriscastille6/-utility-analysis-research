# Shared Simulation UA chrome: sidebar entries + tab for course simulation context,
# and a stable portfolio URL for cross-app navigation.
# Sourced from deployed Shiny apps; expects repo root as getwd() or UA_REPO_ROOT.

simulation_ua_repo_root <- function() {
  r <- Sys.getenv("UA_REPO_ROOT", unset = "")
  if (nzchar(r) && dir.exists(r)) return(r)
  d <- getwd()
  for (i in 1:10) {
    if (file.exists(file.path(d, "scripts", "shiny_modules", "simulation_ua_chrome.R"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  getwd()
}

#' Base URL for the portfolio app (path /ua/portfolio/ on BayouPAL).
simulation_ua_portfolio_url <- function() {
  u <- Sys.getenv("SIMULATION_UA_PORTFOLIO_URL", unset = "")
  if (nzchar(u)) return(u)
  "https://bayoupal.nicholls.edu/ua/portfolio/"
}

#' Portfolio URL including ?from=app_id for analytics / context.
simulation_ua_portfolio_href <- function(app_id = "unknown") {
  paste0(simulation_ua_portfolio_url(), "?from=", utils::URLencode(app_id))
}

simulation_ua_docs_base <- function() {
  Sys.getenv(
    "SIMULATION_UA_GITHUB_BASE",
    unset = "https://github.com/chriscastille6/-utility-analysis-research/blob/main"
  )
}

#' tabItems fragment: single tab with guidance + links.
simulation_ua_context_tab_item <- function(app_id = "unknown") {
  base <- simulation_ua_docs_base()
  spec_url <- paste0(base, "/docs/SIMULATION_UA_LENS_SPEC.md")
  reports_url <- paste0(base, "/reports/reports-ccastille-20260311122643/README.md")
  tabItem(
    tabName = "simulation_ua_context",
    fluidRow(
      box(
        width = 12,
        title = "Using utility analysis in your simulation",
        status = "primary",
        solidHeader = TRUE,
        tags$p(
          "This calculator teaches UA methods that also apply to the ",
          tags$strong("simulation"),
          " you run in class: staffing, training, wages, benefits, and other HR investments can be compared on a common economic basis when you supply credible assumptions."
        ),
        tags$p(
          "Use the ",
          tags$strong("UA portfolio"),
          " link in the sidebar to model ",
          tags$em("multiple"),
          " interventions together (e.g., mix of practices and timing over several quarters)."
        ),
        tags$h4("Resources"),
        tags$ul(
          tags$li(tags$a(href = spec_url, "Simulation UA Lens design spec (GitHub)", target = "_blank")),
          tags$li(tags$a(href = reports_url, "Sample simulation export batch (README)", target = "_blank"))
        ),
        tags$p(
          tags$small(
            "App context: ",
            tags$code(app_id),
            ". Set ",
            tags$code("SIMULATION_UA_PORTFOLIO_URL"),
            " / ",
            tags$code("SIMULATION_UA_GITHUB_BASE"),
            " if URLs differ on your host."
          )
        )
      )
    )
  )
}
