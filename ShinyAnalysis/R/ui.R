library(bslib)
library(shiny)

# ---- Shared panel objects loaded from copied modules --------------------
# (overview_panel, recovery_panel, timeseries_panel, seasonal_panel defined in ui_*.R)

ui_bootstrap_dir <- if (exists("app_dir", inherits = TRUE)) {
  get("app_dir", inherits = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# Guard: source any missing UI pieces
for (obj_name in c("dashboard_theme", "app_title", "app_header")) {
  if (!exists(obj_name, inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_theme.R"), envir = environment())
    break
  }
}
for (obj_name in c("overview_panel")) {
  if (!exists(obj_name, inherits = TRUE)) {
    if (!exists("resolve_paths", inherits = TRUE))
      sys.source(file.path(ui_bootstrap_dir, "R", "helpers.R"), envir = environment())
    if (!exists("study_summary", inherits = TRUE))
      sys.source(file.path(ui_bootstrap_dir, "R", "data.R"), envir = environment())
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_overview.R"), envir = environment())
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_recovery.R"), envir = environment())
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_timeseries.R"), envir = environment())
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_seasonal.R"), envir = environment())
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_analyze.R"), envir = environment())
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_methods.R"), envir = environment())
    break
  }
}

ui <- page_navbar(
  title        = app_title,
  id           = "main_nav",
  theme        = dashboard_theme,
  window_title = "Thailand ID Recovery Analyzer",
  header       = app_header,
  overview_panel,
  recovery_panel,
  timeseries_panel,
  seasonal_panel,
  analyze_panel,
  methods_panel
)
