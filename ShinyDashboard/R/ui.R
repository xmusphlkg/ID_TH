library(bslib)

ui_bootstrap_dir <- if (exists("app_dir", inherits = TRUE)) {
  get("app_dir", inherits = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (!exists("dashboard_theme", inherits = TRUE) ||
    !exists("app_title", inherits = TRUE) ||
    !exists("app_header", inherits = TRUE) ||
    !exists("overview_panel", inherits = TRUE) ||
    !exists("recovery_panel", inherits = TRUE) ||
    !exists("timeseries_panel", inherits = TRUE) ||
    !exists("seasonal_panel", inherits = TRUE) ||
    !exists("methods_panel", inherits = TRUE)) {
  if (!exists("resolve_paths", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "helpers.R"), envir = environment())
  }
  if (!exists("study_summary", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "data.R"), envir = environment())
  }
  if (!exists("dashboard_theme", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_theme.R"), envir = environment())
  }
  if (!exists("overview_panel", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_overview.R"), envir = environment())
  }
  if (!exists("recovery_panel", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_recovery.R"), envir = environment())
  }
  if (!exists("timeseries_panel", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_timeseries.R"), envir = environment())
  }
  if (!exists("seasonal_panel", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_seasonal.R"), envir = environment())
  }
  if (!exists("methods_panel", inherits = TRUE)) {
    sys.source(file.path(ui_bootstrap_dir, "R", "ui_methods.R"), envir = environment())
  }
}

ui <- page_navbar(
  title = app_title,
  id = "main_nav",
  theme = dashboard_theme,
  window_title = "Thailand Infectious Disease Recovery Dashboard",
  header = app_header,
  overview_panel,
  recovery_panel,
  timeseries_panel,
  seasonal_panel,
  methods_panel
)