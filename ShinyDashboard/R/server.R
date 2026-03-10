#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 12:36:31
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 12:36:49
#####################################
server_bootstrap_dir <- if (exists("app_dir", inherits = TRUE)) {
  get("app_dir", inherits = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (!exists("overview_server", inherits = TRUE) ||
    !exists("recovery_server", inherits = TRUE) ||
    !exists("timeseries_server", inherits = TRUE) ||
    !exists("seasonal_server", inherits = TRUE) ||
    !exists("methods_server", inherits = TRUE)) {
  if (!exists("resolve_paths", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "helpers.R"), envir = environment())
  }
  if (!exists("study_summary", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "data.R"), envir = environment())
  }
  if (!exists("trajectory_plot", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "plots.R"), envir = environment())
  }
  if (!exists("dashboard_build_value_box", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "components.R"), envir = environment())
  }
  if (!exists("overview_server", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_overview.R"), envir = environment())
  }
  if (!exists("recovery_create_selected_metrics", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_recovery_helpers.R"), envir = environment())
  }
  if (!exists("recovery_server", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_recovery.R"), envir = environment())
  }
  if (!exists("seasonal_create_selected_profile", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_seasonal_helpers.R"), envir = environment())
  }
  if (!exists("seasonal_server", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_seasonal.R"), envir = environment())
  }
  if (!exists("timeseries_server", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_timeseries.R"), envir = environment())
  }
  if (!exists("methods_server", inherits = TRUE)) {
    sys.source(file.path(server_bootstrap_dir, "R", "server_methods.R"), envir = environment())
  }
}

server <- function(input, output, session) {
  overview_server(input, output, session)
  recovery_server(input, output, session)
  timeseries_server(input, output, session)
  seasonal_server(input, output, session)
  methods_server(input, output, session)
}