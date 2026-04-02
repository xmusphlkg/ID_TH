####################################
## Main server function
## Delegates to per-module servers
####################################

server <- function(input, output, session) {

  # Core dashboard modules (shared with ShinyDashboard)
  overview_server(input, output, session)
  recovery_server(input, output, session)
  timeseries_server(input, output, session)
  seasonal_server(input, output, session)

  # New modules specific to ShinyAnalysis
  analyze_server(input, output, session)
  methods_server(input, output, session)
}
