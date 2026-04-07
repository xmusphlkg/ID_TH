####################################
## Server — Methods / Reference tab
####################################

methods_server <- function(input, output, session) {

  output$study_flow_table <- renderTable({
    data.frame(
      Step  = c(
        "1. Choose a starting point",
        "2. Normalize monthly input",
        "3. Define disruption settings",
        "4. Fit counterfactual models",
        "5. Review recovery outputs",
        "6. Compare seasonality and export"
      ),
      Description = c(
        "Upload your own monthly CSV or load the bundled Thailand example dataset.",
        "Dates are normalized to calendar months and duplicate disease-month rows are summed.",
        "Set disruption year, RP threshold, persistence window, and optional square-root transform.",
        "ETS and SARIMA are fit on pre-disruption data only, with bootstrap predictive intervals.",
        "The app computes RP, BP, deficit depth, relative deficit, rebound intensity, and model-specific trajectories.",
        "Seasonal profiles are compared across pre-disruption, post-disruption observed, and post-disruption counterfactual periods; results can be downloaded as CSV."
      ),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
}
