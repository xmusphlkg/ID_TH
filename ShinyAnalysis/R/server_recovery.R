recovery_server <- function(input, output, session) {
  selected_metrics <- recovery_create_selected_metrics(input)

  recovery_bind_plots(input, output)
  recovery_bind_boxes(output, selected_metrics)
  recovery_bind_interpretation(output, selected_metrics)
  recovery_bind_metrics_table(output)
}