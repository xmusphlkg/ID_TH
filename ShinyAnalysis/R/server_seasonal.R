seasonal_server <- function(input, output, session) {
  selected_seasonal <- seasonal_create_selected_profile(input)
  selected_peak_table <- seasonal_create_peak_table(selected_seasonal)

  seasonal_bind_plot(input, output, selected_seasonal)
  seasonal_bind_value_views(input, output, selected_seasonal)
  seasonal_bind_boxes(output, selected_peak_table)
  seasonal_bind_interpretation(output, selected_peak_table)
}