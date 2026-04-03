#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 14:05:47
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 14:05:57
#####################################
seasonal_create_selected_profile <- function(input) {
  shiny::reactive({
    shiny::req(input$seasonal_disease)
    seasonal_profile(input$seasonal_disease)
  })
}

seasonal_create_peak_table <- function(selected_seasonal) {
  shiny::reactive({
    seasonal_peaks(selected_seasonal())
  })
}

seasonal_bind_plot <- function(input, output, selected_seasonal) {
  output$seasonal_plot <- shiny::renderPlot({
    season_data <- selected_seasonal()

    ggplot2::ggplot(season_data, ggplot2::aes(x = month, y = normalized, color = period, group = period)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2.6) +
      ggplot2::scale_color_manual(values = c(
        "Observed pre-pandemic" = "#4C6A92",
        "Observed post-PHSM" = "#C54A36",
        "Counterfactual post-PHSM" = "#0B6E69"
      )) +
      ggplot2::labs(
        title = input$seasonal_disease,
        subtitle = "Normalized monthly shape across prepandemic, observed post-PHSM, and counterfactual post-PHSM periods",
        x = NULL,
        y = "Normalized seasonal profile",
        color = NULL
      ) +
      ggplot2::theme_minimal(base_family = "Segoe UI") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(family = "Georgia", face = "bold", size = 16),
        plot.subtitle = ggplot2::element_text(size = 11, color = "#4A5563"),
        legend.position = "top",
        panel.grid.minor = ggplot2::element_blank()
      )
  }, res = 110)
}

seasonal_bind_value_views <- function(input, output, selected_seasonal) {
  output$seasonal_value_plot <- shiny::renderPlot({
    seasonal_value_plot(selected_seasonal(), input$seasonal_disease)
  }, res = 110)

  output$seasonal_value_table <- DT::renderDT({
    DT::datatable(
      seasonal_monthly_wide_table(selected_seasonal()),
      rownames = FALSE,
      options = list(pageLength = 6, autoWidth = TRUE, scrollX = TRUE, dom = "tip")
    )
  })

  output$seasonal_download <- shiny::downloadHandler(
    filename = function() {
      paste0("seasonal-wide-table-", input$seasonal_disease, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(seasonal_monthly_wide_table(selected_seasonal()), file)
    }
  )
}

seasonal_bind_boxes <- function(output, selected_peak_table) {
  output$peak_pre_box <- shiny::renderUI({
    peak_row <- dplyr::filter(selected_peak_table(), period == "Observed pre-pandemic")
    dashboard_build_peak_box("Prepandemic peak", peak_row, "#E6EDF6")
  })

  output$peak_post_box <- shiny::renderUI({
    peak_row <- dplyr::filter(selected_peak_table(), period == "Observed post-PHSM")
    dashboard_build_peak_box("Observed post-PHSM peak", peak_row, "#FBE2DD")
  })

  output$peak_counterfactual_box <- shiny::renderUI({
    peak_row <- dplyr::filter(selected_peak_table(), period == "Counterfactual post-PHSM")
    dashboard_build_peak_box("Counterfactual peak", peak_row, "#E8F1EF")
  })
}

seasonal_bind_interpretation <- function(output, selected_peak_table) {
  output$seasonal_interpretation <- shiny::renderUI({
    dashboard_build_seasonal_interpretation(selected_peak_table())
  })
}