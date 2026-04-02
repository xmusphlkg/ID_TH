recovery_create_selected_metrics <- function(input) {
  shiny::reactive({
    shiny::req(input$selected_disease)
    dplyr::filter(metrics, Shortname == input$selected_disease)
  })
}

recovery_bind_plots <- function(input, output) {
  output$trajectory_plot <- shiny::renderPlot({
    trajectory_plot(
      disease = input$selected_disease,
      show_interval = isTRUE(input$trajectory_show_interval)
    )
  }, res = 110)

  output$scatter_plot <- shiny::renderPlot({
    scatter_plot(input$scatter_group)
  }, res = 110)
}

recovery_bind_boxes <- function(output, selected_metrics) {
  output$status_box <- shiny::renderUI({
    dashboard_build_status_box(selected_metrics())
  })

  output$recovery_box <- shiny::renderUI({
    disease_row <- selected_metrics()
    dashboard_build_value_box(
      title = "Recovery period (RP)",
      value = ifelse(is.na(disease_row$Recovery_Months), "Not recovered", paste0(disease_row$Recovery_Months, " months")),
      subtitle = ifelse(is.na(disease_row$Date_Recovery), "No RP by December 2025", format(disease_row$Date_Recovery, "%Y-%m")),
      bg = "#E8F1EF"
    )
  })

  output$balance_box <- shiny::renderUI({
    disease_row <- selected_metrics()
    dashboard_build_value_box(
      title = "Balance period (BP)",
      value = ifelse(is.na(disease_row$Balance_Months), "Not balanced", paste0(disease_row$Balance_Months, " months")),
      subtitle = ifelse(is.na(disease_row$Date_Balance), "No BP by December 2025", format(disease_row$Date_Balance, "%Y-%m")),
      bg = "#FFF3D6"
    )
  })

  output$deficit_box <- shiny::renderUI({
    disease_row <- selected_metrics()
    dashboard_build_value_box(
      title = "Max cumulative deficit",
      value = scales::comma(round(disease_row$Deficit_Absolute)),
      subtitle = paste0("Relative deficit: ", scales::percent(disease_row$Deficit_Percent, accuracy = 0.1)),
      bg = "#FBE2DD"
    )
  })
}

recovery_bind_interpretation <- function(output, selected_metrics) {
  output$disease_interpretation <- shiny::renderUI({
    dashboard_build_disease_interpretation(selected_metrics())
  })
}

recovery_bind_metrics_table <- function(output) {
  output$metrics_table <- DT::renderDT({
    DT::datatable(
      dplyr::transmute(
        metrics,
        Disease = Shortname,
        Group,
        Status,
        `Recovery (months)` = Recovery_Months,
        `Balance (months)` = Balance_Months,
        `Recovery date` = ifelse(is.na(Date_Recovery), "Not recovered", format(Date_Recovery, "%Y-%m")),
        `Balance date` = ifelse(is.na(Date_Balance), "Not balanced", format(Date_Balance, "%Y-%m")),
        `Max deficit` = scales::comma(round(Deficit_Absolute)),
        `Relative deficit` = scales::percent(Deficit_Percent, accuracy = 0.1),
        `Rebound intensity` = round(Rebound_Intensity, 2)
      ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 8, autoWidth = TRUE, scrollX = TRUE)
    )
  })
}