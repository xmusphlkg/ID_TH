prioritization_server <- function(input, output, session) {
  filtered_priority_data <- shiny::reactive({
    shiny::req(input$priority_category, input$priority_shift)

    df <- priority_data

    if (!identical(input$priority_group, "All")) {
      df <- df %>% dplyr::filter(Group == input$priority_group)
    }

    df <- df %>% dplyr::filter(FrameworkPriority %in% input$priority_category)

    if (!identical(input$priority_shift, "All")) {
      df <- df %>% dplyr::filter(SeasonalShiftFlag == input$priority_shift)
    }

    df
  })

  priority_count <- function(category) {
    filtered_priority_data() %>%
      dplyr::filter(FrameworkPriority == category) %>%
      nrow()
  }

  output$priority_high_box <- shiny::renderUI({
    dashboard_build_value_box(
      title = "High priority",
      value = priority_count("High priority manual review"),
      subtitle = "No RP by end of follow-up",
      bg = priority_palette[["High priority manual review"]],
      fg = "white"
    )
  })

  output$priority_cumulative_box <- shiny::renderUI({
    dashboard_build_value_box(
      title = "Cumulative review",
      value = priority_count("Cumulative review needed"),
      subtitle = "RP achieved, BP unresolved",
      bg = priority_palette[["Cumulative review needed"]]
    )
  })

  output$priority_recalibrate_box <- shiny::renderUI({
    recalibrate_count <- sum(
      filtered_priority_data()$FrameworkPriority %in% c(
        "Recalibrate and monitor",
        "Recovered but recalibrate seasonality"
      )
    )

    dashboard_build_value_box(
      title = "Recalibration",
      value = recalibrate_count,
      subtitle = "Seasonal review warranted",
      bg = "#EBD9C8"
    )
  })

  output$priority_low_box <- shiny::renderUI({
    low_count <- sum(
      filtered_priority_data()$FrameworkPriority %in% c(
        "Low priority routine review",
        "No deficit monitoring"
      )
    )

    dashboard_build_value_box(
      title = "Lower intensity",
      value = low_count,
      subtitle = "Routine review or monitoring",
      bg = "#DCE9E4"
    )
  })

  output$priority_plot <- shiny::renderPlot({
    df <- filtered_priority_data()
    shiny::req(nrow(df) > 0)
    priority_plot(df)
  }, res = 110)

  output$priority_table <- DT::renderDT({
    df <- filtered_priority_data()

    DT::datatable(
      df %>%
        dplyr::transmute(
          Disease = Shortname,
          Group,
          Priority = FrameworkPriority,
          Phenotype = PrimaryPhenotype,
          `Pr(RP)` = round(Pr_RP, 3),
          `Pr(BP)` = round(Pr_BP, 3),
          `RP month` = RP_Months,
          `BP month` = BP_Months,
          `Shift vs pre` = shift_vs_pre,
          `Shift vs pred` = shift_vs_pred,
          `Amplitude ratio` = round(amplitude_ratio_vs_pre, 2),
          `Status stability` = StatusStability,
          `Model matches` = ModelRuleMatches,
          `Sensitivity stable` = SensitivityStable
        ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, autoWidth = TRUE, scrollX = TRUE)
    )
  })

  output$priority_download <- shiny::downloadHandler(
    filename = function() {
      paste0("prioritization-", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_priority_data(), file)
    }
  )
}
