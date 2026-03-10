#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 14:21:41
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 14:21:55
#####################################
timeseries_server <- function(input, output, session) {
  available_ts_diseases <- shiny::reactive({
    if (identical(input$ts_group, "All")) {
      return(disease_choices)
    }

    all_time_series_data %>%
      dplyr::filter(Group == input$ts_group) %>%
      dplyr::distinct(Shortname) %>%
      dplyr::arrange(Shortname) %>%
      dplyr::pull(Shortname)
  })

  shiny::observeEvent(available_ts_diseases(), {
    selected_values <- input$ts_diseases
    if (is.null(selected_values)) {
      selected_values <- character(0)
    }

    selected <- intersect(selected_values, available_ts_diseases())
    if (length(selected) == 0) {
      selected <- head(available_ts_diseases(), 4)
    }

    shiny::updateSelectizeInput(
      session,
      "ts_diseases",
      choices = available_ts_diseases(),
      selected = selected,
      server = TRUE
    )
  }, ignoreNULL = FALSE)

  shiny::observeEvent(input$ts_select_group, {
    shiny::updateSelectizeInput(
      session,
      "ts_diseases",
      choices = available_ts_diseases(),
      selected = available_ts_diseases(),
      server = TRUE
    )
  })

  shiny::observeEvent(input$ts_clear_selection, {
    shiny::updateSelectizeInput(
      session,
      "ts_diseases",
      choices = available_ts_diseases(),
      selected = character(0),
      server = TRUE
    )
  })

  selected_time_series <- shiny::reactive({
    shiny::req(input$ts_diseases, input$ts_series, input$ts_date_range)

    all_time_series_data %>%
      dplyr::filter(
        Shortname %in% input$ts_diseases,
        date >= input$ts_date_range[1],
        date <= input$ts_date_range[2]
      )
  })

  output$all_ts_plot <- shiny::renderPlot({
    ts_data <- selected_time_series()
    shiny::req(nrow(ts_data) > 0)
    all_time_series_plot(ts_data, input$ts_series, facet_cols = input$ts_ncol)
  }, res = 110)

  output$all_ts_table <- DT::renderDT({
    ts_data <- selected_time_series()

    DT::datatable(
      dplyr::transmute(
        ts_data,
        Disease = Shortname,
        Date = format(date, "%Y-%m"),
        `Observed cases` = round(Observed, 2),
        `Counterfactual median` = round(Counterfactual, 2),
        `80% lower` = round(lower_80, 2),
        `80% upper` = round(upper_80, 2),
        `95% lower` = round(lower_95, 2),
        `95% upper` = round(upper_95, 2)
      ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, autoWidth = TRUE, scrollX = TRUE)
    )
  })

  output$ts_download <- shiny::downloadHandler(
    filename = function() {
      paste0("time-series-", input$ts_download_format, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (identical(input$ts_download_format, "wide")) {
        readr::write_csv(all_time_series_wide_table(selected_time_series(), input$ts_series), file)
      } else {
        readr::write_csv(selected_time_series(), file)
      }
    }
  )
}