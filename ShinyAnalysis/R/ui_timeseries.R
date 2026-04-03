library(shiny)
library(bslib)
library(DT)
library(htmltools)

timeseries_panel <- nav_panel(
  "Time Series",
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput(
          "ts_group",
          "Disease group",
          choices = c("All", names(group_palette)),
          selected = "All"
        ),
        selectizeInput(
          "ts_diseases",
          "Diseases",
          choices = disease_choices,
          selected = head(disease_choices, 4),
          multiple = TRUE
        ),
        layout_columns(
          col_widths = c(6, 6),
          actionButton("ts_select_group", "Select All"),
          actionButton("ts_clear_selection", "Clear")
        ),
        checkboxGroupInput(
          "ts_series",
          "Series",
          choices = c("Observed cases", "Counterfactual median"),
          selected = c("Observed cases", "Counterfactual median")
        ),
        radioButtons(
          "ts_download_format",
          "Download format",
          choices = c("Long" = "long", "Wide" = "wide"),
          selected = "long",
          inline = TRUE
        ),
        sliderInput("ts_ncol", "Facet columns", min = 1, max = 4, value = 2, step = 1),
        dateRangeInput(
          "ts_date_range",
          "Date range",
          start = min(all_time_series_data$date, na.rm = TRUE),
          end = max(all_time_series_data$date, na.rm = TRUE),
          min = min(all_time_series_data$date, na.rm = TRUE),
          max = max(all_time_series_data$date, na.rm = TRUE)
        ),
        downloadButton("ts_download", "Download filtered series"),
        tags$div(
          class = "control-note",
          "Filter disease groups, compare observed versus counterfactual series, and export only the current selection."
        )
      ),
      card(
        card_header("Selected time-series comparison"),
        card_body(plotOutput("all_ts_plot", height = "760px"))
      ),
      card(
        card_header("Data table"),
        card_body(DTOutput("all_ts_table"))
      )
    )
  )
)