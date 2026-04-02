library(shiny)
library(bslib)
library(DT)
library(htmltools)

seasonal_panel <- nav_panel(
  "Seasonality",
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("seasonal_disease", "Disease", choices = disease_choices, selected = "HAV"),
        downloadButton("seasonal_download", "Download Wide Table"),
        tags$div(
          class = "control-note",
          "Profiles are normalized within period so the chart emphasizes timing and shape rather than absolute volume."
        )
      ),
      layout_column_wrap(
        width = 1 / 3,
        uiOutput("peak_pre_box"),
        uiOutput("peak_post_box"),
        uiOutput("peak_counterfactual_box")
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Seasonal re-alignment profile"),
          card_body(plotOutput("seasonal_plot", height = "480px"))
        ),
        card(
          card_header("Peak timing"),
          card_body(uiOutput("seasonal_interpretation"))
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Monthly mean values"),
          card_body(plotOutput("seasonal_value_plot", height = "420px"))
        ),
        card(
          card_header("Wide table"),
          card_body(DTOutput("seasonal_value_table"))
        )
      )
    )
  )
)