library(shiny)
library(bslib)
library(DT)
library(htmltools)

recovery_panel <- nav_panel(
  "Recovery",
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput("selected_disease", "Disease", choices = disease_choices, selected = "Influenza"),
        selectInput(
          "scatter_group",
          "Transmission group filter",
          choices = c("All", names(group_palette)),
          selected = "All"
        ),
        checkboxInput("trajectory_show_interval", "Show forecast uncertainty bands", value = FALSE),
        tags$div(
          class = "control-note",
          HTML("<strong>RP</strong>: first stable return to expected monthly incidence.<br><strong>BP</strong>: first month cumulative deviation closes back to zero.")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        uiOutput("status_box"),
        uiOutput("recovery_box"),
        uiOutput("balance_box"),
        uiOutput("deficit_box")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Disease-specific trajectory"),
          card_body(plotOutput("trajectory_plot", height = "500px"))
        ),
        card(
          card_header("Key signals"),
          card_body(uiOutput("disease_interpretation"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Cross-disease comparison"),
          card_body(plotOutput("scatter_plot", height = "430px"))
        ),
        card(
          card_header("Metrics table"),
          card_body(DTOutput("metrics_table"))
        )
      )
    )
  )
)