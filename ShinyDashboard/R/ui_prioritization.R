library(shiny)
library(bslib)
library(DT)
library(htmltools)

prioritization_panel <- nav_panel(
  "Prioritization",
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput(
          "priority_group",
          "Disease group",
          choices = c("All", names(group_palette)),
          selected = "All"
        ),
        selectizeInput(
          "priority_category",
          "Priority category",
          choices = priority_choices,
          selected = priority_choices,
          multiple = TRUE
        ),
        radioButtons(
          "priority_shift",
          "Seasonal shift flag",
          choices = c("All", "Shifted", "Stable"),
          selected = "All",
          inline = TRUE
        ),
        downloadButton("priority_download", "Download prioritization table"),
        tags$div(
          class = "control-note",
          HTML("This tab joins recovery phenotype, seasonal displacement, uncertainty, and sensitivity checks into one operational review layer.")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        uiOutput("priority_high_box"),
        uiOutput("priority_cumulative_box"),
        uiOutput("priority_recalibrate_box"),
        uiOutput("priority_low_box")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Portfolio prioritization mix"),
          card_body(plotOutput("priority_plot", height = "440px"))
        ),
        card(
          card_header("How to interpret the categories"),
          card_body(
            tags$ul(
              class = "insight-list",
              tags$li("High priority manual review: no RP by end of follow-up."),
              tags$li("Cumulative review needed: RP achieved but BP unresolved, without major seasonal displacement."),
              tags$li("Recalibrate and monitor: RP achieved but cumulative or seasonal misalignment remains operationally relevant."),
              tags$li("Low priority routine review: balanced and seasonally stable."),
              tags$li("No deficit monitoring: distinct pattern without sustained cumulative deficit.")
            )
          )
        )
      ),
      card(
        card_header("Disease-level prioritization table"),
        card_body(DTOutput("priority_table"))
      )
    )
  )
)
