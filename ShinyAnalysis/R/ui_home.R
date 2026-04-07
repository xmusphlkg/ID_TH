library(bslib)
library(htmltools)
library(shiny)

home_panel <- nav_panel(
  "Home",
  page_fillable(
    layout_columns(
      col_widths = c(8, 4),
      card(
        class = "hero-card",
        card_body(
          tags$div(class = "caption-chip", tags$span(icon("wave-square"), " Upload-first workflow")),
          h2("Analyze your own surveillance data"),
          p(
            class = "hero-lead",
            "ShinyAnalysis is the user-facing workbench for monthly disease time series. ",
            "Upload your own CSV, fit counterfactual baselines, inspect RP/BP milestones, ",
            "and compare seasonal profiles after disruption."
          ),
          div(
            class = "hero-strip",
            div(class = "hero-pill", "Monthly CSV upload"),
            div(class = "hero-pill", "ETS and SARIMA comparison"),
            div(class = "hero-pill", "RP / BP recovery metrics"),
            div(class = "hero-pill", "Bundled Thailand example")
          )
        )
      ),
      card(
        class = "mode-card",
        card_body(
          tags$div(class = "mini-kicker", "Two app modes"),
          h3("Upload workspace"),
          p(
            class = "section-copy compact",
            "Use the Upload Analysis tab to run the framework on your own monthly counts."
          ),
          tags$ul(
            class = "insight-list",
            tags$li("Accepts monthly CSV files with date, disease, and cases columns."),
            tags$li("Supports multiple diseases in one file and configurable disruption settings."),
            tags$li("Returns trajectory plots, RP/BP metrics, seasonal profiles, and downloadable output.")
          ),
          tags$hr(),
          h3("Thailand example library"),
          p(
            class = "section-copy compact",
            "The Thailand tabs remain available as an internal example bundle so users can inspect a fully worked case."
          )
        )
      )
    ),
    layout_column_wrap(
      width = 1 / 3,
      card(
        class = "process-card",
        card_body(
          tags$div(class = "workflow-step", "1"),
          h3("Prepare monthly data"),
          p(
            class = "section-copy compact",
            "Provide one row per disease-month, or let the app aggregate duplicate rows within the same month."
          )
        )
      ),
      card(
        class = "process-card",
        card_body(
          tags$div(class = "workflow-step", "2"),
          h3("Run counterfactual analysis"),
          p(
            class = "section-copy compact",
            "Choose the disruption year, recovery threshold, persistence window, and model family before fitting."
          )
        )
      ),
      card(
        class = "process-card",
        card_body(
          tags$div(class = "workflow-step", "3"),
          h3("Review and export"),
          p(
            class = "section-copy compact",
            "Inspect RP/BP timing, deficit depth, rebound intensity, and seasonal realignment, then export the result table."
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("What the app computes"),
        card_body(
          tags$ul(
            class = "insight-list",
            tags$li("Counterfactual trajectories fitted on pre-disruption data only."),
            tags$li("Recovery Point (RP) and Balance Point (BP) using user-defined operational thresholds."),
            tags$li("Absolute and relative deficit summaries, plus rebound intensity."),
            tags$li("Normalized seasonal profile comparisons before and after disruption.")
          )
        )
      ),
      card(
        class = "overview-note-card",
        card_body(
          tags$div(class = "mini-kicker", "Bundled example"),
          h3("Start with Thailand data if needed"),
          p(
            class = "section-copy compact",
            "If you do not have a file ready, load the bundled Thailand example directly in the Upload Analysis tab or browse the Thailand Example menu for fixed demonstration pages."
          ),
          tags$ul(
            class = "insight-list",
            tags$li("Bundled example CSV mirrors the upload schema used by the app."),
            tags$li("Fixed example pages show how the full framework looks after offline preprocessing."),
            tags$li("Both modes live inside the same standalone folder for deployment to shinyapps.io.")
          )
        )
      )
    )
  )
)