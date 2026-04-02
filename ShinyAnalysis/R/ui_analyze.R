library(bslib)
library(htmltools)
library(shiny)

#####################################
## UI — Analyze tab (user-uploaded data)
## Users upload their own longitudinal monthly disease surveillance CSV.
## The app runs two-model (ETS + SARIMA) counterfactual estimation,
## then classifies RP/BP and plots seasonal profiles.
#####################################

analyze_panel <- nav_panel(
  "Analyze",
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        # ---- Upload zone -----------------------------------------
        div(
          class = "upload-zone",
          tags$p(class = "mini-kicker", "Step 1 — Upload your data"),
          fileInput(
            inputId  = "user_file",
            label    = NULL,
            accept   = c(".csv", "text/csv"),
            placeholder = "Browse or drag-and-drop a CSV file"
          ),
          tags$p(
            class = "section-copy compact",
            "Required columns: ", tags$code("date"), " (YYYY-MM-DD or YYYY-MM),",
            " ", tags$code("disease"), " (text label),",
            " ", tags$code("cases"), " (non-negative integer or numeric)."
          )
        ),
        hr(),

        # ---- Configuration ---------------------------------------
        tags$p(class = "mini-kicker", "Step 2 — Configure"),
        selectInput(
          inputId  = "analyze_disease",
          label    = "Disease to analyze",
          choices  = character(0)
        ),
        radioButtons(
          inputId      = "analyze_model",
          label        = "Counterfactual model",
          choiceNames  = c(
            "ETS (fast, recommended)",
            "SARIMA (seasonal ARIMA)",
            "Both — compare"
          ),
          choiceValues = c("ETS", "SARIMA", "Both"),
          selected     = "ETS"
        ),
        sliderInput(
          inputId = "analyze_cutoff_year",
          label   = "Disruption year (PHSM onset)",
          min     = 2018L, max   = 2024L, value = 2020L, step = 1L,
          sep     = ""
        ),
        sliderInput(
          inputId = "analyze_recovery_threshold",
          label   = "RP recovery threshold (%)",
          min     = 80L, max   = 99L, value = 95L, step   = 1L,
          post    = "%"
        ),
        sliderInput(
          inputId = "analyze_persistence",
          label   = "RP persistence window (months)",
          min     = 2L, max   = 6L, value = 3L, step = 1L
        ),
        checkboxInput(
          inputId = "analyze_log_transform",
          label   = "Apply log transform (+ 0.01 offset)",
          value   = TRUE
        ),
        hr(),
        actionButton(
          inputId = "analyze_run",
          label   = tags$span(
            tags$b("Run analysis"),
            tags$small(" [ETS + SARIMA]", style = "opacity:0.75")
          ),
          class   = "btn btn-primary w-100",
          icon    = icon("play-circle")
        ),
        tags$div(
          class = "control-note mt-2",
          "The analysis fits pre-disruption data only. Counterfactual months represent expected incidence absent the disruption.",
          tags$br(),
          tags$small(style = "opacity:0.75",
                     "Minimum recommended training length: 36 months (ideally ≥ 60 months).")
        ),
        hr(),
        downloadButton("analyze_download", "Download results (CSV)", class = "btn btn-outline-secondary w-100")
      ), # end sidebar

      # ---- Main panel -----------------------------------------------
      # Hero / status row
      layout_columns(
        col_widths = c(8, 4),
        card(
          class = "hero-card",
          card_body(
            tags$div(class = "caption-chip", tags$span(icon("upload"), " Upload & Analyze")),
            h2("Run your own recovery analysis"),
            p(class = "hero-lead",
              "Upload monthly disease surveillance data and the framework will automatically fit ",
              "counterfactual models, compute RP and BP, and visualise seasonal re-alignment."),
            div(
              class = "hero-strip",
              div(class = "hero-pill", "ETS & SARIMA counterfactuals"),
              div(class = "hero-pill", "RP / BP classification"),
              div(class = "hero-pill", "Seasonal profile comparison"),
              div(class = "hero-pill", "Downloadable results")
            )
          )
        ),
        card(
          class = "overview-note-card",
          card_body(
            tags$div(class = "mini-kicker", "Required CSV format"),
            HTML('
              <table class="table format-table table-bordered mb-1">
                <thead><tr>
                  <th>date</th><th>disease</th><th>cases</th>
                </tr></thead>
                <tbody>
                  <tr><td>2015-01-01</td><td>Influenza</td><td>1523</td></tr>
                  <tr><td>2015-02-01</td><td>Influenza</td><td>2018</td></tr>
                  <tr><td>…</td><td>…</td><td>…</td></tr>
                </tbody>
              </table>
            '),
            tags$p(class = "section-copy compact",
                   "Multiple diseases per file are supported. Each will appear in the disease selector after upload.")
          )
        )
      ),

      # Status KPI row (shown after analysis)
      layout_column_wrap(
        width = 1 / 4,
        uiOutput("analyze_status_box"),
        uiOutput("analyze_rp_box"),
        uiOutput("analyze_bp_box"),
        uiOutput("analyze_training_box")
      ),

      # Main trajectory + metrics
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Counterfactual trajectory"),
          card_body(
            uiOutput("analyze_plot_area")
          )
        ),
        card(
          card_header("Key metrics"),
          card_body(
            uiOutput("analyze_metrics_card")
          )
        )
      ),

      # Seasonal analysis + data preview
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Seasonal re-alignment profile"),
          card_body(
            plotOutput("analyze_seasonal_plot", height = "400px")
          )
        ),
        card(
          card_header("Uploaded data preview"),
          card_body(
            DTOutput("analyze_data_table")
          )
        )
      ),

      # Methodology notes
      card(
        card_header("Analysis notes & data quality checks"),
        card_body(
          uiOutput("analyze_diagnostics")
        )
      )
    )
  )
)
