library(bslib)
library(htmltools)

overview_panel <- nav_panel(
  "Overview",
  page_fillable(
    layout_columns(
      col_widths = c(8, 4),
      card(
        class = "hero-card",
        full_screen = FALSE,
        card_body(
          tags$div(class = "caption-chip", "National surveillance view"),
          h2("Recovery is not one signal"),
          p(
            class = "hero-lead",
            "Track burden, incidence recovery, cumulative balance, and seasonal re-alignment after the pandemic shock."
          ),
          div(
            class = "hero-strip",
            div(class = "hero-pill", "24 diseases with counterfactual models"),
            div(class = "hero-pill", "RP and BP tracked separately"),
            div(class = "hero-pill", "Seasonal timing compared before and after PHSM")
          )
        )
      ),
      card(
        class = "overview-note-card",
        card_body(
          tags$div(class = "mini-kicker", "Key Read"),
          h4("What matters on this page"),
          tags$ul(
            class = "insight-list",
            tags$li("RP shows short-term incidence normalization."),
            tags$li("BP shows whether cumulative losses have been repaid.")
          )
        )
      )
    ),
    layout_column_wrap(
      width = 1 / 4,
      value_box(
        class = "kpi-box kpi-emerald",
        title = "Modelled diseases",
        value = study_summary$value[3],
        p("Diseases retained for counterfactual modelling")
      ),
      value_box(
        class = "kpi-box kpi-amber",
        title = "Median RP",
        value = paste0(round(median_rp, 1), " months"),
        p("Median time to operational incidence recovery")
      ),
      value_box(
        class = "kpi-box kpi-coral",
        title = "Median BP",
        value = paste0(round(median_bp, 1), " months"),
        p("Median time to cumulative zero-crossing")
      ),
      value_box(
        class = "kpi-box kpi-slate",
        title = "Recovered without BP",
        value = status_summary$n[status_summary$Status == "Recovered"],
        p("Diseases that regained incidence but not cumulative balance")
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Burden ranking"),
        card_body(
          p(class = "metric-grid-copy", "Rank diseases by cases or deaths to separate high-volume burden from fatal burden."),
          radioButtons(
            inputId = "burden_metric",
            label = NULL,
            choiceNames = burden_choices,
            choiceValues = burden_choices,
            inline = TRUE,
            selected = "Cases"
          ),
          plotOutput("burden_plot", height = "520px")
        )
      ),
      card(
        card_header("Recovery mix"),
        card_body(
          tags$ul(
            class = "insight-list",
            tags$li("13 diseases reached both incidence recovery and cumulative balance."),
            tags$li("7 diseases recovered in incidence but still carried a cumulative deficit."),
            tags$li("3 diseases remained suppressed through follow-up."),
            tags$li("Malaria did not show a sustained cumulative deficit state.")
          ),
          plotOutput("status_group_plot", height = "440px")
        )
      )
    )
  )
)