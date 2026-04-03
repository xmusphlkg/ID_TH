library(bslib)
library(htmltools)
library(shiny)

#####################################
## Reference / Methods panel for ShinyAnalysis
#####################################

methods_panel <- nav_panel(
  "Reference",
  page_fillable(
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Definitions"),
        card_body(
          tags$div(class = "caption-chip", "Quick reference"),
          tags$dl(
            class = "definition-list",
            tags$dt("Recovery Period (RP)"),
            tags$dd("First stable return to expected monthly incidence for three consecutive months (or as configured)."),
            tags$dt("Balance Period (BP)"),
            tags$dd("First month after the trough when observed-minus-expected cumulative deviation returns to zero."),
            tags$dt("Counterfactual"),
            tags$dd("Model-predicted incidence assuming no disruption — trained on pre-disruption data only."),
            tags$dt("Rebound intensity"),
            tags$dd("Maximum ratio of observed to expected incidence after the trough (values > 1 indicate exceedance)."),
            tags$dt("Relative deficit"),
            tags$dd("Cumulative absolute deficit at trough divided by total cumulative expected incidence."),
            tags$dt("Seasonal profile"),
            tags$dd("Monthly normalized mean incidence pattern across three periods: pre-disruption, post-PHSM observed, post-PHSM counterfactual.")
          )
        )
      ),
      card(
        card_header("Thailand fixed-dataset scope"),
        card_body(
          tableOutput("study_flow_table"),
          p(class = "section-copy compact",
            "The Thailand fixed dataset loads cached analysis outputs. Use the Analyze tab to run your own data.")
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("How to use the Analyze tab"),
        card_body(
          tags$ul(
            class = "insight-list",
            tags$li(tags$b("Upload:"), " A CSV with columns ", tags$code("date"), ", ", tags$code("disease"), ", ", tags$code("cases"), "."),
            tags$li(tags$b("Configure:"), " Select the disease, model, disruption year, recovery threshold."),
            tags$li(tags$b("Run:"), " Click 'Run analysis' — results appear within seconds."),
            tags$li(tags$b("Interpret:"), " RP / BP status boxes and trajectory chart appear automatically."),
            tags$li(tags$b("Download:"), " Export the full result table as a CSV.")
          )
        )
      ),
      card(
        card_header("Model specifications"),
        card_body(
          tags$dl(
            class = "definition-list",
            tags$dt("ETS"),
            tags$dd("Error-trend-seasonal exponential smoothing; AICc model selection via forecast::ets()."),
            tags$dt("SARIMA"),
            tags$dd("Seasonal ARIMA selected by auto.arima() with AICc; seasonal period = 12 months."),
            tags$dt("Both"),
            tags$dd("Runs ETS and SARIMA and overlays both counterfactuals for visual comparison."),
            tags$dt("Log transform"),
            tags$dd("Models are fitted on log(cases + 0.01) and back-transformed for display."),
            tags$dt("Limitations"),
            tags$dd("Monte Carlo uncertainty (1000 paths) is applied to ETS and SARIMA via bootstrap residuals. Results should be interpreted as indicative for small, noisy, or short datasets.")
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data quality guidance"),
        card_body(
          tags$ul(
            class = "insight-list",
            tags$li("Minimum training length: 36 months pre-disruption (≥ 60 recommended)."),
            tags$li("Consistent case definitions throughout the series — breaks in reporting protocol will bias counterfactuals."),
            tags$li("Zero counts are accepted; Laplace smoothing (+0.01) prevents log(0) errors."),
            tags$li("Multiple diseases in a single file are supported — switch disease in the sidebar selector."),
            tags$li("Monthly granularity required — weekly or daily data must be aggregated before upload.")
          )
        )
      ),
      card(
        card_header("Citation"),
        card_body(
          p(class = "section-copy",
            "If you use this dashboard in published work, please cite:"),
          tags$blockquote(
            style = "border-left: 3px solid rgba(10,103,98,0.4); padding-left: 1rem; color: #33424f;",
            "Li K, Xie Y, Zenghuang Y, et al. A digital surveillance framework reveals decoupled and heterogeneous recovery of infectious diseases in Thailand after COVID-19 disruption. 2026."
          ),
          tags$p(
            class = "section-copy compact",
            "Dashboard code: ",
            tags$a(href  = "https://github.com/xmusphlkg/ID_TH",
                   target = "_blank",
                   "https://github.com/xmusphlkg/ID_TH")
          )
        )
      )
    )
  )
)
