library(shiny)
library(bslib)
library(htmltools)

#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 13:48:34
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 13:48:54
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
            tags$dd("First stable return to expected monthly incidence for three consecutive months."),
            tags$dt("Balance Period (BP)"),
            tags$dd("First month after the cumulative trough when observed-minus-expected incidence returns to zero."),
            tags$dt("Counterfactual scope"),
            tags$dd("24 diseases with stable prepandemic signal and long-horizon forecastability."),
            tags$dt("Seasonal shift view"),
            tags$dd("Monthly profiles compare prepandemic observed, post-PHSM observed, and post-PHSM counterfactual trajectories.")
          )
        )
      ),
      card(
        card_header("Study flow"),
        card_body(
          tableOutput("study_flow_table"),
          p(class = "section-copy compact", "The app loads cached analysis outputs rather than rerunning the forecasting pipeline.")
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Source files"),
        card_body(
          tags$ul(
            class = "source-list",
            tags$li("ScriptAnalysis/temp/month.RData"),
            tags$li("ScriptAnalysis/temp/outcome.RData"),
            tags$li("Outcome/TotalCasesDeaths.csv")
          ),
          p(class = "section-copy compact", "These cached files keep the dashboard fast in review and presentation settings.")
        )
      ),
      card(
        card_header("How to read the app"),
        card_body(
          tags$ul(
            class = "insight-list",
            tags$li("Overview: burden ranking and portfolio-level recovery mix."),
            tags$li("Recovery: disease-level RP, BP, and deficit trajectories."),
            tags$li("Time Series: filtered monthly series for inspection and export."),
            tags$li("Seasonality: peak timing and profile re-alignment after PHSM.")
          )
        )
      )
    )
  )
)