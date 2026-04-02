####################################
## Server — Methods / Reference tab
####################################

methods_server <- function(input, output, session) {

  output$study_flow_table <- renderTable({
    data.frame(
      Step  = c("1. Data acquisition", "2. Data cleaning",
                "3. Ensemble model selection", "4. Counterfactual generation",
                "5. Deficit detection", "6. RP/BP classification",
                "7. Seasonal analysis", "8. Disruption phenotyping"),
      Description = c(
        "Monthly notifiable disease reports (506 surveillance, 2002–2023) for 24 diseases across 77 provinces",
        "Outlier clipping (3 × IQR), zero-inflation check, population-denominator adjustment",
        "ETS and SARIMA variants evaluated across three temporal splits (AIC/BIC/sMAPE); adaptive per-disease selection",
        "Best-fit model trained on pre-disruption data; 500 bootstrap simulation paths; 80% and 95% PI",
        "Cumulative case deficit C(t) = Σ[observed – counterfactual]; disruption onset defined as first month C(t) < 0",
        "RP: first month observed ≥ threshold × counterfactual for ≥ k consecutive months; BP: first month C(t) ≥ 0",
        "Normalised monthly mean profiles compared across pre- and post-disruption periods using radar decomposition",
        "K-means clustering (k = 3) of diseases by deficit depth, RP, BP, and rebound intensity; 2-D PCA visualisation"
      ),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
}
