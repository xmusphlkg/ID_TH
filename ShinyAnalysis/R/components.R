dashboard_build_value_box <- function(title, value, subtitle = NULL, bg, fg = "#22313F") {
  bslib::value_box(
    title = title,
    value = value,
    if (!is.null(subtitle)) htmltools::tags$p(subtitle),
    theme = bslib::value_box_theme(bg = bg, fg = fg)
  )
}

dashboard_build_status_box <- function(disease_row) {
  palette <- get("status_palette", inherits = TRUE)

  bslib::value_box(
    title = "Status",
    value = disease_row$Status,
    htmltools::tags$p(disease_row$Group),
    theme = bslib::value_box_theme(bg = palette[[disease_row$Status]], fg = "white")
  )
}

dashboard_build_peak_box <- function(title, peak_row, bg) {
  dashboard_build_value_box(
    title = title,
    value = peak_row$peak_month,
    subtitle = scales::comma(round(peak_row$peak_value)),
    bg = bg
  )
}

dashboard_build_disease_interpretation <- function(disease_row) {
  bullet_items <- switch(
    disease_row$Status,
    "Debt Repaid" = c(
      "Monthly incidence recovered and the cumulative deficit closed back to zero.",
      "This is the strongest recovery pattern in the portfolio.",
      paste0("Peak rebound intensity reached ", round(disease_row$Rebound_Intensity, 2), " times the expected level.")
    ),
    "Recovered" = c(
      "Monthly incidence recovered, but the cumulative deficit remains open.",
      "Incidence recovery and cumulative recovery are not aligned.",
      paste0("The disease reached RP in ", disease_row$Recovery_Months, " months but not BP by December 2025.")
    ),
    "Suppressed" = c(
      "The disease stayed below the recovery threshold through follow-up.",
      "This may reflect persistent disruption, surveillance change, or both.",
      "Several vaccine-preventable infections fall into this class."
    ),
    c(
      "The cumulative process did not enter a sustained deficit state after January 2020.",
      "BP is therefore not defined for this trajectory.",
      "Treat this as a distinct post-pandemic pattern rather than a rebound case."
    )
  )

  htmltools::tagList(
    htmltools::tags$p(class = "section-copy compact", paste0(disease_row$Shortname, " | ", disease_row$Group)),
    htmltools::tags$ul(class = "insight-list", lapply(bullet_items, htmltools::tags$li))
  )
}

dashboard_build_seasonal_interpretation <- function(peak_table) {
  pre_peak <- peak_table$peak_month[peak_table$period == "Observed pre-pandemic"]
  post_peak <- peak_table$peak_month[peak_table$period == "Observed post-PHSM"]
  cf_peak <- peak_table$peak_month[peak_table$period == "Counterfactual post-PHSM"]

  htmltools::tagList(
    htmltools::tags$ul(
      class = "insight-list",
      htmltools::tags$li(paste0("Prepandemic peak: ", pre_peak)),
      htmltools::tags$li(paste0("Observed post-PHSM peak: ", post_peak)),
      htmltools::tags$li(paste0("Counterfactual post-PHSM peak: ", cf_peak)),
      htmltools::tags$li("A persistent offset suggests seasonal re-phasing rather than simple volume recovery.")
    )
  )
}