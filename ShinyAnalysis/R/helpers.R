library(dplyr)
library(lubridate)
library(purrr)
library(tibble)

utils::globalVariables(c(
  "Balance_Months", "Cases", "Date_Balance", "Date_Recovery", "Deficit_Absolute",
  "Deficit_Percent", "Group", "Recovery_Months", "Rebound_Intensity", "Shortname",
  "Status", "lower_80", "lower_95", "month", "normalized", "peak_month",
  "period", "ratio", "upper_80", "upper_95", "value", "metrics",
  "status_palette", "status_group_plot", "overview_server", "recovery_server",
  "seasonal_server", "methods_server", "trajectory_plot", "scatter_plot",
  "timeseries_server", "burden_plot", "seasonal_profile", "seasonal_peaks", "study_summary",
  "seasonal_monthly_wide_table", "seasonal_value_plot", "all_time_series_plot", "all_time_series_wide_table",
  "dashboard_build_status_box", "dashboard_build_value_box", "dashboard_build_peak_box",
  "dashboard_build_disease_interpretation", "dashboard_build_seasonal_interpretation",
  "recovery_create_selected_metrics", "recovery_bind_plots", "recovery_bind_boxes",
  "recovery_bind_interpretation", "recovery_bind_metrics_table",
  "seasonal_create_selected_profile", "seasonal_create_peak_table",
  "seasonal_bind_plot", "seasonal_bind_value_views", "seasonal_bind_boxes", "seasonal_bind_interpretation",
  "all_time_series_data", "seasonal_period_levels", "timeseries_panel"
))

if (FALSE) {
  Balance_Months <- Cases <- Date_Balance <- Date_Recovery <- Deficit_Absolute <- NULL
  Deficit_Percent <- Group <- Recovery_Months <- Rebound_Intensity <- Shortname <- NULL
  Status <- lower_80 <- lower_95 <- month <- normalized <- peak_month <- period <- NULL
  ratio <- upper_80 <- upper_95 <- value <- metrics <- status_palette <- NULL
  status_group_plot <- overview_server <- recovery_server <- seasonal_server <- methods_server <- timeseries_server <- NULL
  trajectory_plot <- scatter_plot <- burden_plot <- seasonal_profile <- seasonal_peaks <- study_summary <- NULL
  seasonal_monthly_wide_table <- seasonal_value_plot <- all_time_series_plot <- all_time_series_wide_table <- NULL
  all_time_series_data <- seasonal_period_levels <- timeseries_panel <- NULL
  dashboard_build_status_box <- dashboard_build_value_box <- dashboard_build_peak_box <- NULL
  dashboard_build_disease_interpretation <- dashboard_build_seasonal_interpretation <- NULL

  recovery_create_selected_metrics <- function(input) NULL
  recovery_bind_plots <- function(input, output) NULL
  recovery_bind_boxes <- function(output, selected_metrics) NULL
  recovery_bind_interpretation <- function(output, selected_metrics) NULL
  recovery_bind_metrics_table <- function(output) NULL

  seasonal_create_selected_profile <- function(input) NULL
  seasonal_create_peak_table <- function(selected_seasonal) NULL
  seasonal_bind_plot <- function(input, output, selected_seasonal) NULL
  seasonal_bind_value_views <- function(input, output, selected_seasonal) NULL
  seasonal_bind_boxes <- function(output, selected_peak_table) NULL
  seasonal_bind_interpretation <- function(output, selected_peak_table) NULL
}

resolve_paths <- function(app_dir = getwd()) {
  candidates <- list(
    list(
      project_root = normalizePath(file.path(app_dir, "data"), winslash = "/", mustWork = FALSE),
      script_root = normalizePath(file.path(app_dir, "data"), winslash = "/", mustWork = FALSE)
    ),
    list(
      project_root = normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = FALSE),
      script_root = normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE)
    ),
    list(
      project_root = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
      script_root = normalizePath(file.path(getwd(), "ScriptAnalysis"), winslash = "/", mustWork = FALSE)
    ),
    list(
      project_root = normalizePath(file.path(getwd(), "../.."), winslash = "/", mustWork = FALSE),
      script_root = normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
    )
  )

  for (candidate in candidates) {
    if (file.exists(file.path(candidate$script_root, "temp", "month.RData"))) {
      return(candidate)
    }
  }

  stop("Could not locate bundled or repository data files for the Shiny dashboard.")
}

month_diff <- function(start, end) {
  if (is.na(start) || is.na(end)) {
    return(NA_real_)
  }

  (year(end) - year(start)) * 12 + month(end) - month(start)
}

calculate_disease_metrics <- function(outcome_list, start_date_calc = as.Date("2020-01-01"), recovery_threshold = 0.95) {
  purrr::map_dfr(outcome_list, function(item) {
    outcome_data <- item$outcome_data
    if (is.null(outcome_data) || nrow(outcome_data) == 0) {
      return(NULL)
    }

    shortname <- unique(outcome_data$Shortname)[1]

    df_calc <- outcome_data %>%
      filter(date >= start_date_calc) %>%
      arrange(date) %>%
      mutate(
        diff = value - median,
        cum_diff = cumsum(diff),
        cum_median = cumsum(median)
      )

    if (nrow(df_calc) == 0) {
      return(NULL)
    }

    trough_idx <- which.min(df_calc$cum_diff)
    trough_date <- df_calc$date[trough_idx]
    max_deficit_raw <- df_calc$cum_diff[trough_idx]
    cum_expected_at_trough <- df_calc$cum_median[trough_idx]
    relative_deficit <- if (cum_expected_at_trough > 0) max_deficit_raw / cum_expected_at_trough else NA_real_

    first_drop_idx <- which(df_calc$cum_diff < 0)[1]
    start_deficit_date <- if (!is.na(first_drop_idx)) df_calc$date[first_drop_idx] else as.Date(NA)

    recovery_date <- as.Date(NA)
    balance_date <- as.Date(NA)
    rebound_intensity <- NA_real_
    status <- "No Deficit"

    if (!is.na(start_deficit_date) && max_deficit_raw < 0) {
      df_search <- df_calc %>%
        filter(date >= start_deficit_date)

      is_recovered_trend <- df_search$value >= df_search$median * recovery_threshold
      is_paying_back <- df_search$cum_diff - dplyr::lag(df_search$cum_diff, default = df_search$cum_diff[1]) >= 0

      robust_recovery <- zoo::rollapply(
        is_recovered_trend & is_paying_back,
        width = 3,
        FUN = all,
        fill = FALSE,
        align = "left"
      )

      recovery_idx <- which(robust_recovery)[1]
      if (!is.na(recovery_idx)) {
        recovery_date <- df_search$date[recovery_idx]
        status <- "Recovered"
      } else {
        status <- "Suppressed"
      }

      df_post_trough <- df_calc %>%
        filter(date > trough_date)

      balance_idx <- which(df_post_trough$cum_diff >= 0)[1]
      if (!is.na(balance_idx)) {
        balance_date <- df_post_trough$date[balance_idx]
        status <- "Debt Repaid"
      }

      rebound_intensity <- df_calc %>%
        filter(date >= trough_date) %>%
        transmute(ratio = value / (median + 1)) %>%
        summarise(ratio = max(ratio, na.rm = TRUE)) %>%
        pull(ratio)
    }

    tibble(
      Shortname = shortname,
      Date_Start_Deficit = start_deficit_date,
      Date_Trough = trough_date,
      Date_Recovery = recovery_date,
      Date_Balance = balance_date,
      Max_Deficit_Raw = max_deficit_raw,
      Relative_Deficit = relative_deficit,
      Rebound_Intensity = rebound_intensity,
      Status = status
    )
  })
}

normalize_shortname <- function(x) {
  dplyr::recode(
    x,
    "HFM" = "HFMD",
    "HepatitisA" = "HAV",
    "HepatitisB" = "HBV",
    "HepatitisC" = "HCV",
    "Condyloma Acuminata" = "CA (HPV)",
    "Genital Herpes Simplex" = "Genital herpes",
    "DHF Total" = "Dengue fever",
    "Streptococcus suis" = "S. suis",
    .default = x
  )
}