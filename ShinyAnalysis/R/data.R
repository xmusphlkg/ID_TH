library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)
library(tibble)

app_bootstrap_dir <- if (exists("app_dir", inherits = TRUE)) {
  get("app_dir", inherits = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (!exists("app_dir", inherits = TRUE)) {
  app_dir <- app_bootstrap_dir
}

if (!exists("resolve_paths", inherits = TRUE) ||
    !exists("normalize_shortname", inherits = TRUE) ||
    !exists("month_diff", inherits = TRUE) ||
    !exists("calculate_disease_metrics", inherits = TRUE)) {
  sys.source(file.path(app_bootstrap_dir, "R", "helpers.R"), envir = environment())
}

utils::globalVariables(c("Cases", "Counterfactual", "Group", "Observed", "Shortname", "date", "month", "month_num", "normalized", "period", "value"))

status_palette <- c(
  "Debt Repaid" = "#0D5D56",
  "Recovered" = "#E59F2A",
  "Suppressed" = "#C54A36",
  "No Deficit" = "#4C6A92"
)

group_palette <- c(
  "Respiratory IDs" = "#D96C47",
  "Vector-borne and zoonotic IDs" = "#0B6E69",
  "Gastrointestinal IDs" = "#E0B04B",
  "Sexually IDs" = "#955E42",
  "Other IDs" = "#5F7DA8"
)

paths <- resolve_paths(app_dir)
data_env <- new.env(parent = emptyenv())
load(file.path(paths$script_root, "temp", "month.RData"), envir = data_env)
load(file.path(paths$script_root, "temp", "outcome.RData"), envir = data_env)

data_month <- data_env$data_month %>%
  mutate(Shortname = normalize_shortname(Shortname))

data_class <- data_env$data_class %>%
  mutate(Shortname = normalize_shortname(Shortname))

outcome <- data_env$outcome
outcome_lookup <- setNames(outcome, purrr::map_chr(outcome, ~ unique(.x$outcome_data$Shortname)[1]))

metrics <- calculate_disease_metrics(outcome) %>%
  mutate(Shortname = normalize_shortname(Shortname)) %>%
  left_join(data_class %>% select(Shortname, Group), by = "Shortname") %>%
  mutate(
    Recovery_Months = purrr::map2_dbl(Date_Start_Deficit, Date_Recovery, month_diff),
    Balance_Months = purrr::map2_dbl(Date_Start_Deficit, Date_Balance, month_diff),
    Deficit_Absolute = abs(Max_Deficit_Raw),
    Deficit_Percent = abs(Relative_Deficit)
  ) %>%
  arrange(factor(Group, levels = names(group_palette)), Shortname)

total_burden <- readr::read_csv(
  file.path(paths$project_root, "Outcome", "TotalCasesDeaths.csv"),
  show_col_types = FALSE
) %>%
  mutate(Shortname = normalize_shortname(Disease)) %>%
  left_join(data_class %>% select(Shortname, Group), by = "Shortname")

study_summary <- tibble(
  label = c("All monitored diseases", "Descriptive analysis", "Counterfactual modelling"),
  value = c(72, 43, 24)
)

status_summary <- metrics %>%
  count(Status, name = "n") %>%
  complete(Status = names(status_palette), fill = list(n = 0))

median_rp <- metrics %>%
  summarise(value = median(Recovery_Months, na.rm = TRUE)) %>%
  pull(value)

median_bp <- metrics %>%
  summarise(value = median(Balance_Months, na.rm = TRUE)) %>%
  pull(value)

burden_choices <- c("Cases", "Deaths")
disease_choices <- metrics$Shortname
seasonal_period_levels <- c("Observed pre-pandemic", "Observed post-PHSM", "Counterfactual post-PHSM")

all_time_series_data <- purrr::map_dfr(outcome, function(item) {
  outcome_data <- item$outcome_data
  if (is.null(outcome_data) || nrow(outcome_data) == 0) {
    return(NULL)
  }

  outcome_data %>%
    transmute(
      Shortname = normalize_shortname(Shortname),
      date = as.Date(date),
      Observed = value,
      Counterfactual = median,
      lower_80,
      upper_80,
      lower_95,
      upper_95
    )
}) %>%
  left_join(data_class %>% select(Shortname, Group), by = "Shortname") %>%
  arrange(Shortname, date)

seasonal_profile <- function(disease) {
  observed <- data_month %>%
    filter(Shortname == disease) %>%
    mutate(period = case_when(
      year(Date) <= 2019 ~ "Observed pre-pandemic",
      year(Date) >= 2023 ~ "Observed post-PHSM",
      TRUE ~ NA_character_,
      .default = NA_character_
    )) %>%
    filter(!is.na(period)) %>%
    mutate(month_num = month(Date)) %>%
    group_by(period, month_num) %>%
    summarise(value = mean(Cases, na.rm = TRUE), .groups = "drop")

  predicted <- outcome_lookup[[disease]]$outcome_data %>%
    filter(year(date) >= 2023) %>%
    mutate(month_num = month(date)) %>%
    group_by(month_num) %>%
    summarise(value = mean(median, na.rm = TRUE), .groups = "drop") %>%
    mutate(period = "Counterfactual post-PHSM")

  bind_rows(observed, predicted) %>%
    complete(period = seasonal_period_levels, month_num = 1:12, fill = list(value = 0)) %>%
    mutate(
      period = factor(period, levels = seasonal_period_levels),
      month = factor(month.abb[month_num], levels = month.abb)
    ) %>%
    group_by(period) %>%
    mutate(
      normalized = if (max(value, na.rm = TRUE) == min(value, na.rm = TRUE)) {
        0.5
      } else {
        (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
      }
    ) %>%
    ungroup()
}

seasonal_peaks <- function(season_df) {
  season_df %>%
    group_by(period) %>%
    slice_max(order_by = normalized, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(period, peak_month = as.character(month), peak_value = value)
}

seasonal_monthly_wide_table <- function(season_df) {
  season_df %>%
    transmute(
      Period = as.character(period),
      Metric = "Average cases",
      Month = as.character(month),
      Value = round(value, 2)
    ) %>%
    bind_rows(
      season_df %>%
        transmute(
          Period = as.character(period),
          Metric = "Normalized",
          Month = as.character(month),
          Value = round(normalized, 3)
        )
    ) %>%
    tidyr::pivot_wider(names_from = Month, values_from = Value) %>%
    arrange(match(Period, seasonal_period_levels), match(Metric, c("Average cases", "Normalized")))
}