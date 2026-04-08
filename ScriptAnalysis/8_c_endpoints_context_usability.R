library(tidyverse)
library(openxlsx)
library(lubridate)

Sys.setlocale("LC_TIME", "C")
remove(list = ls())

load("./temp/outcome.RData")
load("./temp/month.RData")
appendix_tables_dir <- file.path("..", "Outcome", "Appendix", "Tables")
dir.create(appendix_tables_dir, showWarnings = FALSE, recursive = TRUE)

data_class <- data_class |>
  filter(Shortname %in% purrr::map_chr(outcome, ~ unique(.x$outcome_data$Shortname)[1]))

get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) return(NA_real_)
  (year(end) - year(start)) * 12 + month(end) - month(start)
}

find_sustained_date <- function(dates, condition, persistence = 3) {
  if (length(dates) < persistence) return(as.Date(NA))
  for (i in seq_len(length(dates) - persistence + 1)) {
    if (all(condition[i:(i + persistence - 1)])) {
      return(dates[i])
    }
  }
  as.Date(NA)
}

calc_status_one <- function(dates,
                            observed,
                            expected,
                            lower_95 = NULL,
                            upper_95 = NULL,
                            start_date = as.Date("2020-01-01"),
                            recovery_threshold = 0.95,
                            persistence = 3) {
  keep <- dates >= start_date

  df <- tibble(
    date = dates[keep],
    observed = observed[keep],
    expected = expected[keep],
    lower_95 = if (is.null(lower_95)) NA_real_ else lower_95[keep],
    upper_95 = if (is.null(upper_95)) NA_real_ else upper_95[keep]
  ) |>
    arrange(date) |>
    mutate(
      diff = observed - expected,
      cum_diff = cumsum(diff),
      cum_expected = cumsum(expected),
      ratio = if_else(expected > 0, observed / expected, NA_real_)
    )

  if (nrow(df) == 0) {
    return(tibble(
      Status = "No Deficit",
      Date_Start_Deficit = as.Date(NA),
      Date_Trough = as.Date(NA),
      Date_Recovery = as.Date(NA),
      Date_Balance = as.Date(NA),
      Date_PI95 = as.Date(NA),
      Date_Ratio100 = as.Date(NA),
      Date_HalfDeficit = as.Date(NA),
      Suppression_Months = NA_real_,
      Payback_Months = NA_real_,
      HalfDeficit_Months = NA_real_
    ))
  }

  trough_idx <- which.min(df$cum_diff)
  trough_date <- df$date[trough_idx]
  max_deficit_raw <- df$cum_diff[trough_idx]

  first_drop_idx <- which(df$cum_diff < 0)[1]
  start_deficit_date <- if (!is.na(first_drop_idx)) df$date[first_drop_idx] else as.Date(NA)

  recovery_date <- as.Date(NA)
  status <- "No Deficit"

  if (!is.na(start_deficit_date) && max_deficit_raw < 0) {
    df_search <- df |>
      filter(date >= start_deficit_date)

    lag_base <- c(df_search$cum_diff[1], head(df_search$cum_diff, -1))
    is_recovered_trend <- df_search$observed >= df_search$expected * recovery_threshold
    is_paying_back <- (df_search$cum_diff - lag_base) >= 0

    recovery_date <- find_sustained_date(
      dates = df_search$date,
      condition = is_recovered_trend & is_paying_back,
      persistence = persistence
    )

    if (!is.na(recovery_date)) {
      status <- "Recovered"
    } else {
      status <- "Suppressed"
    }
  }

  balance_date <- as.Date(NA)
  if (!is.na(trough_date) && max_deficit_raw < 0) {
    df_post_trough <- df |>
      filter(date > trough_date)
    balance_idx <- which(df_post_trough$cum_diff >= 0)[1]
    if (!is.na(balance_idx)) {
      balance_date <- df_post_trough$date[balance_idx]
      status <- "Debt Repaid"
    }
  }

  pi95_date <- as.Date(NA)
  ratio100_date <- as.Date(NA)
  half_deficit_date <- as.Date(NA)

  if (!is.na(start_deficit_date) && max_deficit_raw < 0) {
    df_search <- df |>
      filter(date >= start_deficit_date)

    within_pi95 <- df_search$observed >= df_search$lower_95 & df_search$observed <= df_search$upper_95
    ratio100 <- df_search$ratio >= 1

    pi95_date <- find_sustained_date(df_search$date, within_pi95, persistence = persistence)
    ratio100_date <- find_sustained_date(df_search$date, ratio100, persistence = persistence)

    df_post_trough <- df |>
      filter(date > trough_date)
    if (nrow(df_post_trough) > 0) {
      half_deficit_idx <- which(df_post_trough$cum_diff >= max_deficit_raw / 2)[1]
      if (!is.na(half_deficit_idx)) {
        half_deficit_date <- df_post_trough$date[half_deficit_idx]
      }
    }
  }

  suppression_months <- get_months(start_deficit_date, recovery_date)
  payback_months <- get_months(trough_date, balance_date)
  half_deficit_months <- get_months(trough_date, half_deficit_date)

  if (status == "Suppressed" && !is.na(start_deficit_date)) {
    suppression_months <- get_months(start_deficit_date, max(df$date))
  }

  tibble(
    Status = status,
    Date_Start_Deficit = start_deficit_date,
    Date_Trough = trough_date,
    Date_Recovery = recovery_date,
    Date_Balance = balance_date,
    Date_PI95 = pi95_date,
    Date_Ratio100 = ratio100_date,
    Date_HalfDeficit = half_deficit_date,
    Suppression_Months = suppression_months,
    Payback_Months = payback_months,
    HalfDeficit_Months = half_deficit_months
  )
}

primary_summary <- purrr::map_dfr(outcome, function(item) {
  shortname <- unique(item$outcome_data$Shortname)[1]
  res <- calc_status_one(
    dates = item$outcome_data$date,
    observed = item$outcome_data$value,
    expected = item$outcome_data$median,
    lower_95 = item$outcome_data$lower_95,
    upper_95 = item$outcome_data$upper_95
  )

  tibble(
    Shortname = shortname,
    Status = res$Status,
    Date_Start_Deficit = res$Date_Start_Deficit,
    Date_Trough = res$Date_Trough,
    Date_Recovery = res$Date_Recovery,
    Date_Balance = res$Date_Balance,
    Recovery_Months = ifelse(is.na(res$Date_Recovery), NA_real_, get_months(as.Date("2020-01-01"), res$Date_Recovery)),
    Balance_Months = ifelse(is.na(res$Date_Balance), NA_real_, get_months(as.Date("2020-01-01"), res$Date_Balance)),
    PI95_Date = res$Date_PI95,
    PI95_Months = ifelse(is.na(res$Date_PI95), NA_real_, get_months(as.Date("2020-01-01"), res$Date_PI95)),
    Ratio100_Date = res$Date_Ratio100,
    Ratio100_Months = ifelse(is.na(res$Date_Ratio100), NA_real_, get_months(as.Date("2020-01-01"), res$Date_Ratio100)),
    HalfDeficit_Date = res$Date_HalfDeficit,
    HalfDeficit_Months = ifelse(is.na(res$Date_HalfDeficit), NA_real_, get_months(as.Date("2020-01-01"), res$Date_HalfDeficit))
  )
}) |>
  left_join(select(data_class, Shortname, Group), by = "Shortname") |>
  mutate(
    PrimaryPhenotype = case_when(
      Status == "Debt Repaid" ~ "Balanced",
      Status == "Recovered" ~ "Recovered but not balanced",
      Status == "Suppressed" ~ "Suppressed",
      TRUE ~ "No deficit"
    ),
    PrimaryRP_Achieved = !is.na(Date_Recovery),
    PI95_Achieved = !is.na(PI95_Date),
    Ratio100_Achieved = !is.na(Ratio100_Date),
    HalfDeficit_Achieved = !is.na(HalfDeficit_Date)
  )

alt_endpoint_focus <- primary_summary |>
  filter(Status == "Recovered") |>
  transmute(
    Shortname,
    Group,
    PrimaryPhenotype,
    RP_Months = Recovery_Months,
    PI95_Months,
    Ratio100_Months,
    HalfDeficit_Months,
    PI95_Achieved,
    Ratio100_Achieved,
    HalfDeficit_Achieved
  )

season_data_obs <- data_month |>
  filter(Shortname %in% data_class$Shortname) |>
  select(Shortname, year = Year, month = Month, value = Cases, Group) |>
  mutate(
    date = ymd(paste(year, month, "01", sep = "-")),
    Period = case_when(
      year <= 2019 ~ "Pre-COVID (Observed)",
      year >= 2023 ~ "Post-PHSM (Observed)",
      TRUE ~ "Pandemic"
    )
  ) |>
  filter(Period != "Pandemic")

season_data_pred <- purrr::map_dfr(outcome, ~ .x$outcome_data) |>
  filter(year(date) >= 2023) |>
  mutate(
    year = year(date),
    month = month(date),
    Period = "Post-PHSM (Predicted)",
    value = median
  ) |>
  left_join(select(data_class, Shortname, Group), by = "Shortname") |>
  select(Shortname, year, month, value, Group, Period)

df_season <- bind_rows(
  season_data_obs |> select(Shortname, year, month, value, Group, Period),
  season_data_pred
)

df_monthly_mean <- df_season |>
  group_by(Shortname, Group, Period, month) |>
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

get_peak_month_com <- function(df) {
  theta <- 2 * pi * (df$month - 1) / 12
  x <- sum(df$avg_value * cos(theta), na.rm = TRUE)
  y <- sum(df$avg_value * sin(theta), na.rm = TRUE)
  peak_month <- round((atan2(y, x) * 12) / (2 * pi)) + 1
  ((peak_month - 1) %% 12) + 1
}

correct_circular_shift <- function(x) {
  ifelse(x > 6, x - 12, ifelse(x < -6, x + 12, x))
}

phase_summary <- df_monthly_mean |>
  group_by(Shortname, Group, Period) |>
  summarise(
    peak_com = get_peak_month_com(pick(month, avg_value)),
    peak_to_trough = max(avg_value, na.rm = TRUE) / pmax(min(avg_value, na.rm = TRUE), 1e-6),
    .groups = "drop"
  ) |>
  pivot_wider(
    id_cols = c(Shortname, Group),
    names_from = Period,
    values_from = c(peak_com, peak_to_trough)
  ) |>
  mutate(
    shift_vs_pre = correct_circular_shift(`peak_com_Post-PHSM (Observed)` - `peak_com_Pre-COVID (Observed)`),
    shift_vs_pred = correct_circular_shift(`peak_com_Post-PHSM (Observed)` - `peak_com_Post-PHSM (Predicted)`),
    amplitude_ratio_vs_pre = `peak_to_trough_Post-PHSM (Observed)` / `peak_to_trough_Pre-COVID (Observed)`,
    amplitude_ratio_vs_pred = `peak_to_trough_Post-PHSM (Observed)` / `peak_to_trough_Post-PHSM (Predicted)`
  )

oxcgrt <- read_csv("../Data/OxCGRT_compact_national_v1.csv", show_col_types = FALSE) |>
  filter(CountryCode == "THA", Jurisdiction == "NAT_TOTAL") |>
  mutate(date = ymd(Date)) |>
  mutate(date = floor_date(date, "month"))

oxcgrt_monthly <- oxcgrt |>
  group_by(month = date) |>
  summarise(
    StringencyIndex = mean(StringencyIndex_Average, na.rm = TRUE),
    GovernmentResponseIndex = mean(GovernmentResponseIndex_Average, na.rm = TRUE),
    SchoolClosing = mean(`C1M_School closing`, na.rm = TRUE),
    InternalMovement = mean(`C7M_Restrictions on internal movement`, na.rm = TRUE),
    InternationalTravel = mean(`C8EV_International travel controls`, na.rm = TRUE),
    TestingPolicy = mean(`H2_Testing policy`, na.rm = TRUE),
    .groups = "drop"
  )

who_covid <- read_csv("../Data/WHO-COVID-19-global-data.csv", show_col_types = FALSE) |>
  filter(Country == "Thailand") |>
  mutate(
    date = ymd(Date_reported),
    month = floor_date(date, "month"),
    New_cases = replace_na(New_cases, 0),
    New_deaths = replace_na(New_deaths, 0)
  ) |>
  group_by(month) |>
  summarise(
    WHO_COVID_Cases = sum(New_cases, na.rm = TRUE),
    WHO_COVID_Deaths = sum(New_deaths, na.rm = TRUE),
    .groups = "drop"
  )

portfolio_monthly <- purrr::map_dfr(outcome, function(item) {
  item$outcome_data |>
    transmute(
      Shortname,
      date,
      observed = value,
      expected = median
    )
}) |>
  group_by(date) |>
  summarise(
    observed = sum(observed, na.rm = TRUE),
    expected = sum(expected, na.rm = TRUE),
    ratio = observed / expected,
    diff = sum(observed - expected, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(date) |>
  mutate(
    cum_diff = cumsum(diff),
    month = floor_date(date, "month")
  ) |>
  select(month, observed, expected, ratio, diff, cum_diff)

contextual_monthly <- portfolio_monthly |>
  left_join(oxcgrt_monthly, by = "month") |>
  left_join(who_covid, by = "month")

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

context_period_summary <- contextual_monthly |>
  mutate(
    Period = case_when(
      month < as.Date("2022-01-01") ~ "2020-2021 restriction-intensive",
      month < as.Date("2023-01-01") ~ "2022 transition",
      month <= max(who_covid$month, na.rm = TRUE) ~ "2023 to 2024-06 WHO-context only",
      TRUE ~ "Late follow-up without external context"
    )
  ) |>
  group_by(Period) |>
  summarise(
    Mean_Portfolio_Ratio = safe_mean(ratio),
    Median_Portfolio_Ratio = median(ratio, na.rm = TRUE),
    Mean_Stringency = safe_mean(StringencyIndex),
    Mean_SchoolClosing = safe_mean(SchoolClosing),
    Mean_InternalMovement = safe_mean(InternalMovement),
    Mean_InternationalTravel = safe_mean(InternationalTravel),
    Mean_TestingPolicy = safe_mean(TestingPolicy),
    Mean_WHO_COVID_Cases = safe_mean(WHO_COVID_Cases),
    .groups = "drop"
  )

context_correlations <- tibble(
  Indicator = c(
    "StringencyIndex",
    "GovernmentResponseIndex",
    "SchoolClosing",
    "InternalMovement",
    "InternationalTravel",
    "TestingPolicy",
    "log1p(WHO_COVID_Cases)"
  ),
  SpearmanRho = c(
    cor(contextual_monthly$ratio, contextual_monthly$StringencyIndex, method = "spearman", use = "pairwise.complete.obs"),
    cor(contextual_monthly$ratio, contextual_monthly$GovernmentResponseIndex, method = "spearman", use = "pairwise.complete.obs"),
    cor(contextual_monthly$ratio, contextual_monthly$SchoolClosing, method = "spearman", use = "pairwise.complete.obs"),
    cor(contextual_monthly$ratio, contextual_monthly$InternalMovement, method = "spearman", use = "pairwise.complete.obs"),
    cor(contextual_monthly$ratio, contextual_monthly$InternationalTravel, method = "spearman", use = "pairwise.complete.obs"),
    cor(contextual_monthly$ratio, contextual_monthly$TestingPolicy, method = "spearman", use = "pairwise.complete.obs"),
    cor(contextual_monthly$ratio, log1p(contextual_monthly$WHO_COVID_Cases), method = "spearman", use = "pairwise.complete.obs")
  )
) |>
  mutate(SpearmanRho = round(SpearmanRho, 3))

find_first_sustained_month <- function(df, condition, persistence = 3) {
  find_sustained_date(df$month, condition, persistence = persistence)
}

low_restriction_month <- find_first_sustained_month(
  contextual_monthly,
  contextual_monthly$StringencyIndex < 20 &
    contextual_monthly$SchoolClosing <= 1 &
    contextual_monthly$InternalMovement <= 1,
  persistence = 3
)

portfolio_norm_month <- find_first_sustained_month(
  contextual_monthly,
  contextual_monthly$ratio >= 0.95,
  persistence = 3
)

context_milestones <- bind_rows(
  contextual_monthly |>
    slice_max(StringencyIndex, n = 1, with_ties = FALSE) |>
    transmute(
      Milestone = "Peak stringency month",
      month,
      PortfolioRatio = ratio,
      StringencyIndex,
      SchoolClosing,
      InternalMovement,
      InternationalTravel,
      WHO_COVID_Cases
    ),
  contextual_monthly |>
    slice_max(WHO_COVID_Cases, n = 1, with_ties = FALSE) |>
    transmute(
      Milestone = "Peak WHO COVID-19 case month",
      month,
      PortfolioRatio = ratio,
      StringencyIndex,
      SchoolClosing,
      InternalMovement,
      InternationalTravel,
      WHO_COVID_Cases
    ),
  contextual_monthly |>
    filter(month == low_restriction_month) |>
    transmute(
      Milestone = "First sustained low-restriction month",
      month,
      PortfolioRatio = ratio,
      StringencyIndex,
      SchoolClosing,
      InternalMovement,
      InternationalTravel,
      WHO_COVID_Cases
    ),
  contextual_monthly |>
    filter(month == portfolio_norm_month) |>
    transmute(
      Milestone = "First sustained portfolio normalization month",
      month,
      PortfolioRatio = ratio,
      StringencyIndex,
      SchoolClosing,
      InternalMovement,
      InternationalTravel,
      WHO_COVID_Cases
    )
) |>
  arrange(month)

heuristic_rubric <- tribble(
  ~Score, ~Definition,
  1, "Weak support: the task is hard to discover or cannot be completed from the interface alone.",
  2, "Partial support: the task is possible but requires searching or interpretation outside the focal view.",
  3, "Good support: the task is directly available in one module with minimal navigation.",
  4, "Strong support: the task is explicit, interpretable, and can be audited or exported."
)

heuristic_tasks <- tribble(
  ~TaskID, ~PublicHealthTask, ~PrimaryModule, ~MinimumInteractions, ~Discoverability, ~Interpretability, ~Auditability, ~SupportStatus, ~ResidualFriction,
  "T1", "Identify which diseases are balanced, RP-only, suppressed, or no-deficit at portfolio level.", "Overview", 1, 4, 4, 3, "Fully supported", "The overview gives counts but not the full disease list in the same card.",
  "T2", "Inspect one disease's RP, BP, deficit depth, and trajectory against its counterfactual.", "Recovery", 1, 4, 4, 4, "Fully supported", "None beyond disease selection.",
  "T3", "Compare several diseases side by side and export the filtered time series for review meetings.", "Time Series", 3, 3, 3, 4, "Fully supported", "Users must decide which subset to prefilter before export.",
  "T4", "Determine whether apparent recovery is accompanied by persistent seasonal timing shift.", "Seasonality", 1, 3, 4, 4, "Fully supported", "The user still has to combine timing and magnitude mentally.",
  "T5", "Check definitions, study flow, and source files before interpreting outputs operationally.", "Reference", 1, 4, 4, 3, "Fully supported", "Reference material is descriptive rather than interactive.",
  "T6", "Translate recovery plus seasonality into an action-oriented prioritization category.", "Prioritization", 1, 4, 4, 4, "Fully supported", "The action labels remain rule-based and should still be reviewed against local workflow."
) |>
  mutate(
    MeanHeuristicScore = round((Discoverability + Interpretability + Auditability) / 3, 2)
  )

heuristic_summary <- heuristic_tasks |>
  summarise(
    TasksAssessed = n(),
    FullySupported = sum(SupportStatus == "Fully supported"),
    SupportedWithExportedTable = sum(SupportStatus == "Supported with exported table"),
    MedianInteractions = median(MinimumInteractions),
    MeanHeuristicScore = round(mean(MeanHeuristicScore), 2)
  ) |>
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")

uncertainty_summary <- read_csv(file.path(appendix_tables_dir, "Recovery_uncertainty_summary.csv"), show_col_types = FALSE)
joint_operational_summary <- read_csv(file.path(appendix_tables_dir, "Joint_operational_summary.csv"), show_col_types = FALSE)
interruption_sensitivity <- read.xlsx(file.path(appendix_tables_dir, "New_robustness_operational_summaries.xlsx"), sheet = "InterruptionSensitivity")
model_selection_sensitivity <- read.xlsx(file.path(appendix_tables_dir, "New_robustness_operational_summaries.xlsx"), sheet = "ModelSelectionSensitivity")
primary_best <- read.xlsx(file.path(appendix_tables_dir, "Best_model_outcome.xlsx")) |>
  filter(Best == 1) |>
  transmute(
    Shortname = disease,
    BestModel = if_else(Method == "Hybrid**", "Hybrid", Method)
  )

main_text_summary <- primary_summary |>
  select(
    Shortname,
    Group,
    PrimaryPhenotype,
    RP_Months = Recovery_Months,
    BP_Months = Balance_Months,
    PI95_Months,
    Ratio100_Months,
    HalfDeficit_Months
  ) |>
  left_join(primary_best, by = "Shortname") |>
  left_join(
    select(
      phase_summary,
      Shortname,
      shift_vs_pre,
      shift_vs_pred,
      amplitude_ratio_vs_pre
    ),
    by = "Shortname"
  ) |>
  left_join(
    select(
      uncertainty_summary,
      Shortname,
      PrimaryStatusProb
    ),
    by = "Shortname"
  ) |>
  left_join(
    select(
      joint_operational_summary,
      Shortname,
      FrameworkPriority
    ),
    by = "Shortname"
  ) |>
  left_join(
    interruption_sensitivity |>
      transmute(
        Shortname,
        InterruptionStable = !(Changed_vs_2020_01_for_2020_03 | Changed_vs_2020_01_for_2020_04)
      ),
    by = "Shortname"
  ) |>
  left_join(
    model_selection_sensitivity |>
      transmute(
        Shortname,
        ModelRuleMatches = RankMatch + SMAPEMatch + WeightedMatch
      ),
    by = "Shortname"
  ) |>
  mutate(
    StatusStability = case_when(
      PrimaryStatusProb >= 0.90 ~ "High",
      PrimaryStatusProb >= 0.75 ~ "Moderate",
      TRUE ~ "Lower"
    ),
    SensitivityStable = if_else(InterruptionStable & ModelRuleMatches >= 2, "Yes", "Partial")
  ) |>
  arrange(factor(Group, levels = unique(data_class$Group)), Shortname)

write_csv(primary_summary, file.path(appendix_tables_dir, "Alternative_endpoint_sensitivity.csv"))
write_csv(contextual_monthly, file.path(appendix_tables_dir, "Contextual_triangulation_monthly.csv"))
write_csv(heuristic_tasks, file.path(appendix_tables_dir, "Interface_heuristic_assessment.csv"))
write_csv(main_text_summary, file.path(appendix_tables_dir, "Main_text_summary_table.csv"))

alt_endpoint_export <- read_csv(
  file.path(appendix_tables_dir, "Alternative_endpoint_sensitivity.csv"),
  show_col_types = FALSE
)

alt_deficit <- alt_endpoint_export[alt_endpoint_export$Status != "No Deficit", ]

alt_endpoint_counts <- tibble(
  Metric = c(
    "TotalDiseasesWithDeficit",
    "PrimaryRP_Achieved",
    "PI95_Achieved",
    "Ratio100_Achieved",
    "HalfDeficit_Achieved",
    "RP_vs_PI95_Agreement",
    "RP_vs_Ratio100_Agreement"
  ),
  Value = c(
    nrow(alt_deficit),
    sum(alt_deficit$PrimaryRP_Achieved, na.rm = TRUE),
    sum(alt_deficit$PI95_Achieved, na.rm = TRUE),
    sum(alt_deficit$Ratio100_Achieved, na.rm = TRUE),
    sum(alt_deficit$HalfDeficit_Achieved, na.rm = TRUE),
    sum(alt_deficit$PrimaryRP_Achieved == alt_deficit$PI95_Achieved, na.rm = TRUE),
    sum(alt_deficit$PrimaryRP_Achieved == alt_deficit$Ratio100_Achieved, na.rm = TRUE)
  )
)

write.xlsx(
  list(
    AlternativeEndpoints = primary_summary,
    AlternativeEndpointCounts = alt_endpoint_counts,
    RPOnlyAlternativeEndpoints = alt_endpoint_focus,
    ContextPeriods = context_period_summary,
    ContextCorrelations = context_correlations,
    ContextMilestones = context_milestones,
    HeuristicRubric = heuristic_rubric,
    HeuristicTasks = heuristic_tasks,
    HeuristicSummary = heuristic_summary,
    MainTextSummary = main_text_summary
  ),
  file = file.path(appendix_tables_dir, "New_endpoint_context_usability_summaries.xlsx"),
  overwrite = TRUE
)

message("Alternative endpoint counts:")
print(alt_endpoint_counts)

message("Context period summary:")
print(context_period_summary)

message("Context correlations:")
print(context_correlations)

message("Heuristic summary:")
print(heuristic_summary)
