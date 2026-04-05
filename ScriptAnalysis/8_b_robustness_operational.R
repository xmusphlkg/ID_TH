library(tidyverse)
library(openxlsx)
library(lubridate)

Sys.setlocale("LC_TIME", "C")
remove(list = ls())

# load primary outcome and any optional simulation-time sensitivity outcomes
load("./temp/outcome.RData")
primary_outcome <- outcome

parse_scenario <- function(path) {
  file_name <- basename(path)
  stripped <- sub("^outcome_", "", sub("\\.RData$", "", file_name))
  mat <- stringr::str_match(stripped, "^(.*)_np([0-9]+)_bsts([0-9]+)_seed([0-9]+)$")
  tibble(
    Scenario = ifelse(is.na(mat[1, 2]), stripped, mat[1, 2]),
    n_paths = ifelse(is.na(mat[1, 3]), NA_integer_, as.integer(mat[1, 3])),
    bsts_niter = ifelse(is.na(mat[1, 4]), NA_integer_, as.integer(mat[1, 4])),
    seed = ifelse(is.na(mat[1, 5]), NA_integer_, as.integer(mat[1, 5])),
    OutcomePath = path
  )
}

scenario_files <- list.files("./temp", pattern = "^outcome_.*\\.RData$", full.names = TRUE)
scenario_files <- scenario_files[!grepl("^outcome\\.RData$", basename(scenario_files))]

scenario_manifest <- bind_rows(
  tibble(
    Scenario = "primary",
    n_paths = 1000L,
    bsts_niter = 1000L,
    seed = 20251209L,
    OutcomePath = "./temp/outcome.RData"
  ),
  purrr::map_dfr(scenario_files, parse_scenario)
) |>
  distinct(Scenario, .keep_all = TRUE)

load("./temp/month.RData")
appendix_tables_dir <- file.path("..", "Outcome", "Appendix", "Tables")
dir.create(appendix_tables_dir, showWarnings = FALSE, recursive = TRUE)

data_class <- data_class |>
  filter(Shortname %in% purrr::map_chr(primary_outcome, ~ unique(.x$outcome_data$Shortname)[1]))

get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) return(NA_real_)
  y_diff <- year(end) - year(start)
  m_diff <- month(end) - month(start)
  y_diff * 12 + m_diff
}

calc_status_one <- function(dates,
                            observed,
                            expected,
                            start_date = as.Date("2020-01-01"),
                            recovery_threshold = 0.95,
                            persistence = 3) {
  keep <- dates >= start_date
  df <- tibble(
    date = dates[keep],
    observed = observed[keep],
    expected = expected[keep]
  ) |>
    arrange(date) |>
    mutate(
      diff = observed - expected,
      cum_diff = cumsum(diff),
      cum_expected = cumsum(expected)
    )

  if (nrow(df) == 0) {
    return(tibble(
      Status = "No Deficit",
      Date_Start_Deficit = as.Date(NA),
      Date_Trough = as.Date(NA),
      Date_Recovery = as.Date(NA),
      Date_Balance = as.Date(NA),
      Suppression_Months = NA_real_,
      Payback_Months = NA_real_
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

    is_recovered_trend <- df_search$observed >= df_search$expected * recovery_threshold
    lag_base <- c(df_search$cum_diff[1], head(df_search$cum_diff, -1))
    is_paying_back <- (df_search$cum_diff - lag_base) >= 0

    robust_window <- rep(FALSE, nrow(df_search))
    if (nrow(df_search) >= persistence) {
      for (i in seq_len(nrow(df_search) - persistence + 1)) {
        robust_window[i] <- all((is_recovered_trend & is_paying_back)[i:(i + persistence - 1)])
      }
    }

    recovery_idx <- which(robust_window)[1]
    if (!is.na(recovery_idx)) {
      recovery_date <- df_search$date[recovery_idx]
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

  suppression_months <- get_months(start_deficit_date, recovery_date)
  payback_months <- get_months(trough_date, balance_date)

  if (status == "Suppressed" && !is.na(start_deficit_date)) {
    suppression_months <- get_months(start_deficit_date, max(df$date))
  }

  tibble(
    Status = status,
    Date_Start_Deficit = start_deficit_date,
    Date_Trough = trough_date,
    Date_Recovery = recovery_date,
    Date_Balance = balance_date,
    Suppression_Months = suppression_months,
    Payback_Months = payback_months
  )
}

calc_status_paths <- function(item,
                              start_date = as.Date("2020-01-01"),
                              recovery_threshold = 0.95,
                              persistence = 3) {
  od <- item$outcome_data |>
    arrange(date)

  primary <- calc_status_one(
    dates = od$date,
    observed = od$value,
    expected = od$median,
    start_date = start_date,
    recovery_threshold = recovery_threshold,
    persistence = persistence
  )

  sim_list <- vector("list", ncol(item$MCMC))
  for (j in seq_len(ncol(item$MCMC))) {
    sim_list[[j]] <- calc_status_one(
      dates = od$date,
      observed = od$value,
      expected = item$MCMC[, j],
      start_date = start_date,
      recovery_threshold = recovery_threshold,
      persistence = persistence
    )
  }

  list(primary = primary, sims = bind_rows(sim_list))
}

status_label_map <- c(
  "Debt Repaid" = "Balanced",
  "Recovered" = "Recovered but not balanced",
  "Suppressed" = "Suppressed",
  "No Deficit" = "No deficit"
)

load_outcome_from_file <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  env$outcome
}

build_uncertainty_summary <- function(outcome_data,
                                      start_date = as.Date("2020-01-01"),
                                      recovery_threshold = 0.95,
                                      persistence = 3) {
  purrr::map_dfr(outcome_data, function(item) {
    shortname <- unique(item$outcome_data$Shortname)[1]
    res <- calc_status_paths(item,
                             start_date = start_date,
                             recovery_threshold = recovery_threshold,
                             persistence = persistence)

    primary_status <- res$primary$Status[1]
    sims <- res$sims
    status_prob <- sims |>
      count(Status, name = "n") |>
      mutate(prob = n / sum(n))

    primary_prob <- status_prob |>
      filter(Status == primary_status) |>
      pull(prob)
    if (length(primary_prob) == 0) primary_prob <- 0

    rp_months <- purrr::map_dbl(
      sims$Date_Recovery,
      ~ if (is.na(.x)) NA_real_ else get_months(start_date, .x)
    )
    bp_months <- purrr::map_dbl(
      sims$Date_Balance,
      ~ if (is.na(.x)) NA_real_ else get_months(start_date, .x)
    )

    tibble(
      Shortname = shortname,
      PrimaryStatus = primary_status,
      PrimaryStatusLabel = unname(status_label_map[primary_status]),
      Pr_RP = mean(sims$Status %in% c("Recovered", "Debt Repaid")),
      Pr_BP = mean(sims$Status == "Debt Repaid"),
      Pr_NoDeficit = mean(sims$Status == "No Deficit"),
      PrimaryStatusProb = primary_prob,
      RP_MedianMonths = ifelse(any(!is.na(rp_months)), median(rp_months, na.rm = TRUE), NA_real_),
      RP_Q025Months = ifelse(any(!is.na(rp_months)), quantile(rp_months, 0.025, na.rm = TRUE), NA_real_),
      RP_Q975Months = ifelse(any(!is.na(rp_months)), quantile(rp_months, 0.975, na.rm = TRUE), NA_real_),
      BP_MedianMonths = ifelse(any(!is.na(bp_months)), median(bp_months, na.rm = TRUE), NA_real_),
      BP_Q025Months = ifelse(any(!is.na(bp_months)), quantile(bp_months, 0.025, na.rm = TRUE), NA_real_),
      BP_Q975Months = ifelse(any(!is.na(bp_months)), quantile(bp_months, 0.975, na.rm = TRUE), NA_real_)
    )
  })
}

all_uncertainty_summary <- purrr::map_dfr(seq_len(nrow(scenario_manifest)), function(i) {
  setting <- scenario_manifest[i, ]
  setting_outcome <- load_outcome_from_file(setting$OutcomePath)
  build_uncertainty_summary(setting_outcome) |>
    mutate(
      Scenario = setting$Scenario,
      n_paths = setting$n_paths,
      bsts_niter = setting$bsts_niter,
      seed = setting$seed
    )
}) |>
  left_join(select(data_class, Shortname, Group), by = "Shortname") |>
  mutate(across(c(Pr_RP, Pr_BP, Pr_NoDeficit, PrimaryStatusProb), ~ round(.x, 3)))

uncertainty_summary <- all_uncertainty_summary |>
  filter(Scenario == "primary")

if (nrow(uncertainty_summary) == 0) {
  uncertainty_summary <- all_uncertainty_summary |>
    slice_head(n = length(unique(all_uncertainty_summary$Shortname)))
}

simulation_time_sensitivity <- all_uncertainty_summary |>
  select(Shortname, Group, Scenario, n_paths, bsts_niter,
         PrimaryStatus, Pr_RP, Pr_BP, PrimaryStatusProb,
         RP_MedianMonths, BP_MedianMonths) |>
  left_join(
    all_uncertainty_summary |>
      filter(Scenario == "primary") |>
      select(Shortname,
             PrimaryStatus_ref = PrimaryStatus,
             Pr_RP_ref = Pr_RP,
             Pr_BP_ref = Pr_BP,
             RP_MedianMonths_ref = RP_MedianMonths,
             BP_MedianMonths_ref = BP_MedianMonths),
    by = "Shortname"
  ) |>
  mutate(
    StatusChangedVsPrimary = if_else(is.na(PrimaryStatus_ref), NA, PrimaryStatus != PrimaryStatus_ref),
    Delta_Pr_RP = Pr_RP - Pr_RP_ref,
    Delta_Pr_BP = Pr_BP - Pr_BP_ref,
    Delta_RP_Months = RP_MedianMonths - RP_MedianMonths_ref,
    Delta_BP_Months = BP_MedianMonths - BP_MedianMonths_ref
  )

simulation_time_counts <- simulation_time_sensitivity |>
  filter(Scenario != "primary") |>
  group_by(Scenario, n_paths, bsts_niter) |>
  summarise(
    ReclassifiedDiseases = sum(StatusChangedVsPrimary, na.rm = TRUE),
    MaxAbsDeltaPrRP = if_else(all(is.na(Delta_Pr_RP)), NA_real_, max(abs(Delta_Pr_RP), na.rm = TRUE)),
    MaxAbsDeltaPrBP = if_else(all(is.na(Delta_Pr_BP)), NA_real_, max(abs(Delta_Pr_BP), na.rm = TRUE)),
    MeanAbsDeltaPrRP = if_else(all(is.na(Delta_Pr_RP)), NA_real_, mean(abs(Delta_Pr_RP), na.rm = TRUE)),
    MeanAbsDeltaPrBP = if_else(all(is.na(Delta_Pr_BP)), NA_real_, mean(abs(Delta_Pr_BP), na.rm = TRUE)),
    .groups = "drop"
  )

interrupt_dates <- as.Date(c("2020-01-01", "2020-03-01", "2020-04-01"))

interruption_sensitivity <- purrr::map_dfr(interrupt_dates, function(start_date) {
  purrr::map_dfr(primary_outcome, function(item) {
    shortname <- unique(item$outcome_data$Shortname)[1]
    res <- calc_status_paths(item, start_date = start_date)$primary
    tibble(
      StartDate = as.character(start_date),
      Shortname = shortname,
      Status = res$Status,
      RecoveryDate = res$Date_Recovery,
      BalanceDate = res$Date_Balance
    )
  })
}) |>
  pivot_wider(
    id_cols = Shortname,
    names_from = StartDate,
    values_from = c(Status, RecoveryDate, BalanceDate)
  ) |>
  rename(
    Status_2020_01 = `Status_2020-01-01`,
    Status_2020_03 = `Status_2020-03-01`,
    Status_2020_04 = `Status_2020-04-01`,
    RecoveryDate_2020_01 = `RecoveryDate_2020-01-01`,
    RecoveryDate_2020_03 = `RecoveryDate_2020-03-01`,
    RecoveryDate_2020_04 = `RecoveryDate_2020-04-01`,
    BalanceDate_2020_01 = `BalanceDate_2020-01-01`,
    BalanceDate_2020_03 = `BalanceDate_2020-03-01`,
    BalanceDate_2020_04 = `BalanceDate_2020-04-01`
  ) |>
  mutate(
    Changed_vs_2020_01_for_2020_03 = Status_2020_01 != Status_2020_03,
    Changed_vs_2020_01_for_2020_04 = Status_2020_01 != Status_2020_04
  ) |>
  left_join(select(data_class, Shortname, Group), by = "Shortname")

model_results <- read.xlsx(file.path(appendix_tables_dir, "Model_test_results.xlsx")) |>
  filter(Index %in% c("SMAPE", "RMSE", "MASE")) |>
  pivot_longer(
    cols = starts_with("Test_"),
    names_to = "Split",
    values_to = "Value"
  )

primary_best <- read.xlsx(file.path(appendix_tables_dir, "Best_model_outcome.xlsx")) |>
  filter(Best == 1) |>
  transmute(
    Shortname = disease,
    PrimaryBest = if_else(Method == "Hybrid**", "Hybrid", Method)
  )

rank_choice <- model_results |>
  group_by(disease, Index, Split) |>
  mutate(RankScore = -rank(Value, ties.method = "average")) |>
  ungroup() |>
  group_by(disease, Method) |>
  summarise(TotalScore = sum(RankScore, na.rm = TRUE), .groups = "drop") |>
  group_by(disease) |>
  slice_max(TotalScore, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(Shortname = disease, RankBest = Method)

smape_choice <- model_results |>
  filter(Index == "SMAPE") |>
  group_by(disease, Method) |>
  summarise(TotalSMAPE = sum(Value, na.rm = TRUE), .groups = "drop") |>
  group_by(disease) |>
  slice_min(TotalSMAPE, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(Shortname = disease, SMAPEBest = Method)

split_weights <- c(
  Test_2019 = 0.50,
  Test_2018_2019 = 0.30,
  Test_2017_2019 = 0.20
)

weighted_choice <- model_results |>
  mutate(weight = unname(split_weights[Split])) |>
  group_by(disease, Index, Split) |>
  mutate(ZScore = -(Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(WeightedZ = ZScore * weight) |>
  group_by(disease, Method) |>
  summarise(TotalWeightedScore = sum(WeightedZ, na.rm = TRUE), .groups = "drop") |>
  group_by(disease) |>
  slice_max(TotalWeightedScore, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(Shortname = disease, WeightedBest = Method)

model_selection_sensitivity <- primary_best |>
  left_join(rank_choice, by = "Shortname") |>
  left_join(smape_choice, by = "Shortname") |>
  left_join(weighted_choice, by = "Shortname") |>
  mutate(
    RankMatch = PrimaryBest == RankBest,
    SMAPEMatch = PrimaryBest == SMAPEBest,
    WeightedMatch = PrimaryBest == WeightedBest
  ) |>
  left_join(select(data_class, Shortname, Group), by = "Shortname")

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

season_data_pred <- purrr::map_dfr(primary_outcome, ~ .x$outcome_data) |>
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
    names_from = Period,
    values_from = c(peak_com, peak_to_trough)
  ) |>
  mutate(
    shift_vs_pre = correct_circular_shift(`peak_com_Post-PHSM (Observed)` - `peak_com_Pre-COVID (Observed)`),
    shift_vs_pred = correct_circular_shift(`peak_com_Post-PHSM (Observed)` - `peak_com_Post-PHSM (Predicted)`),
    amplitude_ratio_vs_pre = `peak_to_trough_Post-PHSM (Observed)` / `peak_to_trough_Pre-COVID (Observed)`,
    amplitude_ratio_vs_pred = `peak_to_trough_Post-PHSM (Observed)` / `peak_to_trough_Post-PHSM (Predicted)`
  )

primary_summary <- purrr::map_dfr(primary_outcome, function(item) {
  shortname <- unique(item$outcome_data$Shortname)[1]
  res <- calc_status_paths(item)$primary
  tibble(
    Shortname = shortname,
    PrimaryStatus = res$Status,
    RP_Months = ifelse(is.na(res$Date_Recovery), NA_real_, get_months(as.Date("2020-01-01"), res$Date_Recovery)),
    BP_Months = ifelse(is.na(res$Date_Balance), NA_real_, get_months(as.Date("2020-01-01"), res$Date_Balance))
  )
})

joint_operational_summary <- primary_summary |>
  left_join(select(data_class, Shortname, Group), by = "Shortname") |>
  left_join(
    select(
      uncertainty_summary,
      Shortname,
      Pr_RP,
      Pr_BP,
      PrimaryStatusProb
    ),
    by = "Shortname"
  ) |>
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
  mutate(
    SeasonalDisplacement = ifelse(abs(shift_vs_pre) >= 2 | abs(shift_vs_pred) >= 2, "Shifted", "Stable"),
    MonthlyDashboardView = ifelse(
      PrimaryStatus %in% c("Recovered", "Debt Repaid"),
      "Recovered on monthly incidence",
      "Not recovered on monthly incidence"
    ),
    FrameworkPriority = case_when(
      PrimaryStatus == "Recovered" & SeasonalDisplacement == "Shifted" ~ "Recalibrate and monitor",
      PrimaryStatus == "Recovered" ~ "Cumulative review needed",
      PrimaryStatus == "Debt Repaid" & SeasonalDisplacement == "Shifted" ~ "Recovered but recalibrate seasonality",
      PrimaryStatus == "Debt Repaid" ~ "Low priority routine review",
      PrimaryStatus == "Suppressed" ~ "High priority manual review",
      TRUE ~ "No deficit monitoring"
    )
  )

interruption_counts <- tibble(
  StartDate = c("2020-03-01", "2020-04-01"),
  ChangedDiseasesVs2020_01 = c(
    sum(interruption_sensitivity$Changed_vs_2020_01_for_2020_03, na.rm = TRUE),
    sum(interruption_sensitivity$Changed_vs_2020_01_for_2020_04, na.rm = TRUE)
  )
)

model_selection_counts <- tibble(
  AlternativeRule = c("Rank aggregation", "sMAPE only", "Horizon-weighted composite"),
  MatchesPrimary = c(
    sum(model_selection_sensitivity$RankMatch, na.rm = TRUE),
    sum(model_selection_sensitivity$SMAPEMatch, na.rm = TRUE),
    sum(model_selection_sensitivity$WeightedMatch, na.rm = TRUE)
  ),
  TotalDiseases = 24
)

decision_utility_counts <- joint_operational_summary |>
  count(FrameworkPriority, name = "Diseases")

write.xlsx(
  list(
    ScenarioManifest = scenario_manifest,
    Uncertainty = uncertainty_summary,
    UncertaintyAllScenarios = all_uncertainty_summary,
    SimulationTimeSensitivity = simulation_time_sensitivity,
    SimulationTimeCounts = simulation_time_counts,
    InterruptionSensitivity = interruption_sensitivity,
    InterruptionCounts = interruption_counts,
    ModelSelectionSensitivity = model_selection_sensitivity,
    ModelSelectionCounts = model_selection_counts,
    JointOperationalSummary = joint_operational_summary,
    DecisionUtilityCounts = decision_utility_counts
  ),
  file = file.path(appendix_tables_dir, "New_robustness_operational_summaries.xlsx"),
  overwrite = TRUE
)

write.csv(
  joint_operational_summary,
  file.path(appendix_tables_dir, "Joint_operational_summary.csv"),
  row.names = FALSE
)

write.csv(
  uncertainty_summary,
  file.path(appendix_tables_dir, "Recovery_uncertainty_summary.csv"),
  row.names = FALSE
)

write.csv(
  simulation_time_sensitivity,
  file.path(appendix_tables_dir, "Simulation_time_sensitivity.csv"),
  row.names = FALSE
)

write.csv(
  simulation_time_counts,
  file.path(appendix_tables_dir, "Simulation_time_sensitivity_counts.csv"),
  row.names = FALSE
)

message("Primary status counts:")
print(primary_summary |> count(PrimaryStatus))

message("Low-stability diseases (primary-status probability < 0.80):")
print(
  uncertainty_summary |>
    filter(PrimaryStatusProb < 0.80) |>
    arrange(PrimaryStatusProb) |>
    select(Shortname, Group, PrimaryStatus, PrimaryStatusProb, Pr_RP, Pr_BP)
)

message("Interruption timing changes:")
print(interruption_counts)

message("Model-selection rule matches:")
print(model_selection_counts)

message("Decision-support summary:")
print(decision_utility_counts)

message("Simulation-time sensitivity summary:")
print(simulation_time_counts)
