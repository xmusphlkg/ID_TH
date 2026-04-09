#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(openxlsx)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(parallel)
})

resolve_script_dir_early <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_hits <- grep(paste0("^", file_arg), args, value = TRUE)

  if (length(file_hits) > 0) {
    script_path <- sub(file_arg, "", file_hits[1])
    if (!is.na(script_path) && nzchar(script_path)) {
      return(dirname(normalizePath(path.expand(script_path), winslash = "/", mustWork = FALSE)))
    }
  }

  if (dir.exists("ScriptAnalysis")) {
    return(normalizePath("ScriptAnalysis", winslash = "/", mustWork = FALSE))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

script_dir <- resolve_script_dir_early()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
setwd(script_dir)

source("./function/theme_set.R")
source("./function/revision_utils.R")

load(file.path(script_dir, "temp", "month.RData"))
load(file.path(script_dir, "temp", "outcome.RData"))

modelled_shortnames <- sort(unique(vapply(
  outcome,
  function(item) unique(item$outcome_data$Shortname)[1],
  character(1)
)))

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
weekly_cases_dir <- file.path(project_root, "Data", "WeeklyCasesData")
workbook_path <- file.path(project_root, "Data", "TotalCasesDeaths.xlsx")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

read_weekly_cases <- function() {
  data_map_name <- read.xlsx(workbook_path, sheet = "DiseaseName") |>
    filter(!is.na(short_name))

  csv_files <- list.files(
    path = weekly_cases_dir,
    pattern = "csv",
    include.dirs = TRUE,
    all.files = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  csv_files <- csv_files[
    !grepl("__", basename(csv_files), fixed = TRUE) &
      !is.na(file.info(csv_files)$size) &
      file.info(csv_files)$size > 0
  ]

  data_list <- lapply(csv_files, function(f) {
    tryCatch(
      read.csv(f) |>
        mutate(filename = f),
      error = function(e) NULL
    )
  })

  data_week_raw <- bind_rows(data_list)
  names(data_week_raw) <- c("week_value", "week_alias", "location_value", "location_alias", "cases", "filename")

  data_week_raw |>
    mutate(
      filepath = as.character(filename),
      filename_only = basename(filepath),
      disease_full = tools::file_path_sans_ext(filename_only),
      year = str_extract(filepath, "(?<=/|\\\\)\\d{3,4}(?=/|\\\\)"),
      year = if_else(is.na(year), str_extract(filepath, "\\d{3,4}"), year),
      year = as.integer(year) - 543,
      age_group = if_else(str_detect(disease_full, "__"), str_replace(disease_full, ".*__", ""), NA_character_),
      disease = str_replace(disease_full, "__.*$", "")
    ) |>
    filter(year >= 2020, year <= 2025) |>
    filter(location_value == "%all%", is.na(age_group), week_value != "%all%") |>
    left_join(data_map_name, by = c("disease" = "original_name")) |>
    filter(!is.na(short_name)) |>
    transmute(year, Shortname = short_name, week = as.integer(week_value), cases = as.numeric(cases)) |>
    group_by(year, Shortname, week) |>
    summarize(cases = sum(cases, na.rm = TRUE), .groups = "drop")
}

build_date_maps <- function() {
  data_date_seq <- tibble(
    date = seq.Date(from = as.Date("2019-12-01"), to = as.Date("2026-01-31"), by = "day")
  ) |>
    mutate(
      year = year(date),
      week = isoweek(date),
      month = month(date),
      year = if_else(month == 12 & week == 1, year + 1L, year),
      year = if_else(month == 1 & week >= 52, year - 1L, year)
    ) |>
    filter(date >= as.Date("2020-01-01"), date < as.Date("2026-01-01"))

  week_date_map <- data_date_seq |>
    group_by(year, week) |>
    summarize(week_start = min(date), dates = list(date), .groups = "drop")

  list(data_date_seq = data_date_seq, week_date_map = week_date_map)
}

reconstruct_monthly_day_allocation <- function(weekly_cases, date_maps) {
  weekly_cases |>
    left_join(date_maps$week_date_map, by = c("year", "week")) |>
    rowwise() |>
    mutate(dates = list(unlist(dates))) |>
    tidyr::unnest_longer(dates) |>
    mutate(month = month(dates)) |>
    group_by(year, Shortname, week) |>
    mutate(total_days = n()) |>
    ungroup() |>
    group_by(year, Shortname, week, month, cases, total_days) |>
    summarize(n_days = n(), .groups = "drop") |>
    mutate(month_cases = cases * (n_days / total_days)) |>
    group_by(year, Shortname, month) |>
    summarize(cases = round(sum(month_cases, na.rm = TRUE)), .groups = "drop")
}

compute_phase_summary <- function(observed_monthly) {
  season_data_obs <- observed_monthly |>
    filter(Shortname %in% modelled_shortnames) |>
    mutate(
      date = as.Date(paste(Year, Month, "01", sep = "-")),
      Period = case_when(
        Year <= 2019 ~ "Pre-COVID (Observed)",
        Year >= 2023 ~ "Post-PHSM (Observed)",
        TRUE ~ "Pandemic"
      )
    ) |>
    filter(Period != "Pandemic") |>
    select(Shortname, year = Year, month = Month, value = Cases, Group, Period)

  season_data_pred <- bind_rows(lapply(outcome, function(item) {
    item$outcome_data |>
      filter(year(date) >= 2023) |>
      transmute(
        Shortname = unique(Shortname),
        year = year(date),
        month = month(date),
        value = median,
        Period = "Post-PHSM (Predicted)"
      )
  })) |>
    left_join(select(observed_monthly, Shortname, Group) |> distinct(), by = "Shortname") |>
    distinct()

  df_season <- bind_rows(
    season_data_obs,
    season_data_pred
  )

  df_monthly_mean <- df_season |>
    group_by(Shortname, Group, Period, year, month) |>
    summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

  point_monthly_mean <- df_monthly_mean |>
    group_by(Shortname, Group, Period, month) |>
    summarize(avg_value = mean(avg_value, na.rm = TRUE), .groups = "drop")

  peak_shift_table <- point_monthly_mean |>
    mutate(Period_key = case_when(
      Period == "Pre-COVID (Observed)" ~ "pre_obs",
      Period == "Post-PHSM (Observed)" ~ "post_obs",
      Period == "Post-PHSM (Predicted)" ~ "post_pred",
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(Period_key)) |>
    group_by(Shortname, Group, Period_key) |>
    summarize(
      peak_month_max = get_peak_month_max(pick(month, avg_value)),
      peak_month_com = get_peak_month_com(pick(month, avg_value)),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = Period_key, values_from = c(peak_month_max, peak_month_com)) |>
    mutate(
      shift_vs_pre = correct_circular_shift(peak_month_com_post_obs - peak_month_com_pre_obs),
      shift_vs_pred = correct_circular_shift(peak_month_com_post_obs - peak_month_com_post_pred),
      shift_max_vs_pre = correct_circular_shift(peak_month_max_post_obs - peak_month_max_pre_obs),
      shift_max_vs_pred = correct_circular_shift(peak_month_max_post_obs - peak_month_max_post_pred),
      ShiftFlag = (abs(replace_na(shift_vs_pre, 0)) >= 2) | (abs(replace_na(shift_vs_pred, 0)) >= 2),
      MaxShiftFlag = (abs(replace_na(shift_max_vs_pre, 0)) >= 2) | (abs(replace_na(shift_max_vs_pred, 0)) >= 2)
    )

  list(
    phase_summary = peak_shift_table,
    monthly_by_year = df_monthly_mean
  )
}

bootstrap_one_disease <- function(df_one, n_boot = 2000L) {
  sample_profile <- function(df_period) {
    if (nrow(df_period) == 0) {
      return(tibble(month = 1:12, avg_value = NA_real_))
    }
    years <- sort(unique(df_period$year))
    if (length(years) == 0) {
      return(tibble(month = 1:12, avg_value = NA_real_))
    }
    sampled_years <- sample(years, size = length(years), replace = TRUE)
    bind_rows(lapply(sampled_years, function(y) {
      df_period |>
        filter(year == y)
    })) |>
      group_by(month) |>
      summarize(avg_value = mean(avg_value, na.rm = TRUE), .groups = "drop")
  }

  pre_df <- df_one |> filter(Period == "Pre-COVID (Observed)")
  post_obs_df <- df_one |> filter(Period == "Post-PHSM (Observed)")
  post_pred_df <- df_one |> filter(Period == "Post-PHSM (Predicted)")

  point_pre <- pre_df |>
    group_by(month) |>
    summarize(avg_value = mean(avg_value, na.rm = TRUE), .groups = "drop")
  point_post_obs <- post_obs_df |>
    group_by(month) |>
    summarize(avg_value = mean(avg_value, na.rm = TRUE), .groups = "drop")
  point_post_pred <- post_pred_df |>
    group_by(month) |>
    summarize(avg_value = mean(avg_value, na.rm = TRUE), .groups = "drop")

  boot_df <- bind_rows(lapply(seq_len(n_boot), function(i) {
    boot_pre <- sample_profile(pre_df)
    boot_post_obs <- sample_profile(post_obs_df)
    boot_post_pred <- sample_profile(post_pred_df)

    tibble(
      Shift_vs_Pre = correct_circular_shift(get_peak_month_com(boot_post_obs) - get_peak_month_com(boot_pre)),
      Shift_vs_Pred = correct_circular_shift(get_peak_month_com(boot_post_obs) - get_peak_month_com(boot_post_pred))
    )
  }))

  tibble(
    PointShift_vs_Pre = correct_circular_shift(get_peak_month_com(point_post_obs) - get_peak_month_com(point_pre)),
    PointShift_vs_Pred = correct_circular_shift(get_peak_month_com(point_post_obs) - get_peak_month_com(point_post_pred)),
    CI025_vs_Pre = quantile(boot_df$Shift_vs_Pre, 0.025, na.rm = TRUE),
    CI975_vs_Pre = quantile(boot_df$Shift_vs_Pre, 0.975, na.rm = TRUE),
    CI025_vs_Pred = quantile(boot_df$Shift_vs_Pred, 0.025, na.rm = TRUE),
    CI975_vs_Pred = quantile(boot_df$Shift_vs_Pred, 0.975, na.rm = TRUE),
    PrAbsShiftGE2_vs_Pre = mean(abs(boot_df$Shift_vs_Pre) >= 2, na.rm = TRUE),
    PrAbsShiftGE2_vs_Pred = mean(abs(boot_df$Shift_vs_Pred) >= 2, na.rm = TRUE)
  )
}

run_bootstrap_task <- function(df_one, n_boot = 1000L) {
  bootstrap_one_disease(df_one, n_boot = n_boot) |>
    mutate(
      Shortname = unique(df_one$Shortname)[1],
      Group = unique(df_one$Group)[1],
      .before = 1
    )
}

assign_framework_priority <- function(status, shift_vs_pre, shift_vs_pred) {
  shifted <- (abs(replace_na(shift_vs_pre, 0)) >= 2) | (abs(replace_na(shift_vs_pred, 0)) >= 2)

  case_when(
    status == "Recovered" & shifted ~ "Recalibrate and monitor",
    status == "Recovered" ~ "Cumulative review needed",
    status == "Debt Repaid" & shifted ~ "Recovered but recalibrate seasonality",
    status == "Debt Repaid" ~ "Low priority routine review",
    status == "Suppressed" ~ "High priority manual review",
    TRUE ~ "No deficit monitoring"
  )
}

current_phase <- compute_phase_summary(
  data_month |>
    select(Year, Month, Shortname, Cases, Group)
)

weekly_cases <- read_weekly_cases()
date_maps <- build_date_maps()
alt_month_recon <- reconstruct_monthly_day_allocation(weekly_cases, date_maps)

observed_monthly_alt <- data_month |>
  select(Year, Month, Shortname, Cases, Group) |>
  left_join(
    alt_month_recon |>
      rename(Year = year, Month = month, Cases_alt = cases),
    by = c("Year", "Month", "Shortname")
  ) |>
  mutate(
    Cases = if_else(Year >= 2024 & !is.na(Cases_alt), Cases_alt, Cases)
  ) |>
  select(Year, Month, Shortname, Cases, Group)

alt_phase <- compute_phase_summary(observed_monthly_alt)

bootstrap_groups <- split(current_phase$monthly_by_year, current_phase$monthly_by_year$Shortname)
bootstrap_reps <- 1000L
worker_count <- max(1L, min(8L, parallel::detectCores(logical = TRUE) - 1L, length(bootstrap_groups)))

message(sprintf(
  "Running seasonal-shift bootstrap with %d worker(s) across %d diseases.",
  worker_count,
  length(bootstrap_groups)
))

if (worker_count > 1L) {
  cl <- parallel::makeCluster(worker_count)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterSetRNGStream(cl, iseed = 20260408L)
  parallel::clusterExport(
    cl,
    varlist = c(
      "bootstrap_groups", "bootstrap_reps", "run_bootstrap_task", "bootstrap_one_disease",
      "correct_circular_shift", "get_peak_month_com"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
    NULL
  })
  bootstrap_summary <- bind_rows(parallel::parLapply(
    cl,
    bootstrap_groups,
    run_bootstrap_task,
    n_boot = bootstrap_reps
  ))
} else {
  bootstrap_summary <- bind_rows(lapply(
    bootstrap_groups,
    run_bootstrap_task,
    n_boot = bootstrap_reps
  ))
}

bootstrap_summary <- bootstrap_summary |>
  left_join(
    current_phase$phase_summary |>
      select(Shortname, shift_vs_pre, shift_vs_pred, shift_max_vs_pre, shift_max_vs_pred, ShiftFlag, MaxShiftFlag),
    by = "Shortname"
  ) |>
  mutate(
    COM_Max_Agree = ShiftFlag == MaxShiftFlag,
    BorderlineShift = (CI025_vs_Pre <= 2 & CI975_vs_Pre >= 2) |
      (CI025_vs_Pre <= -2 & CI975_vs_Pre >= -2) |
      (CI025_vs_Pred <= 2 & CI975_vs_Pred >= 2) |
      (CI025_vs_Pred <= -2 & CI975_vs_Pred >= -2)
  ) |>
  arrange(desc(BorderlineShift), desc(ShiftFlag), Shortname)

baseline_priority <- read_csv(file.path(tables_dir, "Main_text_summary_table.csv"), show_col_types = FALSE) |>
  transmute(
    Shortname,
    PrimaryPhenotype,
    CurrentPriority = FrameworkPriority,
    Status = case_when(
      PrimaryPhenotype == "Balanced" ~ "Debt Repaid",
      PrimaryPhenotype == "Recovered but not balanced" ~ "Recovered",
      PrimaryPhenotype == "Suppressed" ~ "Suppressed",
      TRUE ~ "No Deficit"
    )
  )

reconstruction_sensitivity <- current_phase$phase_summary |>
  select(Shortname, Group, CurrentShiftPre = shift_vs_pre, CurrentShiftPred = shift_vs_pred, CurrentShiftFlag = ShiftFlag) |>
  left_join(
    alt_phase$phase_summary |>
      select(Shortname, AltShiftPre = shift_vs_pre, AltShiftPred = shift_vs_pred, AltShiftFlag = ShiftFlag),
    by = "Shortname"
  ) |>
  left_join(baseline_priority, by = "Shortname") |>
  mutate(
    AltPriority = assign_framework_priority(Status, AltShiftPre, AltShiftPred),
    QueueChanged = AltPriority != CurrentPriority,
    ShiftFlagChanged = AltShiftFlag != CurrentShiftFlag
  ) |>
  arrange(desc(QueueChanged), desc(ShiftFlagChanged), Shortname)

reconstruction_summary <- reconstruction_sensitivity |>
  summarise(
    DiseasesAssessed = n(),
    ShiftFlagChanged = sum(ShiftFlagChanged, na.rm = TRUE),
    QueueChanged = sum(QueueChanged, na.rm = TRUE)
  )

queue_change_table <- reconstruction_sensitivity |>
  filter(ShiftFlagChanged | QueueChanged) |>
  transmute(
    Shortname,
    `Current COM shift vs pre` = CurrentShiftPre,
    `Alternative COM shift vs pre` = AltShiftPre,
    `Current COM shift vs pred` = CurrentShiftPred,
    `Alternative COM shift vs pred` = AltShiftPred,
    `Current priority` = CurrentPriority,
    `Alternative priority` = AltPriority
  )

bootstrap_csv_path <- file.path(tables_dir, "Seasonal_shift_bootstrap_summary.csv")
recon_csv_path <- file.path(tables_dir, "Seasonal_shift_reconstruction_sensitivity.csv")
xlsx_path <- file.path(tables_dir, "Seasonal_shift_uncertainty.xlsx")

write_csv(bootstrap_summary, bootstrap_csv_path)
write_csv(reconstruction_sensitivity, recon_csv_path)

write.xlsx(
  list(
    BootstrapSummary = bootstrap_summary,
    ReconstructionSummary = reconstruction_summary,
    ReconstructionDiseaseLevel = reconstruction_sensitivity
  ),
  file = xlsx_path,
  overwrite = TRUE
)

summary_block <- c(
  "# Seasonal Shift Uncertainty",
  "",
  "This attachment adds bootstrap uncertainty to the center-of-mass seasonal shift metric and compares the main spline-based weekly-to-monthly reconstruction with a simpler day-allocation baseline for the late follow-up period.",
  "",
  "**Bootstrap summary for center-of-mass shifts**",
  md_table(
    bootstrap_summary |>
      transmute(
        Shortname,
        `Point shift vs pre` = round(PointShift_vs_Pre, 2),
        `95% CI vs pre` = sprintf("%.2f to %.2f", CI025_vs_Pre, CI975_vs_Pre),
        `Pr(|shift|>=2) vs pre` = round(PrAbsShiftGE2_vs_Pre, 3),
        `Point shift vs pred` = round(PointShift_vs_Pred, 2),
        `95% CI vs pred` = sprintf("%.2f to %.2f", CI025_vs_Pred, CI975_vs_Pred),
        `Pr(|shift|>=2) vs pred` = round(PrAbsShiftGE2_vs_Pred, 3),
        `Borderline` = ifelse(BorderlineShift, "Yes", "No"),
        `COM/max agree` = ifelse(COM_Max_Agree, "Yes", "No")
      )
  ),
  "",
  "**Queue changes under alternative weekly-to-monthly reconstruction**",
  if (nrow(queue_change_table) > 0) md_table(queue_change_table) else c(
    "| Result | Value |",
    "| --- | --- |",
    "| Queue changes detected | None |"
  ),
  "",
  "Source files:",
  sprintf("- `%s`", paste0("./Tables/", basename(xlsx_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(bootstrap_csv_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(recon_csv_path)))
)

message("Seasonal uncertainty outputs written:")
message(sprintf(" - %s", xlsx_path))
