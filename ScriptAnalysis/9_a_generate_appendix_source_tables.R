#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(openxlsx)
  library(readr)
  library(parallel)
})

resolve_script_dir <- function() {
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

script_dir <- resolve_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

workbook_path <- file.path(project_root, "Data", "TotalCasesDeaths.xlsx")
clean_data_dir <- file.path(project_root, "Data", "CleanData")
weekly_cases_dir <- file.path(project_root, "Data", "WeeklyCasesData")

exclusion_map <- c(
  "No cases" = "Zero reported incidence over the study period",
  "Duplication" = "Overlapping surveillance categories",
  "Uninfectious disease" = "Not aligned with the transmissible infectious-disease framework",
  "Unspecifed disease" = "Ill-defined or residual surveillance categories",
  "Unreported in 2025" = "Incomplete reporting in the most recent surveillance year",
  "Shifting in surveillance" = "Structural changes in surveillance definitions"
)

exclusion_order <- c(
  "Zero reported incidence over the study period",
  "Overlapping surveillance categories",
  "Not aligned with the transmissible infectious-disease framework",
  "Ill-defined or residual surveillance categories",
  "Incomplete reporting in the most recent surveillance year",
  "Structural changes in surveillance definitions"
)

read_monthly_cases <- function(data_class) {
  list_disease_files <- list.files(clean_data_dir, pattern = "mcd.csv", full.names = TRUE)
  data_all_mcd <- lapply(list_disease_files, read.csv) |>
    bind_rows() |>
    filter(Year >= 2020, Year <= 2023)

  data_all_mcd |>
    filter(Areas == "Total", Month != "Total", Disease %in% data_class$Disease) |>
    left_join(data_class, by = "Disease") |>
    mutate(
      Month = month(parse_date_time(Month, "b"), label = FALSE, abbr = FALSE)
    ) |>
    select(Year, Month, Shortname, Type, Count) |>
    pivot_wider(names_from = Type, values_from = Count, values_fill = 0) |>
    transmute(Shortname, Year, Month, official = Cases)
}

read_weekly_cases <- function(data_map_name) {
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
      error = function(e) {
        warning(sprintf("Failed to read '%s': %s", f, e$message))
        NULL
      }
    )
  })

  data_week_raw <- bind_rows(data_list)
  names(data_week_raw) <- c("week_value", "week_alias", "location_value", "location_alias", "cases", "filename")

  data_week_raw |>
    mutate(
      filepath = as.character(filename),
      filename_only = basename(filepath),
      disease_full = tools::file_path_sans_ext(filename_only),
      year = stringr::str_extract(filepath, "(?<=/|\\\\)\\d{3,4}(?=/|\\\\)"),
      year = if_else(is.na(year), stringr::str_extract(filepath, "\\d{3,4}"), year),
      year = as.integer(year) - 543,
      age_group = if_else(
        stringr::str_detect(disease_full, "__"),
        stringr::str_replace(disease_full, ".*__", ""),
        NA_character_
      ),
      disease = stringr::str_replace(disease_full, "__.*$", "")
    ) |>
    filter(year >= 2020, year <= 2024) |>
    filter(location_value == "%all%", is.na(age_group), week_value != "%all%") |>
    left_join(data_map_name, by = c("disease" = "original_name")) |>
    filter(!is.na(short_name)) |>
    transmute(year, Shortname = short_name, week = as.integer(week_value), cases = as.numeric(cases)) |>
    group_by(year, Shortname, week) |>
    summarize(cases = sum(cases, na.rm = TRUE), .groups = "drop")
}

build_date_maps <- function() {
  data_date_seq <- data.frame(
    date = seq.Date(from = as.Date("2019-12-01"), to = as.Date("2025-01-31"), by = "day")
  ) |>
    mutate(
      year = year(date),
      week = isoweek(date),
      month = month(date),
      year = if_else(month == 12 & week == 1, year + 1L, year),
      year = if_else(month == 1 & week >= 52, year - 1L, year)
    ) |>
    filter(date >= as.Date("2020-01-01"), date < as.Date("2025-01-01"))

  week_date_map <- data_date_seq |>
    group_by(year, week) |>
    summarize(week_start = min(date), week_mid = median(date), dates = list(date), .groups = "drop")

  list(data_date_seq = data_date_seq, week_date_map = week_date_map)
}

reconstruct_one <- function(df_week, week_map, data_date_seq) {
  year_val <- unique(df_week$year)
  if (length(year_val) != 1) {
    stop("multiple years in reconstruct_one")
  }

  wk <- df_week |>
    left_join(week_map, by = c("year", "week"))

  dates_year <- data_date_seq |>
    filter(year %in% year_val) |>
    arrange(date)

  if (nrow(wk) < 2) {
    out <- wk |>
      rowwise() |>
      mutate(dates = list(unlist(dates))) |>
      tidyr::unnest_longer(dates) |>
      rename(date = dates) |>
      mutate(daily = if_else(is.na(cases), 0, as.numeric(cases) / 7)) |>
      ungroup() |>
      transmute(year, Shortname, date, daily)
    return(out)
  }

  x <- as.numeric(as.Date(wk$week_mid))
  y <- as.numeric(wk$cases)

  sp <- stats::splinefun(x, y, method = "natural")

  xout <- as.numeric(dates_year$date)
  pred <- sp(xout)
  pred[pred < 0] <- 0

  daily_tbl <- tibble(date = dates_year$date, predicted = pred) |>
    left_join(data_date_seq |> select(date, year, week), by = "date")

  wk_original <- wk |> select(year, week, cases)

  daily_scaled <- daily_tbl |>
    group_by(year, week) |>
    mutate(sum_pred = sum(predicted, na.rm = TRUE)) |>
    left_join(wk_original, by = c("year", "week")) |>
    mutate(
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      factor = if_else(sum_pred > 0, cases / sum_pred, 0),
      daily = predicted * factor
    ) |>
    ungroup() |>
    select(date, year, week, daily)

  daily_corrected <- daily_scaled |>
    group_by(year, week) |>
    mutate(
      d_floor = floor(daily),
      frac = daily - d_floor
    ) |>
    mutate(
      need = as.integer(round(
        (wk_original$cases[match(paste(year, week), paste(wk_original$year, wk_original$week))]) - sum(d_floor)
      ))
    ) |>
    group_modify(~ {
      df <- .x
      n_need <- unique(df$need)
      if (is.na(n_need)) {
        n_need <- 0
      }

      df$d_final <- df$d_floor
      if (n_need > 0) {
        idx <- order(-df$frac)
        df$d_final[idx[seq_len(min(n_need, nrow(df)))]] <- df$d_final[idx[seq_len(min(n_need, nrow(df)))]] + 1
      } else if (n_need < 0) {
        idx <- order(df$frac)
        take <- seq_len(min(abs(n_need), nrow(df)))
        df$d_final[idx[take]] <- pmax(0, df$d_final[idx[take]] - 1)
      }
      df
    }) |>
    ungroup() |>
    select(date, daily = d_final)

  tibble(
    date = daily_corrected$date,
    year = year_val,
    Shortname = unique(df_week$Shortname),
    daily = as.numeric(daily_corrected$daily)
  ) |>
    filter(date < as.Date("2025-01-01"))
}

reconstruct_all <- function(data_week, week_map, data_date_seq) {
  groups <- data_week |>
    group_by(Shortname, year) |>
    group_split()

  if (length(groups) == 0) {
    return(tibble())
  }

  available_cores <- parallel::detectCores(logical = TRUE)
  n_workers <- max(1L, min(length(groups), max(1L, available_cores - 1L), 8L))

  if (n_workers == 1L) {
    res_list <- lapply(groups, reconstruct_one, week_map = week_map, data_date_seq = data_date_seq)
    return(bind_rows(res_list))
  }

  cl <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  week_map_local <- week_map
  data_date_seq_local <- data_date_seq

  parallel::clusterExport(
    cl,
    varlist = c("reconstruct_one", "week_map_local", "data_date_seq_local"),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
    NULL
  })

  res_list <- parallel::parLapply(cl, groups, function(df) {
    reconstruct_one(df, week_map_local, data_date_seq_local)
  })

  bind_rows(res_list)
}

calculate_overlap_metrics <- function() {
  disease_meta <- read.xlsx(workbook_path, sheet = "TotalCasesDeaths")
  data_class <- disease_meta |>
    filter(Including == 1) |>
    select(Disease, Shortname, Group)

  data_map_name <- read.xlsx(workbook_path, sheet = "DiseaseName") |>
    filter(!is.na(short_name))

  official <- read_monthly_cases(data_class)
  weekly <- read_weekly_cases(data_map_name)
  date_maps <- build_date_maps()

  daily_recon <- reconstruct_all(weekly, date_maps$week_date_map, date_maps$data_date_seq) |>
    mutate(month = month(date), year = year(date))

  month_recon <- daily_recon |>
    group_by(Shortname, year, month) |>
    summarize(recon = sum(daily, na.rm = TRUE), .groups = "drop") |>
    transmute(Shortname, Year = year, Month = month, recon)

  cmp <- inner_join(official, month_recon, by = c("Shortname", "Year", "Month")) |>
    mutate(
      abs_error = abs(recon - official),
      ape = case_when(
        official == 0 & recon == 0 ~ 0,
        official == 0 & recon != 0 ~ NA_real_,
        TRUE ~ abs(recon - official) / official * 100
      )
    ) |>
    arrange(Shortname, Year, Month)

  summary_table <- tibble(
    Metric = c(
      "Overlap years retained in analytical cache",
      "Disease-month observations",
      "Overall Pearson correlation",
      "Mean absolute error, cases",
      "Median absolute error, cases",
      "Mean absolute percentage error, %",
      "Median absolute percentage error, %"
    ),
    Value = c(
      "2020-2023",
      as.character(nrow(cmp)),
      sprintf("%.4f", cor(cmp$official, cmp$recon)),
      sprintf("%.2f", mean(cmp$abs_error, na.rm = TRUE)),
      sprintf("%.2f", median(cmp$abs_error, na.rm = TRUE)),
      sprintf("%.2f", mean(cmp$ape, na.rm = TRUE)),
      sprintf("%.2f", median(cmp$ape, na.rm = TRUE))
    )
  )

  selected_diseases <- c("Pneumonia", "Influenza", "Dengue fever", "HFMD", "HAV", "HCV")
  example_table <- cmp |>
    filter(Shortname %in% selected_diseases) |>
    group_by(Shortname) |>
    summarize(
      `Mean absolute error` = round(mean(abs_error, na.rm = TRUE), 2),
      `Median absolute error` = round(median(abs_error, na.rm = TRUE), 2),
      `Mean absolute percentage error, %` = round(mean(ape, na.rm = TRUE), 2),
      `Median absolute percentage error, %` = round(median(ape, na.rm = TRUE), 2),
      `Pearson correlation` = round(cor(official, recon), 4),
      .groups = "drop"
    ) |>
    mutate(Shortname = factor(Shortname, levels = selected_diseases)) |>
    arrange(Shortname) |>
    rename(Disease = Shortname)

  list(
    comparison = cmp,
    summary = summary_table,
    examples = example_table
  )
}

meta <- read.xlsx(workbook_path, sheet = "TotalCasesDeaths")

table_s2 <- meta |>
  filter(Including == 0) |>
  mutate(`Exclusion category` = unname(exclusion_map[Label])) |>
  transmute(`Raw disease name` = Disease, `Exclusion category`) |>
  mutate(`Exclusion category` = factor(`Exclusion category`, levels = exclusion_order)) |>
  arrange(`Exclusion category`, `Raw disease name`) |>
  mutate(`Exclusion category` = as.character(`Exclusion category`))

if (any(is.na(table_s2$`Exclusion category`))) {
  stop("Unmapped exclusion labels found while preparing Table S2.")
}

table_s4 <- read.xlsx(workbook_path, sheet = "Predictors") |>
  filter(!is.na(Shortname)) |>
  select(
    Shortname,
    Group,
    Incubation_period,
    Infectious_period,
    Vaccine,
    Immune_protection_vaccine,
    Immune_protection_nature
  ) |>
  rename(
    Disease = Shortname,
    Category = Group,
    `Incubation period, days` = Incubation_period,
    `Infectious period` = Infectious_period,
    Vaccine = Vaccine,
    `Vaccine protection` = Immune_protection_vaccine,
    `Natural protection` = Immune_protection_nature
  )

overlap_tables <- calculate_overlap_metrics()

write_csv(table_s2, file.path(tables_dir, "Appendix_S2_excluded_series.csv"))
write_csv(table_s4, file.path(tables_dir, "Appendix_S4_predictor_definitions.csv"))
write_csv(overlap_tables$summary, file.path(tables_dir, "Appendix_S6_overlap_summary.csv"))
write_csv(overlap_tables$examples, file.path(tables_dir, "Appendix_S7_overlap_examples.csv"))
write_csv(overlap_tables$comparison, file.path(tables_dir, "Appendix_overlap_monthly_validation.csv"))

cat("Wrote appendix source tables:\n")
cat(" - Appendix_S2_excluded_series.csv\n")
cat(" - Appendix_S4_predictor_definitions.csv\n")
cat(" - Appendix_S6_overlap_summary.csv\n")
cat(" - Appendix_S7_overlap_examples.csv\n")
cat(" - Appendix_overlap_monthly_validation.csv\n")
