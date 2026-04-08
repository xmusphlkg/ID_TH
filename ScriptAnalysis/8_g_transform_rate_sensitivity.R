#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(openxlsx)
  library(purrr)
  library(parallel)
  library(forecast)
  library(forecastHybrid)
  library(bsts)
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
source("./function/forecast.R")
source("./function/revision_utils.R")

load(file.path(script_dir, "temp", "month.RData"))
load(file.path(script_dir, "temp", "outcome.RData"))

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
summary_md_path <- file.path(project_root, "Outcome", "Appendix", "Transform_rate_sensitivity.md")
main_appendix_path <- file.path(project_root, "Outcome", "Appendix", "Supplementary_Appendix.md")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

best_models <- read.xlsx(file.path(tables_dir, "Best_model_outcome.xlsx")) |>
  filter(Best == 1) |>
  transmute(
    Shortname = disease,
    Method = if_else(Method == "Hybrid**", "Hybrid", Method)
  )

build_uncertainty_summary <- function(dates, observed, median_expected, sim_matrix) {
  primary <- calc_status_one(
    dates = dates,
    observed = observed,
    expected = median_expected,
    start_date = split_dates[1]
  )

  sim_rows <- lapply(seq_len(ncol(sim_matrix)), function(j) {
    calc_status_one(
      dates = dates,
      observed = observed,
      expected = sim_matrix[, j],
      start_date = split_dates[1]
    )
  }) |>
    bind_rows()

  rp_months <- map_dbl(
    sim_rows$Date_Recovery,
    ~ if (is.na(.x)) NA_real_ else get_months(split_dates[1], .x)
  )
  bp_months <- map_dbl(
    sim_rows$Date_Balance,
    ~ if (is.na(.x)) NA_real_ else get_months(split_dates[1], .x)
  )

  status_prob <- sim_rows |>
    count(Status, name = "n") |>
    mutate(prob = n / sum(n))

  primary_prob <- status_prob |>
    filter(Status == primary$Status[[1]]) |>
    pull(prob)
  if (length(primary_prob) == 0) {
    primary_prob <- 0
  }

  tibble(
    PrimaryStatus = primary$Status[[1]],
    PrimaryPhenotype = unname(status_label_map[primary$Status[[1]]]),
    RP_Month = ifelse(is.na(primary$Date_Recovery[[1]]), NA_real_, get_months(split_dates[1], primary$Date_Recovery[[1]])),
    BP_Month = ifelse(is.na(primary$Date_Balance[[1]]), NA_real_, get_months(split_dates[1], primary$Date_Balance[[1]])),
    Pr_RP = mean(sim_rows$Status %in% c("Recovered", "Debt Repaid")),
    Pr_BP = mean(sim_rows$Status == "Debt Repaid"),
    PrimaryStatusProb = primary_prob,
    RP_Q025 = ifelse(any(!is.na(rp_months)), quantile(rp_months, 0.025, na.rm = TRUE), NA_real_),
    RP_Q975 = ifelse(any(!is.na(rp_months)), quantile(rp_months, 0.975, na.rm = TRUE), NA_real_),
    BP_Q025 = ifelse(any(!is.na(bp_months)), quantile(bp_months, 0.025, na.rm = TRUE), NA_real_),
    BP_Q975 = ifelse(any(!is.na(bp_months)), quantile(bp_months, 0.975, na.rm = TRUE), NA_real_)
  )
}

build_deterministic_summary <- function(dates, observed, expected) {
  primary <- calc_status_one(
    dates = dates,
    observed = observed,
    expected = expected,
    start_date = split_dates[1]
  )

  tibble(
    PrimaryStatus = primary$Status[[1]],
    PrimaryPhenotype = unname(status_label_map[primary$Status[[1]]]),
    RP_Month = ifelse(is.na(primary$Date_Recovery[[1]]), NA_real_, get_months(split_dates[1], primary$Date_Recovery[[1]])),
    BP_Month = ifelse(is.na(primary$Date_Balance[[1]]), NA_real_, get_months(split_dates[1], primary$Date_Balance[[1]])),
    Pr_RP = NA_real_,
    Pr_BP = NA_real_,
    PrimaryStatusProb = NA_real_,
    RP_Q025 = NA_real_,
    RP_Q975 = NA_real_,
    BP_Q025 = NA_real_,
    BP_Q975 = NA_real_
  )
}

baseline_summary <- bind_rows(lapply(seq_along(outcome), function(i) {
  item <- outcome[[i]]
  od <- item$outcome_data |>
    arrange(date)

  build_uncertainty_summary(
    dates = od$date,
    observed = od$value,
    median_expected = od$median,
    sim_matrix = item$MCMC
  ) |>
    mutate(
      Shortname = unique(od$Shortname)[1],
      Method = best_models$Method[match(unique(od$Shortname)[1], best_models$Shortname)],
      Config = "Count sqrt",
      SeriesType = "count",
      Transform = "sqrt"
    )
}))

uncertainty_sensitive <- baseline_summary |>
  filter(PrimaryStatusProb < 0.80) |>
  pull(Shortname)

run_config <- function(shortname, method, config, series_col, transform_method, seed, include_sim = FALSE) {
  data_single <- data_month |>
    filter(Shortname == shortname) |>
    transmute(date = Date, value = .data[[series_col]]) |>
    arrange(date)

  observed_post <- data_single |>
    filter(date >= split_dates[1])

  ts_train <- data_single |>
    filter(date < split_dates[1]) |>
    pull(value) |>
    ts(
      frequency = 12,
      start = c(
        as.numeric(format(min(data_single$date), "%Y")),
        as.numeric(format(min(data_single$date), "%m"))
      )
    )

  transform_lambda <- estimate_transform_lambda(ts_train, method = transform_method, offset = add_value)
  ts_train <- positive_forward_transform(
    ts_train,
    method = transform_method,
    offset = add_value,
    lambda = transform_lambda
  )

  method_used <- method
  deterministic <- tryCatch(
    forecast_model_ts(
      ts_train = ts_train,
      h = nrow(observed_post),
      method = method_used,
      hybrid_parallel = FALSE,
      hybrid_cores = 1,
      bsts_niter = 250L,
      seed = seed,
      transform_method = transform_method,
      transform_lambda = transform_lambda
    ),
    error = function(e) NULL
  )

  if (is.null(deterministic) && identical(method, "ARIMA + Fourier")) {
    method_used <- "SARIMA"
    deterministic <- tryCatch(
      forecast_model_ts(
        ts_train = ts_train,
        h = nrow(observed_post),
        method = method_used,
        hybrid_parallel = FALSE,
        hybrid_cores = 1,
        bsts_niter = 250L,
        seed = seed,
        transform_method = transform_method,
        transform_lambda = transform_lambda
      ),
      error = function(e) NULL
    )
  }

  if (is.null(deterministic)) {
    return(tibble(
      Shortname = shortname,
      Method = method,
      Config = config,
      SeriesType = ifelse(series_col == "Incidence", "rate", "count"),
      Transform = transform_method,
      PrimaryStatus = NA_character_,
      PrimaryPhenotype = NA_character_,
      RP_Month = NA_real_,
      BP_Month = NA_real_,
      Pr_RP = NA_real_,
      Pr_BP = NA_real_,
      PrimaryStatusProb = NA_real_,
      RP_Q025 = NA_real_,
      RP_Q975 = NA_real_,
      BP_Q025 = NA_real_,
      BP_Q975 = NA_real_,
      MethodUsed = NA_character_,
      FitFailed = TRUE,
      SimulationIncluded = include_sim
    ))
  }

  summary_row <- build_deterministic_summary(
    dates = observed_post$date,
    observed = observed_post$value,
    expected = deterministic$mean
  )

  if (include_sim) {
    sim_res <- tryCatch(
      forecast_model_sim(
        ts_train = ts_train,
        h = nrow(observed_post),
        method = method_used,
        hybrid_parallel = FALSE,
        hybrid_cores = 1,
        bsts_niter = 200L,
        n_paths = 200L,
        seed = seed,
        transform_method = transform_method,
        transform_lambda = transform_lambda
      ),
      error = function(e) NULL
    )

    if (!is.null(sim_res)) {
      summary_row <- build_uncertainty_summary(
        dates = observed_post$date,
        observed = observed_post$value,
        median_expected = sim_res$median,
        sim_matrix = sim_res$MCMC
      )
    }
  }

  summary_row |>
    mutate(
      Shortname = shortname,
      Method = method,
      Config = config,
      SeriesType = ifelse(series_col == "Incidence", "rate", "count"),
      Transform = transform_method,
      MethodUsed = method_used,
      FitFailed = FALSE,
      SimulationIncluded = include_sim
    )
}

sensitivity_specs <- tibble(
  Config = c("Count log", "Rate sqrt"),
  SeriesCol = c("Cases", "Incidence"),
  Transform = c("log", "sqrt")
)

task_grid <- tidyr::expand_grid(
  disease_idx = seq_len(nrow(best_models)),
  spec_idx = seq_len(nrow(sensitivity_specs))
)

run_task <- function(task_row) {
  i <- task_row$disease_idx[[1]]
  j <- task_row$spec_idx[[1]]
  shortname <- best_models$Shortname[[i]]
  method <- best_models$Method[[i]]

  run_config(
    shortname = shortname,
    method = method,
    config = sensitivity_specs$Config[[j]],
    series_col = sensitivity_specs$SeriesCol[[j]],
    transform_method = sensitivity_specs$Transform[[j]],
    seed = 20260408L + i * 10L + j,
    include_sim = shortname %in% uncertainty_sensitive
  )
}

worker_count <- max(1L, min(8L, parallel::detectCores(logical = TRUE) - 1L))

if (worker_count > 1L) {
  cl <- parallel::makeCluster(worker_count)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterSetRNGStream(cl, iseed = 20260408L)
  parallel::clusterExport(
    cl,
    varlist = c(
      "task_grid", "best_models", "sensitivity_specs", "uncertainty_sensitive",
      "data_month", "split_dates", "add_value", "forecast_transform",
      "estimate_transform_lambda", "positive_forward_transform", "forecast_model_ts",
      "forecast_model_sim", "positive_inverse_transform", "fit_fourier_arima",
      "build_deterministic_summary", "build_uncertainty_summary",
      "calc_status_one", "find_sustained_date", "status_label_map", "get_months", "run_config"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(purrr)
    library(forecast)
    library(forecastHybrid)
    library(bsts)
    NULL
  })
  sensitivity_results <- bind_rows(parallel::parLapply(cl, split(task_grid, seq_len(nrow(task_grid))), run_task))
} else {
  sensitivity_results <- bind_rows(lapply(split(task_grid, seq_len(nrow(task_grid))), run_task))
}

all_results <- bind_rows(
  baseline_summary,
  sensitivity_results
) |>
  select(
    Shortname, Method, Config, SeriesType, Transform,
    PrimaryStatus, PrimaryPhenotype, RP_Month, BP_Month,
    Pr_RP, Pr_BP, PrimaryStatusProb,
    RP_Q025, RP_Q975, BP_Q025, BP_Q975,
    MethodUsed, FitFailed, SimulationIncluded
  ) |>
  arrange(Shortname, factor(Config, levels = c("Count sqrt", "Count log", "Rate sqrt")))

baseline_ref <- all_results |>
  filter(Config == "Count sqrt") |>
  select(
    Shortname,
    BaselineStatus = PrimaryStatus,
    BaselinePhenotype = PrimaryPhenotype,
    Baseline_RP_Month = RP_Month,
    Baseline_BP_Month = BP_Month,
    Baseline_Pr_RP = Pr_RP,
    Baseline_Pr_BP = Pr_BP,
    BaselineStatusProb = PrimaryStatusProb
  )

comparison_table <- all_results |>
  filter(Config != "Count sqrt") |>
  left_join(baseline_ref, by = "Shortname") |>
  mutate(
    PhenotypeChanged = PrimaryPhenotype != BaselinePhenotype,
    StatusChanged = PrimaryStatus != BaselineStatus,
    RP_Month_Delta = RP_Month - Baseline_RP_Month,
    BP_Month_Delta = BP_Month - Baseline_BP_Month,
    Abs_RP_Month_Delta = abs(RP_Month_Delta),
    Abs_BP_Month_Delta = abs(BP_Month_Delta),
    Delta_Pr_RP = Pr_RP - Baseline_Pr_RP,
    Delta_Pr_BP = Pr_BP - Baseline_Pr_BP,
    Delta_StatusProb = PrimaryStatusProb - BaselineStatusProb,
    MaterialTimingShift = coalesce(Abs_RP_Month_Delta >= 6, FALSE) | coalesce(Abs_BP_Month_Delta >= 6, FALSE)
  ) |>
  arrange(desc(PhenotypeChanged), desc(MaterialTimingShift), Config, Shortname)

summary_table <- comparison_table |>
  group_by(Config) |>
  summarise(
    DiseasesAssessed = n(),
    PhenotypeChanged = sum(PhenotypeChanged, na.rm = TRUE),
    StatusChanged = sum(StatusChanged, na.rm = TRUE),
    MaterialTimingShift = sum(MaterialTimingShift, na.rm = TRUE),
    MedianAbsRPShift = median(Abs_RP_Month_Delta, na.rm = TRUE),
    MedianAbsBPShift = median(Abs_BP_Month_Delta, na.rm = TRUE),
    MaxAbsRPShift = ifelse(all(is.na(Abs_RP_Month_Delta)), NA_real_, max(Abs_RP_Month_Delta, na.rm = TRUE)),
    MaxAbsBPShift = ifelse(all(is.na(Abs_BP_Month_Delta)), NA_real_, max(Abs_BP_Month_Delta, na.rm = TRUE)),
    MeanDeltaPrRP = mean(Delta_Pr_RP, na.rm = TRUE),
    MeanDeltaPrBP = mean(Delta_Pr_BP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

exceptions_table <- comparison_table |>
  filter(PhenotypeChanged | MaterialTimingShift) |>
  transmute(
    Shortname,
    Config,
    `Baseline phenotype` = BaselinePhenotype,
    `Sensitivity phenotype` = PrimaryPhenotype,
    `RP month delta` = RP_Month_Delta,
    `BP month delta` = BP_Month_Delta,
    `Delta Pr(RP)` = round(Delta_Pr_RP, 3),
    `Delta Pr(BP)` = round(Delta_Pr_BP, 3),
    `Delta primary-status probability` = round(Delta_StatusProb, 3)
  )

summary_csv_path <- file.path(tables_dir, "Transform_rate_sensitivity_summary.csv")
comparison_csv_path <- file.path(tables_dir, "Transform_rate_sensitivity_comparison.csv")
all_csv_path <- file.path(tables_dir, "Transform_rate_sensitivity_all_results.csv")
xlsx_path <- file.path(tables_dir, "Transform_rate_sensitivity.xlsx")

write_csv(summary_table, summary_csv_path)
write_csv(comparison_table, comparison_csv_path)
write_csv(all_results, all_csv_path)

write.xlsx(
  list(
    Summary = summary_table,
    Exceptions = exceptions_table,
    Comparison = comparison_table,
    AllResults = all_results
  ),
  file = xlsx_path,
  overwrite = TRUE
)

summary_block <- c(
  "# Transform and Rate Sensitivity",
  "",
  "This attachment evaluates whether the RP/BP recovery classifications are sensitive to the positive-support transform or to replacing monthly counts with all-age incidence rates.",
  "",
  "**Summary of sensitivity scenarios**",
  md_table(summary_table),
  "",
  if (nrow(exceptions_table) > 0) "**Diseases with phenotype changes or >=6-month timing shifts**" else "**Diseases with phenotype changes or >=6-month timing shifts**",
  if (nrow(exceptions_table) > 0) md_table(exceptions_table) else "| Result | Value |\n| --- | --- |\n| Exceptions detected | None |",
  "",
  "Source files:",
  sprintf("- `%s`", paste0("./Tables/", basename(xlsx_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(summary_csv_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(comparison_csv_path)))
)

writeLines(summary_block, summary_md_path)

if (file.exists(main_appendix_path)) {
  appendix_lines <- readLines(main_appendix_path, warn = FALSE)
  appendix_block <- c(
    "## Part 9: Transform and denominator sensitivity",
    "",
    "This supplementary analysis holds the selected model family fixed for each disease and re-runs the recovery workflow under the primary square-root count specification, a log-transformed count specification, and a square-root incidence-rate specification using the linked annual population denominators.",
    "",
    "**Table S20. Portfolio-level transform and denominator sensitivity summary.**",
    md_table(summary_table),
    "",
    "**Table S21. Diseases with phenotype changes or material timing shifts in transform and denominator sensitivity analyses.**",
    if (nrow(exceptions_table) > 0) md_table(exceptions_table) else c(
      "| Result | Value |",
      "| --- | --- |",
      "| Exceptions detected | None |"
    ),
    "",
    "The disease-level outputs for all 24 modelled diseases are provided in `Tables/Transform_rate_sensitivity.xlsx`."
  )
  appendix_lines <- replace_or_append_block(appendix_lines, "TRANSFORM_RATE_SENSITIVITY", appendix_block)
  writeLines(appendix_lines, main_appendix_path)
}

message("Transform/rate sensitivity outputs written:")
message(sprintf(" - %s", xlsx_path))
