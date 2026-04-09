#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(openxlsx)
  library(ggplot2)
  library(cowplot)
  library(scales)
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

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
figure_dir <- file.path(project_root, "Outcome", "Appendix", "Supplementary Appendix 1_8")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

best_models <- read.xlsx(file.path(tables_dir, "Best_model_outcome.xlsx")) |>
  filter(Best == 1) |>
  transmute(
    Shortname = disease,
    BestModel = if_else(Method == "Hybrid**", "Hybrid", Method)
  )

split_specs <- tibble(
  split = c("2019", "2018-2019", "2017-2019"),
  start_date = as.Date(c("2019-01-01", "2018-01-01", "2017-01-01")),
  end_date = as.Date(c("2019-12-01", "2019-12-01", "2019-12-01")),
  split_label = c("Pseudo interruption 2019", "Pseudo interruption 2018", "Pseudo interruption 2017")
)

status_palette <- c(
  "No deficit" = "#BDBDBD",
  "Recovered but not balanced" = "#E64B35FF",
  "Balanced" = "#0B6E69",
  "Suppressed" = "#CC3D24"
)

interval_score <- function(lower, upper, observed, alpha) {
  width <- upper - lower
  width +
    (2 / alpha) * (lower - observed) * (observed < lower) +
    (2 / alpha) * (observed - upper) * (observed > upper)
}

weighted_interval_score <- function(observed, median, lower80, upper80, lower95, upper95) {
  is80 <- interval_score(lower80, upper80, observed, alpha = 0.20)
  is95 <- interval_score(lower95, upper95, observed, alpha = 0.05)
  (0.5 * abs(observed - median) + 0.1 * is80 + 0.025 * is95) / 0.625
}

calc_tempered_status <- function(month_df, primary_status, persistence = 3L, min_hits = 2L) {
  if (primary_status == "No Deficit") {
    return(primary_status)
  }

  outside_lower80 <- month_df$actual < month_df$lower_80
  outside_lower80[is.na(outside_lower80)] <- FALSE

  if (length(outside_lower80) < persistence) {
    return("No Deficit")
  }

  for (i in seq_len(length(outside_lower80) - persistence + 1L)) {
    if (sum(outside_lower80[i:(i + persistence - 1L)]) >= min_hits) {
      return(primary_status)
    }
  }

  "No Deficit"
}

run_placebo_split <- function(shortname, best_model, spec, n_paths = 1000L) {
  data_single <- data_month |>
    filter(Shortname == shortname) |>
    transmute(date = Date, actual = Cases, Group) |>
    arrange(date)

  ts_train <- data_single |>
    filter(date < spec$start_date) |>
    pull(actual) |>
    ts(
      frequency = 12,
      start = c(
        as.numeric(format(min(data_single$date), "%Y")),
        as.numeric(format(min(data_single$date), "%m"))
      )
    )

  transform_lambda <- estimate_transform_lambda(ts_train, method = forecast_transform, offset = add_value)
  ts_train <- positive_forward_transform(
    ts_train,
    method = forecast_transform,
    offset = add_value,
    lambda = transform_lambda
  )

  test_df <- data_single |>
    filter(date >= spec$start_date, date <= spec$end_date) |>
    arrange(date)

  res <- forecast_model_sim(
    ts_train = ts_train,
    h = nrow(test_df),
    method = best_model,
    hybrid_parallel = FALSE,
    hybrid_cores = 1,
    bsts_niter = 1000L,
    n_paths = n_paths,
    seed = 20260408L + as.integer(factor(shortname, levels = best_models$Shortname)) +
      match(spec$split, split_specs$split) * 100L,
    transform_method = forecast_transform,
    transform_lambda = transform_lambda
  )

  month_df <- test_df |>
    mutate(
      split = spec$split,
      split_label = spec$split_label,
      start_date = spec$start_date,
      end_date = spec$end_date,
      horizon = row_number(),
      expected_mean = res$mean,
      expected_median = res$median,
      lower_80 = res$lower_80,
      lower_95 = res$lower_95,
      upper_80 = res$upper_80,
      upper_95 = res$upper_95
    ) |>
    mutate(
      Coverage80 = actual >= lower_80 & actual <= upper_80,
      Coverage95 = actual >= lower_95 & actual <= upper_95,
      IntervalScore80 = interval_score(lower_80, upper_80, actual, alpha = 0.20),
      IntervalScore95 = interval_score(lower_95, upper_95, actual, alpha = 0.05),
      WIS = weighted_interval_score(actual, expected_median, lower_80, upper_80, lower_95, upper_95)
    )

  pit_vals <- vapply(seq_len(nrow(month_df)), function(i) {
    sims <- res$MCMC[i, ]
    sims <- sims[is.finite(sims)]
    if (length(sims) == 0) {
      return(NA_real_)
    }
    lower_mass <- mean(sims < month_df$actual[i])
    equal_mass <- mean(sims == month_df$actual[i])
    lower_mass + runif(1) * equal_mass
  }, numeric(1))

  month_df$PIT <- pit_vals

  primary <- calc_status_one(
    dates = month_df$date,
    observed = month_df$actual,
    expected = month_df$expected_median,
    start_date = spec$start_date
  )
  primary_status <- primary$Status[[1]]
  tempered_status <- calc_tempered_status(month_df, primary_status)

  split_row <- tibble(
    Shortname = shortname,
    Group = unique(data_single$Group)[1],
    BestModel = best_model,
    split = spec$split,
    split_label = spec$split_label,
    start_date = spec$start_date,
    end_date = spec$end_date,
    PlaceboStatus = primary_status,
    PlaceboStatusLabel = unname(status_label_map[primary_status]),
    TemperedStatus = tempered_status,
    TemperedStatusLabel = unname(status_label_map[tempered_status]),
    FalseAlert = primary_status != "No Deficit",
    FalseAlertTempered = tempered_status != "No Deficit",
    FalseSuppressed = primary_status == "Suppressed",
    Coverage80 = mean(month_df$Coverage80, na.rm = TRUE),
    Coverage95 = mean(month_df$Coverage95, na.rm = TRUE),
    MeanIntervalScore80 = mean(month_df$IntervalScore80, na.rm = TRUE),
    MeanIntervalScore95 = mean(month_df$IntervalScore95, na.rm = TRUE),
    MeanWIS = mean(month_df$WIS, na.rm = TRUE),
    MeanPIT = mean(month_df$PIT, na.rm = TRUE),
    PIT_KS_p = suppressWarnings(tryCatch(stats::ks.test(month_df$PIT[is.finite(month_df$PIT)], "punif")$p.value, error = function(e) NA_real_)),
    MeanWidth95 = mean(month_df$upper_95 - month_df$lower_95, na.rm = TRUE),
    MonthsEvaluated = nrow(month_df)
  )

  list(
    split_summary = split_row,
    month_level = month_df |>
      mutate(
        Shortname = shortname,
        BestModel = best_model
      )
  )
}

task_grid <- tidyr::expand_grid(
  disease_idx = seq_len(nrow(best_models)),
  split_idx = seq_len(nrow(split_specs))
)

run_placebo_task <- function(task_row) {
  i <- task_row$disease_idx[[1]]
  j <- task_row$split_idx[[1]]

  run_placebo_split(
    shortname = best_models$Shortname[[i]],
    best_model = best_models$BestModel[[i]],
    spec = split_specs[j, ],
    n_paths = 1000L
  )
}

worker_count <- max(1L, min(4L, parallel::detectCores(logical = TRUE) - 1L, nrow(task_grid)))

message(sprintf(
  "Running placebo falsification with %d worker(s) across %d disease-window tasks.",
  worker_count,
  nrow(task_grid)
))

if (worker_count > 1L) {
  cl <- parallel::makeCluster(worker_count)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterSetRNGStream(cl, iseed = 20260408L)
  parallel::clusterExport(
    cl,
    varlist = c(
      "task_grid", "best_models", "split_specs", "data_month", "add_value", "forecast_transform",
      "estimate_transform_lambda", "positive_forward_transform", "positive_inverse_transform",
      "fit_fourier_arima", "forecast_model_sim", "interval_score", "weighted_interval_score",
      "calc_tempered_status", "calc_status_one", "find_sustained_date", "status_label_map", "get_months",
      "run_placebo_split", "run_placebo_task"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
    library(forecast)
    library(forecastHybrid)
    library(bsts)
    NULL
  })
  placebo_runs <- parallel::parLapply(
    cl,
    split(task_grid, seq_len(nrow(task_grid))),
    run_placebo_task
  )
} else {
  placebo_runs <- lapply(
    split(task_grid, seq_len(nrow(task_grid))),
    run_placebo_task
  )
}

placebo_split_level <- bind_rows(lapply(placebo_runs, `[[`, "split_summary"))
placebo_month_level <- bind_rows(lapply(placebo_runs, `[[`, "month_level"))

placebo_disease_summary <- placebo_split_level |>
  group_by(Shortname, Group, BestModel) |>
  summarise(
    SplitsAssessed = n(),
    FalseAlerts = sum(FalseAlert, na.rm = TRUE),
    FalseAlertsTempered = sum(FalseAlertTempered, na.rm = TRUE),
    FalseSuppressed = sum(FalseSuppressed, na.rm = TRUE),
    MeanCoverage80 = mean(Coverage80, na.rm = TRUE),
    MeanCoverage95 = mean(Coverage95, na.rm = TRUE),
    MeanIntervalScore95 = mean(MeanIntervalScore95, na.rm = TRUE),
    MeanWIS = mean(MeanWIS, na.rm = TRUE),
    MeanPIT = mean(MeanPIT, na.rm = TRUE),
    MeanWidth95 = mean(MeanWidth95, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(FalseAlerts), Shortname)

placebo_group_summary <- placebo_split_level |>
  group_by(split, split_label, Group) |>
  summarise(
    DiseasesAssessed = n(),
    FalseAlertRate = mean(FalseAlert, na.rm = TRUE),
    FalseAlertRateTempered = mean(FalseAlertTempered, na.rm = TRUE),
    MeanCoverage95 = mean(Coverage95, na.rm = TRUE),
    MeanWIS = mean(MeanWIS, na.rm = TRUE),
    .groups = "drop"
  )

placebo_portfolio_summary <- placebo_split_level |>
  group_by(split, split_label, start_date, end_date) |>
  summarise(
    DiseasesAssessed = n(),
    FalseAlerts = sum(FalseAlert, na.rm = TRUE),
    FalseAlertRate = mean(FalseAlert, na.rm = TRUE),
    FalseAlertsTempered = sum(FalseAlertTempered, na.rm = TRUE),
    FalseAlertRateTempered = mean(FalseAlertTempered, na.rm = TRUE),
    FalseSuppressed = sum(FalseSuppressed, na.rm = TRUE),
    MeanCoverage80 = mean(Coverage80, na.rm = TRUE),
    MeanCoverage95 = mean(Coverage95, na.rm = TRUE),
    MeanIntervalScore95 = mean(MeanIntervalScore95, na.rm = TRUE),
    MeanWIS = mean(MeanWIS, na.rm = TRUE),
    MeanPIT = mean(MeanPIT, na.rm = TRUE),
    MeanWidth95 = mean(MeanWidth95, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(match(split, split_specs$split)) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

horizon_summary <- placebo_month_level |>
  mutate(
    HorizonBin = case_when(
      horizon <= 12 ~ "1-12",
      horizon <= 24 ~ "13-24",
      TRUE ~ "25-36"
    )
  ) |>
  group_by(HorizonBin) |>
  summarise(
    MeanCoverage80 = mean(Coverage80, na.rm = TRUE),
    MeanCoverage95 = mean(Coverage95, na.rm = TRUE),
    MeanIntervalScore95 = mean(IntervalScore95, na.rm = TRUE),
    MeanWIS = mean(WIS, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

table_s19_md <- placebo_portfolio_summary |>
  transmute(
    `Pseudo interruption` = split_label,
    `Diseases assessed` = DiseasesAssessed,
    `False alerts` = FalseAlerts,
    `False-alert rate` = FalseAlertRate,
    `Tempered false alerts` = FalseAlertsTempered,
    `Tempered false-alert rate` = FalseAlertRateTempered,
    `Mean 80% coverage` = MeanCoverage80,
    `Mean 95% coverage` = MeanCoverage95,
    `Mean 95% interval score` = MeanIntervalScore95,
    `Mean WIS` = MeanWIS
  )

heatmap_order <- placebo_disease_summary |>
  arrange(desc(FalseAlerts), desc(FalseSuppressed), Shortname) |>
  pull(Shortname)

heatmap_df <- placebo_split_level |>
  mutate(
    Shortname = factor(Shortname, levels = rev(heatmap_order)),
    split_label = factor(split_label, levels = split_specs$split_label)
  )

panel_a <- ggplot(heatmap_df, aes(x = split_label, y = Shortname, fill = PlaceboStatusLabel)) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_manual(values = status_palette, drop = FALSE) +
  theme_plot() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(5, 10, 5, 5)
  ) +
  labs(title = "A")

coverage_plot_df <- horizon_summary |>
  pivot_longer(
    cols = c(MeanCoverage80, MeanCoverage95),
    names_to = "Interval",
    values_to = "EmpiricalCoverage"
  ) |>
  mutate(
    Interval = if_else(Interval == "MeanCoverage80", "80% interval", "95% interval"),
    NominalCoverage = if_else(Interval == "80% interval", 0.80, 0.95)
  )

panel_b <- ggplot(coverage_plot_df, aes(x = HorizonBin, y = EmpiricalCoverage, color = Interval, group = Interval)) +
  geom_hline(aes(yintercept = NominalCoverage, color = Interval), linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  scale_color_manual(values = c("80% interval" = "#3C5488FF", "95% interval" = "#E64B35FF")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.7, 1.0)) +
  theme_plot() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(y = "Empirical coverage", title = "B")

pit_hist_df <- placebo_month_level |>
  filter(is.finite(PIT))

panel_c <- ggplot(pit_hist_df, aes(x = PIT)) +
  geom_histogram(binwidth = 0.1, boundary = 0, fill = "#4DBBD5FF", color = "white") +
  geom_hline(yintercept = nrow(pit_hist_df) / 10, linetype = "dashed", color = "#666666", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_plot() +
  theme(
    axis.title.x = element_blank()
  ) +
  labs(y = "Count", title = "C")

false_alert_plot_df <- placebo_portfolio_summary |>
  select(split_label, FalseAlertRate, FalseAlertRateTempered) |>
  pivot_longer(
    cols = c(FalseAlertRate, FalseAlertRateTempered),
    names_to = "Rule",
    values_to = "FalseAlertRate"
  ) |>
  mutate(
    Rule = recode(
      Rule,
      FalseAlertRate = "Deterministic median rule",
      FalseAlertRateTempered = "Tempered rule with 80% PI corroboration"
    )
  )

panel_d <- ggplot(false_alert_plot_df, aes(x = split_label, y = FalseAlertRate, fill = Rule)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = c(
    "Deterministic median rule" = "#E64B35FF",
    "Tempered rule with 80% PI corroboration" = "#3C5488FF"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.85)) +
  theme_plot() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(y = "False-alert rate", title = "D")

combined_figure <- cowplot::plot_grid(
  panel_a,
  panel_b,
  panel_c,
  panel_d,
  ncol = 2,
  align = "hv"
)

figure_png_path <- file.path(figure_dir, "falsification_and_calibration.png")
ggsave(
  filename = figure_png_path,
  plot = combined_figure,
  width = 15,
  height = 12,
  dpi = 320,
  bg = "white"
)

split_csv_path <- file.path(tables_dir, "Placebo_interruption_split_level.csv")
disease_csv_path <- file.path(tables_dir, "Placebo_interruption_disease_summary.csv")
summary_csv_path <- file.path(tables_dir, "Placebo_interruption_portfolio_summary.csv")
month_csv_path <- file.path(tables_dir, "Placebo_interruption_month_level.csv")
horizon_csv_path <- file.path(tables_dir, "Calibration_horizon_summary.csv")
group_csv_path <- file.path(tables_dir, "Placebo_interruption_group_summary.csv")
summary_xlsx_path <- file.path(tables_dir, "Falsification_and_calibration.xlsx")

write_csv(placebo_split_level, split_csv_path)
write_csv(placebo_disease_summary, disease_csv_path)
write_csv(placebo_portfolio_summary, summary_csv_path)
write_csv(placebo_month_level, month_csv_path)
write_csv(horizon_summary, horizon_csv_path)
write_csv(placebo_group_summary, group_csv_path)

write.xlsx(
  list(
    PortfolioSummary = placebo_portfolio_summary,
    GroupSummary = placebo_group_summary,
    DiseaseSummary = placebo_disease_summary,
    SplitLevel = placebo_split_level,
    HorizonSummary = horizon_summary,
    MonthLevel = placebo_month_level,
    SplitSpecs = split_specs
  ),
  file = summary_xlsx_path,
  overwrite = TRUE
)

lowest_false_alert <- placebo_portfolio_summary |>
  arrange(FalseAlertsTempered, FalseAlerts) |>
  slice(1)

summary_lines <- c(
  "# Placebo Interruption and Calibration",
  "",
  "This attachment evaluates whether the best-model forecasting pipeline produces spurious operational alerts when the RP/BP workflow is applied to pre-pandemic placebo interruption dates, and whether the predictive distributions remain calibrated under the same placebo windows.",
  sprintf(
    "Across the three placebo windows, the deterministic median rule produced %d to %d false alerts (rate %.1f%% to %.1f%%), whereas the exploratory tempered rule produced %d to %d false alerts.",
    min(placebo_portfolio_summary$FalseAlerts),
    max(placebo_portfolio_summary$FalseAlerts),
    min(placebo_portfolio_summary$FalseAlertRate) * 100,
    max(placebo_portfolio_summary$FalseAlertRate) * 100,
    min(placebo_portfolio_summary$FalseAlertsTempered),
    max(placebo_portfolio_summary$FalseAlertsTempered)
  ),
  sprintf(
    "The lowest tempered false-alert burden occurred for %s, with %d false alerts, mean 95%% empirical coverage of %.1f%%, and mean WIS of %.2f.",
    lowest_false_alert$split_label,
    lowest_false_alert$FalseAlertsTempered,
    lowest_false_alert$MeanCoverage95 * 100,
    lowest_false_alert$MeanWIS
  ),
  "",
  "**Portfolio-level placebo summary**",
  md_table(table_s19_md),
  "",
  sprintf("![Falsification and calibration figure](./Supplementary%%20Appendix%%201_8/%s)", basename(figure_png_path)),
  "",
  "Source files:",
  sprintf("- `%s`", paste0("./Tables/", basename(summary_xlsx_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(summary_csv_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(disease_csv_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(horizon_csv_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(group_csv_path)))
)

message("Falsification and calibration outputs written:")
message(sprintf(" - %s", summary_xlsx_path))
