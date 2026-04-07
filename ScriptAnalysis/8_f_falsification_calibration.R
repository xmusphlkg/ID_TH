#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(openxlsx)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
  library(readr)
  library(scales)
})

invisible(Sys.setlocale("LC_TIME", "C"))

args <- commandArgs(trailingOnly = FALSE)
resolve_script_dir <- function() {
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
temp_dir_candidates <- c(
  file.path(script_dir, "temp"),
  file.path(project_root, "temp"),
  file.path(project_root, "ShinyDashboard", "data", "temp")
)
analysis_temp_dir <- temp_dir_candidates[file.exists(file.path(temp_dir_candidates, "month.RData"))][1]

if (is.na(analysis_temp_dir) || !nzchar(analysis_temp_dir)) {
  stop("Could not locate month.RData in expected temp directories.")
}

source(file.path(script_dir, "function", "theme_set.R"))

load(file.path(analysis_temp_dir, "month.RData"))

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
figure_dir <- file.path(project_root, "Outcome", "Appendix", "Supplementary Appendix 1_8")
summary_md_path <- file.path(project_root, "Outcome", "Appendix", "Falsification_and_calibration.md")
main_appendix_path <- file.path(project_root, "Outcome", "Appendix", "Supplementary_Appendix.md")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

best_model_path <- file.path(tables_dir, "Best_model_outcome.xlsx")
figure_png_path <- file.path(figure_dir, "falsification_and_calibration.png")
split_csv_path <- file.path(tables_dir, "Placebo_interruption_split_level.csv")
disease_csv_path <- file.path(tables_dir, "Placebo_interruption_disease_summary.csv")
summary_csv_path <- file.path(tables_dir, "Placebo_interruption_portfolio_summary.csv")
summary_xlsx_path <- file.path(tables_dir, "Falsification_and_calibration.xlsx")

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

coverage_palette <- c(
  "80% interval" = "#3C5488FF",
  "95% interval" = "#E64B35FF"
)

get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) return(NA_real_)
  (year(end) - year(start)) * 12 + month(end) - month(start)
}

find_sustained_date <- function(dates, condition, persistence = 3L) {
  condition[is.na(condition)] <- FALSE

  if (length(dates) < persistence) {
    return(as.Date(NA))
  }

  for (i in seq_len(length(dates) - persistence + 1L)) {
    if (all(condition[i:(i + persistence - 1L)])) {
      return(dates[i])
    }
  }

  as.Date(NA)
}

calc_status_one <- function(dates,
                            observed,
                            expected,
                            start_date,
                            recovery_threshold = 0.95,
                            persistence = 3L) {
  keep <- dates >= start_date

  df <- tibble(
    date = dates[keep],
    observed = observed[keep],
    expected = expected[keep]
  ) |>
    arrange(date) |>
    mutate(
      diff = observed - expected,
      cum_diff = cumsum(diff)
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

    lag_base <- c(df_search$cum_diff[1], head(df_search$cum_diff, -1))
    is_recovered_trend <- df_search$observed >= df_search$expected * recovery_threshold
    is_paying_back <- (df_search$cum_diff - lag_base) >= 0

    recovery_date <- find_sustained_date(df_search$date, is_recovered_trend & is_paying_back, persistence = persistence)

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

status_label_map <- c(
  "Debt Repaid" = "Balanced",
  "Recovered" = "Recovered but not balanced",
  "Suppressed" = "Suppressed",
  "No Deficit" = "No deficit"
)

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

actual_monthly <- data_month |>
  transmute(
    Shortname,
    date = ymd(paste(Year, Month, "01", sep = "-")),
    actual = Cases,
    Group
  )

best_models <- read.xlsx(best_model_path) |>
  filter(Best == 1) |>
  transmute(
    Shortname = disease,
    BestModel = if_else(Method == "Hybrid**", "Hybrid", Method)
  )

build_placebo_rows <- function(shortname, best_model) {
  forecast_path <- file.path(project_root, "Outcome", "Appendix", "Forecasts_with_intervals", paste0(shortname, "_forecasts.csv"))

  if (!file.exists(forecast_path)) {
    warning(sprintf("Forecast interval file not found for %s", shortname))
    return(tibble())
  }

  fc_raw <- suppressMessages(read_csv(forecast_path, show_col_types = FALSE))
  drop_cols <- names(fc_raw)[names(fc_raw) %in% c("", "X") | startsWith(names(fc_raw), "...")]

  fc <- fc_raw |>
    select(-any_of(drop_cols)) |>
    mutate(date = as.Date(date)) |>
    filter(Method == best_model, split %in% split_specs$split)

  if (nrow(fc) == 0) {
    warning(sprintf("No matching best-model interval forecasts for %s", shortname))
    return(tibble())
  }

  bind_rows(lapply(seq_len(nrow(split_specs)), function(i) {
    spec <- split_specs[i, ]

    one_split <- fc |>
      filter(split == spec$split[[1]]) |>
      left_join(
        actual_monthly |>
          filter(Shortname == shortname) |>
          select(date, actual, Group),
        by = "date"
      ) |>
      arrange(date)

    if (nrow(one_split) == 0) {
      return(tibble())
    }

    status <- calc_status_one(
      dates = one_split$date,
      observed = one_split$actual,
      expected = one_split$mean,
      start_date = spec$start_date[[1]]
    )

    tibble(
      Shortname = shortname,
      Group = one_split$Group[[1]],
      BestModel = best_model,
      split = spec$split[[1]],
      split_label = spec$split_label[[1]],
      start_date = spec$start_date[[1]],
      end_date = spec$end_date[[1]],
      PlaceboStatus = status$Status[[1]],
      PlaceboStatusLabel = unname(status_label_map[status$Status[[1]]]),
      FalseAlert = status$Status[[1]] != "No Deficit",
      FalseSuppressed = status$Status[[1]] == "Suppressed",
      RP_Months = ifelse(is.na(status$Date_Recovery[[1]]), NA_real_, get_months(spec$start_date[[1]], status$Date_Recovery[[1]])),
      BP_Months = ifelse(is.na(status$Date_Balance[[1]]), NA_real_, get_months(spec$start_date[[1]], status$Date_Balance[[1]])),
      Coverage80 = safe_mean(one_split$actual >= one_split$lower_80 & one_split$actual <= one_split$upper_80),
      Coverage95 = safe_mean(one_split$actual >= one_split$lower_95 & one_split$actual <= one_split$upper_95),
      MeanWidth80 = safe_mean(one_split$upper_80 - one_split$lower_80),
      MeanWidth95 = safe_mean(one_split$upper_95 - one_split$lower_95),
      MonthsEvaluated = nrow(one_split)
    )
  }))
}

placebo_split_level <- bind_rows(lapply(seq_len(nrow(best_models)), function(i) {
  build_placebo_rows(best_models$Shortname[[i]], best_models$BestModel[[i]])
}))

placebo_disease_summary <- placebo_split_level |>
  group_by(Shortname, Group, BestModel) |>
  summarise(
    SplitsAssessed = n(),
    FalseAlerts = sum(FalseAlert, na.rm = TRUE),
    FalseSuppressed = sum(FalseSuppressed, na.rm = TRUE),
    MeanCoverage80 = round(mean(Coverage80, na.rm = TRUE), 3),
    MeanCoverage95 = round(mean(Coverage95, na.rm = TRUE), 3),
    MeanWidth80 = round(mean(MeanWidth80, na.rm = TRUE), 1),
    MeanWidth95 = round(mean(MeanWidth95, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  arrange(desc(FalseAlerts), Shortname)

placebo_portfolio_summary <- placebo_split_level |>
  group_by(split, split_label, start_date, end_date) |>
  summarise(
    DiseasesAssessed = n(),
    FalseAlerts = sum(FalseAlert, na.rm = TRUE),
    FalseAlertRate = round(mean(FalseAlert, na.rm = TRUE), 3),
    FalseSuppressed = sum(FalseSuppressed, na.rm = TRUE),
    MeanCoverage80 = round(mean(Coverage80, na.rm = TRUE), 3),
    MeanCoverage95 = round(mean(Coverage95, na.rm = TRUE), 3),
    MeanWidth95 = round(mean(MeanWidth95, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  arrange(match(split, split_specs$split))

table_s19_md <- placebo_portfolio_summary |>
  transmute(
    `Pseudo interruption` = split_label,
    `Diseases assessed` = DiseasesAssessed,
    `False alerts` = FalseAlerts,
    `False suppressed` = FalseSuppressed,
    `Mean 80% coverage` = MeanCoverage80,
    `Mean 95% coverage` = MeanCoverage95,
    `Mean 95% interval width` = MeanWidth95
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
  geom_text(aes(label = PlaceboStatusLabel), size = 3, fontface = "bold") +
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

calibration_df <- placebo_disease_summary |>
  transmute(
    Shortname,
    `80% interval` = MeanCoverage80,
    `95% interval` = MeanCoverage95
  ) |>
  pivot_longer(-Shortname, names_to = "Interval", values_to = "EmpiricalCoverage") |>
  mutate(NominalCoverage = if_else(Interval == "80% interval", 0.80, 0.95))

portfolio_calibration <- placebo_split_level |>
  summarise(
    `80% interval` = mean(Coverage80, na.rm = TRUE),
    `95% interval` = mean(Coverage95, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "Interval", values_to = "EmpiricalCoverage") |>
  mutate(NominalCoverage = if_else(Interval == "80% interval", 0.80, 0.95))

panel_b <- ggplot(calibration_df, aes(x = NominalCoverage, y = EmpiricalCoverage, group = Shortname)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.8, color = "#666666") +
  geom_line(linewidth = 0.5, color = alpha("#8C8C8C", 0.6)) +
  geom_point(aes(color = Interval), size = 2.4, alpha = 0.9) +
  geom_point(
    data = portfolio_calibration,
    aes(x = NominalCoverage, y = EmpiricalCoverage, color = Interval),
    inherit.aes = FALSE,
    size = 5,
    shape = 21,
    stroke = 1.2,
    fill = "white"
  ) +
  geom_text(
    data = portfolio_calibration,
    aes(x = NominalCoverage, y = EmpiricalCoverage, label = percent(EmpiricalCoverage, accuracy = 0.1), color = Interval),
    inherit.aes = FALSE,
    nudge_y = 0.025,
    fontface = "bold",
    size = 4
  ) +
  scale_color_manual(values = coverage_palette) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.75, 1.0), breaks = c(0.8, 0.95)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.55, 1.0)) +
  theme_plot() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "B")

combined_figure <- cowplot::plot_grid(
  panel_a,
  panel_b,
  ncol = 1,
  rel_heights = c(1.95, 1.05),
  align = "v"
)

ggsave(
  filename = figure_png_path,
  plot = combined_figure,
  width = 15,
  height = 14,
  dpi = 320,
  bg = "white"
)

write_csv(placebo_split_level, split_csv_path)
write_csv(placebo_disease_summary, disease_csv_path)
write_csv(placebo_portfolio_summary, summary_csv_path)

write.xlsx(
  list(
    PortfolioSummary = placebo_portfolio_summary,
    DiseaseSummary = placebo_disease_summary,
    SplitLevel = placebo_split_level,
    SplitSpecs = split_specs
  ),
  file = summary_xlsx_path,
  overwrite = TRUE
)

escape_md <- function(x) {
  x <- ifelse(is.na(x), "NA", as.character(x))
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("[\r\n]+", " ", x)
  x
}

md_table <- function(df) {
  headers <- names(df)
  lines <- c(
    paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
  )

  if (nrow(df) == 0) {
    return(lines)
  }

  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(escape_md(row), collapse = " | "), " |")
  })

  c(lines, rows)
}

replace_or_append_block <- function(lines, block_id, replacement_lines) {
  begin_marker <- paste0("<!-- BEGIN ", block_id, " -->")
  end_marker <- paste0("<!-- END ", block_id, " -->")
  wrapped_replacement <- c(begin_marker, replacement_lines, end_marker)

  begin_idx <- which(trimws(lines) == begin_marker)[1]
  end_idx <- which(trimws(lines) == end_marker)[1]

  if (!is.na(begin_idx) && !is.na(end_idx) && end_idx > begin_idx) {
    return(c(
      if (begin_idx > 1) lines[seq_len(begin_idx - 1)] else character(),
      wrapped_replacement,
      if (end_idx < length(lines)) lines[(end_idx + 1):length(lines)] else character()
    ))
  }

  c(
    lines,
    "",
    "<div style=\"page-break-after: always;\"></div>",
    "",
    wrapped_replacement
  )
}

lowest_false_alert <- placebo_portfolio_summary |>
  arrange(FalseAlerts, desc(MeanCoverage95)) |>
  slice(1)

summary_lines <- c(
  "# Placebo Interruption and Calibration",
  "",
  "This attachment evaluates whether the best-model forecasting pipeline produces spurious operational alerts when the same RP/BP workflow is applied to pre-pandemic placebo interruption dates, and whether predictive interval coverage remains calibrated in those placebo windows.",
  sprintf(
    "Across the three placebo windows, the lowest false-alert count occurred for %s, with %d false alerts and mean 95%% interval coverage of %.1f%%.",
    lowest_false_alert$split_label,
    lowest_false_alert$FalseAlerts,
    lowest_false_alert$MeanCoverage95 * 100
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
  sprintf("- `%s`", paste0("./Tables/", basename(split_csv_path)))
)

writeLines(summary_lines, summary_md_path)

if (file.exists(main_appendix_path)) {
  appendix_lines <- readLines(main_appendix_path, warn = FALSE)
  appendix_block <- c(
    "## Part 8: Placebo interruption and predictive interval calibration",
    "",
    "This supplementary analysis applies the same RP/BP workflow to pre-pandemic placebo interruption dates and summarizes empirical predictive interval coverage for the selected best models.",
    "",
    "**Table S19. Portfolio-level placebo interruption and predictive interval calibration summary.**",
    md_table(table_s19_md),
    "",
    "<div style=\"page-break-after: always;\"></div>",
    "",
    sprintf("![**Fig. S128. Placebo interruption falsification and predictive interval calibration.**](Supplementary%%20Appendix%%201_8/%s)", basename(figure_png_path)),
    "",
    "**Fig. S128. Placebo interruption falsification and predictive interval calibration.** Panel A shows the status assigned to each disease when the RP/BP workflow is applied to three pre-pandemic placebo interruption dates using the disease-specific best model. Panel B compares nominal and empirical predictive interval coverage across diseases, with larger outlined markers indicating the portfolio mean."
  )
  appendix_lines <- replace_or_append_block(appendix_lines, "PLACEBO_INTERRUPTION_CALIBRATION", appendix_block)
  writeLines(appendix_lines, main_appendix_path)
}

message("Falsification and calibration outputs written:")
message(sprintf(" - %s", summary_xlsx_path))
message(sprintf(" - %s", summary_csv_path))
message(sprintf(" - %s", disease_csv_path))
message(sprintf(" - %s", split_csv_path))
message(sprintf(" - %s", figure_png_path))