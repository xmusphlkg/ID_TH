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
analysis_temp_dir <- temp_dir_candidates[file.exists(file.path(temp_dir_candidates, "outcome.RData")) & file.exists(file.path(temp_dir_candidates, "month.RData"))][1]

if (is.na(analysis_temp_dir) || !nzchar(analysis_temp_dir)) {
  stop("Could not locate outcome.RData and month.RData in expected temp directories.")
}

source(file.path(script_dir, "function", "theme_set.R"))

load(file.path(analysis_temp_dir, "outcome.RData"))
load(file.path(analysis_temp_dir, "month.RData"))

selected_shortnames <- purrr::map_chr(outcome, ~ unique(.x$outcome_data$Shortname)[1])
data_class <- data_class |>
  filter(Shortname %in% selected_shortnames)

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
figure_dir <- file.path(project_root, "Outcome", "Appendix", "Supplementary Appendix 1_7")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

figure_png_path <- file.path(figure_dir, "temporal_utility_validation.png")
disease_csv_path <- file.path(tables_dir, "Temporal_utility_disease_level.csv")
summary_csv_path <- file.path(tables_dir, "Temporal_utility_freeze_summary.csv")
summary_xlsx_path <- file.path(tables_dir, "Temporal_utility_validation.xlsx")

freeze_specs <- tibble(
  FreezeDate = as.Date(c("2023-12-01", "2024-06-01")),
  ValidationEnd = as.Date(c("2024-12-01", "2025-12-01")),
  WindowLabel = c("Freeze 2023-12 to 2024-12", "Freeze 2024-06 to 2025-12")
)

priority_levels <- c(
  "High priority manual review",
  "Recalibrate and monitor",
  "Cumulative review needed",
  "Recovered but recalibrate seasonality",
  "Low priority routine review",
  "No deficit monitoring"
)

priority_palette <- c(
  "High priority manual review" = "#CC3D24",
  "Recalibrate and monitor" = "#0B6E69",
  "Cumulative review needed" = "#E64B35FF",
  "Recovered but recalibrate seasonality" = "#3C5488FF",
  "Low priority routine review" = "#91D1C2FF",
  "No deficit monitoring" = "#BDBDBD"
)

method_palette <- c(
  "Framework queue" = "#0B6E69",
  "Incidence-only queue" = "#CC3D24"
)

priority_abbrev <- c(
  "High priority manual review" = "High",
  "Recalibrate and monitor" = "Recal",
  "Cumulative review needed" = "Cum",
  "Recovered but recalibrate seasonality" = "Season",
  "Low priority routine review" = "Routine",
  "No deficit monitoring" = "No-def"
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
                            start_date = as.Date("2020-01-01"),
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

truncate_item_to_date <- function(item, end_date) {
  keep <- item$outcome_data$date <= end_date
  list(
    outcome_data = item$outcome_data[keep, , drop = FALSE],
    MCMC = item$MCMC[keep, , drop = FALSE]
  )
}

calc_status_paths <- function(item,
                              end_date,
                              start_date = as.Date("2020-01-01"),
                              recovery_threshold = 0.95,
                              persistence = 3L,
                              path_cap = 400L) {
  truncated <- truncate_item_to_date(item, end_date)
  od <- truncated$outcome_data |>
    arrange(date)

  primary <- calc_status_one(
    dates = od$date,
    observed = od$value,
    expected = od$median,
    start_date = start_date,
    recovery_threshold = recovery_threshold,
    persistence = persistence
  )

  n_paths <- ncol(truncated$MCMC)
  if (n_paths == 0) {
    return(list(primary = primary, sims = tibble(), paths_used = 0L))
  }

  if (n_paths > path_cap) {
    path_idx <- unique(round(seq(1, n_paths, length.out = path_cap)))
  } else {
    path_idx <- seq_len(n_paths)
  }

  sim_results <- lapply(path_idx, function(j) {
    calc_status_one(
      dates = od$date,
      observed = od$value,
      expected = truncated$MCMC[, j],
      start_date = start_date,
      recovery_threshold = recovery_threshold,
      persistence = persistence
    )
  })

  list(
    primary = primary,
    sims = bind_rows(sim_results),
    paths_used = length(path_idx)
  )
}

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

compute_phase_summary <- function(end_date) {
  season_data_obs <- data_month |>
    filter(Shortname %in% selected_shortnames) |>
    select(Shortname, year = Year, month = Month, value = Cases, Group) |>
    mutate(
      date = ymd(paste(year, month, "01", sep = "-")),
      Period = case_when(
        year <= 2019 ~ "Pre-COVID (Observed)",
        date >= as.Date("2023-01-01") & date <= end_date ~ "Post-PHSM (Observed)",
        TRUE ~ "Ignore"
      )
    ) |>
    filter(Period != "Ignore")

  season_data_pred <- purrr::map_dfr(outcome, ~ .x$outcome_data) |>
    filter(date >= as.Date("2023-01-01"), date <= end_date) |>
    mutate(
      year = year(date),
      month = month(date),
      Period = "Post-PHSM (Predicted)",
      value = median
    ) |>
    left_join(select(data_class, Shortname, Group), by = "Shortname") |>
    select(Shortname, year, month, value, Group, Period)

  df_monthly_mean <- bind_rows(
    season_data_obs |> select(Shortname, year, month, value, Group, Period),
    season_data_pred
  ) |>
    group_by(Shortname, Group, Period, month) |>
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

  phase_summary <- df_monthly_mean |>
    group_by(Shortname, Group, Period) |>
    summarise(
      peak_com = if (all(is.na(avg_value))) NA_real_ else get_peak_month_com(pick(month, avg_value)),
      peak_to_trough = if (all(is.na(avg_value))) NA_real_ else max(avg_value, na.rm = TRUE) / pmax(min(avg_value, na.rm = TRUE), 1e-6),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = c(Shortname, Group),
      names_from = Period,
      values_from = c(peak_com, peak_to_trough)
    ) |>
    mutate(
      shift_vs_pre = correct_circular_shift(`peak_com_Post-PHSM (Observed)` - `peak_com_Pre-COVID (Observed)`),
      shift_vs_pred = correct_circular_shift(`peak_com_Post-PHSM (Observed)` - `peak_com_Post-PHSM (Predicted)`),
      amplitude_ratio_vs_pre = `peak_to_trough_Post-PHSM (Observed)` / `peak_to_trough_Pre-COVID (Observed)`
    ) |>
    select(Shortname, Group, shift_vs_pre, shift_vs_pred, amplitude_ratio_vs_pre)

  phase_summary
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

priority_needs_review <- function(priority) {
  !priority %in% c("Low priority routine review", "No deficit monitoring")
}

status_label_map <- c(
  "Debt Repaid" = "Balanced",
  "Recovered" = "Recovered but not balanced",
  "Suppressed" = "Suppressed",
  "No Deficit" = "No deficit"
)

build_one_freeze_row <- function(item, freeze_date, validation_end, phase_freeze, phase_validation, window_label) {
  shortname <- unique(item$outcome_data$Shortname)[1]

  frozen <- calc_status_paths(item, end_date = freeze_date)
  realized_item <- truncate_item_to_date(item, validation_end)
  realized_primary <- calc_status_one(
    dates = realized_item$outcome_data$date,
    observed = realized_item$outcome_data$value,
    expected = realized_item$outcome_data$median,
    start_date = as.Date("2020-01-01")
  )

  frozen_primary <- frozen$primary

  frozen_phase <- phase_freeze |>
    filter(Shortname == shortname) |>
    slice(1)
  realized_phase <- phase_validation |>
    filter(Shortname == shortname) |>
    slice(1)

  frozen_shift_pre <- if (nrow(frozen_phase) == 0) NA_real_ else frozen_phase$shift_vs_pre[[1]]
  frozen_shift_pred <- if (nrow(frozen_phase) == 0) NA_real_ else frozen_phase$shift_vs_pred[[1]]
  realized_shift_pre <- if (nrow(realized_phase) == 0) NA_real_ else realized_phase$shift_vs_pre[[1]]
  realized_shift_pred <- if (nrow(realized_phase) == 0) NA_real_ else realized_phase$shift_vs_pred[[1]]

  frozen_priority <- assign_framework_priority(frozen_primary$Status[[1]], frozen_shift_pre, frozen_shift_pred)
  realized_priority <- assign_framework_priority(realized_primary$Status[[1]], realized_shift_pre, realized_shift_pred)

  sims <- frozen$sims
  pr_rp <- if (nrow(sims) == 0) NA_real_ else mean(sims$Status %in% c("Recovered", "Debt Repaid"))
  pr_bp <- if (nrow(sims) == 0) NA_real_ else mean(sims$Status == "Debt Repaid")

  incidence_only_status <- case_when(
    frozen_primary$Status[[1]] == "Suppressed" ~ "Needs review by incidence only",
    frozen_primary$Status[[1]] == "No Deficit" ~ "No deficit monitoring",
    TRUE ~ "Routine by incidence only"
  )

  realized_needs_review <- priority_needs_review(realized_priority)
  frozen_framework_needs_review <- priority_needs_review(frozen_priority)
  frozen_incidence_needs_review <- frozen_primary$Status[[1]] == "Suppressed"

  tibble(
    Shortname = shortname,
    Group = data_class$Group[match(shortname, data_class$Shortname)],
    FreezeDate = freeze_date,
    ValidationEnd = validation_end,
    WindowLabel = window_label,
    FrozenStatus = frozen_primary$Status[[1]],
    FrozenStatusLabel = unname(status_label_map[frozen_primary$Status[[1]]]),
    FrozenPrRP = round(pr_rp, 3),
    FrozenPrBP = round(pr_bp, 3),
    FrozenFrameworkPriority = frozen_priority,
    FrozenIncidenceOnly = incidence_only_status,
    RealizedStatus = realized_primary$Status[[1]],
    RealizedStatusLabel = unname(status_label_map[realized_primary$Status[[1]]]),
    RealizedFrameworkPriority = realized_priority,
    LaterReviewNeed = realized_needs_review,
    FrameworkNeedsReview = frozen_framework_needs_review,
    IncidenceOnlyNeedsReview = frozen_incidence_needs_review,
    FrameworkDecisionAcc = frozen_framework_needs_review == realized_needs_review,
    IncidenceDecisionAcc = frozen_incidence_needs_review == realized_needs_review,
    AvertedUnderTriage = realized_needs_review & frozen_framework_needs_review & !frozen_incidence_needs_review,
    FrozenShiftVsPre = round(frozen_shift_pre, 2),
    FrozenShiftVsPred = round(frozen_shift_pred, 2),
    RealizedShiftVsPre = round(realized_shift_pre, 2),
    RealizedShiftVsPred = round(realized_shift_pred, 2),
    FrozenRPMonths = ifelse(is.na(frozen_primary$Date_Recovery[[1]]), NA_real_, get_months(as.Date("2020-01-01"), frozen_primary$Date_Recovery[[1]])),
    FrozenBPMonths = ifelse(is.na(frozen_primary$Date_Balance[[1]]), NA_real_, get_months(as.Date("2020-01-01"), frozen_primary$Date_Balance[[1]])),
    RealizedRPMonths = ifelse(is.na(realized_primary$Date_Recovery[[1]]), NA_real_, get_months(as.Date("2020-01-01"), realized_primary$Date_Recovery[[1]])),
    RealizedBPMonths = ifelse(is.na(realized_primary$Date_Balance[[1]]), NA_real_, get_months(as.Date("2020-01-01"), realized_primary$Date_Balance[[1]])),
    ProbabilityPathsUsed = frozen$paths_used
  )
}

phase_cache <- lapply(seq_len(nrow(freeze_specs)), function(i) {
  list(
    freeze = compute_phase_summary(freeze_specs$FreezeDate[i]),
    validation = compute_phase_summary(freeze_specs$ValidationEnd[i])
  )
})

temporal_disease <- bind_rows(lapply(seq_len(nrow(freeze_specs)), function(i) {
  spec <- freeze_specs[i, ]
  phase_bundle <- phase_cache[[i]]

  bind_rows(lapply(outcome, build_one_freeze_row,
                   freeze_date = spec$FreezeDate[[1]],
                   validation_end = spec$ValidationEnd[[1]],
                   phase_freeze = phase_bundle$freeze,
                   phase_validation = phase_bundle$validation,
                   window_label = spec$WindowLabel[[1]]))
})) |>
  mutate(
    FreezeDateLabel = format(FreezeDate, "%Y-%m"),
    PriorityRank = match(FrozenFrameworkPriority, priority_levels)
  )

temporal_summary <- temporal_disease |>
  group_by(FreezeDate, ValidationEnd, WindowLabel) |>
  summarise(
    DiseasesAssessed = n(),
    LaterReviewDiseases = sum(LaterReviewNeed, na.rm = TRUE),
    FrameworkCaptured = sum(LaterReviewNeed & FrameworkNeedsReview, na.rm = TRUE),
    IncidenceOnlyCaptured = sum(LaterReviewNeed & IncidenceOnlyNeedsReview, na.rm = TRUE),
    AvertedUnderTriageDiseases = sum(AvertedUnderTriage, na.rm = TRUE),
    FrameworkAccuracy = round(mean(FrameworkDecisionAcc, na.rm = TRUE), 3),
    IncidenceOnlyAccuracy = round(mean(IncidenceDecisionAcc, na.rm = TRUE), 3),
    FrameworkSensitivity = round(ifelse(sum(LaterReviewNeed, na.rm = TRUE) == 0, NA_real_, sum(LaterReviewNeed & FrameworkNeedsReview, na.rm = TRUE) / sum(LaterReviewNeed, na.rm = TRUE)), 3),
    IncidenceOnlySensitivity = round(ifelse(sum(LaterReviewNeed, na.rm = TRUE) == 0, NA_real_, sum(LaterReviewNeed & IncidenceOnlyNeedsReview, na.rm = TRUE) / sum(LaterReviewNeed, na.rm = TRUE)), 3),
    FrameworkFalsePositives = sum(!LaterReviewNeed & FrameworkNeedsReview, na.rm = TRUE),
    IncidenceOnlyFalsePositives = sum(!LaterReviewNeed & IncidenceOnlyNeedsReview, na.rm = TRUE),
    PathsUsedPerDisease = max(ProbabilityPathsUsed, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(FreezeDate)

table_s18_md <- temporal_summary |>
  transmute(
    `Freeze point` = format(FreezeDate, "%Y-%m-%d"),
    `Validation end` = format(ValidationEnd, "%Y-%m-%d"),
    `Later review diseases` = LaterReviewDiseases,
    `Framework captured` = FrameworkCaptured,
    `Incidence-only captured` = IncidenceOnlyCaptured,
    `Averted under-triage` = AvertedUnderTriageDiseases,
    `Framework accuracy` = FrameworkAccuracy,
    `Incidence-only accuracy` = IncidenceOnlyAccuracy
  )

disease_levels <- temporal_disease |>
  group_by(Shortname) |>
  summarise(OrderKey = min(PriorityRank, na.rm = TRUE), .groups = "drop") |>
  arrange(OrderKey, Shortname) |>
  pull(Shortname)

heatmap_df <- temporal_disease |>
  mutate(
    Shortname = factor(Shortname, levels = rev(disease_levels)),
    WindowLabel = factor(WindowLabel, levels = freeze_specs$WindowLabel),
    PriorityLabel = unname(priority_abbrev[FrozenFrameworkPriority]),
    LaterReviewFlag = if_else(LaterReviewNeed, "Needs later review", "Routine later")
  )

panel_a <- ggplot(heatmap_df, aes(x = WindowLabel, y = Shortname, fill = FrozenFrameworkPriority)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = PriorityLabel), size = 3.1, fontface = "bold") +
  scale_fill_manual(values = priority_palette, drop = FALSE) +
  theme_plot() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 10, 5, 5)
  ) +
  labs(title = "A")

metric_df <- temporal_summary |>
  transmute(
    WindowLabel,
    `Framework queue capture` = FrameworkSensitivity,
    `Incidence-only capture` = IncidenceOnlySensitivity,
    `Framework queue accuracy` = FrameworkAccuracy,
    `Incidence-only accuracy` = IncidenceOnlyAccuracy
  ) |>
  pivot_longer(-WindowLabel, names_to = "Metric", values_to = "Value") |>
  mutate(
    Method = case_when(
      str_detect(Metric, "Framework") ~ "Framework queue",
      TRUE ~ "Incidence-only queue"
    ),
    Metric = case_when(
      str_detect(Metric, "capture") ~ "Later review capture",
      TRUE ~ "Overall decision accuracy"
    ),
    WindowLabel = factor(WindowLabel, levels = freeze_specs$WindowLabel)
  )

panel_b <- ggplot(metric_df, aes(x = Metric, y = Value, fill = Method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  facet_wrap(~ WindowLabel, nrow = 1) +
  scale_fill_manual(values = method_palette) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_plot() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(title = "B")

combined_figure <- cowplot::plot_grid(
  panel_a,
  panel_b,
  ncol = 1,
  rel_heights = c(2.0, 1.05),
  align = "v",
  axis = "lr"
)

ggsave(
  filename = figure_png_path,
  plot = combined_figure,
  width = 15,
  height = 14,
  dpi = 320,
  bg = "white"
)

write_csv(temporal_disease, disease_csv_path)
write_csv(temporal_summary, summary_csv_path)

write.xlsx(
  list(
    FreezeSummary = temporal_summary,
    DiseaseLevel = temporal_disease,
    FreezeSpecs = freeze_specs
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

best_gain <- temporal_summary |>
  mutate(CaptureGain = FrameworkCaptured - IncidenceOnlyCaptured) |>
  arrange(desc(CaptureGain)) |>
  slice(1)

summary_lines <- c(
  "# Temporal Utility Validation",
  "",
  "This attachment evaluates whether framework-based review queues created at fixed decision freeze points better captured later disease-level review needs than an incidence-only comparator.",
  sprintf(
    "Two freeze points were evaluated: %s.",
    paste(format(freeze_specs$FreezeDate, "%Y-%m-%d"), collapse = " and ")
  ),
  sprintf(
    "The largest capture gain occurred at the %s freeze point, where the framework captured %d later review-needing diseases versus %d under incidence-only review.",
    format(best_gain$FreezeDate, "%Y-%m-%d"),
    best_gain$FrameworkCaptured,
    best_gain$IncidenceOnlyCaptured
  ),
  "",
  "**Freeze-point summary**",
  md_table(table_s18_md),
  "",
  sprintf("![Temporal utility validation figure](./Supplementary%%20Appendix%%201_7/%s)", basename(figure_png_path)),
  "",
  "Source files:",
  sprintf("- `%s`", paste0("./Tables/", basename(summary_xlsx_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(summary_csv_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(disease_csv_path)))
)

message("Temporal utility validation outputs written:")
message(sprintf(" - %s", summary_xlsx_path))
message(sprintf(" - %s", summary_csv_path))
message(sprintf(" - %s", disease_csv_path))
message(sprintf(" - %s", figure_png_path))
