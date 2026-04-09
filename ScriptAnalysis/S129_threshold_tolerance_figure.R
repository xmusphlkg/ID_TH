#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(cowplot)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(scales)
  library(tibble)
  library(tidyr)
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
setwd(script_dir)

source("./function/theme_set.R")

outcome_path <- file.path(script_dir, "temp", "outcome.RData")
if (!file.exists(outcome_path)) {
  stop(sprintf("Missing required cache: %s", outcome_path))
}

load(outcome_path)

appendix_tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
appendix_fig_dir <- file.path(project_root, "Outcome", "Appendix", "Supplementary Appendix 1_8")
dir.create(appendix_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(appendix_fig_dir, recursive = TRUE, showWarnings = FALSE)

find_sustained_date <- function(dates, condition, persistence = 3L) {
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
    return("No Deficit")
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

    status <- if (!is.na(recovery_date)) "Recovered" else "Suppressed"
  }

  if (!is.na(trough_date) && max_deficit_raw < 0) {
    df_post_trough <- df |>
      filter(date > trough_date)

    balance_idx <- which(df_post_trough$cum_diff >= 0)[1]
    if (!is.na(balance_idx)) {
      status <- "Debt Repaid"
    }
  }

  status
}

primary <- map_dfr(outcome, function(item) {
  tibble(
    Shortname = unique(item$outcome_data$Shortname)[1],
    PrimaryStatus = calc_status_one(
      dates = item$outcome_data$date,
      observed = item$outcome_data$value,
      expected = item$outcome_data$median,
      recovery_threshold = 0.95,
      persistence = 3L
    )
  )
})

thresholds <- seq(0.85, 1.10, by = 0.05)
persistences <- 2:4
threshold_levels <- percent(thresholds, accuracy = 1)
persistence_levels <- paste0(persistences, " mo")
max_share <- 1 / nrow(primary)

threshold_grid <- tidyr::crossing(
  Threshold = thresholds,
  Persistence = persistences
) |>
  mutate(
    details = map2(Threshold, Persistence, function(thresh, pers) {
      map_dfr(outcome, function(item) {
        tibble(
          Shortname = unique(item$outcome_data$Shortname)[1],
          Status = calc_status_one(
            dates = item$outcome_data$date,
            observed = item$outcome_data$value,
            expected = item$outcome_data$median,
            recovery_threshold = thresh,
            persistence = pers
          )
        )
      }) |>
        left_join(primary, by = "Shortname") |>
        mutate(Changed = Status != PrimaryStatus)
    })
  ) |>
  mutate(
    summary = map(details, function(df) {
      changed_names <- sort(unique(df$Shortname[df$Changed]))
      tibble(
        N_changed = sum(df$Changed, na.rm = TRUE),
        P_changed = mean(df$Changed, na.rm = TRUE),
        Changed_diseases = if (length(changed_names) == 0) "None" else paste(changed_names, collapse = ", ")
      )
    })
  ) |>
  select(-details) |>
  unnest(summary) |>
  mutate(
    ThresholdLabel = percent(Threshold, accuracy = 1),
    PersistenceLabel = paste0(Persistence, " mo"),
    ConfigLabel = paste0(ThresholdLabel, " / ", PersistenceLabel),
    TileLabel = if_else(
      N_changed == 0,
      "0",
      paste0(N_changed, "/24\n", Changed_diseases)
    )
  )

write_csv(
  threshold_grid |>
    select(Threshold, Persistence, N_changed, P_changed, Changed_diseases),
  file.path(appendix_tables_dir, "Threshold_tolerance_stress_test.csv")
)

heatmap_df <- threshold_grid |>
  mutate(
    ThresholdLabel = factor(ThresholdLabel, levels = threshold_levels),
    PersistenceLabel = factor(PersistenceLabel, levels = rev(persistence_levels))
  )

panel_a <- ggplot(
  heatmap_df,
  aes(x = ThresholdLabel, y = PersistenceLabel, fill = P_changed)
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = TileLabel),
    size = 4.4,
    lineheight = 0.9,
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colours = c("#F4F5F7", "#FDD0A2", "#E64B35FF"),
    limits = c(0, max_share),
    breaks = c(0, max_share / 2, max_share),
    labels = percent_format(accuracy = 0.1),
    name = "Reclassified share"
  ) +
  theme_plot() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(5, 12, 5, 5)
  ) +
  labs(
    title = "A",
    x = "RP threshold",
    y = "Persistence requirement"
  )

changed_configs <- threshold_grid |>
  filter(N_changed > 0) |>
  mutate(
    ConfigLabel = factor(
      ConfigLabel,
      levels = rev(ConfigLabel[order(Threshold, Persistence)])
    )
  )

if (nrow(changed_configs) > 0) {
  panel_b <- ggplot(
    changed_configs,
    aes(x = ConfigLabel, y = N_changed)
  ) +
    geom_col(fill = "#E64B35FF", width = 0.62) +
    geom_text(
      aes(label = paste0(Changed_diseases, " (", N_changed, "/24)")),
      hjust = -0.08,
      size = 4.4,
      fontface = "bold"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, max(changed_configs$N_changed) + 0.9),
      breaks = 0:max(changed_configs$N_changed),
      expand = expansion(mult = c(0, 0.03))
    ) +
    theme_plot() +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.margin = margin(5, 35, 5, 5)
    ) +
    labs(
      title = "B",
      x = NULL,
      y = "Diseases reclassified"
    )
} else {
  panel_b <- ggplot() +
    annotate(
      "text",
      x = 1,
      y = 1,
      label = "No reclassifications across the tested grid",
      size = 6,
      fontface = "bold"
    ) +
    theme_void() +
    labs(title = "B") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      plot.margin = margin(5, 5, 5, 5)
    )
}

combined_figure <- cowplot::plot_grid(
  panel_a,
  panel_b,
  ncol = 2,
  rel_widths = c(1.55, 0.95),
  align = "h",
  axis = "tb"
)

ggsave(
  filename = file.path(appendix_fig_dir, "threshold_tolerance.png"),
  plot = combined_figure,
  width = 13.5,
  height = 6.4,
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = file.path(appendix_fig_dir, "threshold_tolerance.pdf"),
  plot = combined_figure,
  width = 13.5,
  height = 6.4,
  bg = "white"
)
