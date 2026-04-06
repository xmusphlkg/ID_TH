#####################################
## Fig 4 — Seasonal Displacement Detection
## npj Digital Medicine submission
## Output: ../Outcome/Publish/npjDM/fig4.pdf | fig4.png
##
## Panel A–D:  IRR heatmaps by disease group (observed / expected ratio, 2020–2025)
## Panels E–AB: Radar charts — pre-pandemic vs. post-PHSM observed vs. counterfactual
##   seasonal profiles per disease (normalized, closed polygon)
#####################################

library(tidyverse)
library(openxlsx)
library(patchwork)
library(paletteer)
library(ggh4x)
library(ggnewscale)

Sys.setlocale("LC_TIME", "C")
remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

out_dir <- "../Outcome/Publish/npjDM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Design palette -----
COL_TEAL  <- "#0B6E69"
COL_CORAL <- "#CC3D24"
COL_SLATE <- "#22313F"
COL_MUTED <- "#62707B"

# Seasonal period colours — consistent with ShinyDashboard
PERIOD_COLORS <- c(
     "Pre-COVID (Observed)"    = "#7E6148FF",
     "Post-PHSM (Observed)"   = "#E64B35FF",
     "Post-PHSM (Predicted)"  = "#91D1C2FF"
)

# ============================================================
# Data
# ============================================================

load("./temp/month.RData")
load("./temp/outcome.RData")
load("./temp/best_model_figure.RData")

data_class <- data_class |>
     mutate(label = paste0(int2col(id + 4), ": ", Shortname))

# ============================================================
# PART 1 — IRR heatmaps (Panels A–D)
# ============================================================

data_outcome_all <- lapply(seq(nrow(data_class)), function(i) outcome[[i]]$outcome_data) |>
     bind_rows() |>
     mutate(
          diff_percent  = round((value + add_value) / (median + add_value), 3),
          label         = if_else(diff_percent > 10, "*", ""),
          diff_visual   = if_else(diff_percent > 10, 10, diff_percent),
          date_num      = format(ymd(date), "%Y.%m")
     ) |>
     left_join(select(data_class, Shortname, Group), by = "Shortname") |>
     select(Shortname, date, date_num, value, median, diff_visual, label, Group)

disease_groups_select <- unique(data_outcome_all$Group)

names(fill_color) <- levels(data_class$Group)

plot_irr_panel <- function(g) {
     disease_names <- data_class$Shortname[data_class$Group == disease_groups[g]]
     
     data_g <- data_outcome_all |>
          filter(Shortname %in% disease_names) |>
          mutate(Shortname = factor(Shortname, levels = disease_names))
     
     ggplot(data_g, aes(fill = diff_visual, x = date_num, y = Shortname)) +
          geom_tile() +
          geom_text(aes(label = label), vjust = 0.5, size = 2.8) +
          coord_cartesian(ratio = 2) +
          scale_fill_gradientn(
               colors   = paletteer_d("LaCroixColoR::Lemon", direction = -1)[-1],
               trans    = log_fill,
               breaks   = c(0, 0.5, 1, 2, 5, 10),
               labels   = c("0", "0.5", "1.0", "2.0", "5.0", ">10.0"),
               limits   = c(0, 10)
          ) +
          scale_x_discrete(
               breaks = paste(unique(year(data_g$date)), "01", sep = "."),
               labels = unique(year(data_g$date)),
               expand = expansion(add = c(0, 0))
          ) +
          scale_y_discrete(limits = rev(disease_names), expand = c(0, 0)) +
          theme_bw() +
          theme(
               legend.position      = "right",
               plot.title           = element_text(size = 12, hjust = 0, vjust = 0.5, face = "bold"),
               plot.title.position  = "plot",
               axis.text.x          = element_text(size = 9),
               axis.text.y          = element_text(size = 9),
               panel.grid           = element_blank()
          ) +
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10, title.position = "top")) +
          labs(
               x     = "Date",
               y     = NULL,
               fill  = "IRR",
               title = paste0(LETTERS[g], ": ", disease_groups_select[g])
          )
}

irr_list <- lapply(seq_along(disease_groups_select), plot_irr_panel)

panel_irr_1 <- wrap_plots(irr_list[1:2], ncol = 1, guides = "collect")
panel_irr_1 <- collect_guides_bottom(panel_irr_1)

panel_irr_2 <- wrap_plots(irr_list[3:4], ncol = 1, guides = "collect")
panel_irr_2 <- collect_guides_bottom(panel_irr_2)

# ============================================================
# PART 2 — Radar charts (Panels E–AB)
# ============================================================

# Seasonal data preparation
season_data_obs <- data_month |>
     filter(Shortname %in% data_class$Shortname) |>
     select(Shortname, year = Year, month = Month, value = Cases, Group) |>
     mutate(
          date        = ymd(paste(year, month, "01", sep = "-")),
          month_label = month(date, label = TRUE, abbr = TRUE),
          Period      = case_when(
               year <= 2019 ~ "Pre-COVID (Observed)",
               year >= 2023 ~ "Post-PHSM (Observed)",
               TRUE ~ "Pandemic"
          )
     ) |>
     filter(Period != "Pandemic")

season_data_pred <- lapply(seq_along(outcome), function(i) outcome[[i]]$outcome_data) |>
     bind_rows() |>
     filter(year(date) >= 2023) |>
     mutate(
          year        = year(date),
          month       = month(date),
          month_label = month(date, label = TRUE, abbr = TRUE),
          Period      = "Post-PHSM (Predicted)",
          value       = median
     ) |>
     left_join(select(data_class, Shortname, Group), by = "Shortname") |>
     select(Shortname, year, month, value, Group, month_label, Period)

df_season <- bind_rows(season_data_obs, season_data_pred)

# Helper: close radar polygon by duplicating month 1 as month 13
make_closed_data <- function(data) {
     data |>
          group_split(Shortname, Period) |>
          purrr::map_dfr(function(df) {
               month1 <- df |> filter(month == 1)
               month13 <- month1 |> mutate(month = 13)
               bind_rows(df, month13)
          })
}

df_monthly_mean <- df_season |>
     group_by(Shortname, Group, Period, month, month_label) |>
     summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

period_levels <- c("Pre-COVID (Observed)", "Post-PHSM (Observed)", "Post-PHSM (Predicted)")

df_monthly_pattern <- df_monthly_mean |>
     group_by(Shortname, Period) |>
     mutate(
          norm_value = (avg_value - min(avg_value)) / pmax(max(avg_value) - min(avg_value), 1e-6),
          Period     = factor(Period, levels = period_levels)
     ) |>
     ungroup() |>
     make_closed_data()

# Amplitude metrics
df_season_amplitude <- df_monthly_mean |>
     group_by(Shortname, Group, Period) |>
     summarise(
          peak_to_trough = max(avg_value, na.rm = TRUE) / pmax(min(avg_value, na.rm = TRUE), 1e-6),
          .groups = "drop"
     ) |>
     mutate(Period = factor(Period, levels = period_levels))

# Peak month helpers
get_peak_month_com <- function(df) {
     m <- df$month
     w <- df$avg_value
     theta <- 2 * pi * (m - 1) / 12
     x <- sum(w * cos(theta), na.rm = TRUE)
     y <- sum(w * sin(theta), na.rm = TRUE)
     m_com <- (round((atan2(y, x) * 12) / (2 * pi)) + 1)
     ((m_com - 1) %% 12) + 1
}

correct_circular_shift <- function(s) {
     dplyr::case_when(s > 6 ~ s - 12, s < -6 ~ s + 12, TRUE ~ s)
}

peak_table <- df_monthly_mean |>
     filter(!is.na(Period)) |>
     group_by(Shortname, Group, Period) |>
     summarise(
          peak_com  = get_peak_month_com(cur_data()),
          peak_max  = month[which.max(avg_value)],
          .groups   = "drop"
     ) |>
     mutate(Period = factor(Period, levels = period_levels))

# Phase shift summary (for annotations)
phase_shift <- peak_table |>
     select(Shortname, Group, Period, peak_com) |>
     pivot_wider(names_from = Period, values_from = peak_com) |>
     rename(pre = `Pre-COVID (Observed)`,
            post_obs  = `Post-PHSM (Observed)`,
            post_pred = `Post-PHSM (Predicted)`) |>
     mutate(
          shift_vs_pre  = correct_circular_shift(post_obs - pre),
          shift_vs_pred = correct_circular_shift(post_obs - post_pred)
     )

# Rainy season ribbon data
rainy_ribbon <- data.frame(month = 1:13) |>
     mutate(
          season = if_else(month %in% 5:10, "Rainy season", "Dry season"),
          ymin   = 0,
          ymax   = 1
     )

# Build one radar panel per disease
plot_radar <- function(i) {
     dname <- data_class$Shortname[i]
     title_lab <- data_class$label[i]
     
     pdata <- df_monthly_pattern |>
          filter(Shortname == dname) |>
          arrange(Period, month)
     
     tdata <- peak_table |>
          filter(Shortname == dname) |>
          left_join(
               df_monthly_pattern |> select(Shortname, Period, month, norm_value),
               by = c("Shortname", "Period", "peak_max" = "month")
          ) |>
          rename(peak_norm_max = norm_value) |>
          left_join(
               df_monthly_pattern |> select(Shortname, Period, month, norm_value),
               by = c("Shortname", "Period", "peak_com" = "month")
          ) |>
          rename(peak_norm_com = norm_value)
     
     ps <- phase_shift |> filter(Shortname == dname)
     subtitle_text <- sprintf("Δpre: %+d m  ·  Δcf: %+d m",
                              if (nrow(ps) > 0) ps$shift_vs_pre  else 0,
                              if (nrow(ps) > 0) ps$shift_vs_pred else 0)
     
     ggplot(pdata, aes(x = month, y = norm_value, colour = Period, fill = Period)) +
          geom_ribbon(data = rainy_ribbon,
                      aes(x = month, ymin = ymin, ymax = ymax, fill = season),
                      inherit.aes = FALSE, alpha = 0.25) +
          scale_fill_manual(
               values = c("Rainy season" = "#B3D7F0", "Dry season" = "#FFFBEA"),
               name = "Season"
          ) +
          ggnewscale::new_scale_fill() +
          geom_area(position = "identity", alpha = 0.12, linewidth = 0.5) +
          geom_point(data = tdata,
                     aes(x = peak_max, y = peak_norm_max + 0.02, shape = "Max peak"),
                     size = 2.0, show.legend = TRUE) +
          geom_point(data = tdata,
                     aes(x = peak_com, y = peak_norm_com + 0.02, shape = "Phase (COM)"),
                     size = 2.0, show.legend = TRUE) +
          coord_polar(start = 0) +
          scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(1, 13)) +
          scale_y_continuous(labels = NULL, breaks = NULL) +
          scale_colour_manual(values = PERIOD_COLORS, name = "Period") +
          scale_fill_manual(values  = PERIOD_COLORS, name = "Period") +
          scale_shape_manual(values = c("Max peak" = 16, "Phase (COM)" = 17), name = "Peak type") +
          theme_minimal() +
          theme(
               axis.title           = element_blank(),
               legend.position      = "bottom",
               panel.grid.major.x   = element_line(colour = "grey80", linetype = "dotted"),
               panel.grid.major.y   = element_line(colour = "grey90", linetype = "dashed"),
               plot.title           = element_text(face = "bold", size = 9, hjust = 0),
               plot.subtitle        = element_text(size = 7.5, colour = COL_MUTED),
               plot.title.position  = "plot"
          ) +
          labs(title = title_lab)
}

radar_list <- lapply(seq_len(nrow(data_class)), plot_radar)

panel_radar <- wrap_plots(radar_list, ncol = 6, guides = "collect")
panel_radar <- collect_guides_bottom(panel_radar)

# ============================================================
# Assemble: IRR heatmaps (top) / Radar charts (bottom)
# ============================================================

full_fig <- cowplot::plot_grid(cowplot::plot_grid(panel_irr_1, panel_irr_2, ncol = 2, rel_widths = c(1, 1.1)),
                               panel_radar,
                               ncol = 1, rel_heights = c(1, 3.2))

ggsave(file.path(out_dir, "fig4.pdf"),
       full_fig, width = 14, height = 16,
       device = cairo_pdf, family = "Times New Roman", limitsize = FALSE)

ggsave(file.path(out_dir, "fig4.png"),
       full_fig, width = 14, height = 14.5,
       dpi = 300, limitsize = FALSE)

message("fig4 saved to ", out_dir)
