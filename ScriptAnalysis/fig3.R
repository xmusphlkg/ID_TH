#####################################
## Fig 3 — Dual-Metric Recovery Classification
## npj Digital Medicine submission
## Output: ../Outcome/Publish/npjDM/fig3.pdf | fig3.png
##
## Panel A: 4-quadrant summary tile (RP/BP status distribution)
## Panels B–Y: 24 disease-specific trajectory plots
##   Red line  = observed monthly cases
##   Teal line = counterfactual median forecast + 95% interval ribbon
##   Blue band = recovery period (disruption onset → RP)
##   Gold band = balance period (RP → BP)
##   Green/red area fill = cumulative surplus / deficit
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
COL_BLUE  <- "#004F7AFF"
COL_GOLD  <- "#F3C558FF"
COL_SLATE <- "#22313F"
COL_MUTED <- "#62707B"

# ============================================================
# Data
# ============================================================

load("./temp/month.RData")
load("./temp/outcome.RData")
load("./temp/best_model_figure.RData")

data_class <- data_class |>
  mutate(label = paste0(int2col(id + 1), ": ", Shortname))

# Recovery metrics
df_metrics <- calculate_disease_metrics(outcome)

df_display <- df_metrics |>
  mutate(
    Trough_Label    = format(Date_Trough, "%Y-%m"),
    Recovery_Label  = format(Date_Recovery, "%Y-%m"),
    Balance_Label   = format(Date_Balance, "%Y-%m"),
    Recovery_Label  = if_else(is.na(Recovery_Label), "Not recovered", Recovery_Label),
    Balance_Label   = if_else(is.na(Balance_Label),  "Not balanced",  Balance_Label),
    Recovery_Period = lubridate::interval(Date_Start_Deficit, Date_Recovery) %/% months(1),
    Balance_Period  = lubridate::interval(Date_Start_Deficit, Date_Balance)  %/% months(1)
  )

data_recovery_visual <- df_display |>
  select(
    Shortname, Status,
    StartDate       = Date_Start_Deficit,
    Recovery_Date   = Date_Recovery,
    Balance_Date    = Date_Balance,
    Recovery_Period,
    Balance_Period
  ) |>
  mutate(StartDate = if_else(is.na(StartDate),
                             as.Date(min(StartDate, na.rm = TRUE)),
                             as.Date(StartDate))) |>
  pivot_longer(
    cols      = c(Recovery_Date, Balance_Date, Recovery_Period, Balance_Period),
    names_to  = c("type", ".value"),
    names_sep = "_"
  ) |>
  select(Shortname, StartDate, Status, type, EndDate = Date, Period) |>
  mutate(
    EndDate = case_when(
      is.na(EndDate) & type %in% c("Balance", "Recovery") ~ as.Date(max(data_month$Date)),
      TRUE ~ as.Date(EndDate)
    ),
    Period = case_when(
      is.na(Period) & type == "Balance"  & Status != "No Deficit" ~ "Not balanced",
      is.na(Period) & type == "Recovery" & Status != "No Deficit" ~ "Not recovered",
      Status == "No Deficit" ~ "No deficit",
      TRUE ~ paste0(as.character(Period), "m")
    )
  )

# ============================================================
# Panel A — Summary classification tile
# ============================================================

status_counts <- df_metrics |>
  left_join(data_class |> select(Shortname, Group), by = "Shortname") |>
  count(Group, Status) |>
  complete(
    Group  = disease_groups,
    Status = c("Debt Repaid", "Recovered", "Suppressed", "No Deficit"),
    fill   = list(n = 0)
  ) |>
  mutate(
    Group  = factor(Group, levels = disease_groups),
    Status = factor(Status, levels = c("Debt Repaid", "Recovered", "Suppressed", "No Deficit"))
  )

status_palette <- c(
  "Debt Repaid" = "#0D5D56",
  "Recovered"   = "#D89A2B",
  "Suppressed"  = "#BE4C3A",
  "No Deficit"  = "#4C6A92"
)

panel_A <- ggplot(status_counts, aes(x = Status, y = fct_rev(Group), fill = Status, label = ifelse(n > 0, n, ""))) +
  geom_tile(colour = "white", linewidth = 0.9, width = 0.92, height = 0.88, aes(alpha = n)) +
  geom_text(colour = "white", fontface = "bold", size = 4.2) +
  scale_fill_manual(values = status_palette, guide = "none") +
  scale_alpha_continuous(range = c(0.25, 0.95), guide = "none") +
  theme_bw() +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y     = element_text(size = 9),
    plot.title      = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot",
    plot.margin     = margin(5, 10, 5, 5)
  ) +
  labs(title = "A", x = NULL, y = NULL,
       subtitle = "Recovery classification by disease group and status")

# full status breakdown as horizontal bar (right side of A)
status_total <- df_metrics |>
  count(Status) |>
  complete(Status = c("Debt Repaid", "Recovered", "Suppressed", "No Deficit"), fill = list(n = 0)) |>
  mutate(Status = factor(Status, levels = c("No Deficit", "Suppressed", "Recovered", "Debt Repaid")))

panel_A_bar <- ggplot(status_total, aes(x = n, y = Status, fill = Status)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 4, fontface = "bold") +
  scale_fill_manual(values = status_palette, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  theme(
    panel.grid     = element_blank(),
    axis.text.y    = element_text(size = 9.5),
    plot.title     = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot",
    plot.margin    = margin(5, 10, 5, 5)
  ) +
  labs(title = NULL, x = "Number of diseases", y = NULL)

panel_A_combined <- (panel_A | panel_A_bar) + plot_layout(widths = c(2, 1))

# ============================================================
# Panels B–Y — 24 disease trajectory panels
# ============================================================

fig_panels <- lapply(
  seq_len(nrow(data_class)),
  plot_single_panel,
  outcome          = outcome,
  recovery         = data_recovery_visual,
  display_recovery = TRUE,
  titles           = data_class$label
)

fig_grid <- wrap_plots(fig_panels, ncol = 6, guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

# ============================================================
# Assemble  A / B-Y
# ============================================================

full_fig <- panel_A_combined /
  fig_grid +
  plot_layout(heights = c(1, 8)) +
  plot_annotation(
    title   = "Dual-metric recovery classification of 24 infectious diseases",
    caption = "Panel A: summary of recovery phenotype assignments by disease group.\nPanels B–Y: monthly observed (red) vs. counterfactual forecast (teal) with 95% predictive intervals.\nBlue bars = recovery period (disruption onset to RP); gold bars = balance period (RP to BP).\nFill areas show cumulative surplus (green) or deficit (red) relative to counterfactual.",
    theme = theme(
      plot.title   = element_text(face = "bold", size = 14, hjust = 0),
      plot.caption = element_text(size = 8.5, colour = COL_MUTED, hjust = 0)
    )
  )

ggsave(file.path(out_dir, "fig3.pdf"),
       full_fig, width = 20, height = 26,
       device = cairo_pdf, family = "Times New Roman", limitsize = FALSE)

ggsave(file.path(out_dir, "fig3.png"),
       full_fig, width = 20, height = 26, dpi = 200, limitsize = FALSE)

# ---- Save figure data ---------------------------------------------------
data_outcome_list <- lapply(seq_along(outcome), function(x) outcome[[x]]$outcome_data)
data_outcome_list <- append(list(data_map), data_outcome_list)
data_outcome_list <- append(data_outcome_list, list(data_recovery_visual))
names(data_outcome_list) <- int2col(seq_along(data_outcome_list))
write.xlsx(data_outcome_list, file = file.path(out_dir, "fig3_data.xlsx"))

message("fig3 saved to ", out_dir)
