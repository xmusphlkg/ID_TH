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
  mutate(label = paste0(int2col(id), ": ", Shortname))

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

fig_grid <- wrap_plots(fig_panels, ncol = 6, guides = "collect", axis_titles = "collect")

ggsave(file.path(out_dir, "fig3.pdf"),
       fig_grid, width = 14, height = 12,
       device = cairo_pdf, family = "Times New Roman", limitsize = FALSE)

ggsave(file.path(out_dir, "fig3.png"),
       fig_grid, width = 14, height = 12, dpi = 200, limitsize = FALSE)

# ---- Save figure data ---------------------------------------------------
data_outcome_list <- lapply(seq_along(outcome), function(x) outcome[[x]]$outcome_data)
data_outcome_list <- append(list(data_map), data_outcome_list)
data_outcome_list <- append(data_outcome_list, list(data_recovery_visual))
names(data_outcome_list) <- int2col(seq_along(data_outcome_list))
write.xlsx(data_outcome_list, file = file.path(out_dir, "fig3_data.xlsx"))

message("fig3 saved to ", out_dir)
