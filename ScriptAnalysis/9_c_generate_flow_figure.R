#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(openxlsx)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)][1])
script_dir <- dirname(normalizePath(script_path))
project_root <- normalizePath(file.path(script_dir, ".."))

flow_path <- file.path(project_root, "Outcome", "Appendix", "Tables", "Disease_flow_summary.xlsx")
out_dir <- file.path(project_root, "Outcome", "Appendix", "Supplementary Appendix 1_0")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

flow_summary <- read.xlsx(flow_path, sheet = "FlowSummary")
excluded_43 <- read.xlsx(flow_path, sheet = "ExcludedFrom43")
excluded_24 <- read.xlsx(flow_path, sheet = "Included43_Not24")

count_stage <- function(label) {
  flow_summary$N[match(label, flow_summary$Stage)]
}

count_rows <- function(df, column, value) {
  sum(df[[column]] == value, na.rm = TRUE)
}

lookup_n <- function(df, label_col, n_col, value) {
  df[[n_col]][match(value, df[[label_col]])]
}

all_n <- count_stage("All monitored notifiable diseases")
desc_n <- count_stage("Included in descriptive 43-disease analysis")
model_n <- count_stage("Included in 24-disease counterfactual analysis")
excluded_desc_n <- all_n - desc_n
excluded_model_n <- desc_n - model_n

boxes <- data.frame(
  xmin = c(-1.7, -1.7, -5.4, -1.7, -5.4, 2.0),
  xmax = c(1.7, 1.7, -1.9, 1.7, -1.9, 5.4),
  ymin = c(5.2, 3.5, 3.2, 1.3, 0.9, 0.9),
  ymax = c(6.2, 4.5, 4.8, 2.3, 2.7, 2.7),
  fill = c("#DDEBDD", "#DDEBDD", "#F8E2D7", "#DDEBDD", "#F8E2D7", "#E4ECF8"),
  label = c(
    sprintf("%d monitored\nnotifiable disease series", all_n),
    sprintf("%d retained for descriptive\nportfolio analysis", desc_n),
    paste0(
      excluded_desc_n,
      " excluded before\ndescriptive analysis\n",
      lookup_n(excluded_43, "Label", "N", "Duplication"), " overlapping categories\n",
      lookup_n(excluded_43, "Label", "N", "Uninfectious disease"), " outside infectious scope\n",
      lookup_n(excluded_43, "Label", "N", "Unspecifed disease"), " residual or ill-defined\n",
      lookup_n(excluded_43, "Label", "N", "No cases"), " zero-incidence\n",
      lookup_n(excluded_43, "Label", "N", "Unreported in 2025"), " incomplete recent reporting\n",
      lookup_n(excluded_43, "Label", "N", "Shifting in surveillance"), " surveillance-definition shift"
    ),
    sprintf("%d retained for\ncounterfactual modelling", model_n),
    paste0(
      excluded_model_n,
      " excluded from\ncounterfactual modelling\n",
      count_rows(excluded_24, "Reason.for.descriptive-only.retention", "Insufficient cases"), " insufficient long-horizon signal\n",
      count_rows(excluded_24, "Reason.for.descriptive-only.retention", "Non-seasonal trend"), " non-seasonal or weak seasonality\n",
      count_rows(excluded_24, "Reason.for.descriptive-only.retention", "Insufficient duration"), " insufficient prepandemic duration\n",
      count_rows(excluded_24, "Reason.for.descriptive-only.retention", "Unspecifed disease"), " residual or unspecified definitions"
    ),
    paste0(
      "Eligibility criteria\n",
      "144 prepandemic months\n",
      "stable surveillance definition\n",
      "sufficient signal for\n72-month extrapolation\n",
      "not dominated by structural zeros"
    )
  ),
  stringsAsFactors = FALSE
)

arrows <- data.frame(
  x = c(0, -1.7, 0, -1.7, 1.7),
  y = c(5.2, 4.0, 3.5, 1.8, 1.8),
  xend = c(0, -1.9, 0, -1.9, 2.0),
  yend = c(4.5, 4.0, 2.3, 1.8, 1.8)
)

p <- ggplot() +
  geom_curve(
    data = arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0,
    arrow = arrow(length = grid::unit(0.18, "inches"), type = "closed"),
    linewidth = 0.7,
    colour = "#4A5568"
  ) +
  geom_rect(
    data = boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = boxes$fill,
    colour = "#4A5568",
    linewidth = 0.7
  ) +
  geom_text(
    data = transform(boxes, x = (xmin + xmax) / 2, y = (ymin + ymax) / 2),
    aes(x = x, y = y, label = label),
    family = "sans",
    lineheight = 1.05,
    size = 2.95
  ) +
  annotate(
    "text",
    x = 0,
    y = 6.35,
    label = "Two-stage disease selection flow for the Thailand surveillance portfolio",
    fontface = "bold",
    size = 5
  ) +
  coord_cartesian(xlim = c(-5.6, 5.6), ylim = c(0.7, 6.8), clip = "off") +
  theme_void()

ggsave(
  filename = file.path(out_dir, "flow_diagram.png"),
  plot = p,
  width = 11,
  height = 8,
  dpi = 320,
  bg = "white"
)
