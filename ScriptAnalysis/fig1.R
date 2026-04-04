#####################################
## Fig 1 - Framework Architecture Diagram
## npj Digital Medicine submission
## Output: ../Outcome/Publish/npjDM/fig1.pdf | fig1.png
##
## Six-layer pipeline:
##   1. Data Sources
##   2. Digital Data Processing
##   3. Ensemble Forecasting Framework
##   4. Counterfactual Trajectory Generation
##   5. Analytical Modules (RP/BP + seasonality + context)
##   6. Surveillance Dashboard / Prioritization
##
## Plus a right-column robustness / deployment panel.
#####################################

library(ggplot2)
library(dplyr)
library(grid)

Sys.setlocale("LC_TIME", "C")

out_dir <- "../Outcome/Publish/npjDM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

COL_TEAL <- "#0A6762"
COL_CORAL <- "#C96A43"
COL_GOLD <- "#D89A2B"
COL_SLATE <- "#22313F"
COL_SAND <- "#F5EFE6"
COL_LINE <- "#C1B8AD"
COL_MUTED <- "#62707B"
COL_GREEN <- "#2D8B81"

layer_data <- tibble::tribble(
  ~yc, ~h, ~fill, ~border, ~label, ~sublabel,
  5.5, 0.75, "#EAF4F3", COL_TEAL, "DATA SOURCES",
  "Thailand national infectious disease surveillance\n43 retained diseases, 2008-2025, national monthly analytical cache",
  4.5, 0.75, "#FDF4E7", COL_GOLD, "DIGITAL DATA PROCESSING",
  "Weekly-to-monthly disaggregation with overlap-year validation\nPearson r = 0.999 and median absolute percentage error = 3.9%",
  3.5, 0.75, "#EAF4F3", COL_TEAL, "ENSEMBLE FORECASTING FRAMEWORK",
  "Six model families, rolling hold-out validation, composite scoring\nAlternative selection rules retained the primary model for 18/24 to 21/24 diseases",
  2.5, 0.75, "#FDF4E7", COL_GOLD, "COUNTERFACTUAL TRAJECTORY GENERATION",
  "Disease-adaptive best model refit on 2008-2019 baseline\n1,000 simulated forecast paths with 80% and 95% predictive intervals",
  1.5, 0.75, "#F9EDE9", COL_CORAL, "ANALYTICAL MODULES",
  "RP/BP classification, alternative endpoint checks, uncertainty propagation\nSeasonal displacement, contextual triangulation, and operational synthesis",
  0.5, 0.75, "#EAF4F3", COL_GREEN, "INTERACTIVE REVIEW LAYER",
  "Overview, recovery, prioritization, time series, seasonality, and reference tabs\nCached outputs support decision review without live model refitting"
)

arrow_ys <- c(5.12, 4.12, 3.12, 2.12, 1.12)

val_data <- tibble::tribble(
  ~yc, ~h, ~fill, ~border, ~label, ~sublabel,
  5.0, 0.55, "#FAFAF7", COL_MUTED, "DATA VALIDATION",
  "Reconstructed versus official monthly totals\n1,968 overlap observations; median absolute error = 3 cases",
  3.5, 0.55, "#FAFAF7", COL_MUTED, "ROBUSTNESS CHECKS",
  "Alternative model rules matched for 18/24 to 21/24 diseases\nUniform ETS preserved 19/24 and uniform SARIMA preserved 16/24 classifications",
  2.0, 0.55, "#FAFAF7", COL_MUTED, "ENDPOINT AND WORKFLOW CHECKS",
  "Threshold and alternative endpoint sensitivity assessed\nInterruption stable at 2020-03; only 2 diseases changed at 2020-04"
)

p <- ggplot() +
  theme_void() +
  coord_cartesian(xlim = c(0, 10.5), ylim = c(0, 6.5), expand = FALSE) +
  theme(
    plot.background = element_rect(fill = COL_SAND, colour = NA),
    plot.margin = margin(14, 16, 12, 14)
  ) +
  annotate(
    "text",
    x = 0.15, y = 6.35,
    label = "FRAMEWORK ARCHITECTURE",
    hjust = 0, vjust = 1, size = 3.2, fontface = "bold",
    colour = COL_MUTED
  ) +
  annotate(
    "text",
    x = 0.15, y = 6.18,
    label = "Digital counterfactual surveillance pipeline",
    hjust = 0, vjust = 1, size = 5.6, fontface = "bold",
    colour = COL_SLATE
  ) +
  annotate(
    "text",
    x = 0.15, y = 5.95, label = "Pipeline layers",
    hjust = 0, size = 3.2, colour = COL_MUTED, fontface = "italic"
  ) +
  annotate(
    "text",
    x = 6.65, y = 5.95, label = "Robustness and deployment checks",
    hjust = 0, size = 3.2, colour = COL_MUTED, fontface = "italic"
  )

for (i in seq_len(nrow(layer_data))) {
  row <- layer_data[i, ]
  ymin <- row$yc - row$h / 2
  ymax <- row$yc + row$h / 2

  p <- p +
    annotate(
      "rect",
      xmin = 0.1, xmax = 6.1,
      ymin = ymin, ymax = ymax,
      fill = row$fill, colour = row$border, linewidth = 0.8, alpha = 0.95
    ) +
    annotate(
      "rect",
      xmin = 0.1, xmax = 0.65,
      ymin = ymin, ymax = ymax,
      fill = row$border, colour = NA, alpha = 0.9
    ) +
    annotate(
      "text",
      x = 0.38, y = row$yc,
      label = as.character(nrow(layer_data) - i + 1),
      hjust = 0.5, vjust = 0.5, size = 4.2, fontface = "bold",
      colour = "white"
    ) +
    annotate(
      "text",
      x = 0.82, y = row$yc + 0.13,
      label = row$label,
      hjust = 0, vjust = 0.5, size = 3.5, fontface = "bold",
      colour = COL_SLATE
    ) +
    annotate(
      "text",
      x = 0.82, y = row$yc - 0.13,
      label = row$sublabel,
      hjust = 0, vjust = 1, size = 2.85,
      colour = COL_MUTED
    )
}

for (ay in arrow_ys) {
  p <- p +
    annotate(
      "segment",
      x = 3.1, xend = 3.1,
      y = ay, yend = ay - 0.10,
      colour = COL_LINE, linewidth = 1.0,
      arrow = arrow(length = unit(0.12, "inches"), type = "closed")
    )
}

for (i in seq_len(nrow(val_data))) {
  row <- val_data[i, ]
  ymin <- row$yc - row$h / 2
  ymax <- row$yc + row$h / 2

  p <- p +
    annotate(
      "rect",
      xmin = 6.4, xmax = 10.4,
      ymin = ymin, ymax = ymax,
      fill = row$fill, colour = row$border, linewidth = 0.6,
      linetype = "dashed", alpha = 0.9
    ) +
    annotate(
      "text",
      x = 6.58, y = row$yc + 0.10,
      label = row$label,
      hjust = 0, vjust = 0.5, size = 3.1, fontface = "bold",
      colour = COL_SLATE
    ) +
    annotate(
      "text",
      x = 6.58, y = row$yc - 0.10,
      label = row$sublabel,
      hjust = 0, vjust = 1, size = 2.7,
      colour = COL_MUTED
    )
}

connector_pairs <- list(
  c(5.0, 5.0),
  c(3.5, 3.5),
  c(2.5, 2.0)
)

for (pair in connector_pairs) {
  p <- p +
    annotate(
      "segment",
      x = 6.1, xend = 6.4,
      y = pair[[1]], yend = pair[[2]],
      colour = COL_LINE, linewidth = 0.55, linetype = "dotted"
    )
}

legend_items <- tibble::tribble(
  ~x, ~label, ~fill,
  0.15, "Data flow", COL_TEAL,
  1.8, "Analysis modules", COL_CORAL,
  3.55, "Interactive review", COL_GREEN,
  5.25, "Robustness checks", COL_MUTED
)

for (i in seq_len(nrow(legend_items))) {
  li <- legend_items[i, ]
  p <- p +
    annotate(
      "rect",
      xmin = li$x, xmax = li$x + 0.22, ymin = 0.06, ymax = 0.22,
      fill = li$fill, colour = NA, alpha = 0.85
    ) +
    annotate(
      "text",
      x = li$x + 0.30, y = 0.14, label = li$label,
      hjust = 0, vjust = 0.5, size = 2.95, colour = COL_SLATE
    )
}

ggsave(file.path(out_dir, "fig1.pdf"), p, width = 10.5, height = 7, device = cairo_pdf)
ggsave(file.path(out_dir, "fig1.png"), p, width = 10.5, height = 7, dpi = 300)

message("fig1 saved to ", out_dir)
