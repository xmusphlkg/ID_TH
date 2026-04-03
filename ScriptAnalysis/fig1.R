#####################################
## Fig 1 — Framework Architecture Diagram
## npj Digital Medicine submission
## Output: ../Outcome/Publish/npjDM/fig1.pdf | fig1.png
##
## Six-layer pipeline:
##   1. Data Sources
##   2. Digital Data Processing
##   3. Ensemble Forecasting Framework
##   4. Counterfactual Trajectory Generation
##   5. Analytical Modules (RP/BP + Seasonality)
##   6. Surveillance Dashboard
##
## Plus a right-column Validation Protocol box.
#####################################

library(ggplot2)
library(dplyr)
library(grid)
library(ggtext)
library(patchwork)

Sys.setlocale("LC_TIME", "C")

out_dir <- "../Outcome/Publish/npjDM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Colour palette (matches ShinyDashboard design language) --------
COL_TEAL   <- "#0A6762"
COL_CORAL  <- "#C96A43"
COL_GOLD   <- "#D89A2B"
COL_SLATE  <- "#22313F"
COL_SAND   <- "#F5EFE6"
COL_LINE   <- "#C1B8AD"
COL_MUTED  <- "#62707B"
COL_GREEN  <- "#2D8B81"
COL_AMBER  <- "#B87D1F"

# ---- Main pipeline boxes (left column) ----------------------------------
# Each layer: y-centre, height, fill, border, label, sublabel
layer_data <- tibble::tribble(
  ~yc,  ~h,   ~fill,       ~border,    ~label,                              ~sublabel,
  5.5,  0.75, "#EAF4F3",   COL_TEAL,   "DATA SOURCES",
    "Thailand Bureau of Epidemiology weekly surveillance\n43 notifiable diseases · 2008–2025 · ~1,000 disease-months",
  4.5,  0.75, "#FDF4E7",   COL_GOLD,   "DIGITAL DATA PROCESSING",
    "Weekly → monthly disaggregation (constrained splines)\nOverlap-year validation: Pearson r = 0.999 · MAPE = 3.9%",
  3.5,  0.75, "#EAF4F3",   COL_TEAL,   "ENSEMBLE FORECASTING FRAMEWORK",
    "6 model families: NNAR · ETS · SARIMA · TBATS · Hybrid · BSTS\n3-split rolling hold-out · z-standardised composite scoring",
  2.5,  0.75, "#FDF4E7",   COL_GOLD,   "COUNTERFACTUAL TRAJECTORY GENERATION",
    "Disease-adaptive best-model selected per rolling CV\n1 000 Monte Carlo paths · 80% & 95% predictive intervals",
  1.5,  0.75, "#F9EDE9",   COL_CORAL,  "ANALYTICAL MODULES",
    "RP/BP recovery classification  ·  Seasonal displacement detection\nDisruption magnitude profiling  ·  Resilience k-means clustering",
  0.5,  0.75, "#EAF4F3",   COL_GREEN,  "INTERACTIVE SURVEILLANCE DASHBOARD",
    "R Shiny · 5 modules · cached outputs · open-source\nhttps://lkg1116.shinyapps.io/TH_ID/"
)

# ---- Arrow connectors (x=0.5, between each pair of boxes) ---------------
arrow_ys <- c(5.12, 4.12, 3.12, 2.12, 1.12)

# ---- Validation protocol boxes (right column) ---------------------------
val_data <- tibble::tribble(
  ~yc,  ~h,   ~fill,      ~border,   ~label,                  ~sublabel,
  5.0,  0.55, "#FAFAF7",  COL_MUTED, "DATA VALIDATION",
    "Reconstructed vs. official monthly totals\n1 968 observations · Median error = 3 cases",
  3.5,  0.55, "#FAFAF7",  COL_MUTED, "MODEL SENSITIVITY",
    "Uniform ETS: 19/24 classifications preserved\nUniform SARIMA: 16/24 classifications preserved",
  2.0,  0.55, "#FAFAF7",  COL_MUTED, "THRESHOLD SENSITIVITY",
    "RP threshold: 90% vs 95% (2-, 3-, 4-month persistence)\nClassifications stable across all variants"
)

# ---- Build plot ----------------------------------------------------------
p <- ggplot() +
  # Background
  theme_void() +
  coord_cartesian(xlim = c(0, 10.5), ylim = c(0, 6.5), expand = FALSE) +
  theme(
    plot.background = element_rect(fill = COL_SAND, colour = NA),
    plot.margin = margin(14, 16, 12, 14)
  ) +

  # --- Title / kicker
  annotate("text", x = 0.15, y = 6.35,
           label = "FRAMEWORK ARCHITECTURE",
           hjust = 0, vjust = 1, size = 3.2, fontface = "bold",
           colour = COL_MUTED, family = "sans",
           letterSpacing = 1.5) +
  annotate("text", x = 0.15, y = 6.18,
           label = "Digital counterfactual surveillance pipeline",
           hjust = 0, vjust = 1, size = 5.6, fontface = "bold",
           colour = COL_SLATE, family = "sans") +

  # --- Section divider labels
  annotate("text", x = 0.15, y = 5.95, label = "Pipeline layers",
           hjust = 0, size = 3.2, colour = COL_MUTED, fontface = "italic") +
  annotate("text", x = 6.7, y = 5.95, label = "Validation protocol",
           hjust = 0, size = 3.2, colour = COL_MUTED, fontface = "italic")

# ---- Add layer boxes (left column, x: 0.1 – 6.1) -----------------------
for (i in seq_len(nrow(layer_data))) {
  row <- layer_data[i, ]
  ymin <- row$yc - row$h / 2
  ymax <- row$yc + row$h / 2

  p <- p +
    # Box fill
    annotate("rect",
             xmin = 0.1, xmax = 6.1,
             ymin = ymin, ymax = ymax,
             fill = row$fill, colour = row$border, linewidth = 0.8, alpha = 0.95) +
    # Layer number badge
    annotate("rect",
             xmin = 0.1, xmax = 0.65,
             ymin = ymin, ymax = ymax,
             fill = row$border, colour = NA, alpha = 0.9) +
    annotate("text", x = 0.38, y = row$yc,
             label = as.character(nrow(layer_data) - i + 1),
             hjust = 0.5, vjust = 0.5, size = 4.2, fontface = "bold",
             colour = "white") +
    # Label
    annotate("text", x = 0.82, y = row$yc + 0.13,
             label = row$label,
             hjust = 0, vjust = 0.5, size = 3.5, fontface = "bold",
             colour = COL_SLATE) +
    # Sublabel
    annotate("text", x = 0.82, y = row$yc - 0.13,
             label = row$sublabel,
             hjust = 0, vjust = 1, size = 2.9,
             colour = COL_MUTED)
}

# ---- Arrows between boxes -----------------------------------------------
for (ay in arrow_ys) {
  p <- p +
    annotate("segment",
             x = 3.1, xend = 3.1,
             y = ay, yend = ay - 0.10,
             colour = COL_LINE, linewidth = 1.0,
             arrow = arrow(length = unit(0.12, "inches"), type = "closed"))
}

# ---- Validation boxes (right column, x: 6.4 – 10.4) --------------------
for (i in seq_len(nrow(val_data))) {
  row <- val_data[i, ]
  ymin <- row$yc - row$h / 2
  ymax <- row$yc + row$h / 2

  p <- p +
    annotate("rect",
             xmin = 6.4, xmax = 10.4,
             ymin = ymin, ymax = ymax,
             fill = row$fill, colour = row$border, linewidth = 0.6,
             linetype = "dashed", alpha = 0.9) +
    annotate("text", x = 6.58, y = row$yc + 0.10,
             label = row$label,
             hjust = 0, vjust = 0.5, size = 3.2, fontface = "bold",
             colour = COL_SLATE) +
    annotate("text", x = 6.58, y = row$yc - 0.10,
             label = row$sublabel,
             hjust = 0, vjust = 1, size = 2.75,
             colour = COL_MUTED)
}

# ---- Connector: pipeline boxes to validation boxes ----------------------
connector_pairs <- list(
  c(5.0, 5.0),   # Data sources    -> Data validation
  c(3.5, 3.5),   # Ensemble        -> Model sensitivity
  c(2.5, 2.0)    # Counterfactual  -> Threshold sensitivity
)
for (pair in connector_pairs) {
  p <- p +
    annotate("segment",
             x = 6.1, xend = 6.4,
             y = pair[[1]], yend = pair[[2]],
             colour = COL_LINE, linewidth = 0.55, linetype = "dotted")
}

# ---- Legend strip at bottom ---------------------------------------------
legend_items <- tibble::tribble(
  ~x, ~label, ~fill,
  0.15, "Data flow",        COL_TEAL,
  1.8,  "Analysis modules", COL_CORAL,
  3.45, "Dashboard output", COL_GREEN,
  5.1,  "Validation check", COL_MUTED
)
for (i in seq_len(nrow(legend_items))) {
  li <- legend_items[i, ]
  p <- p +
    annotate("rect", xmin = li$x, xmax = li$x + 0.22, ymin = 0.06, ymax = 0.22,
             fill = li$fill, colour = NA, alpha = 0.85) +
    annotate("text", x = li$x + 0.30, y = 0.14, label = li$label,
             hjust = 0, vjust = 0.5, size = 2.95, colour = COL_SLATE)
}

# ---- Save ---------------------------------------------------------------
ggsave(file.path(out_dir, "fig1.pdf"),
       p, width = 10.5, height = 7, device = cairo_pdf)

ggsave(file.path(out_dir, "fig1.png"),
       p, width = 10.5, height = 7, dpi = 300)

message("fig1 saved to ", out_dir)
