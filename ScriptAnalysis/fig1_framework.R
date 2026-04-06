# ==============================================================================
# Figure 1: Framework Architecture Schematic
# Digital Surveillance Framework Pipeline Diagram
# ==============================================================================

library(ggplot2)
library(grid)
library(gridExtra)

# --- Helper: Rounded rectangle grob ---
roundrect_grob <- function(x, y, w, h, r = 0.02, fill = "white", col = "black", lwd = 1) {
  grid.roundrect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(w, "npc"), height = unit(h, "npc"),
    r = unit(r, "npc"),
    gp = gpar(fill = fill, col = col, lwd = lwd)
  )
}

# --- Color Palette ---
col_data     <- "#4DBBD5FF"   # Data layer (blue)
col_model    <- "#E64B35FF"   # Model layer (red)
col_metric   <- "#3C5488FF"   # Metric layer (dark blue)
col_seasonal <- "#91D1C2FF"   # Seasonal module (teal)
col_dash     <- "#F39B7FFF"   # Dashboard (salmon)
col_arrow    <- "#333333"
col_bg       <- "white"

# --- Build figure using ggplot + annotation ---
fig1 <- ggplot() +
  xlim(0, 10) + ylim(0, 8) +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10)) +

  # === LAYER 1: Data Sources (top) ===
  annotate("rect", xmin = 0.3, xmax = 3.2, ymin = 7.0, ymax = 7.8,
           fill = col_data, alpha = 0.3, color = col_data, linewidth = 1) +
  annotate("text", x = 1.75, y = 7.6, label = "Data Sources", fontface = "bold", size = 4) +
  annotate("text", x = 1.75, y = 7.25, size = 3, lineheight = 0.9,
           label = "Thailand Bureau of Epidemiology\n72 notifiable diseases (2008-2025)\nWeekly case reports") +

  # === LAYER 2: Data Processing Pipeline ===
  annotate("rect", xmin = 0.3, xmax = 3.2, ymin = 5.5, ymax = 6.6,
           fill = col_data, alpha = 0.15, color = col_data, linewidth = 1) +
  annotate("text", x = 1.75, y = 6.35, label = "Digital Data Processing", fontface = "bold", size = 3.5) +
  annotate("text", x = 1.75, y = 5.9, size = 2.8, lineheight = 0.9,
           label = "Weekly-to-monthly reconstruction\nDisease screening (43 descriptive, 24 modelled)\nSquare-root transformation + validation (r = 0.999)") +

  # Arrow: Data → Processing

  annotate("segment", x = 1.75, xend = 1.75, y = 7.0, yend = 6.65,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.8) +

  # === LAYER 3: Ensemble Forecasting Framework (center, wide) ===
  annotate("rect", xmin = 0.3, xmax = 9.7, ymin = 3.8, ymax = 5.2,
           fill = col_model, alpha = 0.1, color = col_model, linewidth = 1.2) +
  annotate("text", x = 5.0, y = 5.0, label = "Ensemble Forecasting Framework",
           fontface = "bold", size = 4.5) +

  # 6 model boxes inside
  annotate("rect", xmin = 0.5, xmax = 2.0, ymin = 4.0, ymax = 4.6,
           fill = "#FDDBC7", color = col_model, linewidth = 0.6) +
  annotate("text", x = 1.25, y = 4.3, label = "NNAR", size = 3, fontface = "bold") +

  annotate("rect", xmin = 2.15, xmax = 3.65, ymin = 4.0, ymax = 4.6,
           fill = "#FDDBC7", color = col_model, linewidth = 0.6) +
  annotate("text", x = 2.9, y = 4.3, label = "ETS", size = 3, fontface = "bold") +

  annotate("rect", xmin = 3.8, xmax = 5.3, ymin = 4.0, ymax = 4.6,
           fill = "#FDDBC7", color = col_model, linewidth = 0.6) +
  annotate("text", x = 4.55, y = 4.3, label = "SARIMA", size = 3, fontface = "bold") +

  annotate("rect", xmin = 5.45, xmax = 6.95, ymin = 4.0, ymax = 4.6,
           fill = "#FDDBC7", color = col_model, linewidth = 0.6) +
  annotate("text", x = 6.2, y = 4.3, label = "TBATS", size = 3, fontface = "bold") +

  annotate("rect", xmin = 7.1, xmax = 8.6, ymin = 4.0, ymax = 4.6,
           fill = "#FDDBC7", color = col_model, linewidth = 0.6) +
  annotate("text", x = 7.85, y = 4.3, label = "Hybrid", size = 3, fontface = "bold") +

  annotate("rect", xmin = 8.75, xmax = 9.6, ymin = 4.0, ymax = 4.6,
           fill = "#FDDBC7", color = col_model, linewidth = 0.6) +
  annotate("text", x = 9.175, y = 4.3, label = "BSTS", size = 3, fontface = "bold") +

  # Model selection annotation
  annotate("text", x = 5.0, y = 4.75, size = 2.8, color = "grey30",
           label = "Rolling hold-out CV (3 splits) \u2192 z-standardized composite (sMAPE + RMSE + MASE) \u2192 Disease-specific best model") +

  # Arrow: Processing → Ensemble
  annotate("segment", x = 1.75, xend = 1.75, y = 5.5, yend = 5.25,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.8) +

  # === LAYER 4: Counterfactual Forecasting ===
  annotate("rect", xmin = 3.0, xmax = 7.0, ymin = 2.6, ymax = 3.5,
           fill = col_model, alpha = 0.15, color = col_model, linewidth = 1) +
  annotate("text", x = 5.0, y = 3.3, label = "Counterfactual Forecasting",
           fontface = "bold", size = 3.5) +
  annotate("text", x = 5.0, y = 2.9, size = 2.8, lineheight = 0.9,
           label = "5000 Monte Carlo trajectories per disease\nMonthly medians + 95% predictive intervals (Jan 2020 \u2013 Dec 2025)") +

  # Arrow: Ensemble → Counterfactual
  annotate("segment", x = 5.0, xend = 5.0, y = 3.8, yend = 3.55,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.8) +

  # === LAYER 5: Two output branches ===

  # --- Branch A: RP/BP Classification (left) ---
  annotate("rect", xmin = 0.3, xmax = 4.3, ymin = 0.8, ymax = 2.2,
           fill = col_metric, alpha = 0.15, color = col_metric, linewidth = 1) +
  annotate("text", x = 2.3, y = 2.0, label = "RP/BP Classification Algorithm",
           fontface = "bold", size = 3.5, color = col_metric) +
  annotate("text", x = 2.3, y = 1.5, size = 2.8, lineheight = 0.9,
           label = "RP: O\u209C \u2265 0.95 \u00D7 E\u209C for 3 months + C\u209C non-decreasing\nBP: first month after trough where C\u209C \u2265 0\n\u2192 4 recovery phenotypes per disease") +

  # --- Branch B: Seasonal Analysis (right) ---
  annotate("rect", xmin = 5.7, xmax = 9.7, ymin = 0.8, ymax = 2.2,
           fill = col_seasonal, alpha = 0.3, color = col_seasonal, linewidth = 1) +
  annotate("text", x = 7.7, y = 2.0, label = "Seasonal Analysis Module",
           fontface = "bold", size = 3.5, color = "#2E7D5B") +
  annotate("text", x = 7.7, y = 1.5, size = 2.8, lineheight = 0.9,
           label = "Circular center-of-mass phase shifts\nAmplitude ratio comparison\nPre-pandemic vs post-PHSM observed vs counterfactual") +

  # Arrows: Counterfactual → RP/BP and → Seasonal
  annotate("segment", x = 4.0, xend = 2.3, y = 2.6, yend = 2.25,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.8) +
  annotate("segment", x = 6.0, xend = 7.7, y = 2.6, yend = 2.25,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.8) +

  # === LAYER 6: Dashboard (bottom) ===
  annotate("rect", xmin = 1.5, xmax = 8.5, ymin = 0.0, ymax = 0.6,
           fill = col_dash, alpha = 0.3, color = "#D35400", linewidth = 1.2) +
  annotate("text", x = 5.0, y = 0.3,
           label = "Interactive Shiny Dashboard  \u2022  Overview  \u2022  Recovery  \u2022  Time Series  \u2022  Seasonality  \u2022  Reference",
           fontface = "bold", size = 3, color = "#D35400") +

  # Arrows: RP/BP → Dashboard and Seasonal → Dashboard
  annotate("segment", x = 2.3, xend = 3.5, y = 0.8, yend = 0.65,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.6) +
  annotate("segment", x = 7.7, xend = 6.5, y = 0.8, yend = 0.65,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.6) +

  # === Side annotation: Training / Forecast periods ===
  annotate("rect", xmin = 4.0, xmax = 9.7, ymin = 5.5, ymax = 6.6,
           fill = "grey95", color = "grey60", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = 6.85, y = 6.35, label = "Validation Protocol", fontface = "bold", size = 3.5) +
  annotate("text", x = 6.85, y = 5.9, size = 2.8, lineheight = 0.9,
           label = "Overlap-year validation (2020-2023): r = 0.999, MAPE = 3.9%\nSensitivity: uniform ETS / SARIMA vs best-model\nThreshold sensitivity: 90-95%, persistence 2-4 months") +

  # Arrow: Processing → Validation
  annotate("segment", x = 3.2, xend = 4.0, y = 6.05, yend = 6.05,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = col_arrow, linewidth = 0.6)

# Save
ggsave("../Outcome/Publish/fig1_framework.pdf", fig1, width = 12, height = 9, dpi = 300)
ggsave("../Outcome/Publish/fig1_framework.png", fig1, width = 12, height = 9, dpi = 300)

cat("Figure 1 (Framework Architecture) saved.\n")
