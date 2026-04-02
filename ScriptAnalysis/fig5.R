#####################################
## Fig 5 — Disruption Magnitude Profiling and Recovery Phenotyping
## npj Digital Medicine submission
## Output: ../Outcome/Publish/npjDM/fig5.pdf | fig5.png
##
## Panel A: Alluvial / Rawgraphs placeholder — absolute deficit by disease
##           (generated externally via rawgraphs.io; placeholder ggplot shown)
## Panel B: Rebound intensity (left bar) + relative suppression % (right bar)
##          by disease, colour-coded by transmission group
## Panel C: Bubble scatter — recovery period (months) vs rebound intensity
##          Bubble size = deficit depth; GAM smooth + Spearman ρ annotation
## Panel D: Resilience clustering — relative suppression (x) vs rebound (y)
##          k-means convex hulls with cluster phenotype labels
#####################################

library(tidyverse)
library(patchwork)
library(ggrepel)
library(factoextra)
library(openxlsx)
library(mgcv)
library(cluster)
library(ggpubr)

Sys.setlocale("LC_TIME", "C")
remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

out_dir <- "../Outcome/Publish/npjDM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Design palette -----
COL_TEAL  <- "#0A6762"
COL_CORAL <- "#C96A43"
COL_SLATE <- "#22313F"
COL_MUTED <- "#62707B"
COL_SAND  <- "#F5EFE6"

# ============================================================
# Data
# ============================================================

load("./temp/month.RData")
load("./temp/outcome.RData")

df_metrics <- calculate_disease_metrics(outcome)

data_class <- data_class |>
  filter(Shortname %in% df_metrics$Shortname)

names(fill_color) <- levels(data_class$Group)

df_metrics <- df_metrics |>
  left_join(data_class |> select(Shortname, Group), by = "Shortname") |>
  mutate(
    Shortname         = if_else(Shortname == "CA", "CA (HPV)", Shortname),
    Relative_Deficit  = abs(Relative_Deficit),
    Max_Deficit_Raw   = abs(Max_Deficit_Raw),
    Suppression_Months = lubridate::interval(Date_Start_Deficit, Date_Recovery) %/% months(1)
  ) |>
  filter(Max_Deficit_Raw > 0)

# ============================================================
# Panel A — Absolute deficit placeholder
# (Panel A raw chart: horizontal bar ordered by deficit depth)
# ============================================================

data_fig_a <- df_metrics |>
  select(Shortname, Group, Max_Deficit_Raw) |>
  arrange(desc(Max_Deficit_Raw)) |>
  mutate(Shortname = factor(Shortname, levels = Shortname))

panel_A <- ggplot(data_fig_a, aes(x = Max_Deficit_Raw, y = Shortname, fill = Group)) +
  geom_col(width = 0.72) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ","),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = fill_color, name = "Group") +
  theme_bw() +
  theme(
    legend.position     = "bottom",
    legend.title.position = "top",
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    plot.title          = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot",
    axis.text.y         = element_text(size = 9)
  ) +
  labs(
    title    = "A",
    x        = "Cumulative absolute deficit (cases)",
    y        = NULL,
    subtitle = "Total case shortfall relative to counterfactual expectation"
  )

# ============================================================
# Panel B — Rebound intensity + relative suppression side bars
# ============================================================

data_fig_b <- df_metrics |>
  select(Shortname, Group, Relative_Deficit, Rebound_Intensity) |>
  mutate(Relative_Deficit_pct = Relative_Deficit * 100)

disease_order <- rev(unique(data_class$Shortname[data_class$Shortname %in% data_fig_b$Shortname]))
# Use group-sorted order from data_class
disease_order <- rev(data_class$Shortname[data_class$Shortname %in% data_fig_b$Shortname])

panel_B1 <- ggplot(data_fig_b, aes(x = Rebound_Intensity, y = Shortname, fill = Group)) +
  geom_col(width = 0.7, show.legend = TRUE) +
  scale_x_continuous(
    limits = rev(range(pretty(data_fig_b$Rebound_Intensity))),
    trans  = "reverse",
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = rev(disease_order), position = "right") +
  scale_fill_manual(values = fill_color, guide = "none") +
  labs(title = "B", x = "Rebound intensity", y = NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(5, 0, 5, 5),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position  = "bottom",
    legend.title.position = "top",
    plot.title       = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot"
  )

panel_B2 <- ggplot(data_fig_b, aes(x = Relative_Deficit_pct, y = Shortname, fill = Group)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::percent_format(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(limits = rev(disease_order)) +
  scale_fill_manual(values = fill_color) +
  labs(x = "Relative suppression (%)", y = NULL) +
  theme_bw() +
  theme(
    panel.grid   = element_blank(),
    plot.margin  = margin(5, 10, 5, 0),
    axis.text.y  = element_text(size = 9, hjust = 0.5),
    legend.position  = "bottom"
  )

panel_B <- panel_B1 + panel_B2 +
  plot_layout(nrow = 1, widths = c(1, 1), guides = "collect") &
  theme(legend.position = "bottom", legend.title.position = "top")

# ============================================================
# Panel C — Bubble scatter: RP months vs rebound intensity
# ============================================================

data_fig_c <- df_metrics |>
  filter(!is.na(Rebound_Intensity), Rebound_Intensity > 1, !is.na(Suppression_Months)) |>
  mutate(Max_Deficit_Raw = abs(Max_Deficit_Raw)) |>
  select(Shortname, Group, Suppression_Months, Rebound_Intensity, Max_Deficit_Raw)

cor_test <- cor.test(data_fig_c$Suppression_Months,
                     data_fig_c$Rebound_Intensity,
                     method = "spearman")

r_val <- formatC(as.numeric(cor_test$estimate), format = "f", digits = 2)
p_val <- signif(cor_test$p.value, 2)
stats_label <- bquote(italic(rho) == .(r_val) ~ "," ~ italic(P) == .(p_val))

gam_res  <- mgcv::gam(Rebound_Intensity ~ s(Suppression_Months), data = data_fig_c)
new_grid <- data.frame(Suppression_Months = seq(
  min(data_fig_c$Suppression_Months, na.rm = TRUE),
  max(data_fig_c$Suppression_Months, na.rm = TRUE),
  length.out = 200
))
gam_pred       <- predict(gam_res, newdata = new_grid, se.fit = TRUE)
new_grid$pred  <- gam_pred$fit
new_grid$upper <- gam_pred$fit + 2 * gam_pred$se.fit
new_grid$lower <- gam_pred$fit - 2 * gam_pred$se.fit

pal_size_breaks <- pretty(range(data_fig_c$Max_Deficit_Raw), n = 5)

panel_C <- ggplot(data_fig_c, aes(x = Suppression_Months, y = Rebound_Intensity)) +
  geom_ribbon(data = new_grid,
              aes(x = Suppression_Months, ymin = lower, ymax = upper),
              inherit.aes = FALSE, fill = "#DDEBF7", alpha = 0.55) +
  geom_line(data = new_grid,
            aes(x = Suppression_Months, y = pred),
            inherit.aes = FALSE, colour = "#377EB8", linewidth = 1.1) +
  geom_point(aes(colour = Group, size = Max_Deficit_Raw), alpha = 0.80) +
  annotate("text", x = Inf, y = Inf, label = deparse(stats_label),
           hjust = 1.1, vjust = 1.8, size = 4.5, parse = TRUE, colour = COL_SLATE) +
  geom_text_repel(aes(label = Shortname), size = 3, max.overlaps = 20, colour = COL_SLATE) +
  scale_colour_manual(values = fill_color, guide = "none") +
  scale_size_continuous(
    limits = range(pal_size_breaks),
    labels = scientific_10,
    breaks = pal_size_breaks
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  theme_bw() +
  theme(
    panel.grid       = element_blank(),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot",
    legend.position  = "inside",
    legend.box       = "vertical",
    legend.direction = "vertical",
    legend.position.inside = c(0.01, 0.99),
    legend.justification.inside = c(0, 1),
    plot.margin      = margin(5, 10, 5, 5)
  ) +
  labs(
    title    = "C",
    x        = "Recovery period (months)",
    y        = "Rebound intensity",
    size     = "Deficit depth",
    subtitle = "Spearman correlation: disruption duration vs rebound intensity"
  ) +
  guides(colour = "none")

# ============================================================
# Panel D — k-means resilience clustering
# ============================================================

data_fig_d <- df_metrics |>
  select(Shortname, Relative_Deficit, Rebound_Intensity) |>
  drop_na() |>
  filter(Shortname != "HCV") |>
  mutate(Log_Deficit = log10(pmax(Relative_Deficit, 1e-6)))

set.seed(20260101)

data_scaled <- data_fig_d |>
  select(Log_Deficit, Rebound_Intensity) |>
  scale()

# Silhouette-based k selection (2–6)
avg_sil <- sapply(2:6, function(k) {
  km <- kmeans(data_scaled, centers = k, nstart = 25)
  sil <- cluster::silhouette(km$cluster, dist(data_scaled))
  mean(sil[, 3])
})
# Use k=3 for consistency with original analysis
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

cluster_labels <- c("1" = "High rebound\nlow suppression",
                    "2" = "Moderate",
                    "3" = "High suppression\nlow rebound")

data_fig_d <- data_fig_d |>
  mutate(
    Cluster = factor(km_res$cluster),
    Group   = df_metrics$Group[match(Shortname, df_metrics$Shortname)]
  )

panel_D <- ggplot(data_fig_d, aes(x = Relative_Deficit, y = Rebound_Intensity)) +
  stat_chull(aes(fill = Cluster), geom = "polygon", alpha = 0.40) +
  geom_point(aes(colour = Group), size = 3.8, show.legend = FALSE) +
  geom_text_repel(aes(label = Shortname), size = 3, max.overlaps = 20, colour = COL_SLATE) +
  scale_colour_manual(values = fill_color) +
  scale_fill_brewer(palette = "Dark2", labels = cluster_labels) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(
    panel.grid       = element_blank(),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot",
    legend.position  = "inside",
    legend.box       = "horizontal",
    legend.direction = "vertical",
    legend.position.inside = c(0.01, 0.99),
    legend.justification.inside = c(0, 1),
    plot.margin      = margin(5, 10, 5, 5)
  ) +
  labs(
    title    = "D",
    x        = "Relative suppression (%)",
    y        = "Rebound intensity",
    fill     = "Resilience phenotype",
    subtitle = "k-means (k = 3); convex hulls delineate phenotype clusters"
  )

# ============================================================
# Assemble full figure
# ============================================================

top_row    <- panel_A | panel_B
bottom_row <- panel_C | panel_D

full_fig <- top_row /
  bottom_row +
  plot_layout(heights = c(1.3, 1)) +
  plot_annotation(
    title   = "Disruption magnitude profiling and recovery phenotyping",
    caption = "Panel A: cumulative absolute case deficit relative to counterfactual expectation per disease.\nPanel B: rebound intensity (left) and relative suppression depth (right) by disease and transmission group.\nPanel C: recovery period (months, x-axis) vs. rebound intensity (y-axis); bubble size proportional to absolute deficit depth;\n  GAM smooth ± 2 SE shown; Spearman ρ tests the immunity-debt hypothesis.\nPanel D: k-means resilience clustering (k = 3) on log relative suppression and rebound intensity;\n  convex hulls delineate phenotype clusters; HCV excluded as outlier.",
    theme = theme(
      plot.title   = element_text(face = "bold", size = 14, hjust = 0),
      plot.caption = element_text(size = 8.5, colour = COL_MUTED, hjust = 0)
    )
  )

ggsave(file.path(out_dir, "fig5.pdf"),
       full_fig, width = 15, height = 13,
       device = cairo_pdf, family = "Times New Roman", limitsize = FALSE)

ggsave(file.path(out_dir, "fig5.png"),
       full_fig, width = 15, height = 13, dpi = 300, limitsize = FALSE)

# Save underlying data
write.xlsx(
  list(
    panel_A = data_fig_a,
    panel_B = data_fig_b,
    panel_C = data_fig_c,
    panel_D = data_fig_d
  ),
  file = file.path(out_dir, "fig5_data.xlsx")
)

message("fig5 saved to ", out_dir)
