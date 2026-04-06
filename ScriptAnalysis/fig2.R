#####################################
## Fig 2 — Disease-Specific Model Selection Performance
## npj Digital Medicine submission
## Output: ../Outcome/Publish/npjDM/fig2.pdf | fig2.png
##
## Panel A: Model selection heatmap (composite z-standardised index)
## Panel B: Per-disease paired dot plot — best model vs ETS vs SARIMA sMAPE
## Panel C: Summary violin/box — sMAPE improvement of best model
##          over uniform-ETS and uniform-SARIMA baselines
#####################################

library(tidyverse)
library(openxlsx)
library(patchwork)
library(paletteer)

Sys.setlocale("LC_TIME", "C")
remove(list = ls())

source("./function/theme_set.R")

out_dir <- "../Outcome/Publish/npjDM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
appendix_tables_dir <- file.path("..", "Outcome", "Appendix", "Tables")

# ---- Colour palette (dashboard design language) -------------------------
COL_TEAL   <- "#0A6762"
COL_CORAL  <- "#C96A43"
COL_GOLD   <- "#D89A2B"
COL_SLATE  <- "#22313F"
COL_MUTED  <- "#62707B"
COL_BG     <- "#F5EFE6"
COL_GREEN  <- "#2D8B81"

# ============================================================
# Data
# ============================================================

load("./temp/month.RData")

data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |>
     filter(Including == 1 & Forecasting == 1) |>
     mutate(Group = factor(Group, levels = disease_groups)) |>
     arrange(Group, desc(Cases)) |>
     select(Disease, Fullname, Shortname, Group) |>
     mutate(
          Group_panel = ceiling(row_number() / 6),
          id = row_number()
     )

data_goodness_raw <- read.xlsx(file.path(appendix_tables_dir, "Model_test_results.xlsx"))

# ============================================================
# Panel A — composite index heatmap (from 4_b_best_model.R)
# ============================================================

data_goodness <- data_goodness_raw |>
     filter(disease %in% data_class$Shortname) |>
     mutate(disease = factor(disease, levels = rev(data_class$Shortname))) |>
     select(disease, Index, Method, starts_with("Test_")) |>
     pivot_longer(cols = starts_with("Test_"), names_to = "Split", values_to = "TestValue") |>
     pivot_wider(names_from = Index, values_from = TestValue) |>
     select(-R_Squared) |>
     group_by(disease, Split) |>
     mutate(
          norSMAPE = -(SMAPE - mean(SMAPE, na.rm = TRUE)) / sd(SMAPE, na.rm = TRUE),
          norRMSE  = -(RMSE  - mean(RMSE,  na.rm = TRUE)) / sd(RMSE,  na.rm = TRUE),
          norMASE  = -(MASE  - mean(MASE,  na.rm = TRUE)) / sd(MASE,  na.rm = TRUE)
     ) |>
     rowwise() |>
     mutate(Index = sum(c_across(norSMAPE:norMASE), na.rm = TRUE)) |>
     ungroup() |>
     group_by(disease, Method) |>
     summarise(Index = sum(Index, na.rm = TRUE), .groups = "drop_last") |>
     mutate(Best = Method[which.max(Index)]) |>
     ungroup()

data_goodness$Best <- as.numeric(data_goodness$Method == data_goodness$Best)
data_goodness$Method <- factor(data_goodness$Method, levels = models, labels = models_label)

diseases <- rev(data_class$Shortname)

data_best <- data_goodness |>
     filter(Best == 1) |>
     select(disease, Method)

data_map <- data_goodness |>
     select(disease, Method, Index) |>
     pivot_wider(names_from = Method, values_from = Index) |>
     left_join(data_class[, c("Group", "Shortname")], by = c("disease" = "Shortname")) |>
     left_join(data_best, by = "disease") |>
     mutate(disease = factor(disease, levels = diseases)) |>
     arrange(disease) |>
     pivot_longer(cols = -c(Group, disease, Method),
                  names_to = "model",
                  values_to = "value") |>
     mutate(label = if_else(model == Method, "*", "")) |>
     left_join(data_class[, c("Shortname", "Group_panel")], by = c("disease" = "Shortname"))

pal_breaks <- pretty(data_map$value)

# --- group annotation sidebar
data_class_group <- data_class |>
     group_by(Group) |>
     summarise(
          Start = first(Shortname),
          End = last(Shortname),
          StartID = first(id),
          EndID = last(id),
          .groups = "drop"
     ) |>
     mutate(
          StartID = nrow(data_class) - StartID + 1,
          EndID   = nrow(data_class) - EndID + 1
     )

names(fill_color) <- levels(data_class$Group)

fig_group <- ggplot(data_class) +
     geom_tile(aes(x = 1.05, y = Shortname, fill = Group),
               colour = "white", width = 0.55 * 2, alpha = 0.5, show.legend = FALSE) +
     geom_text(aes(x = 1.5, y = Shortname, label = Shortname),
               size = 2.5, hjust = 1, colour = "black") +
     geom_rect(data = data_class_group,
               aes(xmin = 0, xmax = 0.5,
                   ymin = StartID + 0.5, ymax = EndID - 0.5, fill = Group),
               colour = "white", linewidth = 0.8, alpha = 0.7, show.legend = FALSE) +
     geom_text(data = data_class_group,
               aes(x = 0.25, y = (StartID + EndID) / 2, label = Group),
               size = 2.3, colour = "black", fontface = "bold", angle = 90) +
     coord_cartesian(ratio = 1 / 3, xlim = c(0, 1.6)) +
     scale_fill_manual(values = fill_color) +
     scale_y_discrete(expand = expansion(add = c(0, 0)), limits = rev(data_class$Shortname)) +
     scale_x_discrete(expand = expansion(add = c(0, 0))) +
     theme_bw() +
     theme(
          legend.position = "bottom",
          legend.title.position = "top",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(5, 0, 5, 5),
          panel.border = element_blank(),
          plot.title = element_text(face = "bold", size = 12, hjust = 0),
          plot.title.position = "plot",
          panel.grid = element_blank()
     ) +
     labs(x = NULL, y = NULL, title = "A", fill = "Disease categories") +
     guides(fill = guide_legend(ncol = 1, byrow = TRUE))

fig_model <- ggplot(data_map) +
     geom_tile(aes(x = model, y = disease, fill = value), colour = "white") +
     geom_text(aes(x = model, y = disease, label = label), size = 2.5, colour = "black") +
     coord_equal(1) +
     scale_fill_gradientn(
          colors   = paletteer_d("Redmonder::dPBIRdGn"),
          breaks   = pal_breaks,
          limits   = range(pal_breaks)
     ) +
     scale_y_discrete(expand = expansion(add = c(0, 0)), limits = rev(data_class$Shortname)) +
     scale_x_discrete(expand = expansion(add = c(0, 0)), limits = models_label) +
     theme_bw() +
     theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          plot.margin = margin(5, 5, 5, 0),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank()
     ) +
     guides(fill = guide_colourbar(barwidth = 10, title.position = "top", barheight = 0.5)) +
     labs(x = NULL, y = NULL, fill = "Composite index")

# ============================================================
# Panels B & C — model comparison against baseline ETS / SARIMA
# ============================================================

# Compute per-disease per-split sMAPE for all 6 models
data_smape <- data_goodness_raw |>
     filter(disease %in% data_class$Shortname) |>
     select(disease, Method, Index, starts_with("Test_")) |>
     pivot_longer(cols = starts_with("Test_"), names_to = "Split", values_to = "TestValue") |>
     pivot_wider(names_from = Index, values_from = TestValue) |>
     select(disease, Method, Split, SMAPE) |>
     # Aggregate across 3 splits (mean)
     group_by(disease, Method) |>
     summarise(sMAPE_mean = mean(SMAPE, na.rm = TRUE), .groups = "drop")

# Identify best model per disease
best_model_per_disease <- data_goodness |>
     filter(Best == 1) |>
     select(disease, best_method = Method) |>
     mutate(
          # Convert labeled method names back to original names for column matching
          best_method = case_match(
               as.character(best_method),
               "Hybrid**" ~ "Hybrid",
               .default = as.character(best_method)
          )
     )

# Build comparison table: best vs ETS vs SARIMA
data_compare <- data_smape |>
     inner_join(best_model_per_disease, by = "disease") |>
     pivot_wider(names_from = Method, values_from = sMAPE_mean) |>
     mutate(
          best_smape  = mapply(function(dm, cols) cols[[dm]],
                               best_method,
                               MoreArgs = list(cols = across(all_of(models))),
                               SIMPLIFY = TRUE),
          smape_best  = pmap_dbl(list(d = disease, bm = best_method), function(d, bm) {
               data_smape$sMAPE_mean[data_smape$disease == d & data_smape$Method == bm]
          }),
          smape_ETS     = ETS,
          smape_SARIMA  = SARIMA,
          # Improvement = (baseline - best) / baseline × 100
          improve_ETS    = (smape_ETS    - smape_best) / smape_ETS    * 100,
          improve_SARIMA = (smape_SARIMA - smape_best) / smape_SARIMA * 100
     ) |>
     left_join(data_class[, c("Shortname", "Group")], by = c("disease" = "Shortname")) |>
     mutate(
          Group = factor(Group, levels = disease_groups),
          disease = factor(disease, levels = rev(data_class$Shortname))
     )

# Panel B: dot plot — per-disease best vs ETS vs SARIMA sMAPE
data_panel_b <- data_compare |>
     select(disease, Group, smape_best, smape_ETS, smape_SARIMA) |>
     pivot_longer(cols = c(smape_best, smape_ETS, smape_SARIMA),
                  names_to = "model_type",
                  values_to = "sMAPE") |>
     mutate(
          model_type = recode(model_type,
                              smape_best   = "Best (adaptive)",
                              smape_ETS    = "Uniform ETS",
                              smape_SARIMA = "Uniform SARIMA"),
          model_type = factor(model_type, levels = c("Uniform SARIMA", "Uniform ETS", "Best (adaptive)"))
     )

point_colors <- c(
     "Uniform SARIMA"  = "#A0B4C0",
     "Uniform ETS"     = COL_GOLD,
     "Best (adaptive)" = COL_TEAL
)

panel_B <- ggplot(data_panel_b, aes(x = sMAPE, y = disease, colour = model_type, shape = model_type)) +
     geom_line(aes(group = disease), colour = "#C8C0B8", linewidth = 0.5) +
     geom_point(size = 2.8, alpha = 0.9) +
     scale_colour_manual(values = point_colors, name = "Model") +
     scale_shape_manual(values = c("Uniform SARIMA" = 1, "Uniform ETS" = 2, "Best (adaptive)" = 16), name = "Model") +
     scale_y_discrete(limits = rev(data_class$Shortname)) +
     scale_x_continuous(limits = c(0, NA), labels = scales::label_number(suffix = "%")) +
     theme_bw() +
     theme(
          legend.position        = "right",
          legend.title.position  = "top",
          strip.text.y.left      = element_text(angle = 0, hjust = 1, size = 7.5, face = "bold"),
          strip.placement        = "outside",
          strip.background       = element_rect(fill = "#F3EEE6", colour = NA),
          axis.text.y            = element_blank(),
          axis.ticks.y           = element_blank(),
          panel.grid.minor       = element_blank(),
          panel.grid.major.y     = element_blank(),
          plot.title             = element_text(face = "bold", size = 12, hjust = 0),
          plot.title.position    = "plot"
     ) +
     labs(title    = "B",
          x        = NULL,
          y        = NULL)

# Panel C: violin/box — sMAPE improvement distribution
data_panel_c <- data_compare |>
     select(disease, Group, improve_ETS, improve_SARIMA) |>
     pivot_longer(cols = c(improve_ETS, improve_SARIMA),
                  names_to = "baseline", values_to = "pct_improve") |>
     mutate(baseline = recode(baseline,
                              improve_ETS    = "vs. ETS",
                              improve_SARIMA = "vs. SARIMA"))

panel_C <- ggplot(data_panel_c, aes(x = baseline, y = pct_improve, fill = baseline)) +
     geom_hline(yintercept = 0, linetype = "dashed", colour = COL_MUTED, linewidth = 0.7) +
     geom_violin(width = 0.6, alpha = 0.30, colour = NA) +
     geom_boxplot(width = 0.20, alpha = 0.85, colour = COL_SLATE, outlier.size = 1.5) +
     geom_jitter(width = 0.08, size = 1.8, alpha = 0.6, colour = COL_SLATE) +
     scale_fill_manual(values = c("vs. ETS" = COL_GOLD, "vs. SARIMA" = "#A0B4C0"), guide = "none") +
     scale_y_continuous(labels = scales::label_number(suffix = " pp")) +
     theme_bw() +
     theme(
          legend.position      = "none",
          panel.grid.minor     = element_blank(),
          panel.grid.major.x   = element_blank(),
          plot.title           = element_text(face = "bold", size = 12, hjust = 0),
          plot.title.position  = "plot",
          axis.text.x          = element_text(size = 11, face = "bold"),
          axis.text.y          = element_text(size = 10)
     ) +
     labs(
          title    = "C",
          x        = NULL,
          y        = "sMAPE improvement over baseline (pp)"
     )

# ============================================================
# Assemble full figure
# ============================================================

design <- "
ABCD
ABCE
"

full_fig <- fig_group + fig_model + panel_B + panel_C + guide_area() +
     plot_layout(nrow = 1,
                 design = design,
                 widths = c(1, 1.1, 2, 1.5),
                 heights = c(1.5, 1),
                 guides = 'collect')

full_fig <- collect_guides_bottom(full_fig)

ggsave(file.path(out_dir, "fig2.pdf"),
       full_fig, width = 10, height = 8,
       device = cairo_pdf, family = "Times New Roman", limitsize = FALSE)

ggsave(file.path(out_dir, "fig2.png"),
       full_fig, width = 10, height = 8, dpi = 300, limitsize = FALSE)

message("fig2 saved to ", out_dir)
