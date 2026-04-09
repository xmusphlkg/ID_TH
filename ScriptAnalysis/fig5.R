
# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggrepel)
library(openxlsx)
library(cluster)

# data --------------------------------------------------------------------

Sys.setlocale("LC_TIME", "C")

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load("./temp/month.RData")
load("./temp/outcome.RData")

publish_fig_dir <- "../Outcome/Publish/npjDM"
publish_data_dir <- "../Outcome/Publish/figure_data"
appendix_fig_dir <- "../Outcome/Appendix/Supplementary Appendix 1_8"

dir.create(publish_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(publish_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(appendix_fig_dir, recursive = TRUE, showWarnings = FALSE)

# estimate rebound metrics -------------------------------------------------

df_metrics <- calculate_disease_metrics(outcome)

data_class <- data_class |>
     mutate(
          Shortname = if_else(Shortname == "CA", "CA (HPV)", Shortname),
          Group = factor(Group, levels = disease_groups)
     ) |>
     filter(Shortname %in% df_metrics$Shortname)

df_metrics <- df_metrics |>
     mutate(Shortname = if_else(Shortname == "CA", "CA (HPV)", Shortname)) |>
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |>
     mutate(Group = factor(Group, levels = disease_groups)) |>
     filter(Max_Deficit_Raw < 0) |>
     mutate(
          Relative_Deficit = abs(Relative_Deficit),
          Absolute_Suppression = abs(Max_Deficit_Raw)
     )

disease_order <- data_class |>
     filter(Shortname %in% df_metrics$Shortname) |>
     pull(Shortname)

names(fill_color) <- disease_groups

# panel A -----------------------------------------------------------------

data_fig1 <- df_metrics |>
     transmute(
          Shortname,
          Group,
          Absolute_Suppression
     )

write.csv(
     data_fig1,
     "../Outcome/Publish/fig5_a_data.csv",
     row.names = FALSE
)

fig1 <- ggplot(data_fig1, aes(x = Absolute_Suppression, y = Shortname, fill = Group)) +
     geom_col(width = 0.7, show.legend = TRUE) +
     scale_x_continuous(
          breaks = scales::pretty_breaks(n = 5),
          labels = scientific_10,
          expand = expansion(mult = c(0, 0.03))
     ) +
     scale_y_discrete(limits = rev(disease_order)) +
     scale_fill_manual(values = fill_color) +
     labs(
          title = "A",
          x = "Absolute suppression",
          y = NULL,
          fill = "Disease categories"
     ) +
     theme_bw() +
     theme(
          panel.grid = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom",
          legend.title.position = "top",
          plot.margin = margin(5, 10, 5, 5)
     )

# panel B -----------------------------------------------------------------

data_fig2 <- df_metrics |>
     transmute(
          Shortname,
          Group,
          Relative_Deficit,
          Relative_Deficit_percent = round(Relative_Deficit * 100, 2),
          Rebound_Intensity
     )

fig2_a <- ggplot(data_fig2, aes(x = Rebound_Intensity, y = Shortname, fill = Group)) +
     geom_col(width = 0.7, show.legend = TRUE) +
     scale_x_continuous(
          limits = range(pretty(data_fig2$Rebound_Intensity)),
          trans = "reverse",
          breaks = scales::pretty_breaks(n = 5),
          expand = expansion(mult = c(0, 0))
     ) +
     scale_y_discrete(limits = rev(disease_order), position = "right") +
     scale_fill_manual(values = fill_color) +
     labs(
          title = "B",
          x = "Rebound intensity",
          y = NULL,
          fill = "Disease categories"
     ) +
     theme_bw() +
     theme(
          panel.grid = element_blank(),
          plot.margin = margin(5, 0, 5, 5),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          legend.title.position = "top",
          plot.title.position = "plot"
     )

fig2_b <- ggplot(data_fig2, aes(x = Relative_Deficit, y = Shortname, fill = Group)) +
     geom_col(width = 0.7, show.legend = FALSE) +
     scale_x_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.2),
          labels = scales::percent_format(accuracy = 1),
          expand = expansion(mult = c(0, 0))
     ) +
     scale_y_discrete(limits = rev(disease_order)) +
     scale_fill_manual(values = fill_color) +
     labs(
          x = "Relative suppression (%)",
          y = NULL
     ) +
     theme_bw() +
     theme(
          panel.grid = element_blank(),
          plot.margin = margin(5, 10, 5, 0),
          axis.text.y = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.title.position = "plot"
     )

# panel C -----------------------------------------------------------------

data_fig3 <- df_metrics |>
     drop_na(Suppression_Months, Rebound_Intensity) |>
     transmute(
          Shortname,
          Group,
          Recovery_Period = Suppression_Months,
          Rebound_Intensity,
          Absolute_Suppression
     )

cor_test <- cor.test(
     data_fig3$Recovery_Period,
     data_fig3$Rebound_Intensity,
     method = "pearson"
)

r_val <- formatC(as.numeric(cor_test$estimate), format = "f", digits = 2)
p_val <- if (cor_test$p.value < 0.001) {
     "< 0.001"
} else {
     formatC(cor_test$p.value, format = "f", digits = 3)
}

stats_label <- paste0("r = ", r_val, ", P = ", p_val)
pal_size_breaks <- pretty(range(data_fig3$Absolute_Suppression), n = 4)

fig3 <- ggplot(data_fig3, aes(x = Recovery_Period, y = Rebound_Intensity)) +
     geom_smooth(
          method = "lm",
          se = TRUE,
          color = "#377EB8",
          fill = "#DDEBF7",
          linewidth = 1
     ) +
     geom_point(aes(color = Group, size = Absolute_Suppression), alpha = 0.75) +
     geom_text_repel(aes(label = Shortname), size = 3) +
     annotate(
          "text",
          x = Inf,
          y = Inf,
          label = stats_label,
          hjust = 1.05,
          vjust = 1.3,
          size = 5
     ) +
     scale_color_manual(values = fill_color) +
     scale_size_continuous(
          limits = range(pal_size_breaks),
          labels = scientific_10,
          breaks = pal_size_breaks
     ) +
     labs(
          title = "C",
          x = "Recovery period (months)",
          y = "Rebound intensity",
          size = "Deficit depth",
          color = "Group"
     ) +
     theme_bw() +
     theme(
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 14, hjust = 0),
          plot.margin = margin(5, 10, 5, 5),
          legend.position = "inside",
          legend.box = "vertical",
          legend.direction = "vertical",
          legend.position.inside = c(0.01, 0.99),
          legend.justification.inside = c(0, 1),
          plot.title.position = "plot"
     ) +
     guides(color = "none")

# panel D -----------------------------------------------------------------

data_fig4 <- df_metrics |>
     drop_na(Relative_Deficit, Rebound_Intensity) |>
     transmute(
          Shortname,
          Group,
          Relative_Deficit,
          Rebound_Intensity
     )

data_fig4_scaled <- data_fig4 |>
     select(Relative_Deficit, Rebound_Intensity) |>
     scale()

set.seed(20260101)

ks <- 2:6
wss <- sapply(ks, function(k) {
     km <- kmeans(data_fig4_scaled, centers = k, nstart = 25)
     km$tot.withinss
})

cluster_selection <- tibble(
     k = ks,
     TotalWithinSS = wss,
     RelativeReductionPct = c(NA_real_, (head(wss, -1) - tail(wss, -1)) / head(wss, -1) * 100)
)

chosen_k <- 3L
cluster_summary <- tibble(
     SelectedK = chosen_k,
     WSS_K2 = wss[ks == 2],
     WSS_K3 = wss[ks == 3],
     Reduction_K3_vs_K2_Pct = (wss[ks == 2] - wss[ks == 3]) / wss[ks == 2] * 100
)

km_res <- kmeans(data_fig4_scaled, centers = chosen_k, nstart = 25)

data_fig4 <- data_fig4 |>
     mutate(Cluster = as.factor(km_res$cluster))

fig4 <- ggplot(data_fig4, aes(x = Relative_Deficit, y = Rebound_Intensity)) +
     ggforce::geom_mark_ellipse(aes(fill = Cluster),
                                color = 'white',
                                alpha = 0.3) +
     geom_point(aes(color = Group), size = 4, show.legend = FALSE) +
     geom_text_repel(aes(label = Shortname), size = 3, show.legend = FALSE) +
     scale_color_manual(values = fill_color) +
     scale_fill_brewer(palette = "Set2") +
     scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
     labs(
          title = "D",
          x = "Relative suppression (%)",
          y = "Rebound intensity",
          fill = "Cluster"
     ) +
     theme_bw() +
     theme(
          legend.position = "inside",
          plot.margin = margin(5, 10, 5, 5),
          plot.title = element_text(face = "bold", size = 14, hjust = 0),
          legend.box = "vertical",
          legend.direction = "vertical",
          legend.position.inside = c(0.99, 0.99),
          legend.justification.inside = c(1, 1),
          plot.title.position = "plot"
     )

# save --------------------------------------------------------------------

fig1 <- fig1 +
     theme(legend.position = "bottom",
           legend.title.position = "top",
           plot.title = element_text(face = "bold", size = 14, hjust = 0))

fig2_a <- fig2_a +
     theme(legend.position = "bottom",
           legend.title.position = "top",
           plot.title = element_text(face = "bold", size = 14, hjust = 0))

fig2_b <- fig2_b +
     theme(legend.position = "bottom",
           legend.title.position = "top",
           plot.title = element_text(face = "bold", size = 14, hjust = 0))

fig3 <- fig3 +
     theme(plot.title = element_text(face = "bold", size = 14, hjust = 0))

fig4 <- fig4 +
     theme(plot.title = element_text(face = "bold", size = 14, hjust = 0))

top_row <- free(fig1) + fig2_a + fig2_b +
     plot_layout(ncol = 3, widths = c(1.25, 0.52, 0.52), guides = "collect")
top_row <- collect_guides_bottom(top_row)

bottom_row <- fig3 + fig4 +
     plot_layout(ncol = 2)

final_plot <- top_row / bottom_row

ggsave(
     file.path(publish_fig_dir, "fig5.pdf"),
     plot = final_plot,
     family = "Times New Roman",
     limitsize = FALSE,
     device = cairo_pdf,
     width = 14,
     height = 10
)

ggsave(
     file.path(publish_fig_dir, "fig5.png"),
     final_plot,
     limitsize = FALSE,
     width = 14,
     height = 10,
     dpi = 300
)

ggsave(
     file.path(appendix_fig_dir, "suppression_rebound_patterns.pdf"),
     plot = final_plot,
     family = "Times New Roman",
     limitsize = FALSE,
     device = cairo_pdf,
     width = 14,
     height = 10
)

ggsave(
     file.path(appendix_fig_dir, "suppression_rebound_patterns.png"),
     final_plot,
     limitsize = FALSE,
     width = 14,
     height = 10,
     dpi = 300
)

figure_data <- list(
     "panel A" = data_fig1,
     "panel B" = data_fig2,
     "panel C" = data_fig3,
     "panel D" = data_fig4,
     "cluster selection" = cluster_selection,
     "cluster summary" = cluster_summary
)

write.xlsx(
     figure_data,
     file = file.path(publish_data_dir, "fig5.xlsx")
)
