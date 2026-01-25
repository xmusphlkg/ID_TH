
# packages ----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(patchwork)
library(ggrepel)
library(broom)
library(factoextra)
library(openxlsx)

# data --------------------------------------------------------------------

Sys.setlocale("LC_TIME", "C")

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load('./temp/month.RData')
load('./temp/outcome.RData')

# estimate Rebound Metrics
df_metrics <- calculate_disease_metrics(outcome)

# Clean data class
data_class <- data_class |> 
     # correct CA to CA (HPV)
     filter(Shortname %in% df_metrics$Shortname)

# correct CA to CA (HPV)
df_metrics <- df_metrics |> 
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |> 
     mutate(Shortname = if_else(Shortname == "CA", "CA (HPV)", Shortname)) |> 
     filter(Max_Deficit_Raw < 0) |> 
     mutate(Relative_Deficit = abs(Relative_Deficit),
            Max_Deficit_Raw = abs(Max_Deficit_Raw))

# reduction ----------------------------------------

names(fill_color) <- levels(data_class$Group)

# Absolute reduction
# using https://app.rawgraphs.io/
# seed: 11

data_fig1 <- df_metrics |> 
     select(Shortname, Group, Max_Deficit_Raw)

write.csv(data_fig1,
          "../Outcome/Publish/fig5_a_data.csv",
          row.names = FALSE)

# create a empty plot for fig1
fig1 <- ggplot() + 
     theme_bw() + 
     labs(title = "A") +
     theme(plot.title.position = "plot")

data_fig2 <- df_metrics |> 
     select(Shortname, Group, Relative_Deficit, Rebound_Intensity) |> 
     mutate(Relative_Deficit_percent = round(Relative_Deficit * 100, 2))

fig2_a <- ggplot(data_fig2, aes(x = Rebound_Intensity, y = Shortname, fill = Group)) +
     geom_col(width = 0.7, show.legend = T) +
     scale_x_continuous(limits = range(pretty(data_fig2$Rebound_Intensity)),
                        trans = 'reverse',
                        breaks = scales::pretty_breaks(n = 5),
                        expand = expansion(mult = c(0, 0))) +
     scale_y_discrete(limits = rev(data_class$Shortname),
                      position = "right")+
     scale_fill_manual(values = fill_color) +
     labs(fill = "Disease categories",
          title = "B",
          x = "Rebound intensity",
          y = NULL) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.margin = margin(5, 0, 5, 5),
           axis.text.y = element_blank(),
           legend.position = 'bottom',
           plot.title.position = "plot")

fig2_b <- ggplot(data_fig2, aes(x = Relative_Deficit, y = Shortname, fill = Group)) +
     geom_col(width = 0.7, show.legend = F) +
     scale_x_continuous(limits = c(0, 1),
                        breaks = seq(0, 1, by = 0.2),
                        labels = scales::percent_format(accuracy = 2),
                        expand = expansion(mult = c(0, 0))) +
     scale_y_discrete(limits = rev(data_class$Shortname))+
     scale_fill_manual(values = fill_color) +
     labs(fill = "Disease categories",
          x = "Reduction during suppression(%)",
          y = NULL) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.margin = margin(5, 10, 5, 0),
           axis.text.y = element_text(hjust = 0.5),
           legend.position = 'bottom',
           plot.title.position = "plot")

# Immunity Debt Hypothesis ------------------------------------

# Hypothesis: Deeper reduction (X) -> Stronger Rebound (Y)

data_fig3 <- df_metrics |> 
     filter(!is.na(Rebound_Intensity),
            Rebound_Intensity > 1,
            !is.na(Suppression_Months)) |> 
     mutate(Max_Deficit_Raw = abs(Max_Deficit_Raw)) |> 
     select(Shortname, Group, Suppression_Months, Rebound_Intensity,  Max_Deficit_Raw)

cor_test <- cor.test(data_fig3$Suppression_Months,
                     data_fig3$Rebound_Intensity,
                     method = "pearson")

r_val <- formatC(as.numeric(cor_test$estimate), format = "f", digits = 2)
p_val <- signif(cor_test$p.value, 2)

stats_label <- bquote(italic(r) == .(r_val) ~ "," ~ italic(P) == .(p_val))

pal_size_breaks <- pretty(range(data_fig3$Max_Deficit_Raw), n = 5)

fig3 <- ggplot(data_fig3, aes(x = Suppression_Months, y = Rebound_Intensity)) +
     geom_smooth(method = "lm", color = "#F89C74FF", fill = "#F6CF71FF", alpha = 0.5) +
     geom_point(aes(color = Group, size = Max_Deficit_Raw), alpha = 0.7) +
     annotate("text", x = Inf, y = Inf, label = deparse(stats_label), 
              hjust = 1.1, vjust = 1.5, size = 5, parse = TRUE) +
     geom_text_repel(aes(label = Shortname), size = 3) +
     scale_color_manual(values = fill_color) +
     scale_size_continuous(limits = range(pal_size_breaks),
                           labels = scientific_10,
                           breaks = pal_size_breaks) +
     labs(
          title = "C",
          x = "Recovery period (months)",
          y = "Rebound intensity ",
          size = "Deficit depth",
          color = "Group"
     ) +
     theme_bw()+
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           plot.margin = margin(5, 10, 5, 5),
           legend.position = "inside",
           legend.box = "vertical",
           legend.direction = "vertical",
           legend.position.inside = c(0.01, 0.99),
           legend.justification.inside = c(0, 1),
           plot.title.position = "plot")+
     guides(color = 'none')

# Resilience Clustering ---------------------------------------------------

# 1. Prepare Data for Clustering (Normalize)
# Select key metrics: Depth (Log_Deficit), Time (Suppression), Intensity (Rebound)

data_fig4 <- df_metrics |>
     select(Shortname,
            Relative_Deficit,
            Rebound_Intensity) |>
     drop_na() |> 
     # drop HCV (outlier)
     filter(Shortname != "HCV") |> 
     mutate(Log_Rebound = log10(Relative_Deficit))

data_fig4_scaled <- data_fig4 |>
     select(Log_Rebound, Rebound_Intensity) |>
     scale()

# 2. K-means Clustering (Assume k=3 for High/Medium/Low risk or patterns)
set.seed(20260101)

km_res <- kmeans(data_fig4_scaled, centers = 3, nstart = 25)

# Add cluster back to data
data_fig4 <- data_fig4 |> 
     mutate(Cluster = as.factor(km_res$cluster)) |> 
     left_join(data_class |> select(Shortname, Group), by = "Shortname")

# 3. Visualize (Bi-plot style)
# X-axis: Susceptibility (Accumulated Deficit)
# Y-axis: Resilience/Reaction (Rebound Intensity)

fig4 <- ggplot(data_fig4, aes(x = Relative_Deficit, y = Rebound_Intensity)) +
     # Draw hull or ellipse
     ggpubr::stat_chull(aes(fill = Cluster), geom = "polygon", alpha = 0.5) +
     geom_point(aes(color = Group), size = 4, show.legend = F) +
     geom_text_repel(aes(label = Shortname), size = 3, show.legend = FALSE) +
     scale_color_manual(values = fill_color) +
     scale_fill_brewer(palette = "Dark2") +
     labs(title = "D",
          x = "Reduction during suppression(%)",
          y = "Rebound intensity") +
     theme_bw() +
     theme(legend.position = "inside",
           plot.margin = margin(5, 10, 5, 5),
           plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           legend.box = "horizontal",
           legend.direction = "horizontal",
           legend.position.inside = c(0.01, 0.99),
           legend.justification.inside = c(0, 1))

# save --------------------------------------------------------------------

final_plot <- cowplot::plot_grid(
     free(fig1) + fig2_a + fig2_b +
          plot_layout(ncol = 3, widths = c(1.2, 0.5, 0.5), byrow = T, guides = 'collect') &
          theme(legend.position = 'bottom',
                legend.title.position = 'top',
                plot.title = element_text(face = 'bold', size = 14, hjust = 0)),
     fig3 + fig4 +
          plot_layout(ncol = 2, byrow = ) &
          theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0)),
     ncol = 1,
     byrow = F,
     rel_heights = c(1.2, 1),
     labels = NULL
)

# Save
ggsave("../Outcome/Publish/fig5.pdf",
       plot = final_plot, 
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 10)

ggsave("../Outcome/Publish/fig5.png",
       final_plot,
       limitsize = FALSE,
       width = 14, height = 10)

outcome <- list('panel A' = data_fig1,
                'panel B' = data_fig2,
                'panel C' = data_fig3,
                'panel D' = data_fig4)

write.xlsx(outcome,
           file = '../Outcome/Publish/figure_data/fig5.xlsx')
