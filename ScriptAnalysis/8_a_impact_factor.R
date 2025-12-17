
# packages ----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(patchwork)
library(ggrepel)
library(broom)
library(factoextra)

# data --------------------------------------------------------------------

Sys.setlocale("LC_TIME", "C")

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load('./month.RData')
load('./outcome.RData')

# load predict dataset
data_predict <- read.xlsx('../Data/TotalCasesDeaths.xlsx', sheet = 'Predictors') |> 
     mutate(R0_mean = (R0_range_min + R0_range_max) / 2,
            R0_mean = case_when(is.na(R0_mean) & R0_refer == 'Not transmitted directly' ~ 0,
                                is.na(R0_mean) & R0_refer == 'Uncertain' ~ NA_real_,
                                TRUE ~ R0_mean),
            Vaccine = factor(Vaccine, levels = c("Unavaliable", "Optional", "EPI")),
            Group = factor(Group, levels = c("Respiratory IDs", "Gastrointestinal IDs", "Sexually IDs", "Vector-borne and zoonotic IDs"))) |> 
     select(Shortname, R0_mean, Vaccine, Group, Incubation_period)

# estimate Rebound Metrics
df_metrics <- calculate_disease_metrics(outcome)

# Merge everything into a Master Analysis Dataframe
df_analysis <- df_metrics |>
     left_join(data_predict, by = "Shortname")

# Clean data class
data_class <- data_class |> 
     filter(Shortname %in% df_analysis$Shortname)

# The Landscape of Immunity Deficit ----------------------------------------

data_fig1 <- df_analysis |> 
     filter(Max_Deficit_Raw < 0) |> 
     mutate(Relative_Deficit = abs(Relative_Deficit),
            Max_Deficit_Raw = abs(Max_Deficit_Raw))

names(fill_color) <- levels(data_class$Group)

# Absolute defivit map
# using https://app.rawgraphs.io/
# seed: 11
write.csv(data_fig1 |> 
               select(Shortname, Group, Max_Deficit_Raw),
          "../Outcome/Publish/fig6_a_data.csv",
          row.names = FALSE)

# create a empty plot for fig1
fig2 <- ggplot() + 
     theme_bw() + 
     labs(title = "B") +
     theme(plot.title.position = "plot")

fig1 <- ggplot(data_fig1, aes(x = Relative_Deficit, y = Shortname, fill = Group)) +
     geom_col(width = 0.7, show.legend = F) +
     scale_x_continuous(limits = c(0, 1),
                        breaks = seq(0, 1, by = 0.2),
                        labels = scales::percent_format(accuracy = 2),
                        expand = expansion(mult = c(0, 0))) +
     scale_y_discrete(limits = rev(data_class$Shortname))+
     scale_fill_manual(values = fill_color) +
     labs(title = "A",
          fill = "Disease categories",
          x = "Relative deficit of cases during suppression(%)",
          y = NULL) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.margin = margin(5, 10, 5, 5),
           legend.position = 'bottom',
           plot.title.position = "plot")

# The Dynamics of Recovery ------------------------------------------------

# Prepare data: Remove diseases that never recovered (Suppression_Months is NA)
data_fig3 <- df_analysis |> 
     filter(!is.na(Suppression_Months))

fig3 <- data_fig3 |>
     filter(!is.na(Suppression_Months)) |>
     mutate(Status = if_else(Status == "Suppressed", "Suppressed", "Recovered")) |> 
     ggplot(aes(x = Suppression_Months, y = Shortname, color = Group, shape = Status)) +
     geom_segment(aes(x = 0, xend = Suppression_Months, y = Shortname, yend = Shortname), color = "gray") +
     geom_point(size = 3) +
     scale_y_discrete(limits = rev(data_class$Shortname))+
     scale_color_manual(values = fill_color) +
     scale_x_continuous(limits = c(0, 12*6),
                        breaks = seq(0, 12*6, by = 12),
                        expand = expansion(mult = c(0, 0))) +
     scale_shape_manual(values = c(16, 17)) +
     labs(title = "C",
          color = "Disease categories",
          shape = "Recovery status",
          x = "Months from start of deficit to trend recovery",
          y = NULL) +
     theme_bw() +
     theme(panel.grid.major.y = element_blank(),
           plot.margin = margin(5, 10, 5, 5),
           legend.position = 'bottom',
           plot.title.position = "plot")+
     guides(color = guide_legend(nrow = 1, order = 2),
            shape = guide_legend(nrow = 1, order = 1))

# Load necessary survival libraries
library(survival)

# Run Models
data_cox <- data_fig3 |> 
     mutate(status_binary = ifelse(str_detect(Status, "Suppressed"), 0, 1),
            time = as.numeric(Suppression_Months))

run_cox <- function(formula, data, label) {
     coxph(formula, data = data) |> 
          tidy(exponentiate = TRUE, conf.int = TRUE) |> 
          filter(term != "(Intercept)") |> 
          mutate(Model = label)
}

# Model A: Vaccine
m_vaccine <- run_cox(Surv(time, status_binary) ~ Vaccine, data_cox, "Vaccine Status")

# Model B: Group
m_group <- run_cox(Surv(time, status_binary) ~ Group, data_cox, "Transmission Mode")

# Model C: R0
m_r0 <- run_cox(Surv(time, status_binary) ~ R0_mean, 
                data_cox |> filter(R0_mean > 0), 
                "R0")

# Model D: Incubation Period
m_incub <- run_cox(Surv(time, status_binary) ~ Incubation_period, 
                   data_cox |> filter(!is.na(Incubation_period)), 
                   "Incubation Period")

# combine results
data_fig4 <- bind_rows(
     m_vaccine |> mutate(category = "Vaccination"),
     m_group   |> mutate(category = "Categories"),
     m_r0      |> mutate(category = "R0"),
     m_incub   |> mutate(category = "Incubation period")
) |> 
     mutate(term_clean = str_remove_all(term, "Vaccine|Group"),
            term_clean = case_when(term_clean == "R0_mean" ~ "Reproductive number",
                                   term_clean == "Incubation_period" ~ "Incubation period",
                                   TRUE ~ term_clean)) |>
     # add control group
     bind_rows(
          tibble(estimate = 1,
                 category = c("Vaccination", "Categories"),
                 term_clean = c("Unavaliable", "Respiratory IDs"))
     ) |> 
     mutate(term_clean = factor(term_clean, levels = rev(c("Reproductive number", "Incubation period",
                                                           "Unavaliable", "Optional", "EPI",
                                                           "Respiratory IDs", "Gastrointestinal IDs", "Sexually IDs", "Vector-borne and zoonotic IDs"))),
            category = factor(category, levels = c('R0', 'Incubation period', 'Vaccination', 'Categories'))) |> 
     arrange(term_clean) |> 
     mutate(order_id = row_number())

bg_data <- data_fig4 |>
     group_by(category) |>
     summarise(ymin = min(order_id) - 0.5,
               ymax = max(order_id) + 0.5) |>
     arrange(ymin) |>
     mutate(fill_color = ifelse(row_number() %% 2 == 1, "grey95", "white"))

fig4 <- ggplot(data_fig4) +
     geom_rect(data = bg_data, 
               aes(xmin = 0.1, xmax = 60, ymin = ymin, ymax = ymax, fill = fill_color), 
               alpha = 1, inherit.aes = FALSE) +
     scale_fill_identity()+
     geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
     geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = term_clean, color = category)) +
     geom_point(aes(x = estimate, y = term_clean, color = category), size = 3) +
     annotate("text", x = 1, y = 0, label = "Faster recovery", hjust = -0.1, size = 4) +
     annotate("text", x = 1, y = 0, label = "Slower recovery", hjust = 1.1, size = 4) +
     scale_x_continuous(trans = "log10",
                        expand = c(0, 0),
                        breaks = c(0.1, 0.3, 1, 3, 10, 30, 60),
                        limits = c(0.1, 60)) +
     scale_y_discrete(expand = expansion(add = c(1.5, 0.5)),
                      limits = levels(data_fig4$term_clean))+
     scale_color_manual(values = fill_color_disease) +
     labs(title = "D",
          x = "Hazard ratio (95% CI)",
          color = "Predictors",
          y = NULL) +
     theme_bw() +
     theme(legend.position = "bottom",
           plot.margin = margin(5, 10, 5, 5),
           panel.grid = element_blank(),
           plot.title.position = "plot")

# Testing the Immunity Debt Hypothesis ------------------------------------

# Hypothesis: Deeper Debt (X) -> Stronger Rebound (Y)
data_fig5 <- df_analysis |> 
     filter(!is.na(Rebound_Intensity),
            Rebound_Intensity > 1,
            !is.na(Suppression_Months)) |> 
     mutate(Max_Deficit_Raw = abs(Max_Deficit_Raw))

cor_test <- cor.test(data_fig5$Suppression_Months, data_fig5$Rebound_Intensity, method = "pearson")
p_val_label <- paste0("R = ", round(cor_test$estimate, 2), ", p = ", signif(cor_test$p.value, 2))

pal_size_breaks <- pretty(range(data_fig5$Max_Deficit_Raw), n = 5)

fig5 <- ggplot(data_fig5, aes(x = Suppression_Months, y = Rebound_Intensity)) +
     geom_smooth(method = "lm", color = "#F89C74FF", fill = "#F6CF71FF", alpha = 0.5) +
     geom_point(aes(color = Group, size = Max_Deficit_Raw), alpha = 0.7) +
     annotate("text", x = Inf, y = Inf, label = p_val_label, hjust = 1.1, vjust = 1.5, size = 5) +
     geom_text_repel(aes(label = Shortname), size = 3) +
     scale_color_manual(values = fill_color) +
     scale_size_continuous(limits = range(pal_size_breaks),
                           labels = scientific_10,
                           breaks = pal_size_breaks) +
     labs(
          title = "E",
          x = "Duration of suppression (months)",
          y = "Rebound intensity ",
          size = "Deficit depth",
          color = "Group"
     ) +
     theme_bw()+
     theme(panel.grid = element_blank(),
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
data_fig6 <- df_analysis |>
     select(Shortname,
            Relative_Deficit,
            Rebound_Intensity) |>
     drop_na() |> 
     # drop HCV (outlier)
     filter(Shortname != "HCV") |> 
     mutate(Deficit_Magnitude = abs(Relative_Deficit),
            Log_Rebound = log10(Deficit_Magnitude + 1))

data_fig6_scaled <- data_fig6 |>
     select(Log_Rebound, Rebound_Intensity) |>
     scale()

# 2. K-means Clustering (Assume k=3 for High/Medium/Low risk or patterns)
set.seed(123)
km_res <- kmeans(data_fig6_scaled, centers = 3, nstart = 25)

# Add cluster back to data
data_fig6 <- data_fig6 |> 
     mutate(Cluster = as.factor(km_res$cluster)) |> 
     left_join(data_class |> select(Shortname, Group), by = "Shortname")

# 3. Visualize (Bi-plot style)
# X-axis: Susceptibility (Accumulated Deficit)
# Y-axis: Resilience/Reaction (Rebound Intensity)

fig6 <- ggplot(data_fig6, aes(x = Deficit_Magnitude, y = Rebound_Intensity)) +
     # Draw hull or ellipse
     ggpubr::stat_chull(aes(fill = Cluster), geom = "polygon", alpha = 0.5) +
     geom_point(aes(color = Group), size = 4, show.legend = F) +
     geom_text_repel(aes(label = Shortname), size = 3, show.legend = FALSE) +
     scale_color_manual(values = fill_color) +
     scale_fill_brewer(palette = "Dark2") +
     labs(title = "F",
          x = "Immunity deficit (relative %)",
          y = "Explosiveness (rebound intensity)") +
     theme_bw() +
     theme(legend.position = "inside",
           plot.margin = margin(5, 10, 5, 5),
           legend.box = "horizontal",
           legend.direction = "horizontal",
           legend.position.inside = c(0.01, 0.99),
           legend.justification.inside = c(0, 1))

# save --------------------------------------------------------------------

final_plot <- cowplot::plot_grid(
     fig1 + fig2 + 
     fig3 + fig4 +
          plot_layout(ncol = 2, widths = c(1, 1.5), byrow = T, guides = 'collect') &
          theme(legend.position = 'bottom',
                legend.title.position = 'top'),
     cowplot::plot_grid(fig5, fig6,
                        ncol = 2,
                        labels = NULL),
     ncol = 1,
     byrow = F,
     rel_heights = c(2.1, 1),
     labels = NULL
)

# Save
ggsave("../Outcome/Publish/fig6.pdf",
       plot = final_plot, 
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 14)

ggsave("../Outcome/Publish/fig6.png",
       final_plot,
       limitsize = FALSE,
       width = 14, height = 14)
