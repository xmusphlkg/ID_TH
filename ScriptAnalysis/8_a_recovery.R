
# packages ----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(patchwork)
library(ggrepel)
library(broom)
library(ggpubr)
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

# load predict dataset
data_predict <- read.xlsx('../Data/TotalCasesDeaths.xlsx', sheet = 'Predictors') |> 
     filter(!is.na(Shortname)) |>
     select(Disease, Shortname, Group,
            Incubation_period, Infectious_period,
            Vaccine, Immune_protection_vaccine, Immune_protection_nature)

# Merge everything into a Master Analysis Dataframe
df_analysis <- df_metrics |>
     left_join(data_predict, by = c("Shortname", 'Group')) |> 
     mutate(Group = factor(as.character(Group), levels = c("Respiratory IDs", "Gastrointestinal IDs", "Sexually IDs", "Vector-borne and zoonotic IDs")),
            Infectious_period = factor(Infectious_period, levels = c('<1 month', '1 month - 1 year', '>1 year')),
            Vaccine = factor(Vaccine, levels = c("Unavaliable", "Optional", "EPI")),
            Immune_protection_vaccine = factor(Immune_protection_vaccine, levels = c("None", "Short-term", "Long-term")),
            Immune_protection_nature = factor(Immune_protection_nature, levels = c("None", "Short-term", "Long-term")))

# The Dynamics of Recovery ------------------------------------------------

# Prepare data: Remove diseases that never recovered (Suppression_Months is NA)
data_fig1 <- df_analysis |> 
     filter(!is.na(Suppression_Months)) |> 
     select(Shortname, Group, Suppression_Months, Status)

fig1 <- data_fig1 |>
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
     labs(title = "A",
          color = "Disease categories",
          shape = "Recovery status",
          x = "Months from start of deficit to trend recovery",
          y = NULL) +
     theme_bw() +
     theme(panel.grid.major.y = element_blank(),
           plot.margin = margin(5, 10, 5, 5),
           legend.position = 'bottom',
           legend.title.position = 'top',
           legend.box = "horizontal",
           legend.box.just = "left",
           plot.title.position = "plot")+
     guides(color = guide_legend(nrow = 2, order = 2),
            shape = guide_legend(nrow = 2, order = 1))

# Load necessary survival libraries
library(survival)

# Run Models
data_cox <- df_analysis |> 
     filter(!is.na(Suppression_Months)) |> 
     select(Shortname, Group, Suppression_Months, Status, Vaccine, Infectious_period,
            Incubation_period, Immune_protection_vaccine, Immune_protection_nature) |>
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

# Model C: Incubation Period
m_incub <- run_cox(Surv(time, status_binary) ~ Incubation_period, 
                   data_cox |> filter(!is.na(Incubation_period)), 
                   "Incubation Period")

# Model D: Infectious Period
m_infect <- run_cox(Surv(time, status_binary) ~ Infectious_period,
                    data_cox |> filter(!is.na(Infectious_period)),
                    "Infectious Period")

# Model E: Immune protection vaccine
m_immune_vac <- run_cox(Surv(time, status_binary) ~ Immune_protection_vaccine,
                        data_cox |> filter(!is.na(Immune_protection_vaccine)),
                        "Vaccine immunity")

# Model F: Immune protection nature
m_immune_nat <- run_cox(Surv(time, status_binary) ~ Immune_protection_nature,
                        data_cox |> filter(!is.na(Immune_protection_nature)),
                        "Natural immunity")

# combine results
data_table <- bind_rows(m_group   |> mutate(category = "Categories"),
                        m_incub   |> mutate(category = "Incubation period"),
                        m_infect  |> mutate(category = "Infectious period"),
                        m_vaccine |> mutate(category = "Vaccination"),
                        m_immune_vac |> mutate(category = "Vaccine immunity"),
                        m_immune_nat |> mutate(category = "Natural immunity")) |> 
     mutate(term_clean = str_remove(term, "^Vaccine"),
            term_clean = str_remove(term_clean, "^Group"),
            term_clean = str_remove(term_clean, "^Infectious_period"),
            term_clean = str_remove(term_clean, "^Immune_protection_vaccine"),
            term_clean = str_remove(term_clean, "^Immune_protection_nature"),
            term_clean = str_replace(term_clean, "Incubation_period", "Incubation period (days)"))

data_fig2 <- bind_rows(tibble(estimate = NA,
                              category = c("Vaccination", "Categories", "Incubation period", "Infectious period", "Vaccine immunity", "Natural immunity"),
                              term = 'TITLE',
                              term_clean = ''),
                       tibble(estimate = 1,
                              category = c("Vaccination", "Categories", "Infectious period", "Vaccine immunity", "Natural immunity"),
                              term_clean = c("Unavaliable", "Respiratory IDs", "< 1 month", "None", "None")),
                       data_table) |> 
     mutate(order_id = row_number()) |>
     group_by(category) |>
     arrange(category, order_id) |> 
     ungroup()


# visualize table
data_table_vis <- data_fig2 |> 
     mutate(is_title = is.na(estimate),
            term_clean = case_when(term == 'TITLE' ~ category,
                                   TRUE ~ paste('  ', term_clean)),
            HR_str = case_when(is.na(term) ~ "Reference",
                               term == 'TITLE' ~ "",
                               TRUE ~ paste0(formatC(estimate, format = 'f', digits = 2),
                                             " (",
                                             formatC(conf.low, format = 'f', digits = 2),
                                             "-",
                                             formatC(conf.high, format = 'f', digits = 2),
                                             ")")),
            p_str = case_when(is.na(term) ~ "",
                              term == 'TITLE' ~ "",
                              p.value < 0.001 ~ "<0.001",
                              TRUE ~ formatC(p.value, format = 'f', digits = 3))) |>
     mutate(row_id = n():1) |> 
     select(row_id, term_clean, HR_str, p_str, is_title)

data_fig2 <- data_fig2 |> 
     mutate(order_id = n() - row_number() + 1,
            term = if_else(estimate == 1 | is.na(estimate), paste0(category, "_", term_clean), term))

bg_data <- data_fig2 |>
     group_by(category) |>
     summarise(ymin = min(order_id) - 0.5,
               ymax = max(order_id) + 0.5) |>
     arrange(ymin) |>
     mutate(fill_color = ifelse(row_number() %% 2 == 1, "grey95", "white"))

max_row <- max(data_table_vis$row_id)

fig2_table <- ggplot(data_table_vis, aes(y = row_id)) +
     geom_rect(data = bg_data, 
               aes(xmin = -0.1, xmax = 3, ymin = ymin, ymax = ymax, fill = fill_color), 
               alpha = 1, inherit.aes = FALSE) +
     scale_fill_identity()+
     geom_text(aes(x = 0, label = term_clean, fontface = ifelse(is_title, "bold", "plain")), 
               hjust = 0, size = 3.5) +
     geom_text(aes(x = 1.5, label = HR_str, fontface = ifelse(is_title, "bold", "plain")), 
               hjust = 0, size = 3.5) +
     geom_text(aes(x = 2.5, label = p_str, fontface = ifelse(is_title, "bold", "plain")), 
               hjust = 0, size = 3.5) +
     # title
     annotate('segment', x = -0.1, xend = 3, y = max(data_table_vis$row_id) + 0.5, yend = max(data_table_vis$row_id) + 0.5, color = "black", size = 0.5)+
     annotate('segment', x = -0.1, xend = 3, y = max(data_table_vis$row_id) + 1.5, yend = max(data_table_vis$row_id) + 1.5, color = "black", size = 0.5)+
     annotate('segment', x = -0.1, xend = 3, y = min(data_table_vis$row_id) - 0.5, yend = min(data_table_vis$row_id) - 0.5, color = "black", size = 0.5)+
     annotate('rect', xmin = -0.1, xmax = 3, ymin = max(data_table_vis$row_id) + 0.5, ymax = max(data_table_vis$row_id) + 1.5, fill = "grey80", size = 0.5)+
     annotate("text", x = 0, y = max(data_table_vis$row_id) + 1, label = "Variables", fontface = "bold", hjust = 0, size = 4) +
     annotate("text", x = 1.5, y = max(data_table_vis$row_id) + 1, label = "HR (95% CI)", fontface = "bold", hjust = 0, size = 4) +
     annotate("text", x = 2.5, y = max(data_table_vis$row_id) + 1, label = "P Value", fontface = "bold", hjust = 0, size = 4) +
     scale_x_continuous(limits = c(-0.1, 3),
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(0.5, max(data_table_vis$row_id) + 1.5),
                        expand = expansion(add = c(0, 0))) +
     theme_void()+
     # theme_classic()+
     theme(plot.margin = margin(5, 5, 5, 5))+
     labs(title = "B")

fig2_line <- ggplot(data_fig2) +
     geom_rect(data = bg_data, 
               aes(xmin = 0.1, xmax = 90, ymin = ymin, ymax = ymax, fill = fill_color), 
               alpha = 1, inherit.aes = FALSE) +
     scale_fill_identity()+
     geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
     geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = term), show.legend = F) +
     geom_point(aes(x = estimate, y = term), show.legend = F, size = 3) +
     annotate("text", x = 1, y = max(data_fig2$order_id) + 1, label = "Faster recovery", hjust = -0.1, size = 4) +
     annotate("text", x = 1, y = max(data_fig2$order_id) + 1, label = "Slower recovery", hjust = 1.1, size = 4) +
     scale_x_continuous(trans = "log10",
                        expand = c(0, 0),
                        breaks = c(0.1, 0.3, 1, 3, 10, 30, 90),
                        limits = c(0.1, 90)) +
     scale_y_discrete(expand = expansion(add = c(0.5, 1.5)),
                      limits = rev(data_fig2$term),
                      labels = rev(data_fig2$term_clean)) +
     labs(x = "Hazard ratio (95% CI)",
          y = NULL) +
     theme_bw() +
     theme(legend.position = "bottom",
           plot.margin = margin(5, 5, 5, 0),
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           panel.grid = element_blank(),
           plot.title.position = "plot")

# Save Figures ------------------------------------------------------------

fig <- free(fig1) + fig2_table +  fig2_line + plot_layout(widths = c(3, 2.3, 2.1), nrow = 1) &
     theme(plot.title.position = "plot")

# Save the visualization
ggsave("../Outcome/Publish/fig6.pdf", 
       fig, 
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 6)

ggsave("../Outcome/Publish/fig6.png", 
       fig, 
       limitsize = FALSE,
       width = 14, height = 6)

outcome <- list('panel A' = data_fig1,
                'panel B' = data_fig2)

write.xlsx(outcome,
           file = '../Outcome/Publish/figure_data/fig6.xlsx')
