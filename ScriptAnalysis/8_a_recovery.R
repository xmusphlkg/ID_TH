#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-01-16 19:20:37
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-01-16 19:25:38
#####################################

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

# load predict dataset
data_predict <- read.xlsx('../Data/TotalCasesDeaths.xlsx', sheet = 'Predictors') |> 
     mutate(R0_mean = (R0_range_min + R0_range_max) / 2,
            R0_mean = case_when(is.na(R0_mean) & R0_refer == 'Not transmitted directly' ~ 0,
                                is.na(R0_mean) & R0_refer == 'Uncertain' ~ NA_real_,
                                TRUE ~ R0_mean),
            Vaccine = factor(Vaccine, levels = c("Unavaliable", "Optional", "EPI")),
            Immune_protection = factor(Immune_protection, levels = c("None", "Short-term", "Long-term")),
            Group = factor(Group, levels = c("Respiratory IDs", "Gastrointestinal IDs", "Sexually IDs", "Vector-borne and zoonotic IDs"))) |> 
     select(Shortname, R0_mean, Vaccine, Group, Incubation_period, Immune_protection)


# Merge everything into a Master Analysis Dataframe
df_analysis <- df_metrics |>
     left_join(data_predict, by = "Shortname")


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

# Model E: Immune protection
m_immune <- run_cox(Surv(time, status_binary) ~ Immune_protection,
                    data_cox |> filter(!is.na(Immune_protection)),
                    "Immune protection")

# combine results
data_fig4 <- bind_rows(
     m_vaccine |> mutate(category = "Vaccination"),
     m_group   |> mutate(category = "Categories"),
     m_r0      |> mutate(category = "R0"),
     m_incub   |> mutate(category = "Incubation period"),
     m_immune  |> mutate(category = "Immunity")
) |> 
     mutate(term_clean = str_remove_all(term, "Vaccine|Group|Immune_protection"),
            term_clean = case_when(term_clean == "R0_mean" ~ "Reproductive number",
                                   term_clean == "Incubation_period" ~ "Incubation period",
                                   TRUE ~ term_clean)) |>
     # add control group
     bind_rows(
          tibble(estimate = 1,
                 category = c("Vaccination", "Categories", "Immunity"),
                 term_clean = c("Unavaliable", "Respiratory IDs", "None"))
     ) |> 
     mutate(term_clean = factor(term_clean, levels = rev(c("Reproductive number", "Incubation period",
                                                           "None", "Short-term", "Long-term",
                                                           "Unavaliable", "Optional", "EPI",
                                                           "Respiratory IDs", "Gastrointestinal IDs", "Sexually IDs", "Vector-borne and zoonotic IDs"))),
            category = factor(category, levels = c('R0', 'Incubation period', 'Immunity', 'Vaccination', 'Categories'))) |> 
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
