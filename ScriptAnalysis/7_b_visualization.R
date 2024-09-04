# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)
library(ggh4x)

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load('./outcome.RData')
load('./month.RData')

data_class$id <- 1:nrow(data_class)

# panel -------------------------------------------------------------------

## plot single panel of disease 

plot_single_panel <- function(i, g, outcome){
     # related data
     outcome_data <- outcome[[i]]$outcome_data
     outcome_plot_1 <- outcome[[i]]$outcome_plot_1
     outcome_plot_2 <- outcome[[i]]$outcome_plot_2
     max_value <- outcome[[i]]$max_value
     min_value <- outcome[[i]]$min_value
     max_case <- outcome[[i]]$max_case
     
     index <- which(data_class$id[data_class$Group == disease_groups[g]] == i)+1
     
     plot_breaks <- pretty(c(min_value, max_value, 0))
     ylab <- ifelse(index %in% c(2, 6, 10), "Monthly cases", "")
     
     fig <- ggplot() +
          geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed"),
                    linewidth = 0.7) +
          geom_line(data = outcome_plot_2, mapping = aes(x = date, y = mean, colour = "Forecasted"),
                    linewidth = 0.7) +
          stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = mean),
                          alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = T) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_dates[1] - 365, NA)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "1 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = ifelse(max_case > 1000, scientific_10, scales::comma),
                             breaks = plot_breaks,
                             limits = range(plot_breaks)) +
          scale_color_manual(values = c(Forecasted = "#004F7AFF", Observed = "#CC3D24FF")) +
          scale_fill_manual(values = c(Decreased = "#004F7A50", Increased = "#CC3D2450"),
                            drop = F) +
          theme_set() +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = 'vertical',
                plot.title.position = "plot",
                legend.text = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = "bold"))+
          labs(x = NULL,
               y = ylab,
               color = NULL,
               fill = NULL,
               title = paste0(LETTERS[index], ": ", data_class$Shortname[i]))+
          guides(color = guide_legend(order = 1, override.aes = list(fill = NA)),
                 fill = guide_legend(order = 2))
     
     return(fig)
}

## plot panel for disease group
# g <- 1

plot_group_panel <- function(g, data_class){
     # read data of multiple diseases from outcome
     disease_id <- data_class$id[data_class$Group == disease_groups[g]]
     disease_name <- data_class$Shortname[data_class$Group == disease_groups[g]]
     
     data_group <- lapply(disease_id, function(i) outcome[[i]]$outcome_data) |> 
          bind_rows() |> 
          mutate(diff_percent = round((mean + add_value) / (value + add_value), 3),
                 diff = round(mean - value, 0),
                 label = if_else(diff_percent > 10, "*", ""),
                 date_num = format(ymd(date), "%Y.%m"))
     
     fig_group <- ggplot(data = data_group,
                         mapping = aes(fill = diff_percent, x = date_num, y = Shortname)) +
          geom_tile() +
          # geom_text(mapping = aes(label = label), vjust = 0.5) +
          scale_fill_gradientn(colors = fill_color_continue,
                               trans = log_fill,
                               breaks = c(0, 0.5, 1, 5, 10),
                               labels = c(0, 0.5, 1, 5, '>10'),
                               na.value = "#009392FF",
                               limits = c(0, 10)) +
          scale_x_discrete(breaks = paste(unique(year(data_group$date)), "01", sep = "."),
                           labels = unique(year(data_group$date)),
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = rev(disease_name),
                           expand = c(0, 0)) +
          theme_plot() +
          theme(legend.position = "right",
                legend.text = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = "bold"),
                plot.title = element_text(size = 14, hjust = 0, vjust = 0.5, face = "bold"),
                plot.title.position = "plot") +
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10, color = "black", title.position = "top")) +
          labs(x = NULL,
               y = NULL,
               fill = "IRR",
               title = paste0(LETTERS[1], ": ", disease_groups[g]))
     
     fig_disease <- lapply(disease_id, plot_single_panel, g = g, outcome = outcome)
     
     if (length(disease_id) == 6) {
          design <- "\nBCDE\nFGHI"
          fig_h <- 9
          rel_heights <- c(1, 2)
     } else if (length(disease_id) == 7) {
          design <- "\nBCDE\nFGHI"
          fig_h <- 9
          rel_heights <- c(1, 2)
     } else if (length(disease_id) == 8) {
          design <- "\nBCDE\nFGHI"
          fig_h <- 10
          rel_heights <- c(1, 2.1)
     } else if (length(disease_id) == 9) {
          design <- "\nBCDE\nFGHI\nJKLN"
          fig_h <- 12
          rel_heights <- c(1, 3)
     }
     
     if (fig_h == 10) {
          fig_disease <- fig_disease |>
               reduce(`+`) +
               plot_layout(design = design, guides = "collect")&
               theme(legend.position = "bottom")
     } else {
          fig_disease <- fig_disease |> 
               reduce(`+`) +
               guide_area() +
               plot_layout(design = design, guides = "collect")
     }
     
     fig <- cowplot::plot_grid(fig_group, fig_disease, ncol = 1, rel_heights = rel_heights)
     
     ggsave(filename = paste0("../outcome/publish/fig", g+6, ".pdf"),
            plot = fig,
            width = 12,
            height = fig_h,
            device = cairo_pdf,
            family = "Times New Roman")
     
     return(data_group)
}

for (g in 1:length(disease_groups)) {
     data <- plot_group_panel(g, data_class)
     
     write.xlsx(data,
                file = paste0("../Outcome/Appendix/figure_data/fig", g+6, ".xlsx"),
                asTable = T)
}
