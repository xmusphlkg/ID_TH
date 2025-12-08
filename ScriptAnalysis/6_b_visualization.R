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

load('./month.RData')
load('./outcome.RData')
load('./best_model_figure.RData')

data_class <- data_class |> 
     mutate(label = paste0(int2col(id + 1), ": ", Shortname))

# panel -------------------------------------------------------------------

## plot single panel of disease 

plot_single_panel <- function(i){
     # related data
     outcome_data <- outcome[[i]]$outcome_data
     outcome_plot_1 <- outcome[[i]]$outcome_plot_1
     outcome_plot_2 <- outcome[[i]]$outcome_plot_2
     max_value <- outcome[[i]]$max_value
     min_value <- outcome[[i]]$min_value
     max_case <- outcome[[i]]$max_case
     
     plot_breaks <- pretty(c(min_value, max_value, 0), n = 3)
     
     fig <- ggplot() +
          geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed")) +
          geom_line(data = outcome_plot_2, mapping = aes(x = date, y = mean, colour = "Forecasted")) +
          stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = mean),
                          alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = T) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_dates[1] - 365, NA)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "2 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = ifelse(max_case > 1000, scientific_10, scales::comma),
                             breaks = plot_breaks,
                             limits = range(plot_breaks)) +
          scale_color_manual(values = c(Forecasted = "#004F7AFF", Observed = "#CC3D24FF")) +
          scale_fill_manual(values = c(Decreased = "#004F7A50", Increased = "#CC3D2450"),
                            drop = F) +
          theme_bw() +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = 'vertical',
                legend.title.position = 'top',
                axis.text.y = element_text(angle = 90, hjust = 0.5),
                panel.grid = element_blank(),
                plot.title = element_text(face = 'bold', size = 14, hjust = 0))+
          labs(x = 'Date',
               y = 'Monthly cases',
               color = 'Monthly cases',
               fill = 'Difference',
               title = data_class$label[i]) +
          guides(color = guide_legend(order = 1, override.aes = list(fill = NA)),
                 fill = guide_legend(order = 2))
     
     return(fig)
}

plot_group_panel <- function(g){
     # read data of multiple diseases from outcome
     disease_id <- data_class$id[data_class$Group == disease_groups[g]]
     disease_name <- data_class$disease[data_class$Group == disease_groups[g]]
     
     data_group <- lapply(disease_id, function(i) outcome[[i]]$outcome_data) |> 
          bind_rows() |> 
          mutate(diff_percent = round((value + add_value) / (mean + add_value), 3),
                 diff = round(mean - value, 0),
                 label = if_else(diff_percent > 10, "*", ""),
                 date_num = format(ymd(date), "%Y.%m"))
     
     fig_group <- ggplot(data = data_group,
                         mapping = aes(fill = diff_percent, x = date_num, y = Shortname)) +
          geom_tile() +
          # geom_text(mapping = aes(label = label), vjust = 0.5) +
          scale_fill_gradientn(colors = paletteer_d("LaCroixColoR::Lemon", direction = -1),
                               trans = log_fill,
                               breaks = c(0, 0.5, 1, 5, 10),
                               labels = c(0, 0.5, 1, 5, '>10'),
                               na.value = fill_color_continue[1],
                               limits = c(0, 10)) +
          scale_x_discrete(breaks = paste(unique(year(data_group$date)), "01", sep = "."),
                           labels = unique(year(data_group$date)),
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = rev(disease_name),
                           expand = c(0, 0)) +
          theme_bw() +
          theme(legend.position = "right",
                legend.text = element_text(size = 10, hjust = 0.5, vjust = 0.5, face = "bold"),
                plot.title = element_text(size = 14, hjust = 0, vjust = 0.5, face = "bold"),
                plot.title.position = "plot") +
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10, color = "black", title.position = "top")) +
          labs(x = NULL,
               y = NULL,
               fill = "IRR",
               title = paste0(LETTERS[1], ": ", disease_groups[g]))
     
     return(fig_group)
}

# save --------------------------------------------------------------------

fig2 <- lapply(1:nrow(data_class), plot_single_panel) |> 
     wrap_plots(ncol = 6, guides = 'collect', axis_titles = 'collect') &
     theme(legend.position = 'bottom')

plot <- cowplot::plot_grid(fig1, fig2,
                           nrow = 1,
                           rel_widths = c(1.5, 7))

ggsave("../Outcome/Publish/fig4.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 10)

ggsave("../Outcome/Publish/fig4.png",
       plot,
       limitsize = FALSE,
       width = 14, height = 10)

data_outcome <- lapply(1:length(outcome), function(x) outcome[[x]]$outcome_data)

data_outcome <- append(list(data_map), data_outcome)

write.xlsx(data_outcome,
           file = "../Outcome/Publish/figure_data/fig4.xlsx")

