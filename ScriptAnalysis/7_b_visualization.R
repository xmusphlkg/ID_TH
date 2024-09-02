# panel -------------------------------------------------------------------

## plot single panel of disease 

plot_single_panel <- function(i, index, outcome){
     # related data
     outcome_data <- outcome[[i]]$outcome_data
     data_rect <- outcome[[i]]$data_rect
     outcome_plot_1 <- outcome[[i]]$outcome_plot_1
     outcome_plot_2 <- outcome[[i]]$outcome_plot_2
     max_value <- outcome[[i]]$max_value
     min_value <- outcome[[i]]$min_value
     max_case <- outcome[[i]]$max_case
     
     plot_breaks <- pretty(c(min_value, max_value, 0))
     
     fig <- ggplot() +
          geom_vline(xintercept = data_rect$end, show.legend = F,
                     color = "grey", linetype = "longdash") +
          geom_rect(data = data_rect, mapping = aes(xmin = start, xmax = end, fill = label),
                    ymax = 0, ymin = max(plot_breaks) / 10, alpha = 0.2, show.legend = F) +
          geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed"),
                    linewidth = 0.7) +
          geom_line(data = outcome_plot_2, mapping = aes(x = date, y = mean, colour = "Forecasted"),
                    linewidth = 0.7) +
          stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = mean),
                          alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = F) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_dates[1] - 365, NA)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "1 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = plot_breaks,
                             limits = range(plot_breaks)) +
          scale_color_manual(values = c(Forecasted = "#004F7AFF", Observed = "#CC3D24FF")) +
          scale_fill_manual(values = c(Decreased = "#004F7A50", Increased = "#CC3D2450", back_color)) +
          theme_set() +
          theme(legend.position = "bottom") +
          labs(x = NULL,
               y = "Monthly cases",
               color = "",
               title = paste0(LETTERS[index], ": ", data_class$disease[i]))
     
     return(fig)
}

## plot panel for disease group
g <- 1

plot_group_panel <- function(g, data_class){
     # read data of multiple diseases from outcome
     disease_name <- data_class$id[data_class$Group == disease_groups[g]]
     
     data_group <- lapply(disease_name, function(i) outcome[[i]]$outcome_data) |> 
          bind_rows() |> 
          mutate(diff_percent = round((mean + add_value) / (value + add_value), 2),
                 diff = round(mean - value, 2))
     
     
     
     plot_breaks <- pretty(c(min_value, max_value, 0))
     
     fig <- ggplot() +
          geom_vline(xintercept = data_rect$end, show.legend = F,
                     color = "grey", linetype = "longdash") +
          geom_rect(data = data_rect, mapping = aes(xmin = start, xmax = end, fill = label),
                    ymax = 0, ymin = max(plot_breaks) / 10, alpha = 0.2, show.legend = F) +
          geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed"),
                    linewidth = 0.7) +
          geom_line(data = outcome_plot_2, mapping = aes(x = date, y = mean, colour = "Forecasted"),
                    linewidth = 0.7) +
          stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = mean),
                          alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = F) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_dates[1] - 365, NA)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "1 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = plot_breaks,
                             limits = range(plot_breaks))
}




plot <- do.call(wrap_plots, outcome) +
     plot_layout(design = layout, guides = "collect") &
     theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
     )

ggsave(
     "./outcome/publish/fig4.pdf",
     plot,
     family = "Times New Roman",
     limitsize = FALSE,
     device = cairo_pdf,
     width = 25,
     height = 14
)

# merge data file ---------------------------------------------------------

file_list <- paste0("./outcome/appendix/forecast/",
                    datafile_class$disease,
                    ".xlsx")
data_list <- lapply(file_list, read.xlsx, detectDates = T)
names(data_list) <- paste0(LETTERS[1:24], " ", datafile_class$disease)
write.xlsx(data_list,
           file = "./outcome/appendix/Figure Data/Fig.4 data.xlsx")