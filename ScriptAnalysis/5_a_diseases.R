# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)
library(tseries)
library(scales)

# data --------------------------------------------------------------------

remove(list = ls())

source("./function/theme_set.R")

load("./month.RData")

# group plot -------------------------------------------------------------

data_plot <- data_analysis |>
     filter(Shortname %in% data_class$Shortname) |>
     select(Date, Shortname, Group, Cases, Deaths) |>
     mutate(
          Shortname = factor(
               Shortname,
               levels = data_class$Shortname,
               labels = data_class$Shortname
          ),
          phase = case_when(
               Date < split_dates[1] ~ split_periods[1],
               Date >= split_dates[1] &
                    Date < split_dates[2] ~ split_periods[2],
               Date >= split_dates[2] &
                    Date < split_dates[3] ~ split_periods[3],
               Date >= split_dates[3] &
                    Date < split_dates[4] ~ split_periods[4],
               Date >= split_dates[4] ~ split_periods[5]
          ),
          phase = factor(phase, levels = split_periods),
          Cases = as.integer(Cases)
     )

# background rect ---------------------------------------------------------

data_rect <- data.frame(start = c(min(data_plot$Date), split_dates),
                            end = c(split_dates, max(data_plot$Date)),
                            label = split_periods) |>
     mutate(m = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01"))

data_group <- data_plot |>
     group_by(phase, Date, Group) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = "drop")

# complete missing data
table(data_plot[, "Shortname"])

# line plot ----------------------------------------------------------------

data_fig <- list()

names(fill_color) <- disease_groups

for (i in 1:length(disease_groups)) {
     group_list <- disease_groups[i]
     data_fig[[paste("panel", LETTERS[i * 2 - 1])]] <- data_group |>
          filter(Group == group_list) |>
          mutate(year = year(Date),
                 month = month(Date),
                 Date = format(ymd(Date), "%Y.%m"))
     data_fig[[paste("panel", LETTERS[i * 2])]] <- data_plot |>
          filter(Group == group_list) |>
          group_by(Shortname) |>
          mutate(year = year(Date),
                 month = month(Date),
                 value_norm = (Cases - mean(Cases, na.rm = T)) / sd(Cases, na.rm = T),
                 Date = format(ymd(Date), "%Y.%m"))
     # data_fig[[paste("panel", LETTERS[i * 3 - 0])]] <- data_plot |>
     #      filter(Group == group_list) |>
     #      group_by(Shortname) |>
     #      mutate(year = year(Date),
     #             month = month(Date),
     #             value_norm = (Deaths - mean(Deaths, na.rm = T)) / sd(Deaths, na.rm = T),
     #             Date = format(ymd(Date), "%Y.%m"))
}

plot_single <- function(i) {
     group_list <- disease_groups[i]
     data_group_single <- data_group |>
          filter(Group == group_list)
     
     scale_factor <- max(data_group_single$Cases) / max(data_group_single$Deaths)
     fig1 <- ggplot(data = data_group_single) +
          geom_rect(data = data_rect,
                    mapping = aes(xmin = start, xmax = end, fill = label),
                    ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
          # geom_line(mapping = aes(x = Date, y = Cases, color = 'Incidence')) +
          # geom_line(mapping = aes(x = Date, y = Deaths * scale_factor, color = 'Deaths'))+
          geom_line(mapping = aes(x = Date, y = Cases, color = Group),
                    show.legend = F) +
          scale_x_date(expand = expansion(add = c(15, 15)),
                       date_breaks = "1 year",
                       date_labels = "%Y") +
          scale_y_continuous(name = "Monthly Incidence",
                             expand = expansion(mult = c(0, 0.25)),
                             limits = c(0, NA),
                             breaks = pretty(c(0, max(data_group_single$Cases)*1.25), n = 4),
                             labels = scientific_10,
                             sec.axis = sec_axis(~ . / scale_factor,
                                                 name = "Monthly Deaths")) +
          scale_fill_manual(values = back_color) +
          scale_color_manual(values = fill_color)+
          theme_bw() +
          theme(legend.position = "none",
                axis.text.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text = element_text(size = 10.5, color = "black"),
                axis.title.y = element_text(size = 11, color = "black", face = "bold"),
                plot.title = element_text(face = "bold", size = 12, color = "black"),
                plot.title.position = "plot") +
          labs(x = NULL,
               y = "Monthly cases",
               color = NULL,
               title = LETTERS[i * 2 - 1])
     
     data_plot_cases <- data_fig[[paste("panel", LETTERS[i * 2])]] |>
          mutate(out_label = if_else(value_norm > 10, "*", ""))
     fig2 <- ggplot(data = data_plot_cases,
                    mapping = aes(fill = value_norm, x = Date, y = Shortname)) +
          geom_tile() +
          geom_text(mapping = aes(label = out_label),
                    vjust = 0.5) +
          coord_equal(3) +
          scale_fill_gradientn(colors = paletteer_d("rcartocolor::Temps"),
                               trans = log_fill,
                               na.value = 'white',
                               limits = c(-5, 10)) +
          scale_x_discrete(breaks = paste(seq(2007, 2024), "01", sep = "."),
                           labels = 2007:2024,
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = rev(data_class$Shortname[data_class$Group == group_list]),
                           expand = c(0, 0)) +
          theme_bw() +
          theme(legend.position = "bottom",
                panel.grid = element_blank(),
                axis.text = element_text(size = 10.5, color = "black"),
                plot.title = element_text(face = "bold", size = 12, color = "black"),
                plot.title.position = "plot") +
          guides(fill = guide_colourbar(barwidth = 10,
                                        barheight = 0.5,
                                        color = "black")) +
          labs(x = "Date",
               y = NULL,
               fill = "Normalized monthly cases",
               title = LETTERS[i * 2])
     
     # data_plot_deaths <- data_fig[[paste("panel", LETTERS[i * 3])]] |>
     #      mutate(out_label = if_else(value_norm > 10, "*", ""))
     # fig3 <- ggplot(data = data_plot_deaths,
     #                mapping = aes(fill = value_norm, x = Date, y = Shortname)) +
     #      geom_tile() +
     #      geom_text(mapping = aes(label = out_label),
     #                vjust = 0.5) +
     #      coord_equal(3) +
     #      scale_fill_gradientn(colors = paletteer_d("rcartocolor::Fall"),
     #                           trans = log_fill,
     #                           na.value = 'white',
     #                           limits = c(-5, 10)) +
     #      scale_x_discrete(breaks = paste(seq(2007, 2024), "01", sep = "."),
     #                       labels = 2007:2024,
     #                       expand = expansion(add = c(0, 0))) +
     #      scale_y_discrete(limits = rev(data_class$Shortname[data_class$Group == group_list]),
     #                       expand = c(0, 0)) +
     #      theme_bw() +
     #      theme(legend.position = "bottom",
     #            panel.grid = element_blank(),
     #            axis.text = element_text(size = 10.5, color = "black"),
     #            plot.title = element_text(face = "bold", size = 12, color = "black"),
     #            plot.title.position = "plot") +
     #      guides(fill = guide_colourbar(barwidth = 10,
     #                                    barheight = 0.5,
     #                                    color = "black")) +
     #      labs(x = "Date",
     #           y = NULL,
     #           fill = "Normalized monthly deaths",
     #           title = LETTERS[i * 3])
     
     return(fig1 + fig2 + plot_layout(ncol = 1, heights = c(1, 1)))
}

## create figure panel for all Group
plot_list <- lapply(1:length(disease_groups), plot_single)
fig <- wrap_plots(plot_list, ncol = 1) +
     plot_layout(guides = "collect") &
     theme(legend.position = "bottom")

ggsave(filename = "../outcome/publish/fig5.pdf",
       plot = fig,
       width = 12,
       height = 20,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
write.xlsx(data_fig,
           file = "../outcome/Appendix/figure_data/fig5.xlsx")
