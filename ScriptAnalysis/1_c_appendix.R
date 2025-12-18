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

load("./temp/month.RData")

data_group <- data_month |>
     group_by(Date, Group) |>
     summarise(Cases = sum(Cases, na.rm = T),
               Deaths = sum(Deaths, na.rm = T),
               .groups = "drop")

# line plot ----------------------------------------------------------------

plot_single <- function(i, value = 'Cases') {
     group_list <- disease_groups[i]
     data_group_single <- data_group |>
          filter(Group == group_list) |> 
          rename(value = !!sym(value)) |> 
          mutate(Year = year(Date),
                 Month = month(Date))
     
     ts_data <- ts(data_group_single$value, start = min(data_group_single$Year), frequency = 12)
     stl_data <- stl(ts_data, s.window = "periodic", t.window = 24, robust = T)
     data_trend <- data.frame(Year = data_group_single$Year,
                              Month = data_group_single$Month,
                              Value = data_group_single$value,
                              Trend = stl_data$time.series[, 'trend']) |> 
          mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
     
     fig1 <- ggplot(data = data_group_single) +
          geom_line(mapping = aes(x = Date, y = value, color = 'Observed'),
                    show.legend = T) +
          geom_line(mapping = aes(x = Date, y = Trend, color = 'STL trend'),
                    data = data_trend,
                    show.legend = T) +
          scale_x_date(expand = expansion(add = c(15, 15)),
                       date_breaks = "1 year",
                       date_labels = "%Y") +
          scale_y_continuous(name = "Monthly cases",
                             expand = expansion(mult = c(0, 0.25)),
                             limits = c(0, NA),
                             breaks = pretty(c(0, max(data_group_single$value)*1.25), n = 4),
                             labels = scientific_10
                             ) +
          scale_color_manual(values = c('Observed' = fill_color[2],
                                        'STL trend' = fill_color[1])) +
          theme_bw() +
          theme(aspect.ratio = 1/5,
                axis.text.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text = element_text(size = 10.5, color = "black"),
                axis.title.y = element_text(size = 11, color = "black", face = "bold", vjust = -1),
                plot.title = element_text(face = "bold", size = 14, color = "black")) +
          labs(x = NULL,
               y = ifelse(value == 'Cases', "Monthly cases", "Monthly deaths"),
               color = NULL,
               title = LETTERS[1])
     
     data_disease_single <- data_month |>
          filter(Group == group_list) |> 
          rename(value_rate = ifelse(value == 'Cases', 'Incidence', 'Mortality')) |>
          group_by(Shortname) |>
          mutate(value_norm = (value_rate - min(value_rate)) / sd(value_rate),
                 value_norm = ifelse(value_rate == 0, 0, value_norm),
                 Date = format(ymd(Date), "%Y.%m"),
                 out_label = ifelse(value_norm > 10, "*", NA)) |> 
          ungroup()
          
     fig2 <- ggplot(data = data_disease_single,
                    mapping = aes(fill = value_norm, x = Date, y = Shortname)) +
          geom_tile() +
          geom_text(mapping = aes(label = out_label),
                    vjust = 0.5) +
          coord_equal(3) +
          scale_fill_gradientn(colors = paletteer_d("rcartocolor::Temps"),
                               trans = log_fill,
                               na.value = 'white',
                               limits = c(-5, 10)) +
          scale_x_discrete(breaks = paste(seq(2008, 2025), "01", sep = "."),
                           labels = 2008:2025,
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = rev(data_class$Shortname[data_class$Group == group_list]),
                           expand = c(0, 0)) +
          theme_bw() +
          theme(legend.position = "bottom",
                panel.grid = element_blank(),
                axis.text = element_text(size = 10.5, color = "black"),
                plot.title = element_text(face = "bold", size = 14, color = "black")) +
          guides(fill = guide_colourbar(barwidth = 10,
                                        barheight = 0.5)) +
          labs(x = "Date",
               y = NULL,
               fill = ifelse(value == 'Cases', "Normalized incidence", "Normalized mortality"),
               title = LETTERS[2])
     
     return(fig1 + fig2 + plot_layout(ncol = 1, guides = 'collect') &
               theme(legend.position = "bottom"))
}

## create figure panel for all Group
plot_list_cases <- lapply(1:length(disease_groups), plot_single, 'Cases')
plot_list_deaths <- lapply(1:length(disease_groups), plot_single, 'Deaths')

# trend -------------------------------------------------------------------

# extract stl trend of each time series
plot_trend <- function(data, title, value = 'Incidence') {
     ts_data <- ts(data[[value]], start = min(data$Year), frequency = 12)
     stl_data <- stl(ts_data, s.window = "periodic", t.window = 24, robust = T)
     data_trend <- data.frame(Year = data$Year,
                              Month = data$Month,
                              Value = data[[value]],
                              Trend = stl_data$time.series[, 'trend']) |> 
          mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
     
     # browser()
     plot_breaks_y <- pretty(c(data_trend$Value, data_trend$Trend), n = 4)
     
     fig <- ggplot(data = data_trend) +
          geom_line(mapping = aes(x = Date, y = Trend),
                    color = fill_color[1]) +
          geom_point(mapping = aes(x = Date, y = Value),
                     color = fill_color[2],
                     alpha = 0.5) +
          scale_x_date(date_breaks = "4 year",
                       date_labels = "%Y",
                       expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0)),
                             limits = range(plot_breaks_y),
                             breaks = plot_breaks_y,
                             labels = ifelse(max(ts_data) >= 100, scientific_10, scales::comma)) +
          scale_color_manual(values = c('Observed' = fill_color[2],
                                        'STL trend' = fill_color[1])) +
          theme_bw() +
          labs(x = 'Date',
               y = paste(value, '(per 100,000)'),
               title = title)
     
     return(fig)
}

for (i in 1:length(disease_groups)) {
     ## incidence ----------------------------------------------------
     g <- disease_groups[i]
     d <- data_class$Shortname[data_class$Group == g]
     
     # panel B: trend of each disease
     data_single <- data_month |>
          filter(Group == g)
     fig_cases <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 2], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Incidence')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_2/Cases ", g, ".png"),
            cowplot::plot_grid(
                 plot_list_cases[[i]],
                 wrap_plots(fig_cases, ncol = 4),
                 ncol = 1
            ),
            device = "png",
            width = 14,
            height = 7 + ceiling(length(d) / 4) * 2 + 1,
            limitsize = FALSE,
            dpi = 300)
     
     ## mortality ----------------------------------------------------
     # drop diseases without deaths
     d <- data_single |>
          filter(Deaths > 0) |> 
          pull(Shortname) |> 
          unique()
     
     # panel D: trend of each disease
     fig_deaths <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 2], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Mortality')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_2/Deaths ", g, ".png"),
            cowplot::plot_grid(
                 plot_list_deaths[[i]],
                 wrap_plots(fig_deaths, ncol = 4),
                 ncol = 1
            ),
            device = "png",
            width = 14,
            height = 7 + ceiling(length(d) / 4) * 2 + 1,
            limitsize = FALSE,
            dpi = 300)
}
