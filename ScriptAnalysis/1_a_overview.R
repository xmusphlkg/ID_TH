# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)

# System setting
Sys.setlocale(locale = "EN")

# data --------------------------------------------------------------------

# loading function
source("./function/theme_set.R")

# list files in the folder: month case and death data
list_disease_files <- list.files("../Data/CleanData/",
                                  pattern = "mcd.csv",
                                  full.names = T)
data_month <- lapply(list_disease_files, read.csv) |>
     bind_rows() |> 
     filter(Year < 2024)

rm(list_disease_files)

data_select <- data_month |>
     filter(Year >= 2007) |>
     filter(Areas == 'Total' & Month == 'Total') |>
     group_by(Disease) |>
     summarise(Cases = sum(Count),
               Count = n(),
               .groups = 'drop')

data_class <- read.csv("../Data/DiseaseClass.csv") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

data_analysis <- data_month |>
     left_join(data_class, by = 'Disease') |>
     filter(Areas == 'Total' & Month != 'Total' & 
                 !is.na(Shortname) & Year >= 2007) |> 
     # transform the Month: Jan -> 01
     mutate(Month = month(parse_date_time(Month, "b"), label = FALSE, abbr = FALSE),
            Group = factor(Group, levels = disease_groups),
            Date = as.Date(paste(Year, Month, "01", sep = "-"))) |> 
     pivot_wider(names_from = Type, values_from = Count, values_fill = 0) |>
     filter(Date < as.Date("2024-07-01")) |> 
     ungroup() |> 
     arrange(Date, Disease)

# summary of NID ----------------------------------------------------------

## each group
data_analysis |>
     group_by(Group, Shortname) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               DateRange = paste(strftime(min(Date), "%Y/%m"),
                                 strftime(max(Date), "%Y/%m"),
                                 sep = " ~ "),
               .groups = "drop") |>
     group_by(Group) |>
     mutate(Cases_p = percent(round(Cases / sum(Cases), 4)),
            Deaths_p = percent(round(Deaths / sum(Deaths), 4)),
            CFR = percent(round(Deaths / Cases, 4)),
            .before = DateRange) |>
     arrange(Group, desc(Cases)) |>
     print(n = Inf)

# panel A & C -----------------------------------------------------------------

# fig1: cumulative cases

names(fill_color) <- disease_groups

fig1_data <- data_analysis |>
     select(Year, Disease = Shortname, Group, Cases, Deaths) |>
     group_by(Group, Disease) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               CFR = Deaths / Cases,
               .groups = 'drop') |> 
     arrange(Group, desc(Cases))

fig1 <- ggplot(data = fig1_data)+
     geom_col(mapping = aes(x = Disease, y = Cases, fill = Group),
              show.legend = T) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig1_data$Disease) +
     scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                        limits = c(0, NA),
                        label = scientific_10)+
     theme_set() +
     theme(legend.position = 'right',
           axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
     labs(y = "Cumulative cases",
          x = NULL,
          fill = "Categories",
          title = 'A')+
     guides(fill = guide_legend(byrow = TRUE))

# separate legend as a dependent plot
fig1_legend <- cowplot::get_legend(fig1)
fig1 <- fig1 + theme(legend.position = 'none')

fig1_in <- ggplot(data = filter(fig1_data, Cases <= 3e5))+
     geom_col(mapping = aes(x = Disease, y = Cases, fill = Group),
              show.legend = F) +
     coord_cartesian(ylim = c(0, 3e5)) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig1_data$Disease[fig1_data$Cases <= 3e5]) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        limits = c(0, 3e5),
                        breaks = c(0, 1e5, 2e5, 3e5),
                        label = scientific_10)+
     theme_set() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
           # transparent background
           panel.background = element_rect(fill = "transparent", color = NA),
           plot.background = element_rect(fill = "transparent", color = NA),
           legend.background = element_rect(fill = "transparent", color = NA)) +
     labs(y = NULL,
          x = NULL,
          fill = NULL)

# add inner plot
fig1 <- fig1 + inset_element(fig1_in, left = 0.05, bottom = 0.13, right = 1, top = 1.16)

remove(fig1_in)

fig3_data <- fig1_data

fig3 <- ggplot(data = fig3_data)+
     geom_col(mapping = aes(x = Disease, y = Deaths, fill = Group),
              show.legend = F) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig1_data$Disease) +
     scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                        limits = c(0, NA),
                        label = scientific_10)+
     theme_set() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
     labs(y = "Cumulative deaths",
          x = NULL,
          fill = NULL,
          title = 'C')

fig3_in <- ggplot(data = filter(fig1_data, Deaths <= 1e3))+
     geom_col(mapping = aes(x = Disease, y = Deaths, fill = Group),
              show.legend = F) +
     coord_cartesian(ylim = c(0, 1e3)) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig1_data$Disease[fig1_data$Deaths <= 1e3]) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        limits = c(0, 1e3),
                        breaks = c(0, 500, 1e3),
                        label = scientific_10)+
     theme_set() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
           # transparent background
           panel.background = element_rect(fill = "transparent", color = NA),
           plot.background = element_rect(fill = "transparent", color = NA),
           legend.background = element_rect(fill = "transparent", color = NA)) +
     labs(y = NULL,
          x = NULL,
          fill = NULL)

# add inner plot
fig3 <- fig3 + inset_element(fig3_in, left = 0.05, bottom = 0.13, right = 1, top = 1.16)

remove(fig3_in)

# panel B & D -------------------------------------------------------------

fig2_data <- data_analysis |>
     group_by(Date) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(Date)

# STL model for cases
stl_cases <- stl(ts(fig2_data$Cases, start = 2007, frequency = 12),
                 s.window = "periodic", t.window = 24, robust = T)
fig2_data$Cases_trend <- stl_cases$time.series[, 'trend']

# STL model for deaths
stl_deaths <- stl(ts(fig2_data$Deaths, start = 2007, frequency = 12),
                  s.window = "periodic", t.window = 24, robust = T)
fig2_data$Deaths_trend <- stl_deaths$time.series[, 'trend']


fig2 <- ggplot(data = fig2_data)+
     geom_point(mapping = aes(x = Date, y = Cases, color = 'Observed'),
                alpha = 0.5,
                size = 1.5) +
     geom_line(mapping = aes(x = Date, y = Cases_trend, color = 'Trend'),
               size = 1) +
     scale_color_manual(values = c('Observed' = 'grey', 'Trend' = 'grey50')) +
     scale_x_date(date_breaks = "4 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        breaks = c(0, 1e5, 2e5),
                        limits = c(0, 2e5),
                        label = scientific_10) +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(y = "Monthly cases",
          x = NULL,
          color = NULL,
          title = 'B')+
     guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(16, NA)),
                                 nrows = 1))

fig4_data <- fig2_data

fig4 <- ggplot(data = fig4_data)+
     geom_point(mapping = aes(x = Date, y = Deaths, color = 'Observed'),
                alpha = 0.5,
                size = 1.5) +
     geom_line(mapping = aes(x = Date, y = Deaths_trend, color = 'Trend'),
               size = 1) +
     scale_color_manual(values = c('Observed' = 'grey', 'Trend' = 'grey50')) +
     scale_x_date(date_breaks = "4 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0.03))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        limits = c(0, 300),
                        label = scientific_10) +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(y = "Monthly deaths",
          x = NULL,
          color = NULL,
          title = 'D')+
     guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(1, NA)),
                                 nrows = 1))

# appendix ----------------------------------------------------------------

# extract stl trend of each time series

plot_trend <- function(data, title, value = 'Cases') {
     ts_data <- ts(data[[value]], start = min(data$Year), frequency = 12)
     stl_data <- stl(ts_data, s.window = "periodic", t.window = 24, robust = T)
     data_trend <- data.frame(Year = data$Year,
                              Month = data$Month,
                              Value = data[[value]],
                              Trend = stl_data$time.series[, 'trend'])
     
     fig <- ggplot(data = data_trend) +
          geom_line(mapping = aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Trend),
                    color = '#4DBBD5FF',
                    size = 1) +
          geom_point(mapping = aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Value),
                     color = '#E64B35FF',
                     alpha = 0.5,
                     size = 1.5) +
          scale_x_date(date_breaks = "4 year",
                       date_labels = "%Y",
                       expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                             limits = c(0, NA),
                             labels = ifelse(max(ts_data) >= 100, scientific_10, scales::comma)) +
          theme_plot() +
          labs(x = NULL,
               y = NULL,
               title = title)
     
     return(fig)
}

# analysis of each group and disease

for (g in disease_groups) {
     # g <- disease_groups[1]
     d <- data_class$Shortname[data_class$Group == g]
     
     # panel A: trend of group cases
     data_group <- data_analysis |>
          filter(Group == g) |>
          group_by(Date, Year, Month) |>
          summarise(Cases = sum(Cases),
                    Deaths = sum(Deaths),
                    .groups = 'drop')
     fig_cases_g <- plot_trend(data_group, paste(LETTERS[1], g, sep = ": "), 'Cases')
     
     # panel B: trend of each disease
     data_single <- data_analysis |>
          filter(Group == g)
     fig_cases <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 1], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Cases')
     })
     
    
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/Cases ", g, ".png"),
            fig_cases_g + reduce(fig_cases, `+`) + plot_layout(widths = c(1, 3)),
            device = "png",
            width = 14, height = 7,
            limitsize = FALSE,
            dpi = 300)
     
     # panel C: trend of group deaths
     fig_deaths_g <- plot_trend(data_group, paste(LETTERS[1], g, sep = ": "), 'Deaths')
     
     # panel D: trend of each disease
     fig_deaths <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 1], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Deaths')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/Deaths ", g, ".png"),
            fig_deaths_g + reduce(fig_deaths, `+`) + plot_layout(widths = c(1, 3)),
            device = "png",
            width = 14, height = 7,
            limitsize = FALSE,
            dpi = 300)
}

# panel E -----------------------------------------------------------------

# fig5_data <- read.csv('../Data/WHO-COVID-19-global-data.csv') |> 
#      filter(Country == 'Thailand') |> 
#      select(Date_reported, New_cases) |> 
#      mutate(Date_reported = as.Date(Date_reported))
# 
# data_rect <- data.frame(start = c(min(fig5_data$Date_reported, na.rm = T), split_dates),
#                         end = c(split_dates, max(data_analysis$Date)),
#                         label = split_periods) |>
#      mutate(med = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01")) 
# 
# fig5 <- ggplot(data = fig5_data) +
#      geom_rect(data = data_rect,
#                aes(xmin = start, xmax = end, fill = label),
#                ymin = -Inf, ymax = Inf, alpha = 0.2,
#                show.legend = F) +
#      geom_line(mapping = aes(x = Date_reported, y = New_cases)) +
#      scale_fill_manual(values = back_color) +
#      scale_y_continuous(expand = c(0, 0),
#                         limits = c(0, 2e5),
#                         breaks = c(0, 1e5, 2e5),
#                         label = scientific_10) +
#      scale_x_date(expand = expansion(add = c(0, 15)),
#                   date_breaks = "1 years",
#                   date_labels = "%Y") +
#      theme_plot() +
#      labs(x = NULL,
#           y = "COVID-19 cases",
#           title = "E")

# panel F & G -------------------------------------------------------------

fig6_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Cases = sum(Cases),
               .groups = "drop")

fig6 <- ggplot(data = fig6_data) +
     geom_line(mapping = aes(x = Date,
                             y = Cases,
                             color = Group)) +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = back_color) +
     scale_y_continuous(expand = c(0, 0),
                        trans = "log10",
                        label = scientific_10,
                        limits = c(1e2, 2e5),
                        breaks = c(1e2, 1e3, 1e4, 1e5, 2e5)) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig6_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(x = NULL,
          y = "Monthly cases",
          color = NULL,
          title = "E")

fig7_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Cases = sum(Cases),
               .groups = "drop") |> 
     group_by(Date) |>
     mutate(Cases = Cases / sum(Cases))

fig7 <- ggplot(data = fig7_data) +
     geom_col(mapping = aes(x = Date, y = Cases, fill = Group),
              show.legend = F,
              color = NA,
              position = "fill") +
     scale_fill_manual(values = fill_color) +
     scale_y_continuous(expand = c(0, 0),
                        labels = scales::percent) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig7_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     labs(x = NULL,
          y = "Percentage",
          title = "F",
          fill = NULL)


# panel H & I -------------------------------------------------------------

fig8_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Deaths = sum(Deaths),
               .groups = "drop")

fig8 <- ggplot(data = fig8_data) +
     geom_line(mapping = aes(x = Date,
                             y = Deaths,
                             color = Group)) +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = back_color) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        limits = c(0, 300)) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig8_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = "none") +
     labs(x = NULL,
          y = "Monthly deaths",
          color = NULL,
          title = "G")

fig9_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Deaths = sum(Deaths),
               .groups = "drop") |> 
     group_by(Date) |>
     mutate(Deaths = Deaths / sum(Deaths))

fig9 <- ggplot(data = fig9_data) +
     geom_col(mapping = aes(x = Date, y = Deaths, fill = Group),
              show.legend = F,
              color = NA,
              position = "fill") +
     scale_fill_manual(values = fill_color) +
     scale_y_continuous(expand = c(0, 0),
                        labels = scales::percent) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig9_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = "none") +
     labs(x = NULL,
          y = "Percentage",
          title = "H",
          fill = NULL)

# save plot ---------------------------------------------------------------

fig <- cowplot::plot_grid(cowplot::plot_grid(fig1/fig3, cowplot::plot_grid(fig1_legend) / fig2 / fig4, nrow = 1, rel_widths = c(3, 1)),
                          cowplot::plot_grid(fig6, fig7, nrow = 1, rel_widths = c(1, 1)),
                          cowplot::plot_grid(fig8, fig9, nrow = 1, rel_widths = c(1, 1)),
                          nrow = 3,
                          ncol = 1,
                          rel_heights = c(2.5, 1, 1))

ggsave(filename = "../Outcome/Publish/fig1.pdf",
       fig,
       width = 14,
       height = 14,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
data_fig <- list("panel A" = fig1_data,
                 "panel B" = fig2_data,
                 "panel C" = fig3_data,
                 "panel D" = fig4_data,
                 "panel E" = fig6_data,
                 "panel F" = fig7_data,
                 "panel G" = fig8_data,
                 "panel H" = fig9_data)

write.xlsx(data_fig,
           file = "../Outcome/Appendix/figure_data/fig1.xlsx")

data_class <- fig1_data |> 
     rename(Shortname = 'Disease')

save(data_analysis, data_month, data_class, file = "./month.RData")
