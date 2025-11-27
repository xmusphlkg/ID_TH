# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)
library(nih.joinpoint)
library(furrr)

# System setting
Sys.setlocale(locale = "EN")

# Options for joinpoint
run_opt = run_options(model="ln",
                      model_selection_method = 'wbic-alt',
                      min_joinpoints = 0,
                      max_joinpoints = 5,
                      n_cores=30)
export_opt = export_options()

# future plan for parallel computing
plan(multisession, workers = 10)

# data --------------------------------------------------------------------

# loading function
source("./function/theme_set.R")

# list files in the folder: month case and death data
list_disease_files <- list.files("../Data/CleanData/",
                                  pattern = "mcd.csv",
                                  full.names = T)
data_month <- lapply(list_disease_files, read.csv) |>
     bind_rows() |> 
     filter(Year < 2025)

rm(list_disease_files)

# filter the total cases and deaths, with the year after 2008
data_select <- data_month |>
     filter(Year >= 2008) |>
     filter(Areas == 'Total' & Month == 'Total') |>
     group_by(Disease) |>
     summarise(Cases = sum(Count),
               Count = n(),
               YearStart = min(Year),
               YearEnd = max(Year),
               .groups = 'drop')

write.csv(data_select,
          file = "../Outcome/TotalCasesDeaths.csv",
          row.names = F)

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

# read population data
data_population <- read.xlsx('../Data/Population/1992-2023.xlsx', sheet = 'age') |> 
     select(YEAR, Total) |> 
     rename(Year = YEAR,
            Population = Total)

data_analysis <- data_month |>
     left_join(data_class, by = 'Disease') |>
     filter(Areas == 'Total' & Month != 'Total' & 
                 !is.na(Shortname) & Year >= 2008) |> 
     # transform the Month: Jan -> 01
     mutate(Month = month(parse_date_time(Month, "b"), label = FALSE, abbr = FALSE),
            Group = factor(Group, levels = disease_groups),
            Date = as.Date(paste(Year, Month, "01", sep = "-"))) |> 
     pivot_wider(names_from = Type, values_from = Count, values_fill = 0) |>
     filter(Date < as.Date("2024-07-01")) |> 
     ungroup() |> 
     arrange(Date, Disease) |> 
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e7,
            Mortality = (Deaths / Population) * 1e7)

# summary of NID ----------------------------------------------------------

## each group
data_print <- data_analysis |>
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
     arrange(Group, desc(Cases))


# overall trend of month --------------------------------------------------

fill_color_trend <- fill_color_continue[c(1, 3, 5)]
names(fill_color_trend) <- c("Observed", "STL Trend", "Joinpoint Trend")

fig1_data <- data_analysis |>
     group_by(Date, Year) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(Date) |> 
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e7,
            Mortality = (Deaths / Population) * 1e7,
            CFR = (Deaths / Cases) * 1000) |> 
     # format year and month to fit joinpoint model
     mutate(Month = month(Date),
            MonthIndex = Year * 12 + Month - min(data_analysis$Year) * 12,
            .after = Year)

## STL model
stl_incidence <- stl(ts(fig1_data$Incidence, start = min(fig1_data$Year), frequency = 12),
                     s.window = "periodic", t.window = 24, robust = TRUE)
fig1_data$Incidence_trend <- stl_incidence$time.series[, 'trend']

stl_mortality <- stl(ts(fig1_data$Mortality, start = min(fig1_data$Year), frequency = 12),
                     s.window = "periodic", t.window = 24, robust = TRUE)
fig1_data$Mortality_trend <- stl_mortality$time.series[, 'trend']

stl_cfr <- stl(ts(fig1_data$CFR, start = 2007, frequency = 12),
               s.window = "periodic", t.window = 24, robust = TRUE)
fig1_data$CFR_trend <- stl_cfr$time.series[, 'trend']


tasks <- list(
     list(data = fig1_data, x = "MonthIndex", y = "Incidence_trend", label = 'Incidence', run_opt = run_opt, export_opt = export_opt),
     list(data = fig1_data, x = "MonthIndex", y = "Mortality_trend", label = 'Mortality', run_opt = run_opt, export_opt = export_opt),
     list(data = fig1_data, x = "MonthIndex", y = "CFR_trend", label = 'CFR', run_opt = run_opt, export_opt = export_opt)
)

results <- future_map(
     tasks,
     ~ joinpoint(.x$data, x = .x$x, y = .x$y, run_opt = .x$run_opt, export_opt = .x$export_opt)
)

## Wait for the results
joinpoint_incidence <- results[[1]]
joinpoint_mortality <- results[[2]]
joinpoint_cfr <- results[[3]]

## add results
fig1_data$Incidence_joinpoint <- joinpoint_incidence$data_export$model
fig1_data$Mortality_joinpoint <- joinpoint_mortality$data_export$model
fig1_data$CFR_joinpoint <- joinpoint_cfr$data_export$model

## figure 1 ----------------------------------------------------------------

fig1 <- ggplot(data = fig1_data)+
     geom_point(mapping = aes(x = Date, y = Incidence, color = 'Observed'),
                alpha = 0.5,
                size = 1.5) +
     geom_line(mapping = aes(x = Date, y = Incidence_trend, color = 'Trend'),
               linewidth = 1) +
     geom_line(mapping = aes(x = Date, y = Incidence_joinpoint, color = 'Joinpoint'),
               linewidth = 1) +
     scale_color_manual(values = c('Observed' = 'grey', 'Trend' = 'grey50')) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0.03))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        breaks = pretty(c(fig1_data$Incidence, fig1_data$Incidence_trend), n = 4),
                        limits = range(pretty(c(fig1_data$Incidence, fig1_data$Incidence_trend), n = 4))) +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(y = "Monthly incidence (per million)",
          x = NULL,
          color = NULL,
          title = 'A')+
     guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(16, NA)),
                                 nrows = 1))

## figure 2 ----------------------------------------------------------------

fig2_data <- fig1_data |> 
     select(Date, Mortality, Mortality_trend)

fig2 <- ggplot(data = fig2_data)+
     geom_point(mapping = aes(x = Date, y = Mortality, color = 'Observed'),
                alpha = 0.5,
                size = 1.5) +
     geom_line(mapping = aes(x = Date, y = Mortality_trend, color = 'Trend'),
               size = 1) +
     scale_color_manual(values = c('Observed' = 'grey', 'Trend' = 'grey50')) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0.03))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        breaks = pretty(c(fig2_data$Mortality, fig2_data$Mortality_trend), n = 4),
                        limits = range(pretty(c(fig2_data$Mortality, fig2_data$Mortality_trend), n = 4))) +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(y = "Monthly mortality (per million)",
          x = NULL,
          color = NULL,
          title = 'B')+
     guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(1, NA)),
                                 nrows = 1))

## figure 3 ----------------------------------------------------------------

fig3_data <- fig1_data |> 
     select(Date, CFR, CFR_trend)

fig3 <- ggplot(data = fig3_data)+
     geom_point(mapping = aes(x = Date, y = CFR, color = 'Observed'),
                alpha = 0.5,
                size = 1.5) +
     geom_line(mapping = aes(x = Date, y = CFR_trend, color = 'Trend'),
               size = 1) +
     scale_color_manual(values = c('Observed' = 'grey', 'Trend' = 'grey50')) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0.03))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        breaks = pretty(c(fig3_data$CFR, fig3_data$CFR_trend), n = 4),
                        labels = scales::label_comma(accuracy = 1),
                        limits = range(pretty(c(fig3_data$CFR, fig3_data$CFR_trend), n = 4))) +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(x = NULL,
          # using ‰
          y = "Monthly CFR (\u2030)",
          color = NULL,
          title = 'C')+
     guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(1, NA)),
                                 nrows = 1))

fig1_data <- fig1_data |> 
     select(Date, Cases, Deaths)

# cumulative cases --------------------------------------------------------

names(fill_color) <- disease_groups

## figure 4 ----------------------------------------------------------------

fig4_data <- data_analysis |>
     select(Year, Disease = Shortname, Group, Cases, Deaths) |>
     group_by(Group, Disease) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               CFR = (Deaths / Cases) * 1000,
               .groups = 'drop') |> 
     # replace CFR NAN with 0
     mutate(CFR = ifelse(is.na(CFR), 0, CFR)) |>
     arrange(Group, desc(Cases))

fig4 <- ggplot(data = fig4_data)+
     geom_col(mapping = aes(x = Disease, y = Cases, fill = Group),
              show.legend = T) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig4_data$Disease) +
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(0, max(fig4_data$Cases) * 1.25), n = 4),
                        limits = range(pretty(c(0, max(fig4_data$Cases) * 1.25), n = 4)),
                        label = scientific_10)+
     theme_set() +
     theme(legend.position = 'bottom',
           legend.text = element_text(face = "bold", size = 14),
           legend.title = element_text(face = "bold", size = 16),
           axis.title.y = element_text(face = "bold", size = 16),
           plot.title = element_text(face = "bold", size = 18, hjust = 0, vjust = 5),
           axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
     labs(y = "Cumulative cases",
          x = NULL,
          fill = NULL,
          title = 'D')+
     guides(fill = guide_legend(byrow = TRUE, nrow = 1))

fig4_in <- ggplot(data = filter(fig4_data, Cases <= 3e5))+
     geom_col(mapping = aes(x = Disease, y = Cases, fill = Group),
              show.legend = F) +
     coord_cartesian(ylim = c(0, 3e5)) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig4_data$Disease[fig4_data$Cases <= 3e5]) +
     scale_y_continuous(expand = c(0, 0),
                        breaks = c(0, 1e5, 2e5, 3e5),
                        limits = c(0, 3e5),
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
fig4 <- fig4 + inset_element(fig4_in, left = 0.03, bottom = 0.13, right = 1, top = 1.2)

remove(fig4_in)

## figure 5 ----------------------------------------------------------------

fig5_data <- fig4_data |> 
     select(Disease, Group, Deaths) |> 
     arrange(Group, desc(Deaths))

fig5 <- ggplot(data = fig5_data)+
     geom_col(mapping = aes(x = Disease, y = Deaths, fill = Group),
              show.legend = F) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig5_data$Disease) +
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(0, max(fig5_data$Deaths) * 1.25), n = 4),
                        limits = range(pretty(c(0, max(fig5_data$Deaths) * 1.25), n = 4)),
                        label = scientific_10)+
     theme_set() +
     theme(legend.position = 'right',
           legend.text = element_text(face = "bold", size = 14),
           legend.title = element_text(face = "bold", size = 16),
           axis.title.y = element_text(face = "bold", size = 16),
           plot.title = element_text(face = "bold", size = 18, hjust = 0, vjust = 5),
           axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
     labs(y = "Cumulative deaths",
          x = NULL,
          fill = NULL,
          title = 'E')

fig5_in <- ggplot(data = filter(fig5_data, Deaths <= 1e3))+
     geom_col(mapping = aes(x = Disease, y = Deaths, fill = Group),
              show.legend = F) +
     coord_cartesian(ylim = c(0, 300)) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig5_data$Disease[fig5_data$Deaths <= 300]) +
     scale_y_continuous(expand = c(0, 0),
                        limits = c(0, 300),
                        breaks = c(0, 100, 200, 300))+
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
fig5 <- fig5 + inset_element(fig5_in, left = 0.03, bottom = 0.13, right = 1, top = 1.2)

remove(fig5_in)

## figure 6 ----------------------------------------------------------------

fig6_data <- fig4_data |> 
     select(Disease, Group, CFR) |> 
     arrange(Group, desc(CFR))

fig6 <- ggplot(data = fig6_data)+
     geom_col(mapping = aes(x = Disease, y = CFR, fill = Group),
              show.legend = F) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig6_data$Disease) +
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(0, max(fig6_data$CFR) * 1.25), n = 4),
                        limits = range(pretty(c(0, max(fig6_data$CFR) * 1.25), n = 4)),
                        label = scientific_10)+
     theme_set() +
     theme(legend.position = 'right',
           legend.text = element_text(face = "bold", size = 14),
           legend.title = element_text(face = "bold", size = 16),
           axis.title.y = element_text(face = "bold", size = 16),
           plot.title = element_text(face = "bold", size = 18, hjust = 0, vjust = 5),
           axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
     labs(y = "Total CFR (\u2030)",
          x = NULL,
          fill = NULL,
          title = 'F')

fig6_in <- ggplot(data = filter(fig6_data, CFR <= 10 & CFR > 0))+
     geom_col(mapping = aes(x = Disease, y = CFR, fill = Group),
              show.legend = F) +
     coord_cartesian(ylim = c(0, 10)) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = fig6_data$Disease[fig6_data$CFR <= 10 & fig6_data$CFR >= 0]) +
     scale_y_continuous(expand = c(0, 0),
                        limits = c(0, 10),
                        breaks = c(0, 2, 4, 6, 8, 10))+
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
fig6 <- fig6 + inset_element(fig6_in, left = 0.15, bottom = 0.13, right = 1, top = 1.2)

remove(fig6_in)

fig4_data <- fig4_data |> 
     select(Disease, Group, Cases)

# save --------------------------------------------------------------------

fig <- cowplot::plot_grid(fig1 + fig2 + fig3 + plot_layout(nrow = 1),
                          fig4 + fig5 + fig6 + plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = "bottom"),
                          nrow = 2,
                          ncol = 1,
                          rel_heights = c(1, 3.2))

ggsave(filename = "../Outcome/Publish/fig1.pdf",
       fig,
       width = 14,
       height = 16,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
data_fig <- list("panel A" = fig1_data,
                 "panel B" = fig2_data,
                 "panel C" = fig3_data,
                 "panel D" = fig4_data,
                 "panel E" = fig5_data,
                 "panel F" = fig6_data)

write.xlsx(data_fig,
           file = "../Outcome/Appendix/figure_data/fig1.xlsx")

# appendix ----------------------------------------------------------------

# extract stl trend of each time series

plot_trend <- function(data, title, value = 'Incidence') {
     ts_data <- ts(data[[value]], start = min(data$Year), frequency = 12)
     stl_data <- stl(ts_data, s.window = "periodic", t.window = 24, robust = T)
     data_trend <- data.frame(Year = data$Year,
                              Month = data$Month,
                              Value = data[[value]],
                              Trend = stl_data$time.series[, 'trend'])
     
     fig <- ggplot(data = data_trend) +
          geom_line(mapping = aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Trend),
                    color = fill_color[1],
                    size = 1) +
          geom_point(mapping = aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = Value),
                     color = fill_color[5],
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

## incidence ----------------------------------------------------

for (g in disease_groups) {
     # g <- disease_groups[1]
     d <- data_class$Shortname[data_class$Group == g]
     
     # panel A: trend of group cases
     data_group <- data_analysis |>
          filter(Group == g) |>
          group_by(Date, Year, Month) |>
          summarise(Cases = sum(Cases),
                    Deaths = sum(Deaths),
                    .groups = 'drop') |> 
          # add population
          left_join(data_population, by = 'Year') |>
          # calculate the rate per million population
          mutate(Incidence = (Cases / sum(Population)) * 1e7,
                 Mortality = (Deaths / sum(Population)) * 1e7,
                 CFR = (Deaths / Cases) * 1000)
     fig_case_g <- plot_trend(data_group, paste(LETTERS[1], g, sep = ": "), 'Incidence')
     
     # panel B: trend of each disease
     data_single <- data_analysis |>
          filter(Group == g)
     fig_cases <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 1], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Incidence')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/Cases ", g, ".png"),
            fig_cases_g + fig_cases + plot_layout(ncol = 4),
            device = "png",
            width = 14, height = 7,
            limitsize = FALSE,
            dpi = 300)
     
     # panel C: trend of group deaths
     fig_deaths_g <- plot_trend(data_group, paste(LETTERS[1], g, sep = ": "), 'Mortality')
     
     # panel D: trend of each disease
     fig_deaths <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 1], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Mortality')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/Deaths ", g, ".png"),
            fig_deaths_g + fig_deaths + plot_layout(ncol = 4),
            device = "png",
            width = 14, height = 7,
            limitsize = FALSE,
            dpi = 300)
}

# save plot ---------------------------------------------------------------

save(data_analysis, data_month, data_class, data_population, file = "./month.RData")

print(data_print, n = Inf)
