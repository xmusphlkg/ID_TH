# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)

# data --------------------------------------------------------------------

remove(list = ls())

source("./function/theme_set.R")

load("./month.RData")

# data prepare ------------------------------------------------------------

# create the ranking of each disease
data_plot <- data_analysis |> 
     mutate(Year_group = if_else(Year >= 2019,
                                 as.character(Year),
                                 if_else(Year >= 2015,
                                         '2015-2018',
                                         if_else(Year >= 2011,
                                                 '2011-2014',
                                                 '2007-2010')))) |>
     group_by(Shortname, Group, Year_group) |> 
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     group_by(Year_group) |> 
     # add ranking of each disease
     mutate(Cases_rank = rank(-Cases, ties.method = 'first'),
            Deaths_rank = rank(-Deaths, ties.method = 'first'),
            Cases_label = paste(Cases_rank, Shortname, sep = '. '),
            Year_mark = case_when(Year_group == '2007-2010' ~ 1,
                                  Year_group == '2011-2014' ~ 2,
                                  Year_group == '2015-2018' ~ 3,
                                  TRUE ~ as.integer(Year_group) - 2015))

# get disease death order
data_death_order <- data_plot |> 
     ungroup() |> 
     group_by(Shortname) |>
     summarise(Deaths = sum(Deaths)) |> 
     arrange(desc(Deaths))

# reassign the rank of the disease with zero death for the first year
data <- data_plot |> 
     ungroup() |> 
     filter(Deaths == 0 & Year_mark == 1) |>
     # get available rank of the disease with zero death
     mutate(Shortname = factor(Shortname, levels = data_death_order$Shortname),
            Deaths_rank = rank(Shortname, ties.method = 'first') + min(Deaths_rank) - 1) |> 
     select(Shortname, Year_group, Deaths_rank_new = Deaths_rank)

# replace the rank of the disease with zero death
data_plot <- data_plot |> 
     ungroup() |> 
     left_join(data, by = c('Shortname', 'Year_group'))

remove(data, data_death_order)

# reassign the rank of the disease with zero death
for (Y in 2:length(unique(data_plot$Year_mark))) {
     # Y <- 2
     Y_prev <- Y - 1
     # get disease death order
     data_prev <- data_plot |> 
          ungroup() |> 
          filter(Year_mark == Y_prev) |> 
          mutate(Deaths_rank_new = if_else(is.na(Deaths_rank_new), Deaths_rank, Deaths_rank_new))
     
     data <- data_plot |> 
          ungroup() |> 
          filter(Deaths == 0 & Year_mark == Y)
     
     data_prev <- data_prev |> 
          filter(Shortname %in% data$Shortname) |>
          arrange(Deaths_rank_new)
     
     data <- data |>
          group_by(Year_group) |>
          # get available rank of the disease with zero death
          mutate(Shortname = factor(Shortname, levels = data_prev$Shortname),
                 Deaths_rank_new = rank(Shortname, ties.method = 'first') + min(Deaths_rank) - 1) |> 
          select(Shortname, Year_group, Deaths_rank_new)

     # merge to the main data
     data_plot <- data_plot |> 
          ungroup() |> 
          left_join(data, by = c('Shortname', 'Year_group')) |> 
          mutate(Deaths_rank_new = coalesce(Deaths_rank_new.x, Deaths_rank_new.y)) |> 
          select(-Deaths_rank_new.x, -Deaths_rank_new.y)
     
     remove(data, data_prev)
}

# replace the rank of the disease with zero death
data_plot <- data_plot |> 
     ungroup() |> 
     mutate(Deaths_rank = if_else(Deaths == 0, Deaths_rank_new, Deaths_rank),
            Deaths_label = paste(Deaths_rank, Shortname, sep = '. ')) |>
     select(-Deaths_rank_new)


# create the label for the disease with zero death
data_zero_death <- data_plot |> 
     ungroup() |>
     filter(Deaths > 0) |> 
     select(Year_mark, Deaths_rank) |> 
     group_by(Year_mark) |>
     summarise(Deaths_rank = max(Deaths_rank)+0.5) |> 
     mutate(Year_mark_start = Year_mark - 0.35,
            Year_mark_start = if_else(Year_mark_start < 1, 0, Year_mark_start),
            Year_mark_end = Year_mark + 0.35,
            Year_mark_end = if_else(Year_mark_end > length(unique(data_plot$Year_mark)),
                                    length(unique(data_plot$Year_mark)) + 1,
                                    Year_mark_end)) |>
     select(Year_mark_start, Year_mark_end, Deaths_rank) |>
     pivot_longer(cols = c(Year_mark_start, Year_mark_end),
                  names_to = 'Year_mark_label',
                  values_to = 'Year_mark')

# create the label for the disease with zero cases
data_zero_cases <- data_plot |> 
     ungroup() |>
     filter(Cases > 0) |> 
     select(Year_mark, Cases_rank) |> 
     group_by(Year_mark) |>
     summarise(Cases_rank = max(Cases_rank)+0.5) |> 
     mutate(Year_mark_start = Year_mark - 0.35,
            Year_mark_start = if_else(Year_mark_start < 1, 0, Year_mark_start),
            Year_mark_end = Year_mark + 0.35,
            Year_mark_end = if_else(Year_mark_end > length(unique(data_plot$Year_mark)),
                                    length(unique(data_plot$Year_mark)) + 1,
                                    Year_mark_end)) |>
     select(Year_mark_start, Year_mark_end, Cases_rank) |>
     pivot_longer(cols = c(Year_mark_start, Year_mark_end),
                  names_to = 'Year_mark_label',
                  values_to = 'Year_mark')

# cases ranking -----------------------------------------------------------

connections <- data_plot |> 
     ungroup() |> 
     group_by(Shortname) |> 
     arrange(Year_mark) |> 
     mutate(Cases_next_rank = lead(Cases_rank),
            Deaths_next_rank = lead(Deaths_rank),
            Cases_next_status = if_else(Cases_next_rank > Cases_rank,
                                        'Decrease',
                                        if_else(Cases_next_rank < Cases_rank,
                                                'Increase',
                                                'Constant')),
            Cases_next_status = factor(Cases_next_status, levels = c('Decrease', 'Constant', 'Increase')),
            Deaths_next_status = if_else(Deaths_next_rank > Deaths_rank,
                                         'Decrease',
                                         if_else(Deaths_next_rank < Deaths_rank,
                                                 'Increase',
                                                 'Constant')),
            Deaths_next_status = factor(Deaths_next_status, levels = c('Decrease', 'Constant', 'Increase')),
            Year_next_mark = lead(Year_mark)) |> 
     filter(!is.na(Cases_next_rank))

# fig1 --------------------------------------------------------------------

fig1 <- ggplot() +
     # add the label for the disease with zero cases
     geom_ribbon(data = data_zero_cases,
                 aes(x = Year_mark, ymax = Cases_rank),
                 ymin = length(unique(data_plot$Cases_rank))+1,
                 fill = '#00A087FF',
                 alpha = 0.3,
                 color = 'white',
                 show.legend = T) +
     annotate(geom = 'text',
              x = min(unique(data_plot$Year_mark)) - 0.15,
              y = max(data_plot$Cases_rank, na.rm = T) - 3,
              label = 'Diseases with zero cases',
              color = 'black',
              fontface = 'bold',
              vjust = 0.5, hjust = -0.1, size = 6) +
     geom_tile(data = data_plot,
               aes(x = Year_mark, y = Cases_rank, fill = Group),
               width = 0.7,
               color = 'white',
               show.legend = T) +
     geom_text(data = data_plot,
               aes(x = Year_mark, y = Cases_rank, label = Cases_label), 
               nudge_x = -0.33,
               color = 'white',
               fontface = 'bold',
               vjust = 0.5, hjust = 0, size = 2.3, check_overlap = F) +
     geom_segment(data = connections,
                  aes(x = Year_mark+0.35, y = Cases_rank,
                      color = Cases_next_status,
                      xend = Year_next_mark-0.35, yend = Cases_next_rank), 
                  lineend = "round",
                  linejoin = "round",
                  arrow = arrow(length = unit(2, "mm"))) +
     coord_cartesian(ylim = c(length(unique(data_plot$Cases_rank))-2, 3), xlim = c(1, length(unique(data_plot$Year_mark))+0.2)) +
     scale_x_continuous(breaks = unique(data_plot$Year_mark),
                        labels = unique(data_plot$Year_group)) +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = c('Decrease' = '#4DBBD5FF', 'Constant' = '#8491B4FF', 'Increase' = '#E64B35FF')) +
     theme_plot() +
     theme(panel.grid = element_blank(),
           legend.position = 'bottom',
           legend.box = 'vertical',
           legend.direction = "horizontal",
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank())+
     labs(x = NULL,
          color = "Changes of ranking",
          fill = "Categories",
          title = '',
          y = NULL)+
     guides(color = guide_legend(nrow = 1, order = 2, override.aes = list(fill = 'white')),
            fill = guide_legend(nrow = 1, order = 1))

ggsave(filename = "../outcome/publish/fig2.pdf",
       plot = fig1,
       width = 14,
       height = 12,
       device = cairo_pdf,
       family = "Times New Roman")


# figure data
write.xlsx(data_plot |> select(Shortname, Group, Year_group, Cases, Cases_rank),
           file = "../outcome/Appendix/figure_data/fig2.xlsx")

# fig2 --------------------------------------------------------------------

fig2 <- ggplot() +
     # add the label for the disease with zero death
     geom_ribbon(data = data_zero_death,
                 aes(x = Year_mark, ymax = Deaths_rank),
                 ymin = length(unique(data_plot$Deaths_rank))+1,
                 fill = '#00A087FF',
                 alpha = 0.3,
                 color = 'white',
                 show.legend = T) +
     annotate(geom = 'text',
              x = min(unique(data_plot$Year_mark)) - 0.15,
              y = max(data_plot$Deaths_rank, na.rm = T) - 3,
              label = 'Diseases with zero deaths',
              color = 'black',
              fontface = 'bold',
              vjust = 0.5, hjust = -0.1, size = 6) +
     geom_tile(data = data_plot,
               aes(x = Year_mark, y = Deaths_rank, fill = Group),
               width = 0.7,
               color = 'white',
               show.legend = T) +
     geom_text(data = data_plot,
               aes(x = Year_mark, y = Deaths_rank, label = Deaths_label), 
               nudge_x = -0.33,
               color = 'white',
               fontface = 'bold',
               vjust = 0.5, hjust = 0, size = 2.3, check_overlap = F) +
     geom_segment(data = connections,
                  aes(x = Year_mark+0.35, y = Deaths_rank,
                      color = Deaths_next_status,
                      xend = Year_next_mark-0.35, yend = Deaths_next_rank), 
                  lineend = "round",
                  linejoin = "round",
                  arrow = arrow(length = unit(2, "mm"))) +
     coord_cartesian(ylim = c(length(unique(data_plot$Deaths_rank))-2, 3), xlim = c(1, length(unique(data_plot$Year_mark))+0.2)) +
     scale_x_continuous(breaks = unique(data_plot$Year_mark),
                        labels = unique(data_plot$Year_group)) +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = c('Decrease' = '#4DBBD5FF', 'Constant' = '#8491B4FF', 'Increase' = '#E64B35FF')) +
     theme_plot() +
     theme(panel.grid = element_blank(),
           legend.position = 'bottom',
           legend.box = 'vertical',
           legend.direction = "horizontal",
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank())+
     labs(x = NULL,
          color = "Changes of ranking",
          fill = "Categories",
          title = '',
          y = NULL)+
     guides(color = guide_legend(nrow = 1, order = 2, override.aes = list(fill = 'white')),
            fill = guide_legend(nrow = 1, order = 1))

ggsave(filename = "../outcome/publish/fig3.pdf",
       plot = fig2,
       width = 14,
       height = 12,
       device = cairo_pdf,
       family = "Times New Roman")
