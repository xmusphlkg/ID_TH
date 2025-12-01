# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(openxlsx)

# data --------------------------------------------------------------------

remove(list = ls())

source("./function/theme_set.R")

load("./month.RData")

arrow_space <- 0.38

# identify diseases that have any deaths across all Year_group
deaths_by_disease <- data_year |> 
     group_by(Shortname) |> 
     summarise(Total_Deaths = sum(Deaths, na.rm = TRUE),
               HasAnyDeath = Total_Deaths > 0,
               .groups = 'drop')

# data prepare ------------------------------------------------------------

# create the ranking of each disease
data_plot <- data_year |> 
     mutate(Year_group = case_when(Year %in% 2008:2010 ~ '2008-2010',
                                   Year %in% 2011:2013 ~ '2011-2013',
                                   Year %in% 2014:2016 ~ '2014-2016',
                                   TRUE ~ as.character(Year)),
            Year_group = factor(Year_group)) |>
     group_by(Shortname, Group, Year_group) |> 
     summarise(Deaths = sum(Deaths),
               .groups = 'drop') |> 
     # attach flag whether the disease ever had any deaths across all Year_group
     left_join(deaths_by_disease |> select(Shortname, HasAnyDeath), by = 'Shortname') |> 
     filter(HasAnyDeath) |>
     # only rank diseases that have any deaths across year groups; others stay NA
     group_by(Year_group) |> 
     mutate(Deaths_rank = as.integer(rank(-Deaths, ties.method = 'first')),
            Year_mark = as.integer(Year_group))

# get disease death order
data_death_order <- data_plot |> 
     ungroup() |> 
     group_by(Shortname) |>
     summarise(Deaths = sum(Deaths)) |> 
     arrange(desc(Deaths))

# reassign the rank of the disease with zero death for the first year
data <- data_plot |> 
     ungroup() |> 
     # only consider diseases that have some deaths (if a disease has no deaths in any year,
     # we skip ranking for it)
     filter(Deaths == 0 & Year_mark == 1 & HasAnyDeath) |>
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
          # only consider diseases that have some deaths across years
          filter(Deaths == 0 & Year_mark == Y & HasAnyDeath)
     
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
     mutate(Deaths_rank = if_else(HasAnyDeath & Deaths == 0, Deaths_rank_new, Deaths_rank),
          # only label diseases that have a (non-NA) deaths rank
          Deaths_label = if_else(!is.na(Deaths_rank), paste(Deaths_rank, Shortname, sep = '. '), NA_character_)) |>
     select(-Deaths_rank_new)

# create the label for the disease with zero death
data_zero_death <- data_plot |> 
     ungroup() |>
     filter(Deaths > 0) |> 
     select(Year_mark, Deaths_rank) |> 
     group_by(Year_mark) |>
     summarise(Deaths_rank_max = max(Deaths_rank)+0.5,
               .groups = 'drop') |> 
     mutate(Year_mark_start = Year_mark - arrow_space,
            Year_mark_start = if_else(Year_mark_start < 1, 0, Year_mark_start),
            Year_mark_end = Year_mark + arrow_space,
            Year_mark_end = if_else(Year_mark_end > length(unique(data_plot$Year_mark)),
                                    length(unique(data_plot$Year_mark)) + 1,
                                    Year_mark_end),
            Deaths_rank_top = max(data_plot$Deaths_rank)+0.5) |>
     select(Year_mark_start, Year_mark_end, Deaths_rank_max, Deaths_rank_top) |>
     pivot_longer(cols = c(Year_mark_start, Year_mark_end),
                  names_to = 'Year_mark_label',
                  values_to = 'Year_mark')

# deaths ranking -----------------------------------------------------------

connections <- data_plot |> 
     ungroup() |> 
     group_by(Shortname) |> 
     arrange(Year_mark) |> 
     mutate(Deaths_next_rank = lead(Deaths_rank),
            Deaths_next_status = if_else(Deaths_next_rank > Deaths_rank,
                                         'Decrease',
                                         if_else(Deaths_next_rank < Deaths_rank,
                                                 'Increase',
                                                 'Constant')),
            Deaths_next_status = factor(Deaths_next_status, levels = c('Decrease', 'Constant', 'Increase')),
            Year_next_mark = lead(Year_mark)) |> 
     filter(!is.na(Deaths_next_status))

# fig2 --------------------------------------------------------------------

fig2 <- ggplot() +
     # add the label for the disease with zero death
     geom_ribbon(data = data_zero_death,
                 aes(x = Year_mark, ymin = Deaths_rank_max, ymax = Deaths_rank_top),
                 fill = '#088BBEFF',
                 alpha = 0.3,
                 color = 'white',
                 show.legend = T) +
     geom_tile(data = data_plot,
               aes(x = Year_mark, y = Deaths_rank, fill = Group),
               width = arrow_space*2,
               color = 'white',
               show.legend = T) +
     geom_text(data = data_plot,
               aes(x = Year_mark, y = Deaths_rank, label = Deaths_label), 
               nudge_x = -0.35,
               color = 'white',
               fontface = 'bold',
               vjust = 0.5, hjust = 0, size = 3, check_overlap = F) +
     geom_segment(data = connections,
                  aes(x = Year_mark+arrow_space, y = Deaths_rank,
                      color = Deaths_next_status,
                      xend = Year_next_mark-arrow_space, yend = Deaths_next_rank), 
                  lineend = "round",
                  linejoin = "round",
                  arrow = arrow(length = unit(2, "mm"))) +
     coord_cartesian(xlim = c(1, length(unique(data_plot$Year_mark)))) +
     scale_x_continuous(breaks = unique(data_plot$Year_mark),
                        labels = unique(data_plot$Year_group)) +
     scale_y_reverse(expand = c(0, 0)) +
     scale_fill_manual(values = fill_color) +
     scale_color_manual(values = c('Decrease' = '#4DBBD5FF', 'Constant' = '#8491B4FF', 'Increase' = '#E64B35FF')) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           legend.position = 'bottom',
           legend.box = 'horizontal',
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
       height = 8,
       device = cairo_pdf,
       family = "Times New Roman")

ggsave(filename = "../outcome/publish/fig3.png",
       plot = fig2,
       width = 14,
       height = 12)

write.xlsx(data_plot |> select(Shortname, Group, Year_group, Deaths, Deaths_rank),
           file = "../outcome/Appendix/figure_data/fig3.xlsx")
