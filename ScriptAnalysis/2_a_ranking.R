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

# data prepare ------------------------------------------------------------

# create the ranking of each disease
data_plot <- data_year |> 
     mutate(Year_group = case_when(Year %in% 2008:2010 ~ '2008-2010',
                                   Year %in% 2011:2013 ~ '2011-2013',
                                   Year %in% 2014:2016 ~ '2014-2016',
                                   TRUE ~ as.character(Year)),
            Year_group = factor(Year_group)) |>
     group_by(Shortname, Group, Year_group) |> 
     summarise(Cases = sum(Cases),
               .groups = 'drop') |> 
     # compute Cases ranking within each Group (for each Year_group),
     group_by(Year_group, Group) |> 
     mutate(Cases_rank = rank(-Cases, ties.method = 'first')) |> 
     ungroup() |> 
     mutate(Cases_label = paste(Cases_rank, Shortname, sep = '. '),
            Year_mark = as.integer(Year_group))

# create the label for the disease with zero cases
data_zero_cases <- data_plot |> 
     ungroup() |>
     filter(Cases > 0) |> 
     group_by(Group, Year_mark) |>
     summarise(Cases_rank_max = max(Cases_rank)+0.5,
               .groups = 'drop') |> 
     mutate(Year_mark_start = Year_mark - arrow_space,
            Year_mark_start = if_else(Year_mark_start < 1, 0, Year_mark_start),
            Year_mark_end = Year_mark + arrow_space,
            Year_mark_end = if_else(Year_mark_end > length(unique(data_plot$Year_mark)),
                                    length(unique(data_plot$Year_mark)) + 1,
                                    Year_mark_end)) |>
     select(Group, Year_mark_start, Year_mark_end, Cases_rank_max) |>
     pivot_longer(cols = c(Year_mark_start, Year_mark_end),
                  names_to = 'Year_mark_label',
                  values_to = 'Year_mark') |> 
     group_by(Group) |> 
     mutate(Cases_rank_top = max(Cases_rank_max)+0.5) |> 
     ungroup()

# cases ranking -----------------------------------------------------------

connections <- data_plot |> 
     ungroup() |> 
     group_by(Shortname) |> 
     arrange(Year_mark) |> 
     mutate(Cases_next_rank = lead(Cases_rank),
            Cases_next_status = if_else(Cases_next_rank > Cases_rank,
                                        'Decrease',
                                        if_else(Cases_next_rank < Cases_rank,
                                                'Increase',
                                                'Constant')),
            Cases_next_status = factor(Cases_next_status, levels = c('Decrease', 'Constant', 'Increase')),
            Year_next_mark = lead(Year_mark)) |> 
     filter(!is.na(Cases_next_rank))

# fig1 --------------------------------------------------------------------

facet_label <- paste(LETTERS[1:length(disease_groups)], ': ', unique(disease_groups), sep = '')
names(facet_label) <- disease_groups

fig1 <- ggplot() +
     # add the label for the disease with zero cases
     geom_ribbon(data = data_zero_cases,
                 aes(x = Year_mark, ymin = Cases_rank_max, ymax = Cases_rank_top),
                 fill = '#088BBEFF',
                 alpha = 0.3,
                 color = 'white',
                 show.legend = T) +
     geom_tile(data = data_plot,
               aes(x = Year_mark, y = Cases_rank, fill = Group),
               width = arrow_space*2,
               color = 'white',
               show.legend = T) +
     geom_text(data = data_plot,
               aes(x = Year_mark, y = Cases_rank, label = Cases_label), 
               nudge_x = -0.35,
               color = 'white',
               fontface = 'bold',
               vjust = 0.5, hjust = 0, size = 3, check_overlap = F) +
     geom_segment(data = connections,
                  aes(x = Year_mark+arrow_space, y = Cases_rank,
                      color = Cases_next_status,
                      xend = Year_next_mark-arrow_space, yend = Cases_next_rank), 
                  lineend = "round",
                  linejoin = "round",
                  arrow = arrow(length = unit(2, "mm"))) +
     coord_cartesian(xlim = c(1, length(unique(data_plot$Year_mark)))) +
     facet_wrap(~Group, ncol = 1, scales = 'free_y',
                labeller = as_labeller(facet_label)) +
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
           strip.background = element_blank(),
           strip.text = element_text(face = 'bold', size = 14, hjust = 0),
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

ggsave(filename = "../outcome/publish/fig2.png",
       plot = fig1,
       width = 14,
       height = 12)

# figure data
write.xlsx(data_plot |> select(Shortname, Group, Year_group, Cases, Cases_rank),
           file = "../outcome/Appendix/figure_data/fig2.xlsx")
