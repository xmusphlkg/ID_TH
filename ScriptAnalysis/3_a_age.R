
# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(openxlsx)

# data --------------------------------------------------------------------

remove(list = ls())

source("./function/theme_set.R")

load("./month.RData")

data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

data_agegroup <- read.csv('../Data/AgeClass.csv')

# list files in the folder: case data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "ac.csv",
                                 full.names = T)
data_case <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2008 & Areas == 'Total') |> 
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Group, Shortname, Age, Year, Cases)
rm(list_disease_files)

# list files in the folder: death data
list_death_files <- list.files("../Data/CleanData/",
                                 pattern = "ad.csv",
                                 full.names = T)
data_death <- lapply(list_death_files, read.csv) |>
     bind_rows() |> 
     filter(Year >= 2008 & Areas == 'Total') |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |>
     select(Group, Shortname, Age, Year, Deaths = Cases)
rm(list_death_files)

## rank --------------------------------------------------------------------

# merge case and death data
data_age_all <- data_case |> 
     left_join(data_death, by = c('Group', 'Shortname', 'Age', 'Year')) |> 
     left_join(data_agegroup, by = 'Age') |>
     filter(AgeGroup != 'Unknown') |> 
     group_by(Shortname, Group, AgeGroup, AgeGroupID, Year) |>
     summarise(Cases = sum(Cases, na.rm = T),
               Deaths = sum(Deaths, na.rm = T),
               .groups = 'drop') |>
     mutate(AgeGroup = factor(AgeGroup, levels = unique(data_agegroup$AgeGroup)),
            Year_group = case_when(Year %in% 2008:2010 ~ '2008-2010',
                                   Year %in% 2011:2013 ~ '2011-2013',
                                   Year %in% 2014:2016 ~ '2014-2016',
                                   TRUE ~ as.character(Year)),
            Year_group = factor(Year_group),
            Year_mark = as.integer(Year_group))

# create the ranking of each disease by cases and deaths
data_age_top <- data_age_all |> 
     # finding leading causes of death, case in each age group
     group_by(AgeGroup, AgeGroupID, Year_group, Year_mark) |>
     summarise(Max_Cases_Disease = Shortname[which.max(Cases)],
               Max_Deaths_Disease = Shortname[which.max(Deaths)],
               Max_Cases_Value = max(Cases),
               Lsit_Cases_Value = list(sort(Cases, decreasing = T)),
               Max_Deaths_Value = max(Deaths),
               Lsit_Deaths_Value = list(sort(Deaths, decreasing = T)),
               .groups = 'drop') |> 
     # replace 0 deaths with no deaths
     mutate(Max_Deaths_Disease = if_else(Max_Deaths_Value == 0, 'No deaths', Max_Deaths_Disease),
            Max_Deaths_Value = if_else(Max_Deaths_Value == 0, NA_real_, Max_Deaths_Value))

# get disease list
max_disease <- data_age_top |>
     select(Max_Cases_Disease, Max_Deaths_Disease) |>
     unlist() |>
     unique()

## cumulative --------------------------------------------------------------

# create cumulative data for plotting
data_age_cumulative <- data_age_all |> 
     select(AgeGroup, AgeGroupID, Shortname, Cases, Deaths) |>
     # find top 3 diseases of each age group
     group_by(AgeGroup, AgeGroupID, Shortname) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     group_by(AgeGroup, AgeGroupID) |>
     arrange(desc(Cases), .by_group = TRUE) |>
     mutate(rank_in_group = row_number(),
            Top_cases_tag = if_else(rank_in_group <= 3, Shortname, "Others")) |> 
     arrange(desc(Deaths), .by_group = TRUE) |>
     mutate(rank_in_group = row_number(),
            Top_deaths_tag = if_else(rank_in_group <= 3, Shortname, 'Others')) |> 
     ungroup()

# check disease list
legend_disease_list <- unique(c(data_age_cumulative$Top_cases_tag,
                                data_age_cumulative$Top_deaths_tag))

# order by cumulative cases
max_disease <- data_age_cumulative |>
     mutate(Max_tag = case_when(Shortname %in% legend_disease_list ~ Shortname,
                                TRUE ~ 'Others')) |> 
     filter(Max_tag != 'Others') |> 
     group_by(Max_tag) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(desc(Cases)) |> 
     pull(Max_tag) |> 
     append('Others')

## get colors
fill_color_disease_max <- fill_color_disease[1:(length(max_disease)-1)]
fill_color_disease_max <- c(fill_color_disease_max, 'grey50')
names(fill_color_disease_max) <- max_disease

# visual ------------------------------------------------------------------
     
data_outcome <- list()

for (i in 1:2) {
     # cumulative panel
     data_cumulative <- data_age_cumulative |>
          select(AgeGroup, AgeGroupID, contains(c('ases', 'eaths')[i])) |>
          filter(AgeGroup != 'Total')
     names(data_cumulative)[3:4] <- c('Outcome', 'Max_tag')
     
     data_cumulative <- data_cumulative |>
          group_by(AgeGroup, AgeGroupID, Max_tag) |>
          summarise(Outcome = sum(Outcome),
                    .groups = 'drop') |>
          mutate(Max_tag = factor(Max_tag, levels = max_disease)) |>
          arrange(AgeGroupID, Max_tag)
     
     data_outcome[[paste('panel', LETTERS[i*2-1], sep = '')]] <- data_cumulative
     
     panel_breaks <- data_cumulative |>
          pull(Outcome) |>
          append(0) |> 
          pretty(n = 5)
     
     fig_cumulative <- data_cumulative |>
          ggplot(aes(x = AgeGroup, y = Outcome, fill = Max_tag)) +
          geom_col(color = 'white',
                   position = 'dodge',
                   width = 0.8,
                   show.legend = T) +
          scale_fill_manual(values = fill_color_disease_max,
                            drop = F) +
          scale_x_discrete(limits = unique(data_age_cumulative$AgeGroup),
                           breaks = unique(data_cumulative$AgeGroup),
                           expand = expansion(add = c(0.6, 0.6))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0)),
                             limits = range(panel_breaks),
                             labels = scientific_10,
                             breaks = panel_breaks) +
          theme_classic()+
          labs(title = LETTERS[i*2-1],
               fill = 'Disease',
               x = NULL,
               y = ifelse(i == 1, 'Cumulative cases', 'Cumulative deaths'))+
          guides(fill = guide_legend(nrow = 1, byrow = T))
     
     # rank panel
     data_rank <- data_age_top |>
          select(AgeGroup, AgeGroupID, Year_group, Year_mark, contains(c('Max_Cases_D', 'Max_Deaths_D')[i])) |>
          rename(Outcome = colnames(data_age_top)[i + 4]) |> 
          left_join(data_class, by = c('Outcome' = 'Shortname')) |>
          select(-c(Disease, Fullname)) |> 
          mutate(Outcome_Fill = if_else(Outcome %in% c(max_disease, 'No deaths'), Outcome, 'Others'))
     
     data_outcome[[paste('panel', LETTERS[i*2], sep = '')]] <- data_rank
     
     fig_rank <- data_rank |>
          ggplot(aes(x = AgeGroupID, y = Year_mark, fill = Outcome_Fill)) +
          geom_tile(aes(width = 1, height = 1),
                    color = 'white',
                    show.legend = F) +
          geom_text(aes(label = Outcome), size = 3, fontface = "bold") +
          scale_fill_manual(values = fill_color_disease_max,
                            na.value = 'white') +
          coord_cartesian(ylim = c(length(unique(data_rank$Year_mark))+0.5, 0.5)) +
          scale_x_continuous(breaks = unique(data_rank$AgeGroupID),
                             labels = unique(data_rank$AgeGroup),
                             expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(breaks = unique(data_rank$Year_mark),
                             labels = unique(data_rank$Year_group),
                             expand = expansion(mult = c(0, 0)))+
          theme_bw()+
          theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                panel.grid = element_blank())
     
     if (i == 2) {
          fig_rank <- fig_rank +
               labs(title = LETTERS[i*2],
                    x = 'Age (years)',
                    y = NULL)
          
          fig_cumulative <- fig_cumulative +
               theme(legend.position = 'none',
                     plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                     panel.grid = element_blank())
     } else {
          fig_rank <- fig_rank +
               labs(title = LETTERS[i*2],
                    x = NULL,
                    y = NULL)
          
          fig_cumulative <- fig_cumulative +
               theme(legend.position = 'inside',
                     legend.justification = c(0.5, 0),
                     legend.position.inside = c(0.45, 0.9),
                     legend.title.position = 'left',
                     legend.background = element_rect(fill = 'transparent'),
                     plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                     panel.grid = element_blank())
     }
     
     assign(paste('fig', i*2, sep = ''), fig_rank)
     assign(paste('fig', i*2-1, sep = ''), fig_cumulative)
}

## add inset of fig3
fig3_a <- fig3 +
     geom_rect(aes(xmin = 0.5, xmax = 8.5, ymin = 0, ymax = 800),
               color = 'black',
               linewidth = 0.5,
               fill = NA)+
     inset_element(fig3 + 
                        coord_cartesian(xlim = c(0.5, 8), ylim = c(0, 800), clip = 'on')+
                        scale_y_continuous(breaks = seq(0, 800, 200),
                                           expand = expansion(mult = c(0, 0)))+
                        scale_x_discrete(limits = unique(data_age_cumulative$AgeGroup)[1:8],
                                         breaks = unique(data_cumulative$AgeGroup)[1:8],
                                         expand = expansion(add = c(0, 0.6))) +
                        theme(axis.title.y = element_blank(),
                              plot.background = element_rect(color = 'black', size = 0.5),
                              legend.position = 'none',
                              plot.title = element_blank()),
                   left = 0.03,
                   bottom = 0.2,
                   right = 0.75,
                   top = 1.3)

fig <- fig1 + fig2 + fig3_a + fig4 +
     plot_layout(ncol = 1, heights = c(0.5, 1, 0.5, 1), guides = 'keep')

ggsave(filename = "../outcome/publish/fig3.pdf",
       plot = fig,
       width = 14,
       height = 10,
       device = cairo_pdf,
       family = "Times New Roman")

ggsave(filename = "../outcome/publish/fig3.png",
       plot = fig,
       width = 14,
       height = 10)

# figure data
write.xlsx(data_outcome,
           file = "../outcome/Publish/figure_data/fig3.xlsx")
