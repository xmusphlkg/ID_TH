
# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(openxlsx)

# data --------------------------------------------------------------------

remove(list = ls())

source("./function/theme_set.R")

load("./month.RData")

data_class <- read.csv("../Data/DiseaseClass.csv") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

data_agegroup <- read.csv('../Data/AgeClass.csv')

# list files in the folder: case data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "ac.csv",
                                 full.names = T)
data_case <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2007 & Areas == 'Total') |> 
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Group, Shortname, Age, Year, Cases) |> 
     filter(Year < 2024)
rm(list_disease_files)

# list files in the folder: death data
list_death_files <- list.files("../Data/CleanData/",
                                 pattern = "ad.csv",
                                 full.names = T)
data_death <- lapply(list_death_files, read.csv) |>
     bind_rows() |> 
     filter(Year >= 2007 & Areas == 'Total') |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |>
     select(Group, Shortname, Age, Year, Deaths = Cases) |> 
     filter(Year < 2024)
rm(list_death_files)

# merge case and death data
data_age <- data_case |> 
     left_join(data_death, by = c('Group', 'Shortname', 'Age', 'Year')) |> 
     left_join(data_agegroup, by = 'Age') |>
     filter(AgeGroup != 'Unknown') |> 
     group_by(Shortname, Group, AgeGroup, AgeGroupID, Year) |>
     summarise(Cases = sum(Cases, na.rm = T),
               Deaths = sum(Deaths, na.rm = T),
               .groups = 'drop') |>
     mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths),
            CFR = Deaths / Cases * 100,
            AgeGroup = factor(AgeGroup, levels = unique(data_agegroup$AgeGroup)),
            Year_group = if_else(Year >= 2019,
                                 as.character(Year),
                                 if_else(Year >= 2015,
                                         '2015-2018',
                                         if_else(Year >= 2011,
                                                 '2011-2014',
                                                 '2007-2010'))),
            Year_mark = case_when(Year_group == '2007-2010' ~ 1,
                                  Year_group == '2011-2014' ~ 2,
                                  Year_group == '2015-2018' ~ 3,
                                  TRUE ~ as.integer(Year_group) - 2015)) |> 
     # finding leading causes of death, case and CFR in each age group
     group_by(AgeGroup, AgeGroupID, Year_group, Year_mark) |>
     summarise(Max_Cases_Disease = Shortname[which.max(Cases)],
               Max_Deaths_Disease = Shortname[which.max(Deaths)],
               Max_CFR_Disease = Shortname[which.max(CFR)],
               .groups = 'drop')

# get disease list

set.seed(20240901)

max_disease <- data_age |>
     select(Max_Cases_Disease, Max_Deaths_Disease, Max_CFR_Disease) |>
     unlist() |>
     table() |> 
     sort(decreasing = T) |>
     as.data.frame()
fill_color_disease_max <- rep(fill_color_disease[-c(1:5)], ceiling((nrow(max_disease)-5)/5))
fill_color_disease_max <- c(fill_color_disease[1:5], fill_color_disease_max[1:(nrow(max_disease)-5)])

names(fill_color_disease_max) <- max_disease$Var1
     
data_outcome <- list()

for (i in 1:3) {
     data <- data_age |>
          select(AgeGroup, AgeGroupID, Year_group, Year_mark, colnames(data_age)[i + 4]) |> 
          rename(Outcome = colnames(data_age)[i + 4]) |> 
          left_join(data_class, by = c('Outcome' = 'Shortname')) |>
          select(-c(Disease, Fullname))
     
     data_outcome[[paste('panel', LETTERS[i], sep = '')]] <- data
     
     fig <- data |>
          ggplot(aes(x = AgeGroupID, y = Year_mark, fill = Outcome)) +
          geom_tile(aes(width = 1, height = 1),
                    color = 'white',
                    show.legend = F) +
          geom_text(aes(label = Outcome), size = 3) +
          scale_fill_manual(values = fill_color_disease_max) +
          coord_cartesian(ylim = c(length(unique(data$Year_mark))+0.5, 0.5)) +
          scale_x_continuous(breaks = unique(data$AgeGroupID),
                             labels = unique(data$AgeGroup),
                             expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(breaks = unique(data$Year_mark),
                             labels = unique(data$Year_group),
                             expand = expansion(mult = c(0, 0)))+
          theme_plot()+
          labs(title = LETTERS[i],
               x = ifelse(i == 3, 'Age (years)', ''),
               y = NULL)
     
     assign(paste('fig', i, sep = ''), fig)
}

fig <- fig1 + fig2 + fig3 +
     plot_layout(ncol = 1, heights = c(1, 1, 1))

ggsave(filename = "../outcome/publish/fig4.pdf",
       plot = fig,
       width = 12,
       height = 8,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
write.xlsx(data_outcome,
           file = "../outcome/Appendix/figure_data/fig4.xlsx")
