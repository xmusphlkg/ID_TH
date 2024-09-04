
# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(cowplot)
library(sf)
library(biscale)
library(ggthemes)
library(doParallel)
library(openxlsx)

# data --------------------------------------------------------------------

remove(list = ls())

# source("./function/theme_set.R")

load("./month.RData")

data_class <- read.csv("../Data/DiseaseClass.csv") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

# list files in the folder: region data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "rate.csv",
                                 full.names = T)
data_region <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2007 & Areas != 'Total' & 
                 !str_detect(Areas, regex("zone", ignore_case = TRUE)) &
                 !str_detect(Areas, regex("region", ignore_case = TRUE))) |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Group, Shortname, Year, Areas, Incidence, Mortality, CFR) |> 
     filter(Year < 2024)
rm(list_disease_files)

# unique(data_region$Areas)

data_map <- read_sf("../Data/gadm41_THA_shp/gadm41_THA_1.shp") |> 
     # drop NL_NAME_1
     select(-NL_NAME_1)

# unique(data_map$NAME_1)

# rename the NAME_1 column:
# Bangkok Metropolis: Bangkok
# Bueng Kan: Bungkan
# Phra Nakhon Si Ayutthaya: P.Nakhon S.Ayutthaya

data_map <- data_map |>
     mutate(NAME_1 = case_when(
          NAME_1 == "Bangkok Metropolis" ~ "Bangkok",
          NAME_1 == "Bueng Kan" ~ "Bungkan",
          NAME_1 == "Phra Nakhon Si Ayutthaya" ~ "P.Nakhon S.Ayutthaya",
          TRUE ~ NAME_1
     ))

# check the difference between the two datasets
which(!unique(data_map$NAME_1) %in% unique(data_region$Areas))
which(!unique(data_region$Areas) %in% unique(data_map$NAME_1))

disease_name <- data_class$Shortname

save.image(file = "./region.RData")

data_region_leading <- data_region |>
     mutate(Year_group = if_else(Year >= 2019,
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
     group_by(Group, Shortname, Year_group, Year_mark, Areas) |>
     summarise(Incidence = mean(Incidence, na.rm = T),
               Mortality = mean(Mortality, na.rm = T),
               CFR = mean(CFR, na.rm = T),
               .groups = 'drop') |> 
     # finding leading causes of death, case and CFR in each age group
     group_by(Areas, Year_group, Year_mark) |>
     summarise(Max_Incidence_Disease = Shortname[which.max(Incidence)],
               Max_Mortality_Disease = Shortname[which.max(Mortality)],
               Max_CFR_Disease = Shortname[which.max(CFR)],
               .groups = 'drop')

# plot --------------------------------------------------------------------

set.seed(20240901)

source("./function/theme_set.R")

year_group <- unique(data_region_leading$Year_group)

max_disease <- data_region_leading |>
     select(Max_Incidence_Disease, Max_Mortality_Disease) |>
     unlist() |>
     table() |> 
     sort(decreasing = T) |>
     as.data.frame() |> 
     rename(Disease = Var1, Count = Freq) |>
     mutate(DiseaseLabel = if_else(Count >= 10, Disease, "Others"))
fill_color_disease <- c(fill_color_disease, "grey")
names(fill_color_disease) <- c(head(max_disease$DiseaseLabel, 10), "Others")

data_region_leading <- data_region_leading |>
     pivot_longer(cols = c(Max_Incidence_Disease, Max_Mortality_Disease),
                  names_to = "Type", values_to = "Disease") |>
     left_join(max_disease, by = "Disease")
     
plot_map_group <- function(index, data_region_leading, data_map, year_group, y) {
     
     # y <- 1
     # index <- 'Max_Incidence_Disease'
     
     data <- sp::merge(data_map, data_region_leading |>
                                      filter(Year_group == year_group[y] & Type == index) |>
                                      select(Areas, Disease, DiseaseLabel),
                                 by.x = "NAME_1", by.y = "Areas", all.x = T) |> 
          mutate(DiseaseLabel = factor(DiseaseLabel, levels = unique(max_disease$DiseaseLabel)))
     
     fig <- ggplot(data) +
          geom_sf(aes(fill = DiseaseLabel),
                  color = "black", linewidth = 0.3, show.legend = T) +
          scale_fill_manual(values = fill_color_disease,
                            drop = F,
                            na.translate = F,
                            na.value = "white") +
          theme_map() +
          theme(legend.position = "bottom",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                legend.text = element_text(face = "bold", size = 10),
                legend.title = element_text(face = "bold", size = 12),
                legend.box.background = element_rect(fill = "transparent", colour = "transparent"))+
          # display all disease label in the legend
          guides(fill = guide_legend(ncol = 1, title.position = 'top')) +
          labs(fill = paste0("Leading disease in\n", tolower(str_extract(index, "Incidence|Mortality"))),
               title = LETTERS[ifelse(index == 'Max_Incidence_Disease', y, y + 9)])
     
     return(fig)
}

fig_incidence <- lapply(1:length(year_group), plot_map_group,
                        data_region_leading = data_region_leading,
                        data_map = data_map,
                        year_group= year_group,
                        index = 'Max_Incidence_Disease')
fig_incidence <- fig_incidence |>
     reduce(`+`) +
     guide_area() +
     plot_layout(ncol = 5, guides = "collect")

fig_mortality <- lapply(1:length(year_group), plot_map_group,
                        data_region_leading = data_region_leading,
                        data_map = data_map,
                        year_group= year_group,
                        index = 'Max_Mortality_Disease')
fig_mortality <- fig_mortality |>
     reduce(`+`) +
     guide_area() +
     plot_layout(ncol = 5, guides = "collect")

# merge plot --------------------------------------------------------------

# bind the two lists
fig <- fig_incidence / fig_mortality

ggsave(filename = "../outcome/publish/fig5.pdf",
       plot = fig,
       width = 12,
       height = 18,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
write.xlsx(data_region_leading,
           file = "../outcome/Appendix/figure_data/fig5.xlsx")
