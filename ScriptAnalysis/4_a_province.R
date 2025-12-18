
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

load("./temp/month.RData")

data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

# list files in the folder: region data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "rate.csv",
                                 full.names = T)
data_region <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2008 & Areas != 'Total' & 
                 !str_detect(Areas, regex("zone", ignore_case = TRUE)) &
                 !str_detect(Areas, regex("region", ignore_case = TRUE))) |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Group, Shortname, Year, Areas, Incidence, Mortality)
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
     mutate(NAME_1 = case_when(NAME_1 == "Bangkok Metropolis" ~ "Bangkok",
                               NAME_1 == "Bueng Kan" ~ "Bungkan",
                               NAME_1 == "Phra Nakhon Si Ayutthaya" ~ "P.Nakhon S.Ayutthaya",
                               TRUE ~ NAME_1))

# check the difference between the two datasets
which(!unique(data_map$NAME_1) %in% unique(data_region$Areas))
which(!unique(data_region$Areas) %in% unique(data_map$NAME_1))

disease_name <- data_class$Shortname

save.image(file = "./temp/province.RData")

data_region_leading <- data_region |>
     mutate(Year_group = as.character(Year),
            Year_group = factor(Year_group),
            Year_mark = as.integer(Year_group)) |> 
     group_by(Group, Shortname, Year_group, Year_mark, Areas) |>
     summarise(Incidence = mean(Incidence, na.rm = T),
               Mortality = mean(Mortality, na.rm = T),
               .groups = 'drop') |> 
     # finding leading causes of death, case in each age group
     group_by(Areas, Year_group, Year_mark) |>
     summarise(Max_Incidence = max(Incidence, na.rm = T),
               Max_Mortality = max(Mortality, na.rm = T),
               Max_Incidence_Disease = Shortname[which.max(Incidence)],
               Max_Mortality_Disease = Shortname[which.max(Mortality)],
               .groups = 'drop') |> 
     # replace 0 deaths with no deaths
     mutate(Max_Mortality_Disease = if_else(Max_Mortality == 0, 'No deaths', Max_Mortality_Disease),
            Max_Mortality = if_else(Max_Mortality == 0, NA_real_, Max_Mortality))

# plot --------------------------------------------------------------------

set.seed(20240901)

source("./function/theme_set.R")

# load('./fill_color_disease_max.RData')

year_group <- unique(data_region_leading$Year_group)

max_disease <- data_region_leading |>
     select(Max_Incidence_Disease, Max_Mortality_Disease) |>
     unlist() |>
     table() |> 
     sort(decreasing = T) |>
     as.data.frame() |> 
     rename(Disease = Var1, Count = Freq)

# choose top 10 diseases excluding the special 'No deaths' label
top10_diseases <- head(max_disease$Disease[max_disease$Disease != 'No deaths'], 10)

max_disease <- max_disease |>
     mutate(DiseaseLabel = case_when(Disease %in% top10_diseases ~ as.character(Disease),
                                     Disease == 'No deaths' ~ 'No deaths',
                                     TRUE ~ 'Others'))

# desired legend/order: top diseases by count, then Others, then No deaths
disease_levels <- c(as.character(top10_diseases), 'Others', 'No deaths')

fill_color_disease <- c(fill_color_disease, "grey", 'white')
names(fill_color_disease) <- disease_levels

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
          mutate(DiseaseLabel = factor(DiseaseLabel, levels = disease_levels))
     
     fig <- ggplot(data) +
          geom_sf(aes(fill = DiseaseLabel),
                  color = "black", linewidth = 0.3, show.legend = T) +
          scale_fill_manual(values = fill_color_disease,
                            drop = F,
                            na.translate = F,
                            na.value = "grey30") +
          theme_map() +
          theme(legend.position = "bottom",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                legend.text = element_text(face = "bold", size = 10),
                legend.title = element_text(face = "bold", size = 12),
                legend.box.background = element_rect(fill = "transparent", colour = "transparent"))+
          # display all disease label in the legend
          guides(fill = guide_legend(ncol = 1, title.position = 'top')) +
          labs(fill = paste0("Leading disease in\n", tolower(str_extract(index, "Incidence|Mortality"))),
               title = paste(LETTERS[y],
                             year_group[y],
                             sep = ": "))
     
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
     plot_layout(ncol = 6, guides = "collect")

fig_mortality <- lapply(1:length(year_group), plot_map_group,
                        data_region_leading = data_region_leading,
                        data_map = data_map,
                        year_group= year_group,
                        index = 'Max_Mortality_Disease')
fig_mortality <- fig_mortality |>
     reduce(`+`) +
     guide_area() +
     plot_layout(ncol = 6, guides = "collect")

# save plot --------------------------------------------------------------

ggsave(filename = "../outcome/Appendix/Supplementary Appendix 1_4/incidence.png",
       plot = fig_incidence,
       width = 14,
       height = 13)

ggsave(filename = "../outcome/Appendix/Supplementary Appendix 1_4/mortality.png",
       plot = fig_mortality,
       width = 14,
       height = 13)

source("./4_b_appendix.R")
