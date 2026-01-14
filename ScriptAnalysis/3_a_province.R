
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

load("./temp/month_subgroup.RData")

data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

## R506 --------------------------------------------------------

# list files in the folder: region data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "rate.csv",
                                 full.names = T)
data_region_1 <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2008,
            Year <= 2023,
            Areas != 'Total',
            !str_detect(Areas, regex("zone", ignore_case = TRUE)),
            !str_detect(Areas, regex("region", ignore_case = TRUE))) |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Group, Shortname, Year, Areas, Incidence, Mortality)

rm(list_disease_files)

## D506 --------------------------------------------------------

# translate location names
data_map_location <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseLocation')

# population data
data_population_location <- read.xlsx("../Data/Population/province.xlsx") |> 
     filter(Year >= 2024) |> 
     pivot_longer(cols = -Year,
                  names_to = "location",
                  values_to = "population") |> 
     # replace . with space
     mutate(location = str_replace_all(location, "\\.", " "))

# read disease name and location mapping
data_map_name <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseName') |> 
     filter(!is.na(short_name))

# duplicate 2024 population to 2025
data_population_location <- data_population_location |>
     filter(Year == 2024) |>
     mutate(Year = 2025) |> 
     rbind(data_population_location)

# cases data
data_region_2 <- data_week_location |> 
     group_by(year, disease, health_zone, location) |>
     summarise(cases = sum(cases, na.rm = T),
               .groups = 'drop') |> 
     # add disease short name
     left_join(data_map_name, by = c("disease" = "original_name")) |> 
     select(year, original_name = disease, disease = short_name, health_zone, location, cases)

# deaths data
data_region_2 <- data_week_location_deaths |>
     group_by(year, Shortname, location_value) |>
     summarise(deaths = sum(deaths, na.rm = T),
               .groups = 'drop') |> 
     # add location name, health zone
     left_join(data_map_location, by = c("location_value" = "location_th")) |> 
     select(year, disease = Shortname, health_zone, location, deaths) |> 
     # join cases and deaths
     full_join(data_region_2, by = c("year", "disease", "health_zone", "location")) |> 
     filter(year >= 2024) |> 
     mutate(# fixed location names: Bueng Kan -> Bungkan; Phra Nakhon Si Ayutthaya -> P Nakhon S Ayutthaya
          location = case_when(location == "Bueng Kan" ~ "Bungkan",
                               location == "Phra Nakhon Si Ayutthaya" ~ "P Nakhon S Ayutthaya",
                               TRUE ~ location)) |> 
     # join population
     left_join(data_population_location, by = c("year" = "Year", "location" = "location")) |> 
     mutate(cases = if_else(deaths == 0 & is.na(cases), 0, cases),
            deaths = if_else(cases == 0 & is.na(deaths), 0, deaths),
            # calculate incidence and mortality per 100,000 population
            Incidence = cases/population * 1e5,
            Mortality = deaths/population * 1e5) |> 
     # drop diseases not in the class data
     filter(disease %in% data_class$Shortname)

# check population is na
unique(data_region_2$location[is.na(data_region_2$population)])

data_region_2 |>
     filter(is.na(Incidence) | is.na(Mortality), location != "Unspecified") |> 
     select(year, disease, health_zone, location, cases, deaths, population)

# combine the two region datasets
data_region <- data_region_2 |> 
     select(Shortname = disease, Year = year, Areas = location, Incidence, Mortality) |>
     # add group
     left_join(select(data_class, Shortname, Group), by = "Shortname") |> 
     rbind(data_region_1) |> 
     # drop Unspecified areas
     filter(Areas != "Unspecified") |> 
     # fixed location names: P.Nakhon S.Ayutthaya -> P Nakhon S Ayutthaya
     mutate(Areas = case_when(Areas == "P.Nakhon S.Ayutthaya" ~ "P Nakhon S Ayutthaya",
                              TRUE ~ Areas))

## map data --------------------------------------------------------

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
                               NAME_1 == "Phra Nakhon Si Ayutthaya" ~ "P Nakhon S Ayutthaya",
                               TRUE ~ NAME_1))

# check the difference between the two datasets
which(!unique(data_map$NAME_1) %in% unique(data_region$Areas))
which(!unique(data_region$Areas) %in% unique(data_map$NAME_1))

disease_name <- data_class$Shortname

save(data_map, data_region, file = "./temp/province.RData")

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
     
     # y <- 18
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
          guides(fill = guide_legend(nrow = 1, title.position = 'top')) +
          labs(fill = paste0("Leading disease in ", tolower(str_extract(index, "Incidence|Mortality"))),
               title = paste(LETTERS[y],
                             year_group[y],
                             sep = ": "))
     
     return(fig)
}

fig_incidence <- lapply(1:length(year_group), plot_map_group,
                        data_region_leading = data_region_leading,
                        data_map = data_map,
                        year_group= year_group,
                        index = 'Max_Incidence_Disease') |>
     reduce(`+`) +
     plot_layout(ncol = 6, guides = "collect")&
     theme(legend.position = "bottom")

fig_mortality <- lapply(1:length(year_group), plot_map_group,
                        data_region_leading = data_region_leading,
                        data_map = data_map,
                        year_group= year_group,
                        index = 'Max_Mortality_Disease') |>
     reduce(`+`) +
     plot_layout(ncol = 6, guides = "collect")&
     theme(legend.position = "bottom")

# save plot --------------------------------------------------------------

ggsave(filename = "../outcome/Appendix/Supplementary Appendix 1_4/incidence.png",
       plot = fig_incidence,
       width = 14,
       height = 14)

ggsave(filename = "../outcome/Appendix/Supplementary Appendix 1_4/mortality.png",
       plot = fig_mortality,
       width = 14,
       height = 14)

source("./3_b_appendix.R")
