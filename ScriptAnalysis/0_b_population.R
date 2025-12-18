
# get the population data

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# data --------------------------------------------------------------------

remove(list = ls())

source('./function/theme_set.R')

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1)|> 
     mutate(Group = factor(Group, levels = disease_groups)) |> 
     arrange(Group, desc(Cases)) |> 
     select(Disease, Fullname, Shortname, Group) 

# list files in the folder: region data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "rate.csv",
                                 full.names = T)

# function to get the 
mode_function <- function(x) {
     ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
}

data_region <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2008 & 
                 !str_detect(Areas, regex("zone", ignore_case = TRUE)) &
                 !str_detect(Areas, regex("region", ignore_case = TRUE))) |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Year, Areas, Population) |> 
     filter(Year < 2025) |> 
     # unique() |> 
     pivot_wider(names_from = Areas, values_from = Population,
                 values_fn = list(Population = mode_function)) |> 
     arrange(Year)

rm(list_disease_files)

total <- rowSums(data_region[,3:ncol(data_region)],na.rm = T)
names(total) <- data_region$Year
total
data_region$Year[data_region$Total != rowSums(data_region[,3:79],na.rm = T)]

# save the data ------------------------------------------------------------

write.xlsx(data_region, "../Data/Population/province.xlsx")
