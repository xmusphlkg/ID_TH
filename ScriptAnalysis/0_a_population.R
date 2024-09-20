
# get the population data

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# data --------------------------------------------------------------------

remove(list = ls())

data_class <- read.csv("../Data/DiseaseClass.csv") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

# list files in the folder: region data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "rate.csv",
                                 full.names = T)
data_region <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2007 & 
                 !str_detect(Areas, regex("zone", ignore_case = TRUE)) &
                 !str_detect(Areas, regex("region", ignore_case = TRUE))) |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Year, Areas, Population) |> 
     filter(Year < 2024) |> 
     unique() |> 
     pivot_wider(names_from = Areas, values_from = Population,
                 values_fn = list(Population = max))

rm(list_disease_files)


