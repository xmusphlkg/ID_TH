
# packages ----------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

# Relative path (from project root): "Data/WeeklyCasesData"
data_dir <- file.path("../Data", "WeeklyCasesData")

# Find all CSV files recursively
csv_files <- list.files(path = data_dir, pattern = "csv",
                        include.dirs = TRUE, all.files = TRUE,
                        recursive = TRUE, full.names = TRUE)

data_list <- lapply(csv_files, function(f) {
     tryCatch(
          read.csv(f) |> 
               mutate(filename = f),
          error = function(e) {
               warning(sprintf("Failed to read '%s': %s", f, e$message))
               NULL
          }
     )
})

data_week <- bind_rows(data_list)
names(data_week) <- c('week_value', 'week_alias', 'location_value', 'location_alias', 'cases', 'filename')

# Extract year, disease, and age_group from the full filepath in `filename`
# Examples:
#  "../Data/WeeklyCasesData/2564/กลุ่มอายุ/Shigellosis...__60.csv" -> year=2564, disease=Shigellosis..., age_group=60
#  "../Data/WeeklyCasesData/2563/Acute_diarrhea.csv" -> year=2563, disease=Acute_diarrhea, age_group=NA

data_week <- data_week |>
     mutate(filepath = as.character(filename),
            filename_only = basename(filepath),
            disease_full = tools::file_path_sans_ext(filename_only),
            # extract year as first 3-4 digit path component (e.g., 2563, 2564)
            year = stringr::str_extract(filepath, "(?<=/|\\\\)\\d{3,4}(?=/|\\\\)"),
            year = if_else(is.na(year), stringr::str_extract(filepath, "\\d{3,4}"), year),
            # trans year to integer, and BE to CE
            year = as.integer(year) - 543,
            # detect age group suffix after double-underscore in filename (e.g. __0_4, __60)
            age_group = if_else(stringr::str_detect(disease_full, "__"),
                                stringr::str_replace(disease_full, ".*__", ""),
                                NA_character_),
            # disease name is the part before any '__' suffix
            disease = stringr::str_replace(disease_full, "__.*$", "")) |>
     select(-filename_only, -disease_full)

# subset data -------------------------------------------------------------

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1)|> 
     select(-c(Cases, Count, Including, Label))

data_map_name <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseName') |> 
     filter(!is.na(short_name))

data_map_location <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseLocation')

# check all disease in data_week are in data_class
unique(data_map_name$short_name)[!unique(data_map_name$short_name) %in% unique(data_class$Shortname)]

unique(data_class$Shortname)[!unique(data_class$Shortname) %in% unique(data_map_name$short_name)]

data_weeks <- data_week |> 
     filter(location_value == "%all%",
            is.na(age_group),
            week_value != "%all%") |> 
     left_join(data_map_name, by = c(disease = 'original_name')) |> 
     filter(!is.na(short_name)) |> 
     select(year, Shortname = short_name, week = week_value, cases)

data_week_age <- data_week |>
     filter(location_value  == "%all%",
            !is.na(age_group),
            week_value != "%all%") |> 
     mutate(age_group = case_when(age_group == "0_4" ~ "0-4",
                                  age_group == "5_9" ~ "5-9",
                                  age_group == "10_14" ~ "10-14",
                                  age_group == "15_19" ~ "15-19",
                                  age_group == "20_29" ~ "20-29",
                                  age_group == "30_39" ~ "30-39",
                                  age_group == "40_49" ~ "40-49",
                                  age_group == "50_59" ~ "50-59",
                                  age_group == "60" ~ "60+",
                                  TRUE ~ age_group)) |> 
     left_join(data_map_name, by = c(disease = 'original_name')) |> 
     filter(!is.na(short_name)) |> 
     select(year, Shortname = short_name, week = week_value, age_group, cases)

data_week_location <- data_week |> 
     filter(location_value != "%all%",
            is.na(age_group),
            week_value != "%all%") |> 
     left_join(data_map_location, by = c(location_value = 'location_th')) |>
     select(year, disease, week = week_value, location, region, health_zone, cases)

save(data_week = data_weeks, data_week_age, data_week_location,
     file = "week.RData")

rm(list = ls())

# compare data ------------------------------------------------------------

load('./week.RData')
load('./month.RData')

data_date_seq <- data.frame(date = seq.Date(from = as.Date("2019-12-01"), to = as.Date("2026-1-31"), by = "day")) |> 
     mutate(year = year(date),
            week = isoweek(date),
            month = month(date),
            # fixed week 1 in December to next year, week 52/53 in January to previous year
            year = if_else(month == 12 & week == 1, year + 1, year),
            year = if_else(month == 1 & week >= 52, year - 1, year)) |> 
     filter(year >= 2020 & year <= 2025)



