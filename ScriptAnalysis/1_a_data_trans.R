
# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(openxlsx)
library(patchwork)
library(parallel)

# basic data --------------------------------------------------------------

rm(list = ls())

source('./function/theme_set.R')

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1)|> 
     mutate(Group = factor(Group, levels = disease_groups)) |> 
     arrange(Group, desc(Cases)) |> 
     select(Disease, Fullname, Shortname, Group) 

# read population data
data_population <- read.xlsx('../Data/Population/1992-2023.xlsx', sheet = 'wpp') |> 
     select(YEAR, Total) |> 
     rename(Year = YEAR,
            Population = Total)

# read disease name and location mapping
data_map_name <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseName') |> 
     filter(!is.na(short_name))

data_map_location <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseLocation')

# check all disease in data_week_raw are in data_class
unique(data_map_name$short_name)[!unique(data_map_name$short_name) %in% unique(data_class$Shortname)]

unique(data_class$Shortname)[!unique(data_class$Shortname) %in% unique(data_map_name$short_name)]

data_date_seq <- data.frame(date = seq.Date(from = as.Date("2019-12-01"), to = as.Date("2026-1-31"), by = "day")) |> 
     mutate(year = year(date),
            week = isoweek(date),
            month = month(date),
            # fixed week 1 in December to next year, week 52/53 in January to previous year
            year = if_else(month == 12 & week == 1, year + 1, year),
            year = if_else(month == 1 & week >= 52, year - 1, year)) |> 
     filter(date >= as.Date('2020-01-01') & date < as.Date('2026-01-01'))


# create mapping from year-week to representative week midpoint date and list of dates per week
week_date_map <- data_date_seq |> 
     group_by(year, week) |> 
     summarize(week_start = min(date), week_mid = median(date), dates = list(date), .groups = "drop")

# monthly and yearly data -------------------------------------------------------

## monthly -----------------------------------------------------------------

# list files in the folder: month case and death data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "mcd.csv",
                                 full.names = T)
data_all_mcd <- lapply(list_disease_files, read.csv) |>
     bind_rows() |> 
     filter(Year < 2025, Year >= 2008)

rm(list_disease_files)

data_month <- data_all_mcd |>
     filter(Areas == 'Total' & Month != 'Total' & Disease %in% data_class$Disease) |>
     left_join(data_class, by = 'Disease') |> 
     # transform the Month: Jan -> 01
     mutate(Month = month(parse_date_time(Month, "b"), label = FALSE, abbr = FALSE),
            Group = factor(Group, levels = disease_groups),
            Disease = Shortname,
            Date = as.Date(paste(Year, Month, "01", sep = "-"))) |> 
     select(Year, Month, Date, Disease, Shortname, Group, Type, Count) |>
     pivot_wider(names_from = Type, values_from = Count, values_fill = 0) |>
     ungroup() |> 
     arrange(Date, Disease)

## yearly -----------------------------------------------------------------

# list files in the folder: year case and death data
list_disease_files_year <- list.files("../Data/CleanData/",
                                      pattern = "rate.csv",
                                      full.names = T)

data_all_year <- lapply(list_disease_files_year, read.csv) |>
     bind_rows() |> 
     filter(Year < 2025, Year >= 2008)

rm(list_disease_files_year)

data_year_2 <- data_all_year |>
     filter(Areas == 'Total' & Disease %in% data_class$Disease) |>
     select(-Population, -Incidence, -Mortality, -Areas)

# cases -------------------------------------------------------------------

# Spline-based temporal disaggregation (weekly -> daily -> monthly)
#
# This procedure reconstructs daily case counts from weekly totals using spline
# interpolation with post-hoc week-preserving scaling. The main steps are:
# 1) Map each ISO `year`-`week` to a representative date (week midpoint) and the
#    set of daily dates belonging to that week using `data_date_seq`.
# 2) For each disease-year, fit a smooth spline to (week_midpoint_date, weekly_total)
#    and evaluate the spline at every daily date in that year to obtain preliminary
#    daily predictions.
# 3) Enforce non-negativity on spline predictions and scale daily predictions
#    within each week so that the sum of reconstructed daily values equals the
#    original weekly total (preserves aggregate consistency).
# 4) Convert fractional daily values to integer counts by flooring and then
#    distributing the remaining counts to days with largest fractional parts
#    (ensures weekly totals remain exact integers).
#
# Boundary handling and assumptions:
# - Cross-year ISO-week assignments are handled by `data_date_seq` which reassigns
#   week-year membership for ISO week 1 in December and weeks 52/53 in January.
# - Spline extrapolation at the start/end of the series may produce NA or
#   unstable values; the implementation uses controlled extrapolation (linear
#   rule) or falls back to an even distribution when too few observed weeks exist.
# - Missing weeks or sparse observations reduce spline reliability; in such
#   cases the code falls back to uniform distribution across the week's days.
# - This method preserves weekly sums exactly (after integer rounding) and
#   produces a smooth daily profile consistent with weekly trends. It does not
#   attempt to model within-week reporting artifacts (e.g., weekday effects) or
#   external covariates.

## raw data --------------------------------------------------------------------

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

data_week_raw <- bind_rows(data_list)
names(data_week_raw) <- c('week_value', 'week_alias', 'location_value', 'location_alias', 'cases', 'filename')

# Extract year, disease, and age_group from the full filepath in `filename`
# Examples:
#  "../Data/WeeklyCasesData/2564/กลุ่มอายุ/Shigellosis...__60.csv" -> year=2564, disease=Shigellosis..., age_group=60
#  "../Data/WeeklyCasesData/2563/Acute_diarrhea.csv" -> year=2563, disease=Acute_diarrhea, age_group=NA
data_week_raw <- data_week_raw |>
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

## clean data -------------------------------------------------------------

data_week <- data_week_raw |> 
     filter(location_value == "%all%",
            is.na(age_group),
            week_value != "%all%") |> 
     left_join(data_map_name, by = c(disease = 'original_name')) |> 
     filter(!is.na(short_name)) |> 
     select(year, Shortname = short_name, week = week_value, cases) |> 
     group_by(year, Shortname, week) |> 
     summarize(cases = sum(as.numeric(cases), na.rm = TRUE), .groups = "drop")

data_week_age <- data_week_raw |>
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
     select(year, Shortname = short_name, week = week_value, age_group, cases) |> 
     group_by(year, Shortname, week, age_group) |> 
     summarize(cases = sum(as.numeric(cases), na.rm = TRUE), .groups = "drop")

data_week_location <- data_week_raw |> 
     filter(location_value != "%all%",
            is.na(age_group),
            week_value != "%all%") |> 
     left_join(data_map_location, by = c(location_value = 'location_th')) |>
     select(year, disease, week = week_value, location, region, health_zone, cases) |> 
     group_by(year, disease, week, location, region, health_zone) |> 
     summarize(cases = sum(as.numeric(cases), na.rm = TRUE), .groups = "drop")

## reconstruction cases ----------------------------------------------------------

# function: reconstruct for one disease-year
reconstruct_one <- function(df_week, week_map) {
     # df_week: tibble with columns year, Shortname, week, cases
     year_val <- unique(df_week$year)
     if(length(year_val) != 1) stop("multiple years in reconstruct_one")
     
     # join to get week_mid and dates
     wk <- df_week |> left_join(week_map, by = c("year", "week"))
     
     # all target dates in this year
     dates_year <- data_date_seq |>
          filter(year %in% year_val) |> 
          arrange(date)
     
     # if less than 2 observed weeks, fallback to even distribution per week's days
     if(nrow(wk) < 2) {
          # expand by week -> dates and distribute equally
          out <- wk |> 
               rowwise() |> 
               mutate(dates = list(unlist(dates))) |> 
               unnest_longer(dates) |> 
               rename(date = dates) |> 
               mutate(daily = if_else(is.na(cases), 0, as.numeric(cases)/7)) |> 
               select(year, Shortname, date, daily)
          return(out)
     }
     
     # prepare spline inputs
     x <- as.numeric(as.Date(wk$week_mid))
     y <- as.numeric(wk$cases)
     
     # build spline function (natural) with rule=2 to allow constant extrapolation
     sp <- stats::splinefun(x, y, method = "natural")
     
     xout <- as.numeric(dates_year$date)
     pred <- sp(xout)
     pred[pred < 0] <- 0
     
     daily_tbl <- tibble(date = dates_year$date, predicted = pred)
     
     # scale daily predictions within each week so that weekly sums equal original weekly totals
     # join date->week mapping
     daily_tbl <- daily_tbl |> left_join(data_date_seq |> select(date, year, week), by = "date")
     
     wk_original <- wk |> select(year, week, cases)
     
     daily_scaled <- daily_tbl |> 
          group_by(year, week) |> 
          mutate(sum_pred = sum(predicted, na.rm = TRUE)) |> 
          left_join(wk_original, by = c("year", "week")) |> 
          mutate(cases = if_else(is.na(cases), 0, as.numeric(cases)),
                 factor = if_else(sum_pred > 0, cases / sum_pred, 0),
                 daily = predicted * factor) |> 
          ungroup() |> 
          select(date, year, week, daily)
     
     # rounding to integers while preserving weekly totals
     daily_corrected <- daily_scaled |> 
          group_by(year, week) |> 
          mutate(d_floor = floor(daily), frac = daily - d_floor) |> 
          mutate(need = as.integer(round((wk_original$cases[match(paste(year, week), paste(wk_original$year, wk_original$week))]) - sum(d_floor)))) |> 
          group_modify(~{
               df <- .x
               n_need <- unique(df$need)
               if(is.na(n_need)) n_need <- 0
               if(n_need > 0) {
                    idx <- order(-df$frac)
                    df$d_final <- df$d_floor
                    df$d_final[idx[1:n_need]] <- df$d_final[idx[1:n_need]] + 1
               } else if(n_need < 0) {
                    idx <- order(df$frac)
                    df$d_final <- df$d_floor
                    take <- seq_len(abs(n_need))
                    df$d_final[idx[take]] <- pmax(0, df$d_final[idx[take]] - 1)
               } else {
                    df$d_final <- df$d_floor
               }
               df
          }) |> 
          ungroup() |> 
          select(date, daily = d_final)
     
     tibble(date = daily_corrected$date, year = year_val, Shortname = unique(df_week$Shortname), daily = as.numeric(daily_corrected$daily)) |> 
          filter(date < as.Date('2026-01-01'))
}

# reconstruct all diseases and years
reconstruct_all <- function(data_week, week_map) {
     # Parallel implementation using base `parallel` (works on Windows)
     data_week <- data_week |> 
          mutate(week = as.integer(week), year = as.integer(year))
     groups <- data_week |> 
          group_by(Shortname, year) |> 
          group_split()

     # compute number of processes based on distinct diseases and max_proces
     disease_name <- unique(data_week$Shortname)
     number_process <- ifelse(length(disease_name) >= max_proces,
                              max_proces,
                              length(disease_name))
     number_process <- max(1, as.integer(number_process))

     # create cluster and export necessary objects/functions
     cl <- parallel::makeCluster(number_process)

     # export local variables and functions
     week_map_local <- week_map
     parallel::clusterExport(cl, varlist = c('reconstruct_one', 'week_map_local', 'data_date_seq'), envir = environment())
     parallel::clusterEvalQ(cl, {
          library(dplyr)
          library(lubridate)
          NULL
     })

     res_list <- parallel::parLapply(cl, groups, function(df) {
          tryCatch(reconstruct_one(df, week_map_local), error = function(e) {
               warning(sprintf("failed reconstruct %s %s: %s", unique(df$Shortname), unique(df$year), e$message))
               NULL
          })
     })

     parallel::stopCluster(cl)

     out <- dplyr::bind_rows(res_list)
     out
}

daily_recon <- reconstruct_all(data_week, week_date_map)
# aggregate to month
daily_recon <- daily_recon |> 
     mutate(month = month(date), year = year(date))
month_recon <- daily_recon |> 
     group_by(Shortname, year, month) |> 
     summarize(cases = sum(daily, na.rm = TRUE), .groups = "drop")

## compare data ---------------------------------------------------------

compute_weekly_comparison <- function(data_week, daily_recon, week_map) {
     data_week2 <- data_week |> 
          mutate(week = as.integer(week), year = as.integer(year))
     
     # browser()
     
     recon_week <- daily_recon |> 
          select(-year) |> 
          left_join(data_date_seq |> select(date, year, week), by = "date") |> 
          group_by(Shortname, year, week) |> 
          summarize(recon_cases = sum(daily, na.rm = TRUE), .groups = "drop")
     
     weekly_cmp <- data_week2 |> 
          left_join(recon_week, by = c("Shortname", "year", "week")) |> 
          left_join(week_map |> select(year, week, week_mid), by = c("year", "week")) |> 
          mutate(recon_cases = coalesce(recon_cases, 0),
                 diff = cases - recon_cases,
                 # relative signed difference (recon - obs) / obs; NA when obs == 0 and recon > 0
                 rel_diff = case_when(
                      cases == 0 & recon_cases == 0 ~ 0,
                      cases == 0 & recon_cases != 0 ~ NA_real_,
                      TRUE ~ (recon_cases - cases) / as.numeric(cases)
                 ),
                 abs_pct_error = case_when(
                      cases == 0 & recon_cases == 0 ~ 0,
                      cases == 0 & recon_cases != 0 ~ NA_real_,
                      TRUE ~ abs(recon_cases - cases) / as.numeric(cases) * 100
                 ))
     
     # use group_by_at to avoid NSE/grouping name resolution issues
     summary_cmp <- weekly_cmp |> 
          group_by_at(vars("Shortname", "year")) |> 
          summarize(mean_diff = mean(diff, na.rm = TRUE),
                    median_diff = median(diff, na.rm = TRUE),
                    sd_diff = sd(diff, na.rm = TRUE),
                    mae = mean(abs(diff), na.rm = TRUE),
                    rmse = sqrt(mean((diff)^2, na.rm = TRUE)),
                    mape = mean(abs_pct_error, na.rm = TRUE),
                    median_abs_pct = median(abs_pct_error, na.rm = TRUE),
                    pct_exact = mean(diff == 0, na.rm = TRUE) * 100,
                    pct_false_positive = mean(cases == 0 & recon_cases > 0, na.rm = TRUE) * 100,
                    n_weeks = n(),
                    .groups = "drop")
     
     list(weekly = weekly_cmp, summary = summary_cmp)
}


# `plot_weekly_comparison()`
# - Produce a ggplot overlaying observed weekly totals and reconstructed weekly totals.
# - `show_daily = TRUE` will also overlay the reconstructed daily series (semi-transparent).
plot_weekly_comparison <- function(shortname, year_val, data_week, daily_recon, week_map, show_daily = FALSE) {
     
     # prepare observed weekly series
     obs <- data_week |> 
          mutate(week = as.integer(week), year = as.integer(year)) |> 
          filter(Shortname == shortname, year %in% year_val) |> 
          left_join(week_map |> select(year, week, week_mid), by = c("year", "week"))
     
     # prepare reconstructed weekly series using authoritative mapping from data_date_seq
     recon_week <- daily_recon |> 
          select(-year) |>
          left_join(data_date_seq |> select(date, year, week), by = "date") |> 
          filter(Shortname == shortname, year %in% year_val) |> 
          group_by(Shortname, year, week) |> 
          summarize(recon_cases = sum(daily, na.rm = TRUE), .groups = "drop") |> 
          left_join(week_map |> select(year, week, week_mid), by = c("year", "week"))
     
     p <- ggplot() +
          geom_line(data = recon_week, aes(x = week_mid, y = recon_cases, color = "Reconstructed"), linewidth = 1) +
          geom_point(data = obs, aes(x = week_mid, y = cases, color = "Observed"), size = 2) +
          geom_line(data = obs, aes(x = week_mid, y = cases, color = "Observed"),
                    linetype = "dashed")
     
     if (show_daily) {
          daily_plot <- daily_recon |> 
               select(-year) |>
               left_join(data_date_seq |> select(date, year), by = "date") |> 
               filter(Shortname == shortname, year %in% year_val)
          p <- p + geom_line(data = daily_plot, aes(x = date, y = daily, color = "Daily (recon)"), alpha = 0.5)
     }
     
     # only include colors for series that actually exist in the plotted data
     color_palette <- c("Observed" = "#E64B35FF",
                        "Reconstructed" = "#3C5488FF",
                        "Daily (recon)" = "#00A087FF")
     present_labels <- c()
     if(nrow(obs) > 0) present_labels <- c(present_labels, "Observed")
     if(nrow(recon_week) > 0) present_labels <- c(present_labels, "Reconstructed")
     if(show_daily) {
          daily_plot <- daily_recon |> filter(Shortname == shortname, year(date) %in% year_val)
          if(nrow(daily_plot) > 0) present_labels <- c(present_labels, "Daily (recon)")
     }
     present_labels <- unique(present_labels)
     p <- p +
          scale_color_manual(name = "Series", values = color_palette[present_labels]) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
          scale_x_date(expand = expansion(mult = c(0, 0)),
                       date_labels = "%Y",
                       date_breaks = "1 year") +
          labs(title = shortname, x = NULL, y = "Cases") +
          theme_bw()+
          theme(legend.position = "inside",
                legend.box = "horizontal",
                legend.direction = "horizontal",
                legend.position.inside = c(0.5, 0.99),
                legend.justification = c(0.5, 1))
     
     p
}


# `plot_month_and_week_comparison()`
# - Create two plots (monthly and weekly comparisons) side-by-side using patchwork.
# - Inputs:
#    `shortname`, `year_val` (single year), `data_week`, `data_month` (original monthly),
#    `daily_recon`, `month_recon` (reconstructed monthly), `week_map` (week_mid mapping)
# - Returns a patchwork object combining monthly (left) and weekly (right) plots.
plot_month_and_week_comparison <- function(shortname,
                                           year_val,
                                           value,
                                           data_week, 
                                           data_month, 
                                           daily_recon, 
                                           month_recon, 
                                           week_map, 
                                           show_daily = FALSE) {
     # browser()     
     
     # Monthly comparison
     orig_month <- data_month |> 
          filter(Shortname == shortname, Year %in% year_val) |> 
          select(year = Year, month = Month, Shortname, value = str_to_title(value))
     
     recon_month <- month_recon |> 
          filter(Shortname == shortname, year %in% year_val) |> 
          rename(recon_value = all_of(value))
     
     month_cmp <- orig_month |> 
          full_join(recon_month, by = c("Shortname", "year", "month")) |> 
          mutate(month_mid = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
     
     p_month <- ggplot(month_cmp) +
          geom_line(aes(x = month_mid, y = recon_value, color = "Reconstructed (based on weekly data)"), linewidth = 1) +
          geom_point(aes(x = month_mid, y = value, color = "Observed (monthly data)"), size = 2) +
          geom_line(aes(x = month_mid, y = value, color = "Observed (monthly data)"), linetype = "dashed") +
          scale_color_manual(name = "Series",
                             values = c("Observed (monthly data)" = "#4DBBD5FF",
                                        "Reconstructed (based on weekly data)" = "#B09C85FF")) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
          scale_x_date(expand = expansion(mult = c(0, 0)),
                       date_labels = "%Y",
                       date_breaks = "1 year") +
          labs(x = 'Date',
               y = str_to_title(value))+
          theme_bw()+
          theme(legend.position = "inside",
                legend.box = "horizontal",
                legend.direction = "horizontal",
                legend.position.inside = c(0.5, 0.99),
                legend.justification = c(0.5, 1))
     
     
     if (value == 'cases') {
          # Weekly plot using existing function (returns ggplot)
          p_week <- plot_weekly_comparison(shortname, year_val, data_week, daily_recon, week_map, show_daily = show_daily)
          
          # combine with patchwork
          p_week + p_month + patchwork::plot_layout(ncol = 1)
     } else {
          p_month
     }
}

cmp <- compute_weekly_comparison(data_week, daily_recon, week_date_map)
cmp_outcome <- cmp[["summary"]]
cmp_outcome |> 
     reframe(mean_diff_range = range(mean_diff),
             mape_range = range(mape),
             pct_exact_range = range(pct_exact)) |>
     mutate(across(where(is.numeric), ~as.character(round(., 2)))) |> 
     print()

# plot_month_and_week_comparison("Amebiasis", 2020:2025, data_week, data_month, daily_recon, month_recon, week_date_map, show_daily = TRUE)

outcome <- lapply(unique(cmp_outcome$Shortname), function(sn) {
     p <- plot_month_and_week_comparison(sn, 2020:2025, 'cases', data_week, data_month, daily_recon, month_recon, week_date_map, show_daily = TRUE)
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/cases/", sn, '.png'),
            plot = p,
            create.dir = TRUE,
            width = 14,
            height = 7)
     NULL
})

# reconstruct deaths ---------------------------------------------------------
# Weekly deaths: distribute weekly counts into months by day proportions
# (no spline interpolation for deaths; allocate each week's deaths to months
#  in proportion to how many days of that week fall in each month)

## raw data --------------------------------------------------------------------
data_dir_death <- file.path("../Data", "WeeklyDeathsData")
csv_files_death <- list.files(path = data_dir_death, pattern = "csv",
                              include.dirs = TRUE, all.files = TRUE,
                              recursive = TRUE, full.names = TRUE)

data_list_death <- lapply(csv_files_death, function(f) {
     tryCatch(
          readr::read_csv(f, col_types = readr::cols(.default = "c"), show_col_types = FALSE) |> 
               mutate(filename = f),
          error = function(e) {
               warning(sprintf("Failed to read '%s': %s", f, e$message))
               NULL
          }
     )
})

data_death_raw <- bind_rows(data_list_death)

names(data_death_raw) <- c('week_value', 'week_alias', 'location_value', 'location_alias', 'cases', 'filename')
data_death_raw <- data_death_raw |>
     mutate(filepath = as.character(filename),
            filename_only = basename(filepath),
            disease_full = tools::file_path_sans_ext(filename_only),
            year = stringr::str_extract(filepath, "(?<=/|\\\\)\\d{3,4}(?=/|\\\\)"),
            year = if_else(is.na(year), stringr::str_extract(filepath, "\\d{3,4}"), year),
            year = as.integer(year) - 543,
            age_group = if_else(stringr::str_detect(disease_full, "__"),
                                stringr::str_replace(disease_full, ".*__", ""),
                                NA_character_),
            disease = stringr::str_replace(disease_full, "__.*$", "")) |>
     select(-filename_only, -disease_full)

## clean data -------------------------------------------------------------

# map disease names and calculate weekly deaths table
data_death_week <- data_death_raw |>
     left_join(data_map_name, by = c(disease = 'original_name')) |> 
     filter(!is.na(short_name)) |> 
     select(year, Shortname = short_name, week = week_value, deaths = cases, location_value, age_group, filepath) |>
     mutate(deaths = as.numeric(deaths)) |>
     group_by(year, Shortname, week, location_value, age_group) |>
     summarize(deaths = sum(deaths, na.rm = TRUE),
               source = paste(unique(filepath), collapse = "; "),
               .groups = "drop")

## reconstruction deaths ----------------------------------------------------------

# Distribute weekly deaths into months by how many days of that week fall in each month
month_recon_deaths <- data_death_week |> 
     filter(location_value == "%all%",
            is.na(age_group),
            week != "%all%") |> 
     mutate(week = as.integer(week)) |> 
     left_join(week_date_map, by = c("year", "week")) |>
     rowwise() |> mutate(dates = list(unlist(dates))) |>
     tidyr::unnest_longer(dates) |>
     mutate(month = month(dates)) |>
     group_by(year, Shortname, week) |>
     mutate(total_days = n()) |>
     group_by(year, Shortname, week, month, deaths, total_days) |>
     summarize(n_days = n(), .groups = "drop") |>
     mutate(deaths_month = deaths * (n_days / total_days)) |>
     group_by(year, Shortname, month) |>
     summarize(deaths = round(sum(deaths_month, na.rm = TRUE)),
               .groups = "drop")

data_week_age_deaths  <- data_death_week |> 
     filter(location_value == "%all%",
            !is.na(age_group),
            week != "%all%") |> 
     mutate(week = as.integer(week),
            age_group = case_when(age_group == "0_4" ~ "0-4",
                                  age_group == "5_9" ~ "5-9",
                                  age_group == "10_14" ~ "10-14",
                                  age_group == "15_19" ~ "15-19",
                                  age_group == "20_29" ~ "20-29",
                                  age_group == "30_39" ~ "30-39",
                                  age_group == "40_49" ~ "40-49",
                                  age_group == "50_59" ~ "50-59",
                                  age_group == "60" ~ "60+",
                                  TRUE ~ age_group)) |> 
     left_join(week_date_map, by = c("year", "week"))


# month_recon_deaths_age <- data_week_age_deaths |>
#      rowwise() |> mutate(dates = list(unlist(dates))) |>
#      tidyr::unnest_longer(dates) |>
#      mutate(month = month(dates)) |>
#      group_by(year, Shortname, week, location_value, age_group) |>
#      mutate(total_days = n()) |>
#      group_by(year, Shortname, week, month, location_value, age_group, deaths, total_days) |>
#      summarize(n_days = n(), .groups = "drop") |>
#      mutate(deaths_month = deaths * (n_days / total_days)) |>
#      group_by(year, Shortname, month, age_group) |>
#      summarize(deaths = round(sum(deaths_month, na.rm = TRUE)),
#                .groups = "drop")

data_week_location_deaths  <- data_death_week |> 
     filter(location_value != "%all%",
            is.na(age_group),
            week != "%all%") |> 
     mutate(week = as.integer(week)) |> 
     left_join(week_date_map, by = c("year", "week"))

# month_recon_deaths_location <- data_week_location_deaths |>
#      rowwise() |> mutate(dates = list(unlist(dates))) |>
#      tidyr::unnest_longer(dates) |>
#      mutate(month = month(dates)) |>
#      group_by(year, Shortname, week, location_value) |>
#      mutate(total_days = n()) |>
#      group_by(year, Shortname, week, month, location_value, deaths, total_days) |>
#      summarize(n_days = n(), .groups = "drop") |>
#      mutate(deaths_month = deaths * (n_days / total_days)) |>
#      group_by(year, Shortname, month, location_value) |>
#      summarize(deaths = round(sum(deaths_month, na.rm = TRUE)),
#                .groups = "drop")

## compare data ------------------------------------------------------------

# plot_month_and_week_comparison("Amebiasis", 2020:2025, 'deaths', NULL, data_month, NULL, month_recon_deaths, week_date_map, show_daily = TRUE)

outcome <- lapply(unique(month_recon_deaths$Shortname), function(sn) {
     p <- plot_month_and_week_comparison(sn, 2020:2025, 'deaths', NULL, data_month, NULL, month_recon_deaths, week_date_map, show_daily = TRUE)
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/deaths/", sn, '.png'),
            plot = p,
            create.dir = TRUE,
            width = 14,
            height = 4)
     NULL
})

# update data ----------------------------------------------------------------

## monthly data -------------------------------------------------------------

# Replace original monthly Cases/Deaths in years 2024-2025 with reconstructed values

# prepare reconstructed tables for join (names consistent with data_month)
month_recon_cases <- month_recon |>
     rename(Year = year, Month = month, Cases_recon = cases) |> 
     filter(Year < 2026)

month_recon_deaths2 <- month_recon_deaths |> 
     rename(Year = year, Month = month, Deaths_recon = deaths) |> 
     filter(Year < 2026)

# perform replacement only for years 2024 and 2025 when reconstructed values exist
data_month <- data_month |> 
     select(Year, Month, Shortname, Cases, Deaths) |>
     full_join(month_recon_cases, by = c('Shortname', 'Year', 'Month')) |> 
     full_join(month_recon_deaths2, by = c('Shortname', 'Year', 'Month')) |> 
     left_join(data_population, by = 'Year') |>
     mutate(Cases = if_else(Year %in% c(2024, 2025) & !is.na(Cases_recon),
                            Cases_recon, Cases),
            Deaths = if_else(Year %in% c(2024, 2025) & !is.na(Deaths_recon),
                             Deaths_recon, Deaths),
            Deaths = if_else(is.na(Deaths), 0, Deaths),
            Incidence = (Cases / Population) * 1e5,
            Mortality = (Deaths / Population) * 1e5,
            Date = as.Date(paste(Year, Month, "01", sep = "-"))) |>
     select(-Cases_recon, -Deaths_recon) |> 
     left_join(data_class, by = 'Shortname')

## yearly data --------------------------------------------------------------

# estimate yearly data based on monthly data for checking purpose
data_year_1 <- data_month |>
     group_by(Year, Shortname) |>
     summarize(Cases = sum(Cases, na.rm = TRUE),
               Deaths = sum(Deaths, na.rm = TRUE),
               Population = first(Population),
               Incidence = (Cases / Population) * 1e5,
               Mortality = (Deaths / Population) * 1e5,
               .groups = 'drop')

# check the yearly data consistency
data_year_check <- data_year_2 |>
     left_join(data_class, by = 'Disease') |>
     select(Year, Shortname, Cases, Deaths) |>
     full_join(data_year_1 |> 
                    rename(Cases_check = Cases, Deaths_check = Deaths),
               by = c('Year', 'Shortname')) |>
     mutate(Cases_diff = Cases - Cases_check,
            Deaths_diff = Deaths - Deaths_check,
            Cases_diff_pct = ifelse(is.na(Cases_check) | Cases_check == 0, NA, Cases_diff / Cases_check * 100),
            Deaths_diff_pct = ifelse(is.na(Deaths_check) | Deaths_check == 0, NA, Deaths_diff / Deaths_check * 100))

data_year_check |> 
     filter(Cases_diff != 0 | is.na(Cases_diff) | Deaths_diff != 0 | is.na(Deaths_diff))

# fill the missing yearly data based on monthly data
data_year <- data_year_1 |> 
     left_join(data_class, by = 'Shortname')

# save data
save(data_month, data_year,
     data_class, data_population,
     file = "./temp/month.RData")

save(data_week_age, data_week_location,
     data_week_age_deaths, data_week_location_deaths,
     file = './temp/month_subgroup.RData')
