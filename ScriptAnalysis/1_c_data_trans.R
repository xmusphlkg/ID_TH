
# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(openxlsx)
library(patchwork)

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

# subset data -------------------------------------------------------------

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1)|> 
     select(-c(Cases, Count, Including, Label))

data_map_name <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseName') |> 
     filter(!is.na(short_name))

data_map_location <- read.xlsx("../Data/TotalCasesDeaths.xlsx", sheet = 'DiseaseLocation')

# check all disease in data_week_raw are in data_class
unique(data_map_name$short_name)[!unique(data_map_name$short_name) %in% unique(data_class$Shortname)]

unique(data_class$Shortname)[!unique(data_class$Shortname) %in% unique(data_map_name$short_name)]

data_week <- data_week_raw |> 
     filter(location_value == "%all%",
            is.na(age_group),
            week_value != "%all%") |> 
     left_join(data_map_name, by = c(disease = 'original_name')) |> 
     filter(!is.na(short_name)) |> 
     select(year, Shortname = short_name, week = week_value, cases)

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
     select(year, Shortname = short_name, week = week_value, age_group, cases)

data_week_location <- data_week_raw |> 
     filter(location_value != "%all%",
            is.na(age_group),
            week_value != "%all%") |> 
     left_join(data_map_location, by = c(location_value = 'location_th')) |>
     select(year, disease, week = week_value, location, region, health_zone, cases)

save(data_week, data_week_age, data_week_location,
     file = "week.RData")

rm(list = ls())

# reconstruction ----------------------------------------------------------

# Spline-based temporal disaggregation (weekly -> daily -> monthly)
#
# Overview:
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
#
# Caveats:
# - Spline smoothing can overshoot around sharp weekly changes; negative
#   predictions are set to zero before scaling, which can bias within-week
#   shapes when totals are small. Inspect reconstructions for low-count diseases.
# - For applications sensitive to weekday structure or reporting delays, consider
#   alternative disaggregation approaches (e.g., proportional disaggregation
#   using historical daily patterns or covariate-informed models).

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


# create mapping from year-week to representative week midpoint date and list of dates per week
week_date_map <- data_date_seq |> 
     group_by(year, week) |> 
     summarize(week_start = min(date), week_mid = median(date), dates = list(date), .groups = "drop")

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
     
     tibble(date = daily_corrected$date, year = year_val, Shortname = unique(df_week$Shortname), daily = as.numeric(daily_corrected$daily))
}

# reconstruct all diseases and years
reconstruct_all <- function(data_week, week_map) {
     data_week <- data_week |> 
          mutate(week = as.integer(week), year = as.integer(year))
     groups <- data_week |> 
          group_by(Shortname, year) |> 
          group_split()
     
     out <- purrr::map_dfr(groups, ~{
          df <- .x
          tryCatch(reconstruct_one(df, week_map), error = function(e) {
               warning(sprintf("failed reconstruct %s %s: %s", unique(df$Shortname), unique(df$year), e$message))
               NULL
          })
     })
     out
}

daily_recon <- reconstruct_all(data_week, week_date_map)
# aggregate to month
daily_recon <- daily_recon |> 
     mutate(month = month(date), year = year(date))
month_recon <- daily_recon |> 
     group_by(Shortname, year, month) |> 
     summarize(cases = sum(daily, na.rm = TRUE), .groups = "drop")

save(daily_recon, month_recon, file = "reconstructed_daily_month.RData")

# compare dataset ---------------------------------------------------------

# Comparison and plotting utilities
#
# `compute_weekly_comparison()`
# - Inputs: `data_week` (original weekly totals), `daily_recon` (reconstructed daily),
#   `week_map` (mapping with `week_mid` dates).
# - Returns a list with two tibbles: `weekly` (row-level comparison per Shortname/year/week)
#   and `summary` (per Shortname-year statistics: mean, median, sd of differences,
#   percent exact matches, and number of weeks).
#
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
plot_month_and_week_comparison <- function(shortname, year_val, data_week, data_month, daily_recon, month_recon, week_map, show_daily = FALSE) {
     # browser()     
     
     # Monthly comparison
     orig_month <- data_month |> 
          filter(Shortname == shortname, Year %in% year_val) |> 
          select(year = Year, month = Month, Shortname, cases = Cases)
     
     recon_month <- month_recon |> 
          filter(Shortname == shortname, year %in% year_val) |> 
          rename(recon_cases = cases)
     
     month_cmp <- orig_month |> 
          full_join(recon_month, by = c("Shortname", "year", "month")) |> 
          mutate(month_mid = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
     
     p_month <- ggplot(month_cmp) +
          geom_line(aes(x = month_mid, y = recon_cases, color = "Reconstructed (based on weekly data)"), linewidth = 1) +
          geom_point(aes(x = month_mid, y = cases, color = "Observed (monthly data)"), size = 2) +
          geom_line(aes(x = month_mid, y = cases, color = "Observed (monthly data)"), linetype = "dashed") +
          scale_color_manual(name = "Series",
                             values = c("Observed (monthly data)" = "#4DBBD5FF",
                                        "Reconstructed (based on weekly data)" = "#B09C85FF")) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
          scale_x_date(expand = expansion(mult = c(0, 0)),
                       date_labels = "%Y",
                       date_breaks = "1 year") +
          labs(x = 'Date', y = "Cases") +
          theme_bw()+
          theme(legend.position = "inside",
                legend.box = "horizontal",
                legend.direction = "horizontal",
                legend.position.inside = c(0.5, 0.99),
                legend.justification = c(0.5, 1))
     
     
     # Weekly plot using existing function (returns ggplot)
     p_week <- plot_weekly_comparison(shortname, year_val, data_week, daily_recon, week_map, show_daily = show_daily)
     
     # combine with patchwork
     combined <- p_week + p_month + patchwork::plot_layout(ncol = 1)
     combined
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
     p <- plot_month_and_week_comparison(sn, 2020:2025, data_week, data_month, daily_recon, month_recon, week_date_map, show_daily = FALSE)
     ggsave(filename = paste0("comparison_", sn, ".png"),
     plot = p,
     width = 10,
     height = 8)
     NULL
})



