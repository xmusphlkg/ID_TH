library(tidyverse)
library(openxlsx)
library(lubridate)
library(forecast)
library(forecastHybrid)
library(bsts)

setwd("/home/mpi/ID_TH/ScriptAnalysis")

source("./function/theme_set.R")
source("./function/forecast.R")

load("./temp/month.RData")

appendix_tables_dir <- file.path("..", "Outcome", "Appendix", "Tables")
best_models <- read.xlsx(file.path(appendix_tables_dir, "Best_model_outcome.xlsx")) |>
  filter(Best == 1) |>
  transmute(
    Shortname = disease,
    Method = if_else(Method == "Hybrid**", "Hybrid", Method)
  )

get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) return(NA_real_)
  (year(end) - year(start)) * 12 + (month(end) - month(start))
}

calc_status_one <- function(dates,
                            observed,
                            expected,
                            start_date = as.Date("2020-01-01"),
                            recovery_threshold = 0.95,
                            persistence = 3) {
  keep <- dates >= start_date
  df <- tibble(
    date = dates[keep],
    observed = observed[keep],
    expected = expected[keep]
  ) |>
    arrange(date) |>
    mutate(
      diff = observed - expected,
      cum_diff = cumsum(diff)
    )

  if (nrow(df) == 0) {
    return(tibble(
      Status = "No Deficit",
      Date_Recovery = as.Date(NA),
      Date_Balance = as.Date(NA)
    ))
  }

  trough_idx <- which.min(df$cum_diff)
  trough_date <- df$date[trough_idx]
  max_deficit_raw <- df$cum_diff[trough_idx]

  first_drop_idx <- which(df$cum_diff < 0)[1]
  start_deficit_date <- if (!is.na(first_drop_idx)) df$date[first_drop_idx] else as.Date(NA)

  recovery_date <- as.Date(NA)
  status <- "No Deficit"

  if (!is.na(start_deficit_date) && max_deficit_raw < 0) {
    df_search <- df |>
      filter(date >= start_deficit_date)

    is_recovered_trend <- df_search$observed >= df_search$expected * recovery_threshold
    lag_base <- c(df_search$cum_diff[1], head(df_search$cum_diff, -1))
    is_paying_back <- (df_search$cum_diff - lag_base) >= 0

    robust_window <- rep(FALSE, nrow(df_search))
    if (nrow(df_search) >= persistence) {
      for (i in seq_len(nrow(df_search) - persistence + 1)) {
        robust_window[i] <- all((is_recovered_trend & is_paying_back)[i:(i + persistence - 1)])
      }
    }

    recovery_idx <- which(robust_window)[1]
    if (!is.na(recovery_idx)) {
      recovery_date <- df_search$date[recovery_idx]
      status <- "Recovered"
    } else {
      status <- "Suppressed"
    }
  }

  balance_date <- as.Date(NA)
  if (!is.na(trough_date) && max_deficit_raw < 0) {
    df_post_trough <- df |>
      filter(date > trough_date)
    balance_idx <- which(df_post_trough$cum_diff >= 0)[1]
    if (!is.na(balance_idx)) {
      balance_date <- df_post_trough$date[balance_idx]
      status <- "Debt Repaid"
    }
  }

  tibble(
    Status = status,
    Date_Recovery = recovery_date,
    Date_Balance = balance_date
  )
}

summarize_uncertainty <- function(dates, observed, median_expected, sim_matrix) {
  start_date <- as.Date("2020-01-01")

  primary <- calc_status_one(
    dates = dates,
    observed = observed,
    expected = median_expected,
    start_date = start_date
  )

  sim_rows <- lapply(seq_len(ncol(sim_matrix)), function(j) {
    calc_status_one(
      dates = dates,
      observed = observed,
      expected = sim_matrix[, j],
      start_date = start_date
    )
  }) |>
    bind_rows()

  rp_months <- purrr::map_dbl(
    sim_rows$Date_Recovery,
    ~ if (is.na(.x)) NA_real_ else get_months(start_date, .x)
  )
  bp_months <- purrr::map_dbl(
    sim_rows$Date_Balance,
    ~ if (is.na(.x)) NA_real_ else get_months(start_date, .x)
  )

  tibble(
    PrimaryStatus = primary$Status[1],
    Pr_RP = mean(sim_rows$Status %in% c("Recovered", "Debt Repaid")),
    Pr_BP = mean(sim_rows$Status == "Debt Repaid"),
    RP_Q025 = ifelse(any(!is.na(rp_months)), quantile(rp_months, 0.025, na.rm = TRUE), NA_real_),
    RP_Q975 = ifelse(any(!is.na(rp_months)), quantile(rp_months, 0.975, na.rm = TRUE), NA_real_),
    BP_Q025 = ifelse(any(!is.na(bp_months)), quantile(bp_months, 0.025, na.rm = TRUE), NA_real_),
    BP_Q975 = ifelse(any(!is.na(bp_months)), quantile(bp_months, 0.975, na.rm = TRUE), NA_real_)
  ) |>
    mutate(
      RP_Width = RP_Q975 - RP_Q025,
      BP_Width = BP_Q975 - BP_Q025
    )
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: Rscript transform_compare_one.R <Shortname> <Transform> <OutputCsv> [n_paths] [bsts_niter] [seed]")
}

shortname <- args[1]
transform_method <- args[2]
out_file <- args[3]
n_paths <- if (length(args) >= 4) as.integer(args[4]) else 1000L
bsts_niter <- if (length(args) >= 5) as.integer(args[5]) else 1000L
seed <- if (length(args) >= 6) as.integer(args[6]) else 20251209L

model_row <- best_models |>
  filter(Shortname == shortname)

if (nrow(model_row) != 1) {
  stop("Best model not found for ", shortname)
}

data_single <- data_month |>
  filter(Shortname == shortname) |>
  transmute(date = Date, value = Cases) |>
  arrange(date)

observed_post <- data_single |>
  filter(date >= split_dates[1])

ts_train <- data_single |>
  filter(date < split_dates[1]) |>
  pull(value) |>
  ts(
    frequency = 12,
    start = c(
      as.numeric(format(min(data_single$date), "%Y")),
      as.numeric(format(min(data_single$date), "%m"))
    )
  )

transform_lambda <- estimate_transform_lambda(
  ts_train,
  method = transform_method,
  offset = add_value
)

ts_train <- positive_forward_transform(
  ts_train,
  method = transform_method,
  offset = add_value,
  lambda = transform_lambda
)

res <- forecast_model_sim(
  ts_train = ts_train,
  h = nrow(observed_post),
  method = model_row$Method[1],
  hybrid_parallel = FALSE,
  hybrid_cores = 1,
  bsts_niter = bsts_niter,
  n_paths = n_paths,
  seed = seed,
  transform_method = transform_method
  ,
  transform_lambda = transform_lambda
)

summary_row <- summarize_uncertainty(
  dates = observed_post$date,
  observed = observed_post$value,
  median_expected = res$median,
  sim_matrix = res$MCMC
) |>
  mutate(
    Shortname = shortname,
    Method = model_row$Method[1],
    Transform = transform_method,
    TransformLambda = transform_lambda,
    n_paths = n_paths,
    bsts_niter = bsts_niter,
    Error = NA_character_
  ) |>
  select(
    Shortname, Method, Transform, TransformLambda, n_paths, bsts_niter, PrimaryStatus,
    Pr_RP, Pr_BP, RP_Q025, RP_Q975, RP_Width, BP_Q025, BP_Q975, BP_Width, Error
  )

dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
write_csv(summary_row, out_file)
print(summary_row)
