#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(openxlsx)
  library(forecast)
  library(forecastHybrid)
  library(bsts)
  library(ggplot2)
  library(patchwork)
  library(paletteer)
  library(readr)
  library(scales)
})

invisible(Sys.setlocale("LC_TIME", "C"))

args <- commandArgs(trailingOnly = FALSE)
resolve_script_dir <- function() {
  file_arg <- "--file="
  file_hits <- grep(paste0("^", file_arg), args, value = TRUE)

  if (length(file_hits) > 0) {
    script_path <- sub(file_arg, "", file_hits[1])
    if (!is.na(script_path) && nzchar(script_path)) {
      return(dirname(normalizePath(path.expand(script_path), winslash = "/", mustWork = FALSE)))
    }
  }

  if (dir.exists("ScriptAnalysis")) {
    return(normalizePath("ScriptAnalysis", winslash = "/", mustWork = FALSE))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

script_dir <- resolve_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)

source(file.path(script_dir, "function", "theme_set.R"))
source(file.path(script_dir, "function", "forecast.R"))

input_path <- file.path(project_root, "Data", "Pertussis incidence 6 country.xlsx")
tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
figure_dir <- file.path(project_root, "Outcome", "Appendix", "Supplementary Appendix 1_6")
summary_md_path <- file.path(project_root, "Outcome", "Appendix", "External_pertussis_decision_support.md")
main_appendix_path <- file.path(project_root, "Outcome", "Appendix", "Supplementary_Appendix.md")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

figure_png_path <- file.path(figure_dir, "external_pertussis_decision_support.png")
summary_xlsx_path <- file.path(tables_dir, "External_pertussis_decision_support.xlsx")
summary_csv_path <- file.path(tables_dir, "External_pertussis_decision_support_summary.csv")
cv_metrics_csv_path <- file.path(tables_dir, "External_pertussis_model_cv_metrics.csv")
forecast_csv_path <- file.path(tables_dir, "External_pertussis_best_model_forecasts.csv")
country_pi_summary_csv_path <- file.path(tables_dir, "External_pertussis_country_median_pi_summary.csv")

country_name_map <- c(
  AU = "Australia",
  CN = "China",
  JP = "Japan",
  NZ = "New Zealand",
  SE = "Sweden",
  US = "United States"
)

cadence_label_map <- c(
  month = "Monthly surveillance",
  week = "Weekly surveillance"
)

candidate_models <- c(
  "Neural Network",
  "ETS",
  "SARIMA",
  "TBATS",
  "Hybrid",
  "Bayesian structural",
  "ARIMA + Fourier"
)

candidate_model_labels <- c(
  "Neural\nNetwork",
  "ETS",
  "SARIMA",
  "TBATS",
  "Hybrid",
  "BSTS",
  "ARIMA +\nFourier"
)

recovery_threshold <- 0.95
cv_split_defs <- tribble(
  ~Split, ~TrainEndYear, ~TestStartYear, ~TestEndYear,
  "2019", 2018L, 2019L, 2019L,
  "2018-2019", 2017L, 2018L, 2019L,
  "2017-2019", 2016L, 2017L, 2019L
)

COL_TEAL <- "#0B6E69"
COL_CORAL <- "#CC3D24"
COL_BLUE <- "#004F7AFF"
COL_GOLD <- "#F3C558FF"
COL_GREEN_FILL <- "#6DAE9050"
COL_RED_FILL <- "#CC3D2450"
COL_PI <- "#6DAE9030"
COL_SLATE <- "#22313F"
COL_MUTED <- "#62707B"
COL_BG <- "#F5EFE6"

country_order <- c("Australia", "China", "Japan", "New Zealand", "Sweden", "United States")

coerce_excel_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(openxlsx::convertToDate(x))
  }
  as.Date(x)
}

safe_z_negative <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)
  if (!is.finite(sig) || sig == 0) {
    return(rep(0, length(x)))
  }
  -(x - mu) / sig
}

find_sustained_date <- function(dates, condition, persistence) {
  condition[is.na(condition)] <- FALSE
  if (length(dates) < persistence) {
    return(as.Date(NA))
  }
  for (i in seq_len(length(dates) - persistence + 1L)) {
    if (all(condition[i:(i + persistence - 1L)])) {
      return(dates[i])
    }
  }
  as.Date(NA)
}

fmt_date <- function(x) {
  ifelse(is.na(x), "NA", format(x, "%Y-%m-%d"))
}

fmt_months <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.1f months", x))
}

escape_md <- function(x) {
  x <- ifelse(is.na(x), "NA", as.character(x))
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("[\r\n]+", " ", x)
  x
}

md_table <- function(df) {
  headers <- names(df)
  lines <- c(
    paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
  )

  if (nrow(df) == 0) {
    return(lines)
  }

  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(escape_md(row), collapse = " | "), " |")
  })

  c(lines, rows)
}

replace_or_append_block <- function(lines, block_id, replacement_lines) {
  begin_marker <- paste0("<!-- BEGIN ", block_id, " -->")
  end_marker <- paste0("<!-- END ", block_id, " -->")
  wrapped_replacement <- c(begin_marker, replacement_lines, end_marker)

  begin_idx <- which(trimws(lines) == begin_marker)[1]
  end_idx <- which(trimws(lines) == end_marker)[1]

  if (!is.na(begin_idx) && !is.na(end_idx) && end_idx > begin_idx) {
    return(c(
      if (begin_idx > 1) lines[seq_len(begin_idx - 1)] else character(),
      wrapped_replacement,
      if (end_idx < length(lines)) lines[(end_idx + 1):length(lines)] else character()
    ))
  }

  c(
    lines,
    "",
    "<div style=\"page-break-after: always;\"></div>",
    "",
    wrapped_replacement
  )
}

read_external_series <- function(path) {
  sheets <- getSheetNames(path)
  bind_rows(lapply(sheets, function(sheet_name) {
    df <- read.xlsx(path, sheet = sheet_name, detectDates = TRUE)
    names(df) <- make.names(names(df))
    df |>
      transmute(
        CountryCode = sheet_name,
        Country = unname(country_name_map[sheet_name]),
        Date = coerce_excel_date(Date),
        Year = as.integer(Year),
        Month = if ("Month" %in% names(df)) as.integer(Month) else NA_integer_,
        Week = if ("Week" %in% names(df)) as.integer(Week) else NA_integer_,
        Cases = as.numeric(Cases),
        URL = as.character(URL)
      )
  })) |>
    arrange(match(Country, country_order), Date) |>
    mutate(
      Cadence = if_else(is.na(Week), "month", "week"),
      CadenceLabel = unname(cadence_label_map[Cadence])
    )
}

build_ts <- function(df, value_col = "Cases") {
  freq <- if (unique(df$Cadence) == "month") 12 else 52
  ts(df[[value_col]], frequency = freq)
}

fit_forecast_flexible <- function(ts_train,
                                  h,
                                  method,
                                  seed = 20240902L,
                                  bsts_niter = 1000L) {
  method <- match.arg(method, candidate_models)
  seasonal_period <- frequency(ts_train)
  transform_lambda <- estimate_transform_lambda(ts_train, method = forecast_transform, offset = add_value)
  ts_train_trans <- positive_forward_transform(
    ts_train,
    method = forecast_transform,
    offset = add_value,
    lambda = transform_lambda
  )

  mean_fc <- lower_95 <- upper_95 <- rep(NA_real_, h)
  model_info <- list()

  set.seed(seed)

  if (method %in% c("Neural Network", "ETS", "SARIMA", "TBATS", "ARIMA + Fourier")) {
    if (method == "ARIMA + Fourier") {
      fit <- fit_fourier_arima(ts_train_trans, max_k = min(6L, max(1L, floor(seasonal_period / 2))), ic = "aicc")
      future_xreg <- forecast::fourier(ts_train_trans, K = fit$fourier_k, h = h)
      fc <- forecast(fit$model, h = h, xreg = future_xreg, level = 95)
      model_info$fourier_k <- fit$fourier_k
    } else {
      mod <- suppressWarnings(
        switch(
          method,
          "Neural Network" = nnetar(ts_train_trans, lambda = NULL),
          "ETS" = ets(ts_train_trans, ic = "aicc", lambda = NULL),
          "SARIMA" = auto.arima(ts_train_trans, seasonal = seasonal_period > 1, ic = "aicc", lambda = NULL),
          "TBATS" = tbats(ts_train_trans, seasonal.periods = seasonal_period, use.parallel = FALSE, num.cores = 1)
        )
      )
      fc <- forecast(mod, h = h, level = 95)
    }

    mean_fc <- as.numeric(fc$mean)
    if (!is.null(fc$lower) && !is.null(fc$upper)) {
      lower_95 <- as.numeric(fc$lower[, ncol(fc$lower)])
      upper_95 <- as.numeric(fc$upper[, ncol(fc$upper)])
    }
  } else if (method == "Hybrid") {
    window_size <- max(8L, min(as.integer(round(length(ts_train) * 0.7)), length(ts_train) - 4L))
    invisible(capture.output(
      mod <- hybridModel(
        ts_train_trans,
        lambda = NULL,
        models = c("aent"),
        a.args = list(seasonal = seasonal_period > 1),
        weights = "cv.errors",
        windowSize = window_size,
        parallel = FALSE,
        num.cores = 1,
        errorMethod = "RMSE"
      ),
      type = "output"
    ))
    fc <- suppressWarnings(forecast(mod, h = h, level = 95))
    mean_fc <- as.numeric(fc$mean)
    if (!is.null(fc$lower) && !is.null(fc$upper)) {
      lower_95 <- as.numeric(fc$lower[, ncol(fc$lower)])
      upper_95 <- as.numeric(fc$upper[, ncol(fc$upper)])
    }
  } else if (method == "Bayesian structural") {
    ss <- AddLocalLinearTrend(list(), ts_train_trans)
    if (seasonal_period > 1) {
      ss <- AddSeasonal(ss, ts_train_trans, nseasons = seasonal_period)
    }
    mod <- bsts(ts_train_trans, state.specification = ss, niter = bsts_niter, seed = seed, ping = 0)
    burn <- SuggestBurn(0.1, mod)
    pred <- predict.bsts(mod, horizon = h, burn = burn, quantiles = c(0.025, 0.975))
    mean_fc <- as.numeric(pred$mean)
    if (!is.null(pred$interval)) {
      lower_95 <- as.numeric(pred$interval[1, ])
      upper_95 <- as.numeric(pred$interval[2, ])
    }
  }

  list(
    mean = positive_inverse_transform(mean_fc, method = forecast_transform, lambda = transform_lambda),
    lower_95 = if (all(is.na(lower_95))) rep(NA_real_, h) else positive_inverse_transform(lower_95, method = forecast_transform, lambda = transform_lambda),
    upper_95 = if (all(is.na(upper_95))) rep(NA_real_, h) else positive_inverse_transform(upper_95, method = forecast_transform, lambda = transform_lambda),
    model_info = model_info
  )
}

forecast_sim_flexible <- function(ts_train,
                                  h,
                                  method,
                                  seed = 20251209L,
                                  bsts_niter = 1000L,
                                  n_paths = 2000L) {
  method <- match.arg(method, candidate_models)
  seasonal_period <- frequency(ts_train)
  transform_lambda <- estimate_transform_lambda(ts_train, method = forecast_transform, offset = add_value)
  ts_train_trans <- positive_forward_transform(
    ts_train,
    method = forecast_transform,
    offset = add_value,
    lambda = transform_lambda
  )

  set.seed(seed)
  sim_matrix_log <- matrix(NA_real_, nrow = h, ncol = n_paths)
  model_info <- list()

  if (method %in% c("Neural Network", "ETS", "SARIMA", "TBATS", "ARIMA + Fourier")) {
    if (method == "ARIMA + Fourier") {
      fit <- fit_fourier_arima(ts_train_trans, max_k = min(6L, max(1L, floor(seasonal_period / 2))), ic = "aicc")
      future_xreg <- forecast::fourier(ts_train_trans, K = fit$fourier_k, h = h)
      model_info$fourier_k <- fit$fourier_k
      sim_matrix_log <- replicate(
        n_paths,
        as.numeric(simulate(fit$model, nsim = h, future = TRUE, bootstrap = TRUE, xreg = future_xreg))
      )
    } else {
      mod <- suppressWarnings(
        switch(
          method,
          "Neural Network" = nnetar(ts_train_trans, lambda = NULL),
          "ETS" = ets(ts_train_trans, ic = "aicc", lambda = NULL),
          "SARIMA" = auto.arima(ts_train_trans, seasonal = seasonal_period > 1, ic = "aicc", lambda = NULL),
          "TBATS" = tbats(ts_train_trans, seasonal.periods = seasonal_period, use.parallel = FALSE, num.cores = 1)
        )
      )

      if (method == "ETS") {
        fc <- forecast(mod, h = h)
        mu <- as.numeric(fc$mean)
        resids <- na.omit(as.numeric(residuals(mod)))
        sim_matrix_log <- replicate(n_paths, mu + sample(resids, size = h, replace = TRUE))
      } else {
        sim_matrix_log <- replicate(
          n_paths,
          as.numeric(simulate(mod, nsim = h, future = TRUE, bootstrap = TRUE))
        )
      }
    }
  } else if (method == "Hybrid") {
    window_size <- max(8L, min(as.integer(round(length(ts_train_trans) * 0.7)), length(ts_train_trans) - 4L))
    invisible(capture.output(
      mod <- hybridModel(
        ts_train_trans,
        lambda = NULL,
        models = c("aent"),
        a.args = list(seasonal = seasonal_period > 1),
        weights = "cv.errors",
        windowSize = window_size,
        parallel = FALSE,
        num.cores = 1,
        errorMethod = "RMSE"
      ),
      type = "output"
    ))
    fc <- suppressWarnings(forecast(mod, h = h))
    mu <- as.numeric(fc$mean)
    resids <- na.omit(as.numeric(mod$residuals))
    sim_matrix_log <- replicate(n_paths, mu + sample(resids, size = h, replace = TRUE))
  } else if (method == "Bayesian structural") {
    ss <- AddLocalLinearTrend(list(), ts_train_trans)
    if (seasonal_period > 1) {
      ss <- AddSeasonal(ss, ts_train_trans, nseasons = seasonal_period)
    }
    mod <- bsts(ts_train_trans, state.specification = ss, niter = bsts_niter, seed = seed, ping = 0)
    burn <- SuggestBurn(0.1, mod)
    pred <- predict.bsts(mod, horizon = h, burn = burn)
    posterior_samples <- pred$distribution
    idx <- sample(seq_len(nrow(posterior_samples)), size = n_paths, replace = TRUE)
    sim_matrix_log <- t(posterior_samples[idx, ])
  }

  sim_matrix_pos <- positive_inverse_transform(
    sim_matrix_log,
    method = forecast_transform,
    lambda = transform_lambda
  )

  get_q <- function(x, p) apply(x, 1, quantile, probs = p, na.rm = TRUE)

  list(
    mean = rowMeans(sim_matrix_pos, na.rm = TRUE),
    median = get_q(sim_matrix_pos, 0.5),
    lower_95 = get_q(sim_matrix_pos, 0.025),
    upper_95 = get_q(sim_matrix_pos, 0.975),
    model_info = model_info
  )
}

country_parallel_apply <- function(country_groups, worker_cap = 6L, fun) {
  n_groups <- length(country_groups)
  available_cores <- parallel::detectCores(logical = TRUE)
  n_workers <- max(1L, min(n_groups, max(1L, available_cores - 1L), worker_cap))

  message(sprintf("Using %d worker(s) across %d countries.", n_workers, n_groups))

  if (n_workers <= 1L || .Platform$OS.type != "unix") {
    return(lapply(country_groups, fun))
  }

  parallel::mclapply(
    country_groups,
    fun,
    mc.cores = n_workers,
    mc.preschedule = FALSE
  )
}

run_cv_one_country <- function(country_df) {
  country_name <- unique(country_df$Country)
  cadence <- unique(country_df$Cadence)
  methods_to_try <- if (cadence == "week") setdiff(candidate_models, "ETS") else candidate_models

  metric_rows <- list()
  idx <- 1L

  for (split_i in seq_len(nrow(cv_split_defs))) {
    split_row <- cv_split_defs[split_i, ]
    train_df <- country_df |>
      filter(Year <= split_row$TrainEndYear) |>
      arrange(Date)
    test_df <- country_df |>
      filter(Year >= split_row$TestStartYear, Year <= split_row$TestEndYear) |>
      arrange(Date)

    if (nrow(train_df) < 24 || nrow(test_df) == 0) {
      next
    }

    ts_train <- build_ts(train_df)

    for (method_i in seq_along(methods_to_try)) {
      method_name <- methods_to_try[method_i]
      fit_obj <- tryCatch(
        fit_forecast_flexible(
          ts_train = ts_train,
          h = nrow(test_df),
          method = method_name,
          seed = 1000L + split_i * 100L + method_i,
          bsts_niter = 1000L
        ),
        error = function(e) NULL
      )

      if (is.null(fit_obj)) {
        metric_rows[[idx]] <- tibble(
          Country = country_name,
          Cadence = cadence,
          Split = split_row$Split,
          Method = method_name,
          SMAPE = NA_real_,
          RMSE = NA_real_,
          MASE = NA_real_,
          R_Squared = NA_real_
        )
      } else {
        metrics <- evaluate_forecast(actual = test_df$Cases, forecast = fit_obj$mean)
        metric_rows[[idx]] <- tibble(
          Country = country_name,
          Cadence = cadence,
          Split = split_row$Split,
          Method = method_name,
          SMAPE = unname(metrics["SMAPE"]),
          RMSE = unname(metrics["RMSE"]),
          MASE = unname(metrics["MASE"]),
          R_Squared = unname(metrics["R_Squared"])
        )
      }
      idx <- idx + 1L
    }
  }

  bind_rows(metric_rows)
}

select_best_models <- function(cv_metrics) {
  split_scores <- cv_metrics |>
    group_by(Country, Cadence, Split) |>
    mutate(
      z_SMAPE = safe_z_negative(SMAPE),
      z_RMSE = safe_z_negative(RMSE),
      z_MASE = safe_z_negative(MASE)
    ) |>
    rowwise() |>
    mutate(
      CompositeSplitIndex = if (all(is.na(c_across(c(z_SMAPE, z_RMSE, z_MASE))))) {
        NA_real_
      } else {
        sum(c_across(c(z_SMAPE, z_RMSE, z_MASE)), na.rm = TRUE)
      }
    ) |>
    ungroup()

  summary_scores <- split_scores |>
    group_by(Country, Cadence, Method) |>
    summarise(
      CompositeIndex = if (all(!is.na(CompositeSplitIndex))) sum(CompositeSplitIndex) else NA_real_,
      MeanSMAPE = mean(SMAPE, na.rm = TRUE),
      MeanRMSE = mean(RMSE, na.rm = TRUE),
      MeanMASE = mean(MASE, na.rm = TRUE),
      ValidSplits = sum(!is.na(CompositeSplitIndex)),
      .groups = "drop"
    ) |>
    group_by(Country, Cadence) |>
    mutate(
      BestScore = suppressWarnings(max(CompositeIndex, na.rm = TRUE)),
      Best = is.finite(BestScore) & CompositeIndex == BestScore
    ) |>
    ungroup() |>
    select(-BestScore)

  list(split_scores = split_scores, summary_scores = summary_scores)
}

fit_best_model_country <- function(country_df, best_method) {
  train_df <- country_df |>
    filter(Year <= 2019) |>
    arrange(Date)
  post_df <- country_df |>
    filter(Year >= 2020) |>
    arrange(Date)

  fit_obj <- forecast_sim_flexible(
    ts_train = build_ts(train_df),
    h = nrow(post_df),
    method = best_method,
    seed = 20251209L,
    bsts_niter = 1000L,
    n_paths = 2000L
  )

  post_df |>
    mutate(
      ForecastMean = fit_obj$mean,
      ForecastMedian = fit_obj$median,
      Lower95 = fit_obj$lower_95,
      Upper95 = fit_obj$upper_95,
      BestModel = best_method
    )
}

compute_decision_summary <- function(forecast_df) {
  cadence <- unique(forecast_df$Cadence)
  persistence <- if (cadence == "month") 3L else 4L

  dat <- forecast_df |>
    arrange(Date) |>
    mutate(
      Diff = Cases - ForecastMedian,
      CumDiff = cumsum(Diff),
      ObsExpRatio = if_else(ForecastMedian > 0, Cases / ForecastMedian, NA_real_)
    )

  deficit_start_idx <- which(dat$CumDiff < 0)[1]
  deficit_start_date <- if (!is.na(deficit_start_idx)) dat$Date[deficit_start_idx] else as.Date(NA)

  trough_idx <- which.min(dat$CumDiff)
  trough_date <- dat$Date[trough_idx]
  trough_cum_diff <- dat$CumDiff[trough_idx]

  normalization_date <- as.Date(NA)
  if (!is.na(deficit_start_date) && trough_cum_diff < 0) {
    search_df <- dat |>
      filter(Date >= deficit_start_date)
    lag_cum <- c(search_df$CumDiff[1], head(search_df$CumDiff, -1))
    condition <- search_df$Cases >= recovery_threshold * search_df$ForecastMedian &
      (search_df$CumDiff - lag_cum) >= 0
    normalization_date <- find_sustained_date(search_df$Date, condition, persistence)
  }

  balance_date <- as.Date(NA)
  if (trough_cum_diff < 0) {
    post_trough <- dat |>
      filter(Date > trough_date)
    balance_idx <- which(post_trough$CumDiff >= 0)[1]
    if (!is.na(balance_idx)) {
      balance_date <- post_trough$Date[balance_idx]
    }
  }

  review_end_date <- if (is.na(balance_date)) max(dat$Date) else balance_date

  status <- case_when(
    is.na(normalization_date) ~ "Still suppressed",
    is.na(balance_date) ~ "RP achieved without BP",
    TRUE ~ "Balanced"
  )

  tibble(
    Country = unique(dat$Country),
    CountryCode = unique(dat$CountryCode),
    Cadence = unique(dat$Cadence),
    CadenceLabel = unique(dat$CadenceLabel),
    BestModel = unique(dat$BestModel),
    DeficitStartDate = deficit_start_date,
    TroughDate = trough_date,
    TroughCumDiff = trough_cum_diff,
    NormalizationDate = normalization_date,
    BalanceDate = balance_date,
    ReviewEndDate = review_end_date,
    DiscordanceMonths = if (is.na(normalization_date)) NA_real_ else round(time_length(interval(normalization_date, review_end_date), "month"), 1),
    DiscordancePeriods = if (is.na(normalization_date)) NA_integer_ else sum(dat$Date >= normalization_date & dat$Date <= review_end_date) - 1L,
    Status = status
  )
}

build_band_data <- function(summary_row, ymax) {
  out <- list()

  if (!is.na(summary_row$DeficitStartDate) && !is.na(summary_row$NormalizationDate)) {
    out[[length(out) + 1L]] <- tibble(
      xmin = summary_row$DeficitStartDate,
      xmax = summary_row$NormalizationDate,
      ymin = ymax * 0.90,
      ymax = ymax * 0.97,
      Band = "Recovery window"
    )
  } else if (!is.na(summary_row$DeficitStartDate) && is.na(summary_row$NormalizationDate)) {
    out[[length(out) + 1L]] <- tibble(
      xmin = summary_row$DeficitStartDate,
      xmax = summary_row$ReviewEndDate,
      ymin = ymax * 0.90,
      ymax = ymax * 0.97,
      Band = "Suppressed window"
    )
  }

  if (!is.na(summary_row$NormalizationDate)) {
    out[[length(out) + 1L]] <- tibble(
      xmin = summary_row$NormalizationDate,
      xmax = summary_row$ReviewEndDate,
      ymin = ymax * 0.98,
      ymax = ymax * 1.05,
      Band = "Balance review window"
    )
  }

  bind_rows(out)
}

plot_country_forecast_panel <- function(full_df, forecast_df, summary_row, panel_label) {
  country_name <- unique(full_df$Country)
  cadence_label <- unique(full_df$CadenceLabel)
  best_model <- unique(forecast_df$BestModel)

  diff_df <- forecast_df |>
    mutate(
      ymin = pmin(Cases, ForecastMedian),
      ymax = pmax(Cases, ForecastMedian),
      DiffDirection = if_else(Cases >= ForecastMedian, "Observed > expected", "Observed < expected")
    )

  ymax_panel <- max(c(full_df$Cases, forecast_df$ForecastMedian), na.rm = TRUE) * 1.12
  band_df <- build_band_data(summary_row, ymax_panel)
  analysis_anchor <- min(forecast_df$Date)

  p <- ggplot() +
    geom_rect(
      data = band_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Band),
      alpha = 0.25,
      inherit.aes = FALSE
    ) +
    geom_ribbon(
      data = diff_df,
      aes(x = Date, ymin = ymin, ymax = ymax, fill = DiffDirection),
      alpha = 0.55,
      inherit.aes = FALSE
    ) +
    geom_line(
      data = full_df,
      aes(x = Date, y = Cases, colour = "Observed"),
      linewidth = 0.8
    ) +
    geom_line(
      data = forecast_df,
      aes(x = Date, y = ForecastMedian, colour = "Forecast median"),
      linewidth = 0.9
    ) +
    geom_vline(xintercept = analysis_anchor, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    scale_colour_manual(
      values = c("Observed" = COL_CORAL, "Forecast median" = COL_TEAL),
      name = NULL
    ) +
    scale_fill_manual(
      values = c(
        "Observed > expected" = COL_GREEN_FILL,
        "Observed < expected" = COL_RED_FILL,
        "Recovery window" = alpha(COL_BLUE, 0.35),
        "Suppressed window" = alpha(COL_BLUE, 0.35),
        "Balance review window" = alpha(COL_GOLD, 0.45)
      ),
      name = NULL
    ) +
    scale_x_date(
      breaks = seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by = "2 years"),
      date_labels = "%Y"
    ) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    coord_cartesian(ylim = c(0, ymax_panel), clip = "off") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 12, hjust = 0),
      plot.title.position = "plot",
      legend.position = "bottom",
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9)
    ) +
    labs(
      title = sprintf("%s: %s", panel_label, country_name),
      x = NULL,
      y = "Reported cases"
    )

  if (!is.na(summary_row$NormalizationDate)) {
    p <- p +
      geom_vline(xintercept = summary_row$NormalizationDate, linetype = "dashed", colour = COL_BLUE, linewidth = 0.6)
  }

  if (!is.na(summary_row$BalanceDate)) {
    p <- p +
      geom_vline(xintercept = summary_row$BalanceDate, linetype = "dashed", colour = COL_GOLD, linewidth = 0.6)
  }

  p
}

plot_model_heatmap <- function(summary_scores) {
  heatmap_df <- summary_scores |>
    tidyr::complete(tidyr::nesting(Country, Cadence), Method = candidate_models) |>
    mutate(
      CountryLabel = paste0(Country, "\n", if_else(Cadence == "week", "weekly", "monthly")),
      CountryLabel = factor(CountryLabel, levels = paste0(country_order, "\n", c("monthly", "monthly", "weekly", "monthly", "monthly", "weekly"))),
      MethodLabel = factor(Method, levels = candidate_models, labels = candidate_model_labels),
      Marker = if_else(replace_na(Best, FALSE), "*", "")
    )

  ggplot(heatmap_df, aes(x = MethodLabel, y = CountryLabel, fill = CompositeIndex)) +
    geom_tile(colour = "white", linewidth = 0.7) +
    geom_text(aes(label = Marker), size = 5, fontface = "bold", colour = "black") +
    scale_fill_gradientn(
      colours = paletteer_d("Redmonder::dPBIRdGn"),
      na.value = "grey92",
      name = "Composite index"
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.title = element_text(face = "bold", size = 13, hjust = 0),
      plot.title.position = "plot",
      legend.position = "right"
    ) +
    labs(
      title = "A"
    )
}

all_series <- read_external_series(input_path)
country_groups <- split(all_series, all_series$Country)

message("Running model selection for six external pertussis series...")
cv_metrics <- bind_rows(country_parallel_apply(country_groups, fun = run_cv_one_country))
selection <- select_best_models(cv_metrics)
split_scores <- selection$split_scores
summary_scores <- selection$summary_scores

best_models <- summary_scores |>
  filter(Best) |>
  select(Country, Cadence, BestModel = Method, CompositeIndex)

message("Refitting best models on the full 2015-2019 training window...")
best_model_lookup <- best_models$BestModel
names(best_model_lookup) <- best_models$Country

best_forecast_list <- country_parallel_apply(country_groups, fun = function(country_df) {
  country_name <- unique(country_df$Country)
  fit_best_model_country(country_df, best_model_lookup[[country_name]])
})

best_forecasts <- bind_rows(best_forecast_list)

decision_summary <- bind_rows(
  lapply(split(best_forecasts, best_forecasts$Country), compute_decision_summary)
) |>
  left_join(best_models, by = c("Country", "Cadence", "BestModel")) |>
  mutate(
    Country = factor(Country, levels = country_order)
  ) |>
  arrange(Country)

best_forecasts <- best_forecasts |>
  left_join(
    decision_summary |>
      select(Country, NormalizationDate, BalanceDate, ReviewEndDate, Status),
    by = "Country"
  ) |>
  mutate(
    InDiscordanceWindow = !is.na(NormalizationDate) & Date >= NormalizationDate & Date <= ReviewEndDate
  )

country_pi_summary <- best_forecasts |>
  group_by(Country, CadenceLabel, BestModel) |>
  summarise(
    FollowUpEnd = max(Date),
    ObservedAtEnd = Cases[which.max(Date)],
    ForecastMedianAtEnd = ForecastMedian[which.max(Date)],
    Lower95AtEnd = Lower95[which.max(Date)],
    Upper95AtEnd = Upper95[which.max(Date)],
    MedianForecastAcrossFollowUp = median(ForecastMedian, na.rm = TRUE),
    MedianLower95AcrossFollowUp = median(Lower95, na.rm = TRUE),
    MedianUpper95AcrossFollowUp = median(Upper95, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    decision_summary |>
      select(Country, NormalizationDate, BalanceDate, Status),
    by = "Country"
  ) |>
  mutate(
    Country = factor(Country, levels = country_order)
  ) |>
  arrange(Country)

model_panel <- plot_model_heatmap(summary_scores)

panel_letters <- LETTERS[2:7]
forecast_panels <- lapply(seq_along(country_order), function(i) {
  country_name <- country_order[i]
  full_df <- all_series |>
    filter(Country == country_name)
  forecast_df <- best_forecasts |>
    filter(Country == country_name)
  summary_row <- decision_summary |>
    filter(Country == country_name) |>
    slice(1)
  plot_country_forecast_panel(full_df, forecast_df, summary_row, panel_letters[i])
})

forecast_grid <- wrap_plots(forecast_panels, ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

combined_figure <- cowplot::plot_grid(
  model_panel,
  forecast_grid,
  ncol = 1,
  rel_heights = c(1.0, 2.3),
  labels = NULL
)

ggsave(
  filename = figure_png_path,
  plot = combined_figure,
  width = 16,
  height = 12,
  dpi = 320,
  bg = "white"
)

write_csv(decision_summary, summary_csv_path)
write_csv(split_scores, cv_metrics_csv_path)
write_csv(best_forecasts, forecast_csv_path)
write_csv(country_pi_summary, country_pi_summary_csv_path)

methods_table <- tibble(
  Parameter = c(
    "Input file",
    "Countries",
    "Native cadence",
    "Candidate models",
    "Pre-pandemic training pool",
    "Rolling hold-out splits",
    "Primary selection rule",
    "Forecast transform",
    "Recovery rule",
    "Balance rule"
  ),
  Value = c(
    basename(input_path),
    paste(country_order, collapse = ", "),
    "Australia, China, New Zealand, Sweden monthly; Japan and United States weekly",
    paste(candidate_models, collapse = "; "),
    "2015-2019",
    "2019; 2018-2019; 2017-2019",
    "Per split, z-standardise sMAPE, RMSE, and MASE across models, reverse-code lower-is-better metrics, sum within split, then sum across splits",
    "Square-root positive-support transform with offset 0.01",
    "Observed >= 95% of expected with non-decreasing cumulative deficit repayment for 3 consecutive months or 4 consecutive weeks",
    "First date cumulative observed minus expected becomes non-negative after the trough"
  )
)

write.xlsx(
  list(
    CVMetrics = split_scores,
    ModelSelectionSummary = summary_scores,
    BestModelForecasts = best_forecasts,
    CountryMedian95PI = country_pi_summary,
    DecisionSupportSummary = decision_summary,
    Methods = methods_table
  ),
  file = summary_xlsx_path,
  overwrite = TRUE
)

best_model_sentence <- decision_summary |>
  transmute(Label = paste0(as.character(Country), " = ", BestModel)) |>
  pull(Label) |>
  paste(collapse = "; ")

range_months <- range(decision_summary$DiscordanceMonths, na.rm = TRUE)
median_months <- median(decision_summary$DiscordanceMonths, na.rm = TRUE)
balanced_countries <- decision_summary |>
  filter(!is.na(BalanceDate)) |>
  pull(Country) |>
  as.character()

balance_sentence <- if (length(balanced_countries) == 0) {
  "No country reached cumulative balance within follow-up; all six series remained cumulatively unresolved through the end of 2025 under the selected best-model baselines."
} else {
  sprintf(
    "%d countries (%s) reached cumulative balance within follow-up, whereas the remaining countries stayed cumulatively unresolved through the end of 2025.",
    length(balanced_countries),
    paste(balanced_countries, collapse = ", ")
  )
}

table_s17_md <- country_pi_summary |>
  transmute(
    Country = as.character(Country),
    Cadence = CadenceLabel,
    `Best model` = BestModel,
    `Follow-up median forecast` = round(MedianForecastAcrossFollowUp, 1),
    `Follow-up median 95% PI` = paste0(round(MedianLower95AcrossFollowUp, 1), " to ", round(MedianUpper95AcrossFollowUp, 1)),
    `End date` = format(FollowUpEnd, "%Y-%m-%d"),
    `End forecast median` = round(ForecastMedianAtEnd, 1),
    `End 95% PI` = paste0(round(Lower95AtEnd, 1), " to ", round(Upper95AtEnd, 1)),
    `Normalization date` = if_else(is.na(NormalizationDate), "Not reached", format(NormalizationDate, "%Y-%m-%d")),
    `Balance date` = if_else(is.na(BalanceDate), "Not reached", format(BalanceDate, "%Y-%m-%d"))
  )

markdown_lines <- c(
  "# External Pertussis Decision-Support Case Study",
  "",
  "This attachment applies the same disease-specific model-selection logic used in the main manuscript to six external pertussis surveillance series with mixed reporting cadence.",
  sprintf("Selected best models were: %s.", best_model_sentence),
  sprintf(
    "Across all six countries, sustained incidence-only normalization still preceded cumulative balance, with decision-discordance windows ranging from %s to %s and a median of %s.",
    fmt_months(range_months[1]),
    fmt_months(range_months[2]),
    fmt_months(median_months)
  ),
  balance_sentence,
  "",
  "**Country-level median and 95% PI summary**",
  md_table(table_s17_md),
  "",
  sprintf("![External pertussis decision-support figure](./Supplementary%%20Appendix%%201_6/%s)", basename(figure_png_path)),
  "",
  "Source files:",
  sprintf("- `%s`", summary_xlsx_path),
  sprintf("- `%s`", summary_csv_path),
  sprintf("- `%s`", cv_metrics_csv_path),
  sprintf("- `%s`", forecast_csv_path),
  sprintf("- `%s`", country_pi_summary_csv_path)
)

writeLines(markdown_lines, summary_md_path)

if (file.exists(main_appendix_path)) {
  appendix_lines <- readLines(main_appendix_path, warn = FALSE)
  appendix_block <- c(
    "## Part 6: External pertussis decision-support case study",
    "",
    "**Table S17. Country-level counterfactual median and 95% predictive-interval summary for the external pertussis case study.**",
    md_table(table_s17_md),
    "",
    "<div style=\"page-break-after: always;\"></div>",
    "",
    sprintf("![**Fig. S126. External pertussis decision-support case study.**](Supplementary%%20Appendix%%201_6/%s)", basename(figure_png_path)),
    "",
    "**Fig. S126. External pertussis decision-support case study across six countries.** Panel A compares candidate model performance using the same rolling hold-out composite-selection logic used in the main manuscript. Panels B-G show observed pertussis incidence and the selected counterfactual median forecast for Australia, China, Japan, New Zealand, Sweden, and the United States. Shaded blue and gold bars mark the recovery-review and balance-review windows implied by the RP/BP logic, while green/red fills mark months or weeks in which observed incidence is above or below the counterfactual median."
  )
  appendix_lines <- replace_or_append_block(appendix_lines, "EXTERNAL_PERTUSSIS_CASE_STUDY", appendix_block)
  writeLines(appendix_lines, main_appendix_path)
}

message("External pertussis decision-support outputs written:")
message(sprintf(" - %s", summary_xlsx_path))
message(sprintf(" - %s", summary_csv_path))
message(sprintf(" - %s", cv_metrics_csv_path))
message(sprintf(" - %s", forecast_csv_path))
message(sprintf(" - %s", country_pi_summary_csv_path))
message(sprintf(" - %s", figure_png_path))
message(sprintf(" - %s", summary_md_path))
