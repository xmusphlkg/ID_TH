
# Evaluation ------------------------------------------------------------------
# actual: actual values (numeric vector)
# forecast: forecast values (numeric vector)
evaluate_forecast <- function(actual, forecast) {
     # find na values
     na_value <- is.na(actual) | is.na(forecast)
     
     # drop na values
     actual <- actual[!na_value]
     forecast <- forecast[!na_value]
     
     smape <- mean(200 * abs(actual - forecast) / (abs(actual) + abs(forecast)))
     rmse <- sqrt(mean((actual - forecast) ^ 2))
     mase <- mean(abs(actual - forecast)) / mean(abs(diff(actual)))
     
     correlation <- cor(actual, forecast)
     r_squared <- correlation^2
     
     return(c('SMAPE' = smape, 'RMSE' = rmse, 'MASE' = mase, 'R_Squared' = r_squared))
}


#' Forecast single-run model
#'
#' Fits a time series model and returns point forecasts and prediction
#' intervals. This function performs a single deterministic forecast (no
#' Monte Carlo simulation). Input `ts_train` is assumed to be log-transformed
#' when required; returned values are back-transformed to the original scale.
#'
#' @param ts_train Time series object (assumed to be log-transformed).
#' @param h Forecast horizon (integer).
#' @param method One of: "Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural".
#' @param hybrid_parallel Logical; passed to `hybridModel` when `method == "Hybrid"`.
#' @param hybrid_cores Integer; number of cores for `hybridModel`.
#' @param bsts_niter Integer; number of iterations for `bsts` when using the Bayesian structural model.
#' @param seed Integer; random seed for reproducibility.
#'
#' @return A named list with elements:
#' - `mean`: point forecast (original scale)
#' - `lower_95`, `lower_80`, `upper_80`, `upper_95`: prediction intervals (original scale)
#'
#' @details Internally this function groups models into three categories:
#' - Group A (standard models): uses `forecast()` on models supported by the `forecast` package.
#' - Group B (hybrid): fits using `hybridModel()` and extracts forecast and intervals.
#' - Group C (Bayesian structural): fits using `bsts()` and extracts predictive intervals.
#' Returned values are back-transformed from log scale.
forecast_model_ts <- function(ts_train, h, method,
                              hybrid_parallel = TRUE, hybrid_cores = 10,
                              bsts_niter = 1000, seed = 20240902) {
     
     # 1. Input Validation and Setup
     valid_methods <- c("Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural")
     method <- match.arg(method, valid_methods)
     
     # Initialize result vectors
     lower_95 <- lower_80 <- upper_80 <- upper_95 <- rep(NA, h)
     mean_forecast <- rep(NA, h)
     
     set.seed(seed)
     
     # ---------------------------------------------------------
     # 2. Model Fitting and Forecasting
     # ---------------------------------------------------------
     
     # GROUP A: Standard Models (Neural Network, ETS, SARIMA, TBATS)
     if (method %in% c("Neural Network", "ETS", "SARIMA", "TBATS")) {
          
          # Fit the appropriate model
          mod <- switch(method,
                        "Neural Network" = nnetar(ts_train, lambda = NULL),
                        "ETS"            = ets(ts_train, ic = "aicc", lambda = NULL),
                        "SARIMA"         = auto.arima(ts_train, seasonal = TRUE, ic = "aicc", lambda = NULL),
                        "TBATS"          = tbats(ts_train, seasonal.periods = 12)
          )
          
          # Generate forecast
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          
          # Extract intervals if available
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
          
          # GROUP B: Hybrid Model
     } else if (method == "Hybrid") {
          
          # Configure window size for cross-validation
          max_window <- length(ts_train) - (2 * 12) - 2
          final_window <- max(2 * 12, min(round(length(ts_train) * 0.7), max_window))
          
          # Fit Hybrid model
          mod <- hybridModel(ts_train,
                             lambda = NULL,
                             models = c("aent"),
                             a.args = list(seasonal = TRUE),
                             weights = "cv.errors",
                             windowSize = final_window,
                             parallel = hybrid_parallel,
                             num.cores = hybrid_cores,
                             errorMethod = "RMSE")
          
          # Generate forecast
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
          
          # GROUP C: Bayesian Structural Time Series
     } else if (method == "Bayesian structural") {
          
          # Define state specification
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = frequency(ts_train))
          
          # Fit BSTS model
          mod <- bsts(ts_train, state.specification = ss, niter = bsts_niter, seed = seed, ping = 0)
          burn <- SuggestBurn(0.1, mod)
          
          # Predict
          pred <- predict.bsts(mod, horizon = h, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          mean_forecast <- as.numeric(pred$mean)
          
          if (!is.null(pred$interval)) {
               lower_95 <- as.numeric(pred$interval[1, ])
               lower_80 <- as.numeric(pred$interval[2, ])
               upper_80 <- as.numeric(pred$interval[3, ])
               upper_95 <- as.numeric(pred$interval[4, ])
          }
     }
     
     # 3. Return Results (Back-transform from Log scale)
     return(list(mean = exp(mean_forecast),
                 lower_95 = if (all(is.na(lower_95))) rep(NA, h) else exp(lower_95),
                 lower_80 = if (all(is.na(lower_80))) rep(NA, h) else exp(lower_80),
                 upper_80 = if (all(is.na(upper_80))) rep(NA, h) else exp(upper_80),
                 upper_95 = if (all(is.na(upper_95))) rep(NA, h) else exp(upper_95)))
}

#' Forecast with Monte Carlo Simulation
#'
#' Fits a time series model and generates simulation paths (MCMC) to capture uncertainty.
#' Returns statistics (mean, CIs) and the raw simulation matrix.
#'
#' @param ts_train Time series object (assumed to be Log-transformed).
#' @param h Forecast horizon (integer).
#' @param method One of: "Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural".
#' @param hybrid_parallel Logical, for Hybrid model parallel processing.
#' @param hybrid_cores Integer, number of cores for Hybrid model.
#' @param bsts_niter Integer, iterations for BSTS model.
#' @param n_paths Integer, number of Monte Carlo simulation paths (default 1000).
#' @param seed Integer, for reproducibility.
#'
#' @return A list containing mean, confidence intervals, and the raw MCMC matrix (original scale).
forecast_model_sim <- function(ts_train, h, method,
                               hybrid_parallel = TRUE, hybrid_cores = 10,
                               bsts_niter = 1000, n_paths = 1000, seed = 20251209) {
     
     # 1. Input Validation and Setup
     valid_methods <- c("Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural")
     method <- match.arg(method, valid_methods)
     
     set.seed(seed)
     
     # Initialize matrix to store log-scale paths (Rows=Time, Cols=Paths)
     sim_matrix_log <- matrix(NA, nrow = h, ncol = n_paths)
     
     # ---------------------------------------------------------
     # 2. Model Fitting and Simulation
     # ---------------------------------------------------------
     
     # GROUP A: Standard Models (Supported by forecast::simulate)
     if (method %in% c("Neural Network", "ETS", "SARIMA", "TBATS")) {
          
          # Fit the appropriate model
          mod <- switch(method,
                        "Neural Network" = nnetar(ts_train, lambda = NULL),
                        "ETS"            = ets(ts_train, ic = "aicc", lambda = NULL),
                        "SARIMA"         = auto.arima(ts_train, seasonal = TRUE, ic = "aicc", lambda = NULL),
                        "TBATS"          = tbats(ts_train, seasonal.periods = 12)
          )
          
          # Generate paths using replicate (Cleaner than for-loop)
          # simulate() handles the bootstrapping of residuals automatically
          sim_matrix_log <- replicate(n_paths, 
                                      as.numeric(simulate(mod, nsim = h, future = TRUE, bootstrap = TRUE)))
          
          # GROUP B: Hybrid Model (Manual Parametric Bootstrap)
     } else if (method == "Hybrid") {
          
          # Configure window size for cross-validation
          max_window <- length(ts_train) - (2 * 12) - 2
          final_window <- max(2 * 12, min(round(length(ts_train) * 0.7), max_window))
          
          # Fit Hybrid model
          mod <- hybridModel(ts_train, lambda = NULL,
                             models = c("aent"), a.args = list(seasonal = TRUE),
                             weights = "cv.errors", windowSize = final_window,
                             parallel = hybrid_parallel, num.cores = hybrid_cores,
                             errorMethod = "RMSE")
          
          # Extract mean forecast and historical residuals
          fc_out <- forecast(mod, h = h)
          mu <- as.numeric(fc_out$mean)
          resids <- na.omit(mod$residuals)
          
          # Simulate: Mean + Bootstrapped Residuals
          sim_matrix_log <- replicate(n_paths, {
               sim_noise <- sample(resids, size = h, replace = TRUE)
               mu + sim_noise
          })
          
          # GROUP C: Bayesian Structural Time Series (Native MCMC)
     } else if (method == "Bayesian structural") {
          
          # Define state specification
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = frequency(ts_train))
          
          # Fit BSTS model
          mod <- bsts(ts_train, state.specification = ss, niter = bsts_niter, seed = seed, ping = 0)
          burn <- SuggestBurn(0.1, mod)
          
          # Predict and extract posterior distribution
          pred <- predict.bsts(mod, horizon = h, burn = burn)
          posterior_samples <- pred$distribution # Dimensions: (n_samples x h)
          
          # Resample to match requested n_paths
          avail_samples <- nrow(posterior_samples)
          idx <- sample(1:avail_samples, size = n_paths, replace = TRUE)
          
          # Transpose to match standard format (Rows=Time, Cols=Paths)
          sim_matrix_log <- t(posterior_samples[idx, ])
     }
     
     # Back-transform from Log scale to Original scale
     sim_matrix_exp <- exp(sim_matrix_log)
     
     # Helper function to compute quantiles cleanly
     get_quantile <- function(x, p) apply(x, 1, quantile, probs = p, na.rm = TRUE)
     
     # Construct result list
     results <- list(
          mean     = rowMeans(sim_matrix_exp, na.rm = TRUE),
          median   = get_quantile(sim_matrix_exp, 0.5),
          lower_95 = get_quantile(sim_matrix_exp, 0.025),
          lower_80 = get_quantile(sim_matrix_exp, 0.100),
          upper_80 = get_quantile(sim_matrix_exp, 0.900),
          upper_95 = get_quantile(sim_matrix_exp, 0.975),
          MCMC     = sim_matrix_exp
     )
     
     return(results)
}

# visualize appendix ---------------------------------------------------------------------------------------------------- 

# plot multi-split forecast outcome in appendix: cross-validation to find best model
plot_outcome_multisplit <- function(datafile_single,
                                    forecasts_df,
                                    split_starts,
                                    max_case,
                                    n,
                                    title) {
     # forecasts_df should contain: date, mean, split (factor/character)
     # datafile_single contains observed series with columns `date` and `value`
     observed_df <- filter(datafile_single,
                           date <= max(forecasts_df$date))
     
     max_value <- max(c(max(observed_df$value, na.rm = TRUE), max(forecasts_df$mean, na.rm = TRUE)), max_case)
     min_value <- min(c(min(observed_df$value, na.rm = TRUE), min(forecasts_df$mean, na.rm = TRUE)))
     
     # plot observed and multi-split forecast means
     fig1 <- ggplot() +
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'),
                    linewidth = 0.7, data = observed_df) +
          geom_line(mapping = aes(x = date, y = mean, colour = split),
                    linewidth = 0.8, data = forecasts_df)
     
     # add vertical lines at each split start
     for (d in split_starts) {
          fig1 <- fig1 + geom_vline(xintercept = as.numeric(d), linetype = 'dashed', colour = 'grey50')
     }
     
     axis_y_breaks <- pretty(c(0, min_value, max_value, 0))
     
     fig1 <- fig1 +
          annotate('text', x = median(observed_df$date), y = Inf, label = 'Observed', vjust = 1) +
          annotate('text', x = median(range(forecasts_df$date)), y = Inf, label = 'Test', vjust = 1) +
          coord_cartesian(ylim = range(axis_y_breaks), clip = 'on') +
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(observed_df$date), max(forecasts_df$date) + 31, by = '2 years')) +
          scale_y_continuous(expand = c(0, 0), label = scientific_10, breaks = axis_y_breaks) +
          theme_set() +
          theme(legend.position = 'bottom') +
          labs(x = 'Date', y = 'Monthly cases', color = '', title = paste0(LETTERS[n], ': ', title))
     
     # allow color/fill by split to show in legend; if split levels exist, add scales for them
     if ('split' %in% names(forecasts_df)) {
          splits <- unique(forecasts_df$split)
          pal <- fill_color[seq_along(splits)]
          names(pal) <- splits
          # add ribbons for 95% and 80% (if available) behind the lines
          if (all(c('lower_95', 'upper_95') %in% names(forecasts_df))) {
               fig1 <- fig1 +
                    geom_ribbon(data = forecasts_df, mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = split),
                                alpha = 0.3, inherit.aes = FALSE,
                                show.legend = F)
          }
          fig1 <- fig1 +
               scale_color_manual(values = c('Observed' = '#43B284FF', pal)) +
               scale_fill_manual(values = c(setNames(pal, splits)))
     }
     
     return(fig1)
}


# visualize main -------------------------------------------------------------------

## plot single panel of disease 

plot_single_panel <- function(i, outcome, titles, recovery = NULL, display_recovery = F, y_angle = 90){
     # related data
     outcome_data <- outcome[[i]]$outcome_data
     outcome_plot_1 <- outcome[[i]]$outcome_plot_1
     outcome_plot_2 <- outcome[[i]]$outcome_plot_2
     max_value <- outcome[[i]]$max_value
     min_value <- outcome[[i]]$min_value
     max_case <- outcome[[i]]$max_case
     
     plot_breaks <- pretty(c(min_value, max_value, 0), n = 3)
     
     # browser()
     
     # determine display recovery interval
     if (display_recovery) {
          data_recovery <- recovery |> 
               filter(Shortname == unique(outcome_data$Shortname)) |> 
               mutate(ymin = max(plot_breaks) * c(1.0, 1.1),
                      ymax = max(plot_breaks) * c(1.1, 1.2),
                      # Compute per-row midpoint date safely by averaging numeric representation
                      xlabs = as.Date((as.numeric(StartDate) + as.numeric(EndDate)) / 2),
                      ylabs = (ymin + ymax) / 2)

          fig <- ggplot(data = data_recovery) +
               geom_rect(mapping = aes(xmin = StartDate, xmax = EndDate, ymin = ymin, ymax = ymax, fill = type),
                         alpha = 0.2,
                         inherit.aes = FALSE,
                         show.legend = TRUE) +
               geom_text(mapping = aes(x = xlabs,
                                       y = ylabs,
                                       group = type,
                                       label = Period),
                         size = 3,
                         inherit.aes = FALSE)+
               # Draw per-row vertical segments for recovery/balance end dates
               geom_segment(data = data_recovery,
                            mapping = aes(x = EndDate, xend = EndDate, y = 0, yend = ymax),
                            linetype = 'dashed',
                            color = 'black',
                            alpha = 0.5,
                            inherit.aes = FALSE)+
               # Draw a baseline vertical line at the start of forecast (full height)
               geom_vline(xintercept = min(outcome_plot_2$date),
                          linetype = 'dashed',
                          color = 'black',
                          alpha = 0.5)+
               geom_hline(yintercept = min(data_recovery$ymin),
                          linetype = 'solid',
                          color = 'black',
                          alpha = 0.7) +
               scale_fill_manual(values = c(Recovery = "#004F7AFF", Balance = "#F3C558FF"),
                                 name = "Periods")
     } else {
          fig <- ggplot()
     }
     
     fig <- fig +
          ggnewscale::new_scale_fill()+
          geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed")) +
          geom_line(data = outcome_plot_2, mapping = aes(x = date, y = median, colour = "Forecasted")) +
          stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = median),
                          alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = T) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_dates[1] - 365, NA)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "2 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = ifelse(max_case > 1000, scientific_10, scales::comma),
                             breaks = plot_breaks) +
          scale_color_manual(values = c(Forecasted = "#6DAE90FF", Observed = "#CC3D24FF")) +
          scale_fill_manual(values = c(Decreased = "#6DAE9050", Increased = "#CC3D2450"),
                            drop = F) +
          theme_bw() +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = 'vertical',
                legend.title.position = 'top',
                legend.key = element_rect(fill = "transparent", colour = NA),
                axis.text.y = element_text(angle = y_angle, hjust = ifelse(y_angle == 90, 0.5, 1)),
                axis.text.x = element_text(hjust = 0),
                panel.grid = element_blank(),
                plot.title.position = ifelse(y_angle == 90, 'panel', 'plot'),
                plot.title = element_text(face = 'bold', size = 14, hjust = 0))+
          labs(x = 'Date',
               y = 'Monthly cases',
               color = 'Monthly cases',
               fill = 'Difference',
               title = titles[i]) +
          guides(color = guide_legend(order = 1, override.aes = list(fill = NA)),
                 fill = guide_legend(order = 2))
     
     return(fig)
}

# recovery ----------------------------------------------------------------

library(dplyr)
library(lubridate)

get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) return(NA_real_)
  y_diff <- lubridate::year(end) - lubridate::year(start)
  m_diff <- lubridate::month(end) - lubridate::month(start)
  return(y_diff * 12 + m_diff)
}


#' Calculate Comprehensive Disease Dynamics Metrics (Restored Balance_Date)
#'
#' @param outcome_list List containing model results.
#' @param start_date_calc Start date for calculation.
#' @param recovery_threshold Threshold for trend recovery.
#'
#' @return A tibble with added Payback metrics.
calculate_disease_metrics <- function(outcome_list, 
                                      start_date_calc = "2020-01-01",
                                      recovery_threshold = 0.95) {
  
  start_date_calc <- as.Date(start_date_calc)
  
  purrr::map_dfr(outcome_list, function(item) {
    
    # --- 1. Data Prep ---
    od <- item$outcome_data
    if (is.null(od) || nrow(od) == 0) return(NULL)
    shortname <- unique(od$Shortname)[1]
    
    # Check columns
    if (!all(c('date', 'value', 'median') %in% names(od))) return(NULL)
    
    df_calc <- od %>%
      filter(date >= start_date_calc) %>%
      arrange(date) %>%
      mutate(
        diff = value - median,
        cum_diff = cumsum(diff),
        cum_median = cumsum(median),
        is_recovered_trend = value >= median * recovery_threshold
      )
    
    if (nrow(df_calc) == 0) return(NULL)
    
    # --- 2. Deficit & Trough ---
    trough_idx <- which.min(df_calc$cum_diff)
    trough_date <- df_calc$date[trough_idx]
    max_deficit_raw <- df_calc$cum_diff[trough_idx]
    
    # Relative Deficit
    cum_expected_at_trough <- df_calc$cum_median[trough_idx]
    relative_deficit <- if (cum_expected_at_trough > 0) max_deficit_raw / cum_expected_at_trough else NA_real_
    
    # Start Deficit
    first_drop_idx <- which(df_calc$cum_diff < 0)[1]
    start_deficit_date <- if (!is.na(first_drop_idx)) df_calc$date[first_drop_idx] else NA
    
    # --- 3. Recovery (Trend) ---
    recovery_date <- NA
    status <- "No Deficit"
    
    if (!is.na(start_deficit_date) && max_deficit_raw < 0) {
      df_search <- df_calc %>% filter(date >= start_deficit_date)
      is_robust <- zoo::rollapply(df_search$is_recovered_trend, width = 3, FUN = all, fill = FALSE, align = "left")
      rec_idx <- which(is_robust)[1]
      
      if (!is.na(rec_idx)) {
        recovery_date <- df_search$date[rec_idx]
        status <- "Recovered"
      } else {
        status <- "Suppressed"
      }
    }
    
    # --- 4. Balance Date (The "Payback" Moment) ---
    # Logic: When does cum_diff cross back above 0 AFTER the trough?
    balance_date <- NA
    
    if (!is.na(trough_date) && max_deficit_raw < 0) {
      # Only search after the trough (we must pay back debt)
      df_post_trough <- df_calc %>% filter(date > trough_date)
      
      # Find first time cum_diff >= 0
      bal_idx <- which(df_post_trough$cum_diff >= 0)[1]
      
      if (!is.na(bal_idx)) {
        balance_date <- df_post_trough$date[bal_idx]
        status <- "Debt Repaid" # Update status if they actually paid it back
      }
    }
    
    # --- 5. Rebound Intensity ---
    rebound_intensity <- NA
    if (!is.na(trough_date) && max_deficit_raw < 0) {
      df_post_trough <- df_calc %>% filter(date >= trough_date)
      if (nrow(df_post_trough) > 0) {
        rebound_intensity <- max(df_post_trough$value / (df_post_trough$median + 1), na.rm = TRUE)
      }
    }
    
    # --- 6. Durations ---
    suppression_months <- get_months(start_deficit_date, recovery_date)
    
    # Payback Months: From Trough (lowest point) to Balance (zero point)
    payback_months <- get_months(trough_date, balance_date)
    
    # Handle suppressed case duration
    if (status == "Suppressed" && !is.na(start_deficit_date)) {
      suppression_months <- get_months(start_deficit_date, max(df_calc$date))
    }

    tibble(
      Shortname = shortname,
      Group = unique(item$info$Group),
      
      # Timeline
      Date_Start_Deficit = start_deficit_date,
      Date_Trough = trough_date,
      Date_Recovery = recovery_date,
      Date_Balance = balance_date, # NEW
      
      # Metrics
      Max_Deficit_Raw = max_deficit_raw,
      Relative_Deficit = relative_deficit,
      Rebound_Intensity = rebound_intensity,
      
      # Durations
      Suppression_Months = suppression_months,
      Payback_Months = payback_months, # NEW
      
      Status = status
    )
  })
}