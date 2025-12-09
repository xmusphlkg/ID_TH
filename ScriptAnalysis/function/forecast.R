
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


# Reusable forecasting helper -------------------------------------------------
# ts_train: a (logged) time series used for fitting (already transformed if needed)
# h: forecast horizon (integer)
# method: one of "Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural"
# hybrid_parallel, hybrid_cores: passed to hybridModel when method == "Hybrid"
# bsts_niter: iterations for bsts when method == "Bayesian structural"
# seed: random seed for reproducibility
forecast_model_ts <- function(ts_train, h, method,
                              hybrid_parallel = FALSE, hybrid_cores = 1,
                              bsts_niter = 1000, seed = 20240902) {
     # initialize
     lower_95 <- lower_80 <- upper_80 <- upper_95 <- rep(NA, h)
     mean_forecast <- rep(NA, h)
     
     if (method == "Neural Network") {
          mod <- nnetar(ts_train, lambda = NULL)
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
     } else if (method == "ETS") {
          mod <- ets(ts_train, ic = "aicc", lambda = NULL)
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
     } else if (method == "SARIMA") {
          mod <- auto.arima(ts_train, seasonal = TRUE, ic = "aicc", lambda = NULL)
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
     } else if (method == "TBATS") {
          mod <- tbats(ts_train, seasonal.periods = 12)
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
     } else if (method == "Hybrid") {
          # hybridModel expects the original (logged) ts; control parallel via args
          max_window <- length(ts_train) - (2 * 12) - 2
          final_window <- max(2 * 12, min(round(length(ts_train) * 0.7), max_window))
          mod <- hybridModel(ts_train,
                             lambda = NULL,
                             models = c("aent"),
                             a.args = list(seasonal = TRUE),
                             weights = "cv.errors",
                             windowSize = final_window,
                             parallel = hybrid_parallel,
                             num.cores = hybrid_cores,
                             errorMethod = "RMSE")
          out <- forecast(mod, h = h)
          mean_forecast <- as.numeric(out$mean)
          if (!is.null(out$lower) && !is.null(out$upper)) {
               lower_80 <- as.numeric(out$lower[, 1])
               lower_95 <- as.numeric(out$lower[, 2])
               upper_80 <- as.numeric(out$upper[, 1])
               upper_95 <- as.numeric(out$upper[, 2])
          }
     } else if (method == "Bayesian structural") {
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = frequency(ts_train))
          mod <- bsts(ts_train, state.specification = ss, niter = bsts_niter, seed = seed)
          burn <- SuggestBurn(0.1, mod)
          pred <- predict.bsts(mod, horizon = h, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          mean_forecast <- as.numeric(pred$mean)
          if (!is.null(pred$interval)) {
               lower_95 <- as.numeric(pred$interval[1, ])
               lower_80 <- as.numeric(pred$interval[2, ])
               upper_80 <- as.numeric(pred$interval[3, ])
               upper_95 <- as.numeric(pred$interval[4, ])
          }
     } else {
          stop(sprintf('Unknown forecasting method: %s', method))
     }
     
     # returned values are on the original scale (undo log)
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
                               hybrid_parallel = FALSE, hybrid_cores = 1,
                               bsts_niter = 1000, n_paths = 1000, seed = 20240902) {
  
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
    lower_95 = get_quantile(sim_matrix_exp, 0.025),
    lower_80 = get_quantile(sim_matrix_exp, 0.100),
    upper_80 = get_quantile(sim_matrix_exp, 0.900),
    upper_95 = get_quantile(sim_matrix_exp, 0.975),
    MCMC     = sim_matrix_exp
  )
  
  return(results)
}

# visualize outcomes ---------------------------------------------------------------------------------------------------- 

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


# panel -------------------------------------------------------------------

## plot single panel of disease 

plot_single_panel <- function(i, outcome, titles, y_angle = 90){
     # related data
     outcome_data <- outcome[[i]]$outcome_data
     outcome_plot_1 <- outcome[[i]]$outcome_plot_1
     outcome_plot_2 <- outcome[[i]]$outcome_plot_2
     max_value <- outcome[[i]]$max_value
     min_value <- outcome[[i]]$min_value
     max_case <- outcome[[i]]$max_case
     
     plot_breaks <- pretty(c(min_value, max_value, 0), n = 3)
     
     fig <- ggplot() +
          geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed")) +
          geom_line(data = outcome_plot_2, mapping = aes(x = date, y = mean, colour = "Forecasted")) +
          stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = mean),
                          alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = T) +
          coord_cartesian(ylim = c(0, NA),
                          xlim = c(split_dates[1] - 365, NA)) +
          scale_x_date(expand = expansion(add = c(0, 0)),
                       date_labels = "%Y",
                       breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "2 years")) +
          scale_y_continuous(expand = c(0, 0),
                             label = ifelse(max_case > 1000, scientific_10, scales::comma),
                             breaks = plot_breaks,
                             limits = range(plot_breaks)) +
          scale_color_manual(values = c(Forecasted = "#004F7AFF", Observed = "#CC3D24FF")) +
          scale_fill_manual(values = c(Decreased = "#004F7A50", Increased = "#CC3D2450"),
                            drop = F) +
          theme_bw() +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = 'vertical',
                legend.title.position = 'top',
                axis.text.y = element_text(angle = y_angle, hjust = ifelse(y_angle == 90, 0.5, 1)),
                axis.text.x = element_text(hjust = 0),
                panel.grid = element_blank(),
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
