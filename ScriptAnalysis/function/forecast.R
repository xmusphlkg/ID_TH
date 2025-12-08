
plot_outcome <- function(outcome_plot_1,
                         outcome_plot_2,
                         datafile_single,
                         split_date,
                         max_case,
                         n,
                         inter,
                         title) {
     
     outcome_plot_1_2_link <- data.frame(
          date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
          value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
                    outcome_plot_2[1, 'mean'])
     )
     
     max_value <- max(c(max(outcome_plot_1[,-1], na.rm = T), max(outcome_plot_2[,'mean'], na.rm = T)), max_case)
     min_value <- min(c(min(outcome_plot_1[,-1], na.rm = T), min(outcome_plot_2[,'mean'], na.rm = T)))
     
     fig1 <- ggplot()+
          geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
                    linewidth = 0.7, data = filter(datafile_single, date < split_date))+
          geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
                    linewidth = 0.7, data = outcome_plot_1)+
          geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
                    linewidth = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
          geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
                    linewidth = 0.7, data = outcome_plot_2)
     
     if (inter) {
          fig1 <- fig1 +
               geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80),
                           data = outcome_plot_2, fill = '#0F7BA2FF', alpha = 0.3, show.legend = F)+
               geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95),
                           data = outcome_plot_2, fill = '#0F7BA2FF', alpha = 0.3, show.legend = F)
     }
     
     fig1 <- fig1 +
          geom_vline(xintercept = median(outcome_plot_1_2_link$date), show.legend = F,
                     linetype = 'longdash')+
          geom_hline(yintercept = 0, show.legend = F)+
          annotate('text', x = median(outcome_plot_1$date), y = Inf, label = 'Train', vjust = 1)+
          annotate('text', x = median(outcome_plot_2$date), y = Inf, label = 'Test', vjust = 1)+
          coord_cartesian(ylim = c(0, range(pretty(c(min_value, max_value, 0)))[2]),
                          clip = 'on')+
          scale_x_date(expand = expansion(add = c(0, 31)),
                       date_labels = '%Y',
                       breaks = seq(min(outcome_plot_1$date), max(outcome_plot_2$date)+31, by="2 years"))+
          scale_y_continuous(expand = c(0, 0),
                             label = scientific_10,
                             breaks = pretty(c(min_value, max_value, 0)))+
          scale_color_manual(
               values = c(Fitted = "#DD5129FF",
                          Forecasted = "#0F7BA2FF",
                          Observed = "#43B284FF")
          )+
          theme_set()+
          theme(legend.position = 'bottom')+
          labs(x = "Date",
               y = 'Monthly incidence',
               color = '',
               title = paste0(LETTERS[n], ': ', title))
     
     return(fig1)
}

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
