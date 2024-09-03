# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(stats)
library(tseries)
library(astsa)
library(forecast)
library(forecastHybrid)
library(caret)
library(bsts)
library(patchwork)
library(Cairo)
library(ggpubr)
library(paletteer)

library(doParallel)

set.seed(202408)

remove(list = ls())

# data load ---------------------------------------------------------------

source("./function/theme_set.R")
source("./function/forecast.R")

load("./month.RData")

train_range <- c(as.Date('2007-1-1'), as.Date('2018-12-1'))
test_range <- c(as.Date('2019-1-1'), as.Date('2019-12-1'))

disease_name <- data_class$Shortname

# process model -----------------------------------------------------------

process_model <- function(mod, ts_train, ts_test, test_length, add_value, index_labels, ts_obse, data_single, split_date, max_case, method_name, plot_number) {
     
     # Generate forecasts
     outcome <- forecast(mod, h = test_length)
     
     # Prepare outcome data for plotting
     outcome_plot_1 <- data.frame(date = zoo::as.Date(time(outcome$x)),
                                  simu = exp(as.matrix(outcome$x)),
                                  fit = exp(as.matrix(outcome$fitted)))
     
     outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                  mean = exp(as.matrix(outcome$mean)))
     
     if ("lower" %in% names(outcome)) {
          outcome_plot_2 <- cbind(outcome_plot_2,
                                  lower_80 = exp(as.matrix(outcome$lower[, 1])),
                                  lower_95 = exp(as.matrix(outcome$lower[, 2])),
                                  upper_80 = exp(as.matrix(outcome$upper[, 1])),
                                  upper_95 = exp(as.matrix(outcome$upper[, 2])))
          plot_ribbon <- T
     } else {
          plot_ribbon <- F
     }
     
     # Calculate fit goodness metrics
     fit_goodness <- data.frame(Method = method_name,
                                Index = index_labels,
                                Train = evaluate_forecast(outcome_plot_1$fit, outcome_plot_1$simu),
                                Test = evaluate_forecast(outcome_plot_2$mean, ts_test))
     
     # Plot results
     fig <- plot_outcome(outcome_plot_1,
                         outcome_plot_2,
                         data_single,
                         split_date,
                         max_case,
                         plot_number,
                         plot_ribbon,
                         method_name)
     
     return(list(fit_goodness, fig))
}

table_build <- function(data_table, i) {
     index <- index_labels[i]
     data <- data_table[data_table$Index == index, 1:3]
     ggtexttable(data,
                 rows = NULL,
                 cols = c("Method", "Train", "Test"),
                 theme = ttheme("blank", base_size = 10, padding = unit(c(5, 5), "mm"))
     ) |>
          tab_add_hline(at.row = nrow(data_table) / 4 + 1, row.side = "bottom", linewidth = 2) |>
          tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) |>
          tab_add_title(paste(LETTERS[i + 6], ":", index, " of models"), face = "bold", size = 14) |>
          tab_add_footnote("*Hybrid: Combined Neural network,\nETS, SARIMA and TBATS model,\nweighted byRMSE",
                           just = "left", hjust = 1, size = 10
          )
}

# data clean --------------------------------------------------------------

i <- 2

auto_select_function <- function(i, split_date, add_value, index_labels) {
     set.seed(202408)
     data_single <- data_analysis |>
          filter(Shortname == disease_name[i]) |>
          select(Date, Shortname, Cases) |> 
          rename(date = 'Date',
                 value = 'Cases')

     df_simu <- data_single |>
          arrange(date) |>
          unique() |>
          filter(date < split_date)
     
     max_case <- max(df_simu$value, na.rm = T)
     
     ts_obse <- ts(df_simu$value,
                   frequency = 12,
                   start = c(as.numeric(format(min(data_single$date), "%Y")),
                             as.numeric(format(min(data_single$date), "%m"))))
     ts_train <- window(ts_obse,
                        start = c(as.numeric(format(train_range[1], "%Y")),
                                  as.numeric(format(train_range[1], "%m"))),
                        end = c(as.numeric(format(train_range[2], "%Y")),
                                as.numeric(format(train_range[2], "%m"))))
     ts_train <- log(ts_train)
     
     ts_test <- window(ts_obse,
                       end = c(as.numeric(format(test_range[2], "%Y")),
                               as.numeric(format(test_range[2], "%m"))),
                       start = c(as.numeric(format(test_range[1], "%Y")),
                                 as.numeric(format(test_range[1], "%m"))))
     ts_test <- log(ts_test)
     test_length <- length(ts_test)
     
     fit_goodness <- data.frame()
     
     ## NNET --------------------------------------------------------------------
     
     mod <- nnetar(ts_train, lambda = "auto")
     outcome <- process_model(mod, ts_train,
                              ts_test, test_length,
                              add_value, index_labels,
                              ts_obse, data_single,
                              split_date, max_case,
                              "Neural Network", 1)
     fit_goodness <- rbind(fit_goodness, outcome[[1]])
     fig_nnet_1 <- outcome[[2]]
     rm(mod, outcome)
     
     # ETS ---------------------------------------------------------------------
     
     mod <- ets(ts_train, ic = "aicc", lambda = "auto")
     outcome <- process_model(mod, ts_train,
                              ts_test, test_length,
                              add_value, index_labels,
                              ts_obse, data_single,
                              split_date, max_case,
                              "ETS", 2)
     fit_goodness <- rbind(fit_goodness, outcome[[1]])
     fig_ets_1 <- outcome[[2]]
     rm(mod, outcome)
     
     # SARIMA -------------------------------------------------------------------
     
     mod <- auto.arima(ts_train, seasonal = T, ic = "aicc", lambda = "auto")
     outcome <- process_model(mod, ts_train,
                              ts_test, test_length,
                              add_value, index_labels,
                              ts_obse, data_single,
                              split_date, max_case,
                              "SARIMA", 3)
     fit_goodness <- rbind(fit_goodness, outcome[[1]])
     fig_sarima_1 <- outcome[[2]]
     rm(mod, outcome)
     
     # tbats -------------------------------------------------------------------
     # Exponential smoothing state space model with Box-Cox transformation, ARMA errors,
     # Trend and Seasonal components
     
     mod <- tbats(ts_train, seasonal.periods = 12)
     outcome <- process_model(mod, ts_train,
                              ts_test, test_length,
                              add_value, index_labels,
                              ts_obse, data_single,
                              split_date, max_case,
                              "TBATS", 4)
     fit_goodness <- rbind(fit_goodness, outcome[[1]])
     fig_tbats_1 <- outcome[[2]]
     rm(mod, outcome)
     
     # Mixture ts --------------------------------------------------------------
     
     mod <- hybridModel(ts_train,
                        lambda = "auto",
                        models = c("aent"),
                        a.args = list(seasonal = T),
                        weights = "cv.errors",
                        windowSize = 36,
                        parallel = TRUE, num.cores = 10,
                        errorMethod = "RMSE")
     outcome <- process_model(mod, ts_train,
                              ts_test, test_length,
                              add_value, index_labels,
                              ts_obse, data_single,
                              split_date, max_case,
                              "Hybrid", 5)
     fit_goodness <- rbind(fit_goodness, outcome[[1]])
     fig_hyb_1 <- outcome[[2]]
     rm(mod, outcome)
     
     # Bayesian --------------------------------------------------------------
     
     ss <- AddLocalLinearTrend(list(), ts_train)
     ss <- AddSeasonal(ss, ts_train, nseasons = 12)
     mod <- bsts(ts_train, state.specification = ss, niter = 1000, seed = 20240902)
     burn <- SuggestBurn(0.1, mod)
     outcome <- predict.bsts(mod, horizon = test_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
     
     outcome_plot_1 <- data.frame(
          date = zoo::as.Date(time(ts_train)),
          simu = as.numeric(ts_train) - add_value,
          fit = as.numeric(-colMeans(mod$one.step.prediction.errors[-(1:burn), ]) + ts_train) - add_value
     )
     outcome_plot_2 <- data.frame(
          date = as.Date(time(ts_test)),
          mean = outcome$mean - add_value,
          lower_80 = outcome$interval[2, ] - add_value,
          lower_95 = outcome$interval[1, ] - add_value,
          upper_80 = outcome$interval[3, ] - add_value,
          upper_95 = outcome$interval[4, ] - add_value
     )
     
     fit_goodness <- fit_goodness |>
          rbind(data.frame(Method = "Bayesian structural",
                           Index = index_labels,
                           Train = evaluate_forecast(outcome_plot_1$simu[!is.na(outcome_plot_1$fit)],
                                                     outcome_plot_1$fit[!is.na(outcome_plot_1$fit)]),
                           Test = evaluate_forecast(outcome_plot_2$mean, ts_test)))
     
     fig_baye_1 <- plot_outcome(
          outcome_plot_1,
          outcome_plot_2,
          data_single,
          split_date,
          max_case,
          6,
          T,
          "Bayesian structural"
     )
     rm(mod, outcome, outcome_plot_1, outcome_plot_2)
     
     # summary table ---------------------------------------------------------
     
     data_table <- fit_goodness |>
          mutate(
               Method = factor(Method, levels = models, labels = models_label),
               Train = round(Train, 2),
               Test = round(Test, 2)
          ) |>
          arrange(Method) |>
          select(Method, Train, Test, Index)
     data_table[is.na(data_table)] <- ""
     
     fig_table <- lapply(1:length(index_labels), table_build, data_table = data_table) |>
          wrap_plots(plot_list, nrow = 1)
     
     # save --------------------------------------------------------------------
     
     fig_ts <- fig_nnet_1 + fig_ets_1 + fig_sarima_1 + fig_tbats_1 + fig_hyb_1 + fig_baye_1 +
          plot_layout(ncol = 2, guides = "collect") &
          theme(legend.position = "bottom",
                plot.margin = margin(5, 15, 5, 5))
     
     fig <- cowplot::plot_grid(fig_ts, fig_table, ncol = 1, rel_heights = c(3, 1))
     
     ggsave(
          filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_3/", disease_name[i], ".png"),
          fig,
          device = "png",
          width = 14, height = 15,
          limitsize = FALSE,
          dpi = 300
     )
     fit_goodness$disease <- disease_name[i]
     
     return(fit_goodness)
}

# run model ---------------------------------------------------------------

# auto_select_function(6)

cl <- makeCluster(length(disease_name))
registerDoParallel(cl)
clusterEvalQ(cl, {
     
     library(tidyverse)
     library(stats)
     library(tseries)
     library(astsa)
     library(forecast)
     library(forecastHybrid)
     library(caret)
     library(bsts)
     library(patchwork)
     library(Cairo)
     library(ggpubr)
     library(paletteer)
     
     set.seed(20240806)
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, 1:length(disease_name), auto_select_function,
                      split_date = split_dates[1],
                      add_value = add_value,
                      index_labels = index_labels)
stopCluster(cl)

data_outcome <- do.call("rbind", outcome)
write.xlsx(data_outcome, "../Outcome/Appendix/Supplementary Appendix 2.xlsx")
