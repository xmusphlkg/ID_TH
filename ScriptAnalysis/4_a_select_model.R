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

load("./temp/month.RData")

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1 & Forecasting == 1)|> 
     mutate(Group = factor(Group, levels = disease_groups)) |> 
     arrange(Group, desc(Cases)) |> 
     select(Disease, Fullname, Shortname, Group) 

# Define three cross-validation splits (train ranges and corresponding test ranges)
cv_splits <- list(
     list(train_start = as.Date('2008-01-01'), train_end = as.Date('2018-12-01'),
          test_start = as.Date('2019-01-01'), test_end = as.Date('2019-12-01'), label = '2019'),
     list(train_start = as.Date('2008-01-01'), train_end = as.Date('2017-12-01'),
          test_start = as.Date('2018-01-01'), test_end = as.Date('2019-12-01'), label = '2018-2019'),
     list(train_start = as.Date('2008-01-01'), train_end = as.Date('2016-12-01'),
          test_start = as.Date('2017-01-01'), test_end = as.Date('2019-12-01'), label = '2017-2019')
)

disease_name <- data_class$Shortname

table_build <- function(data_table, i) {
     index <- index_labels[i]
     
     # detect test columns dynamically (columns starting with 'Test_')
     test_cols <- names(data_table)[grepl('^Test_', names(data_table))]
     if (length(test_cols) == 0) {
          stop('No Test_* columns found in data_table')
     }
     
     # prepare display labels by removing 'Test_' and replacing '_' with '-'
     display_labels <- gsub('_', '-', sub('^Test_', '', test_cols))
     
     # compute span (number of years) for sorting: larger spans first
     spans <- sapply(display_labels, function(lab) {
          if (grepl('-', lab)) {
               parts <- strsplit(lab, '-')[[1]]
               start <- as.numeric(parts[1])
               end <- as.numeric(parts[length(parts)])
               return(end - start + 1)
          } else {
               return(1)
          }
     })
     
     # order by span descending, then by start year ascending
     start_years <- sapply(display_labels, function(lab) {
          if (grepl('-', lab)) as.numeric(strsplit(lab, '-')[[1]][1]) else as.numeric(lab)
     })
     ord <- order(-spans, start_years)
     test_cols_ordered <- test_cols[ord]
     display_ordered <- display_labels[ord]
     
     # subset data for the requested index and select Method + ordered test cols
     data <- data_table[data_table$Index == index, c('Method', test_cols_ordered)]
     # replace NA with empty string for display
     data[is.na(data)] <- ''
     
     ggtexttable(data,
                 rows = NULL,
                 cols = c('Method', display_ordered),
                 theme = ttheme('default', base_size = 9, padding = unit(c(5, 5), 'mm'))) |>
          tab_add_title(paste(LETTERS[i + 6], ':', index, ' of models'), face = 'bold', size = 14) |>
          tab_add_footnote('*Hybrid: Combined Neural network,\nETS, SARIMA and TBATS model,\nweighted by RMSE',
                           just = 'left', hjust = 1, size = 9)
}

# data clean --------------------------------------------------------------

i <- 12
split_date = split_dates[1]

auto_select_function <- function(i, split_date, cv_splits, add_value, index_labels, models, models_label) {
     data_single <- data_month |>
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
     
     fit_goodness <- data.frame()
     
     # random model order
     set.seed(sample(1:100, 1))
     models <- c("Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural")
     models_order <- sample(models)
     print(models_order)
     
     # models -----------------------------------------------------------------
     
     # initialize plot objects to avoid missing-variable errors
     fig_nnet_1 <- fig_ets_1 <- fig_sarima_1 <- fig_tbats_1 <- fig_hyb_1 <- fig_baye_1 <- NULL
     model_type <- models_order[5]
     
     # container to collect forecasts (all models, all splits) for this disease
     forecasts_all <- data.frame()
     
     for (model_type in models_order) {
          set.seed(20251208)
          
          # prepare vector to collect test metrics for each CV split
          test_metrics <- matrix(NA, nrow = length(index_labels), ncol = length(cv_splits))
          # initialize forecasts container for this model
          forecasts_df <- NULL
          
          # We'll run the model for each CV split (fit on train, forecast on test)
          for (s in seq_along(cv_splits)) {
               cv <- cv_splits[[s]]
               ts_train <- window(ts_obse,
                                  start = c(as.numeric(format(cv$train_start, "%Y")), as.numeric(format(cv$train_start, "%m"))),
                                  end = c(as.numeric(format(cv$train_end, "%Y")), as.numeric(format(cv$train_end, "%m")))) + add_value
               ts_train <- log(ts_train)
               
               ts_test <- window(ts_obse,
                                 start = c(as.numeric(format(cv$test_start, "%Y")), as.numeric(format(cv$test_start, "%m"))),
                                 end = c(as.numeric(format(cv$test_end, "%Y")), as.numeric(format(cv$test_end, "%m")))) + add_value
               ts_test <- log(ts_test)
               h <- length(ts_test)
               
               # use centralized forecasting helper (returns mean and intervals on original scale)
               res <- forecast_model_ts(ts_train = ts_train, h = h, method = model_type,
                                        hybrid_parallel = FALSE, hybrid_cores = 10,
                                        bsts_niter = 1000, seed = 20240902)
               preds <- res$mean
               lower_95 <- res$lower_95
               lower_80 <- res$lower_80
               upper_80 <- res$upper_80
               upper_95 <- res$upper_95
               
               # compute test metrics for this split (on original scale)
               actuals <- exp(ts_test)
               test_eval <- evaluate_forecast(preds, actuals)
               test_metrics[, s] <- test_eval
               
               # Collect forecasts for this split into forecasts_df for later multi-split plotting
               dates_seq <- seq(cv$test_start, by = 'month', length.out = h)
               this_df <- data.frame(date = dates_seq,
                                     mean = preds,
                                     lower_95 = lower_95,
                                     lower_80 = lower_80,
                                     upper_80 = upper_80,
                                     upper_95 = upper_95,
                                     split = cv$label,
                                     stringsAsFactors = FALSE)
               if (is.null(forecasts_df)) forecasts_df <- this_df else forecasts_df <- rbind(forecasts_df, this_df)
          }
          
          # after finishing all splits for this model, append model column and add to forecasts_all
          if (!is.null(forecasts_df)) {
               forecasts_df$Method <- model_type
               forecasts_all <- rbind(forecasts_all, forecasts_df)
          }
          
          # Build multi-split plot for this model
          try({
               if (!is.null(forecasts_df)) {
                    split_starts <- sapply(cv_splits, function(x) x$test_start)
                    fig_model <- plot_outcome_multisplit(data_single, forecasts_df, split_starts, max_case = 0, which(models == model_type), model_type)
                    if (model_type == "Neural Network") fig_nnet_1 <- fig_model
                    else if (model_type == "ETS") fig_ets_1 <- fig_model
                    else if (model_type == "SARIMA") fig_sarima_1 <- fig_model
                    else if (model_type == "TBATS") fig_tbats_1 <- fig_model
                    else if (model_type == "Hybrid") fig_hyb_1 <- fig_model
                    else if (model_type == "Bayesian structural") fig_baye_1 <- fig_model
               }
          }, silent = TRUE)
          
          # Combine test metrics into a data.frame row: one row per metric (Index)
          df_row <- data.frame(Method = model_type,
                               Index = index_labels,
                               Test_2019 = test_metrics[, 1],
                               Test_2018_2019 = test_metrics[, 2],
                               Test_2017_2019 = test_metrics[, 3])
          fit_goodness <- rbind(fit_goodness, df_row)
     }
     
     # summary table ---------------------------------------------------------
     
     data_table <- fit_goodness |>
          mutate(Method = factor(Method, levels = models, labels = models_label),
                 Test_2019 = round(Test_2019, 2),
                 Test_2018_2019 = round(Test_2018_2019, 2),
                 Test_2017_2019 = round(Test_2017_2019, 2)) |>
          arrange(Method) |>
          select(Method, Test_2019,   Test_2018_2019, Test_2017_2019, Index)
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
          filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_5/", disease_name[i], ".png"),
          fig,
          device = "png",
          width = 14, height = 15,
          limitsize = FALSE,
          dpi = 300
     )
     fit_goodness$disease <- disease_name[i]
     
     # save forecasts with intervals for this disease (long table)
     if (nrow(forecasts_all) > 0) {
          forecasts_all <- forecasts_all |> 
               mutate(disease = disease_name[i]) |> 
               select(disease, Method, split, date, mean, lower_95, lower_80, upper_80, upper_95)
          dir_out <- "../Outcome/Appendix/Forecasts_with_intervals"
          if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
          write.csv(forecasts_all, file = file.path(dir_out, paste0(disease_name[i], "_forecasts.csv")))
     }
     
     return(fit_goodness)
}

# run model ---------------------------------------------------------------

# auto_select_function(37, split_date = split_dates[1], add_value = add_value,
#                      index_labels = index_labels, models = models, models_label = models_label)

number_process <- ifelse(length(disease_name) >= max_proces,
                         max_proces,
                         length(disease_name))

cl <- makeCluster(number_process)
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
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, 1:length(disease_name), auto_select_function,
                     split_date = split_dates[1],
                     cv_splits = cv_splits,
                     add_value = add_value,
                     index_labels = index_labels,
                     models = models,
                     models_label = models_label)
stopCluster(cl)

data_outcome <- do.call("rbind", outcome)
write.xlsx(data_outcome, "../Outcome/Appendix/Model_test_results.xlsx")

# beat model --------------------------------------------------------------

source('./4_b_best_model.R')
