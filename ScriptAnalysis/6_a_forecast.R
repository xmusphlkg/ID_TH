# packages ----------------------------------------------------------------

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
library(ggh4x)
library(paletteer)
library(doParallel)

Sys.setlocale(locale = "en")
set.seed(20240902)

remove(list = ls())

# data load ---------------------------------------------------------------

source("./function/theme_set.R")
source("./function/forecast.R")

load('./month.RData')

data_class <- openxlsx::read.xlsx("../Outcome/Appendix/Best_model_outcome.xlsx") |>
     filter(Best == 1) |>
     select(disease, Method) |>
     left_join(select(data_class, Shortname, Group), by = c(disease = "Shortname")) |>
     mutate(disease = factor(disease, levels = data_class$Shortname),
            Method = if_else(Method == 'Hybrid**', "Hybrid", Method)) |>
     arrange(disease)
data_class$id <- 1:nrow(data_class)

disease_name <- data_class$disease

# data clean --------------------------------------------------------------

i <- 19

auto_analysis_function <- function(i) {
     
     set.seed(20240902)
     
     data_single <- data_month |>
          filter(Shortname == disease_name[i]) |>
          select(Date, Shortname, Cases) |> 
          rename(date = 'Date',
                 value = 'Cases')
     
     ## setting training data
     ts_train <- data_single |>
          filter(date < split_dates[1]) |>
          pull(value) |> 
          ts(frequency = 12,
             start = c(as.numeric(format(min(data_single$date), "%Y")), as.numeric(format(min(data_single$date), "%m"))))
     
     ts_train <- log(ts_train + add_value)
     
     ## setting the real data
     outcome_plot_1 <- data_single |>
          filter(date >= split_dates[1] - 365) |>
          as.data.frame()
     max_case <- max(outcome_plot_1$value)
     forcast_length <- length(seq(split_dates[1], max(data_single$date), by = "month"))
     
     # Select Method ------------------------------------------------------------
     
     # print(data_class$disease[i])
     # print(data_class$Method[i])
     
     # centralized forecasting helper returns mean and interval vectors (on original scale)
     res <- forecast_model_ts(ts_train = ts_train, h = forcast_length, method = data_class$Method[i],
                              hybrid_parallel = TRUE, hybrid_cores = 10, bsts_niter = 1000, seed = 20240902)
     # build outcome_plot_2 using a month sequence starting at the split date
     dates_seq <- seq(split_dates[1], by = 'month', length.out = forcast_length)
     outcome_plot_2 <- data.frame(date = dates_seq,
                                  mean = res$mean,
                                  Shortname = disease_name[i],
                                  lower_80 = res$lower_80,
                                  lower_95 = res$lower_95,
                                  upper_80 = res$upper_80,
                                  upper_95 = res$upper_95)
     
     # correct all negative value into zero
     max_value <- max(outcome_plot_2[, 2], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[, 2], na.rm = T)
     
     outcome_plot_2 <- outcome_plot_2 |>
          mutate_at(vars(contains("er")), as.numeric)
     outcome_data <- left_join(outcome_plot_2, outcome_plot_1, by = 'date') |>
          mutate(diff = mean - value,
                 color = if_else(diff > 0, "Decrease", "Increase"))
     
     write.csv(outcome_data,
               paste0("../Outcome/Appendix/Forecasts_with_best_model/", data_class$disease[i], ".csv"),
               row.names = F)
     
     return(list(outcome_data = outcome_data,
                 data_single = data_single,
                 outcome_plot_1 = outcome_plot_1,
                 outcome_plot_2 = outcome_plot_2,
                 max_value = max_value,
                 min_value = min_value,
                 max_case = max_case))
}

# run model ---------------------------------------------------------------
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
     library(paletteer)
     
     Sys.setlocale(locale = "en")
     set.seed(20240902)
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, 1:length(disease_name), auto_analysis_function)
stopCluster(cl)

save(outcome, file = "./outcome.RData")

source('./6_b_visualization.R')
