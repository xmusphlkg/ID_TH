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

# simulation settings -----------------------------------------------------

# Primary analysis settings (used by manuscript main results)
simulation_settings <- tibble(
     setting_id = "primary",
     n_paths = 1000L,
     bsts_niter = 1000L,
     seed = 20251209L
)

# Optional simulation-time sensitivity runs
run_simulation_sensitivity <- FALSE
if (run_simulation_sensitivity) {
     simulation_settings <- bind_rows(
          simulation_settings,
          tibble(
               setting_id = c("np500", "np5000", "bsts2000"),
               n_paths = c(500L, 5000L, 1000L),
               bsts_niter = c(1000L, 1000L, 2000L),
               seed = c(20251210L, 20251211L, 20251212L)
          )
     )
}

# data load ---------------------------------------------------------------

source("./function/theme_set.R")
source("./function/forecast.R")

load('./temp/month.RData')
appendix_tables_dir <- file.path("..", "Outcome", "Appendix", "Tables")

data_class <- openxlsx::read.xlsx(file.path(appendix_tables_dir, "Best_model_outcome.xlsx")) |>
     filter(Best == 1) |>
     select(disease, Method) |>
     left_join(select(data_class, Shortname, Group), by = c(disease = "Shortname")) |>
     mutate(disease = factor(disease, levels = data_class$Shortname),
            Method = if_else(Method == 'Hybrid**', "Hybrid", Method)) |>
     arrange(disease)
data_class$id <- seq_len(nrow(data_class))

disease_name <- data_class$disease

# data clean --------------------------------------------------------------

auto_analysis_function <- function(i, setting_row) {
     
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
     res <- forecast_model_sim(ts_train = ts_train, h = forcast_length, method = data_class$Method[i],
                               hybrid_parallel = TRUE,
                               hybrid_cores = 10,
                               bsts_niter = setting_row$bsts_niter,
                               n_paths = setting_row$n_paths,
                               seed = setting_row$seed)
     # build outcome_plot_2 using a month sequence starting at the split date
     dates_seq <- seq(split_dates[1], by = 'month', length.out = forcast_length)
     outcome_plot_2 <- data.frame(date = dates_seq,
                                  mean = res$mean,
                                  median = res$median,
                                  Shortname = disease_name[i],
                                  lower_80 = res$lower_80,
                                  lower_95 = res$lower_95,
                                  upper_80 = res$upper_80,
                                  upper_95 = res$upper_95)
     
     # correct all negative value into zero
     max_value <- max(outcome_plot_2$median, max_case, na.rm = T)
     min_value <- min(outcome_plot_2$median, na.rm = T)
     
     outcome_plot_2 <- outcome_plot_2 |>
          mutate_at(vars(contains("er")), as.numeric)
     outcome_data <- left_join(outcome_plot_2, outcome_plot_1, by = c('Shortname', 'date')) |>
          mutate(diff = median - value,
                 color = if_else(diff > 0, "Decrease", "Increase"))
     
     write.csv(outcome_data,
               paste0("../Outcome/Appendix/Forecasts_with_best_model/", setting_row$setting_id, "_", data_class$disease[i], ".csv"),
               row.names = F)
     
     return(list(outcome_data = outcome_data,
                 data_single = data_single,
                 outcome_plot_1 = outcome_plot_1,
                 outcome_plot_2 = outcome_plot_2,
                 MCMC = res$MCMC,
                 simulation_setting = setting_row,
                 max_value = max_value,
                 min_value = min_value,
                 max_case = max_case))
}

# run model ---------------------------------------------------------------
number_process <- ifelse(length(disease_name) >= max_proces,
                         max_proces,
                         length(disease_name))

for (setting_idx in seq_len(nrow(simulation_settings))) {
     setting_row <- as.list(simulation_settings[setting_idx, ])

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
     outcome <- parLapply(cl,
                          seq_along(disease_name),
                          function(idx) auto_analysis_function(idx, setting_row))
     stopCluster(cl)

     if (setting_row$setting_id == "primary") {
          save(outcome, file = "./temp/outcome.RData")
     }

     save(outcome,
          file = paste0("./temp/outcome_", setting_row$setting_id,
                        "_np", setting_row$n_paths,
                        "_bsts", setting_row$bsts_niter,
                        "_seed", setting_row$seed, ".RData"))
}

source('./5_b_visualization.R')
