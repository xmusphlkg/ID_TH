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

data_class <- openxlsx::read.xlsx("../Outcome/Appendix/figure_data/fig6.xlsx") |>
     filter(Best == 1) |>
     select(disease, Method) |>
     left_join(select(data_class, Shortname, Group), by = c(disease = "Shortname")) |>
     mutate(disease = factor(disease, levels = data_class$Shortname)) |>
     arrange(disease)
data_class$id <- 1:nrow(data_class)

disease_name <- data_class$disease

# data clean --------------------------------------------------------------

# i <- 1

auto_analysis_function <- function(i) {
     
     set.seed(20240902)
     
     data_single <- data_analysis |>
          filter(Shortname == disease_name[i]) |>
          select(Date, Shortname, Cases) |> 
          rename(date = 'Date',
                 value = 'Cases')
     data_rect <- data.frame(start = c(min(data_single$date), split_dates),
                             end = c(split_dates, max(data_single$date)),
                             label = split_periods) |>
          mutate(m = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01"),
                 label = factor(label, levels = split_periods))
     
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
     
     print(data_class$disease[i])
     print(data_class$Method[i])
     
     if (data_class$Method[i] == "Neural Network") {
          mod <- nnetar(ts_train, lambda = "auto")
          outcome_2 <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome_2$mean)),
                                       mean = as.matrix(outcome_2$mean),
                                       lower_80 = NA,
                                       lower_95 = NA,
                                       upper_80 = NA,
                                       upper_95 = NA)
     }
     
     if (data_class$Method[i] == "ETS") {
          outcome <- forecast(ets(ts_train, ic = "aicc", lambda = "auto"), h = forcast_length)
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                       mean = exp(as.matrix(outcome$mean)),
                                       lower_80 = exp(as.matrix(outcome$lower[, 1])),
                                       lower_95 = exp(as.matrix(outcome$lower[, 2])),
                                       upper_80 = exp(as.matrix(outcome$upper[, 1])),
                                       upper_95 = exp(as.matrix(outcome$upper[, 2])))
     }
     
     if (data_class$Method[i] == "SARIMA") {
          mod <- auto.arima(ts_train, seasonal = T, ic = 'aicc', lambda = 'auto')
          outcome <- forecast(mod, h = forcast_length)
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                       mean = exp(as.matrix(outcome$mean)),
                                       lower_80 = exp(as.matrix(outcome$lower[, 1])),
                                       lower_95 = exp(as.matrix(outcome$lower[, 2])),
                                       upper_80 = exp(as.matrix(outcome$upper[, 1])),
                                       upper_95 = exp(as.matrix(outcome$upper[, 2])))
     }
     
     if (data_class$Method[i] == "TBATS") {
          mod <- tbats(ts_train, seasonal.periods = 12)
          outcome <- forecast(mod, h = forcast_length)
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                       mean = exp(as.matrix(outcome$mean)),
                                       lower_80 = exp(as.matrix(outcome$lower[, 1])),
                                       lower_95 = exp(as.matrix(outcome$lower[, 2])),
                                       upper_80 = exp(as.matrix(outcome$upper[, 1])),
                                       upper_95 = exp(as.matrix(outcome$upper[, 2])))
     }
     
     if (data_class$Method[i] == "Hybrid*") {
          mod <- hybridModel(ts_train,
                             lambda = "auto",
                             models = c("aent"),
                             a.args = list(seasonal = T),
                             weights = "cv.errors",
                             windowSize = 36,
                             parallel = TRUE, num.cores = 10,
                             errorMethod = "RMSE")
          outcome <- forecast(mod, h = forcast_length)
          
          outcome_plot_2 <- data.frame(date = zoo::as.Date(time(outcome$mean)),
                                       mean = exp(as.matrix(outcome$mean)),
                                       lower_80 = exp(as.matrix(outcome$lower[, 1])),
                                       lower_95 = exp(as.matrix(outcome$lower[, 2])),
                                       upper_80 = exp(as.matrix(outcome$upper[, 1])),
                                       upper_95 = exp(as.matrix(outcome$upper[, 2])))
     }
     
     
     if (data_class$Method[i] == "Bayesian structural") {
          ss <- AddLocalLinearTrend(list(), ts_train)
          ss <- AddSeasonal(ss, ts_train, nseasons = 12)
          mod <- bsts(ts_train, state.specification = ss, niter = 1000, seed = 20240902)
          
          burn <- SuggestBurn(0.1, mod)
          outcome <- predict.bsts(mod, horizon = forcast_length, burn = burn, quantiles = c(0.025, 0.1, 0.9, 0.975))
          
          outcome_plot_2 <- data.frame(date = tail(outcome_plot_1$date, forcast_length),
                                       mean = exp(outcome$mean),
                                       lower_80 = exp(outcome$interval[2, ]),
                                       lower_95 = exp(outcome$interval[1, ]),
                                       upper_80 = exp(outcome$interval[3, ]),
                                       upper_95 = exp(outcome$interval[4, ]))
     }
     
     # correct all negative value into zero
     max_value <- max(outcome_plot_2[, 2], max_case, na.rm = T)
     min_value <- min(outcome_plot_2[, 2], na.rm = T)
     
     outcome_plot_2 <- outcome_plot_2 |>
          mutate_at(vars(contains("er")), as.numeric)
     outcome_data <- left_join(outcome_plot_2, outcome_plot_1, by = 'date') |>
          mutate(diff = mean - value,
                 color = if_else(diff > 0, "Decrease", "Increase"))
     
     write.csv(outcome_data,
               paste0("../Outcome/Appendix/Forecast/", data_class$disease[i], ".csv"),
               row.names = F)
     
     return(list(outcome_data = outcome_data,
                 data_rect = data_rect,
                 data_single = data_single,
                 outcome_plot_1 = outcome_plot_1,
                 outcome_plot_2 = outcome_plot_2,
                 max_value = max_value,
                 min_value = min_value,
                 max_case = max_case))
}

# run model ---------------------------------------------------------------


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
     library(paletteer)
     
     Sys.setlocale(locale = "en")
     set.seed(20240902)
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, 1:length(disease_name), auto_analysis_function)
stopCluster(cl)

# panel -------------------------------------------------------------------

plot_breaks <- pretty(c(min_value, max_value, 0))

fig1 <- ggplot() +
     geom_vline(xintercept = data_rect$end, show.legend = F,
                color = "grey", linetype = "longdash") +
     geom_rect(data = data_rect, mapping = aes(xmin = start, xmax = end, fill = label),
               ymax = 0, ymin = max(plot_breaks) / 10, alpha = 0.2, show.legend = F) +
     geom_line(data = outcome_plot_1, mapping = aes(x = date, y = value, colour = "Observed"),
               linewidth = 0.7) +
     geom_line(data = outcome_plot_2, mapping = aes(x = date, y = mean, colour = "Forecasted"),
               linewidth = 0.7) +
     stat_difference(data = outcome_data, mapping = aes(x = date, ymin = value, ymax = mean),
                     alpha = 0.3, levels = c("Decreased", "Increased"), show.legend = F) +
     coord_cartesian(ylim = c(0, NA),
                     xlim = c(split_dates[1] - 365, NA)) +
     scale_x_date(expand = expansion(add = c(0, 0)),
                  date_labels = "%Y",
                  breaks = seq(split_dates[1] - 365, max(outcome_plot_2$date), by = "1 years")) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        breaks = plot_breaks,
                        limits = range(plot_breaks)) +
     scale_color_manual(values = c(Forecasted = "#004F7AFF", Observed = "#CC3D24FF")) +
     scale_fill_manual(values = c(Decreased = "#004F7A50", Increased = "#CC3D2450", back_color)) +
     theme_set() +
     theme(legend.position = "bottom") +
     labs(x = NULL,
          y = ifelse(i %in% c(1, 8, 13, 18), "Monthly cases", ""),
          color = "",
          title = paste0(LETTERS[i], ": ", data_class$disease[i]))

plot <- do.call(wrap_plots, outcome) +
     plot_layout(design = layout, guides = "collect") &
     theme(
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
     )

ggsave(
     "./outcome/publish/fig4.pdf",
     plot,
     family = "Times New Roman",
     limitsize = FALSE,
     device = cairo_pdf,
     width = 25,
     height = 14
)

# merge data file ---------------------------------------------------------

file_list <- paste0("./outcome/appendix/forecast/",
                    datafile_class$disease,
                    ".xlsx")
data_list <- lapply(file_list, read.xlsx, detectDates = T)
names(data_list) <- paste0(LETTERS[1:24], " ", datafile_class$disease)
write.xlsx(data_list,
           file = "./outcome/appendix/Figure Data/Fig.4 data.xlsx")
