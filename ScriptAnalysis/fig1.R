#####################################
## Fig 1 - Example forecasting panels for the analytical engine
## Output: ../Outcome/Publish/npjDM/fig1_panels/*.png
#####################################

library(tidyverse)
library(openxlsx)

Sys.setlocale("LC_TIME", "C")
remove(list = ls())

source("./function/theme_set.R")

# ============================================================
# User-editable settings
# ============================================================

example_disease <- "Dengue fever"
example_split <- "2019"
comparison_history_months <- 24L
pi_history_months <- 24L
pi_forecast_months_to_show <- 12L

out_dir <- "../Outcome/Publish/npjDM"
panel_dir <- file.path(out_dir, "fig1_panels")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(panel_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# Design system
# ============================================================

COL_OBS <- "#22313F"
COL_TEST <- "#CC3D24"
COL_FC <- "#0B6E69"
COL_FC_PI <- "#07534E"
COL_SHADE <- "#F4E8DA"
COL_MUTED <- "#7A7A7A"
COL_PI95 <- "#CFE7E3"
COL_PI80 <- "#79B8B0"

model_label_map <- c(
     "Neural Network" = "NNAR",
     "ETS" = "ETS",
     "SARIMA" = "SARIMA",
     "TBATS" = "TBATS",
     "Hybrid" = "Hybrid",
     "Bayesian structural" = "BSTS",
     "ARIMA + Fourier" = "ARIMA+Fourier"
)

model_slug_map <- c(
     "Neural Network" = "nnar",
     "ETS" = "ets",
     "SARIMA" = "sarima",
     "TBATS" = "tbats",
     "Hybrid" = "hybrid",
     "Bayesian structural" = "bsts",
     "ARIMA + Fourier" = "arima_fourier"
)

strip_model_stars <- function(x) {
     stringr::str_replace_all(x, "\\*+", "")
}

resolve_model_label <- function(x) {
     y <- strip_model_stars(x)
     dplyr::coalesce(unname(model_label_map[y]), y)
}

safe_file_label <- function(x) {
     y <- strip_model_stars(x)
     out <- unname(model_slug_map[y])
     ifelse(is.na(out),
            stringr::str_replace_all(stringr::str_to_lower(y), "[^a-z0-9]+", "_"),
            out)
}

axis_labeller <- function(x) {
     if (max(x, na.rm = TRUE) >= 1000) {
          return(scales::label_number(big.mark = ",")(x))
     }

     scales::label_number(accuracy = 1)(x)
}

panel_theme <- function(base_size = 10) {
     theme_bw(base_size = base_size) +
          theme(
               panel.grid = element_blank(),
               legend.position = "none",
               axis.title = element_blank(),
               axis.text.x = element_text(color = "black", size = base_size - 1),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               plot.title = element_blank(),
               plot.subtitle = element_blank(),
               plot.margin = margin(5, 6, 5, 18)
          )
}

# ============================================================
# Data
# ============================================================

load("./temp/month.RData")

best_model_path <- file.path("..", "Outcome", "Appendix", "Tables", "Best_model_outcome.xlsx")
cv_forecast_path <- file.path("..", "Outcome", "Appendix", "Forecasts_with_intervals", paste0(example_disease, "_forecasts.csv"))
best_forecast_path <- file.path("..", "Outcome", "Appendix", "Forecasts_with_best_model", paste0("primary_", example_disease, ".csv"))

required_paths <- c(best_model_path, cv_forecast_path, best_forecast_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
     stop("Required forecast outputs are missing: ", paste(missing_paths, collapse = "; "))
}

observed_all <- data_month |>
     filter(Shortname == example_disease) |>
     transmute(date = as.Date(Date), value = Cases) |>
     arrange(date) |>
     distinct()

if (nrow(observed_all) == 0) {
     stop("Disease not found in temp/month.RData: ", example_disease)
}

best_model_table <- read.xlsx(best_model_path) |>
     mutate(Method = strip_model_stars(Method))

best_method <- best_model_table |>
     filter(disease == example_disease, Best == 1) |>
     arrange(desc(Index)) |>
     slice_head(n = 1) |>
     pull(Method)

if (length(best_method) == 0 || is.na(best_method[1])) {
     stop("Unable to resolve the best model for ", example_disease)
}

best_method <- best_method[1]

cv_forecasts <- readr::read_csv(cv_forecast_path, show_col_types = FALSE) |>
     mutate(
          date = as.Date(date),
          Method = strip_model_stars(Method)
     ) |>
     filter(split == example_split) |>
     mutate(Method = factor(Method, levels = models)) |>
     arrange(Method, date)

if (nrow(cv_forecasts) == 0) {
     stop("Split not found in Forecasts_with_intervals: ", example_split)
}

best_forecast <- readr::read_csv(best_forecast_path, show_col_types = FALSE) |>
     mutate(date = as.Date(date)) |>
     arrange(date)

comparison_start <- min(cv_forecasts$date)
comparison_end <- max(cv_forecasts$date)
comparison_window_start <- comparison_start %m-% months(comparison_history_months)

observed_comparison <- observed_all |>
     filter(date >= comparison_window_start, date <= comparison_end)

observed_train <- observed_comparison |>
     filter(date < comparison_start)

observed_test <- observed_comparison |>
     filter(date >= comparison_start)

comparison_y_breaks <- pretty(
     c(0, observed_comparison$value, cv_forecasts$mean),
     n = 4
)
comparison_y_limits <- c(0, max(comparison_y_breaks, na.rm = TRUE))

pi_start <- min(best_forecast$date)
pi_end <- pi_start %m+% months(pi_forecast_months_to_show - 1L)
pi_window_start <- pi_start %m-% months(pi_history_months)

best_forecast_display <- best_forecast |>
     filter(date >= pi_start, date <= pi_end)

observed_pi <- observed_all |>
     filter(date >= pi_window_start, date <= pi_end)

pi_y_breaks <- pretty(
     c(0, observed_pi$value, best_forecast_display$upper_95),
     n = 5
)
pi_y_limits <- c(0, max(pi_y_breaks, na.rm = TRUE))

# ============================================================
# Plot builders
# ============================================================

build_model_panel <- function(method_name) {
     forecast_df <- cv_forecasts |>
          filter(Method == method_name)

     if (nrow(forecast_df) == 0) {
          return(NULL)
     }

     ggplot() +
          annotate(
               "rect",
               xmin = comparison_start,
               xmax = comparison_end + 15,
               ymin = -Inf,
               ymax = Inf,
               fill = COL_SHADE,
               alpha = 1
          ) +
          geom_line(
               data = observed_train,
               aes(x = date, y = value),
               color = COL_OBS,
               linewidth = 0.7
          ) +
          geom_line(
               data = observed_test,
               aes(x = date, y = value),
               color = COL_TEST,
               linewidth = 0.75
          ) +
          geom_point(
               data = observed_test,
               aes(x = date, y = value),
               color = COL_TEST,
               size = 1.1
          ) +
          geom_line(
               data = forecast_df,
               aes(x = date, y = mean),
               color = COL_FC,
               linewidth = 0.85
          ) +
          geom_vline(
               xintercept = comparison_start,
               linetype = "22",
               linewidth = 0.4,
               color = COL_MUTED
          ) +
          coord_cartesian(
               xlim = c(comparison_window_start, comparison_end),
               ylim = comparison_y_limits,
               expand = FALSE
          ) +
          scale_x_date(
               date_labels = "%Y",
               breaks = seq(floor_date(comparison_window_start, "year"), comparison_end, by = "1 year"),
               expand = expansion(mult = c(0, 0.02))
          ) +
          scale_y_continuous(
               breaks = comparison_y_breaks,
               labels = axis_labeller,
               expand = c(0, 0)
          ) +
          panel_theme()
}

build_pi_panel <- function() {
     forecast_label <- paste0("Counterfactual median (", resolve_model_label(best_method), ")")

     ggplot() +
          annotate(
               "rect",
               xmin = pi_start,
               xmax = pi_end + 15,
               ymin = -Inf,
               ymax = Inf,
               fill = COL_SHADE,
               alpha = 1
          ) +
          geom_line(
               data = observed_pi,
               aes(x = date, y = value, color = "Observed"),
               linewidth = 0.9
          ) +
          geom_ribbon(
               data = best_forecast_display,
               aes(x = date, ymin = lower_95, ymax = upper_95, fill = "95% PI"),
               alpha = 1
          ) +
          geom_ribbon(
               data = best_forecast_display,
               aes(x = date, ymin = lower_80, ymax = upper_80, fill = "80% PI"),
               alpha = 1
          ) +
          geom_line(
               data = best_forecast_display,
               aes(x = date, y = median, color = forecast_label),
               linewidth = 0.95
          ) +
          geom_vline(
               xintercept = pi_start,
               linetype = "22",
               linewidth = 0.45,
               color = COL_MUTED
          ) +
          coord_cartesian(
               xlim = c(pi_window_start, pi_end),
               ylim = pi_y_limits,
               expand = FALSE
          ) +
          scale_x_date(
               date_labels = "%Y",
               breaks = seq(floor_date(pi_window_start, "year"), pi_end, by = "1 year"),
               expand = expansion(mult = c(0, 0.02))
          ) +
          scale_y_continuous(
               breaks = pi_y_breaks,
               labels = axis_labeller,
               expand = c(0, 0)
          ) +
          scale_color_manual(
               values = c(
                    "Observed" = COL_TEST,
                    setNames(COL_FC_PI, forecast_label)
               ),
               name = NULL
          ) +
          scale_fill_manual(
               values = c("95% PI" = COL_PI95, "80% PI" = COL_PI80),
               name = NULL
          ) +
          labs(
               x = NULL,
               y = NULL
          ) +
          theme_bw(base_size = 11) +
          theme(
               panel.grid = element_blank(),
               legend.position = "none",
               legend.key = element_rect(fill = "transparent", color = NA),
               axis.title = element_blank(),
               axis.text.x = element_text(color = "black"),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               plot.title = element_blank(),
               plot.subtitle = element_blank(),
               plot.margin = margin(6, 6, 6, 20)
          )
}

# ============================================================
# Build panels
# ============================================================

model_panels <- purrr::map(models, build_model_panel)
names(model_panels) <- models
model_panels <- Filter(Negate(is.null), model_panels)

pi_panel <- build_pi_panel()

# ============================================================
# Save outputs
# ============================================================

purrr::iwalk(model_panels, function(plot_obj, method_name) {
     slug <- safe_file_label(method_name)

     ggsave(
          filename = file.path(panel_dir, paste0("fig1_", slug, ".png")),
          plot = plot_obj,
          width = 3.2,
          height = 2.5,
          dpi = 300
     )
})

ggsave(
     filename = file.path(panel_dir, "fig1_prediction_interval.png"),
     plot = pi_panel,
     width = 8.6,
     height = 4.2,
     dpi = 300
)

message("fig1 panels saved to ", panel_dir)