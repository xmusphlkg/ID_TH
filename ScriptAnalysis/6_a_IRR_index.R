# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)
library(ggh4x)
library(ggnewscale)

Sys.setlocale("LC_TIME", "C")

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load('./temp/month.RData')
load('./temp/outcome.RData')
load('./temp/best_model_figure.RData')

data_class <- data_class |> 
     mutate(label = paste0(int2col(id + 1), ": ", Shortname))

# panel IRR ---------------------------------------------------------------

data_outcome <- lapply(seq(nrow(data_class)), function(i) outcome[[i]]$outcome_data) |> 
     bind_rows() |> 
     mutate(diff_percent = round((value + add_value) / (median + add_value), 3),
            diff = round(median - value, 0),
            label = if_else(diff_percent > 10, "*", ""),
            diff_visual = if_else(diff_percent > 10, 10, diff_percent),
            date_num = format(ymd(date), "%Y.%m")) |> 
     left_join(select(data_class, Shortname, Group), by = "Shortname") |> 
     select(Shortname, date, date_num, value, median, diff, diff_percent, label, diff_visual, Group)

disease_groups_select <- unique(data_outcome$Group)

g <- 4

plot_group_panel <- function(g){
     # read data of multiple diseases from outcome
     disease_id <- data_class$id[data_class$Group == disease_groups[g]]
     disease_name <- data_class$Shortname[data_class$Group == disease_groups[g]]
     
     data_group <- data_outcome |> 
          filter(Shortname %in% disease_name) |>
          mutate(Shortname = factor(Shortname, levels = disease_name))
     
     fig_group <- ggplot(data = data_group,
                         mapping = aes(fill = diff_visual, x = date_num, y = Shortname)) +
          geom_tile() +
          geom_text(mapping = aes(label = label), vjust = 0.5) +
          coord_cartesian(ratio = 2)+
          scale_fill_gradientn(colors = paletteer_d("LaCroixColoR::Lemon", direction = -1)[-1],
                               trans = log_fill,
                               breaks = c(0, 0.5, 1, 2, 5, 10),
                               labels = c('0', '0.5', '1.0', '2.0', '5.0', '>10.0'),
                               limits = c(0, 10)) +
          scale_x_discrete(breaks = paste(unique(year(data_group$date)), "01", sep = "."),
                           labels = unique(year(data_group$date)),
                           expand = expansion(add = c(0, 0))) +
          scale_y_discrete(limits = rev(disease_name),
                           expand = c(0, 0)) +
          theme_bw() +
          theme(legend.position = "right",
                plot.title = element_text(size = 14, hjust = 0, vjust = 0.5, face = "bold"),
                plot.title.position = "plot") +
          guides(fill = guide_colourbar(barwidth = 1, barheight = 13, title.position = "top")) +
          labs(x = 'Date',
               y = NULL,
               fill = "IRR",
               title = paste0(LETTERS[g], ": ", disease_groups_select[g]))
     
     return(fig_group)
}

fig_list <- lapply(seq_along(disease_groups_select), plot_group_panel)

fig1 <- wrap_plots(fig_list, ncol = 1, guides = 'collect')

ggsave("../Outcome/Publish/fig3_a.pdf",
       fig1,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 7.5, height = 10)

ggsave("../Outcome/Publish/fig3_a.png",
       fig1,
       limitsize = FALSE,
       width = 7.5, height = 10)

data_save <- data_outcome |> 
     select(Shortname, date, value, median, diff, diff_percent) |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |> 
     # split by group
     group_split(Group)

# Disruption of Seasonality -----------------------------------------------

# 1. Data Preparation: Define periods and extract temporal features
# We compare three scenarios:
# Compare three scenarios:
# - Pre-COVID (Observed): <= 2019
# - Post-PHSM (Observed): >= 2023 (Observed surveillance data; not necessarily recovered for all diseases)
# - Post-PHSM (Predicted): >= 2023 (Counterfactual median forecast)

season_data_obs <- data_month |> 
     filter(Shortname %in% unique(data_outcome$Shortname)) |>
     select(Shortname, year = Year, month = Month, value = Cases, Group) |>
     mutate(
          date = ymd(paste(year, month, "01", sep = "-")),
          month_label = month(date, label = TRUE, abbr = TRUE),
          Period = case_when(
               year <= 2019 ~ "Pre-COVID (Observed)",
               year >= 2023 ~ "Post-PHSM (Observed)",
               TRUE ~ "Pandemic"
          )
     ) |> 
     filter(Period != "Pandemic")

season_data_pred <- data_outcome |> 
     filter(year(date) >= 2023) |> 
     mutate(
          year = year(date),
          month = month(date),
          month_label = month(date, label = TRUE, abbr = TRUE),
          Period = "Post-PHSM (Predicted)",
          value = median
     ) |> 
     select(Shortname, year, month, value, Group, month_label, Period)

df_season_analysis <- bind_rows(season_data_obs, season_data_pred)

# 2. Aggregation and Normalization
# This duplicates Month 1 data as "Month 13" to close the gap in the radar chart
make_closed_data <- function(data) {
     data |>
          # Group by the distinct lines we are drawing
          group_split(Shortname, Period) |>
          map_dfr(function(df) {
               # Extract Month 1 data
               month1 <- df |> filter(month == 1)
               # Duplicate it as Month 13
               month13 <- month1 |> mutate(month = 13)
               # Combine original data with the new Month 13
               bind_rows(df, month13)
          })
}

df_monthly_mean <- df_season_analysis |> 
     group_by(Shortname, Group, Period, month, month_label) |> 
     summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

# Shape (0-1) normalization to compare seasonality shape only
df_monthly_pattern <- df_monthly_mean |> 
     group_by(Shortname, Period) |> 
     mutate(
          norm_value = (avg_value - min(avg_value)) / (max(avg_value) - min(avg_value)),
          Period = factor(Period, levels = c("Pre-COVID (Observed)",
                                             "Post-PHSM (Observed)",
                                             "Post-PHSM (Predicted)"))
     ) |> 
     ungroup() |> 
     make_closed_data()

# Amplitude metrics to quantify strength of seasonality (not just shape)
df_season_amplitude <- df_monthly_mean |>
     group_by(Shortname, Group, Period) |>
     summarise(
          mean_monthly = mean(avg_value, na.rm = TRUE),
          peak = max(avg_value, na.rm = TRUE),
          trough = min(avg_value, na.rm = TRUE),
          peak_to_trough = peak / pmax(trough, 1e-6),
          amplitude_ratio = (peak - trough) / pmax(mean_monthly, 1e-6),
          .groups = "drop"
     ) |>
     mutate(
          Period = factor(Period, levels = c("Pre-COVID (Observed)",
                                             "Post-PHSM (Observed)",
                                             "Post-PHSM (Predicted)"))
     )

# 3. Peak Month Calculation and Phase Shift Analysis
# 1) Max-month peak (your original approach)
get_peak_month_max <- function(df) {
     df |> slice_max(avg_value, n = 1, with_ties = FALSE) |> pull(month)
}

# 2) Centre-of-mass month (robust to broad/double peaks)
# Uses circular mean of months weighted by avg_value
get_peak_month_com <- function(df) {
     m <- df$month
     w <- df$avg_value
     # map months to angles on unit circle
     theta <- 2 * pi * (m - 1) / 12
     x <- sum(w * cos(theta), na.rm = TRUE)
     y <- sum(w * sin(theta), na.rm = TRUE)
     ang <- atan2(y, x)
     # convert angle back to month 1..12
     m_com <- (round((ang * 12) / (2 * pi)) + 1)
     # ensure 1..12
     m_com <- ((m_com - 1) %% 12) + 1
     m_com
}

# Circular shift correction: map shifts into [-6, +6]
correct_circular_shift <- function(shift) {
     dplyr::case_when(
          shift > 6 ~ shift - 12,
          shift < -6 ~ shift + 12,
          TRUE ~ shift
     )
}

peak_shift_table <- df_monthly_mean |>
     mutate(Period_key = case_when(
          Period == "Pre-COVID (Observed)" ~ "pre_obs",
          Period == "Post-PHSM (Observed)" ~ "post_obs",
          Period == "Post-PHSM (Predicted)" ~ "post_pred",
          TRUE ~ NA_character_)) |>
     filter(!is.na(Period_key)) |>
     group_by(Shortname, Group, Period_key) |>
     summarise(peak_month_max = get_peak_month_max(cur_data()),
               peak_month_com = get_peak_month_com(cur_data()),
               .groups = "drop") |>
     pivot_wider(names_from = Period_key,
                 values_from = c(peak_month_max, peak_month_com))|>
     mutate(
          # COM peak (primary)
          Shift_Obs_vs_Pre_com   = peak_month_com_post_obs - peak_month_com_pre_obs,
          Shift_Obs_vs_Pred_com  = peak_month_com_post_obs - peak_month_com_post_pred,
          Shift_Corrected_com    = correct_circular_shift(Shift_Obs_vs_Pre_com),
          Shift_CorrectedPred_com = correct_circular_shift(Shift_Obs_vs_Pred_com),
          
          # Max peak (sensitivity / also for figure points)
          Shift_Obs_vs_Pre_max   = peak_month_max_post_obs - peak_month_max_pre_obs,
          Shift_Obs_vs_Pred_max  = peak_month_max_post_obs - peak_month_max_post_pred,
          Shift_Corrected_max    = correct_circular_shift(Shift_Obs_vs_Pre_max),
          Shift_CorrectedPred_max = correct_circular_shift(Shift_Obs_vs_Pred_max)
     )


i <- 1

print(peak_shift_table, n = Inf)

print(df_season_amplitude, n = Inf)

# 4. Visualization: Radar Chart (Polar Coordinates)

# Rainy season data
plot_rainy <- data.frame(month = 1:13,
                         ymin = 0,
                         ymax = 1) |> 
     mutate(season = case_when(month %in% 5:10 ~ "Rainy season",
                               TRUE ~ "Dry season"))

plot_seasonality <- function(i) {
     # Select diseases for the current group
     disease_names <- data_class[i, "Shortname"]
     
     plot_data <- df_monthly_pattern |> 
          filter(Shortname %in% disease_names) |> 
          # Ensure data is sorted by month for correct polygon drawing
          arrange(Shortname, Period, month) |> 
          mutate(Shortname = factor(Shortname, levels = disease_names))
     
     # peak points using centre-of-mass definition (primary)
     
     plot_amplitude <- df_season_amplitude |>
          filter(Shortname %in% disease_names,
                 amplitude_ratio > 0.5)
     
     plot_table <- peak_shift_table |>
          filter(Shortname %in% disease_names) |>
          pivot_longer(cols = -c(Shortname, Group),
                       names_to = "key",
                       values_to = "peak_month") |>
          mutate(peak_type = case_when(str_detect(key, "^peak_month_max_") ~ "Max peak",
                                       str_detect(key, "^peak_month_com_") ~ "Phase centre (COM)",
                                       TRUE ~ NA_character_),
                 Period_key = case_when(str_detect(key, "_pre_obs$")   ~ "Pre-COVID (Observed)",
                                        str_detect(key, "_post_obs$")  ~ "Post-PHSM (Observed)",
                                        str_detect(key, "_post_pred$") ~ "Post-PHSM (Predicted)",
                                        TRUE ~ NA_character_)) |>
          filter(!is.na(peak_type), !is.na(Period_key)) |>
          rename(Period = Period_key) |> 
          left_join(select(plot_data, Period, month, norm_value),
                    by = c("Period", 'peak_month' = 'month')) |> 
          # drop weak seasonality com peaks
          filter(!(peak_type == "Phase centre (COM)" & Shortname %in% plot_amplitude$Shortname == FALSE)) |> 
          mutate(peak_type = factor(peak_type, levels = c("Max peak", "Phase centre (COM)")))
     
     # Create plot
     fig_radar <- ggplot(plot_data, aes(x = month, y = norm_value, color = Period, fill = Period)) +
          # add rainy season shading
          geom_ribbon(data = plot_rainy, aes(x = month, ymin = ymin, ymax = ymax, fill = season),
                      inherit.aes = FALSE) +
          scale_fill_manual(values = c("Rainy season" = "lightblue", "Dry season" = "lightyellow"),
                            name = 'Season') +
          new_scale_fill() +
          # add lines and area
          geom_area(position = "identity", alpha = 0.1, linewidth = 0.5) + 
          # add points at peak months
          geom_point(data = plot_table,
                     aes(x = peak_month, y = norm_value, shape = peak_type),
                     size = 2.2, alpha = 0.5, show.legend = T) +
     
          # Polar Coordinates
          coord_polar(start = 0) + 
          scale_x_continuous(breaks = 1:12, 
                             labels = month.abb, 
                             limits = c(1, 13)) +
          scale_shape_manual(values = c("Max peak" = 16, "Phase centre (COM)" = 17),
                             drop = F) +
          
          # Y-axis
          scale_y_continuous(labels = NULL, breaks = NULL) + 
          scale_color_manual(values = c("Pre-COVID (Observed)" = "#7E6148FF",
                                        "Post-PHSM (Observed)" = "#E64B35FF",
                                        "Post-PHSM (Predicted)" = "#91D1C2FF")) +
          scale_fill_manual(values = c("Pre-COVID (Observed)" = "#7E6148FF",
                                       "Post-PHSM (Observed)" = "#E64B35FF",
                                       "Post-PHSM (Predicted)" = "#91D1C2FF"))+
          theme_minimal() +
          theme(
               axis.title = element_blank(),
               strip.text = element_text(face = "bold", size = 11),
               legend.position = "bottom",
               # Adjust grid lines for aesthetics
               panel.grid.major.x = element_line(color = "grey80", linetype = "dotted"),
               panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
               plot.title = element_text(face = "bold", hjust = 0)
          ) +
          labs(title = paste0(int2col(i+4), ": ", disease_names),
               color = "Period",
               shape = "Peak type")
     
     return(fig_radar)
}

# Generate plots for all groups
fig_season_list <- lapply(seq_along(data_class$Shortname), plot_seasonality)

# Combine plots using patchwork
fig_season_all <- wrap_plots(fig_season_list, ncol = 6, guides = 'collect') & 
     theme(legend.position = "bottom")

# Save the visualization
ggsave("../Outcome/Publish/fig3_b.pdf", 
       fig_season_all, 
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 11)

ggsave("../Outcome/Publish/fig3_b.png", 
       fig_season_all, 
       limitsize = FALSE,
       width = 14, height = 11)

data_save_disease <- df_monthly_pattern |> 
     select(Shortname, Period, month, avg_value, norm_value) |> 
     mutate(Shortname = factor(Shortname, levels = data_class$Shortname)) |>
     arrange(Shortname, Period, month) |> 
     group_split(Shortname)

data_save <- append(data_save,
                    data_save_disease)

names(data_save) <- int2col(seq_along(data_save))

write.xlsx(as.list(data_save),
           file = "../Outcome/Publish/figure_data/fig3.xlsx")
