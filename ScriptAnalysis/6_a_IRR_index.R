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
     left_join(select(data_class, Shortname, Group), by = "Shortname")

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

ggsave("../Outcome/Publish/fig4_a.pdf",
       fig1,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 7.5, height = 10)

ggsave("../Outcome/Publish/fig4_a.png",
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
# - Pre-COVID (Baseline): <= 2019
# - Post-Recovery (Observed): >= 2023 (Actual surveillance data)
# - Post-Recovery (Predicted): >= 2023 (Model predictions / Counterfactual)

# Prepare Observed Data
season_data_obs <- data_month |> 
     filter(Shortname %in% unique(data_outcome$Shortname)) |>
     select(Shortname, year = Year, month = Month, value = Cases, Group) |>
     mutate(date = ymd(paste(year, month, "01", sep = "-")),
            month_label = month(date, label = TRUE, abbr = TRUE),
            Period = case_when(year <= 2019 ~ "Pre-COVID (Observed)",
                               year >= 2023 ~ "Post-Recovery (Observed)",
                               TRUE ~ "Pandemic" # Exclude pandemic years (2020-2022) to focus on the 'new normal'
            )) |> 
     filter(Period != "Pandemic") 

# Prepare Predicted Data (Counterfactual)
season_data_pred <- data_outcome |> 
     filter(year(date) >= 2023) |> 
     mutate(year = year(date),
            month = month(date),
            month_label = month(date, label = TRUE, abbr = TRUE),
            Period = "Post-Recovery (Predicted)",
            value = median) |> 
     select(Shortname, year, month, value, Group, month_label, Period)

# Combine datasets for analysis
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

# Normalize data (0-1) to compare the "shape" of seasonality rather than absolute magnitude
df_monthly_pattern <- df_season_analysis |> 
     group_by(Shortname, Group, Period, month, month_label) |> 
     summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") |> 
     group_by(Shortname, Period) |> 
     mutate(
          # Min-Max Normalization: (x - min) / (max - min)
          # This ensures the peak is always 1.0, making phase shifts visually obvious
          norm_value = (avg_value - min(avg_value)) / (max(avg_value) - min(avg_value)),
          Period = factor(Period, levels = c("Pre-COVID (Observed)",
                                              "Post-Recovery (Observed)",
                                              "Post-Recovery (Predicted)"))
     ) |> 
     ungroup() |> 
     make_closed_data()

# 3. Peak Month Calculation and Phase Shift Analysis
peak_shift_table <- df_monthly_pattern |> 
     group_by(Shortname, Period) |> 
     slice_max(avg_value, n = 1, with_ties = FALSE) |> # Identify the peak month
     select(Shortname, Group, Period, peak_month = month) |> 
     pivot_wider(names_from = Period, values_from = peak_month) |> 
     mutate(# Calculate Shift: Observed 2023 vs Baseline 2019
          Shift_Obs_vs_Pre = `Post-Recovery (Observed)` - `Pre-COVID (Observed)`,
          
          # Calculate Shift: Observed 2023 vs Predicted (Is the shift due to pandemic or natural trend?)
          Shift_Obs_vs_Pred = `Post-Recovery (Observed)` - `Post-Recovery (Predicted)`,
          
          # Correct for circular nature of months (e.g., Jan (1) vs Dec (12) is a 1-month shift, not -11)
          Shift_Corrected = case_when(Shift_Obs_vs_Pre > 6 ~ Shift_Obs_vs_Pre - 12,
                                      Shift_Obs_vs_Pre < -6 ~ Shift_Obs_vs_Pre + 12,
                                      TRUE ~ Shift_Obs_vs_Pre))

i <- 1

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
     
     plot_table <- peak_shift_table |> 
          filter(Shortname %in% disease_names) |> 
          select(-contains('Shift')) |> 
          pivot_longer(cols = c(`Pre-COVID (Observed)`, `Post-Recovery (Observed)`, `Post-Recovery (Predicted)`),
                       names_to = "Period", values_to = "peak_month") |> 
          left_join(plot_data, by = c("Shortname", "Period", "peak_month" = "month"))
     
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
          geom_point(data = plot_table, aes(x = peak_month, y = norm_value),
                     size = 2, alpha = 0.5, show.legend = F) +
          # Polar Coordinates
          coord_polar(start = 0) + 
          scale_x_continuous(breaks = 1:12, 
                             labels = month.abb, 
                             limits = c(1, 13)) +
          
          # Y-axis
          scale_y_continuous(labels = NULL, breaks = NULL) + 
          scale_color_manual(values = c("Pre-COVID (Observed)" = "#7E6148FF",
                                        "Post-Recovery (Observed)" = "#E64B35FF",
                                        "Post-Recovery (Predicted)" = "#91D1C2FF")) +
          scale_fill_manual(values = c("Pre-COVID (Observed)" = "#7E6148FF",
                                       "Post-Recovery (Observed)" = "#E64B35FF",
                                       "Post-Recovery (Predicted)" = "#91D1C2FF")) +
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
          labs(title = paste0(int2col(i+4), ": ", disease_names))
     
     return(fig_radar)
}

# Generate plots for all groups
fig_season_list <- lapply(seq_along(data_class$Shortname), plot_seasonality)

# Combine plots using patchwork
fig_season_all <- wrap_plots(fig_season_list, ncol = 6, guides = 'collect') & 
     theme(legend.position = "bottom")

# Save the visualization
ggsave("../Outcome/Publish/fig4_b.pdf", 
       fig_season_all, 
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 11)

ggsave("../Outcome/Publish/fig4_b.png", 
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
           file = "../Outcome/Publish/figure_data/fig4.xlsx")
