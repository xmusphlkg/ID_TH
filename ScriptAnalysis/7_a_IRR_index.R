# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)
library(ggh4x)

Sys.setenv("LANGUAGE"="en")

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load('./month.RData')
load('./outcome.RData')
load('./best_model_figure.RData')

data_class <- data_class |> 
     mutate(label = paste0(int2col(id + 1), ": ", Shortname))

# panel group cases ----------------------------------------------------

outcome_meta <- tibble(list_index = seq_along(outcome),
                       Shortname  = map_chr(outcome, ~ unique(.x$data_single$Shortname)[1]))

group_map <- outcome_meta |> 
     inner_join(data_class |> select(Shortname, Group), by = "Shortname") |> 
     ungroup()
data_group_list <- split(group_map, group_map$Group, drop = T)

data_group_outcome_plot_2 <- imap_dfr(data_group_list, function(curr_data, grp_name) {
     
     # A. Retrieve MCMC matrices for this group
     # Extract the indices from the current group's dataframe
     target_indices <- curr_data$list_index
     mcmc_list <- map(target_indices, ~ outcome[[.x]]$MCMC)
     
     # B. Matrix Aggregation
     # Sum all matrices element-wise
     total_mcmc <- Reduce(`+`, mcmc_list)
     
     # C. Extract Date Vector
     # Get date from the first disease in this group
     ref_idx <- target_indices[1]
     # Robust date extraction: try outcome_plot_2 first, then data_single
     date_vec <- outcome[[ref_idx]]$outcome_plot_2$date
     if (is.null(date_vec) || length(date_vec) != nrow(total_mcmc)) {
          date_vec <- outcome[[ref_idx]]$data_single$date[1:nrow(total_mcmc)]
     }
     
     # D. Calculate Statistics
     # Using the group name (.y) passed automatically by imap
     tibble(
          Group    = grp_name,  # Here is the group name you wanted!
          date     = date_vec,
          median   = apply(total_mcmc, 1, quantile, probs = 0.500, na.rm = TRUE),
          lower_95 = apply(total_mcmc, 1, quantile, probs = 0.025, na.rm = TRUE),
          upper_95 = apply(total_mcmc, 1, quantile, probs = 0.975, na.rm = TRUE)
     )
})

# observed cases
data_group_outcome_plot_1 <- lapply(seq(length(data_class$Shortname)), function(i) outcome[[i]]$outcome_plot_1) |> 
     bind_rows() |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |>
     group_by(Group, date) |>
     summarise(value = sum(value),
               .groups = 'drop')

data_group_outcome <- data_group_outcome_plot_2 |> 
     left_join(data_group_outcome_plot_1, by = c("Group", "date")) |> 
     mutate(diff_percent = round((value + add_value) / (median + add_value), 3),
            diff = round(median - value, 0),
            label = if_else(diff_percent > 10, "*", ""),
            diff_visual = if_else(diff_percent > 10, 10, diff_percent),
            date_num = format(ymd(date), "%Y.%m"))

## prepare data for visualization
data_group_outcome_list <- lapply(seq(length(data_group_list)), function(i) {
     group_name <- unique(data_group_outcome$Group)[i]
     
     outcome_plot_2  <- data_group_outcome_plot_2 |>
          filter(Group == group_name)
     outcome_plot_1 <- data_group_outcome_plot_1 |>
          filter(Group == group_name)
     outcome_data <- data_group_outcome |> 
          filter(Group == group_name) |> 
          rename(Shortname = 'Group')
     
     max_case <- max(c(outcome_data$median, outcome_data$value), na.rm = T)
     min_case <- min(c(outcome_data$median, outcome_data$value), na.rm = T)
     max_value <- max_case + 0.1 * (max_case - min_case)
     return(list(outcome_data = outcome_data,
                 outcome_plot_1 = outcome_plot_1,
                 outcome_plot_2 = outcome_plot_2,
                 max_value = max_value,
                 min_case = min_case,
                 max_case = max_case))
})

data_group_recovery <- calculate_disease_metrics(data_group_outcome_list) |> 
     mutate(Trough_Label = format(Trough_Date, "%Y-%m"),
            Recovery_Label = format(Recovery_Date, "%Y-%m"),
            Balance_Label = format(Balance_Date, "%Y-%m"),
            Recovery_Label = if_else(is.na(Recovery_Label), "Not recovered", Recovery_Label),
            Balance_Label = if_else(is.na(Balance_Label), "Not balanced", Balance_Label),
            Max_Deficit_label = format(round(Max_Deficit, 0), big.mark = ",", scientific = FALSE),
            Recovery_Period = lubridate::interval(as.Date('2020-1-1'), Recovery_Date) %/% months(1),
            Balance_Period = lubridate::interval(as.Date('2020-1-1'), Balance_Date) %/% months(1)
     )

data_group_recovery_visual <- data_group_recovery |> 
     select(Shortname, 
            Recovery_Date, Balance_Date, 
            Recovery_Period, Balance_Period) |> 
     mutate(StartDate = as.Date('2020-01-01')) |> 
     pivot_longer(
          cols = c(Recovery_Date, Balance_Date, Recovery_Period, Balance_Period),
          names_to = c("type", ".value"), 
          names_sep = "_"
     ) |> 
     select(Shortname, StartDate, type, EndDate = Date, Period) |> 
     # add max date for visualization
     mutate(EndDate = case_when(
          is.na(EndDate) & type %in% c('Balance', 'Recovery') ~ max(data_group_outcome$date),
          TRUE ~ EndDate),
          Period = case_when(
               is.na(Period) & type == 'Balance'  ~ "Not balanced",
               is.na(Period) & type == 'Recovery' ~ "Not recovered",
               TRUE ~ paste(as.character(Period), "months")))
               EndDate = case_when(
                    is.na(EndDate) & type %in% c('Balance', 'Recovery') ~ as.Date(max(data_group_outcome$date)),
                    TRUE ~ as.Date(EndDate)

disease_groups_select <- disease_groups[disease_groups %in% data_class$Group]

fig2 <- lapply(seq_along(disease_groups_select), plot_single_panel,
               outcome = data_group_outcome_list,
               recovery = data_group_recovery_visual,
               titles = paste(LETTERS[seq_along(disease_groups_select)], disease_groups_select, sep = ': '),
               y_angle = 0,
               display_recovery = T) |> 
     wrap_plots(ncol = 4, guides = 'collect', axis_titles = 'collect_y') &
     theme(legend.position = 'right',
           # legend.box = 'vertical',
           legend.direction = 'horizontal')

ggsave("../Outcome/Publish/fig5_b.pdf",
       fig2,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 3.5)

ggsave("../Outcome/Publish/fig5_b.png",
       fig2,
       limitsize = FALSE,
       width = 14, height = 3.5)

data_save <- group_split(data_group_outcome, Group)

# panel IRR ---------------------------------------------------------------

data_outcome <- lapply(seq(nrow(data_class)), function(i) outcome[[i]]$outcome_data) |> 
     bind_rows() |> 
     mutate(diff_percent = round((value + add_value) / (median + add_value), 3),
            diff = round(median - value, 0),
            label = if_else(diff_percent > 10, "*", ""),
            diff_visual = if_else(diff_percent > 10, 10, diff_percent),
            date_num = format(ymd(date), "%Y.%m"))

g <- 2

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
          labs(x = NULL,
               y = NULL,
               fill = "IRR",
               title = paste0(LETTERS[g+4], ": ", disease_groups_select[g]))
     
     return(fig_group)
}

fig_list <- lapply(seq_along(disease_groups_select), plot_group_panel)

fig2 <- wrap_plots(fig_list, ncol = 1, guides = 'collect')

ggsave("../Outcome/Publish/fig5_a.pdf",
       fig2,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 7.5, height = 10)

ggsave("../Outcome/Publish/fig5_a.png",
       fig2,
       limitsize = FALSE,
       width = 7.5, height = 10)

data_save_disease <- data_outcome |> 
     select(Shortname, date, value, median, diff, diff_percent) |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |> 
     # split by group
     group_split(Group)

data_save <- append(data_save,
                    data_save_disease)

write.xlsx(as.list(data_save),
           file = "../Outcome/Publish/figure_data/fig5.xlsx")
