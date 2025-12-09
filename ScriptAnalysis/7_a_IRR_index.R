# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)
library(ggh4x)

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

load('./month.RData')
load('./outcome.RData')
load('./best_model_figure.RData')

data_class <- data_class |> 
     mutate(label = paste0(int2col(id + 1), ": ", Shortname))

data_outcome <- lapply(seq(nrow(data_class)), function(i) outcome[[i]]$outcome_data) |> 
     bind_rows() |> 
     mutate(diff_percent = round((value + add_value) / (mean + add_value), 3),
            diff = round(mean - value, 0),
            label = if_else(diff_percent > 10, "*", ""),
            diff_visual = if_else(diff_percent > 10, 10, diff_percent),
            date_num = format(ymd(date), "%Y.%m"))

# panel IRR ---------------------------------------------------------------

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
                               breaks = c(0, 0.5, 1, 5, 10),
                               labels = c('0', '0.5', '1.0', '5.0', '>10.0'),
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
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10, title.position = "top")) +
          labs(x = NULL,
               y = NULL,
               fill = "IRR",
               title = paste0(LETTERS[g], ": ", disease_groups_select[g]))
     
     return(fig_group)
}

disease_groups_select <- disease_groups[disease_groups %in% data_class$Group]
fig_list <- lapply(seq_along(disease_groups_select), plot_group_panel)

fig1 <- wrap_plots(fig_list, ncol = 1, guides = 'collect')

ggsave("../Outcome/Publish/fig5_a.pdf",
       fig1,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 8, height = 10)

ggsave("../Outcome/Publish/fig5_a.png",
       fig1,
       limitsize = FALSE,
       width = 8, height = 10)

data_save <- data_outcome |> 
     select(Shortname, date, value, mean, diff, diff_percent) |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |> 
     # split by group
     group_split(Group)

# panel group recovery time ----------------------------------------------------

data_group_outcome <- data_outcome |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |> 
     group_by(Group, date) |>
     summarise(value = sum(value),
               mean = sum(mean),
               .groups = 'drop') |> 
     mutate(diff = round(mean - value, 0))

data_group_outcome_plot_1 <- lapply(seq(length(outcome)), function(i) outcome[[i]]$outcome_plot_1) |> 
     bind_rows() |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |>
     group_by(Group, date) |>
     summarise(value = sum(value),
               .groups = 'drop')

data_group_outcome_plot_2 <- lapply(seq(length(outcome)), function(i) outcome[[i]]$outcome_plot_2) |> 
     bind_rows() |> 
     # add group
     left_join(data_class |> select(Shortname, Group), by = "Shortname") |>
     group_by(Group, date) |>
     summarise(mean = sum(mean),
               .groups = 'drop')

## prepare data for visualization
data_group_outcome_list <- lapply(seq(nrow(data_group_outcome)), function(i) {
     outcome_data  <- data_group_outcome |> filter(Group == unique(data_group_outcome$Group)[i])
     outcome_plot_1 <- data_group_outcome_plot_1 |> filter(Group == unique(data_group_outcome$Group)[i])
     outcome_plot_2 <-  data_group_outcome_plot_2 |> filter(Group == unique(data_group_outcome$Group)[i])
     max_case <- max(c(outcome_data$value, outcome_data$mean), na.rm = T)
     min_case <- min(c(outcome_data$value, outcome_data$mean), na.rm = T)
     max_value <- max_case + 0.1 * (max_case - min_case)
     return(list(outcome_data = outcome_data,
                 outcome_plot_1 = outcome_plot_1,
                 outcome_plot_2 = outcome_plot_2,
                 max_value = max_value,
                 min_case = min_case,
                 max_case = max_case))
})

fig2 <- lapply(seq(length(data_group_outcome_list)), plot_single_panel) |> 
     wrap_plots(ncol = 4, guides = 'collect', axis_titles = 'collect') &
     theme(legend.position = 'bottom')
