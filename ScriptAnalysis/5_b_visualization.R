
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

load('./temp/month.RData')
load('./temp/outcome.RData')
load('./temp/best_model_figure.RData')

data_class <- data_class |> 
     mutate(label = paste0(int2col(id + 1), ": ", Shortname))

# recovery ----------------------------------------------------------------

df_metrics <- calculate_disease_metrics(outcome)

# Format dates for display if needed
df_display <- df_metrics |> 
     mutate(Trough_Label = format(Date_Trough, "%Y-%m"),
            Recovery_Label = format(Date_Recovery, "%Y-%m"),
            Balance_Label = format(Date_Balance, "%Y-%m"),
            Recovery_Label = if_else(is.na(Recovery_Label), "Not recovered", Recovery_Label),
            Balance_Label = if_else(is.na(Balance_Label), "Not balanced", Balance_Label),
            Max_Deficit_label = format(round(Max_Deficit_Raw, 0), big.mark = ",", scientific = FALSE),
            Recovery_Period = lubridate::interval(as.Date('2020-1-1'), Date_Recovery) %/% months(1),
            Balance_Period = lubridate::interval(as.Date('2020-1-1'), Date_Balance) %/% months(1))

data_recovery_visual <- df_display |> 
     select(Shortname, Status,
            Recovery_Date = Date_Recovery, Balance_Date = Date_Balance, 
            Recovery_Period, Balance_Period) |> 
     mutate(StartDate = as.Date('2020-01-01')) |> 
     pivot_longer(
          cols = c(Recovery_Date, Balance_Date, Recovery_Period, Balance_Period),
          names_to = c("type", ".value"), 
          names_sep = "_"
     ) |> 
     select(Shortname, StartDate, Status, type, EndDate = Date, Period) |> 
     # add max date for visualization
     mutate(EndDate = case_when(
          is.na(EndDate) & type %in% c('Balance', 'Recovery') ~ as.Date(max(data_month$Date)),
          TRUE ~ as.Date(EndDate)),
          Period = case_when(is.na(Period) & type == 'Balance' & Status != 'No Deficit' ~ "Not balanced",
                             is.na(Period) & type == 'Recovery' & Status != 'No Deficit' ~ "Not recovered",
                             Status == 'No Deficit' ~ "No deficit",
                             TRUE ~ paste0(as.character(Period), "m")))

print(data_recovery_visual, n = Inf)

# save --------------------------------------------------------------------

fig2 <- lapply(1:nrow(data_class),
               plot_single_panel,
               outcome = outcome,
               recovery = data_recovery_visual,
               display_recovery = T,
               titles = data_class$label) |> 
     wrap_plots(ncol = 6, guides = 'collect', axis_titles = 'collect') &
     theme(legend.position = 'bottom')

plot <- cowplot::plot_grid(fig1, fig2,
                           nrow = 1,
                           rel_widths = c(1.5, 7))

ggsave("../Outcome/Publish/fig3.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 12)

ggsave("../Outcome/Publish/fig3.png",
       plot,
       limitsize = FALSE,
       width = 14, height = 12)

data_outcome <- lapply(1:length(outcome), function(x) outcome[[x]]$outcome_data)

data_outcome <- append(list(data_map), data_outcome)

data_outcome <- append(data_outcome, list(data_recovery_visual))

names(data_outcome) <- int2col(seq_along(data_outcome))

write.xlsx(data_outcome,
           file = "../Outcome/Publish/figure_data/fig3.xlsx")
