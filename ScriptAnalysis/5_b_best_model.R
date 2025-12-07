# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(ggsci)
library(Cairo)
library(patchwork)
library(paletteer)

remove(list = ls())

source("./function/theme_set.R")
source("./function/forecast.R")

# data --------------------------------------------------------------------

load('./month.RData')

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1 & Forecasting == 1)|> 
     mutate(Group = factor(Group, levels = disease_groups)) |> 
     arrange(Group, desc(Cases)) |> 
     select(-c(Cases, Count, Including, Forecasting, Label)) |> 
     # add group for each 7 disease
     mutate(Group_panel = ceiling(row_number() / 6))

data_goodness <- read.xlsx("../Outcome/Appendix/Model_test_results.xlsx")

# best model --------------------------------------------------------------

data_goodness <- data_goodness |>
     filter(disease %in% data_class$Shortname) |>
     mutate(disease = factor(disease, levels = rev(data_class$Shortname))) |> 
     select(disease, Index, Method, Test) |>
     pivot_wider(names_from = Index, values_from = Test) |> 
     ## z-normalization for each disease
     group_by(disease) |>
     mutate(norSMAPE = -(SMAPE - mean(SMAPE, na.rm = T)) / sd(SMAPE, na.rm = T),
            norRMSE = -(RMSE - mean(RMSE, na.rm = T)) / sd(RMSE, na.rm = T),
            norMASE = -(MASE - mean(MASE, na.rm = T)) / sd(MASE, na.rm = T)) |>
     rowwise() |>
     mutate(Index = sum(c_across(norSMAPE:norMASE), na.rm = T)) |>
     ungroup() |> 
     ## find the best method for each disease based on the maximum index
     group_by(disease) |>
     mutate(Best = Method[which.max(Index)]) |>
     ungroup()
data_goodness$Best <- as.numeric(data_goodness$Method == data_goodness$Best)
data_goodness$Method <- factor(data_goodness$Method, levels = models, labels = models_label)
diseases <- rev(data_class$Shortname)

## save normalized composite index
data_table <- data_goodness |>
     mutate(across(where(is.numeric), ~ round(., 2))) |>
     left_join(data_class[,c('Group', 'Shortname')], by = c("disease" = 'Shortname'))

write.xlsx(data_table,
           "../Outcome/Appendix/Best_model_outcome.xlsx")

data_map <- data_table |> 
     select(Group, disease, Method, Index) |> 
     pivot_wider(names_from = Method, values_from = Index)

# add best column
data_best <- data_table |> 
     select(disease, Method, Best) |> 
     filter(Best == 1) |>
     select(-Best)

data_map <- data_map |>
     left_join(data_best, by = c("disease" = "disease")) |> 
     mutate(disease = factor(disease, levels = diseases)) |>
     arrange(disease) |> 
     pivot_longer(cols = -c(Group, disease, Method),
                  names_to = "model",
                  values_to = "value") |> 
     mutate(label = if_else(model == Method, "*", "")) |> 
     left_join(data_class[,c('Shortname', 'Group_panel')], by = c("disease" = 'Shortname'))

pal_breaks <- pretty(data_map$value)

# plot --------------------------------------------------------------------

fig_group <- ggplot(data_class)+
     geom_tile(mapping = aes(x = 1, y = Shortname, fill = Group),
               color = "white",
               width = 2,
               alpha = 0.5,
               show.legend = F) +
     geom_text(aes(x = 1.5, y = Shortname, label = Shortname),
               size = 3,
               hjust = 1,
               color = "black") +
     coord_cartesian(ratio = 1/3,
                     xlim = c(0, 1.6)) +
     scale_fill_manual(values = fill_color)+
     scale_y_discrete(expand = expansion(add = c(0, 0)),
                      limits = rev(data_class$Shortname))+
     scale_x_discrete(expand = expansion(add = c(0, 0))) +
     theme_bw() +
     theme(legend.position = "bottom",
           axis.text = element_blank(),
           axis.ticks = element_blank(),
           plot.margin = margin(5, 0, 5, 5),
           panel.border = element_blank(),
           plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           plot.title.position = "plot",
           panel.grid = element_blank()) +
     labs(x = NULL,
          title = 'A',
          y = NULL)

fig_model <- ggplot(data_map) +
     geom_tile(mapping = aes(x = model, y = disease, fill = value),
               color = "white") +
     # add value text
     geom_text(aes(x = model, y = disease, label = label),
               size = 2.5,
               color = "black") +
     # coord_equal(2) +
     scale_fill_gradientn(colors = paletteer_d("Redmonder::dPBIRdGn"),
                          breaks = pal_breaks,
                          limits = range(pal_breaks)) +
     scale_y_discrete(expand = expansion(add = c(0, 0)),
                      limits = rev(data_class$Shortname)) +
     scale_x_discrete(expand = expansion(add = c(0, 0)),
                      limits = models_label) +
     theme_bw() +
     theme(legend.position = "bottom",
           axis.text.x = element_text(angle = 45, hjust = 1),
           plot.margin = margin(5, 5, 5, 0),
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           panel.grid = element_blank()) +
     guides(fill = guide_colourbar(barwidth = 15,
                                   title.position = "top",
                                   barheight = 0.5)) +
     labs(x = NULL,
          y = NULL,
          fill = "Standardized index")

fig1 <- fig_group + fig_model + plot_layout(nrow = 1)

save(fig1, data_class, file = './best_model_figure.RData')
