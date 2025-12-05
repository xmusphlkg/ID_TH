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
     select(-c(Cases, Count, Including, Forecasting, Label))

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
           "../Outcome/Publish/figure_data/fig4.xlsx")

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
     mutate(label = format(value, digits = 2, nsmall = 2),
            label = if_else(model == Method, paste0(label, "**"), label))


pal_breaks <- pretty(data_map$value)

# plot --------------------------------------------------------------------

# create map for each group of diseases

# i <- 5

plot_map <- function(i) {
     data <- data_map |> 
          filter(Group == disease_groups[i]) |>
          select(-Group)
     
     fig <- ggplot(data) +
          geom_tile(mapping = aes(x = model, y = disease, fill = value),
                    color = "white") +
          # add value text
          geom_text(aes(x = model, y = disease, label = label),
                    size = 2.5,
                    color = "black") +
          coord_equal(1/3) +
          scale_fill_gradientn(colors = fill_color_continue,
                               breaks = pal_breaks,
                               limits = range(pal_breaks)) +
          scale_y_discrete(expand = expansion(add = c(0, 0))) +
          scale_x_discrete(expand = expansion(add = c(0, 0))) +
          theme_bw() +
          theme(legend.position = "bottom",
                panel.grid = element_blank(),
                plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                plot.title.position = "plot") +
          guides(fill = guide_colourbar(barwidth = 15,
                                        title.position = "top",
                                        barheight = 0.5,
                                        color = "black")) +
          labs(title = LETTERS[i],
               x = NULL,
               y = NULL,
               fill = "Standardized index")
     
     fig
}

plot <- lapply(1:length(unique(data_class$Group)), plot_map) |> 
     wrap_plots(ncol = 1, guides = 'collect')&
     theme(legend.position = "bottom")

ggsave("../Outcome/Publish/fig4.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 7, height = 10)

ggsave("../Outcome/Publish/fig4.png",
       plot,
       limitsize = FALSE,
       width = 7, height = 10)
