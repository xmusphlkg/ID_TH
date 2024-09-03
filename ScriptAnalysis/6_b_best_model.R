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

data_goodness <- read.xlsx("../Outcome/Appendix/Supplementary Appendix 2.xlsx")

data_goodness$disease <- factor(data_goodness$disease, levels = rev(data_class$Shortname))

# best model --------------------------------------------------------------

data_goodness <- data_goodness |>
     select(disease, Index, Method, Test) |>
     pivot_wider(names_from = Index, values_from = Test) |> 
     ## z-normalization for each disease
     group_by(disease) |>
     mutate(Index = -(RMSE - mean(RMSE, na.rm = T)) / sd(RMSE, na.rm = T)) |>
     rowwise() |>
     # mutate(Index = SMAPE) |> 
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
     select(disease, Method, Best, Index, RMSE) |> 
     left_join(data_class[,c('Group', 'Shortname')], by = c("disease" = "Shortname"))

write.xlsx(data_table,
           "../Outcome/Appendix/figure_data/fig6.xlsx")

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
     arrange(disease)

# plot --------------------------------------------------------------------

# create map for each group of diseases

i <- 1

plot_map <- function(i) {
     data <- data_map |> 
          filter(Group == disease_groups[i]) |>
          select(-Group) |> 
          pivot_longer(cols = -c(disease, Method),
                       names_to = "model",
                       values_to = "value") |> 
          mutate(label = format(value, digits = 2, nsmall = 2),
                 label = if_else(model == Method, paste0(label, "**"), label))
     
     fig <- ggplot(data) +
          geom_tile(mapping = aes(x = model, y = disease, fill = value),
                    color = "white") +
          # add value text
          geom_text(aes(x = model, y = disease, label = label),
                    size = 2.5,
                    color = "black") +
          coord_equal(1/3) +
          scale_fill_gradientn(colors = rev(paletteer_d("rcartocolor::Temps")),
                               limits = c(-3, 3)) +
          scale_y_discrete(expand = expansion(add = c(0, 0))) +
          scale_x_discrete(expand = expansion(add = c(0, 0))) +
          theme_bw() +
          theme(legend.position = "bottom",
                panel.grid = element_blank(),
                axis.text = element_text(size = 8, color = "black"),
                plot.title = element_text(face = "bold", size = 14, color = "black"),
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

plot <- lapply(1:length(disease_groups), plot_map) |> 
     wrap_plots(ncol = 1, guides = 'collect')&
     theme(legend.position = "bottom")

ggsave("../Outcome/Publish/fig6.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 6, height = 12)
