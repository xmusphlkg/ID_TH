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

# save --------------------------------------------------------------------

fig2 <- lapply(1:nrow(data_class), plot_single_panel, outcome = outcome, titles = data_class$label) |> 
     wrap_plots(ncol = 6, guides = 'collect', axis_titles = 'collect') &
     theme(legend.position = 'bottom')

plot <- cowplot::plot_grid(fig1, fig2,
                           nrow = 1,
                           rel_widths = c(1.5, 7))

ggsave("../Outcome/Publish/fig4.pdf",
       plot,
       family = "Times New Roman",
       limitsize = FALSE, device = cairo_pdf,
       width = 14, height = 10)

ggsave("../Outcome/Publish/fig4.png",
       plot,
       limitsize = FALSE,
       width = 14, height = 10)

data_outcome <- lapply(1:length(outcome), function(x) outcome[[x]]$outcome_data)

data_outcome <- append(list(data_map), data_outcome)

write.xlsx(data_outcome,
           file = "../Outcome/Publish/figure_data/fig4.xlsx")
