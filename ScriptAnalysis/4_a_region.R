
# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(cowplot)
library(sf)
library(biscale)
library(ggthemes)
library(doParallel)

# data --------------------------------------------------------------------

remove(list = ls())

# source("./function/theme_set.R")

load("./month.RData")

data_class <- read.csv("../Data/DiseaseClass.csv") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

# list files in the folder: region data
list_disease_files <- list.files("../Data/CleanData/",
                                 pattern = "rate.csv",
                                 full.names = T)
data_region <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2007 & Areas != 'Total' & 
                 !str_detect(Areas, regex("zone", ignore_case = TRUE)) &
                 !str_detect(Areas, regex("region", ignore_case = TRUE))) |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname)) |> 
     select(Group, Shortname, Year, Areas, Incidence, Mortality, CFR)
rm(list_disease_files)

# unique(data_region$Areas)

data_map <- read_sf("../Data/gadm41_THA_shp/gadm41_THA_1.shp") |> 
     # drop NL_NAME_1
     select(-NL_NAME_1)

# unique(data_map$NAME_1)

# rename the NAME_1 column:
# Bangkok Metropolis: Bangkok
# Bueng Kan: Bungkan
# Phra Nakhon Si Ayutthaya: P.Nakhon S.Ayutthaya

data_map <- data_map |>
     mutate(NAME_1 = case_when(
          NAME_1 == "Bangkok Metropolis" ~ "Bangkok",
          NAME_1 == "Bueng Kan" ~ "Bungkan",
          NAME_1 == "Phra Nakhon Si Ayutthaya" ~ "P.Nakhon S.Ayutthaya",
          TRUE ~ NAME_1
     ))

# check the difference between the two datasets
which(!unique(data_map$NAME_1) %in% unique(data_region$Areas))
which(!unique(data_region$Areas) %in% unique(data_map$NAME_1))

# appendix map ------------------------------------------------------------

plot_map_year <- function(d, data_region, data_map, y, breaks_incidence, breaks_mortality) {
     data <- sp::merge(data_map, data_region |>
                                      filter(Year == y & Shortname == d) |>
                                      select(Areas, Incidence, Mortality, CFR),
                                 by.x = "NAME_1", by.y = "Areas", all.x = T) |> 
          # add class based on the breaks
          mutate(i_class = cut(Incidence, breaks = breaks_incidence, include.lowest = T, labels = F),
                 m_class = cut(Mortality, breaks = breaks_mortality, include.lowest = T, labels = F),
                 bi_class = paste(i_class, m_class, sep = "-"))
     
     fig <- ggplot(data) +
          geom_sf(aes(fill = bi_class),
                  color = "black", linewidth = 0.3, show.legend = F) +
          biscale::bi_scale_fill(pal = 'DkBlue2', dim = 4)+
          theme_map() +
          labs(title = y)
     
     return(fig)
}

plot_map <- function(d, data_region, data_map) {
     # d <- 'Chickenpox'
     # check all incidence values are zero
     breaks_incidence <- data_region |>
          filter(Shortname == d) |>
          pull(Incidence)
     
     if(all(breaks_incidence == 0)) {
          breaks_incidence <- c(0, 1, 2, 3, 4)
     } else {
          breaks_incidence <- breaks_incidence |>
               pretty(n = 4, min.n = 4)
     }
     
     # check all mortality values are zero
     breaks_mortality <- data_region |>
          filter(Shortname == d) |>
          pull(Mortality)
     
     if(all(breaks_mortality == 0)) {
          breaks_mortality <- c(0, 0.001, 0.002, 0.003, 0.004)
     } else {
          breaks_mortality <- breaks_mortality |>
               pretty(n = 4, min.n = 4)
     }
     
     # force the breaks to be 4
     if (length(breaks_incidence) != 4) {
          breaks_incidence <- as.numeric(quantile(breaks_incidence))
     }
     if (length(breaks_mortality) != 4) {
          breaks_mortality <- as.numeric(quantile(breaks_mortality))
     }
     
     
     break_vals <- list(
          bi_x = breaks_incidence,
          bi_y = breaks_mortality
     )
     
     
     legend <- biscale::bi_legend(pal = 'DkBlue2', dim = 4,
                                  pad_width = 0.2, pad_color = "white",
                                  xlab = "Incidence", ylab = "Mortality",
                                  breaks = break_vals,
                                  size = 10) +
          # transparent background
          theme(legend.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent"),
                panel.background = element_rect(fill = "transparent"))
     
     fig <- lapply(sort(unique(data_region$Year)), plot_map_year,
                   data_region = data_region,
                   data_map = data_map,
                   d = d,
                   breaks_incidence = breaks_incidence,
                   breaks_mortality = breaks_mortality)
     
     # add legend to fig
     fig[[length(fig) + 1]] <- ggdraw() +
          draw_plot(legend, x = 0.5, y = 0.5, width = 1, height = 1, vjust = 0.5, hjust = 0.5)
     fig <- patchwork::wrap_plots(fig, ncol = 5)
     
     # save the plot
     ggsave(
          filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_2/", d, ".png"),
          fig,
          device = "png",
          width = 14, height = 15,
          limitsize = FALSE,
          dpi = 300
     )
}

fig <- plot_map('Chickenpox', data_region, data_map)

disease_name <- data_class$Shortname

cl <- makeCluster(length(disease_name))
registerDoParallel(cl)
clusterEvalQ(cl, {
     library(tidyverse)
     library(paletteer)
     library(patchwork)
     library(cowplot)
     library(sf)
     library(biscale)
     library(ggthemes)
})

clusterExport(cl, ls()[ls() != "cl"], envir = environment())
outcome <- parLapply(cl, disease_name, plot_map, data_region = data_region, data_map = data_map)
stopCluster(cl)
