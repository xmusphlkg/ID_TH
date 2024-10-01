# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)

# data --------------------------------------------------------------------

remove(list = ls())

source("./function/theme_set.R")

load("./month.RData")

# panel F & G -------------------------------------------------------------

fig1_data <- data_analysis |>
     group_by(Date, Year, Group) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = "drop") |> 
     # add population
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e7,
            Mortality = (Deaths / Population) * 1e7,
            CFR = (Deaths / Cases) * 1000)

fig1 <- ggplot(data = fig1_data) +
     geom_line(mapping = aes(x = Date,
                             y = Incidence,
                             color = Group)) +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = back_color) +
     scale_y_continuous(expand = c(0, 0),
                        trans = "log10",
                        label = scientific_10,
                        limits = c(10, 3e4),
                        breaks = c(10, 1e2, 1e3, 1e4, 3e4)) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig1_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = 'none') +
     labs(x = NULL,
          y = "Monthly incidence (per million)",
          color = NULL,
          title = "A")

fig2_data <- fig1_data |> 
     select(Date, Group, Mortality)

fig2 <- ggplot(data = fig2_data) +
     geom_col(mapping = aes(x = Date, y = Cases, fill = Group),
              show.legend = F,
              color = NA,
              position = "fill") +
     scale_fill_manual(values = fill_color) +
     scale_y_continuous(expand = c(0, 0),
                        labels = scales::percent) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig2_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     labs(x = NULL,
          y = "Percentage",
          title = "F",
          fill = NULL)


# panel H & I -------------------------------------------------------------

fig8_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Deaths = sum(Deaths),
               .groups = "drop")

fig8 <- ggplot(data = fig8_data) +
     geom_line(mapping = aes(x = Date,
                             y = Deaths,
                             color = Group)) +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = back_color) +
     scale_y_continuous(expand = c(0, 0),
                        label = scientific_10,
                        limits = c(0, 300)) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig8_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = "none") +
     labs(x = NULL,
          y = "Monthly deaths",
          color = NULL,
          title = "G")

fig9_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Deaths = sum(Deaths),
               .groups = "drop") |> 
     group_by(Date) |>
     mutate(Deaths = Deaths / sum(Deaths))

fig9 <- ggplot(data = fig9_data) +
     geom_col(mapping = aes(x = Date, y = Deaths, fill = Group),
              show.legend = F,
              color = NA,
              position = "fill") +
     scale_fill_manual(values = fill_color) +
     scale_y_continuous(expand = c(0, 0),
                        labels = scales::percent) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  limits = range(fig9_data$Date),
                  date_breaks = "2 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = "none") +
     labs(x = NULL,
          y = "Percentage",
          title = "H",
          fill = NULL)