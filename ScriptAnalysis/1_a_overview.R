# packages ----------------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)

# System setting
Sys.setlocale(locale = "EN")

# data --------------------------------------------------------------------

# loading function
source("./theme_set.R")

# list files in the folder: month case and death data
list_disease_files <- list.files("../Data/CleanData/",
                                  pattern = "mcd.csv",
                                  full.names = T)
data_month <- lapply(list_disease_files, read.csv) |>
     bind_rows()

rm(list_disease_files)

data_select <- data_month |>
     filter(Year >= 2007) |>
     filter(Areas == 'Total' & Month == 'Total') |>
     group_by(Disease) |>
     summarise(Cases = sum(Count),
               Count = n(),
               .groups = 'drop')

# write.csv(data_select, file = "../Data/DiseaseClass1.csv",
#           row.names = F)

data_class <- read.csv("../Data/DiseaseClass.csv") |> 
     filter(Including == 1) |> 
     select(-c(Cases, Count, Including, Label))

data_analysis <- data_month |>
     left_join(data_class, by = 'Disease') |>
     filter(Areas == 'Total' & Month != 'Total' & 
                 !is.na(Shortname) & Year >= 2007 & Type == 'Cases') |> 
     # transform the Month: Jan -> 01
     mutate(Month = month(parse_date_time(Month, "b"), label = FALSE, abbr = FALSE),
            Date = as.Date(paste(Year, Month, "01", sep = "-"))) |> 
     rename(Cases = Count) |> 
     filter(Date < as.Date("2024-07-01")) |> 
     ungroup() |> 
     arrange(Date, Disease)

# summary of NID ----------------------------------------------------------

## each group
data_analysis |>
     group_by(Group, Shortname) |>
     summarise(Cases = sum(Cases),
               DateRange = paste(strftime(min(Date), "%Y/%m"),
                                 strftime(max(Date), "%Y/%m"),
                                 sep = " ~ "),
               .groups = "drop") |>
     group_by(Group) |>
     mutate(Percent = percent(round(Cases / sum(Cases), 4)),
            .before = DateRange) |>
     arrange(Group, desc(Cases)) |>
     print(n = Inf)

# panel A -----------------------------------------------------------------

names(fill_color) <- sort(unique(data_analysis$Group))

fig1_data <- data_analysis |>
     select(Year, Disease = Shortname, Group, Cases) |>
     group_by(Group, Disease) |>
     summarise(Cases = sum(Cases),
               .groups = 'drop') |> 
     arrange(desc(Group), Cases)

fig1 <- ggplot(data = fig1_data)+
     geom_col(mapping = aes(x = Disease, y = Cases, fill = Group),
              show.legend = F) +
     scale_fill_manual(values = fill_color) +
     scale_x_discrete(expand = c(0, 0),
                      limits = rev(fig1_data$Disease)) +
     scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                        limits = c(0, NA),
                        label = scientific_10)+
     coord_cartesian(ylim = c(0, 3e5))+
     theme_plot() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
     labs(y = "Cumulative cases",
          x = NULL,
          fill = NULL,
          title = 'A')

# panel B --------------------------------------------------------------

fig2_data <- read.csv('../Data/WHO-COVID-19-global-data.csv') |> 
     filter(Country == 'Thailand') |> 
     select(Date_reported, New_cases) |> 
     mutate(Date_reported = as.Date(Date_reported))

data_rect <- data.frame(start = c(min(fig2_data$Date_reported, na.rm = T), split_dates),
                        end = c(split_dates, max(data_analysis$Date)),
                        label = split_periods) |>
     mutate(med = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01")) 

fig2 <- ggplot(data = fig2_data) +
     geom_rect(data = data_rect,
               aes(xmin = start, xmax = end, fill = label),
               ymin = -Inf, ymax = Inf, alpha = 0.2,
               show.legend = F) +
     geom_line(mapping = aes(x = Date_reported, y = New_cases)) +
     scale_fill_manual(values = back_color) +
     scale_y_continuous(expand = c(0, 0),
                        limits = c(0, 2e5),
                        label = scientific_10) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  date_breaks = "1 years",
                  date_labels = "%Y") +
     theme_plot() +
     labs(x = NULL,
          y = "Weekly incidence",
          title = "B")

# panel C ---------------------------------------------------------

data_rect <- data.frame(start = c(min(data_analysis$Date), split_dates),
                        end = c(split_dates, max(data_analysis$Date)),
                        label = split_periods) |>
     mutate(med = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01")) 

fig3_data <- data_analysis |>
     group_by(Date, Group) |>
     summarise(Cases = sum(Cases),
               .groups = "drop") |>
     group_by(Date) |>
     mutate(Percent = Cases / sum(Cases),
            .before = Cases)

fig3 <- ggplot(data = fig3_data) +
     geom_rect(data = data_rect,
               aes(xmin = start, xmax = end, fill = label),
               ymin = -Inf, ymax = Inf, alpha = 0.2,
               show.legend = F) +
     geom_line(mapping = aes(x = Date,
                             y = Cases,
                             color = Group)) +
     scale_color_manual(values = fill_color) +
     scale_fill_manual(values = back_color) +
     scale_y_continuous(expand = c(0, 0),
                        trans = "log10",
                        label = scientific_10,
                        limits = c(1e2, 2e5),
                        breaks = c(1e2, 1e3, 1e4, 1e5, 2e5)) +
     scale_x_date(expand = expansion(add = c(0, 15)),
                  date_breaks = "1 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = "none") +
     labs(x = NULL,
          y = "Monthly incidence",
          color = NULL,
          title = "C")

# panel D -----------------------------------------------------------------

fig4_data <- fig3_data |> 
     group_by(Date) |> 
     mutate(Percent = Cases / sum(Cases)) |> 
     ungroup() |> 
     mutate(Group = factor(Group, levels = unique(fig3_data$Group)))

fig4 <- ggplot(data = fig3_data) +
     geom_col(mapping = aes(x = Date, y = Cases, fill = Group),
              show.legend = F,
              position = "fill") +
     scale_fill_manual(values = fill_color) +
     scale_y_continuous(expand = c(0, 0),
                        labels = scales::percent) +
     scale_x_date(expand = expansion(add = c(15, 15)),
                  date_breaks = "1 years",
                  date_labels = "%Y") +
     theme_plot() +
     theme(legend.position = "bottom") +
     labs(x = "Date",
          y = "Percentage of categories",
          title = "D",
          fill = NULL)

# save plot ---------------------------------------------------------------

fig1_2 <- cowplot::plot_grid(fig1, fig2, nrow = 1, rel_widths = c(2.5, 1.15))

# fig <- fig1_2 + fig3 + fig4 + 
#      plot_layout(ncol = 1, heights = c(3, 3.5, 3.5))

fig <- cowplot::plot_grid(fig1_2, fig3, fig4, nrow = 3, rel_heights = c(3, 3.5, 3.5))

ggsave(
     filename = "../Outcome/Publish/fig1.pdf",
     fig,
     width = 14,
     height = 12,
     device = cairo_pdf,
     family = "Times New Roman"
)

# figure data
data_fig <- list("panel A" = fig1_data,
                 "panel B" = fig2_data,
                 "panel C" = fig3_data,
                 "panel D" = fig4_data)

write.xlsx(data_fig,
           file = "../Outcome/Appendix/figure_data/fig1.xlsx")

data_class <- fig1_data |> 
     rename(Shortname = 'Disease')

save(data_analysis, data_month, data_class, file = "./month.RData")
