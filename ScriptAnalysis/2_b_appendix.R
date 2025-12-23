
# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(openxlsx)
library(ggnewscale)

# Global Settings
remove(list = ls())

source("./function/theme_set.R")

load("./temp/month.RData")

arrow_space <- 0.38
names(fill_color) <- disease_groups

# function ----------------------------------------------------------------

#' Prepare Connections and Zero-Case Ribbons
#' Updated: Removes arrows if both current and next year values are 0
prep_aux <- function(rank_data) {
     
     # 1. Background for Zero/Low values
     # Calculates the boundary between positive values and zero values
     bg <- rank_data |>
          group_by(Year_mark) |>
          summarise(
               ymin = max(Rank[Value > 0], default = 0) + 0.5,
               ymax = max(rank_data$Rank) + 0.5, 
               .groups = 'drop'
          ) |>
          mutate(xmin = pmax(Year_mark - arrow_space, 0),
                 xmax = Year_mark + arrow_space) |>
          pivot_longer(c(xmin, xmax), names_to = "type", values_to = "x")
     
     # 2. Connections
     conn <- rank_data |>
          group_by(Shortname) |>
          arrange(Year_mark) |>
          mutate(
               Next_Rank = lead(Rank),
               Next_Value = lead(Value), # <--- Get next year's value to check for 0-0
               Status = case_when(Next_Rank > Rank ~ 'Decrease',
                                  Next_Rank < Rank ~ 'Increase',
                                  TRUE ~ 'Constant'),
               Status = factor(Status, levels = c('Decrease', 'Constant', 'Increase')),
               Next_x = lead(Year_mark)
          ) |>
          filter(!is.na(Next_Rank)) |>
          # <--- NEW: Remove connection if BOTH current and next values are 0
          filter(!(Value == 0 & Next_Value == 0))
     
     list(bg = bg, conn = conn)
}

#' Calculate Stable Ranks (Backward Historical Inertia + Adjacent Magnitude)
calc_rank <- function(data, val_col, tie_breaker = NULL) {
     
     # 1. Aggregate data
     df_sum <- data |>
          group_by(Shortname, Group, Year_group) |>
          summarise(Value = sum(.data[[val_col]]), .groups = 'drop')
     
     # 2. Pre-calculate Adjacent Values (Previous & Next)
     # We use these to break ties: diseases with higher adjacent activity rank higher.
     df_sum <- df_sum |>
          arrange(Shortname, Year_group) |> # Ensure chronological order per disease
          group_by(Shortname) |>
          mutate(
               Prev_Value = lag(Value, default = 0),
               Next_Value = lead(Value, default = 0),
               # Calculate Neighbor Strength: Sum of adjacent years
               Adj_Strength = Prev_Value + Next_Value
          ) |>
          ungroup()
     
     t_points <- sort(unique(df_sum$Year_group))
     N <- length(t_points)
     res_list <- vector("list", N)
     
     # A. Anchor: Sort the LAST Year
     # For the last year, we only have Prev_Value (Next is 0/NA)
     df_last <- df_sum |> filter(Year_group == t_points[N])
     
     if (!is.null(tie_breaker)) {
          df_last <- df_last |> 
               left_join(tie_breaker, by = "Shortname") |>
               # Sort: Value -> Prev Value -> Total Metric -> Name
               arrange(desc(Value), desc(Prev_Value), desc(metric_total), Shortname)
     } else {
          df_last <- df_last |> 
               arrange(desc(Value), desc(Prev_Value), Shortname)
     }
     
     res_list[[N]] <- df_last |> mutate(Rank = row_number())
     
     # B. Iterate BACKWARDS (N-1 -> 1)
     if (N > 1) {
          for(i in (N-1):1) {
               
               # Use Next Year's Rank to maintain flow continuity
               future_rank <- res_list[[i+1]] |> 
                    select(Shortname, Next_Rank = Rank)
               
               curr <- df_sum |> 
                    filter(Year_group == t_points[i]) |> 
                    left_join(future_rank, by = "Shortname") |> 
                    # Sorting Logic:
                    # 1. Current Value (desc): Reality is absolute.
                    # 2. Adjacent Strength (desc): If tied, the one with more adjacent cases is higher.
                    # 3. Next Rank (asc): If still tied, align with the future line to avoid crossing.
                    # 4. Name: Deterministic fallback.
                    arrange(desc(Value), desc(Adj_Strength), Next_Rank, Shortname) |> 
                    mutate(Rank = row_number()) |> 
                    select(-Next_Rank)
               
               res_list[[i]] <- curr
          }
     }
     
     bind_rows(res_list) |>
          arrange(Year_group, Rank) |>
          mutate(Label = paste(Rank, Shortname, sep = '. '),
                 Year_mark = as.integer(Year_group))
}

plot_ranking <- function(main, aux, title, ribbon_txt = '', legend = TRUE) {
     p <- ggplot()+ 
          geom_ribbon(data = aux$bg, aes(x = x, ymin = ymin, ymax = ymax), 
                      alpha = 0.3, fill = "grey50", color = 'white') +
          # add text on ribbon if specified
          annotate("text", x = min(main$Year_mark), 
                   y = max(aux$bg$ymax) - 2, label = ribbon_txt, 
                   hjust = 0, vjust = 1.5, color = "grey20", size = 3.5, fontface = "italic") +
          geom_tile(data = main, aes(x = Year_mark, y = Rank, fill = Group),
                    width = arrow_space*2, color = 'white') +
          geom_text(data = main, aes(x = Year_mark, y = Rank, label = Label), 
                    nudge_x = -0.35, color = 'white', fontface = 'bold', 
                    size = 2.2, hjust = 0, check_overlap = FALSE) +
          geom_segment(data = aux$conn,
                       aes(x = Year_mark + arrow_space, y = Rank, 
                           xend = Next_x - arrow_space, yend = Next_Rank, 
                           color = Status), 
                       arrow = arrow(length = unit(2, "mm")), 
                       lineend = "round", linejoin = "round") +
          # Scales
          coord_cartesian(xlim = c(1, length(unique(main$Year_mark))), ratio = 0.12) +
          scale_x_continuous(breaks = unique(main$Year_mark), labels = unique(main$Year_group)) +
          scale_y_reverse(expand = c(0, 0)) +
          scale_fill_manual(values = fill_color) +
          scale_color_manual(values = c('Decrease' = "#019875", 'Constant' = "#FECEA8", 'Increase' = "#C0392B")) +
          theme_bw() +
          labs(title = title, x = NULL, y = NULL, color = "Changes of ranking", fill = "Disease categories") +
          theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                plot.title = element_text(face = 'bold', size = 14),
                legend.position = 'bottom', legend.box = 'horizontal')
     
     if (!legend) p <- p + guides(color = 'none', fill = 'none')
     
     return(p)
}

# data --------------------------------------------------------------------

# Pre-process Year Grouping
df_all <- data_year |> 
     # Year Group
     mutate(Year_group = factor(paste0(Year - (Year %% 2), "-", Year - (Year %% 2) + 1)))

# figure ------------------------------------------------------------------

d_cases <- calc_rank(df_all, "Cases")
aux_cases <- prep_aux(d_cases)
fig1 <- plot_ranking(d_cases, aux_cases, "A", legend = TRUE, ribbon_txt = "No cases reported")

# Filter diseases with ANY historical deaths
d_deaths_raw <- df_all |> 
     group_by(Shortname) |> 
     filter(sum(Deaths) > 0) |> 
     ungroup()

# Tie-breaker: Total deaths
tb_deaths <- d_deaths_raw |> 
     group_by(Shortname) |> 
     summarise(metric_total = sum(Deaths), .groups = 'drop')

d_deaths <- calc_rank(d_deaths_raw, "Deaths", tie_breaker = tb_deaths)
aux_deaths <- prep_aux(d_deaths)

# Plot (Note: ribbon_txt triggers the special grey legend)
fig2 <- plot_ranking(d_deaths, aux_deaths, "B", ribbon_txt = "No deaths reported", legend = FALSE)

# save --------------------------------------------------------------------

final_plot <- fig1 / fig2 + 
     plot_layout(ncol = 1, guides = 'collect') & 
     theme(legend.position = 'bottom', legend.title.position = 'top')

ggsave("../outcome/publish/fig2.pdf", final_plot, width = 14, height = 15, device = cairo_pdf, family = "Times New Roman")
ggsave("../outcome/publish/fig2.png", final_plot, width = 14, height = 15)

# Export Data
write.xlsx(list('A' = d_cases, 'B' = d_deaths), "../outcome/Publish/figure_data/fig2.xlsx")
