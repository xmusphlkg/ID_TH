
# packages ----------------------------------------------------------------

library(tidyverse)
library(paletteer)
library(patchwork)
library(openxlsx)
library(ungroup)

# data --------------------------------------------------------------------

source('./2_b_appendix.R')

# Load old age-stratified data (2008-2023)
list_disease_files <- list.files("../Data/CleanData/", pattern = "ac.csv", full.names = T)
data_case_raw <- lapply(list_disease_files, read.csv) |>
     bind_rows() |>
     filter(Year >= 2008 & Areas == 'Total') |> 
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname))

# Clean raw age strings before processing (remove Total/Unknown)
data_case_clean <- data_case_raw |>
     filter(!Age %in% c("Total", "Unknown")) |>
     select(Group, Shortname, Age, Year, Cases)

# Load old death age-stratified data (2008-2023)
list_death_files <- list.files("../Data/CleanData/", pattern = "ad.csv", full.names = T)
data_death_raw <- lapply(list_death_files, read.csv) |>
     bind_rows() |> 
     filter(Year >= 2008 & Areas == 'Total') |>
     left_join(data_class, by = 'Disease') |>
     filter(!is.na(Shortname))

# Clean raw age strings before processing (remove Total/Unknown)
data_death_clean <- data_death_raw |>
     filter(!Age %in% c("Total", "Unknown")) |>
     select(Group, Shortname, Age, Year, Deaths = Cases)

# Harmonize age groups --------------------------------------------------------

# This ensures both old and new data map to exactly the same structure
TARGET_GROUPS <- c("0-4", "5-9", "10-14", "15-19", "20-39", "40-59", "60+")

# To analyze long-term epidemiological trends, we harmonized inconsistent age-stratified 
# reporting formats between historical data (2008–2024, irregular intervals such as 15-24) 
# and recent data (2020–2025, standard decadal intervals).
#
# We standardized all counts into seven unified age groups: 
# 0–4, 5–9, 10–14, 15–19, 20–39, 40–59, and ≥60 years.
#
# For historical data containing coarse, overlapping intervals (e.g., 15–24 years), 
# we applied the Penalized Composite Link Model (PCLM) using the 'ungroup' package in R.
# This method estimates a smooth underlying single-year age distribution (0, 1, ..., 99) 
# from grouped data by maximizing a penalized likelihood. This approach allows for 
# scientifically robust disaggregation and subsequent re-aggregation into the target groups, 
# minimizing bias compared to simple uniform distribution assumptions.
#
# Technical Adjustments:
# 1. Zero-Handling: To address convergence issues in PCLM caused by zero counts, 
#    a negligible constant (epsilon = 1e-5) was added to inputs. Post-estimation, 
#    counts were rescaled to conserve the original total reported cases.
# 2. Sparse Data: For year-disease combinations with extremely low total counts (<5), 
#    a uniform distribution assumption was applied as a fallback to prevent overfitting.
# 3. Validation: The method was validated using the overlapping period (2020–2024) 
#    where both coarse and fine-scale data were available.

harmonize_old_data <- function(df_input, value_col = "Cases") {
     
     # 1. Map raw strings to numeric intervals for PCLM
     # Adjust this table if your raw text differs (e.g., "7-9" vs "7-10")
     map_intervals <- tibble(
          Age = c("0", "<1", "1-1", "2-2", "3-3", "4-4", "5-5", "6-6", "7-9", 
                  "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
          Start = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 10, 15, 25, 35, 45, 55, 65),
          Width = c(1, 1, 1, 1, 1, 1, 1, 1, 3, 5, 10, 10, 10, 10, 10, 35) # Assumes 65+ spans 35 years (to 100)
     )
     
     df_ready <- df_input |>
          inner_join(map_intervals, by = "Age") |>
          group_by(Shortname, Year) |>
          arrange(Start)
     
     # 2. Inner Helper: Safe PCLM Execution
     run_pclm_safe <- function(x, y, n, disease_name, year_val) {
          
          total_y <- sum(y, na.rm = TRUE)
          
          # CASE A: Sparse Data / Zeros
          # If total cases are extremely low (< 20), PCLM cannot form a reliable spline.
          # Fallback to simple uniform distribution (Average) to avoid errors.
          if(total_y < 20) { 
               res <- numeric(100) # Vector for ages 0-99
               for(i in seq_along(x)) {
                    s <- x[i] + 1     # R indices are 1-based
                    e <- min(x[i] + n[i], 100)
                    if(s <= 100) {
                         res[s:e] <- y[i] / n[i] # Distribute evenly
                    }
               }
               return(tibble(Age_1y = 0:99, Est_Val = res))
          }
          
          # CASE B: Standard PCLM
          tryCatch({
               # Add epsilon to handle zeros in log-link
               y_adj <- y + 1e-5
               
               # Fit Model
               model <- suppressWarnings(pclm(x = x, y = y_adj, nlast = n[length(n)]))
               fitted_val <- fitted(model)
               
               # Pad result to ensure length 100
               len <- length(fitted_val)
               if(len < 100) { fitted_val <- c(fitted_val, rep(0, 100-len)) }
               fitted_val <- fitted_val[1:100]
               
               # RESCALE: Total Conservation Principle
               # Ensure sum(fitted) equals original sum(y) exactly
               correction_factor <- total_y / sum(fitted_val)
               fitted_val <- fitted_val * correction_factor
               
               return(tibble(Age_1y = 0:99, Est_Val = fitted_val))
               
          }, error = function(e) {
               # Fallback if PCLM fails convergence
               warning(paste("PCLM failed for", disease_name, year_val, "- using uniform split"))
               # Repeat uniform logic from Case A
               res <- numeric(100)
               for(i in seq_along(x)) {
                    s <- x[i] + 1
                    e <- min(x[i] + n[i], 100)
                    if(s <= 100) res[s:e] <- y[i] / n[i]
               }
               return(tibble(Age_1y = 0:99, Est_Val = res))
          })
     }
     
     # 3. Apply to all groups
     df_processed <- df_ready |>
          group_by(Shortname, Year) |>
          summarise(
               pclm_res = list(run_pclm_safe(Start, !!sym(value_col), Width, unique(Shortname), unique(Year))),
               .groups = "drop"
          ) |>
          unnest(pclm_res)
     
     # 4. Re-aggregate to Target Groups
     df_final <- df_processed |>
          mutate(
               AgeGroup = case_when(
                    Age_1y >= 0 & Age_1y <= 4   ~ "0-4",
                    Age_1y >= 5 & Age_1y <= 9   ~ "5-9",
                    Age_1y >= 10 & Age_1y <= 14 ~ "10-14",
                    Age_1y >= 15 & Age_1y <= 19 ~ "15-19",
                    Age_1y >= 20 & Age_1y <= 39 ~ "20-39",
                    Age_1y >= 40 & Age_1y <= 59 ~ "40-59",
                    Age_1y >= 60                ~ "60+",
                    TRUE                        ~ "Unknown"
               )
          ) |>
          group_by(Shortname, Year, AgeGroup) |>
          summarise(!!sym(value_col) := sum(Est_Val), .groups = "drop")
     
     return(df_final)
}

# Apply harmonization for cases
data_case_harmonized <- harmonize_old_data(data_case_clean, "Cases")
rm(list_disease_files, data_case_raw, data_case_clean)

# Apply harmonization for deaths
data_death_harmonized <- harmonize_old_data(data_death_clean, "Deaths")
rm(list_death_files, data_death_raw, data_death_clean)

# data prepare ------------------------------------------------------------

# Process New Data (2024-2025) - Simple Aggregation
data_age_2024 <- data_week_age_deaths |> 
     select(Shortname, year, week, age_group, deaths) |>
     mutate(week = as.character(week)) |> 
     left_join(data_week_age, by = c("Shortname", "year", "week", "age_group")) |> 
     rename(Year = year) |>
     filter(Year > 2023) |> # Or 2020 depending on where you want cut-off
     mutate(
          AgeGroup = case_when(
               age_group == "0-4" ~ "0-4",
               age_group == "5-9" ~ "5-9",
               age_group == "10-14" ~ "10-14",
               age_group == "15-19" ~ "15-19",
               age_group %in% c("20-29", "30-39") ~ "20-39",
               age_group %in% c("40-49", "50-59") ~ "40-59",
               age_group == "60+" ~ "60+",
               TRUE ~ "Unknown"
          )
     ) |>
     filter(AgeGroup != "Unknown") |>
     group_by(Shortname, Year, AgeGroup) |>
     summarise(Cases = sum(cases, na.rm = TRUE),
               Deaths = sum(deaths, na.rm = TRUE),
               .groups = 'drop')

# Merge harmonized cases and deaths (Old Data)
data_age_old_combined <- data_case_harmonized |>
     full_join(data_death_harmonized, by = c("Shortname", "Year", "AgeGroup")) |>
     replace_na(list(Cases = 0, Deaths = 0))

# Combine Old (2008-2023) and New (2024-2025)
data_age_all <- data_age_old_combined |>
     filter(Year <= 2023) |>
     bind_rows(data_age_2024) |>
     mutate(AgeGroup = factor(AgeGroup, levels = TARGET_GROUPS),
            Year_group = factor(paste0(Year - (Year %% 2), "-", Year - (Year %% 2) + 1)),
            Year_mark = as.integer(Year_group)) |>
     arrange(Shortname, Year, AgeGroup) 

data_age_all <- data_age_all|> 
     group_by(Shortname, AgeGroup, Year_group, Year_mark) |> 
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop')

## compare old and new data for 2020-2023 ----------------------------------

# Reusable validator: compares harmonized estimates to weekly observed data
validate_pclm_validation <- function(harmonized_df,
                                     weekly_deaths_df,
                                     weekly_cases_df,
                                     value_col = c("Cases", "Deaths"),
                                     years = 2020:2023,
                                     target_groups = TARGET_GROUPS,
                                     fill_colors = fill_color_disease,
                                     label_fun = scientific_10) {
     value_col <- match.arg(value_col)

     # Estimated from harmonized (renamed to Estimated)
     val_estimated <- harmonized_df |>
          filter(Year %in% years) |>
          rename(Estimated = !!sym(value_col))

     # Observed from weekly raw sources. We start from weekly_deaths_df (contains deaths)
     # and join weekly_cases_df (contains cases) so we can aggregate either column.
     observed_col <- tolower(value_col)

     val_observed <- weekly_deaths_df |>
          select(Shortname, year, week, age_group, deaths) |>
          mutate(week = as.character(week)) |>
          left_join(weekly_cases_df, by = c("Shortname", "year", "week", "age_group")) |>
          filter(year %in% years) |>
          mutate(AgeGroup = case_when(
                    age_group == "0-4"   ~ "0-4",
                    age_group == "5-9"   ~ "5-9",
                    age_group == "10-14" ~ "10-14",
                    age_group == "15-19" ~ "15-19",
                    age_group %in% c("20-29", "30-39") ~ "20-39",
                    age_group %in% c("40-49", "50-59") ~ "40-59",
                    age_group == "60+"   ~ "60+",
                    TRUE ~ "Unknown"),
               AgeGroup = factor(AgeGroup, levels = target_groups)) |>
          filter(AgeGroup != "Unknown") |>
          group_by(Shortname, Year = year, AgeGroup) |>
          summarise(Observed = sum(!!sym(observed_col), na.rm = TRUE), .groups = "drop")

     validation_df <- inner_join(val_estimated,
                                 val_observed,
                                 by = c("Shortname", "Year", "AgeGroup"))

     # Pearson correlation
     cor_val <- cor(validation_df$Estimated, validation_df$Observed, method = "pearson")

     # Plot
     p <- ggplot(validation_df, aes(x = Observed, y = Estimated)) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
          geom_point(aes(color = AgeGroup), alpha = 0.6, size = 2) +
          scale_color_manual(values = fill_colors) +
          scale_x_continuous(labels = label_fun) +
          scale_y_continuous(labels = label_fun) +
          labs(
               title = paste0("Validation of PCLM age-reconstruction method (", paste(range(years), collapse = "-"), ")"),
               subtitle = sprintf("Comparison of Estimated (from coarse bins) vs Observed (fine bins) Pearson r = %.4f", cor_val),
               x = paste0("Observed ", tolower(value_col)),
               y = paste0("Estimated ", tolower(value_col))
          ) +
          theme_classic() +
          theme(legend.position = "bottom") +
          guides(color = guide_legend(title = "Age Group", nrow = 2, byrow = TRUE))

     return(list(plot = p, cor = cor_val, df = validation_df))
}

# Run validation for Cases
res_cases <- validate_pclm_validation(data_case_harmonized, data_week_age_deaths, data_week_age, value_col = "Cases")
p_validation_cases <- res_cases$plot

# Run validation for Deaths
res_deaths <- validate_pclm_validation(data_death_harmonized, data_week_age_deaths, data_week_age, value_col = "Deaths")
p_validation_deaths <- res_deaths$plot

ggsave("../Outcome/Appendix/Supplementary Appendix 1_3/validation_age.png",
       p_validation_cases + p_validation_deaths,
       width = 14,
       height = 7)

## rank --------------------------------------------------------------------

# create the ranking of each disease by cases and deaths
data_age_top <- data_age_all |> 
     # Calsulate total cases and deaths by year and disease
     group_by(Shortname, Year_group, Year_mark) |> 
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |>
     mutate(AgeGroup = 'Total') |> 
     bind_rows(data_age_all) |>
     # finding leading causes of death, case in each age group
     group_by(AgeGroup, Year_group, Year_mark) |>
     summarise(Max_Cases_Disease = Shortname[which.max(Cases)],
               Max_Deaths_Disease = Shortname[which.max(Deaths)],
               Max_Cases_Value = max(Cases),
               Lsit_Cases_Value = list(sort(Cases, decreasing = T)),
               Max_Deaths_Value = max(Deaths),
               Lsit_Deaths_Value = list(sort(Deaths, decreasing = T)),
               .groups = 'drop') |> 
     # replace 0 deaths with no deaths
     mutate(Max_Deaths_Disease = if_else(Max_Deaths_Value == 0, 'No deaths', Max_Deaths_Disease),
            Max_Deaths_Value = if_else(Max_Deaths_Value == 0, NA_real_, Max_Deaths_Value),
            AgeGroup = factor(AgeGroup, levels = c(TARGET_GROUPS, "Total")),
            AgeGroupID = as.numeric(AgeGroup))

# get disease list
max_disease <- data_age_top |>
     select(Max_Cases_Disease, Max_Deaths_Disease) |>
     unlist() |>
     unique()

## cumulative --------------------------------------------------------------

# create cumulative data for plotting
data_age_cumulative <- data_age_all |> 
     select(AgeGroup, Shortname, Cases, Deaths) |>
     # find top 3 diseases of each age group
     group_by(AgeGroup, Shortname) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     group_by(AgeGroup) |>
     arrange(desc(Cases), .by_group = TRUE) |>
     mutate(rank_in_group = row_number(),
            Top_cases_tag = if_else(rank_in_group <= 3, Shortname, "Others")) |> 
     arrange(desc(Deaths), .by_group = TRUE) |>
     mutate(rank_in_group = row_number(),
            Top_deaths_tag = if_else(rank_in_group <= 3, Shortname, 'Others')) |> 
     ungroup() |> 
     mutate(AgeGroup = factor(AgeGroup, levels = c(TARGET_GROUPS, "Total")),
            AgeGroupID = as.numeric(AgeGroup))

# check disease list
legend_disease_list <- unique(c(data_age_cumulative$Top_cases_tag,
                                data_age_cumulative$Top_deaths_tag))

# order by cumulative cases
max_disease <- data_age_cumulative |>
     mutate(Max_tag = case_when(Shortname %in% legend_disease_list ~ Shortname,
                                TRUE ~ 'Others')) |> 
     filter(Max_tag != 'Others') |> 
     group_by(Max_tag) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(desc(Cases)) |> 
     pull(Max_tag) |> 
     append('Others')

## get colors
fill_color_disease_max <- fill_color_disease[1:(length(max_disease)-1)]
fill_color_disease_max <- c(fill_color_disease_max, 'grey50')
names(fill_color_disease_max) <- max_disease

# visual ------------------------------------------------------------------
     
data_outcome <- list()

data_outcome[['panel A']] <- d_cases

data_outcome[['panel C']] <- d_deaths

for (i in 1:2) {
     # cumulative panel
     data_cumulative <- data_age_cumulative |>
          select(AgeGroup, AgeGroupID, contains(c('ases', 'eaths')[i])) |>
          filter(AgeGroup != 'Total')
     names(data_cumulative)[3:4] <- c('Outcome', 'Max_tag')
     
     data_cumulative <- data_cumulative |>
          group_by(AgeGroup, AgeGroupID, Max_tag) |>
          summarise(Outcome = sum(Outcome),
                    .groups = 'drop') |>
          mutate(Max_tag = factor(Max_tag, levels = max_disease)) |>
          arrange(AgeGroupID, Max_tag)
     
     data_outcome[[paste('panel', LETTERS[i*3-1], sep = '')]] <- data_cumulative
     
     panel_breaks <- data_cumulative |>
          pull(Outcome) |>
          append(0) |> 
          pretty(n = 5)
     
     fig_cumulative <- data_cumulative |>
          ggplot(aes(x = AgeGroup, y = Outcome, fill = Max_tag)) +
          geom_col(color = 'white',
                   position = 'dodge',
                   width = 0.8,
                   show.legend = i == 1) +
          scale_fill_manual(values = fill_color_disease_max,
                            drop = F) +
          scale_x_discrete(expand = expansion(add = c(0.6, 0.6))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0)),
                             limits = range(panel_breaks),
                             labels = scientific_10,
                             breaks = panel_breaks) +
          theme_classic()+
          guides(fill = guide_legend(ncol = 4, byrow = T, order = 3))
     
     # rank panel
     data_rank <- data_age_top |>
          select(AgeGroup, AgeGroupID, Year_group, Year_mark, contains(c('Max_Cases_D', 'Max_Deaths_D')[i])) |>
          rename(Outcome = colnames(data_age_top)[i + 3]) |> 
          left_join(data_class, by = c('Outcome' = 'Shortname')) |>
          select(-c(Disease, Fullname)) |> 
          filter(AgeGroup != 'Total') |>
          mutate(Outcome_Fill = if_else(Outcome %in% c(max_disease, 'No deaths'), Outcome, 'Others'))
     
     data_outcome[[paste('panel', LETTERS[i*3], sep = '')]] <- data_rank
     
     fig_rank <- data_rank |>
          ggplot(aes(x = AgeGroupID, y = Year_mark, fill = Outcome_Fill)) +
          geom_tile(aes(width = 1, height = 1),
                    color = 'white',
                    show.legend = F) +
          geom_text(aes(label = Outcome), size = 3) +
          scale_fill_manual(values = fill_color_disease_max,
                            na.value = 'white') +
          coord_cartesian(ylim = c(length(unique(data_rank$Year_mark))+0.5, 0.5)) +
          scale_x_continuous(breaks = unique(data_rank$AgeGroupID),
                             labels = unique(data_rank$AgeGroup),
                             expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(breaks = unique(data_rank$Year_mark),
                             labels = unique(data_rank$Year_group),
                             expand = expansion(mult = c(0, 0)))+
          theme_bw()+
          theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                panel.grid = element_blank())
     
     if (i == 2) {
          fig_rank <- fig_rank +
               labs(title = LETTERS[i*3],
                    x = 'Age (years)',
                    y = 'Year')
          
          fig_cumulative <- fig_cumulative +
               theme(legend.position = 'none',
                     plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                     panel.grid = element_blank())+
               labs(title = LETTERS[i*3-1],
                    fill = 'Disease',
                    x = 'Age (years)',
                    y = ifelse(i == 1, 'Cumulative cases', 'Cumulative deaths'))
     } else {
          fig_rank <- fig_rank +
               labs(title = LETTERS[i*3],
                    x = NULL,
                    y = 'Year')
          
          fig_cumulative <- fig_cumulative +
               theme(legend.position = 'inside',
                     legend.justification = c(0, 1),
                     legend.position.inside = c(0.01, 1.1),
                     legend.title.position = 'top',
                     legend.background = element_rect(fill = 'transparent'),
                     plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                     panel.grid = element_blank())+
               labs(title = LETTERS[i*3-1],
                    fill = 'Disease',
                    x = NULL,
                    y = ifelse(i == 1, 'Cumulative cases', 'Cumulative deaths'))
     }
     
     data_rank |> 
          select(AgeGroup, Year_group, Outcome) |>
          pivot_wider(names_from = AgeGroup,
                      values_from = Outcome) |> 
          print()
     
     assign(paste('fig', i*3, sep = ''), fig_rank)
     assign(paste('fig', i*3-1, sep = ''), fig_cumulative)
}

## add inset of fig5
fig5_a <- fig5 +
     geom_rect(aes(xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 600),
               color = 'black',
               linewidth = 0.2,
               fill = NA)+
     inset_element(fig5 + 
                        coord_cartesian(xlim = c(0.5, 4), ylim = c(0, 600), clip = 'on')+
                        scale_y_continuous(breaks = seq(0, 600, 200),
                                           expand = expansion(mult = c(0, 0)))+
                        scale_x_discrete(limits = unique(data_age_cumulative$AgeGroup)[1:4],
                                         breaks = unique(data_cumulative$AgeGroup)[1:4],
                                         expand = expansion(add = c(0, 0.6))) +
                        theme(axis.title = element_blank(),
                              plot.background = element_rect(color = 'black', size = 0.5),
                              legend.position = 'none',
                              plot.title = element_blank()),
                   left = 0.03,
                   bottom = 0.2,
                   right = 0.7,
                   top = 1.1)

design <- "
AA
BC
DD
EF
"

fig <- free(fig1, side = 'l') + fig2 + fig3 + free(fig4, side = 'l') + fig5_a + fig6 +
     plot_layout(ncol = 2, widths = c(0.8, 1),
                 heights = c(1.2, 1, 1.2, 1),
                 design = design, guides = 'collect')&
     theme(plot.title.position = 'plot',
           legend.title.position = 'top',
           text = element_text(family = 'Times New Roman'),
           legend.position = 'bottom')

ggsave(filename = "../outcome/publish/fig2.png",
       plot = fig,
       width = 14,
       height = 12)

ggsave(filename = "../outcome/publish/fig2.pdf",
       plot = fig,
       width = 14,
       height = 12,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
write.xlsx(data_outcome,
           file = "../outcome/Publish/figure_data/fig2.xlsx")
