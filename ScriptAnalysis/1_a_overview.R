# packages ----------------------------------------------------------------

library(nih.joinpoint)
library(openxlsx)
library(tidyverse)
library(ggsci)
library(paletteer)
library(patchwork)
library(furrr)

# System setting
Sys.setlocale(locale = "EN")

# future plan for parallel computing
plan(multisession, workers = 3)

# data --------------------------------------------------------------------

# loading function
source("./function/theme_set.R")

# read disease class data
data_class <- read.xlsx("../Data/TotalCasesDeaths.xlsx") |> 
     filter(Including == 1)|> 
     mutate(Group = factor(Group, levels = disease_groups)) |> 
     arrange(Group, desc(Cases)) |> 
     select(-c(Cases, Count, Including, Label)) 

# read population data
data_population <- read.xlsx('../Data/Population/1992-2023.xlsx', sheet = 'age') |> 
     select(YEAR, Total) |> 
     rename(Year = YEAR,
            Population = Total)

## monthly -----------------------------------------------------------------

# list files in the folder: month case and death data
list_disease_files <- list.files("../Data/CleanData/",
                                  pattern = "mcd.csv",
                                  full.names = T)
data_all <- lapply(list_disease_files, read.csv) |>
     bind_rows() |> 
     filter(Year < 2025, Year >= 2008)

rm(list_disease_files)

# filter the total cases and deaths, with the year after 2008
data_all |>
     filter(Areas == 'Total' & Month == 'Total') |>
     group_by(Disease) |>
     summarise(Cases = sum(Count),
               Count = n(),
               YearStart = min(Year),
               YearEnd = max(Year),
               .groups = 'drop') |> 
     write.csv(file = "../Outcome/TotalCasesDeaths.csv",
          row.names = F)

data_month <- data_all |>
     filter(Areas == 'Total' & Month != 'Total' & Disease %in% data_class$Disease) |>
     left_join(data_class, by = 'Disease') |> 
     # transform the Month: Jan -> 01
     mutate(Month = month(parse_date_time(Month, "b"), label = FALSE, abbr = FALSE),
            Group = factor(Group, levels = disease_groups),
            Disease = Shortname,
            Date = as.Date(paste(Year, Month, "01", sep = "-"))) |> 
     pivot_wider(names_from = Type, values_from = Count, values_fill = 0) |>
     ungroup() |> 
     arrange(Date, Disease) |> 
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e7,
            Mortality = (Deaths / Population) * 1e7)
## yearly -----------------------------------------------------------------

# list files in the folder: year case and death data
list_disease_files_year <- list.files("../Data/CleanData/",
                                       pattern = "rate.csv",
                                       full.names = T)

data_all_year <- lapply(list_disease_files_year, read.csv) |>
     bind_rows() |> 
     filter(Year < 2025, Year >= 2008)

rm(list_disease_files_year)

data_year_2 <- data_all_year |>
     filter(Areas == 'Total' & Disease %in% data_class$Disease) |>
     select(-Population, -Incidence, -Mortality, -Areas)

# estimate yearly data based on monthly data for checking purpose
data_year_1 <- data_all |>
     filter(Areas == 'Total' & Month == 'Total' & Disease %in% data_class$Disease) |>
     pivot_wider(names_from = Type, values_from = Count, values_fill = 0) |> 
     select(Year, Disease, Cases, Deaths)

# check the yearly data consistency
data_year_check <- data_year_2 |>
     select(Year, Disease, Cases, Deaths) |>
     full_join(data_year_1 |> 
                    rename(Cases_check = Cases, Deaths_check = Deaths),
               by = c('Year', 'Disease')) |>
     mutate(Cases_diff = Cases - Cases_check,
            Deaths_diff = Deaths - Deaths_check)

data_year_check |> 
     filter(Cases_diff != 0 | is.na(Cases_diff) | Deaths_diff != 0 | is.na(Deaths_diff))

# fill the missing yearly data based on monthly data
data_year <- data_year_check |>
     mutate(Cases = ifelse(is.na(Cases), Cases_check, Cases),
            Deaths = ifelse(is.na(Deaths), Deaths_check, Deaths)) |>
     select(-c(Cases_check, Deaths_check)) |> 
     left_join(data_class, by = 'Disease') |> 
     mutate(Group = factor(Group, levels = disease_groups),
            Disease = Shortname) |> 
     ungroup() |> 
     arrange(Year, Disease) |> 
     # make consistent cases and deaths
     mutate(Deaths = ifelse(is.na(Cases) & Deaths == 0, NA, Deaths)) |>
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e7,
            Mortality = (Deaths / Population) * 1e7)
     
# summary of NID ----------------------------------------------------------

## each group
data_month |>
     group_by(Group, Shortname) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               DateRange = paste(strftime(min(Date), "%Y/%m"),
                                 strftime(max(Date), "%Y/%m"),
                                 sep = " ~ "),
               .groups = "drop") |>
     group_by(Group) |>
     mutate(Cases_p = percent(round(Cases / sum(Cases), 4)),
            Deaths_p = percent(round(Deaths / sum(Deaths), 4)),
            CFR = percent(round(Deaths / Cases, 4)),
            .before = DateRange) |>
     arrange(Group, desc(Cases)) |> 
     print(n = Inf)

# overall trend of month --------------------------------------------------

fill_color_trend <- fill_color_continue[c(1, 3, 5)]
names(fill_color_trend) <- c("Observed", "STL Trend", "Joinpoint Trend")

data_month_total <- data_month |>
     group_by(Date, Year) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(Date) |> 
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e5,
            Mortality = (Deaths / Population) * 1e5,
            CFR = (Deaths / Cases) * 1000) |> 
     # format year and month to fit joinpoint model
     mutate(Month = month(Date),
            MonthIndex = Year * 12 + Month - min(data_month$Year) * 12,
            .after = Year)

## joinpoint model ----------------------------------------------------
# Using yearly data for joinpoint analysis to find the optimal number of joinpoints

# Options for joinpoint
run_opt = run_options(model="ln",
                      model_selection_method = 'bic',
                      ci_method = "parametric",
                      min_joinpoints = 0,
                      max_joinpoints = 4,
                      n_cores=4)
export_opt = export_options()

data_year_total <- data_year |>
     group_by(Year) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(Year) |> 
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e7,
            Mortality = (Deaths / Population) * 1e7,
            CFR = (Deaths / Cases) * 1000)

tasks <- list(
     list(data = data_year_total, x = "Year", y = "Incidence", label = 'Incidence', run_opt = run_opt, export_opt = export_opt),
     list(data = data_year_total, x = "Year", y = "Mortality", label = 'Mortality', run_opt = run_opt, export_opt = export_opt),
     list(data = data_year_total, x = "Year", y = "CFR", label = 'CFR', run_opt = run_opt, export_opt = export_opt)
)

jp_year_results <- future_map(
     tasks,
     ~ joinpoint(.x$data, x = .x$x, y = .x$y, run_opt = .x$run_opt, export_opt = .x$export_opt)
)

rm(tasks, run_opt, export_opt)

## Trend Component ----------------------------------------------------
# fit STL model to extract the trend component

stl_incidence <- stl(ts(data_month_total$Incidence, start = min(data_month_total$Year), frequency = 12),
                     s.window = "periodic", t.window = 24, robust = TRUE)
stl_mortality <- stl(ts(data_month_total$Mortality, start = min(data_month_total$Year), frequency = 12),
                     s.window = "periodic", t.window = 24, robust = TRUE)
stl_cfr <- stl(ts(data_month_total$CFR, start = min(data_month_total$Year), frequency = 12),
               s.window = "periodic", t.window = 24, robust = TRUE)

data_month_total <- data_month_total |> 
     mutate(Incidence_trend = stl_incidence$time.series[, 'trend'],
            Mortality_trend = stl_mortality$time.series[, 'trend'],
            CFR_trend = stl_cfr$time.series[, 'trend'])

## Extract Knots from Trend Component ------------------------------------------------
# Using trend component to find breakpoints (knots) based on the optimal K from yearly joinpoint analysis

extract_knots_from_trend <- function(jp_result, monthly_data, trend_var, month_index_var = "MonthIndex") {
     
     # Determine the number of joinpoints (K) from the yearly analysis result
     k_optimal <- jp_result$selected_model$model
     
     # Fit the base linear model using the Trend component
     formula_trend <- as.formula(paste0("log(", trend_var, ") ~ ", month_index_var))
     base_model_trend <- lm(formula_trend, data = monthly_data)
     
     # Fit segmented model to find knots
     if (k_optimal > 0) {
          tryCatch({
               seg_model_trend <- segmented::segmented(base_model_trend, 
                                                       seg.Z = as.formula(paste0("~", month_index_var)), 
                                                       npsi = k_optimal) # Let segmented find the best locations
               
               # Extract the estimated breakpoints (Est.)
               return(seg_model_trend$psi[, "Est."])
               
          }, error = function(e) {
               warning("Segmented convergence failed on trend data. Returning NULL.")
               return(NULL)
          })
     } else {
          return(NULL) # No breakpoints
     }
}

## Calculate APC using fixed knots ------------------------------------------------
# Using the knots obtained from trend component to fit segmented model on raw data and calculate APC with valid statistical inference

calculate_apc_statistics <- function(monthly_data, raw_var, knots, month_index_var = "MonthIndex") {
     
     # Fit base linear model on RAW data (Log-transformed)
     formula_raw <- as.formula(paste0("log(", raw_var, ") ~ ", month_index_var))
     base_model_raw <- lm(formula_raw, data = monthly_data)
     
     # Fit segmented model with FIXED knots (psi)
     if (!is.null(knots) && length(knots) > 0) {
          seg_model_final <- segmented::segmented(base_model_raw, 
                                                  seg.Z = as.formula(paste0("~", month_index_var)), 
                                                  psi = knots)
          
          # Use slope() to get valid statistical inference (Slope, SE, t-value)
          # The slope() function accounts for the covariance structure
          slope_summary <- segmented::slope(seg_model_final)[[month_index_var]]
          print(slope_summary)
          
          estimates <- slope_summary[, "Est."]
          std_errors <- slope_summary[, "St.Err."]
          t_values <- slope_summary[, "t value"]
          
          # Calculate P-values (Two-sided)
          df_resid <- summary(seg_model_final)$df[2]
          p_values <- 2 * pt(abs(t_values), df = df_resid, lower.tail = FALSE)
          
     } else {
          # Case: No joinpoints (Linear model)
          seg_model_final <- base_model_raw
          
          # Extract coefficients manually for linear model
          coef_summary <- summary(base_model_raw)$coefficients
          idx <- grep(month_index_var, rownames(coef_summary))
          
          estimates <- coef_summary[idx, "Estimate"]
          std_errors <- coef_summary[idx, "Std. Error"]
          t_values <- coef_summary[idx, "t value"]
          p_values <- coef_summary[idx, "Pr(>|t|)"]
          df_resid <- base_model_raw$df.residual
     }
     
     # Calculate APC and 95% Confidence Intervals
     # Step A: Calculate CI for the slope (beta) first
     alpha <- 0.05
     crit_val <- qt(1 - alpha/2, df = df_resid)
     
     slope_lower <- estimates - crit_val * std_errors
     slope_upper <- estimates + crit_val * std_errors
     
     # Step B: Transform slope to APC (Annual Percent Change)
     # Formula: APC = (exp(12 * beta) - 1) * 100
     apc_est <- (exp(12 * estimates) - 1) * 100
     apc_lower <- (exp(12 * slope_lower) - 1) * 100
     apc_upper <- (exp(12 * slope_upper) - 1) * 100
     
     # Compile Results Table
     min_idx <- min(monthly_data[[month_index_var]])
     max_idx <- max(monthly_data[[month_index_var]])
     all_indices <- sort(c(min_idx, knots, max_idx))
     
     res_df <- data.frame(
          Segment = seq_along(estimates),
          Slope = estimates,
          Slope_SE = std_errors,
          P_value = p_values,
          APC = apc_est,
          APC_LCI = apc_lower,
          APC_UCI = apc_upper,
          Start_Idx = ceiling(head(all_indices, -1)),
          End_Idx = floor(tail(all_indices, -1))
     )
     
     # Extract Fitted Values (transformed back to original scale)
     fitted_data <- monthly_data |> 
          select(Date, MonthIndex, all_of(raw_var)) |> 
          mutate(Fitted_Log = predict(seg_model_final),
                 Fitted_Rate = exp(Fitted_Log))
     
     return(list(
          model = seg_model_final,
          apc_results = res_df,
          fitted_data = fitted_data
     ))
}

# Wrapper function to connect the two steps
run_complete_pipeline <- function(item) {
     
     # Step 1: Find knots using the smooth TREND
     knots <- extract_knots_from_trend(jp_result = item$jp_result,
                                       monthly_data = item$data,
                                       trend_var = item$trend_var)
     
     # Step 2: Calculate stats using RAW data but FIXED knots
     results <- calculate_apc_statistics(monthly_data = item$data,
                                         raw_var = item$raw_var,
                                         knots = knots)
     
     return(results)
}

tasks <- list(
     list(jp_result = jp_year_results[[1]], 
          data = data_month_total, 
          trend_var = "Incidence_trend", 
          raw_var = "Incidence"),
     
     list(jp_result = jp_year_results[[2]], 
          data = data_month_total, 
          trend_var = "Mortality_trend", 
          raw_var = "Mortality"),
     
     list(jp_result = jp_year_results[[3]], 
          data = data_month_total, 
          trend_var = "CFR_trend", 
          raw_var = "CFR")
)

jp_segmented_results <- future_map(
     tasks,
     run_complete_pipeline,
     .options = furrr_options(seed = 20251127)
)

rm(tasks)

data_month_total <- data_month_total |> 
     left_join(jp_segmented_results[[1]]$fitted_data |> 
                    select(MonthIndex, Incidence_joinpoint = Fitted_Rate), 
               by = 'MonthIndex') |> 
     left_join(jp_segmented_results[[2]]$fitted_data |> 
                    select(MonthIndex, Mortality_joinpoint = Fitted_Rate), 
               by = 'MonthIndex') |>
     left_join(jp_segmented_results[[3]]$fitted_data |> 
                    select(MonthIndex, CFR_joinpoint = Fitted_Rate), 
               by = 'MonthIndex')

data_apc <- list(
     Incidence = jp_segmented_results[[1]]$apc_results,
     Mortality = jp_segmented_results[[2]]$apc_results,
     CFR = jp_segmented_results[[3]]$apc_results
) |> 
     bind_rows(.id = 'Measure') |> 
     mutate(across(c(APC, APC_LCI, APC_UCI), ~ round(., 2)),
            Measure = factor(Measure, levels = c('Incidence', 'Mortality', 'CFR')),
            StartDate = min(data_month_total$Date) + months(Start_Idx - 1),
            StartDateLabel = format(StartDate, "%Y/%m"),
            EndDate = min(data_month_total$Date) + months(End_Idx - 1),
            EndDateLabel = format(EndDate, "%Y/%m"),
            DateRange = paste(StartDateLabel, EndDateLabel, sep = "~"),
            P_value_Label = case_when(P_value < 0.001 ~ "***",
                                      P_value < 0.01 ~ "**",
                                      P_value < 0.05 ~ "*",
                                      TRUE ~ ""),
            APC_CI = paste0("(", APC_LCI, "%,", APC_UCI, "%)"))

## figure 1 ----------------------------------------------------------------

line_color <- c('Observed' = "#E64B35FF",
                'Trend' = "#B09C85FF",
                'Model' = "#00A087FF")

plot_breaks_apc <- pretty(data_apc$APC, n = 4)

fig1_data <- data_month_total |> 
     select(Date, Cases, Incidence, Incidence_trend, Incidence_joinpoint)

plot_breaks_1 <- pretty(c(fig1_data$Incidence, fig1_data$Incidence_trend), n = 4)

fig1 <- ggplot(data = fig1_data)+
     geom_rect(data = data_apc |> filter(Measure == 'Incidence'),
               mapping = aes(xmin = StartDate, xmax = EndDate, ymin = 0, ymax = -0.2*max(plot_breaks_1), fill = APC),
               inherit.aes = FALSE) +
     geom_text(data = data_apc |> filter(Measure == 'Incidence'),
               mapping = aes(x = StartDate + (EndDate - StartDate) / 2,
                             y = -0.1*max(plot_breaks_1),
                             label = paste0(APC, "%", P_value_Label, "\n", APC_CI)),
                                            
               size = 3,
               vjust = 0.5,
               color = 'black',
               inherit.aes = FALSE) +
     geom_point(mapping = aes(x = Date, y = Incidence, color = 'Observed'),
                alpha = 0.5) +
     geom_line(mapping = aes(x = Date, y = Incidence_trend, color = 'Trend'), linewidth = 1) +
     geom_line(mapping = aes(x = Date, y = Incidence_joinpoint, color = 'Model'), linewidth = 1) +
     scale_color_manual(values = line_color,
                        name = 'Rate',
                        breaks = names(line_color)) +
     scale_fill_gradientn(colors = paletteer_d("Redmonder::dPBIRdGn", direction = -1),
                          name = "APC (95%CI)",
                          limits = range(plot_breaks_apc),
                          breaks = plot_breaks_apc) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        breaks = plot_breaks_1,
                        limits = c(-0.2, 1)*max(plot_breaks_1)) +
     theme_bw() +
     theme(legend.position = 'none',
           panel.grid = element_blank()) +
     labs(y = "Monthly incidence rate",
          x = 'Date',
          title = 'A')

## figure 2 ----------------------------------------------------------------

fig2_data <- data_month_total |> 
     select(Date, Deaths, Mortality, Mortality_trend, Mortality_joinpoint)

plot_breaks_2 <- pretty(c(fig2_data$Mortality, fig2_data$Mortality_trend), n = 4)

fig2 <- ggplot(data = fig2_data)+
     geom_rect(data = data_apc |> filter(Measure == 'Mortality'),
               mapping = aes(xmin = StartDate, xmax = EndDate, ymin = 0, ymax = -0.2*max(plot_breaks_2), fill = APC),
               inherit.aes = FALSE) +
     geom_text(data = data_apc |> filter(Measure == 'Mortality'),
               mapping = aes(x = StartDate + (EndDate - StartDate) / 2,
                             y = -0.1*max(plot_breaks_2),
                             label = paste0(APC, "%", P_value_Label, "\n", APC_CI)),
               size = 3,
               color = 'black',
               inherit.aes = FALSE) +
     geom_point(mapping = aes(x = Date, y = Mortality, color = 'Observed'),
                alpha = 0.5) +
     geom_line(mapping = aes(x = Date, y = Mortality_trend, color = 'Trend'), linewidth = 1) +
     geom_line(mapping = aes(x = Date, y = Mortality_joinpoint, color = 'Model'), linewidth = 1) +
     scale_color_manual(values = line_color,
                        breaks = names(line_color)) +
     scale_fill_gradientn(colors = paletteer_d("Redmonder::dPBIRdGn", direction = -1),
                          limits = range(plot_breaks_apc),
                          breaks = plot_breaks_apc,
                          labels = plot_breaks_apc) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0))) +
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        breaks = plot_breaks_2,
                        limits = c(-0.2, 1)*max(plot_breaks_2)) +
     theme_bw() +
     theme(legend.position = 'inside',
           legend.box = 'horizontal',
           legend.direction = 'horizontal',
           legend.position.inside = c(0.99, 0.99),
           legend.justification = c(1, 1),
           panel.grid = element_blank()) +
     labs(y = "Monthly mortality rate",
          x = 'Date',
          color = 'Rate (per 100,000)',
          fill = "APC (%)",
          title = 'B')+
     guides(color = guide_legend(nrow = 1, order = 1, byrow = TRUE, title.position = "top"),
            fill = guide_colorbar(order = 2, barwidth = 10, barheight = 1, title.position = "top"))

# group and disease -----------------------------------------------

names(fill_color) <- disease_groups

## figure 3 ----------------------------------------------------------------

data_group <- data_month |>
     group_by(Group, Date, Year) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |> 
     # add population
     left_join(data_population, by = 'Year') |>
     # calculate the rate per million population
     mutate(Incidence = (Cases / Population) * 1e5,
            Mortality = (Deaths / Population) * 1e5) |>
     arrange(Group, desc(Cases))

fig3_data <- data_group |> 
     select(Date, Group, Incidence)

plot_breaks_3 <- c(log10(0.1), 0, 1, 2, 3)

fig3 <- ggplot(data = fig3_data)+
     geom_line(mapping = aes(x = Date, y = Incidence, color = Group),
              show.legend = F) +
     scale_color_manual(values = fill_color,
                        breaks = names(fill_color)) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0))) +
     scale_y_continuous(expand = c(0, 0),
                        limits = 10^(range(plot_breaks_3)),
                        breaks = 10^plot_breaks_3,
                        # force label to normal format
                        labels = scales::number(10^plot_breaks_3),
                        trans = 'log10')+
     theme_bw()+
     theme(legend.position = 'none',
           panel.grid = element_blank())+
     labs(y = "Monthly incidence (per 100,000)",
          x = 'Date',
          title = 'C')

## figure 4 ----------------------------------------------------------------

fig4_data <- data_group |> 
     select(Group, Date, Mortality)

# pretty(log10(fig4_data$Mortality), n = 4)
plot_breaks_4 <- pretty(fig4_data$Mortality, n = 4)

fig4 <- ggplot(data = fig4_data)+
     geom_line(mapping = aes(x = Date, y = Mortality, color = Group),
              show.legend = T) +
     scale_color_manual(values = fill_color) +
     scale_x_date(date_breaks = "2 year",
                  date_labels = "%Y",
                  expand = expansion(mult = c(0, 0))) +
     scale_y_continuous(expand = c(0, 0),
                        breaks = plot_breaks_4,
                        limits = range(plot_breaks_4))+
     theme_bw()+
     theme(legend.position = 'inside',
           legend.background = element_rect(fill = "transparent", color = NA),
           legend.position.inside = c(0.99, 0.99),
           legend.justification = c(1, 1),
           panel.grid = element_blank())+
     guides(color = guide_legend(ncol = 2, byrow = TRUE))+
     labs(y = "Monthly mortality (per 100,000)",
          x = 'Date',
          color = "Disease categories",
          title = 'D')

# heatmap ------------------------------------------------------------------

data_heat <- data_year |> 
     select(Disease, Group, Year, Incidence, Mortality, Cases, Deaths) |>
     group_by(Group, Disease) |>
     mutate(Incidence_normal = (Incidence - min(Incidence)) / sd(Incidence),
            Mortality_normal = (Mortality - min(Mortality)) / sd(Mortality),
            Incidence_normal = ifelse(Incidence == 0, 0, Incidence_normal),
            Mortality_normal = ifelse(Mortality == 0, 0, Mortality_normal))

## figure 5 ----------------------------------------------------------------

fig5_data <- data_year |> 
     select(Disease, Group, Year, Cases) |> 
     group_by(Group, Disease) |>
     summarise(TotalCases = sum(Cases),
               .groups = 'drop') |> 
     arrange(desc(TotalCases))

plot_breaks_5 <- pretty(fig5_data$TotalCases/10**6, n = 4)

fig5 <- ggplot(data = fig5_data)+
     geom_bar(mapping = aes(x = TotalCases/10**6, y = Disease, fill = Group),
              stat = 'identity',
              show.legend = T)+
     scale_x_continuous(expand = c(0, 0),
                        trans = 'reverse',
                        limits = range(plot_breaks_5),
                        breaks = plot_breaks_5)+
     scale_y_discrete(expand = c(0, 0),
                      limits = rev(fig5_data$Disease))+
     scale_fill_manual(values = fill_color) +
     theme_bw()+
     theme(legend.position = 'bottom',
           panel.grid.minor = element_blank(),
           plot.margin = margin(5, 0, 5, 10),
           axis.ticks.y = element_blank(),
           axis.text.y = element_blank())+
     labs(y = NULL,
          x = "Cumulative cases (\u00D710\u2076)",
          fill = "Disease categories",
          title = 'E: Cumulative cases')+
     guides(fill = guide_legend(nrow = 1, byrow = TRUE))

fig5_a <- fig5_data |> 
     mutate(TotalCasesGroup = cut(TotalCases,
                                   breaks = c(0, 1e3, 1e4, 1e5, Inf),
                                   labels = c('<1k', '1k-10k', '10k-100k', '>100k'))) |>
     ggplot()+
     geom_tile(mapping = aes(x = '1', y = Disease, fill = TotalCasesGroup),
               color = "white")+
     scale_fill_manual(values = paletteer_d("MapPalettes::sunset", direction = -1)[-3])+
     scale_y_discrete(limits = rev(fig5_data$Disease),
                      expand = c(0, 0))+
     scale_x_discrete(expand = c(0, 0))+
     theme_bw()+
     theme(axis.ticks = element_blank(),
           axis.text = element_blank(),
           plot.margin = margin(5, 5, 5, 0),
           legend.position = 'bottom')+
     labs(y = NULL,
          x = NULL,
          fill = "Cumulative cases")

## figure 6 ----------------------------------------------------------------

fig6_data <- data_heat |> 
     ungroup() |> 
     select(Disease, Year, Incidence, Incidence_normal) |> 
     complete(Disease = unique(fig5_data$Disease),
              Year = seq(min(Year), max(Year)),
              fill = list(Incidence = NA, Incidence_normal = NA))

plot_breaks_6 <- pretty(fig6_data$Incidence_normal, n = 4)

fig6 <- ggplot(data = fig6_data)+
     geom_tile(mapping = aes(x = Year, y = Disease, fill = Incidence_normal),
               show.legend = T)+
     scale_x_continuous(breaks = seq(min(fig6_data$Year), max(fig6_data$Year), by = 2),
                        expand = c(0, 0))+
     scale_y_discrete(expand = c(0, 0),
                      limits = rev(fig5_data$Disease),
                      position = "right")+
     scale_fill_gradientn(colors = paletteer_d("LaCroixColoR::Lemon", direction = -1),
                          na.value = "white",
                          name = "Normalized rate",
                          limits = range(plot_breaks_6),
                          breaks = plot_breaks_6,
                          labels = round(plot_breaks_6, 2))+
     theme_bw()+
     theme(legend.position = 'bottom',
           plot.margin = margin(5, 0, 5, 0),
           axis.ticks.y = element_blank(),
           legend.title.position = "top",
           axis.text.y = element_blank())+
     labs(y = NULL,
          x = 'Year',
          title = 'F: Normalized incidence rate')+
     guides(fill = guide_colorbar(barwidth = 35, barheight = 1))

## figure 8 --------------------------------------------------------------

fig8_data <- data_year |> 
     select(Disease, Group, Year, Deaths) |> 
     group_by(Group, Disease) |>
     summarise(TotalDeaths = sum(Deaths),
               .groups = 'drop') |> 
     arrange(desc(TotalDeaths))

plot_breaks_8 <- pretty(fig8_data$TotalDeaths/1e4, n = 4)

fig8 <- ggplot(data = fig8_data)+
     geom_bar(mapping = aes(x = TotalDeaths/1e4, y = Disease, fill = Group),
              stat = 'identity',
              show.legend = F)+
     scale_x_continuous(expand = c(0, 0),
                        limits = range(plot_breaks_8),
                        breaks = plot_breaks_8)+
     scale_y_discrete(expand = c(0, 0),
                      limits = rev(fig8_data$Disease))+
     scale_fill_manual(values = fill_color) +
     theme_bw()+
     theme(legend.position = 'bottom',
           plot.margin = margin(5, 10, 5, 0),
           panel.grid.minor = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.y = element_blank())+
     labs(y = NULL,
          x = "Cumulative deaths (\u00D710\u2074)",
          fill = "Disease categories")

fig8_a <- fig8_data |> 
     mutate(TotalDeathsGroup = cut(TotalDeaths,
                                 breaks = c(-1, 10, 1e2, 1e3, Inf),
                                 labels = c('<10', '10-100', '100-1k', '>1k'))) |>
     ggplot()+
     geom_tile(mapping = aes(x = '1', y = Disease, fill = TotalDeathsGroup),
               color = "white")+
     scale_fill_manual(values = paletteer_d("MapPalettes::the_joker")[-3])+
     scale_y_discrete(limits = rev(fig8_data$Disease))+
     scale_x_discrete(expand = c(0, 0))+
     theme_bw()+
     theme(axis.ticks = element_blank(),
           axis.text = element_blank(),
           plot.margin = margin(5, 0, 5, 5),
           axis.line = element_blank(),
           legend.position = 'bottom')+
     labs(y = NULL,
          x = NULL,
          fill = "Cumulative deaths",
          title = 'I: Cumulative deaths')

## figure 7 ----------------------------------------------------------------

fig7_data <- data_heat |> 
     ungroup() |>
     select(Disease, Year, Mortality, Mortality_normal) |> 
     complete(Disease = unique(fig8_data$Disease),
              Year = seq(min(Year), max(Year)),
              fill = list(Mortality = NA, Mortality_normal = NA))

plot_breaks_7 <- pretty(fig7_data$Mortality_normal, n = 4)

fig7 <- ggplot(data = fig7_data)+
     geom_tile(mapping = aes(x = Year, y = Disease, fill = Mortality_normal),
              show.legend = F)+
     scale_x_continuous(breaks = seq(min(fig7_data$Year), max(fig7_data$Year), by = 2),
                        expand = c(0, 0))+
     scale_y_discrete(expand = c(0, 0),
                      limits = rev(fig8_data$Disease))+
     scale_fill_gradientn(colors = paletteer_d("LaCroixColoR::Lemon", direction = -1),
                          name = "Normalized rate",
                          na.value = "white",
                          limits = range(plot_breaks_7),
                          breaks = plot_breaks_7,
                          labels = round(plot_breaks_7, 2))+
     theme_bw()+
     theme(legend.position = 'bottom',
           plot.margin = margin(5, 0, 5, 0),
           legend.title.position = "top",
           axis.ticks.y = element_blank(),
           axis.text.y = element_blank())+
     labs(y = NULL,
          x = 'Year',
          title = 'H: Normalized motality rate')+
     guides(fill = guide_colorbar(barwidth = 35, barheight = 1))

# connection --------------------------------------------------------------

library(ggbump)

data_connect <- fig5_data |> 
     mutate(cases_rank = rank(TotalCases, ties.method = 'first')) |>
     left_join(fig8_data |>
                    mutate(deaths_rank = rank(TotalDeaths, ties.method = 'first')),
               by = c('Disease', 'Group')) |> 
     select(Disease, Group, cases_rank, deaths_rank) |> 
     mutate(x_start = 1.7,
            x_end = 3.3)

fig_connect <- ggplot(data = data_connect)+
     geom_sigmoid(mapping = aes(x = x_start, y = cases_rank, xend = x_end, yend = deaths_rank,
                                color = Group,
                                group = Disease),
                  smooth = 5,
                  alpha = 0.5,
                  linewidth = 1.5,
                  show.legend = F) +
     geom_tile(mapping = aes(x = 1, y = cases_rank, fill = Group),
               color = 'white',
               width = 1.4,
               alpha = 0.5,
               show.legend = F)+
     geom_tile(mapping = aes(x = 4, y = deaths_rank, fill = Group),
               color = 'white',
               width = 1.4,
               alpha = 0.5,
               show.legend = F)+
     geom_text(mapping = aes(x = 0.5, y = cases_rank, label = Disease),
               color = 'black',
               hjust = 0,
               size = 3)+
     geom_text(mapping = aes(x = 4.5, y = deaths_rank, label = Disease),
               color = 'black',
               hjust = 1,
               size = 3)+
     scale_fill_manual(values = fill_color)+
     scale_color_manual(values = fill_color)+
     coord_cartesian(xlim = c(0.4, 4.6),
                     ylim = c(0.5, nrow(data_connect) + 0.5),
                     expand = F)+
     theme_bw()+
     theme(axis.ticks = element_blank(),
           axis.text = element_blank(),
           axis.title = element_blank(),
           panel.grid = element_blank(),
           legend.position = 'none',
           plot.background = element_blank(),
           panel.background = element_blank(),
           plot.margin = margin(5, 0, 5, 0))+
     labs(title = 'G')

# save --------------------------------------------------------------------

fig <- cowplot::plot_grid(fig1 + fig2 + fig3 + fig4 + plot_layout(nrow = 2)&
                               theme(legend.title.position = "top",
                                     plot.title = element_text(face = 'bold', size = 14, hjust = 0)),
                          fig5 + fig5_a + fig6 + fig_connect + fig7 + fig8_a + fig8 + 
                               plot_layout(nrow = 1, widths = c(0.9, 0.1, 1.5, 2, 1.5, 0.1, 0.9), guides = 'collect', axes = 'collect') &
                               theme(legend.position = "bottom",
                                     legend.box = 'vertical',
                                     plot.title = element_text(face = 'bold', size = 14, hjust = 0),
                                     legend.title.position = "top"),
                          nrow = 2,
                          ncol = 1,
                          rel_heights = c(2, 3))

ggsave(filename = "../Outcome/Publish/fig1.png",
       fig,
       width = 14,
       height = 18)

ggsave(filename = "../Outcome/Publish/fig1.pdf",
       fig,
       width = 14,
       height = 18,
       device = cairo_pdf,
       family = "Times New Roman")

# figure data
data_fig <- list("panel A" = fig1_data,
                 "panel B" = fig2_data,
                 "panel C" = fig3_data,
                 "panel D" = fig4_data,
                 "panel E" = fig5_data,
                 "panel F" = fig6_data,
                 "panel G" = fig7_data,
                 "panel H" = fig8_data)

write.xlsx(data_fig,
           file = "../Outcome/Publish/figure_data/fig1.xlsx")

## save apc table ---------------------------------------------------------------

write.xlsx(data_apc |> 
                select(DateRange, Measure, APC, APC_LCI, APC_UCI, P_value_Label),
           file = "../Outcome/Appendix/Joinpoint_APC_results.xlsx")

# appendix ----------------------------------------------------------------

source("./1_b_appendix.R")

# extract stl trend of each time series

plot_trend <- function(data, title, value = 'Incidence') {
     ts_data <- ts(data[[value]], start = min(data$Year), frequency = 12)
     stl_data <- stl(ts_data, s.window = "periodic", t.window = 24, robust = T)
     data_trend <- data.frame(Year = data$Year,
                              Month = data$Month,
                              Value = data[[value]],
                              Trend = stl_data$time.series[, 'trend']) |> 
          mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
     
     # browser()
     plot_breaks_y <- pretty(c(data_trend$Value, data_trend$Trend), n = 4)
     
     fig <- ggplot(data = data_trend) +
          geom_line(mapping = aes(x = Date, y = Trend),
                    color = fill_color[1]) +
          geom_point(mapping = aes(x = Date, y = Value),
                     color = fill_color[2],
                     alpha = 0.5) +
          scale_x_date(date_breaks = "4 year",
                       date_labels = "%Y",
                       expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(expand = expansion(mult = c(0, 0)),
                             limits = range(plot_breaks_y),
                             breaks = plot_breaks_y,
                             labels = ifelse(max(ts_data) >= 100, scientific_10, scales::comma)) +
          theme_bw() +
          labs(x = 'Date',
               y = paste(value, '(per 100,000)'),
               title = title)
     
     return(fig)
}

## incidence ----------------------------------------------------

for (i in 1:length(disease_groups)) {
     g <- disease_groups[i]
     d <- data_class$Shortname[data_class$Group == g]
     
     # panel A: trend of group cases
     data_group <- data_month |>
          filter(Group == g) |>
          group_by(Date, Year, Month) |>
          summarise(Cases = sum(Cases),
                    Deaths = sum(Deaths),
                    .groups = 'drop') |> 
          # add population
          left_join(data_population, by = 'Year') |>
          # calculate the rate per million population
          mutate(Incidence = (Cases / Population) * 1e5,
                 Mortality = (Deaths / Population) * 1e5,
                 CFR = (Deaths / Cases) * 1000)
     fig_case_g <- plot_trend(data_group, paste(LETTERS[3], g, sep = ": "), 'Incidence')
     
     # panel B: trend of each disease
     data_single <- data_month |>
          filter(Group == g)
     fig_cases <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 3], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Incidence')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/Cases ", g, ".png"),
            cowplot::plot_grid(
                 plot_list[[i]],
                 fig_case_g + fig_cases + plot_layout(ncol = 4),
                 ncol = 1
                 ),
            device = "png",
            width = 14,
            height = 7 + ceiling(length(d) / 4) * 2 + 1,
            limitsize = FALSE,
            dpi = 300)
     
     # panel C: trend of group deaths
     fig_deaths_g <- plot_trend(data_group, paste(LETTERS[3], g, sep = ": "), 'Mortality')
     
     # panel D: trend of each disease
     fig_deaths <- lapply(d, function(x) {
          title_single <- paste(LETTERS[which(d == x) + 3], x, sep = ": ")
          plot_trend(data_single |> filter(Shortname == x),
                     title_single,
                     'Mortality')
     })
     
     ggsave(filename = paste0("../Outcome/Appendix/Supplementary Appendix 1_1/Deaths ", g, ".png"),
            fig_deaths_g + fig_deaths + plot_layout(ncol = 4),
            device = "png",
            width = 14,
            height = ceiling(length(d) / 4) * 2 + 1,
            limitsize = FALSE,
            dpi = 300)
}

# save plot ---------------------------------------------------------------

save(data_month, data_year,
     data_month_total, data_year_total,
     jp_year_results, jp_segmented_results,
     data_class, data_population, file = "./month.RData")
