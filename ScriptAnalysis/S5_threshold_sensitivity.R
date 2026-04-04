# Supplementary Table S5 — Threshold Sensitivity Analysis
# Tests recovery status stability across threshold (90-95%) and persistence (2-4 months) combinations

library(tidyverse)
library(openxlsx)
library(lubridate)

Sys.setlocale("LC_TIME", "C")

# Load data
load("./temp/outcome.RData")
load("./temp/month.RData")
appendix_tables_dir <- file.path("..", "Outcome", "Appendix", "Tables")
dir.create(appendix_tables_dir, showWarnings = FALSE, recursive = TRUE)

data_class <- data_class |> 
  filter(Shortname %in% purrr::map_chr(outcome, ~ unique(.x$outcome_data$Shortname)[1]))

# Helper functions
get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) return(NA_real_)
  (year(end) - year(start)) * 12 + month(end) - month(start)
}

find_sustained_date <- function(dates, condition, persistence = 3) {
  if (length(dates) < persistence) return(as.Date(NA))
  for (i in seq_len(length(dates) - persistence + 1)) {
    if (all(condition[i:(i + persistence - 1)])) {
      return(dates[i])
    }
  }
  as.Date(NA)
}

calc_status_one <- function(dates,
                            observed,
                            expected,
                            start_date = as.Date("2020-01-01"),
                            recovery_threshold = 0.95,
                            persistence = 3) {
  keep <- dates >= start_date
  
  df <- tibble(
    date = dates[keep],
    observed = observed[keep],
    expected = expected[keep]
  ) |>
    arrange(date) |>
    mutate(
      diff = observed - expected,
      cum_diff = cumsum(diff),
      ratio = if_else(expected > 0, observed / expected, NA_real_)
    )
  
  if (nrow(df) == 0) {
    return(tibble(
      Status = "No Deficit",
      Date_Recovery = as.Date(NA),
      Date_Balance = as.Date(NA),
      Recovery_Months = NA_real_,
      Balance_Months = NA_real_
    ))
  }
  
  trough_idx <- which.min(df$cum_diff)
  trough_date <- df$date[trough_idx]
  max_deficit_raw <- df$cum_diff[trough_idx]
  
  first_drop_idx <- which(df$cum_diff < 0)[1]
  start_deficit_date <- if (!is.na(first_drop_idx)) df$date[first_drop_idx] else as.Date(NA)
  
  recovery_date <- as.Date(NA)
  status <- "No Deficit"
  
  if (!is.na(start_deficit_date) && max_deficit_raw < 0) {
    df_search <- df |> filter(date >= start_deficit_date)
    
    lag_base <- c(df_search$cum_diff[1], head(df_search$cum_diff, -1))
    is_recovered_trend <- df_search$observed >= df_search$expected * recovery_threshold
    is_paying_back <- (df_search$cum_diff - lag_base) >= 0
    
    recovery_date <- find_sustained_date(
      dates = df_search$date,
      condition = is_recovered_trend & is_paying_back,
      persistence = persistence
    )
    
    if (!is.na(recovery_date)) {
      status <- "Recovered"
    } else {
      status <- "Suppressed"
    }
  }
  
  balance_date <- as.Date(NA)
  if (!is.na(trough_date) && max_deficit_raw < 0) {
    df_post_trough <- df |> filter(date > trough_date)
    balance_idx <- which(df_post_trough$cum_diff >= 0)[1]
    if (!is.na(balance_idx)) {
      balance_date <- df_post_trough$date[balance_idx]
      status <- "Debt Repaid"
    }
  }
  
  tibble(
    Status = status,
    Date_Recovery = recovery_date,
    Date_Balance = balance_date,
    Recovery_Months = ifelse(is.na(recovery_date), NA_real_, get_months(as.Date("2020-01-01"), recovery_date)),
    Balance_Months = ifelse(is.na(balance_date), NA_real_, get_months(as.Date("2020-01-01"), balance_date))
  )
}

# Run sensitivity analysis across threshold × persistence grid
thresholds <- c(0.90, 0.95)
persistences <- c(2, 3, 4)

all_results <- list()

for (thresh in thresholds) {
  for (pers in persistences) {
    cat(sprintf("Running threshold=%.2f, persistence=%d\n", thresh, pers))
    
    res <- purrr::map_dfr(outcome, function(item) {
      shortname <- unique(item$outcome_data$Shortname)[1]
      calc_status_one(
        dates = item$outcome_data$date,
        observed = item$outcome_data$value,
        expected = item$outcome_data$median,
        recovery_threshold = thresh,
        persistence = pers
      ) |>
        mutate(Shortname = shortname)
    }) |>
      left_join(select(data_class, Shortname, Group), by = "Shortname") |>
      mutate(
        Threshold = thresh,
        Persistence = pers,
        Config = sprintf("%.0f%% / %d mo", thresh * 100, pers)
      ) |>
      select(Config, Threshold, Persistence, Shortname, Group, Status, Date_Recovery, Date_Balance, Recovery_Months, Balance_Months)
    
    all_results[[sprintf("T%.0f_P%d", thresh * 100, pers)]] <- res
  }
}

combined <- bind_rows(all_results)

# Summary: count status changes across configurations
status_summary <- combined |>
  group_by(Threshold, Persistence, Config, Status) |>
  summarise(Count = n(), .groups = "drop") |>
  mutate(Pct = round(Count / 24 * 100, 1))

# Identify diseases whose status changes across configurations
status_changes <- combined |>
  group_by(Shortname, Group) |>
  summarise(
    UniqueStatuses = n_distinct(Status),
    Statuses = paste(unique(Status), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(UniqueStatuses > 1) |>
  arrange(desc(UniqueStatuses))

# Save outputs
write.csv(combined, file.path(appendix_tables_dir, "Threshold_sensitivity_analysis.csv"), row.names = FALSE)
write.xlsx(combined, file.path(appendix_tables_dir, "Threshold_sensitivity_analysis.xlsx"), overwrite = TRUE)
write.csv(status_summary, file.path(appendix_tables_dir, "Threshold_sensitivity_summary.csv"), row.names = FALSE)
write.xlsx(status_summary, file.path(appendix_tables_dir, "Threshold_sensitivity_summary.xlsx"), overwrite = TRUE)

cat("\nThreshold sensitivity analysis complete.\n")
cat(sprintf("Status distribution:\n"))
print(status_summary)
cat(sprintf("\nDiseases with status changes (%d):\n", nrow(status_changes)))
print(status_changes)
