suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
})

resolve_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_hits <- grep(paste0("^", file_arg), args, value = TRUE)

  if (length(file_hits) > 0) {
    script_path <- sub(file_arg, "", file_hits[1])
    if (!is.na(script_path) && nzchar(script_path)) {
      return(dirname(normalizePath(path.expand(script_path), winslash = "/", mustWork = FALSE)))
    }
  }

  if (dir.exists("ScriptAnalysis")) {
    return(normalizePath("ScriptAnalysis", winslash = "/", mustWork = FALSE))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

resolve_project_root <- function(script_dir = resolve_script_dir()) {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
}

get_months <- function(start, end) {
  if (is.na(start) || is.na(end)) {
    return(NA_real_)
  }

  (year(end) - year(start)) * 12 + month(end) - month(start)
}

find_sustained_date <- function(dates, condition, persistence = 3L) {
  condition[is.na(condition)] <- FALSE

  if (length(dates) < persistence) {
    return(as.Date(NA))
  }

  for (i in seq_len(length(dates) - persistence + 1L)) {
    if (all(condition[i:(i + persistence - 1L)])) {
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
                            persistence = 3L) {
  keep <- dates >= start_date

  df <- tibble(
    date = dates[keep],
    observed = observed[keep],
    expected = expected[keep]
  ) |>
    arrange(date) |>
    mutate(
      diff = observed - expected,
      cum_diff = cumsum(diff)
    )

  if (nrow(df) == 0) {
    return(tibble(
      Status = "No Deficit",
      Date_Start_Deficit = as.Date(NA),
      Date_Trough = as.Date(NA),
      Date_Recovery = as.Date(NA),
      Date_Balance = as.Date(NA),
      Suppression_Months = NA_real_,
      Payback_Months = NA_real_
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
    df_search <- df |>
      filter(date >= start_deficit_date)

    lag_base <- c(df_search$cum_diff[1], head(df_search$cum_diff, -1))
    is_recovered_trend <- df_search$observed >= df_search$expected * recovery_threshold
    is_paying_back <- (df_search$cum_diff - lag_base) >= 0

    recovery_date <- find_sustained_date(
      dates = df_search$date,
      condition = is_recovered_trend & is_paying_back,
      persistence = persistence
    )

    status <- ifelse(is.na(recovery_date), "Suppressed", "Recovered")
  }

  balance_date <- as.Date(NA)
  if (!is.na(trough_date) && max_deficit_raw < 0) {
    df_post_trough <- df |>
      filter(date > trough_date)

    balance_idx <- which(df_post_trough$cum_diff >= 0)[1]
    if (!is.na(balance_idx)) {
      balance_date <- df_post_trough$date[balance_idx]
      status <- "Debt Repaid"
    }
  }

  suppression_months <- get_months(start_deficit_date, recovery_date)
  payback_months <- get_months(trough_date, balance_date)

  if (status == "Suppressed" && !is.na(start_deficit_date)) {
    suppression_months <- get_months(start_deficit_date, max(df$date))
  }

  tibble(
    Status = status,
    Date_Start_Deficit = start_deficit_date,
    Date_Trough = trough_date,
    Date_Recovery = recovery_date,
    Date_Balance = balance_date,
    Suppression_Months = suppression_months,
    Payback_Months = payback_months
  )
}

status_label_map <- c(
  "Debt Repaid" = "Balanced",
  "Recovered" = "Recovered but not balanced",
  "Suppressed" = "Suppressed",
  "No Deficit" = "No deficit"
)

escape_md <- function(x) {
  x <- ifelse(is.na(x), "NA", as.character(x))
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("[\r\n]+", " ", x)
  x
}

md_table <- function(df) {
  headers <- names(df)
  lines <- c(
    paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
  )

  if (nrow(df) == 0) {
    return(lines)
  }

  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(escape_md(row), collapse = " | "), " |")
  })

  c(lines, rows)
}

replace_or_append_block <- function(lines, block_id, replacement_lines) {
  begin_marker <- paste0("<!-- BEGIN ", block_id, " -->")
  end_marker <- paste0("<!-- END ", block_id, " -->")
  wrapped_replacement <- c(begin_marker, replacement_lines, end_marker)

  begin_idx <- which(trimws(lines) == begin_marker)[1]
  end_idx <- which(trimws(lines) == end_marker)[1]

  if (!is.na(begin_idx) && !is.na(end_idx) && end_idx > begin_idx) {
    return(c(
      if (begin_idx > 1) lines[seq_len(begin_idx - 1)] else character(),
      wrapped_replacement,
      if (end_idx < length(lines)) lines[(end_idx + 1):length(lines)] else character()
    ))
  }

  c(
    lines,
    "",
    "<div style=\"page-break-after: always;\"></div>",
    "",
    wrapped_replacement
  )
}

correct_circular_shift <- function(x) {
  ifelse(x > 6, x - 12, ifelse(x < -6, x + 12, x))
}

get_peak_month_max <- function(df) {
  df |>
    slice_max(avg_value, n = 1, with_ties = FALSE) |>
    pull(month)
}

get_peak_month_com <- function(df) {
  theta <- 2 * pi * (df$month - 1) / 12
  x <- sum(df$avg_value * cos(theta), na.rm = TRUE)
  y <- sum(df$avg_value * sin(theta), na.rm = TRUE)
  peak_month <- round((atan2(y, x) * 12) / (2 * pi)) + 1
  ((peak_month - 1) %% 12) + 1
}
