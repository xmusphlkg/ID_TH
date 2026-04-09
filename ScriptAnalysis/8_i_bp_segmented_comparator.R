#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(openxlsx)
  library(purrr)
  library(broom)
})

resolve_script_dir_early <- function() {
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

script_dir <- resolve_script_dir_early()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
setwd(script_dir)

source("./function/revision_utils.R")

load(file.path(script_dir, "temp", "outcome.RData"))

tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

fit_segmented_bp <- function(item) {
  od <- item$outcome_data |>
    arrange(date) |>
    transmute(
      date,
      Shortname = unique(Shortname),
      observed = value,
      expected = median
    ) |>
    filter(date >= as.Date("2020-01-01")) |>
    mutate(
      month_index = row_number() - 1L,
      diff = observed - expected,
      cum_diff = cumsum(diff)
    )

  primary <- calc_status_one(
    dates = od$date,
    observed = od$observed,
    expected = od$expected,
    start_date = as.Date("2020-01-01")
  )

  trough_idx <- which.min(od$cum_diff)
  trough_month <- od$month_index[trough_idx]
  trough_date <- od$date[trough_idx]

  if (all(od$cum_diff >= 0) || is.na(trough_month)) {
    return(tibble(
      Shortname = unique(od$Shortname),
      PrimaryStatus = primary$Status[[1]],
      PrimaryBPMonth = ifelse(is.na(primary$Date_Balance[[1]]), NA_real_, get_months(as.Date("2020-01-01"), primary$Date_Balance[[1]])),
      TroughMonth = trough_month,
      TroughDate = trough_date,
      PostSlope = NA_real_,
      SegmentedBPMonth = NA_real_,
      SegmentedBPDate = as.Date(NA),
      SegmentedBPAchieved = FALSE,
      BPAgreement = is.na(primary$Date_Balance[[1]])
    ))
  }

  fit_df <- od |>
    mutate(post_trough = pmax(0, month_index - trough_month))

  fit <- lm(cum_diff ~ month_index + post_trough, data = fit_df)
  coefs <- coef(fit)
  post_slope <- unname(coefs["month_index"] + coefs["post_trough"])
  intercept_post <- unname(coefs["(Intercept)"] - coefs["post_trough"] * trough_month)

  segmented_bp_month <- NA_real_
  segmented_bp_date <- as.Date(NA)

  if (is.finite(post_slope) && post_slope > 0) {
    segmented_bp_month <- ceiling((-intercept_post) / post_slope)
    if (is.finite(segmented_bp_month) && segmented_bp_month >= trough_month && segmented_bp_month <= max(fit_df$month_index)) {
      segmented_bp_date <- as.Date("2020-01-01") %m+% months(segmented_bp_month)
    } else {
      segmented_bp_month <- NA_real_
    }
  }

  primary_bp_month <- ifelse(is.na(primary$Date_Balance[[1]]), NA_real_, get_months(as.Date("2020-01-01"), primary$Date_Balance[[1]]))

  tibble(
    Shortname = unique(od$Shortname),
    PrimaryStatus = primary$Status[[1]],
    PrimaryBPMonth = primary_bp_month,
    TroughMonth = trough_month,
    TroughDate = trough_date,
    PostSlope = post_slope,
    SegmentedBPMonth = segmented_bp_month,
    SegmentedBPDate = segmented_bp_date,
    SegmentedBPAchieved = !is.na(segmented_bp_month),
    BPAgreement = xor(is.na(primary_bp_month), is.na(segmented_bp_month)) == FALSE &&
      (is.na(primary_bp_month) || abs(primary_bp_month - segmented_bp_month) <= 6)
  )
}

bp_comparator <- bind_rows(lapply(outcome, fit_segmented_bp)) |>
  mutate(
    BPMonthDelta = SegmentedBPMonth - PrimaryBPMonth,
    AgreementLabel = case_when(
      is.na(PrimaryBPMonth) & is.na(SegmentedBPMonth) ~ "Both unresolved",
      !is.na(PrimaryBPMonth) & !is.na(SegmentedBPMonth) & abs(BPMonthDelta) <= 6 ~ "Agree within 6 months",
      !is.na(PrimaryBPMonth) & is.na(SegmentedBPMonth) ~ "Primary only",
      is.na(PrimaryBPMonth) & !is.na(SegmentedBPMonth) ~ "Segmented only",
      TRUE ~ "Both reached, timing differs"
    )
  ) |>
  arrange(AgreementLabel, Shortname)

summary_table <- bp_comparator |>
  summarise(
    DiseasesAssessed = n(),
    PrimaryBalanced = sum(!is.na(PrimaryBPMonth), na.rm = TRUE),
    SegmentedBalanced = sum(!is.na(SegmentedBPMonth), na.rm = TRUE),
    AgreeWithin6Months = sum(AgreementLabel == "Agree within 6 months", na.rm = TRUE),
    BothUnresolved = sum(AgreementLabel == "Both unresolved", na.rm = TRUE),
    PrimaryOnly = sum(AgreementLabel == "Primary only", na.rm = TRUE),
    SegmentedOnly = sum(AgreementLabel == "Segmented only", na.rm = TRUE),
    TimingDiffers = sum(AgreementLabel == "Both reached, timing differs", na.rm = TRUE),
    MedianAbsBPMonthDelta = median(abs(BPMonthDelta), na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

summary_csv_path <- file.path(tables_dir, "BP_segmented_comparator_summary.csv")
detail_csv_path <- file.path(tables_dir, "BP_segmented_comparator.csv")
xlsx_path <- file.path(tables_dir, "BP_segmented_comparator.xlsx")

write_csv(summary_table, summary_csv_path)
write_csv(bp_comparator, detail_csv_path)

write.xlsx(
  list(
    Summary = summary_table,
    DiseaseLevel = bp_comparator
  ),
  file = xlsx_path,
  overwrite = TRUE
)

detail_table <- bp_comparator |>
  transmute(
    Shortname,
    `Primary BP month` = PrimaryBPMonth,
    `Segmented BP month` = SegmentedBPMonth,
    `Month delta` = BPMonthDelta,
    `Agreement` = AgreementLabel
  )

summary_block <- c(
  "# BP Segmented Comparator",
  "",
  "This attachment compares the primary BP rule with an exploratory segmented linear comparator fitted to the cumulative observed-minus-expected deviation curve using a fixed knot at the empirical trough.",
  "",
  "**Summary of BP agreement**",
  md_table(summary_table),
  "",
  "**Disease-level BP comparison**",
  md_table(detail_table),
  "",
  "Source files:",
  sprintf("- `%s`", paste0("./Tables/", basename(xlsx_path))),
  sprintf("- `%s`", paste0("./Tables/", basename(detail_csv_path)))
)

message("Segmented BP comparator outputs written:")
message(sprintf(" - %s", xlsx_path))
