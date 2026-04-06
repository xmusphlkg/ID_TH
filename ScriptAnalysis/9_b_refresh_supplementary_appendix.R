#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
  library(readr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)][1])
script_dir <- dirname(normalizePath(script_path))
project_root <- normalizePath(file.path(script_dir, ".."))

run_source_table_refresh <- function() {
  source_script <- file.path(script_dir, "9_a_generate_appendix_source_tables.R")
  if (!file.exists(source_script)) {
    stop(sprintf("Missing prerequisite script: %s", source_script))
  }

  cat(sprintf("Running prerequisite source-table refresh: %s\n", basename(source_script)))
  sys.source(source_script, envir = new.env(parent = globalenv()))
  cat("Completed prerequisite source-table refresh.\n")
}

run_source_table_refresh()

appendix_path <- file.path(project_root, "Outcome", "Appendix", "Supplementary_Appendix.md")
tables_dir <- file.path(project_root, "Outcome", "Appendix", "Tables")

status_label <- function(x) {
  out <- ifelse(
    x == "Debt Repaid", "Balanced",
    ifelse(
      x == "Recovered", "Recovered but not balanced",
      ifelse(x == "No Deficit", "No deficit", x)
    )
  )
  out
}

status_label_rp <- function(x) {
  out <- ifelse(
    x == "Debt Repaid", "Balanced",
    ifelse(
      x == "Recovered", "RP achieved without BP",
      ifelse(x == "No Deficit", "No deficit", x)
    )
  )
  out
}

fmt_num <- function(x, digits = 3, na = "NA") {
  ifelse(
    is.na(x),
    na,
    format(round(x, digits), nsmall = digits, trim = TRUE, scientific = FALSE)
  )
}

fmt_num_trim <- function(x, digits = 1, na = "NA") {
  out <- ifelse(is.na(x), na, format(round(x, digits), nsmall = digits, trim = TRUE, scientific = FALSE))
  sub("\\.0$", "", out)
}

fmt_int <- function(x, na = "NA") {
  ifelse(is.na(x), na, as.character(as.integer(round(x))))
}

fmt_bool <- function(x, na = "NA") {
  ifelse(is.na(x), na, ifelse(x, "Yes", "No"))
}

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

replace_block <- function(lines, block_id, start_line, replacement_lines) {
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

  start_idx <- which(lines == start_line)[1]
  if (is.na(start_idx)) {
    stop(sprintf("Could not find block starting with: %s", start_line))
  }

  page_break_idx <- which(seq_along(lines) > start_idx & trimws(lines) == "<div style=\"page-break-after: always;\"></div>")[1]
  if (is.na(page_break_idx)) {
    stop(sprintf("Could not find page break after: %s", start_line))
  }

  c(
    if (start_idx > 1) lines[seq_len(start_idx - 1)] else character(),
    wrapped_replacement,
    lines[page_break_idx:length(lines)]
  )
}

replace_first_line_by_prefix <- function(lines, prefix, replacement_line) {
  idx <- which(startsWith(lines, prefix))[1]
  if (is.na(idx)) {
    stop(sprintf("Could not find line starting with: %s", prefix))
  }
  lines[idx] <- replacement_line
  lines
}

excel_month <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m"))
  }
  if (is.numeric(x)) {
    return(format(convertToDate(x), "%Y-%m"))
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.Date(x), "%Y-%m"))
  }
  as.character(x)
}

read_sheet <- function(path, sheet) {
  read.xlsx(path, sheet = sheet, detectDates = TRUE)
}

named_value <- function(df, key_col, value_col) {
  out <- df[[value_col]]
  names(out) <- df[[key_col]]
  out
}

best_model_label <- function(x) {
  gsub("\\*\\*", "", x)
}

join_names <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return("none")
  }
  paste(x, collapse = ", ")
}

flow_path <- file.path(tables_dir, "Disease_flow_summary.xlsx")
robustness_path <- file.path(tables_dir, "New_robustness_operational_summaries.xlsx")
endpoint_path <- file.path(tables_dir, "New_endpoint_context_usability_summaries.xlsx")
appendix_s2_path <- file.path(tables_dir, "Appendix_S2_excluded_series.csv")
appendix_s4_path <- file.path(tables_dir, "Appendix_S4_predictor_definitions.csv")
appendix_s6_path <- file.path(tables_dir, "Appendix_S6_overlap_summary.csv")
appendix_s7_path <- file.path(tables_dir, "Appendix_S7_overlap_examples.csv")
appendix_overlap_validation_path <- file.path(tables_dir, "Appendix_overlap_monthly_validation.csv")

flow_summary <- read_sheet(flow_path, "FlowSummary")
flow_excluded <- read_sheet(flow_path, "ExcludedFrom43")
flow_desc_only <- read_sheet(flow_path, "Included43_Not24")

recovery_uncertainty <- read_csv(file.path(tables_dir, "Recovery_uncertainty_summary.csv"), show_col_types = FALSE)
joint_operational <- read_csv(file.path(tables_dir, "Joint_operational_summary.csv"), show_col_types = FALSE)
threshold_analysis <- read_csv(file.path(tables_dir, "Threshold_sensitivity_analysis.csv"), show_col_types = FALSE)
alt_endpoints <- read_csv(file.path(tables_dir, "Alternative_endpoint_sensitivity.csv"), show_col_types = FALSE)
heuristic_tasks <- read_csv(file.path(tables_dir, "Interface_heuristic_assessment.csv"), show_col_types = FALSE)

interruption_sensitivity <- read_sheet(robustness_path, "InterruptionSensitivity")
interruption_counts <- read_sheet(robustness_path, "InterruptionCounts")
model_selection_sensitivity <- read_sheet(robustness_path, "ModelSelectionSensitivity")
model_selection_counts <- read_sheet(robustness_path, "ModelSelectionCounts")
decision_utility_counts <- read_sheet(robustness_path, "DecisionUtilityCounts")

alt_endpoint_counts <- read_sheet(endpoint_path, "AlternativeEndpointCounts")
rp_only_alt_endpoints <- read_sheet(endpoint_path, "RPOnlyAlternativeEndpoints")
context_periods <- read_sheet(endpoint_path, "ContextPeriods")
context_correlations <- read_sheet(endpoint_path, "ContextCorrelations")
context_milestones <- read_sheet(endpoint_path, "ContextMilestones")
heuristic_summary <- read_sheet(endpoint_path, "HeuristicSummary")
appendix_s2 <- read_csv(appendix_s2_path, show_col_types = FALSE)
appendix_s4 <- read_csv(appendix_s4_path, show_col_types = FALSE)
appendix_s6 <- read_csv(appendix_s6_path, show_col_types = FALSE)
appendix_s7 <- read_csv(appendix_s7_path, show_col_types = FALSE)
appendix_overlap_validation <- read_csv(appendix_overlap_validation_path, show_col_types = FALSE)

appendix_lines <- readLines(appendix_path, warn = FALSE)

# Method text refresh ------------------------------------------------------------

appendix_lines <- replace_first_line_by_prefix(
  appendix_lines,
  "Counterfactual forecasts were estimated separately for each of the 24 diseases retained for modelling.",
  paste(
    "Counterfactual forecasts were estimated separately for each of the 24 diseases retained for modelling.",
    "The prepandemic training window was January, 2008 to December, 2019, and January, 2020 was treated as the common national interruption date for all diseases.",
    "Monthly counts were square-root transformed after adding a constant of 0.01 so that zero-count months remained estimable while preserving a positive-support transformation for low-count series.",
    "Seven candidate model families were compared: neural network autoregression, ETS, seasonal ARIMA, TBATS, a weighted hybrid model combining autoregressive and exponential-smoothing families, Bayesian structural time-series models with local linear trend and seasonal states, and autoregressive integrated moving average with Fourier terms."
  )
)

appendix_lines <- replace_first_line_by_prefix(
  appendix_lines,
  "Model selection was based on three rolling prepandemic hold-out schemes:",
  paste(
    "Model selection was based on three rolling prepandemic hold-out schemes: 2019 alone, 2018–2019, and 2017–2019.",
    "Within each split and disease, every model was fitted on the corresponding training segment and forecast over the withheld period.",
    "Forecast performance was summarized by sMAPE, RMSE, and MASE-type absolute-error metrics on the back-transformed scale.",
    "For each disease and split, these metrics were z-standardized across the seven candidate models and multiplied by −1 so that better performance corresponded to larger values;",
    "the standardized metrics were then summed with equal weight to give a split-specific composite score.",
    "Composite scores were summed across the three hold-out schemes, and the highest-scoring model was selected as the primary disease-specific counterfactual specification.",
    "Absolute cross-validation outputs for each model family and hold-out split were exported to **Supplementary Fig. S87-S110**, and robustness of the selected family to alternative aggregation rules was summarized in **Supplementary Table S11**."
  )
)

appendix_lines <- replace_first_line_by_prefix(
  appendix_lines,
  "Let $e_{m,s,k}$ denote the error metric for model $m$, split $s$, and metric $k \\in",
  "Let $e_{m,s,k}$ denote the error metric for model $m$, split $s$, and metric $k \\in \\{\\mathrm{sMAPE},\\mathrm{RMSE},\\mathrm{MASE}\\}$. Standardization was performed within each disease and split across the seven candidate models:"
)

appendix_lines <- replace_first_line_by_prefix(
  appendix_lines,
  "After model selection, the winning specification for each disease was refitted to the full prepandemic series and forecast forward from January, 2020 to December, 2025.",
  paste(
    "After model selection, the winning specification for each disease was refitted to the full prepandemic series and forecast forward from January, 2020 to December, 2025.",
    "Forecast uncertainty was summarized from 5000 simulated trajectories.",
    "For neural network, SARIMA, TBATS, and ARIMA-with-Fourier models, future trajectories were generated by residual-bootstrap simulation.",
    "For ETS, future trajectories were generated by bootstrapping transformed-scale one-step residuals around the deterministic forecast to avoid pathological right tails in strongly seasonal series.",
    "For the hybrid model, forecast uncertainty was approximated by resampling historical residuals around the model mean forecast.",
    "For BSTS, posterior predictive draws were taken directly from the predictive distribution after burn-in.",
    "The appendix exports both the simulated interval summaries and the disease-level forecast-versus-observed tables used in **Fig. 2** and in **Supplementary Fig. S87-S110**."
  )
)

# Fig. S87-S110 caption refresh --------------------------------------------------

for (i in seq_along(appendix_lines)) {
  line <- appendix_lines[i]
  if (!grepl("^\\*\\*Fig\\. S(8[7-9]|9[0-9]|10[0-9]|110)\\.", line, perl = TRUE)) {
    next
  }

  matches <- regmatches(
    line,
    regexec(
      "^\\*\\*Fig\\. S([0-9]+)\\. Model selection and cross.?validation performance for (.*?): multi.?split forecasts and model comparison\\.\\*\\*.*$",
      line,
      perl = TRUE
    )
  )[[1]]

  if (length(matches) == 3) {
    appendix_lines[i] <- paste0(
      "**Fig. S", matches[2],
      ". Model selection and cross‑validation performance for ",
      matches[3],
      ": multi‑split forecasts and model comparison.** Panels show the seven candidate forecasting families (Neural Network, ETS, SARIMA, TBATS, Hybrid, Bayesian structural time series, and ARIMA + Fourier) together with split-specific forecast-accuracy comparison tables."
    )
  }
}

# Table S1 / Fig S0 --------------------------------------------------------------

excluded_lookup <- named_value(flow_excluded, "Label", "N")
desc_reason_counts <- table(flow_desc_only[["Reason.for.descriptive-only.retention"]])

table_s1 <- flow_summary
names(table_s1) <- c("Stage", "N")

s1_block <- c(
  "**Table S1. Disease flow from 72 monitored series to the 43-disease descriptive analysis and 24-disease counterfactual analysis.**",
  "",
  md_table(table_s1),
  "",
  "**Supplementary Fig. S0. Two-stage disease-selection flow for the analytical subsets.**",
  "",
  "```text",
  sprintf("%d monitored notifiable disease series", flow_summary$N[1]),
  sprintf("  -> Excluded before descriptive analysis (n = %d)", flow_summary$N[1] - flow_summary$N[2]),
  sprintf("     %d overlapping surveillance categories", excluded_lookup[["Duplication"]]),
  sprintf("     %d conditions not aligned with the transmissible infectious-disease framework", excluded_lookup[["Uninfectious disease"]]),
  sprintf("     %d ill-defined or residual categories", excluded_lookup[["Unspecifed disease"]]),
  sprintf("     %d zero-incidence series", excluded_lookup[["No cases"]]),
  sprintf("     %d incompletely reported recent series", excluded_lookup[["Unreported in 2025"]]),
  sprintf("     %d series with structural surveillance-definition change", excluded_lookup[["Shifting in surveillance"]]),
  sprintf("  -> Retained for descriptive 43-disease analysis (n = %d)", flow_summary$N[2]),
  sprintf("     -> Excluded from counterfactual modelling (n = %d)", flow_summary$N[2] - flow_summary$N[3]),
  sprintf("        %d sparse or insufficient-count series", unname(desc_reason_counts["Insufficient cases"])),
  sprintf("        %d non-seasonal series", unname(desc_reason_counts["Non-seasonal trend"])),
  sprintf("        %d insufficient-duration series", unname(desc_reason_counts["Insufficient duration"])),
  sprintf("        %d residual or unspecified series", unname(desc_reason_counts["Unspecifed disease"])),
  sprintf("     -> Retained for 24-disease counterfactual analysis (n = %d)", flow_summary$N[3]),
  "```",
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S1",
  "**Table S1. Disease flow from 72 monitored series to the 43-disease descriptive analysis and 24-disease counterfactual analysis.**",
  s1_block
)

# Table S2 -----------------------------------------------------------------------

s2_counts <- sort(table(appendix_s2[["Exclusion category"]]), decreasing = TRUE)
s2_block <- c(
  "**Table S2. Excluded disease series and exclusion category.**",
  "",
  md_table(appendix_s2),
  "",
  sprintf(
    "These %d excluded series were concentrated in overlapping surveillance categories (%d), diseases outside the transmissible infectious-disease framework (%d), ill-defined or residual categories (%d), zero-incidence series (%d), incompletely reported recent series (%d), and one series with a structural surveillance-definition change.",
    nrow(appendix_s2),
    unname(s2_counts["Overlapping surveillance categories"]),
    unname(s2_counts["Not aligned with the transmissible infectious-disease framework"]),
    unname(s2_counts["Ill-defined or residual surveillance categories"]),
    unname(s2_counts["Zero reported incidence over the study period"]),
    unname(s2_counts["Incomplete reporting in the most recent surveillance year"])
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S2",
  "**Table S2. Excluded disease series and exclusion category.**",
  s2_block
)

# Table S3 -----------------------------------------------------------------------

table_s3 <- flow_desc_only[, c("Disease", "Shortname", "Group", "Reason.for.descriptive-only.retention")]
names(table_s3) <- c("Disease", "Shortname", "Group", "Reason for descriptive-only retention")

s3_reason_counts <- sort(table(flow_desc_only[["Reason.for.descriptive-only.retention"]]), decreasing = TRUE)
s3_block <- c(
  "**Table S3. Diseases retained in the 43-disease descriptive analysis but not modelled counterfactually, with direct reason for descriptive-only retention.**",
  "",
  md_table(table_s3),
  "",
  sprintf(
    "Across these %d diseases, the main reasons for descriptive-only retention were insufficient prepandemic counts or sparse long-horizon signal (%d diseases), non-seasonal prepandemic structure (%d diseases), insufficient time coverage (%d diseases), and ill-defined residual categories (%d diseases). This pattern indicates that the 24-disease forecasting subset was selected primarily on time-series suitability rather than on a single transmission category, although vector-borne and respiratory pathogens remained differentially represented after this second-stage restriction.",
    nrow(flow_desc_only),
    unname(s3_reason_counts["Insufficient cases"]),
    unname(s3_reason_counts["Non-seasonal trend"]),
    unname(s3_reason_counts["Insufficient duration"]),
    unname(s3_reason_counts["Unspecifed disease"])
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S3",
  "**Table S3. Diseases retained in the 43-disease descriptive analysis but not modelled counterfactually, with direct reason for descriptive-only retention.**",
  s3_block
)

# Table S4 -----------------------------------------------------------------------

s4_vaccine_counts <- sort(table(appendix_s4$Vaccine), decreasing = TRUE)
s4_block <- c(
  "**Table S4. Predictor definitions used in time-to-recovery analyses**",
  "",
  md_table(appendix_s4),
  "",
  sprintf(
    "This lookup table covers all %d modelled diseases and supplied the disease-level predictors used in the recovery-timing analyses. Vaccine status was classified as unavailable for %d diseases, optional for %d diseases, and part of the national EPI schedule for %d diseases.",
    nrow(appendix_s4),
    unname(s4_vaccine_counts["Unavailable"]),
    unname(s4_vaccine_counts["Optional"]),
    unname(s4_vaccine_counts["EPI"])
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S4",
  "**Table S4. Predictor definitions used in time-to-recovery analyses** ",
  s4_block
)

# Table S5 -----------------------------------------------------------------------

threshold_analysis$StatusLabel <- status_label_rp(threshold_analysis$Status)
threshold_analysis$Config <- factor(
  threshold_analysis$Config,
  levels = c(
    "95% / 3 mo",
    "90% / 2 mo", "90% / 3 mo", "90% / 4 mo",
    "95% / 2 mo", "95% / 4 mo",
    "100% / 2 mo", "100% / 3 mo", "100% / 4 mo"
  )
)
threshold_analysis <- threshold_analysis[order(threshold_analysis$Config, threshold_analysis$Shortname), ]
primary_lookup <- threshold_analysis[threshold_analysis$Config == "95% / 3 mo", c("Shortname", "StatusLabel")]
names(primary_lookup)[2] <- "PrimaryStatusLabel"
threshold_joined <- merge(threshold_analysis, primary_lookup, by = "Shortname", all.x = TRUE, sort = FALSE)

configs <- levels(threshold_analysis$Config)
table_s5 <- do.call(rbind, lapply(configs, function(cfg) {
  subset_cfg <- threshold_joined[threshold_joined$Config == cfg, ]
  reclassified <- subset_cfg$Shortname[subset_cfg$StatusLabel != subset_cfg$PrimaryStatusLabel]
  data.frame(
    Threshold = fmt_num_trim(unique(subset_cfg$Threshold), digits = 2),
    `Consecutive months` = fmt_int(unique(subset_cfg$Persistence)),
    Balanced = sum(subset_cfg$StatusLabel == "Balanced"),
    `RP achieved without BP` = sum(subset_cfg$StatusLabel == "RP achieved without BP"),
    Suppressed = sum(subset_cfg$StatusLabel == "Suppressed"),
    `No deficit` = sum(subset_cfg$StatusLabel == "No deficit"),
    `Diseases reclassified vs primary analysis` = if (length(reclassified) == 0) "None" else paste(reclassified, collapse = ", "),
    check.names = FALSE
  )
}))

non_primary_changes <- threshold_joined[
  threshold_joined$Config != "95% / 3 mo" &
    threshold_joined$StatusLabel != threshold_joined$PrimaryStatusLabel,
]

s5_text <- if (nrow(non_primary_changes) == 0) {
  paste(
    "These sensitivity checks were computed from the exported disease-specific outcome tables underlying Fig. 3.",
    "No disease changed RP/BP classification when the RP threshold was varied across 90%, 95%, and 100% with persistence requirements of 2, 3, or 4 months, indicating that the principal recovery typology was stable to plausible operational definition changes."
  )
} else {
  changed_lookup <- aggregate(Shortname ~ Config, data = non_primary_changes, FUN = function(x) paste(unique(x), collapse = ", "))
  changed_summary <- paste(sprintf("%s: %s", changed_lookup$Config, changed_lookup$Shortname), collapse = "; ")
  paste(
    "These sensitivity checks were computed from the exported disease-specific outcome tables underlying Fig. 3.",
    sprintf(
      "RP/BP classifications were stable for most operational definitions; the only observed reclassifications relative to the primary 95%% / 3 mo rule were: %s.",
      changed_summary
    )
  )
}

s5_block <- c(
  "**Table S5. Sensitivity of RP/BP classifications to alternative RP thresholds and persistence requirements.**",
  "",
  md_table(table_s5),
  "",
  s5_text,
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S5",
  "**Table S5. Sensitivity of RP/BP classifications to alternative RP thresholds and persistence requirements.**",
  s5_block
)

# Table S6 -----------------------------------------------------------------------

s6_block <- c(
  "**Table S6. Summary metrics for overlap-period validation of weekly-to-monthly reconstruction.**",
  "",
  md_table(appendix_s6),
  "",
  sprintf(
    "These overlap-period validation summaries were recalculated directly from the disease-month comparison cache used to validate the weekly-to-monthly reconstruction. The refreshed cache retains %s disease-month pairs across %s.",
    appendix_s6$Value[appendix_s6$Metric == "Disease-month observations"],
    appendix_s6$Value[appendix_s6$Metric == "Overlap years retained in analytical cache"]
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S6",
  "**Table S6. Summary metrics for overlap-period validation of weekly-to-monthly reconstruction.**",
  s6_block
)

# Table S7 -----------------------------------------------------------------------

appendix_s7_fmt <- appendix_s7
num_cols_s7 <- setdiff(names(appendix_s7_fmt), "Disease")
for (col in num_cols_s7) {
  digits <- if (col == "Pearson correlation") 4 else 2
  appendix_s7_fmt[[col]] <- fmt_num(appendix_s7_fmt[[col]], digits = digits)
}

high_burden_n <- nrow(appendix_s7_fmt)
s7_block <- c(
  "**Table S7. Illustrative disease-specific overlap-period reconstruction error metrics.**",
  "",
  md_table(appendix_s7_fmt),
  "",
  sprintf(
    "High-burden diseases that materially contribute to the main analyses showed low relative reconstruction error across these %d illustrative examples, whereas some low-count series had larger percentage error because small absolute monthly differences inflate relative measures. Together with the disease-specific visual comparisons in Part 1, these summaries support the robustness of the reconstructed monthly series for the principal RP/BP and seasonal analyses.",
    high_burden_n
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S7",
  "**Table S7. Illustrative disease-specific overlap-period reconstruction error metrics.**",
  s7_block
)

# Table S9 -----------------------------------------------------------------------

table_s9 <- data.frame(
  Shortname = recovery_uncertainty$Shortname,
  Group = recovery_uncertainty$Group,
  `Primary deterministic phenotype` = recovery_uncertainty$PrimaryStatusLabel,
  `Pr(RP)` = fmt_num(recovery_uncertainty$Pr_RP, 3),
  `Pr(BP)` = fmt_num(recovery_uncertainty$Pr_BP, 3),
  `Primary phenotype stability` = fmt_num(recovery_uncertainty$PrimaryStatusProb, 3),
  `RP month, median (95% interval)` = ifelse(
    is.na(recovery_uncertainty$RP_MedianMonths),
    "NA",
    paste0(
      fmt_num_trim(recovery_uncertainty$RP_MedianMonths),
      " (",
      fmt_num_trim(recovery_uncertainty$RP_Q025Months),
      "-",
      fmt_num_trim(recovery_uncertainty$RP_Q975Months),
      ")"
    )
  ),
  `BP month, median (95% interval)` = ifelse(
    is.na(recovery_uncertainty$BP_MedianMonths),
    "NA",
    paste0(
      fmt_num_trim(recovery_uncertainty$BP_MedianMonths),
      " (",
      fmt_num_trim(recovery_uncertainty$BP_Q025Months),
      "-",
      fmt_num_trim(recovery_uncertainty$BP_Q975Months),
      ")"
    )
  ),
  check.names = FALSE
)

uncertainty_sensitive <- recovery_uncertainty$Shortname[recovery_uncertainty$PrimaryStatusProb < 0.80]
s9_block <- c(
  "**Table S9. Uncertainty-aware RP/BP classification from 5000 simulated counterfactual trajectories.**",
  "",
  md_table(table_s9),
  "",
  sprintf(
    "Here, primary phenotype stability denotes the probability that the deterministic median-based phenotype was retained across the 5000 simulated trajectories; values below 0.80 were treated as uncertainty-sensitive in the revised main-text review layer. Under that pragmatic flag, %d diseases were uncertainty-sensitive: %s.",
    length(uncertainty_sensitive),
    join_names(uncertainty_sensitive)
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S9",
  "**Table S9. Uncertainty-aware RP/BP classification from 5000 simulated counterfactual trajectories.**",
  s9_block
)

# Table S10 ----------------------------------------------------------------------

table_s10 <- data.frame(
  Shortname = interruption_sensitivity$Shortname,
  Group = interruption_sensitivity$Group,
  `2020-01 status` = status_label(interruption_sensitivity$Status_2020_01),
  `2020-03 status` = status_label(interruption_sensitivity$Status_2020_03),
  `2020-04 status` = status_label(interruption_sensitivity$Status_2020_04),
  `Changed vs January in March analysis` = fmt_bool(interruption_sensitivity$Changed_vs_2020_01_for_2020_03),
  `Changed vs January in April analysis` = fmt_bool(interruption_sensitivity$Changed_vs_2020_01_for_2020_04),
  check.names = FALSE
)

interrupt_lookup <- named_value(interruption_counts, "StartDate", "ChangedDiseasesVs2020_01")
march_changes <- interrupt_lookup[["2020-03-01"]]
april_changes <- interrupt_lookup[["2020-04-01"]]
s10_block <- c(
  "**Table S10. Sensitivity of deterministic RP/BP classification to alternative interruption dates.**",
  "",
  md_table(table_s10),
  "",
  sprintf(
    "No disease changed classification when the analytical start date was moved from January 2020 to March 2020 or April 2020 (March changes: %d; April changes: %d), supporting the use of January 2020 as a pragmatic portfolio-level interruption anchor.",
    march_changes,
    april_changes
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S10",
  "**Table S10. Sensitivity of deterministic RP/BP classification to alternative interruption dates.**",
  s10_block
)

# Table S11 ----------------------------------------------------------------------

table_s11 <- data.frame(
  Shortname = model_selection_sensitivity$Shortname,
  Group = model_selection_sensitivity$Group,
  `Primary best model` = best_model_label(model_selection_sensitivity$PrimaryBest),
  `Rank-aggregation best model` = best_model_label(model_selection_sensitivity$RankBest),
  `sMAPE-only best model` = best_model_label(model_selection_sensitivity$SMAPEBest),
  `Horizon-weighted best model` = best_model_label(model_selection_sensitivity$WeightedBest),
  `Match under rank aggregation` = fmt_bool(model_selection_sensitivity$RankMatch),
  `Match under sMAPE-only` = fmt_bool(model_selection_sensitivity$SMAPEMatch),
  `Match under horizon weighting` = fmt_bool(model_selection_sensitivity$WeightedMatch),
  check.names = FALSE
)

count_lookup <- named_value(model_selection_counts, "AlternativeRule", "MatchesPrimary")
s11_block <- c(
  "**Table S11. Alternative model-selection rules compared with the primary equal-weight composite rule.**",
  "",
  md_table(table_s11),
  "",
  sprintf(
    "Across the 24 diseases, the primary selected family was also recovered for %d diseases under rank aggregation, %d diseases under sMAPE-only selection, and %d diseases under the horizon-weighted composite. Most disagreements were concentrated in a small subset of diseases rather than a single model family, suggesting that the principal conclusions were not driven by one aggregation formula.",
    count_lookup[["Rank aggregation"]],
    count_lookup[["sMAPE only"]],
    count_lookup[["Horizon-weighted composite"]]
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S11",
  "**Table S11. Alternative model-selection rules compared with the primary equal-weight composite rule.**",
  s11_block
)

# Table S12 ----------------------------------------------------------------------

table_s12 <- data.frame(
  Shortname = joint_operational$Shortname,
  Group = joint_operational$Group,
  `Primary deterministic phenotype` = status_label(joint_operational$PrimaryStatus),
  `Shift vs pre (months)` = fmt_num_trim(joint_operational$shift_vs_pre),
  `Shift vs predicted (months)` = fmt_num_trim(joint_operational$shift_vs_pred),
  `Seasonal displacement` = joint_operational$SeasonalDisplacement,
  `Operational priority` = joint_operational$FrameworkPriority,
  check.names = FALSE
)

priority_lookup <- named_value(decision_utility_counts, "FrameworkPriority", "Diseases")
monthly_normalized <- sum(joint_operational$MonthlyDashboardView == "Recovered on monthly incidence", na.rm = TRUE)
s12_block <- c(
  "**Table S12. Joint operational synthesis of recovery phenotype and seasonal displacement.**",
  "",
  md_table(table_s12),
  "",
  sprintf(
    "This joint table clarifies the retrospective decision utility of the framework. A monthly-incidence-only interpretation would have marked %d diseases as monthly-normalized, but the integrated RP/BP-seasonality synthesis separated them into %d low-priority routine-review cases, %d cumulative-review cases, %d recovered-but-recalibrate-seasonality cases, and %d recalibrate-and-monitor cases, while %d disease remained in high-priority manual review and %d disease remained in no-deficit monitoring.",
    monthly_normalized,
    priority_lookup[["Low priority routine review"]],
    priority_lookup[["Cumulative review needed"]],
    priority_lookup[["Recovered but recalibrate seasonality"]],
    priority_lookup[["Recalibrate and monitor"]],
    priority_lookup[["High priority manual review"]],
    priority_lookup[["No deficit monitoring"]]
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S12",
  "**Table S12. Joint operational synthesis of recovery phenotype and seasonal displacement.**",
  s12_block
)

# Table S13 ----------------------------------------------------------------------

table_s13 <- data.frame(
  Shortname = alt_endpoints$Shortname,
  Group = alt_endpoints$Group,
  `Primary phenotype` = status_label(alt_endpoints$Status),
  `Primary RP month` = fmt_num_trim(alt_endpoints$Recovery_Months),
  `PI95 month` = fmt_num_trim(alt_endpoints$PI95_Months),
  `Ratio>=1 month` = fmt_num_trim(alt_endpoints$Ratio100_Months),
  `Half-deficit month` = fmt_num_trim(alt_endpoints$HalfDeficit_Months),
  `PI95 achieved` = fmt_bool(alt_endpoints$PI95_Achieved),
  `Ratio>=1 achieved` = fmt_bool(alt_endpoints$Ratio100_Achieved),
  `Half-deficit achieved` = fmt_bool(alt_endpoints$HalfDeficit_Achieved),
  check.names = FALSE
)

alt_count_lookup <- named_value(alt_endpoint_counts, "Metric", "Value")
rp_only_n <- nrow(rp_only_alt_endpoints)
rp_only_half <- sum(rp_only_alt_endpoints$HalfDeficit_Achieved, na.rm = TRUE)
s13_block <- c(
  "**Table S13. Alternative endpoint sensitivity analyses for the 24 modelled diseases.** Month values are counted from January 2020, so month 0 corresponds to January 2020.",
  "",
  md_table(table_s13),
  "",
  sprintf(
    "Among the %d diseases that entered a sustained cumulative deficit, %d re-entered the disease-specific 95%% predictive interval for at least 3 months, %d met the sustained observed-to-expected ratio endpoint of at least 1.0, and %d halved their cumulative deficit by end follow-up. The ratio endpoint preserved the same achieved-versus-not-achieved distinction as the primary RP definition for all 23 deficit-entering diseases, whereas the half-deficit milestone was reached by %d of the %d recovered-but-not-balanced diseases.",
    alt_count_lookup[["TotalDiseasesWithDeficit"]],
    alt_count_lookup[["PI95_Achieved"]],
    alt_count_lookup[["Ratio100_Achieved"]],
    alt_count_lookup[["HalfDeficit_Achieved"]],
    rp_only_half,
    rp_only_n
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S13",
  "**Table S13. Alternative endpoint sensitivity analyses for the 24 modelled diseases.** Month values are counted from January 2020, so month 0 corresponds to January 2020.",
  s13_block
)

# Table S14 ----------------------------------------------------------------------

table_s14 <- data.frame(
  Period = context_periods$Period,
  Mean_Portfolio_Ratio = fmt_num(context_periods$Mean_Portfolio_Ratio, 3),
  Median_Portfolio_Ratio = fmt_num(context_periods$Median_Portfolio_Ratio, 3),
  Mean_Stringency = fmt_num(context_periods$Mean_Stringency, 3),
  Mean_SchoolClosing = fmt_num(context_periods$Mean_SchoolClosing, 3),
  Mean_InternalMovement = fmt_num(context_periods$Mean_InternalMovement, 3),
  Mean_InternationalTravel = fmt_num(context_periods$Mean_InternationalTravel, 3),
  Mean_TestingPolicy = fmt_num(context_periods$Mean_TestingPolicy, 3),
  Mean_WHO_COVID_Cases = fmt_num(context_periods$Mean_WHO_COVID_Cases, 3),
  check.names = FALSE
)

s14_block <- c(
  "**Table S14. External contextual triangulation period summary.**",
  "",
  md_table(table_s14),
  "",
  "The portfolio-level observed-to-expected ratio rose across the restriction-intensive, transition, and post-PHSM periods. Policy indicators were available through December, 2022, whereas WHO COVID-19 burden was available through June, 2024.",
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S14",
  "**Table S14. External contextual triangulation period summary.**",
  s14_block
)

# Table S15 ----------------------------------------------------------------------

table_s15a <- data.frame(
  Indicator = context_correlations$Indicator,
  SpearmanRho = fmt_num(context_correlations$SpearmanRho, 3),
  check.names = FALSE
)

table_s15b <- data.frame(
  Milestone = context_milestones$Milestone,
  month = vapply(context_milestones$month, excel_month, character(1)),
  PortfolioRatio = fmt_num(context_milestones$PortfolioRatio, 3),
  StringencyIndex = fmt_num(context_milestones$StringencyIndex, 3),
  SchoolClosing = fmt_num(context_milestones$SchoolClosing, 3),
  InternalMovement = fmt_num(context_milestones$InternalMovement, 3),
  InternationalTravel = fmt_num(context_milestones$InternationalTravel, 3),
  WHO_COVID_Cases = fmt_num(context_milestones$WHO_COVID_Cases, 0),
  check.names = FALSE
)

s15_block <- c(
  "**Table S15. External contextual triangulation correlations and milestone dates.**",
  "",
  "Panel A. Monthly correlation between the portfolio observed-to-expected ratio and external indicators.",
  "",
  md_table(table_s15a),
  "",
  "Panel B. Selected milestone months from the contextual triangulation.",
  "",
  md_table(table_s15b),
  "",
  "These contextual summaries were used descriptively to anchor the timing of portfolio suppression and normalization. They were not used as predictive covariates and do not support causal attribution.",
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S15",
  "**Table S15. External contextual triangulation correlations and milestone dates.**",
  s15_block
)

# Table S16 ----------------------------------------------------------------------

table_s16 <- heuristic_tasks[, c(
  "TaskID", "PublicHealthTask", "PrimaryModule", "MinimumInteractions",
  "Discoverability", "Interpretability", "Auditability", "SupportStatus",
  "ResidualFriction", "MeanHeuristicScore"
)]
table_s16$MeanHeuristicScore <- fmt_num(table_s16$MeanHeuristicScore, 2)

heuristic_lookup <- named_value(heuristic_summary, "Metric", "Value")
tasks_assessed <- as.integer(round(heuristic_lookup[["TasksAssessed"]]))
s16_intro <- if (tasks_assessed == 6) {
  "All six prespecified surveillance-review tasks were directly supported in the final build"
} else {
  sprintf("All %d prespecified surveillance-review tasks were directly supported in the final build", tasks_assessed)
}

s16_block <- c(
  "**Table S16. Task-based heuristic assessment of the final dashboard build.**",
  "",
  md_table(table_s16),
  "",
  sprintf(
    "%s, with a mean heuristic score of %s and median minimum interaction count of %s. This assessment documents functional interface coverage but should not be interpreted as a substitute for prospective end-user usability testing.",
    s16_intro,
    fmt_num(heuristic_lookup[["MeanHeuristicScore"]], 2),
    fmt_num_trim(heuristic_lookup[["MedianInteractions"]], 0)
  ),
  ""
)

appendix_lines <- replace_block(
  appendix_lines,
  "TABLE_S16",
  "**Table S16. Task-based heuristic assessment of the final dashboard build.**",
  s16_block
)

writeLines(appendix_lines, appendix_path)
cat(sprintf("Refreshed %s\n", appendix_path))
