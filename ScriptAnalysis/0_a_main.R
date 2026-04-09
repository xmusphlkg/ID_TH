library(tidyverse)
library(lubridate)
library(openxlsx)
library(patchwork)
library(parallel)
library(nih.joinpoint)
library(patchwork)
library(future)
library(ggbump)
library(ungroup)
library(ggnewscale)
library(cowplot)
library(sf)
library(biscale)
library(ggthemes)

library(stats)
library(tseries)
library(astsa)
library(forecast)
library(forecastHybrid)
library(caret)
library(bsts)
library(Cairo)
library(ggpubr)
library(doParallel)

library(ggh4x)
library(ggrepel)
library(broom)
library(factoextra)

run_appendix_builder <- function() {
  builder_script <- normalizePath("./build_supplementary_appendix.py", winslash = "/", mustWork = TRUE)
  env_python <- Sys.getenv("PYTHON_BIN", unset = "")
  candidates <- Filter(
    function(x) nzchar(x$cmd),
    list(
      list(cmd = env_python, args = character()),
      list(cmd = Sys.which("python3"), args = character()),
      list(cmd = Sys.which("py"), args = c("-3")),
      list(cmd = Sys.which("python"), args = character())
    )
  )

  if (length(candidates) == 0) {
    stop("Could not find a Python interpreter to run build_supplementary_appendix.py.")
  }

  for (candidate in candidates) {
    status <- tryCatch(
      system2(candidate$cmd, c(candidate$args, shQuote(builder_script, type = "cmd"))),
      error = function(e) e
    )

    if (identical(status, 0L)) {
      return(invisible(NULL))
    }
  }

  stop("Supplementary appendix builder failed. Check Python availability and the appendix builder output.")
}

cat("Start data transformation...\n")
source('./0_b_population.R')
source('./1_a_data_trans.R')

cat("Start part 1: overview...\n")
source('./1_b_overview.R')

cat("Start part 2: age distribution...\n")
source('./2_a_age.R')

cat("Start part 3: spatial distribution...\n")
source('./3_a_province.R')

cat("Start part 4: finding best model...\n")
source('./4_a_select_model.R')

cat("Start part 5: forecasting best model...\n")
source('./5_a_forecast.R')

cat("Start part 6: figure 3 visualization...\n")
# source('./6_a_IRR_index.R')

cat("Start part 7: figure 4 visualization (suppression)...\n")
source('./7_a_suppression.R')

cat("Start part 8: figure 5 visualization (recovery)...\n")
source('./8_a_recovery.R')

cat("Start part 9: robustness and operational sensitivity...\n")
source('./8_b_robustness_operational.R')

cat("Start part 10: endpoint, context, and usability analysis...\n")
source('./8_c_endpoints_context_usability.R')

cat("Start part 11: generate supplementary flow figure...\n")
source('./9_c_generate_flow_figure.R')

cat("Start part 12: external pertussis decision support case study...\n")
source('./8_d_external_pertussis_decision_support.R')

cat("Start part 13: temporal utility validation...\n")
source('./8_e_temporal_utility_validation.R')

cat("Start part 14: placebo interruption and interval calibration...\n")
source('./8_f_falsification_calibration.R')

cat("Start part 15: transform and denominator sensitivity...\n")
source('./8_g_transform_rate_sensitivity.R')

cat("Start part 16: seasonal uncertainty and reconstruction sensitivity...\n")
source('./8_h_seasonal_uncertainty.R')

cat("Start part 17: segmented BP comparator...\n")
source('./8_i_bp_segmented_comparator.R')

cat("Start part 17b: threshold-tolerance stress-test figure...\n")
source('./S129_threshold_tolerance_figure.R')

cat("Start part 18: regenerate npjDM figure set (fig2-fig5); Figure 1 is now specified in ../manuscript/figure1_ai_brief.md for external generation...\n")
source('./fig2.R')
source('./fig3.R')
source('./fig4.R')
source('./fig5.R')

cat("Start part 19: refresh supplementary appendix from Python...\n")
run_appendix_builder()

cat("All one-stop analyses completed.\n")
