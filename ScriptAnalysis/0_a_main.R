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

cat("Start part 11: refresh supplementary appendix text and tables...\n")
# 9_b internally runs 9_a_generate_appendix_source_tables.R
source('./9_b_refresh_supplementary_appendix.R')

cat("Start part 12: generate supplementary flow figure...\n")
source('./9_c_generate_flow_figure.R')

cat("Start part 13: external pertussis decision support case study...\n")
source('./8_d_external_pertussis_decision_support.R')

cat("Start part 14: regenerate npjDM figure set (fig1-fig5)...\n")
source('./fig1.R')
source('./fig2.R')
source('./fig3.R')
source('./fig4.R')
source('./fig5.R')

cat("All one-stop analyses completed.\n")
