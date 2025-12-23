
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

cat("Start part 2: ranking...\n")
source('./2_b_appendix.R')

cat("Start part 3: age distribution...\n")
source('./2_a_age.R')

cat("Start part 4: spatial distribution...\n")
source('./3_a_province.R')

cat("Start part 5: finding best model...\n")
source('./4_a_select_model.R')

cat("Start part 6: forecasting best model...\n")
source('./5_a_forecast.R')

cat("Start part 7: figure 5 visualization...\n")
source('./6_a_IRR_index.R')

cat("Start part 8: figure 6 visualization...\n")
source('./7_a_impact_factor.R')

cat("Start creating appendix...\n")

# reticulate::source_python('./9_a_create_appendix.py')
# reticulate::source_python('./9_b_create_appendix.py')
# reticulate::source_python('./9_c_create_appendix.py')
# reticulate::source_python('./9_d_merge_appendix.py')