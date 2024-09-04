
source("./1_a_overview.R")
print("Figure 1 generated")
rm(list = ls())

source("./2_a_ranking.R")
print("Figure 2 generated")
rm(list = ls())

# reticulate::source_python('./script/3_c_merge_appendix_1_1.py')
# print("Appendix 1_2 combinded")
# rm(list = ls())

source("./3_a_age.R")
print("Figure 3 generated")
rm(list = ls())

source("./4_a_region.R")
source('./4_b_appendix.R')
print("Figure 4 generated")
rm(list = ls())

source("./5_a_diseases.R")
print("Figure 6 generated")
rm(list = ls())

source("./6_a_select_model.R")
source('./6_b_best_model.R')
print("Figure 7 generated")
rm(list = ls())

source("./7_a_forecast.R")
source('./7_b_visualization.R')
print("Figure 8-12 data generated")
rm(list = ls())

reticulate::source_python('./8_a_create_appendix.py')
reticulate::source_python('./8_b_create_appendix.py')
reticulate::source_python('./8_c_create_appendix.py')
reticulate::source_python('./8_d_merge_appendix.py')
