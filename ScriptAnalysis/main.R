
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
print("Figure 4 generated")
rm(list = ls())

source("./5_a_diseases.R")
print("Figure 5 generated")
rm(list = ls())

source("./6_a_select_model.R")
source('./6_b_best_model.R')
print("Figure 6 generated")
rm(list = ls())

source("./7_a_forecast.R")
print("Figure 7 data generated")
rm(list = ls())
# 
# source("./script/8_a_province.R")
# print("Appendix 1_1 figures generated")
# rm(list = ls())
# 
# reticulate::source_python('./script/8_b_merge_appendix_1_2.py')
# print("Appendix 1_1 combinded")
# rm(list = ls())
# 
# source("./script/9_a_rubella.R")
# print("Appendix 1_3 figures generated")
# rm(list = ls())
# 
# reticulate::source_python('./script/9_b_merge_appendix_1_3.py')
# print("Appendix 1 combinded")
# rm(list = ls())


