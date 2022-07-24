library(grf)
library(tidyverse)


## Load grf objects
# grf_033_inc_gm <- readRDS("H:/wmo_heterogeneity/models/gm_tau_forest_033_1250ms_500t_18plus.rds")
gm_ate <- readRDS("H:/wmo_heterogeneity/data/final/ate_gm_tau_forest_033_1250ms_500t_18plus.rds")
gm_vi <- readRDS("H:/wmo_heterogeneity/data/final/vi_gm_tau_forest_033_1250ms_500t_18plus.rds")

writexl::write_xlsx(gm_vi[gm_vi$var_imp > 0, ], "H:/wmo_heterogeneity/output/220711_Output_WMO3/vi_gm_tau_forest_033_1250ms_500t_18plus.xlsx")

grf_033_ex_gm <- readRDS("H:/wmo_heterogeneity/models/non_gm_tau_forest_033_1250ms_500t_18plus.rds")
non_gm_ate <- grf::average_treatment_effect(grf_033_ex_gm)
saveRDS(non_gm_ate, "H:/wmo_heterogeneity/data/final/ate_non_gm_tau_forest_033_1250ms_500t_18plus.rds")
non_gm_vi <- grf::variable_importance(grf_033_inc_gm)
