library(tidyverse)
library(grf)
library(causalTree)
library(rpart.plot)
rm(list = ls())

source("./src_new/05_functions.R")

## Data
df_analysis_sample <- readRDS("data/final/ct_analysis_set.rds")

## Causal Tree analysis
options(scipen = 99)

## Ex_sel_GM
ct_exc_sel_gm <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_exc_sel_gm_18plus.rds")

plot(as.data.frame(ct_exc_sel_gm$cptable)$xerror[1:50])

op_ct_exc_sel_gm <- prune_ct(ct_exc_sel_gm, 12)
rpart.plot(op_ct_exc_sel_gm)

ct_exc_sel_gm_results <- generate_output_ct(op_ct_object = op_ct_exc_sel_gm,
                                            df_sample = df_analysis_sample %>%
                                              filter(!(gem_2019 %in% c("344", "34", "599", "518", "363"))))

gen_ate_y_plot(ct_exc_sel_gm_results, hline = 0.0079)

## Ex_sel_GM
ct_exc_sel_gm2 <- readRDS("H:/wmo_heterogeneity/models/ct_1000000_ms2500_exc_sel_gm_18plus.rds")

plot(as.data.frame(ct_exc_sel_gm2$cptable)$xerror[1:50])

op_ct_exc_sel_gm2 <- prune_ct(ct_exc_sel_gm2, 14)
rpart.plot(op_ct_exc_sel_gm2)

ct_exc_sel_gm_results2 <- generate_output_ct(op_ct_object = op_ct_exc_sel_gm2,
                                             df_sample = df_analysis_sample %>%
                                               filter(!(gem_2019 %in% c("344", "34", "599", "518", "363"))))

gen_ate_y_plot(ct_exc_sel_gm_results2, hline = 0.009)

# Including per GM: 363

ct_sel_gm_363 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_363_18plus.rds")
plot(as.data.frame(ct_sel_gm_363$cptable)$xerror[1:50])
op_ct_sel_gm_363 <- prune_ct(ct_sel_gm_363, 12)
rpart.plot(op_ct_sel_gm_363)
ct_gm_363_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_363,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "363"))

gen_ate_y_plot(ct_gm_363_results, hline = 0.01)

# Including per GM: 518

ct_sel_gm_518 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_518_18plus.rds")
plot(as.data.frame(ct_sel_gm_518$cptable)$xerror[1:50])
op_ct_sel_gm_518 <- prune_ct(ct_sel_gm_518, 8)
rpart.plot(op_ct_sel_gm_518)
ct_gm_518_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_518,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "518"))
gen_ate_y_plot(ct_gm_518_results, hline = -0.0013)

# Including per GM: 344

ct_sel_gm_344 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_344_18plus.rds")
plot(as.data.frame(ct_sel_gm_344$cptable)$xerror[1:50])
op_ct_sel_gm_344 <- prune_ct(ct_sel_gm_344, 9)
rpart.plot(op_ct_sel_gm_344)
ct_gm_344_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_344,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "344"))
gen_ate_y_plot(ct_gm_344_results, hline=0.00026)


# Including per GM: 34

ct_sel_gm_34 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_34_18plus.rds")
plot(as.data.frame(ct_sel_gm_34$cptable)$xerror[1:50])
op_ct_sel_gm_34 <- prune_ct(ct_sel_gm_34, 9)
rpart.plot(op_ct_sel_gm_34)
ct_gm_34_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_34,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "34"))
gen_ate_y_plot(ct_gm_34_results, hline=0.0037)

# Including per GM: 599

ct_sel_gm_599 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_599_18plus.rds")
plot(as.data.frame(ct_sel_gm_599$cptable)$xerror[1:50])
op_ct_sel_gm_599 <- prune_ct(ct_sel_gm_599, 7)
rpart.plot(op_ct_sel_gm_599)
ct_gm_599_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_599,
                                       df_sample = df_analysis_sample %>% filter(gem_2019 == "599"))
gen_ate_y_plot(ct_gm_599_results, hline=0.0056)


## Write files

# Result tables for each CT
openxlsx::write.xlsx(
  list('ex_sel_gm_500K_2500ms_18plus' = ct_exc_sel_gm_results,
       'ex_sel_gm_1M_2500ms_18plus' = ct_exc_sel_gm_results2,
       'sel_gm_363_500K_2500ms_18plus' = ct_gm_363_results,
       'sel_gm_34_500K_2500ms_18plus' = ct_gm_34_results,
       'sel_gm_344_500K_2500ms_18plus' = ct_gm_344_results,
       'sel_gm_518_500K_2500ms_18plus' = ct_gm_518_results,
       'sel_gm_599_500K_2500ms_18plus' = ct_gm_599_results),
  file = "./output/220711_Output_WMO3/ct_tables.xlsx"
)

# Figures for each CT
rpart.plot(op_ct_exc_sel_gm)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_ex_sel_gm_500K_18plus.pdf")
rpart.plot(op_ct_exc_sel_gm2)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_ex_sel_gm_1M_18plus.pdf")
rpart.plot(op_ct_sel_gm_518)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_sel_gm_518_500K_18plus.pdf")
rpart.plot(op_ct_sel_gm_34)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_sel_gm_34_500K_18plus.pdf")
rpart.plot(op_ct_sel_gm_344)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_sel_gm_344_500K_18plus.pdf")
rpart.plot(op_ct_sel_gm_599)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_sel_gm_599_500K_18plus.pdf")
rpart.plot(op_ct_sel_gm_363)
ggsave(last_plot(), filename = "output/220711_Output_WMO3/ct_sel_gm_363_500K_18plus.pdf")

# Elbow scores
df_elbows <- data.frame(exc_sel_gm_500K = as.data.frame(ct_exc_sel_gm$cptable)$xerror[1:25],
                        exc_sel_gm_1M = as.data.frame(ct_exc_sel_gm2$cptable)$xerror[1:25],
                        sel_gm_363_500K = as.data.frame(ct_sel_gm_363$cptable)$xerror[1:25],
                        sel_gm_34_500K = as.data.frame(ct_sel_gm_34$cptable)$xerror[1:25],
                        sel_gm_344_500K = as.data.frame(ct_sel_gm_344$cptable)$xerror[1:25],
                        sel_gm_599_500K = as.data.frame(ct_sel_gm_599$cptable)$xerror[1:25],
                        sel_gm_518_500K = as.data.frame(ct_sel_gm_518$cptable)$xerror[1:25])
writexl::write_xlsx(df_elbows, "./output/220711_Output_WMO3/elbow_tables.xlsx")
