library(tidyverse)
library(rpart)
library(causalTree)
library(rpart.plot)

source("src_new/05_functions.R")

## Data
df_analysis <- readRDS("data/final/ct_analysis_set.rds")
df_analysis <- format_names(df_analysis)
df_analysis$h_eerstegen <- NULL

df_analysis$match_id <- paste0(df_analysis$rinpersoon, "_", df_analysis$treat)
df_analysis$leeftijd2 <- df_analysis$leeftijd^2
df_analysis$leeftijd3 <- df_analysis$leeftijd^3
df_analysis$income_sm2 <- df_analysis$income_sm^2
df_analysis$income_sm3 <- df_analysis$income_sm^3  

## Load CT
ct_exc_sel_gm2 <- readRDS("H:/wmo_heterogeneity/models/ct_1000000_ms2500_exc_sel_gm_18plus.rds")
op_ct_exc_sel_gm2 <- prune_ct(ct_exc_sel_gm2, 14)

rpart.plot(op_ct_exc_sel_gm2)

df_analysis$group <- NA
df_analysis <- gen_grouped_df(op_ct_exc_sel_gm2, df_analysis)

saveRDS(df_analysis, "data/final/df_analysis_with_groups.rds")

df_analysis_inc_effect <- add_group_effects(df_analysis)
full_summ_tables <- make_summ_tables(df_analysis_inc_effect)
write_summ(full_summ_tables, "output/220711_Output_WMO3/group_summ_full.xlsx")

lm_mod <- estimate_effects(df_analysis_inc_effect)
write_lm_mod(lm_mod, "output/220711_Output_WMO3/group_multivariate_full.xlsx")

df_age <- make_age_effect(lm_mod)
df_inc <- make_inc_effect(lm_mod)

ggplot(df_age, aes(y = effect, x = leeftijd)) + geom_line()
ggplot(df_inc, aes(y = effect, x = income)) + geom_line()

## -- Amsterdam set
ct_sel_gem_363 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_363_18plus.rds")

df_analysis_363 <- df_analysis[df_analysis$gem_2019 == "363", ]
op_ct_sel_gem_363 <- prune_ct(ct_sel_gem_363, 12)
df_analysis_363 <- gen_grouped_df(op_ct_sel_gem_363, df_analysis_363)

df_analysis_363_inc_effect <- add_group_effects(df_analysis_363)
summ_tables_363 <- make_summ_tables(df_analysis_363_inc_effect)

write_summ(summ_tables_363, "output/220711_Output_WMO3/group_summ_363.xlsx")

lm_mod_363 <- estimate_effects(df_analysis_363_inc_effect)
write_lm_mod(lm_mod_363, "output/220711_Output_WMO3/group_multivariate_363.xlsx")

## -- 518 set
ct_sel_gem_518 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_518_18plus.rds")

df_analysis_518 <- df_analysis[df_analysis$gem_2019 == "518", ]
op_ct_sel_gem_518 <- prune_ct(ct_sel_gem_518, 8)
df_analysis_518 <- gen_grouped_df(op_ct_sel_gem_518, df_analysis_518)

df_analysis_518_inc_effect <- add_group_effects(df_analysis_518)
summ_tables_518 <- make_summ_tables(df_analysis_518_inc_effect)

write_summ(summ_tables_518, "output/220711_Output_WMO3/group_summ_518.xlsx")

lm_mod_518 <- estimate_effects(df_analysis_518_inc_effect)
write_lm_mod(lm_mod_518, "output/220711_Output_WMO3/group_multivariate_518.xlsx")

## -- 34 set
ct_sel_gem_34 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_34_18plus.rds")

df_analysis_34 <- df_analysis[df_analysis$gem_2019 == "34", ]
op_ct_sel_gem_34 <- prune_ct(ct_sel_gem_34, 8)
df_analysis_34 <- gen_grouped_df(op_ct_sel_gem_34, df_analysis_34)

df_analysis_34_inc_effect <- add_group_effects(df_analysis_34)
summ_tables_34 <- make_summ_tables(df_analysis_34_inc_effect)

write_summ(summ_tables_34, "output/220711_Output_WMO3/group_summ_34.xlsx")

lm_mod_34 <- estimate_effects(df_analysis_34_inc_effect)
write_lm_mod(lm_mod_34, "output/220711_Output_WMO3/group_multivariate_34.xlsx")

## -- 344 set
ct_sel_gem_344 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_344_18plus.rds")

df_analysis_344 <- df_analysis[df_analysis$gem_2019 == "344", ]
op_ct_sel_gem_344 <- prune_ct(ct_sel_gem_344, 8)
df_analysis_344 <- gen_grouped_df(op_ct_sel_gem_344, df_analysis_344)

df_analysis_344_inc_effect <- add_group_effects(df_analysis_344)
summ_tables_344 <- make_summ_tables(df_analysis_344_inc_effect)

write_summ(summ_tables_344, "output/220711_Output_WMO3/group_summ_344.xlsx")

lm_mod_344 <- estimate_effects(df_analysis_344_inc_effect)
write_lm_mod(lm_mod_344, "output/220711_Output_WMO3/group_multivariate_344.xlsx")

## -- 599 set
ct_sel_gem_599 <- readRDS("H:/wmo_heterogeneity/models/ct_500000_ms2500_gem_599_18plus.rds")

df_analysis_599 <- df_analysis[df_analysis$gem_2019 == "599", ]
op_ct_sel_gem_599 <- prune_ct(ct_sel_gem_599, 8)
df_analysis_599 <- gen_grouped_df(op_ct_sel_gem_599, df_analysis_599)

df_analysis_599_inc_effect <- add_group_effects(df_analysis_599)
summ_tables_599 <- make_summ_tables(df_analysis_599_inc_effect)

write_summ(summ_tables_599, "output/220711_Output_WMO3/group_summ_599.xlsx")

lm_mod_599 <- estimate_effects(df_analysis_599_inc_effect)
write_lm_mod(lm_mod_599, "output/220711_Output_WMO3/group_multivariate_599.xlsx")

openxlsx::write.xlsx(
  list(
    "Full"=make_group_descs(df_analysis),
    "363"=make_group_descs(df_analysis_363),
    "518"=make_group_descs(df_analysis_518),
    "599"=make_group_descs(df_analysis_599),
    "34"=make_group_descs(df_analysis_34),
    "344"=make_group_descs(df_analysis_344)
  ),
  file = "output/220711_Output_WMO3/group_desc.xlsx"
)



ggplot(summ_tables_599[[1]], aes(y=mean_effect, geslacht, size = n/100000)) + geom_point()
ggplot(summ_tables_599[[2]], aes(y=mean_effect, herkomst3, size = n/100000)) + geom_point()
ggplot(summ_tables_599[[3]], aes(y=mean_effect, huishouden, size = n/100000)) + geom_point()
ggplot(summ_tables_599[[4]], aes(y=mean_effect, income, size = n/100000)) + geom_point()
ggplot(summ_tables_599[[5]], aes(y=mean_effect, leeftijd, size = n/100000)) + geom_point()

