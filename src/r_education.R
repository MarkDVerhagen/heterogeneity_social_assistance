library(tidyverse)
library(grf)
library(causalTree)
library(rpart.plot)

source("H:/wmo_heterogeneity/src_new/r_functions.R")

## -- Booleans
run_grf <- F
run_ct <- F
run_exc_sel <- F
run_sel <- F

## -- Read education data
educ_2016 <- readRDS("H:/data/education/2016/rin_educ.rds")
educ_2019 <- readRDS("H:/data/education/2019/rin_educ.rds")

## -- Read analysis set
df_analysis <- readRDS("H:/wmo_heterogeneity/data/edit/df_analysis_2016_2019_inc.rds")

## -- Merge in education
tot_educ <- rbind(
  educ_2016 %>% mutate(treat = 0) %>% remove_numerics(),
  educ_2019 %>% mutate(treat = 1) %>% remove_numerics()
)

tot_educ <- tot_educ %>%
  mutate(rinpersoon = as.numeric(RINPERSOON)) %>%
  dplyr::select(-RINPERSOON)

df_analysis <- df_analysis %>%
  left_join(tot_educ, by = c("rinpersoon", "treat"))

df_analysis <- df_analysis %>%
  filter(leeftijd >= 18)

df_analysis$leeftijd <- round(df_analysis$leeftijd / 5)*5
df_analysis$lower_bound_num <- ifelse(df_analysis$lower_bound_num > 0,
                                             round(df_analysis$lower_bound_num / 5) * 5,
                                             df_analysis$lower_bound_num)

## Merge all very small muicipalities (< 25,000 inhabitants)
small_gms <- names(table(df_analysis$gem_2019))[(table(df_analysis$gem_2019) < 25000)]
df_analysis$gem_2019 <- as.character(df_analysis$gem_2019)

df_analysis$gem_2019[df_analysis$gem_2019 %in% small_gms] <- "small"

df_analysis$huishoudsamenstelling <- ifelse(df_analysis$huishoudsamenstelling %in%
                                                     c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                                   as.character(df_analysis$huishoudsamenstelling))

rm(educ_2016, educ_2019, tot_educ)
saveRDS(df_analysis, "data/edit/r_educ_ct_analysis_set.rds")
## -- GRF
# Make a train set which is a set fraction of the total
if (run_grf) {
  set.seed(1704)
  df_analysis <- df_analysis %>%
    sample_frac(0.1)
  
  overall_samples <- gen_samples_educ(df_analysis)
  rm(df_analysis)
  
  print("Starting analysis including gem_2019")
  
  start <- Sys.time()
  set.seed(1704)
  tau_forest <- causal_forest(X = overall_samples[[2]], Y = overall_samples[[4]], W = overall_samples[[3]], min.node.size = 1250,
                              num.trees = 500)
  end <- Sys.time()
  
  print(paste0("Time elapsed:", end - start))
  
  saveRDS(tau_forest, "H:/wmo_heterogeneity/models/r_educ_gm_tau_forest_010_1250ms_500t_18plus.rds")
  
  var_df <- data.frame(var_imp = grf::variable_importance(tau_forest),
                       variable = names(as.data.frame(tau_forest$X.orig)))
}

## -- Estimate causal trees

if (run_ct) {
  ## Encode
  df_analysis_dummy <- df_analysis %>%
    mutate(geslacht = ifelse(geslacht == "vrouw", 1, 0)) %>%
    fastDummies::dummy_cols(select_columns = c("herkomst", "huishoudsamenstelling", "hbopl"))
  
  names(df_analysis_dummy) <- case_when(
    grepl("herkomst_", names(df_analysis_dummy)) ~ gsub("herkomst_", "H_", names(df_analysis_dummy)),
    grepl("huishoudsamenstelling_", names(df_analysis_dummy)) ~ gsub("huishoudsamenstelling_", "HH_", names(df_analysis_dummy)),
    grepl("hbopl_", names(df_analysis_dummy)) ~ gsub("hbopl_", "OPL_", names(df_analysis_dummy)),
    TRUE ~ names(df_analysis_dummy)
  )
  
  names(df_analysis_dummy) <- gsub("lower_bound_num", "Income_SM", names(df_analysis_dummy))
  
  df_analysis_dummy <- df_analysis_dummy %>%
    dplyr::select(-huishoudsamenstelling, -inkomen_klasse, -herkomst, -contains("wmo_"), -year, -wc_2019, -percsm,
                  -inkomen_pers, -wc, -bc, -hbopl)
  
  set.seed(1704)
  df_analysis_sample <- df_analysis_dummy
  sample_n <- 500000
  rm(df_analysis)
  rm(df_analysis_dummy)
  
  if (run_exc_sel) {
    
    ## -- Estimate exc_sel_gem
    df_analysis_sample_exc_gm <- df_analysis_sample %>%
      filter(!(gem_2019 %in% c("344", "34", "599", "518", "363")))
    
    set.seed(1704)
    df_analysis_sample_exc_gm <- df_analysis_sample_exc_gm %>%
      sample_n(sample_n)
    
    df_analysis_sample_exc_gm$id <- 1 : nrow(df_analysis_sample_exc_gm)
    
    df_analysis_sample_exc_gm_train <- df_analysis_sample_exc_gm %>%
      sample_n(0.5*nrow(df_analysis_sample_exc_gm))
    
    df_analysis_sample_exc_gm_test <- df_analysis_sample_exc_gm %>%
      filter(!(id %in% df_analysis_sample_exc_gm_train$id))
    
    ## Ex-GM
    df_analysis_sample_exc_gm_ex_gm <- df_analysis_sample_exc_gm %>%
      dplyr::select(-H_eerstegen, -rinpersoon, -gem_2019, -id)
    df_analysis_sample_exc_gm_train_ex_gm <- df_analysis_sample_exc_gm_train %>%
      dplyr::select(-H_eerstegen, -rinpersoon, -gem_2019, -id)
    df_analysis_sample_exc_gm_test_ex_gm <- df_analysis_sample_exc_gm_test %>%
      dplyr::select(-H_eerstegen, -rinpersoon, -gem_2019, -id)
    
    ## Honest Causal Tree
    t1 <- Sys.time()
    set.seed(1704)
    honestTree_ex_gm <- honest.causalTree(y ~ .,
                                          data = df_analysis_sample_exc_gm_train_ex_gm,
                                          treatment = df_analysis_sample_exc_gm_train_ex_gm$treat,
                                          est_data = df_analysis_sample_exc_gm_test_ex_gm,
                                          est_treatment = df_analysis_sample_exc_gm_test_ex_gm$treat,
                                          split.Rule = "CT", split.Honest = T,
                                          HonestSampleSize = nrow(df_analysis_sample_exc_gm_test_ex_gm), cv.option = "fit",
                                          cv.Honest = F, minsize = 2500)
    t2 <- Sys.time()
    t2 - t1
    
    saveRDS(honestTree_ex_gm, "H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_exc_sel_gm_18plus.rds") 
  }
  
  if (run_sel) {
    ## -- Estimate sel_gem
    gms_grf <- c('363', '518', "344", "34", "599")
    
    df_analysis_sample_gms <- lapply(gms_grf,
                                     function(x) {
                                       set.seed(1704)
                                       df_analysis_sample[df_analysis_sample$gem_2019 == x, ] %>%
                                         sample_n(sample_n, replace = T)
                                     }
    )
    
    for (dataset in df_analysis_sample_gms) {
      gm_selected <- unique(dataset$gem_2019)
      print(gm_selected)
      dataset$id <- 1 : nrow(dataset)
      
      df_analysis_sample_gms_train <- dataset %>%
        sample_n(0.5*nrow(dataset))
      
      df_analysis_sample_gms_test <- dataset %>%
        filter(!(id %in% df_analysis_sample_gms_train$id))
      
      df_analysis_sample_gms_train <- df_analysis_sample_gms_train %>%
        dplyr::select(-rinpersoon, -gem_2019, -id, -H_eerstegen)
      
      df_analysis_sample_gms_test <- df_analysis_sample_gms_test %>%
        dplyr::select(-rinpersoon, -gem_2019, -id, -H_eerstegen)
      
      causalTree <- estimate_CT(df_analysis_sample_gms_train,
                                df_analysis_sample_gms_test)
      saveRDS(causalTree, paste0("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_", gm_selected, "_18plus.rds"))
    }
  }
}

####
## -- Analyze CT's
source("./src_new/05_functions.R")

## Data
df_analysis <- readRDS("data/edit/r_educ_ct_analysis_set.rds")
df_analysis <- df_analysis %>%
  filter(leeftijd >= 18)

df_analysis$leeftijd <- round(df_analysis$leeftijd / 2)*2
df_analysis$lower_bound_num <- ifelse(df_analysis$lower_bound_num > 0,
                                      round(df_analysis$lower_bound_num / 5) * 5,
                                      df_analysis$lower_bound_num)

df_analysis$huishoudsamenstelling <- ifelse(df_analysis$huishoudsamenstelling %in%
                                              c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                            as.character(df_analysis$huishoudsamenstelling))

## Merge all very small muicipalities (< 25,000 inhabitants)
df_analysis_dummy <- df_analysis %>%
  mutate(geslacht = ifelse(geslacht == "vrouw", 1, 0)) %>%
  fastDummies::dummy_cols(select_columns = c("herkomst", "huishoudsamenstelling", "hbopl"))

names(df_analysis_dummy) <- case_when(
  grepl("herkomst_", names(df_analysis_dummy)) ~ gsub("herkomst_", "H_", names(df_analysis_dummy)),
  grepl("huishoudsamenstelling_", names(df_analysis_dummy)) ~ gsub("huishoudsamenstelling_", "HH_", names(df_analysis_dummy)),
  grepl("hbopl_", names(df_analysis_dummy)) ~ gsub("hbopl_", "OPL_", names(df_analysis_dummy)),
  TRUE ~ names(df_analysis_dummy)
)

names(df_analysis_dummy) <- gsub("lower_bound_num", "Income_SM", names(df_analysis_dummy))

df_analysis_dummy <- df_analysis_dummy %>%
  dplyr::select(-huishoudsamenstelling, -inkomen_klasse, -herkomst, -contains("wmo_"), -year, -wc_2019, -percsm,
                -inkomen_pers, -wc, -bc, -hbopl)

saveRDS(df_analysis_dummy, "data/final/r_educ_ct_analysis_set.rds")

## -- Causal Tree analysis
df_analysis_dummy <- readRDS("data/final/r_educ_ct_analysis_set.rds")

options(scipen = 99)
df_analysis_sample <- df_analysis_dummy

## Ex_sel_GM
ct_exc_sel_gm <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_exc_sel_gm_18plus.rds")

plot(as.data.frame(ct_exc_sel_gm$cptable)$xerror[1:50])
source("./src_new/05_functions.R")

op_ct_exc_sel_gm <- prune_ct(ct_exc_sel_gm, 10)
rpart.plot(op_ct_exc_sel_gm)

ct_exc_sel_gm_results <- generate_output_ct(op_ct_object = op_ct_exc_sel_gm,
                                            df_sample = df_analysis_sample %>%
                                              filter(!(gem_2019 %in% c("344", "34", "599", "518", "363"))))

gen_ate_y_plot(ct_exc_sel_gm_results, hline = 0.0079)

# Including per GM: 363

ct_sel_gm_363 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_363_18plus.rds")
plot(as.data.frame(ct_sel_gm_363$cptable)$xerror[1:50])
op_ct_sel_gm_363 <- prune_ct(ct_sel_gm_363, 10)
rpart.plot(op_ct_sel_gm_363)
ct_gm_363_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_363,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "363"))

gen_ate_y_plot(ct_gm_363_results, hline = 0.01)

# Including per GM: 518

ct_sel_gm_518 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_518_18plus.rds")
plot(as.data.frame(ct_sel_gm_518$cptable)$xerror[1:50])
op_ct_sel_gm_518 <- prune_ct(ct_sel_gm_518, 10)
rpart.plot(op_ct_sel_gm_518)
ct_gm_518_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_518,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "518"))
gen_ate_y_plot(ct_gm_518_results, hline = -0.0013)

# Including per GM: 344

ct_sel_gm_344 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_344_18plus.rds")
plot(as.data.frame(ct_sel_gm_344$cptable)$xerror[1:50])
op_ct_sel_gm_344 <- prune_ct(ct_sel_gm_344, 12)
rpart.plot(op_ct_sel_gm_344)
ct_gm_344_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_344,
                                        df_sample = df_analysis_sample %>% filter(gem_2019 == "344"))
gen_ate_y_plot(ct_gm_344_results, hline=0.00026)


# Including per GM: 34

ct_sel_gm_34 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_34_18plus.rds")
plot(as.data.frame(ct_sel_gm_34$cptable)$xerror[1:50])
op_ct_sel_gm_34 <- prune_ct(ct_sel_gm_34, 9)
rpart.plot(op_ct_sel_gm_34)
ct_gm_34_results <- generate_output_ct(op_ct_object = op_ct_sel_gm_34,
                                       df_sample = df_analysis_sample %>% filter(gem_2019 == "34"))
gen_ate_y_plot(ct_gm_34_results, hline=0.0037)

# Including per GM: 599

ct_sel_gm_599 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_599_18plus.rds")
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
       'sel_gm_363_500K_2500ms_18plus' = ct_gm_363_results,
       'sel_gm_34_500K_2500ms_18plus' = ct_gm_34_results,
       'sel_gm_344_500K_2500ms_18plus' = ct_gm_344_results,
       'sel_gm_518_500K_2500ms_18plus' = ct_gm_518_results,
       'sel_gm_599_500K_2500ms_18plus' = ct_gm_599_results),
  file = "./output/220711_Output_WMO3/r_educ_ct_tables.xlsx"
)


# Elbow scores
df_elbows <- data.frame(exc_sel_gm_500K = as.data.frame(ct_exc_sel_gm$cptable)$xerror[1:25],
                        sel_gm_363_500K = as.data.frame(ct_sel_gm_363$cptable)$xerror[1:25],
                        sel_gm_34_500K = as.data.frame(ct_sel_gm_34$cptable)$xerror[1:25],
                        sel_gm_344_500K = as.data.frame(ct_sel_gm_344$cptable)$xerror[1:25],
                        sel_gm_599_500K = as.data.frame(ct_sel_gm_599$cptable)$xerror[1:25],
                        sel_gm_518_500K = as.data.frame(ct_sel_gm_518$cptable)$xerror[1:25])
writexl::write_xlsx(df_elbows, "./output/220711_Output_WMO3/r_educ_elbow_tables.xlsx")

####
## -- Make descriptives of the heterogenous groups

## Data
df_analysis <- readRDS("data/final/r_educ_ct_analysis_set.rds")
df_analysis <- format_names(df_analysis)

df_analysis$match_id <- paste0(df_analysis$rinpersoon, "_", df_analysis$treat)
df_analysis$leeftijd2 <- df_analysis$leeftijd^2
df_analysis$leeftijd3 <- df_analysis$leeftijd^3
df_analysis$income_sm2 <- df_analysis$income_sm^2
df_analysis$income_sm3 <- df_analysis$income_sm^3  

df_analysis$h_eerstegen <- NULL

## Load CT
ct_exc_sel_gm <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_exc_sel_gm_18plus.rds")
op_ct_exc_sel_gm <- prune_ct(ct_exc_sel_gm, 10)

df_analysis$group <- NA
df_analysis <- gen_grouped_df(op_ct_exc_sel_gm, df_analysis)
assertthat::assert_that(mean(is.na(df_analysis$group)) == 0)

df_analysis_inc_effect <- add_group_effects(df_analysis)

full_summ_tables <- make_summ_tables(df_analysis_inc_effect)
write_summ(full_summ_tables, "output/220711_Output_WMO3/r_educ_group_summ_full.xlsx")

lm_mod <- estimate_effects(df_analysis_inc_effect)
write_lm_mod(lm_mod, "output/220711_Output_WMO3/r_educ_group_multivariate_full.xlsx")

## -- Amsterdam set
ct_sel_gem_363 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_363_18plus.rds")

df_analysis_363 <- df_analysis[df_analysis$gem_2019 == "363", ]
df_analysis_363$group <- NA
op_ct_sel_gem_363 <- prune_ct(ct_sel_gem_363, 12)
df_analysis_363 <- gen_grouped_df(op_ct_sel_gem_363, df_analysis_363)
assertthat::assert_that(mean(is.na(df_analysis_363$group)) == 0)

df_analysis_363_inc_effect <- add_group_effects(df_analysis_363)
summ_tables_363 <- make_summ_tables(df_analysis_363_inc_effect)

write_summ(summ_tables_363, "output/220711_Output_WMO3/r_educ_group_summ_363.xlsx")

lm_mod_363 <- estimate_effects(df_analysis_363_inc_effect)
write_lm_mod(lm_mod_363, "output/220711_Output_WMO3/r_educ_group_multivariate_363.xlsx")

## -- 518 set
ct_sel_gem_518 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_518_18plus.rds")

df_analysis_518 <- df_analysis[df_analysis$gem_2019 == "518", ]
df_analysis_518$group <- NA
op_ct_sel_gem_518 <- prune_ct(ct_sel_gem_518, 8)
df_analysis_518 <- gen_grouped_df(op_ct_sel_gem_518, df_analysis_518)
assertthat::assert_that(mean(is.na(df_analysis_518$group)) == 0)

df_analysis_518_inc_effect <- add_group_effects(df_analysis_518)
summ_tables_518 <- make_summ_tables(df_analysis_518_inc_effect)

write_summ(summ_tables_518, "output/220711_Output_WMO3/r_educ_group_summ_518.xlsx")

lm_mod_518 <- estimate_effects(df_analysis_518_inc_effect)
write_lm_mod(lm_mod_518, "output/220711_Output_WMO3/r_educ_group_multivariate_518.xlsx")

## -- 34 set
ct_sel_gem_34 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_34_18plus.rds")

df_analysis_34 <- df_analysis[df_analysis$gem_2019 == "34", ]
df_analysis_34$group <- NA
op_ct_sel_gem_34 <- prune_ct(ct_sel_gem_34, 8)
df_analysis_34 <- gen_grouped_df(op_ct_sel_gem_34, df_analysis_34)
assertthat::assert_that(mean(is.na(df_analysis_34$group)) == 0)

df_analysis_34_inc_effect <- add_group_effects(df_analysis_34)
summ_tables_34 <- make_summ_tables(df_analysis_34_inc_effect)

write_summ(summ_tables_34, "output/220711_Output_WMO3/r_educ_group_summ_34.xlsx")

lm_mod_34 <- estimate_effects(df_analysis_34_inc_effect)
write_lm_mod(lm_mod_34, "output/220711_Output_WMO3/r_educ_group_multivariate_34.xlsx")

## -- 344 set
ct_sel_gem_344 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_344_18plus.rds")

df_analysis_344 <- df_analysis[df_analysis$gem_2019 == "344", ]
df_analysis_344$group <- NA
op_ct_sel_gem_344 <- prune_ct(ct_sel_gem_344, 8)
df_analysis_344 <- gen_grouped_df(op_ct_sel_gem_344, df_analysis_344)
assertthat::assert_that(mean(is.na(df_analysis_344$group)) == 0)

df_analysis_344_inc_effect <- add_group_effects(df_analysis_344)
summ_tables_344 <- make_summ_tables(df_analysis_344_inc_effect)

write_summ(summ_tables_344, "output/220711_Output_WMO3/r_educ_group_summ_344.xlsx")

lm_mod_344 <- estimate_effects(df_analysis_344_inc_effect)
write_lm_mod(lm_mod_344, "output/220711_Output_WMO3/r_educ_group_multivariate_344.xlsx")

## -- 599 set
ct_sel_gem_599 <- readRDS("H:/wmo_heterogeneity/models/r_educ_ct_500000_ms2500_gem_599_18plus.rds")

df_analysis_599 <- df_analysis[df_analysis$gem_2019 == "599", ]
df_analysis_599$group <- NA
op_ct_sel_gem_599 <- prune_ct(ct_sel_gem_599, 8)
df_analysis_599 <- gen_grouped_df(op_ct_sel_gem_599, df_analysis_599)
assertthat::assert_that(mean(is.na(df_analysis_599$group)) == 0)

df_analysis_599_inc_effect <- add_group_effects(df_analysis_599)
summ_tables_599 <- make_summ_tables(df_analysis_599_inc_effect)

write_summ(summ_tables_599, "output/220711_Output_WMO3/r_educ_group_summ_599.xlsx")

lm_mod_599 <- estimate_effects(df_analysis_599_inc_effect)
write_lm_mod(lm_mod_599, "output/220711_Output_WMO3/r_educ_group_multivariate_599.xlsx")

openxlsx::write.xlsx(
  list(
    "Full"=make_group_descs(df_analysis),
    "363"=make_group_descs(df_analysis_363),
    "518"=make_group_descs(df_analysis_518),
    "599"=make_group_descs(df_analysis_599),
    "34"=make_group_descs(df_analysis_34),
    "344"=make_group_descs(df_analysis_344)
  ),
  file = "output/220711_Output_WMO3/r_educ_group_desc.xlsx"
)

