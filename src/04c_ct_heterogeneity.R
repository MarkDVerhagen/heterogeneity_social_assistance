### Script to generate causal trees
## Input
#' @input df_2016_2019_inc dataset including all relevant data for 2016 and 2019
## Output
#' @output models/ct_500000_ms2500_exc_sel_gm_18plus.rds
#' @output models/ct_500000_ms2500_gem_363_18plus.rds
#' @output models/ct_500000_ms2500_gem_518_18plus.rds
#' @output models/ct_500000_ms2500_gem_344_18plus.rds
#' @output models/ct_500000_ms2500_gem_34_18plus.rds
#' @output models/ct_500000_ms2500_gem_599_18plus.rds
###

## Load libraries
library(tidyverse)
library(causalTree)

## Load data
df_analysis <- readRDS("data/edit/df_analysis_2016_2019_inc.rds")
df_analysis <- df_analysis %>%
  filter(leeftijd >= 18)

## Include age in steps of two
df_analysis$leeftijd <- round(df_analysis$leeftijd / 2)*2

## Encode
df_analysis_dummy <- df_analysis %>%
  mutate(geslacht = ifelse(geslacht == "vrouw", 1, 0)) %>%
  fastDummies::dummy_cols(select_columns = c("herkomst", "huishoudsamenstelling"))

## Rename columns to make tree analysis easier
names(df_analysis_dummy) <- case_when(
  grepl("herkomst_", names(df_analysis_dummy)) ~ gsub("herkomst_", "H_", names(df_analysis_dummy)),
  grepl("huishoudsamenstelling_", names(df_analysis_dummy)) ~ gsub("huishoudsamenstelling_", "HH_", names(df_analysis_dummy)),
  TRUE ~ names(df_analysis_dummy)
)
names(df_analysis_dummy) <- gsub("lower_bound_num", "Income_SM", names(df_analysis_dummy))

## Only retain dummy version of variables
df_analysis_dummy <- df_analysis_dummy %>%
  dplyr::select(-huishoudsamenstelling, -inkomen_klasse, -herkomst, -contains("wmo_"), -year, -wc_2019, -percsm,
                -inkomen_pers, -wc, -bc)

df_analysis_sample <- df_analysis_dummy
sample_n <- 500000

## Clean space
rm(df_analysis)
rm(df_analysis_dummy)

## Analyses per alternate municipality
gms_grf <- c('363', '518', "344", "34", "599")

## Make samples of 500,000
df_analysis_sample_gms <- lapply(
  gms_grf,
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
  saveRDS(causalTree, paste0("models/ct_500000_ms2500_gem_", gm_selected, "_18plus.rds"))
}


## Analysis for consistent municipalities
df_analysis_sample <- df_analysis_sample %>%
  filter(!(gem_2019 %in% c("344", "34", "599", "518", "363")))

set.seed(1704)
df_analysis_sample <- df_analysis_sample %>%
  sample_n(sample_n)

df_analysis_sample$id <- 1 : nrow(df_analysis_sample)

df_analysis_sample_train <- df_analysis_sample %>%
  sample_n(0.5*nrow(df_analysis_sample))

df_analysis_sample_test <- df_analysis_sample %>%
  filter(!(id %in% df_analysis_sample_train$id))

## Ex-GM
df_analysis_sample_ex_gm <- df_analysis_sample %>%
  dplyr::select(-H_eerstegen, -rinpersoon, -gem_2019, -id)
df_analysis_sample_train_ex_gm <- df_analysis_sample_train %>%
  dplyr::select(-H_eerstegen, -rinpersoon, -gem_2019, -id)
df_analysis_sample_test_ex_gm <- df_analysis_sample_test %>%
  dplyr::select(-H_eerstegen, -rinpersoon, -gem_2019, -id)

## Honest Causal Tree
t1 <- Sys.time()

honestTree_ex_gm <- estimate_CT(
  df_analysis_sample_train_ex_gm,
  df_analysis_sample_test_ex_gm)

saveRDS(honestTree_ex_gm, "models/ct_500000_ms2500_exc_sel_gm_18plus.rds")