### 04_ct_heterogeneity.R
## Description: Script to assess heterogeneity in the effect of the policy change using a causalTree
###
library(tidyverse)
library(causalTree)

## Type of estimation
ex_gm <- F
sel_gm <- F
exc_sel_gm <- T
inc_sel_gm <- F

# ## Load data (cover 2016)
# df_analysis <- readRDS("data/edit/df_analysis_2016_2019.rds") %>%
#   mutate(gem_2019 = as.factor(gem_2019))
# 
# ## Load continuous income
# num_inc_2016 <- readRDS("H:/data/numeric_income/2016/rin_num_income.rds")
# num_inc_2019 <- readRDS("H:/data/numeric_income/2019/rin_num_income.rds")
# 
# num_inc_tot <- bind_rows(
#   num_inc_2016 %>% mutate(year = 2016),
#   num_inc_2019 %>% mutate(year = 2019)
# )
# 
# df_analysis <- df_analysis %>%
#   left_join(num_inc_tot)
# 
# df_analysis <- df_analysis %>%
#   filter(!is.na(lower_bound_num))
#
# saveRDS(df_analysis, "data/edit/df_analysis_2016_2019_inc.rds")

df_analysis <- readRDS("data/edit/df_analysis_2016_2019_inc.rds")
df_analysis <- df_analysis %>%
  filter(leeftijd >= 18)

df_analysis$leeftijd <- round(df_analysis$leeftijd / 2)*2
df_analysis$lower_bound_num <- ifelse(df_analysis$lower_bound_num > 0,
                                             round(df_analysis$lower_bound_num / 5) * 5,
                                             df_analysis$lower_bound_num)

df_analysis$huishoudsamenstelling <- ifelse(df_analysis$huishoudsamenstelling %in%
                                              c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                            as.character(df_analysis$huishoudsamenstelling))

## Encode
df_analysis_dummy <- df_analysis %>%
  mutate(geslacht = ifelse(geslacht == "vrouw", 1, 0)) %>%
  fastDummies::dummy_cols(select_columns = c("herkomst", "huishoudsamenstelling"))

names(df_analysis_dummy) <- case_when(
  grepl("herkomst_", names(df_analysis_dummy)) ~ gsub("herkomst_", "H_", names(df_analysis_dummy)),
  grepl("huishoudsamenstelling_", names(df_analysis_dummy)) ~ gsub("huishoudsamenstelling_", "HH_", names(df_analysis_dummy)),
  TRUE ~ names(df_analysis_dummy)
)

names(df_analysis_dummy) <- gsub("lower_bound_num", "Income_SM", names(df_analysis_dummy))

df_analysis_dummy <- df_analysis_dummy %>%
  dplyr::select(-huishoudsamenstelling, -inkomen_klasse, -herkomst, -contains("wmo_"), -year, -wc_2019, -percsm,
                -inkomen_pers, -wc, -bc)

set.seed(1704)
df_analysis_sample <- df_analysis_dummy
sample_n <- 1000000
rm(df_analysis)
rm(df_analysis_dummy)

# saveRDS(df_analysis_sample, "data/edit/df_analysis_sample.rds")
## Analyses for a selection of GM

if (sel_gm) {
  ## Analyses by GM
  gms_grf <- c('363', '518', "344", "34", "599")
  
  df_analysis_sample_gms <- lapply(gms_grf,
                                   function(x) {
                                     set.seed(1704)
                                     df_analysis_sample[df_analysis_sample$gem_2019 == x, ] %>%
                                       sample_n(sample_n, replace = T)
                                   }
  )
  
  
  
  estimate_CT <- function(data_train, data_test, seed=1704, minsize = 2500) {
    ## Function to estimate a CausalTree
    #' @param data_train training data
    #' @param data_test testing data
    #' @param seed seed
    #' @return estimated CausalTree
    
    t1 <- Sys.time()
    set.seed(seed)
    honestTree <- honest.causalTree(y ~ .,
                                    data = data_train,
                                    treatment = data_train$treat,
                                    est_data = data_test,
                                    est_treatment = data_test$treat,
                                    split.Rule = "CT", split.Honest = T,
                                    HonestSampleSize = nrow(data_test), cv.option = "fit",
                                    cv.Honest = F, minsize = minsize)
    t2 <- Sys.time()
    print(t2 - t1)
    return(honestTree)
  }
  
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
    saveRDS(causalTree, paste0("H:/wmo_heterogeneity/models/ct_200000_ms2500_gem_", gm_selected, "_18plus.rds"))
  }
}


## No subsettings
if (ex_gm) {
  ## All data, excluding all municipality codes
  
  set.seed(1704)
  df_analysis_sample <- df_analysis_sample %>%
    sample_frac(sample_frac)
  
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
  set.seed(1704)
  honestTree_ex_gm <- honest.causalTree(y ~ .,
                                        data = df_analysis_sample_train_ex_gm,
                                        treatment = df_analysis_sample_train_ex_gm$treat,
                                        est_data = df_analysis_sample_test_ex_gm,
                                        est_treatment = df_analysis_sample_test_ex_gm$treat,
                                        split.Rule = "CT", split.Honest = T,
                                        HonestSampleSize = nrow(df_analysis_sample_test_ex_gm), cv.option = "fit",
                                        cv.Honest = F, minsize = 50)
  t2 <- Sys.time()
  t2 - t1
  
  saveRDS(honestTree_ex_gm, "H:/wmo_heterogeneity/models/ct_005_25_ex_gm_all.rds")  
}

if (exc_sel_gm) {
  ## All data, excluding the GRF municipality codes
  
  # based on GRF
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
  set.seed(1704)
  honestTree_ex_gm <- honest.causalTree(y ~ .,
                                        data = df_analysis_sample_train_ex_gm,
                                        treatment = df_analysis_sample_train_ex_gm$treat,
                                        est_data = df_analysis_sample_test_ex_gm,
                                        est_treatment = df_analysis_sample_test_ex_gm$treat,
                                        split.Rule = "CT", split.Honest = T,
                                        HonestSampleSize = nrow(df_analysis_sample_test_ex_gm), cv.option = "fit",
                                        cv.Honest = F, minsize = 2500)
  t2 <- Sys.time()
  t2 - t1

  saveRDS(honestTree_ex_gm, "H:/wmo_heterogeneity/models/ct_1000000_ms2500_exc_sel_gm_18plus.rds")  
}

if (inc_sel_gm) {
  ## GM SEL
  # based on GRF
  gm_selection <- paste0("gem_2019_", c("344", "34", "599", "518", "363"))
  
  other_selection <- names(df_analysis_sample %>%
                             dplyr::select(-gem_2019, -H_eerstegen, -rinpersoon))
  
  df_analysis_sample_gm_sel <- df_analysis_sample %>%
    dplyr::select(-H_eerstegen, -rinpersoon) %>%
    fastDummies::dummy_cols(select_columns = "gem_2019", remove_selected_columns = T) %>%
    dplyr::select_at(vars(c(gm_selection, other_selection)))
  
  set <- unique(df_analysis$gem_2019)
  
  df_analysis_sample_train_gm_sel <- df_analysis_sample_train %>%
    dplyr::select(-H_eerstegen, -rinpersoon) %>%
    fastDummies::dummy_cols(select_columns = "gem_2019", remove_selected_columns = T) %>%
    dplyr::select_at(vars(c(gm_selection, other_selection)))
  df_analysis_sample_test_gm_sel <- df_analysis_sample_test %>%
    dplyr::select(-H_eerstegen, -rinpersoon) %>%
    fastDummies::dummy_cols(select_columns = "gem_2019", remove_selected_columns = T) %>%
    dplyr::select_at(vars(c(gm_selection, other_selection)))
  
  rm(num_inc_2016)
  rm(num_inc_2019)
  rm(num_inc_tot)
  rm(df_analysis)
  rm(df_analysis_0)
  rm(df_analysis_1)
  
  ## Honest Causal Tree
  t1 <- Sys.time()
  set.seed(1704)
  honestTree_sel_gm <- honest.causalTree(y ~ .,
                                         data = df_analysis_sample_train_gm_sel,
                                         treatment = df_analysis_sample_train_gm_sel$treat,
                                         est_data = df_analysis_sample_test_gm_sel,
                                         est_treatment = df_analysis_sample_test_gm_sel$treat,
                                         split.Rule = "CT", split.Honest = T,
                                         HonestSampleSize = nrow(df_analysis_sample_test_gm_sel), cv.option = "fit",
                                         cv.Honest = F, minsize = 50)
  t2 <- Sys.time()
  t2 - t1
  
  saveRDS(honestTree_sel_gm, "H:/wmo_heterogeneity/models/ct_250000_inc_gm_sel_all.rds")
}



# 
# 
# min_complex <- which.min(honestTree_ex_gm$cptable[, 4])
# opcp <- honestTree_ex_gm$cptable[, 1][min_complex]
# op_honestTree_ex_gm <- prune(honestTree_ex_gm, opcp)
# rpart.plot(op_honestTree_ex_gm)
# 
# ## -- Old code
# 
# 
# tree_ex_gm <- causalTree(y ~ ., data = quick_sample,
#                          treatment = quick_sample$treat, split.Rule = "CT", split.Honest = T, 
#                          cv.option = "matching", cv.Honest = F, split.Bucket = F, xval = 10)
# 
# rpart.plot(tree_ex_gm)
# 
# opcp <- tree_ex_gm$cptable[, 1][which.min(tree_ex_gm$cptable[,4])]
# opcp_df <- as.data.frame(tree_ex_gm$cptable)
# 
# ggplot(opcp_df) + geom_point(aes(y = xerror, x = nsplit))
# 
# head(opcp_df[order(opcp_df$xerror, decreasing = F), ])
# 
# ## Prune tree
# op_tree_ex_gm <- prune(tree_ex_gm, cp = opcp_df$CP[opcp_df$nsplit == 15])
# 
# rpart.plot(op_tree_ex_gm)
# 
# dim(simulation.1)
# typeof(df_analysis_sample_test)
# 
# 
# gen_samples <- function(df) {
#   X_sample <- df %>%
#     select(gem_2019, huishoudsamenstelling, leeftijd,
#            geslacht, inkomen_klasse, herkomst, treat)
#   
#   X_sample_dummies <- X_sample %>%
#     dplyr::select(leeftijd, geslacht, herkomst, huishoudsamenstelling,
#                   inkomen_klasse, gem_2019) %>%
#     fastDummies::dummy_columns(select_columns =
#                                  c("geslacht", "herkomst", "huishoudsamenstelling",
#                                    "inkomen_klasse", "gem_2019"),
#                                remove_selected_columns = T)
#   
#   
#   ## Sample analysis excluding gem
#   W_sample <- X_sample$treat
#   X_sample_matrix_ex_gm <- as.matrix(X_sample_dummies %>%
#                                        dplyr::select(-contains("gem_2019")))
#   X_sample_matrix_inc_gm <- as.matrix(X_sample_dummies)
#   Y_sample <- df$y
#   return(list(X_sample_matrix_ex_gm, X_sample_matrix_inc_gm, W_sample, Y_sample))
# }
# 
# train_samples <- gen_samples(df_analysis_sample_train)
# test_samples <- gen_samples(df_analysis_sample_test)
# saveRDS(test_samples, "data/edit/test_samples_04.rds")
# 
# ## Analysis excluding GEM level
# ## Clean space
# rm(df_analysis)
# rm(df_analysis_0)
# rm(df_analysis_1)
# rm(X_sample_dummies)
# rm(X_sample)
# 
# print("Starting sampled analysis witout gem_2019")
# 
# start <- Sys.time()
# # tau_forest <- causal_forest(X = train_samples[[1]], Y = train_samples[[4]], W = train_samples[[3]])
# end <- Sys.time()
# 
# print(paste0("Time elapsed:", end - start))
# 
# saveRDS(tau_forest, "models/non_gm_tau_forest_04.rds")
# 
# ## Sample analysis including gem
# 
# print("Starting sampled analysis including gem_2019")
# 
# start <- Sys.time()
# tau_forest <- causal_forest(X = train_samples[[2]], Y = train_samples[[4]], W = train_samples[[3]])
# end <- Sys.time()
# 
# print(paste0("Time elapsed:", end - start))
# 
# saveRDS(tau_forest, "models/gm_tau_forest_04.rds")
