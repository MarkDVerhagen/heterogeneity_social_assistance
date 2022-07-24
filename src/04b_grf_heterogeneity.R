### 04_model_descriptives.R
## Description: Script to assess heterogeneity in the effect of the policy change
###
library(tidyverse)
library(grf)


df_analysis_sample <- readRDS("data/edit/df_analysis_2016_2019_inc.rds")
df_analysis_sample <- df_analysis_sample %>%
  filter(leeftijd >= 18)

df_analysis_sample$leeftijd <- round(df_analysis_sample$leeftijd / 5)*5
df_analysis_sample$lower_bound_num <- ifelse(df_analysis_sample$lower_bound_num > 0,
                                             round(df_analysis_sample$lower_bound_num / 5) * 5,
                                             df_analysis_sample$lower_bound_num)

## Merge all very small muicipalities (< 25,000 inhabitants)
small_gms <- names(table(df_analysis_sample$gem_2019))[(table(df_analysis_sample$gem_2019) < 25000)]
df_analysis_sample$gem_2019 <- as.character(df_analysis_sample$gem_2019)

df_analysis_sample$gem_2019[df_analysis_sample$gem_2019 %in% small_gms] <- "small"

df_analysis_sample$huishoudsamenstelling <- ifelse(df_analysis_sample$huishoudsamenstelling %in%
                                                     c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                                   as.character(df_analysis_sample$huishoudsamenstelling))


# Make a train set which is a set fraction of the total
set.seed(1704)
df_analysis_sample <- df_analysis_sample %>%
  sample_frac(0.33)

# saveRDS(df_analysis_sample_train$rinpersoon, "data/edit/train_sample_075_55.rds")

# df_analysis_sample_test <- df_analysis_sample %>%
#   filter(!(rinpersoon %in% df_analysis_sample_train$rinpersoon))
# 
# ## Unit tests
# assertthat::assert_that(all(unique(df_analysis_sample_train$gem_2019) %in%
#                               unique(df_analysis_sample_test$gem_2019)))
# 
# assertthat::assert_that(all(unique(df_analysis_sample_train$gem_2019) %in%
#                               unique(df_analysis_sample$gem_2019)))
# 
# n_gem <- as.data.frame(table(df_analysis_sample_train$gem_2019))

overall_samples <- gen_samples(df_analysis_sample)

# saveRDS(overall_samples, "H:/wmo_heterogeneity/data/edit/overall_samples_033_all.rds")

## Sample analysis excluding gem
print("Starting analysis witout gem_2019")

start <- Sys.time()
tau_forest <- causal_forest(X = overall_samples[[1]], Y = overall_samples[[4]], W = overall_samples[[3]], min.node.size = 1250,
                            num.trees = 500)
end <- Sys.time()

print(paste0("Time elapsed:", end - start))

saveRDS(tau_forest, "models/non_gm_tau_forest_033_1250ms_500t_18plus.rds")

grf::average_treatment_effect(tau_forest)

## Sample analysis including gem
print("Starting analysis including gem_2019")

start <- Sys.time()
set.seed(1704)
tau_forest <- causal_forest(X = overall_samples[[2]], Y = overall_samples[[4]], W = overall_samples[[3]], min.node.size = 1250,
                            num.trees = 500)
end <- Sys.time()

print(paste0("Time elapsed:", end - start))

saveRDS(tau_forest, "models/gm_tau_forest_033_1250ms_500t_18plus.rds")

grf::average_treatment_effect(tau_forest)

var_df <- data.frame(var_imp = grf::variable_importance(tau_forest),
                     variable = names(as.data.frame(tau_forest$X.orig)))

saveRDS(var_df, "data/final/vi_gm_tau_forest_033_1250ms_500t_18plus.rds")

