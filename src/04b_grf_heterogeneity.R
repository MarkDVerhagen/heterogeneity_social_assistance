### Script to generate causal forests
## Input
#' @input df_2016_2019_inc dataset including all relevant data for 2016 and 2019
## Output
#' @output data/edit/overall_samples_033_all.rds the sample used for GRF estimation
#' @output models/non_gm_tau_forest_033_1250ms_500t_18plus.rds forest estimated without municipality codes
#' @output models/gm_tau_forest_033_1250ms_500t_18plus.rds forest estimated with municipality codes
#' @output data/final/vi_gm_tau_forest_033_1250ms_500t_18plus.rds variable importance table including municipality codes
###

## Set seed
set.seed(1704)

## Load libraries
library(tidyverse)
library(grf)

## Read data
df_analysis_sample <- readRDS("data/edit/df_analysis_2016_2019_inc.rds")
df_analysis_sample <- df_analysis_sample %>%
  filter(leeftijd >= 18)

## Transform age into 5-year intervals for computational convenience
df_analysis_sample$leeftijd <- round(df_analysis_sample$leeftijd / 5)*5
## Transform income into 5% intervals for computational convenience
df_analysis_sample$lower_bound_num <- ifelse(
  df_analysis_sample$lower_bound_num > 0,
  round(df_analysis_sample$lower_bound_num / 5) * 5,
  df_analysis_sample$lower_bound_num)

## Merge all very small muicipalities (< 25,000 inhabitants)
small_gms <- names(table(df_analysis_sample$gem_2019))[(table(df_analysis_sample$gem_2019) < 25000)]
df_analysis_sample$gem_2019 <- as.character(df_analysis_sample$gem_2019)
df_analysis_sample$gem_2019[df_analysis_sample$gem_2019 %in% small_gms] <- "small"

## Make a train set which is a set fraction of the total
df_analysis_sample <- df_analysis_sample %>%
  sample_frac(0.33)

## Generate samples requires for causal forest analysis
overall_samples <- gen_samples(df_analysis_sample)
saveRDS(overall_samples, "data/edit/overall_samples_033_all.rds")

## Sample analysis excluding gem
print("Starting analysis witout gem_2019")

start <- Sys.time()
tau_forest <- causal_forest(X = overall_samples[[1]], Y = overall_samples[[4]], W = overall_samples[[3]], min.node.size = 1250,
                            num.trees = 500)
end <- Sys.time()

print(paste0("Time elapsed:", end - start))

## Save forest
saveRDS(tau_forest, "models/non_gm_tau_forest_033_1250ms_500t_18plus.rds")

## Sample analysis including gem
print("Starting analysis including gem_2019")

start <- Sys.time()
set.seed(1704)
tau_forest <- causal_forest(X = overall_samples[[2]], Y = overall_samples[[4]], W = overall_samples[[3]], min.node.size = 1250,
                            num.trees = 500)
end <- Sys.time()

print(paste0("Time elapsed:", end - start))

## Save forest
saveRDS(tau_forest, "models/gm_tau_forest_033_1250ms_500t_18plus.rds")

## Make variable importance dataframe
var_df <- data.frame(var_imp = grf::variable_importance(tau_forest),
                     variable = names(as.data.frame(tau_forest$X.orig)))

## Save variable importance table
saveRDS(var_df, "data/final/vi_gm_tau_forest_033_1250ms_500t_18plus.rds")