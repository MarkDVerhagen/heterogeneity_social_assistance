### -- 03_model_descriptives.R
## Description: Script to assess predictive ability of WMO use
## Input:
## - ["data/edit/full_df_2016_2019.rds"]
## - Information on continuous rather than categorical income
## - ["data/edit/wmo_demog_15_19.rda"]
## - wmo_demog_2015 dataframe with demograpics and social assistance use in 2015
## - wmo_demog_2016 dataframe with demograpics and social assistance use in 2016
## - wmo_demog_2017 dataframe with demograpics and social assistance use in 2017
## - wmo_demog_2018 dataframe with demograpics and social assistance use in 2018
## - wmo_demog_2019 dataframe with demograpics and social assistance use in 2019
## - ["data/edit/cw_bu_gem_2019.rda"]
## - kwb_2016: all municipalities in 2016 including the 2019 equivalent
## - kwb_2017: all municipalities in 2017 including the 2019 equivalent
## - kwb_2018: all municipalities in 2018 including the 2019 equivalent
## Output:
## - ["data/edit/full_wmo_2016_cover.rds"]: dataset including all relevant data for 2016 and 2019
###

## Load libraries
library(lme4)
library(tidyverse)
library(patchwork)

source("src/03_functions.R")

## Load data (cover 2016)
df_analysis <- readRDS("H:/wmo_heterogeneity/data/edit/full_df_2016_2019.rds") %>%
  filter(year %in% c(2016, 2019))

##
df_analysis <- df_analysis[df_analysis$leeftijd >= 18, ]
df_analysis$leeftijd2 <- df_analysis$leeftijd^2
df_analysis$lower_bound_num2 <- df_analysis$lower_bound_num^2
df_analysis_2016 <- df_analysis[df_analysis$year == 2016, ]
df_analysis_2019 <- df_analysis[df_analysis$year == 2019, ]

## Include education
educ_2016 <- readRDS("H:/data/education/2016/rin_educ.rds")
educ_2019 <- readRDS("H:/data/education/2019/rin_educ.rds")

## -- Merge in education
tot_educ <- rbind(
  educ_2016 %>% mutate(treat = 0) %>% remove_numerics(),
  educ_2019 %>% mutate(treat = 1) %>% remove_numerics()
)

tot_educ <- tot_educ %>%
  mutate(rinpersoon = as.numeric(RINPERSOON)) %>%
  dplyr::select(-RINPERSOON)

df_analysis <- df_analysis %>%
  left_join(tot_educ)

df_analysis$income_cat <- round(df_analysis$lower_bound_num / 25)

## Including treatment effect
lm_treat <- lm(y ~ 1 + treat + leeftijd + leeftijd2 + as.factor(herkomst) +
                 as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                 as.factor(income_cat), data = df_analysis)

saveRDS(lm_treat, "H:/wmo_heterogeneity/models/lm_treat.rds")

## Basic model analysis based on 2016

df_analysis_2016 <- df_analysis %>%
  filter(year == 2016)

lm_2016 <- lm(y ~ 1 + leeftijd + leeftijd2 + as.factor(herkomst) +
                  as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                  as.factor(income_cat), data = df_analysis_2016, y = F, model = F)
lm_2016$effects <- c()

saveRDS(lm_2016, 'models/lm_2016.rds')

lm_2016_educ <- lm(y ~ 1 + leeftijd + leeftijd2 + as.factor(herkomst) +
                     as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                     as.factor(income_cat) + as.factor(hbopl),
                   data = df_analysis_2016, y = F,
                   model = F)
lm_2016_educ$effects <- c()

saveRDS(lm_2016_educ, 'models/lm_2016_educ.rds')

## Multilevel
## Random intercepts
lmer_2016 <- lmer(y ~ (1 | gem_2019) + leeftijd + leeftijd2 + as.factor(herkomst) +
                    as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                    as.factor(income_cat),
                  data = df_analysis_2016)
saveRDS(lmer_2016, "models/lmer_2016.rds")

lmer_2016_educ <- lmer(y ~ (1 | gem_2019) + leeftijd + leeftijd2 + as.factor(herkomst) +
                         as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                         as.factor(income_cat),
                       data = df_analysis_2016)
saveRDS(lmer_2016_educ, "models/lmer_2016_educ.rds")

## Save coefficient frames

lm_2016_coefs <- gen_coef_df(lm_2016)
writexl::write_xlsx(lm_2016_coefs, "H:/wmo_heterogeneity/tables/coefs_2016_v2.xlsx")

lmer_2016_coefs <- gen_coef_df(lmer_2016)
writexl::write_xlsx(lmer_2016_coefs, "H:/wmo_heterogeneity/tables/coefs_lmer_2016_v2.xlsx")

lm_2016_coefs_educ <- gen_coef_df(lm_2016_educ)
writexl::write_xlsx(lm_2016_coefs_educ, "H:/wmo_heterogeneity/tables/coefs_2016_educ.xlsx")

lmer_2016_coefs_educ <- gen_coef_df(lmer_2016_educ)
writexl::write_xlsx(lmer_2016_coefs_educ, "H:/wmo_heterogeneity/tables/coefs_lmer_2016_educ.xlsx")

## Generate plots of coefs
lm_2016_coefs <- format_labels(lm_2016_coefs)
lmer_2016_coefs <- format_labels(lmer_2016_coefs)

comb_2016_coefs <- rbind(
  lm_2016_coefs %>% dplyr::select(-`Pr(>|t|)`) %>% mutate(model = "LM"),
  lmer_2016_coefs %>% mutate(model = "LMER")
)

gen_coef_plot(comb_2016_coefs)

age_lm <- gen_age_effect(lm_2016_coefs)
age_lmer <- gen_age_effect(lmer_2016_coefs)

## Show random effects
re_df_2016 <- gen_re_df(lmer_2016) %>% mutate(year = "2016")

re_df_2016$id <- 1 : nrow(re_df_2016)
n_gem <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(n = n()) %>%
  mutate(n = DescTools::RoundTo(n, 5))
re_df_2016 <- re_df_2016 %>%
  mutate(gem_2019 = as.numeric(as.character(gem_2019))) %>%
  left_join(n_gem)

assertthat::assert_that(all(!is.na(re_df_2016$n)))

writexl::write_xlsx(re_df_2016, "tables/re_coefs_2016.xlsx")

re_df_2016_educ <- gen_re_df(lmer_2016_educ) %>% mutate(year = "2016")

summary(lmer_2016_educ)

re_df_2016_educ$id <- 1 : nrow(re_df_2016_educ)
n_gem <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(n = n()) %>%
  mutate(n = DescTools::RoundTo(n, 5))
re_df_2016_educ <- re_df_2016_educ %>%
  mutate(gem_2019 = as.numeric(as.character(gem_2019))) %>%
  left_join(n_gem)

assertthat::assert_that(all(!is.na(re_df_2016$n)))

writexl::write_xlsx(re_df_2016, "tables/re_coefs_2016_educ.xlsx")

## Predicted versus actual on the gemeente level at 2016
## Outcome
df_analysis_2016$pred_y_lpm <- predict(lm_2016, df_analysis_2016)

## Overall mean
overall_y_mean_2016 <- mean(df_analysis_2016$y)

## Expected use on gemeente level in 2016
gem_predict_2016 <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(pred_wmo_use_prob = mean(pred_y_lpm),
            y_mean = mean(y)) %>%
  mutate(delta_predict = y_mean - pred_wmo_use_prob,
         delta_mean = y_mean - overall_y_mean_2016)

writexl::write_xlsx(gem_predict_2016, "tables/wmo_2016_predict_v2.xlsx")

## Including Education
df_analysis_2016$pred_y_lpm_educ <- predict(lm_2016_educ, df_analysis_2016)

## Overall mean
overall_y_mean_2016 <- mean(df_analysis_2016$y)

## Expected use on gemeente level in 2016
gem_predict_2016_educ <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(pred_wmo_use_prob = mean(pred_y_lpm_educ),
            y_mean = mean(y)) %>%
  mutate(delta_predict = y_mean - pred_wmo_use_prob,
         delta_mean = y_mean - overall_y_mean_2016)

ggplot(gem_predict_2016_educ, aes(y = y_mean, x = pred_wmo_use_prob)) +
  geom_point() + geom_abline(slope = 1) + ylim(0.02, 0.15) + xlim(0.02, 0.15)

writexl::write_xlsx(gem_predict_2016_educ, "tables/wmo_2016_predict_educ.xlsx")
