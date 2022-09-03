### -- 03_model_descriptives.R
## Description: Script to generate insights into the determinants of social assistance.
## Includes demographic determinants of social assistance use (in 2016),
## random intercepts, robustness when including education and OOS prediction
## of municipality level use
## Input:
#' @input df_2016_2019_inc dataset including all relevant data for 2016 and 2019
#' @input H:/data/education/2016/rin_educ Dataframe w education for the 2016 population
#' @input H:/data/education/2019/rin_educ Dataframe w education for the 2016 population
## Output:
#' @output lm_treat model object from LPM with social assistance use as outcome (2016 & 2019)
#' @output lm_2016 model object from LPM with social assistance use as outcome (2016)
#' @output lm_2016_educ model object from LPM inc education with social assistance use as outcome (2016)
#' @output lmer_2016 model object from LPM inc random intercepts with social assistance use as outcome (2016)
#' @output lmer_2016_educ model object from LPM inc random intercepts and educ with social assistance use as outcome (2016)
#' @output tables/coefs_2016 coefficients of lm_2016
#' @output tables/coefs_2016_educ coefficients of lm_2016_educ
#' @output tables/coefs_lmer_2016 coefficients of lmer_2016
#' @output tables/re_coefs_2016 random intercepts of lmer_2016
#' @output tables/coefs_lmer_2016_educ coefficients of lmer_2016_educ
#' @output tables/re_coefs_2016_educ random intercepts of lmer_2016_educ
#' @output tables/wmo_2016_predict OOS-predictions of municipality use (2016)

## Load libraries
library(lme4)
library(tidyverse)
library(patchwork)

## Load functions
source("src/03_functions.R")

## Load full dataset on 2016 and 2019
df_analysis <- readRDS("./data/edit/full_df_2016_2019_inc.rds") %>%
  filter(year %in% c(2016, 2019))

## Include education for robustness
educ_2016 <- readRDS("H:/data/education/2016/rin_educ.rds")
educ_2019 <- readRDS("H:/data/education/2019/rin_educ.rds")

## Merge in education
tot_educ <- rbind(
  educ_2016 %>% mutate(treat = 0) %>% remove_numerics(),
  educ_2019 %>% mutate(treat = 1) %>% remove_numerics()
)

tot_educ <- tot_educ %>%
  mutate(rinpersoon = as.numeric(RINPERSOON)) %>%
  dplyr::select(-RINPERSOON)

df_analysis <- df_analysis %>%
  left_join(tot_educ)

## Generate squared version of age and non-linear income
df_analysis$leeftijd2 <- df_analysis$leeftijd^2
df_analysis$income_cat <- round(df_analysis$lower_bound_num / 25)

## Make separate sets for 2016 and 2019
df_analysis_2016 <- df_analysis[df_analysis$year == 2016, ]
df_analysis_2019 <- df_analysis[df_analysis$year == 2019, ]

## Model results for full set
## Standard LPM on full data including treatment coefficient
lm_treat <- lm(y ~ 1 + treat + leeftijd + leeftijd2 + as.factor(herkomst) +
                 as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                 as.factor(income_cat), data = df_analysis)
## Save model
saveRDS(lm_treat, "./models/lm_treat.rds")

## Model results for 2016 data
## Standard LPM
lm_2016 <- lm(y ~ 1 + leeftijd + leeftijd2 + as.factor(herkomst) +
                  as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                  as.factor(income_cat), data = df_analysis_2016, y = F, model = F)
lm_2016$effects <- c()
## Save model
saveRDS(lm_2016, 'models/lm_2016.rds')
lm_2016_coefs <- gen_coef_df(lm_2016)
writexl::write_xlsx(lm_2016_coefs,
                    "./tables/coefs_2016.xlsx")

## Standard LPM including education
lm_2016_educ <- lm(y ~ 1 + leeftijd + leeftijd2 + as.factor(herkomst) +
                     as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                     as.factor(income_cat) + as.factor(hbopl),
                   data = df_analysis_2016, y = F,
                   model = F)
lm_2016_educ$effects <- c()
## Save model
saveRDS(lm_2016_educ, 'models/lm_2016_educ.rds')
lm_2016_coefs_educ <- gen_coef_df(lm_2016_educ)
writexl::write_xlsx(lm_2016_coefs_educ,
                    "./tables/coefs_2016_educ.xlsx")

## Random intercepts
lmer_2016 <- lmer(y ~ (1 | gem_2019) + leeftijd + leeftijd2 + as.factor(herkomst) +
                    as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                    as.factor(income_cat),
                  data = df_analysis_2016)
## Save model
saveRDS(lmer_2016, "models/lmer_2016.rds")
lmer_2016_coefs <- gen_coef_df(lmer_2016)
writexl::write_xlsx(lmer_2016_coefs,
                    "./tables/coefs_lmer_2016.xlsx")

## Random intercepts and education
lmer_2016_educ <- lmer(y ~ (1 | gem_2019) + leeftijd + leeftijd2 + as.factor(herkomst) +
                         as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                         as.factor(income_cat),
                       data = df_analysis_2016)
## Save model
saveRDS(lmer_2016_educ, "models/lmer_2016_educ.rds")
lmer_2016_coefs_educ <- gen_coef_df(lmer_2016_educ)
writexl::write_xlsx(lmer_2016_coefs_educ,
                    "./tables/coefs_lmer_2016_educ.xlsx")

## Extract random effects
re_df_2016 <- gen_re_df(lmer_2016) %>% mutate(year = "2016")

re_df_2016$id <- 1 : nrow(re_df_2016)
n_gem <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(n = n()) %>%
  mutate(n = DescTools::RoundTo(n, 5))
re_df_2016 <- re_df_2016 %>%
  mutate(gem_2019 = as.numeric(as.character(gem_2019))) %>%
  left_join(n_gem)

## Unit test
assertthat::assert_that(all(!is.na(re_df_2016$n)))
writexl::write_xlsx# Write output table of random effects per municipality
d(re_df_2016, "tables/re_coefs_2016.xlsx")

## Including education
re_df_2016_educ <- gen_re_df(lmer_2016_educ) %>% mutate(year = "2016")
re_df_2016_educ$id <- 1 : nrow(re_df_2016_educ)
n_gem <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(n = n()) %>%
  mutate(n = DescTools::RoundTo(n, 5))
re_df_2016_educ <- re_df_2016_educ %>%
  mutate(gem_2019 = as.numeric(as.character(gem_2019))) %>%
  left_join(n_gem)

## Unit test
assertthat::assert_that(all(!is.na(re_df_2016$n)))
# Write output table of random effects per municipality
writexl::write_xlsx(re_df_2016, "tables/re_coefs_2016_educ.xlsx")

## Make OOS predictions of municipality level use based on demographics

df_preds <- data.frame()  ## Generate empty dataframe to store predictions
gems <- unique(df_analysis_2016$gem_2019)  ## Make list of all municipalities

for (i in gems) {
  ## Make OOS-predictions by training to dataset excluding municipality

  ## Train set
  df_train <- df_analysis_2016 %>%
    filter(!(gem_2019 %in% i)) %>%
    sample_frac(0.25)  ## For computational convenience

  ## Test set
  df_test <- df_analysis_2016 %>%
    filter(gem_2019 %in% i)

  lm_train <- lm(y ~ 1 + leeftijd + leeftijd2 + as.factor(herkomst) +
                  as.factor(geslacht) + as.factor(huishoudsamenstelling) +
                  as.factor(income_cat), data = df_analysis_2016, y = F, model = F)

  df_preds <- rbind(df_preds,
                    data.frame(y_pred_oos_lpm = predict(lm_train, df_test),
                               rinpersoon = df_test$rinpersoon))
}

## Merge predictions into analysis set
df_analysis_2016_inc_preds <- df_analysis_2016 %>%
  left_join(df_preds)

## Expected use on municipality level in 2016
gem_predict_2016 <- df_analysis_2016 %>%
  group_by(gem_2019) %>%
  summarise(predicted = mean(y_pred_oos_lpm),
            actual = mean(y),
            n = n())

## Write output table with municipality level predictions
writexl::write_xlsx(gem_predict_2016,
                    "tables/wmo_2016_predict.xlsx")