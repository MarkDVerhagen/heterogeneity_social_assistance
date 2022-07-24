### -- 01_merge_data.R
## Description: This script generates datasets containing merged data on
## Social Assistance use and demographics
## Input:
## - ["H:/wmo_heterogeneity/data/numeric_income/yyy/rin_num_income.rds"]
## - Information on continuous rather than categorical income
## - ["H:/wmo_heterogeneity/data/edit/wmo_demog_15_19.rda"]
## - wmo_demog_2015 dataframe with demograpics and social assistance use in 2015
## - wmo_demog_2016 dataframe with demograpics and social assistance use in 2016
## - wmo_demog_2017 dataframe with demograpics and social assistance use in 2017
## - wmo_demog_2018 dataframe with demograpics and social assistance use in 2018
## - wmo_demog_2019 dataframe with demograpics and social assistance use in 2019
## - ["H:/wmo_heterogeneity/data/edit/cw_bu_gem_2019.rda"]
## - kwb_2016: all municipalities in 2016 including the 2019 equivalent
## - kwb_2017: all municipalities in 2017 including the 2019 equivalent
## - kwb_2018: all municipalities in 2018 including the 2019 equivalent
## Output:
## - ["H:/wmo_heterogeneity/data/edit/full_df_2016_2019.rds"]: dataset including all relevant data for 2016 and 2019
###

## -- Load libraries
library(tidyverse)

## -- Load functions
source("H:/wmo_heterogeneity/src_new/01_functions.R")

## -- Load wmo_demog data for 2015-2019
load("H:/wmo_heterogeneity/data/edit/wmo_demog_15_19.rda")

## -- Load numeric income
income_2015 <- readRDS("H:/data/numeric_income/2015/rin_num_income.rds")
income_2016 <- readRDS("H:/data/numeric_income/2016/rin_num_income.rds")

## -- Make analysis set
names(wmo_demog_2015) <- gsub("_\\d{4}", "", names(wmo_demog_2015))
names(wmo_demog_2016) <- gsub("_\\d{4}", "", names(wmo_demog_2016))

df_analysis <- rbind(
  wmo_demog_2015 %>% mutate(treat = 0),
  wmo_demog_2016 %>% mutate(treat = 1)
)

gem_2015 <- unique(wmo_demog_2015$gem[!is.na(wmo_demog_2015$wmo_gebruik)])
gem_2016 <- unique(wmo_demog_2016$gem[!is.na(wmo_demog_2016$wmo_gebruik)])

## -- Only include municipalities that submitted in both years
df_analysis <- df_analysis %>%
  filter(gem %in% gem_2015)
na_gem <- df_analysis$gem[is.na(df_analysis$wmo_gebruik)]
df_analysis <- df_analysis %>%
  filter(!(gem %in% na_gem))

## -- Encode variables
df_analysis$y <- ifelse(df_analysis$wmo_gebruik == "ja", 1, 0)
df_analysis$leeftijd2 <- df_analysis$leeftijd^2
df_analysis <- df_analysis %>%
  filter(leeftijd >= 18)

df_analysis$herkomst3 <- ifelse(df_analysis$herkomst == "Autochtoon", "Nederlands",
                                ifelse(df_analysis$herkomst == "Westers allochtoon", "Westers", "Niet-Westers"))
df_analysis$huishoudsamenstelling <- ifelse(df_analysis$huishoudsamenstelling %in%
                                              c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                            as.character(df_analysis$huishoudsamenstelling))

## -- Include numeric income
tot_income <- rbind(
  income_2015 %>%
    mutate(rinpersoon = as.numeric(rinpersoon),
           treat = 0) %>%
    dplyr::select(rinpersoon, lower_bound_num, treat),
  income_2016 %>%
    mutate(rinpersoon = as.numeric(rinpersoon),
           treat = 1) %>%
    dplyr::select(rinpersoon, lower_bound_num, treat)
  )

df_analysis_inc <- df_analysis %>%
  left_join(tot_income, by = c("rinpersoon", "treat"))

df_analysis_inc$lower_bound_num2 <- df_analysis_inc$lower_bound_num^2
df_analysis_inc$neg_inc <- ifelse(df_analysis_inc$lower_bound_num < 0, 1, 0)

rm(wmo_demog_2015, wmo_demog_2016, wmo_demog_2017, wmo_demog_2018, wmo_demog_2019)
rm(income_2015, income_2016)
df_analysis_inc$income_cat <- round(df_analysis_inc$lower_bound_num / 25)

lm_2015_2016_cat <- lm(y ~ 1 + treat + leeftijd + leeftijd2 + as.factor(herkomst3) + as.factor(geslacht) + as.factor(huishoudsamenstelling) + as.factor(income_cat), data = df_analysis_inc)
summary(lm_2015_2016_cat)
options(scipen = 3)

writexl::write_xlsx(
  rbind(
    broom::tidy(lm_2015_2016_cat) %>% as.data.frame,
    c("N", stats::nobs(lm_2015_2016_cat), NA, NA, NA),
    c("Degrees of Freedom", summary(lm_2015_2016_cat)$df[2], NA, NA, NA),
    c("R-squared", summary(lm_2015_2016_cat)$r.squared, NA, NA, NA)),
  "output/220711_Output_WMO3/r_regression_2015_2016.xlsx"
)
