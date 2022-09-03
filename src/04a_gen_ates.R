### Script to generate basic average treatment effects using
## various methods
## Input
#' @input df_2016_2019_inc dataset including all relevant data for 2016 and 2019
## Output
#' @output models/ates_ex_gm_all_df Ates using various methods
###

## Load libraries
library(tidyverse)
library(grf)

## Load functions
source("src/04_functions.R")

## Load datasets
df_analysis <- readRDS("data/edit/df_analysis_2016_2019_inc.rds")

## Generate age squared and income non-linearity
df_analysis$leeftijd <- round(df_analysis$leeftijd / 2)*2
df_analysis$lower_bound_num <- ifelse(
  df_analysis$lower_bound_num > 0,
  round(df_analysis$lower_bound_num / 5) * 5,
  df_analysis$lower_bound_num)

## Encode
df_analysis_dummy <- df_analysis %>%
  mutate(geslacht = ifelse(geslacht == "vrouw", 1, 0)) %>%
  fastDummies::dummy_cols(
    select_columns = c("herkomst", "huishoudsamenstelling")) %>%
  rename(geslacht_vrouw = geslacht)

## Format column names to exclude whitespace
names(df_analysis_dummy) <- gsub(" ", "_", names(df_analysis_dummy))

df_ate <- df_analysis_dummy %>%
       rename(`herkomst_Niet_Westers` = `herkomst_Niet-Westers`) %>%
       select(leeftijd, lower_bound_num, geslacht_vrouw, starts_with("herkomst_"),
              starts_with("huishoudsamenstelling_"), y, treat, -herkomst_eerstegen)

rm(df_analysis, df_analysis_dummy)

## Setup balance formulas
## Omit outcomes and other irrelevant determinants
all_cols <- names(df_ate %>% dplyr::select(-y, -treat, -contains('gem_2019')))
## Setup balance formula (all explanatory variables)
bal_formula_standard <- paste0("treat ~ 1 +", paste0(all_cols, collapse = " + "))

## Make interactions between explanatory variables
int_cols <- all_cols[!grepl("leeftijd|lower_bound", all_cols)]
bal_formula_interacted <- paste0(
  bal_formula_standard, "+",
  paste0(
    c(paste0(int_cols[1], ":", int_cols[!grepl("geslacht", int_cols)]),
      paste0(int_cols[2], ":", int_cols[!grepl("geslacht|herkomst", int_cols)]),
      paste0(int_cols[3], ":", int_cols[!grepl("geslacht|herkomst", int_cols)]),
      paste0(int_cols[4], ":", int_cols[!grepl("geslacht|herkomst", int_cols)])),
    collapse = "+")
)

## Estimate overall ATE
## Simple descriptive difference
naive_estimate <- mean(df_ate$y[df_ate$treat == 1]) -
  mean(df_ate$y[df_ate$treat == 0])

ates_ex_gm <- generate_treatment_effect(
  df_ate, bal_fun = bal_formula_standard)
saveRDS(ates_ex_gm, "models/ates_ex_gm_all_df.rds")