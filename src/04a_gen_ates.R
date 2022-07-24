library(tidyverse)
library(grf)

setwd("H:/wmo_heterogeneity/")
source("src_new/04_functions.R")

## Load datasets
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
  fastDummies::dummy_cols(select_columns = c("herkomst", "huishoudsamenstelling")) %>%
  rename(geslacht_vrouw = geslacht)
names(df_analysis_dummy) <- gsub(" ", "_", names(df_analysis_dummy))

df_grf <- df_analysis_dummy %>%
       rename(`herkomst_Niet_Westers` = `herkomst_Niet-Westers`) %>%
       select(leeftijd, lower_bound_num, geslacht_vrouw, starts_with("herkomst_"),
              starts_with("huishoudsamenstelling_"), y, treat, -herkomst_eerstegen)

# df_grf_gm <- df_analysis_dummy %>%
#   rename(`herkomst_Niet_Westers` = `herkomst_Niet-Westers`) %>%
#   select(leeftijd, lower_bound_num, geslacht_vrouw, starts_with("herkomst_"),
#          starts_with("huishoudsamenstelling_"), gem_2019, y, treat)

rm(df_analysis, df_analysis_dummy)

## Setup balance formulas
all_cols <- names(df_grf %>% dplyr::select(-y, -treat, -contains('gem_2019')))
bal_formula_standard <- paste0("treat ~ 1 +", paste0(all_cols, collapse = " + "))

int_cols <- all_cols[!grepl("leeftijd|lower_bound", all_cols)]
bal_formula_interacted <- paste0(bal_formula_standard, "+",
                                 paste0(
                                   c(paste0(int_cols[1], ":", int_cols[!grepl("geslacht", int_cols)]),
                                     paste0(int_cols[2], ":", int_cols[!grepl("geslacht|herkomst", int_cols)]),
                                     paste0(int_cols[3], ":", int_cols[!grepl("geslacht|herkomst", int_cols)]),
                                     paste0(int_cols[4], ":", int_cols[!grepl("geslacht|herkomst", int_cols)])),
                                   collapse = "+")
)


## Estimate overall ATE excluding municipality codes
naive_estimate <- mean(df_grf$y[df_grf$treat == 1]) - mean(df_grf$y[df_grf$treat == 0])
set.seed(1704)

ates_ex_gm <- generate_treatment_effect(df_grf, bal_fun = bal_formula_standard)

saveRDS(ates_ex_gm, "models/ates_ex_gm_all_df.rds")

ates <- readRDS("H:/wmo_heterogeneity/data/final/ate_non_gm_tau_forest_033_1250ms_500t_18plus.rds") %>%
  t() %>%
  as.data.frame() %>%
  rename(Estimate = estimate,
         `Std. Error` = std.err)
bind_rows(
  bind_rows(ates_ex_gm),
  ates %>% mutate(gem_2019 = "Full", Freq = round(nrow(df) / 3))
) %>%
  writexl::write_xlsx("output/220711_Output_WMO3/ates.xlsx")
