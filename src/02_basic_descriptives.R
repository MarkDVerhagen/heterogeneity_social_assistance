### -- 01_merge_data.R
## Description: This script generates datasets containing merged data on
## Social Assistance use and demographics
## Input:
## - ["data/numeric_income/yyy/rin_num_income.rds"]
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

## -- Load libraries
library(tidyverse)

## -- Load functions
source("H:/wmo_heterogeneity/src_new/02_functions.r")

## -- Load data
full <- readRDS("H:/wmo_heterogeneity/data/edit/full_df_2016_2019.rds")
demog_2016 <- readRDS("H:/data/demog/2016/rin_demog.rds")
demog_2019 <- readRDS("H:/data/demog/2019/rin_demog.rds")
load("H:/wmo_heterogeneity/data/edit/wmo_demog_15_19.rda")
income_2016 <- readRDS("H:/data/numeric_income/2016/rin_num_income.rds")
income_2019 <- readRDS("H:/data/numeric_income/2019/rin_num_income.rds")

full_all_years <- full
full <- full_all_years %>%
  filter(year %in% c(2016, 2019))

yearly_wmo <- gen_desc_wmo_use(full_all_years %>% filter(leeftijd >= 18), group_vars = c("year"))
yearly_wmo_gem <- gen_desc_wmo_use(full %>% filter(leeftijd >= 18), group_vars = c("year", "gem_2019"))

make_output_ready <- function(table, start_col = 3) {
  table[, start_col:ncol(table)] <- DescTools::RoundTo(table[, start_col:ncol(table)], 5)
  table[, start_col:ncol(table)][table[, start_col:ncol(table)] < 10] <- NA
  return(table)
}

writexl::write_xlsx(make_output_ready(yearly_wmo), "H:/wmo_heterogeneity/tables/yearly_wmo.xlsx")
writexl::write_xlsx(make_output_ready(yearly_wmo_gem), "H:/wmo_heterogeneity/tables/yearly_wmo_gem.xlsx")

## Make melted versions
yearly_wmo_melt <- yearly_wmo %>%
  reshape2::melt(id.vars = c("year"))

yearly_wmo_gem_melt <- yearly_wmo_gem %>%
  reshape2::melt(id.vars = c("year", "gem_2019"))

## Delta's per gemeente
gem_deltas_wmo <- gen_mean_use_2016_2019(full, "wmo_gebruik") %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_huishoudelijkehulp")) %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_hulpmiddel_diensten")) %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_ondersteuning")) %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_verblijf_opvang"))

## [[ Merge in N and make output ready ]]

gem_deltas_wmo %>%
  writexl::write_xlsx("tables/gem_deltas_16_19.xlsx")


### -- Desriptive Tables

## Save Table indicating number of municipalities that submitted to the registry yearly
## including total coverage of the population

table_munic_submissions <-
  bind_rows(
    lapply(list(wmo_demog_2015, wmo_demog_2016, wmo_demog_2017, wmo_demog_2018, wmo_demog_2019),
           extract_gem_info))

table_munic_submissions$in_sample_n_gem <- c(NA, length(unique(full_all_years$gem[full_all_years$year == "2016"])),
                                             length(unique(full_all_years$gem[full_all_years$year == "2017"])),
                                             length(unique(full_all_years$gem[full_all_years$year == "2018"])),
                                             length(unique(full_all_years$gem[full_all_years$year == "2019"])))
table_munic_submissions$in_sample_n <- c(NA, nrow(full_all_years[full_all_years$year == "2016", ]),
                                         nrow(full_all_years[full_all_years$year == "2017", ]),
                                         nrow(full_all_years[full_all_years$year == "2018", ]),
                                         nrow((full_all_years[full_all_years$year == "2019", ])))
table_munic_submissions$in_sample_2019_gem <- c(NA, length(unique(full_all_years$gem_2019[full_all_years$year == "2016"])),
                                                length(unique(full_all_years$gem_2019[full_all_years$year == "2017"])),
                                                length(unique(full_all_years$gem_2019[full_all_years$year == "2018"])),
                                                length(unique(full_all_years$gem_2019[full_all_years$year == "2019"])))

writexl::write_xlsx(table_munic_submissions, "H:/wmo_heterogeneity/tables//table_munic_submissions_full.xlsx")

rm(wmo_demog_2015, wmo_demog_2016, wmo_demog_2017, wmo_demog_2018, wmo_demog_2019)

## Format data into dummy-based
pop_df <- bind_rows(
  demog_2016 %>% left_join(income_2016) %>% mutate(year = 2016),
  demog_2019 %>% left_join(income_2019) %>% mutate(year = 2019)
)
pop_df <- align_herkomst(pop_df)
pop_df$huishoudsamenstelling <- ifelse(pop_df$huishoudsamenstelling %in%  ## Merge Onbekend and Institutioneel
                                       c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                     as.character(pop_df$huishoudsamenstelling))

pop_df <- pop_df[!is.na(pop_df$lower_bound_num), ]

rm(demog_2016, demog_2019, income_2016, income_2019)

full_dummy <- make_dummy_set(full)

sample_desc <- make_desc_dummy(full_dummy %>% filter(leeftijd >= 18))

pop_dummy <- make_dummy_set(pop_df, pop = TRUE)

pop_desc <- make_desc_dummy(pop_dummy %>% filter(leeftijd >= 18))

sample_desc_df <- sample_desc %>% t() %>% as.data.frame()
sample_desc_df$var <- rownames(sample_desc_df)

pop_desc_df <- pop_desc %>% t() %>% as.data.frame()
pop_desc_df$var <- rownames(pop_desc_df)

## Fill in exta descriptives on the continuous variables
sd(full$leeftijd[(full$year == "2016") & (full$leeftijd >= 18)])
sd(full$leeftijd[(full$year == "2019") & (full$leeftijd >= 18)])
sd(pop_df$leeftijd[(full$year == "2016") & (pop_df$leeftijd >= 18)])
sd(pop_df$leeftijd[(full$year == "2019") & (pop_df$leeftijd >= 18)])
sd(full$lower_bound_num[(full$year == "2016") & (full$leeftijd >= 18)])
sd(full$lower_bound_num[(full$year == "2019") & (full$leeftijd >= 18)])
sd(pop_df$lower_bound_num[(pop_df$year == "2016") & (pop_df$leeftijd >= 18)])
sd(pop_df$lower_bound_num[(pop_df$year == "2019") & (pop_df$leeftijd >= 18)])

table(pop_df$herkomst[pop_df$year == "2016"]) / nrow(pop_df[pop_df$year == "2016",])
table(pop_df$herkomst[pop_df$year == "2019"]) / nrow(pop_df[pop_df$year == "2019",])

comb_desc <- sample_desc_df %>%
  left_join(pop_desc_df, by = "var") %>%
  dplyr::select(var, everything())
names(comb_desc) <- c("Var", "Sample_2016", "Sample_2019", "Pop_2016", "Pop_2019")

writexl::write_xlsx(comb_desc, "H:/wmo_heterogeneity/tables/table_desc_2016_2019.xlsx")

## Table: demog_by_wmo

desc_by_wmo <- make_desc_dummy(full_dummy %>% filter(leeftijd >= 18), group_vars = c("year", "wmo_gebruik_ja"))
desc_by_wmo_df <- as.data.frame(desc_by_wmo %>% t())
names(desc_by_wmo_df) <- c("2016_non_wmo", "2016_wmo", "2019_non_wmo", "2019_wmo")
desc_by_wmo_df$var <- rownames(desc_by_wmo_df)
writexl::write_xlsx(desc_by_wmo_df %>% dplyr::select(var, everything()),
                    "H:/wmo_heterogeneity/tables/table_desc_by_wmo_2016_2019.xlsx")
