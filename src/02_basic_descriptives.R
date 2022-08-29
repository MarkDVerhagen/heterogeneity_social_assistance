### 02_basic_descriptives.R
## Description: This script generates basic descriptive
## statistics of the analysis set
## Input:
#' @input full analysis dataframe including only municipalities that submitted
#' @input demog_2016 entire population in 2016
#' @input demog_2019 entire population in 2019
#' @input ["data/edit/wmo_demog_15_19.rda"]
#' @input wmo_demog_2015 dataframe with demograpics and social assistance use in 2015
#' @input wmo_demog_2016 dataframe with demograpics and social assistance use in 2016
#' @input wmo_demog_2017 dataframe with demograpics and social assistance use in 2017
#' @input wmo_demog_2018 dataframe with demograpics and social assistance use in 2018
#' @input wmo_demog_2019 dataframe with demograpics and social assistance use in 2019
#' @input income_2016 income data on entire population in 2016
#' @input income_2019 income data on entire population in 2016

## Output:
#' @output tables/gem_deltas_16_19.xlsx differences in social assistance use
#' @output tables//table_munic_submissions_full.xlsx number of submitting municipalities
#' @output tables/table_desc_2016_2019.xlsx descriptives 2016 and 2019
#' @output tables/table_desc_by_wmo_2016_2019.xlsx descriptives by social assistance use 2016 and 2019
#' @output tables/table_univ_wmo_2016_2019.xlsx 2016 and 2019 use by univariate groups
###

## Load libraries
library(tidyverse)

## Load functions
source("./src_new/02_functions.r")

## Load analysis data
full <- readRDS("./data/edit/full_df_2016_2019.rds")

## Load population data
demog_2016 <- readRDS("H:/data/demog/2016/rin_demog.rds")
demog_2019 <- readRDS("H:/data/demog/2019/rin_demog.rds")
load("./data/edit/wmo_demog_15_19.rda")
income_2016 <- readRDS("H:/data/numeric_income/2016/rin_num_income.rds")
income_2019 <- readRDS("H:/data/numeric_income/2019/rin_num_income.rds")

## Subset to relevant timeframe (2016 and 2019)
full_all_years <- full
full <- full_all_years %>%
  filter(year %in% c(2016, 2019))

## Make descriptive tables with yearly use (overall and municipality level)
yearly_wmo <- gen_desc_wmo_use(full_all_years %>% filter(leeftijd >= 18), group_vars = c("year"))
yearly_wmo_gem <- gen_desc_wmo_use(full %>% filter(leeftijd >= 18), group_vars = c("year", "gem_2019"))

## Write tables
writexl::write_xlsx(make_output_ready(yearly_wmo), "./tables/yearly_wmo.xlsx")
writexl::write_xlsx(make_output_ready(yearly_wmo_gem), "./tables/yearly_wmo_gem.xlsx")

## Make melted versions
yearly_wmo_melt <- yearly_wmo %>%
  reshape2::melt(id.vars = c("year"))

yearly_wmo_gem_melt <- yearly_wmo_gem %>%
  reshape2::melt(id.vars = c("year", "gem_2019"))

## Table with differences in use per municipality
gem_deltas_wmo <- gen_mean_use_2016_2019(full, "wmo_gebruik") %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_huishoudelijkehulp")) %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_hulpmiddel_diensten")) %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_ondersteuning")) %>%
  left_join(gen_mean_use_2016_2019(full, "wmo_verblijf_opvang"))

## Write table
gem_deltas_wmo %>%
  writexl::write_xlsx("./tables/gem_deltas_16_19.xlsx")

## Table with number of submitting municipalities and n over years
table_munic_submissions <-
  bind_rows(
    lapply(list(wmo_demog_2015, wmo_demog_2016, wmo_demog_2017,
                wmo_demog_2018, wmo_demog_2019),
           extract_gem_info))

table_munic_submissions$in_sample_n_gem <-
  c(NA, length(unique(full_all_years$gem[full_all_years$year == "2016"])),
    length(unique(full_all_years$gem[full_all_years$year == "2017"])),
    length(unique(full_all_years$gem[full_all_years$year == "2018"])),
    length(unique(full_all_years$gem[full_all_years$year == "2019"]))
    )
table_munic_submissions$in_sample_n <-
c(NA, nrow(full_all_years[full_all_years$year == "2016", ]),
  nrow(full_all_years[full_all_years$year == "2017", ]),
  nrow(full_all_years[full_all_years$year == "2018", ]),
  nrow((full_all_years[full_all_years$year == "2019", ]))
  )
table_munic_submissions$in_sample_2019_gem <- c(
  NA, length(unique(full_all_years$gem_2019[full_all_years$year == "2016"])),
  length(unique(full_all_years$gem_2019[full_all_years$year == "2017"])),
  length(unique(full_all_years$gem_2019[full_all_years$year == "2018"])),
  length(unique(full_all_years$gem_2019[full_all_years$year == "2019"]))
  )

writexl::write_xlsx(table_munic_submissions, "./tables//table_munic_submissions_full.xlsx")

## clean space
rm(wmo_demog_2015, wmo_demog_2016, wmo_demog_2017, wmo_demog_2018, wmo_demog_2019)

## Format population data into dummy-based
pop_df <- bind_rows(
  demog_2016 %>% left_join(income_2016) %>% mutate(year = 2016),
  demog_2019 %>% left_join(income_2019) %>% mutate(year = 2019)
)
pop_df <- align_herkomst(pop_df)
pop_df$huishoudsamenstelling <- ifelse(
  pop_df$huishoudsamenstelling %in%  ## Merge Onbekend and Institutioneel
  c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
  as.character(pop_df$huishoudsamenstelling))

## Remove NA income
pop_df <- pop_df[!is.na(pop_df$lower_bound_num), ]

## clean space
rm(demog_2016, demog_2019, income_2016, income_2019)

## Make dummy set based on analysis set
full_dummy <- make_dummy_set(full)
sample_desc <- make_desc_dummy(full_dummy %>% filter(leeftijd >= 18))
pop_dummy <- make_dummy_set(pop_df, pop = TRUE)

## Make descriptive tables
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

## write table
writexl::write_xlsx(comb_desc, "./tables/table_desc_2016_2019.xlsx")

## Table of descriptives by social assistance use
desc_by_wmo <- make_desc_dummy(
  full_dummy %>% filter(leeftijd >= 18), group_vars = c("year", "wmo_gebruik_ja")
  )
desc_by_wmo_df <- as.data.frame(desc_by_wmo %>% t())
names(desc_by_wmo_df) <- c("2016_non_wmo", "2016_wmo", "2019_non_wmo", "2019_wmo")
desc_by_wmo_df$var <- rownames(desc_by_wmo_df)

## Write table
writexl::write_xlsx(desc_by_wmo_df %>% dplyr::select(var, everything()),
                    "./tables/table_desc_by_wmo_2016_2019.xlsx")

## Table: univariate WMO use 2016 and 2019
full$age_cat <- round(full$leeftijd / 10)
full$income_cat <- as.character(round(full$lower_bound_num / 25))

univ_desc <- rbind(
  gen_group_value(full %>% filter(leeftijd >= 18), "geslacht"),
  gen_group_value(full %>% filter(leeftijd >= 18), "herkomst"),
  gen_group_value(full %>% filter(leeftijd >= 18), "huishoudsamenstelling"),
  gen_group_value(full %>% filter(leeftijd >= 18), "age_cat"),
  gen_group_value(full %>% filter(leeftijd >= 18), "income_cat")
)

## Write table
writexl::write_xlsx(univ_desc, "./tables/table_univ_wmo_2016_2019.xlsx")