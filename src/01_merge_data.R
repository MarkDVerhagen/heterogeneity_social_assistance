### 01_merge_data.R
## Description: This script generates datasets containing merged data on
## Social Assistance use and demographics
## Input:
#' @input ["./data/numeric_income/yyy/rin_num_income.rds"]
#' @input Information on continuous rather than categorical income
#' @input ["./data/edit/wmo_demog_15_19.rda"]
#' @input wmo_demog_2015 dataframe with demograpics and social assistance use in 2015
#' @input wmo_demog_2016 dataframe with demograpics and social assistance use in 2016
#' @input wmo_demog_2017 dataframe with demograpics and social assistance use in 2017
#' @input wmo_demog_2018 dataframe with demograpics and social assistance use in 2018
#' @input wmo_demog_2019 dataframe with demograpics and social assistance use in 2019
#' @input ["./data/edit/cw_bu_gem_2019.rda"]
#' @input kwb_2016: all municipalities in 2016 including the 2019 equivalent
#' @input kwb_2017: all municipalities in 2017 including the 2019 equivalent
#' @input kwb_2018: all municipalities in 2018 including the 2019 equivalent
## Output:
#' @output full_df_2016_2019.rds dataset including all relevant data for 2016 and 2019
###

## Load libraries
library(tidyverse)

## Load functions
source("./src/01_functions.R")

## Load wmo_demog data for 2015-2019
load("./data/edit/wmo_demog_15_19.rda")

## Load neighbourhood statistics
kwb_2016 <- readRDS("./data/raw/kwb-2016.rds")
kwb_2017 <- readRDS("./data/raw/kwb-2017.rds") %>%
  mutate(gm_code = as.numeric(substr(gwb_code_8, 1, 4)))
kwb_2018 <- readRDS("./data/raw/kwb-2018.rds") %>%
  mutate(gm_code = as.numeric(substr(gwb_code_8, 1, 4)))

## Load numeric income data
income_list <- lapply(list("2016", "2017", "2018", "2019"),
                      function(x) {readRDS(paste0("H:/data/numeric_income/", x,
                                                  "/rin_num_income.rds"))}
)

income_list <- lapply(income_list, function(x) {x %>% rename(rinpersoon = RINPERSOON) %>%
    mutate(rinpersoon = as.numeric(rinpersoon))})

## Load municipality crosswalk
load("./data/edit/cw_bu_gem_2019.rda")

## Unit tests
assertthat::assert_that(
  ## Ensure that gemeente is only ever missing when there is no information on bc
  all(wmo_demog_2016$bc_2016[is.na(wmo_demog_2016$gem_2016)] %in% c("--------", NA)) &
    all(wmo_demog_2017$bc_2017[is.na(wmo_demog_2017$gem_2017)] %in% c("--------", NA)) &
    all(wmo_demog_2018$bc_2017[is.na(wmo_demog_2018$gem_2018)] %in% c("--------", NA)) &
    all(wmo_demog_2019$bc_2017[is.na(wmo_demog_2019$gem_2019)] %in% c("--------", NA))  
)

## Merge in 2019 equivalent municipality and omit when missing (<0.1%)
wmo_demog_2016 <- wmo_demog_2016 %>% merge_2019_gemeente(kwb_2016) %>% filter(!is.na(gem_2019))
wmo_demog_2017 <- wmo_demog_2017 %>% merge_2019_gemeente(kwb_2017) %>% filter(!is.na(gem_2019))
wmo_demog_2018 <- wmo_demog_2018 %>% merge_2019_gemeente(kwb_2018) %>% filter(!is.na(gem_2019))
wmo_demog_2019 <- wmo_demog_2019 %>%
  rename(bc = bc_2019) %>%
  mutate(gem = gem_2019,
         inkomen_pers = NA,
         bc = as.numeric(bc))

## Unit tests
assertthat::assert_that(
  ## Assert that 2019 municipalities is never missing
  all(!is.na(wmo_demog_2019$gem_2019))
)
assertthat::assert_that(
  ## Assert that number of unique 2019 municipalities is the same for every year
  length(unique(wmo_demog_2016$gem_2019)) == length(unique(wmo_demog_2017$gem_2019)))
assertthat::assert_that(length(unique(wmo_demog_2016$gem_2019)) ==
                          length(unique(wmo_demog_2018$gem_2019)))
assertthat::assert_that(length(unique(wmo_demog_2016$gem_2019)) ==
                          length(unique(wmo_demog_2019$gem_2019)))

## Define list with all relevant years
all_wmo_list <- list(wmo_demog_2016, wmo_demog_2017, wmo_demog_2018, wmo_demog_2019)

## Make three-level migrant background variable
all_wmo_list <- lapply(all_wmo_list, align_herkomst)

## Unit test
## Municipalities who submitted in 2016
gem_wmo_2016 <- unique(all_wmo_list[[1]]$gem[all_wmo_list[[1]]$wmo_gebruik == "ja"])

gem_wmo_na_2016 <- wmo_demog_2016 %>%
  group_by(gem) %>%
  summarise(wmo_na = mean(is.na(wmo_gebruik)))

assertthat::assert_that( ## Assert that municipalities who did not submit are 100% non-NA
  all(gem_wmo_na_2016$gem[gem_wmo_na_2016$wmo_na == 0] %in% gem_wmo_2016))

## Omit municipalities that merged together where one of the merging municipalities did not submit
to_omit <- c(81, 962)  
gem_wmo_2016_cover_ex_merge <- gem_wmo_2016[!(gem_wmo_2016 %in% to_omit)]
gem_2019_wmo_2016_cover_ex_merge <- unique(wmo_demog_2016$gem_2019[wmo_demog_2016$gem %in% gem_wmo_2016_cover_ex_merge])

## The following gemeenten are merged into a gem_2019 of which some did not submit in 2019
not_submit <- c(707, 393, 140, 53)
not_submit_2019_gem <- unique(wmo_demog_2016$gem_2019[wmo_demog_2016$gem %in% not_submit])
gem_2019_2016_cover_final <- gem_2019_wmo_2016_cover_ex_merge[!(gem_2019_wmo_2016_cover_ex_merge %in% not_submit_2019_gem)]

all_wmo_list <- lapply(all_wmo_list, FUN = function(x) {x %>% filter(gem_2019 %in% gem_2019_2016_cover_final)})

## The following 13 gemeenten submitted in 2016 but did not submit in 2019
stopped_submitting <- all_wmo_list[[4]] %>%
  filter(is.na(wmo_gebruik)) %>%
  pull(gem) %>%
  unique

all_wmo_list <- lapply(all_wmo_list, FUN = function(x) {x %>% filter(!(gem_2019 %in% stopped_submitting))})

full <- bind_rows( ## Only use the years 2016 and 2019 for analysis
  all_wmo_list[[1]] %>% mutate(year = 2016),
  all_wmo_list[[2]] %>% mutate(year = 2017),
  all_wmo_list[[3]] %>% mutate(year = 2018),
  all_wmo_list[[4]] %>% mutate(year = 2019)
)

## Unit tests for 2016 and 2019
assertthat::assert_that( ## There are no NA's in 2016
  sum(is.na(full$wmo_gebruik[full$year == 2016])) == 0)
assertthat::assert_that( ## There are no NA's in 2019
  sum(is.na(full$wmo_gebruik[full$year == 2019])) == 0)

## Specify treatment and control years
full$treat <- ifelse(full$year == "2019", 1, 0)
full$y <- ifelse(full$wmo_gebruik == "ja", 1, 0)

## Merge in numeric income
income_2016_2019 <- bind_rows(
  income_list[[1]] %>% dplyr::select(rinpersoon, lower_bound_num) %>% mutate(year = 2016),
  income_list[[2]] %>% dplyr::select(rinpersoon, lower_bound_num) %>% mutate(year = 2017),
  income_list[[3]] %>% dplyr::select(rinpersoon, lower_bound_num) %>% mutate(year = 2018),
  income_list[[4]] %>% dplyr::select(rinpersoon, lower_bound_num) %>% mutate(year = 2019)
)

full <- full %>%
  left_join(income_2016_2019)

## Drop NA income
full <- full[!is.na(full$lower_bound_num), ]  ## Lose <0.01% due to different measurement dates

full$huishoudsamenstelling <- ifelse(full$huishoudsamenstelling %in%  ## Merge Onbekend and Institutioneel
                                       c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                     as.character(full$huishoudsamenstelling))

## Drop NA use (implies individual moved from municipality that did not submit to one that did around new years)
full <- full[!is.na(full$wmo_gebruik), ]

## Unit tests
assertthat::assert_that( ## Assert that pgb does not feature in use
  all(colMeans(full[(full$wmo_pgb_ind == 1) & (full$y == 0), ] %>% select(starts_with("wmo_"), -wmo_pgb_ind) == "nee") == 1))
assertthat::assert_that(
  all(colMeans(full[(full$wmo_pgb_ind == 1) & (full$y == 1), ] %>% select(starts_with("wmo_"), -wmo_pgb_ind, -wmo_gebruik) == "nee") != 1))

## Save resultant set
saveRDS(full, "./data/edit/full_df_2016_2019.rds")
