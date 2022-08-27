### 00_data.R
## Description: This script generates datasets containing merged data on
## Social Assistance use and demographics
## Input:
## - ["data/wmo/yyyy/rin_wmo.rds"]
## - Information on social assistance use for the years 2015-2019
## - ["data/demog/yyyy/rin_demog.rds"]
## - Information on demographics for the years 2015-2019
## Output:
## - ["data/edit/wmo_demog_15_19.rda"]
## - wmo_demog_2015 dataframe with demograpics and social assistance use in 2015
## - wmo_demog_2016 dataframe with demograpics and social assistance use in 2016
## - wmo_demog_2017 dataframe with demograpics and social assistance use in 2017
## - wmo_demog_2018 dataframe with demograpics and social assistance use in 2018
## - wmo_demog_2019 dataframe with demograpics and social assistance use in 2019
## - kwb-2016 dataframe with information on all municipalities
###

## Load libraries
library(tidyverse)
library(assertthat)

## Load functions
source("./src/00_functions.R")

## Read wmo_demog files for 2016-2019
wmo_demog_2016 <- gen_wmo_demog(2016)
wmo_demog_2017 <- gen_wmo_demog(2017)
wmo_demog_2018 <- gen_wmo_demog(2018)
wmo_demog_2019 <- gen_wmo_demog(2019)

## Read wmo_demog file for 2015
wmo_demog_2015 <- read_rds("data/raw/demog_wmo_2015.rds") %>%
  mutate(gem_2015 = as.numeric(gem_2015)) %>%
  mutate(wmo_pgb_ind = NA,
         rinpersoon = as.numeric(rinpersoon)) %>%
  dplyr::select(gsub("2016", "2015", names(wmo_demog_2016)))

## Save all demography files of population in 2015-2019 incl WMO
save(wmo_demog_2015, wmo_demog_2016, wmo_demog_2017, 
     wmo_demog_2018, wmo_demog_2019,
     file = "data/edit/wmo_demog_15_19.rda")

## Read in regional statistics
kwb_2016 <- readxl::read_xls("data/raw/kwb-2016.xls") %>%
  select(gwb_code_8, gwb_code, gm_naam) %>%
  mutate(gm_code = as.numeric(substr(gwb_code_8, 1, 4)),
         gwb_code_8 = as.numeric(gwb_code_8)) %>%
  filter(gm_code != 0)

saveRDS(kwb_2016, "data/edit/kwb-2016.rds")

# Checks ------------------------------------------------------------------

## Checks general gem_codes
gem_2015 <- unique(wmo_demog_2015$gem_2015)
gem_2016 <- unique(wmo_demog_2016$gem_2016)
gem_2017 <- unique(wmo_demog_2017$gem_2017)
gem_2018 <- unique(wmo_demog_2018$gem_2018)
gem_2019 <- unique(wmo_demog_2019$gem_2019)

## Ensure that most municipality codes feature across the time window
assert_that(mean(mean(gem_2016 %in% gem_2015), mean(gem_2016 %in% gem_2017), mean(gem_2016 %in% gem_2018),
                 mean(gem_2016 %in% gem_2019)) > 0.95)
assert_that(mean(mean(gem_2017 %in% gem_2015), mean(gem_2017 %in% gem_2016), mean(gem_2017 %in% gem_2018),
                 mean(gem_2017 %in% gem_2019)) > 0.95)
assert_that(mean(mean(gem_2018 %in% gem_2015), mean(gem_2018 %in% gem_2016), mean(gem_2018 %in% gem_2017),
                 mean(gem_2018 %in% gem_2019)) > 0.95)
assert_that(mean(mean(gem_2019 %in% gem_2015), mean(gem_2019 %in% gem_2016), mean(gem_2019 %in% gem_2017),
                 mean(gem_2019 %in% gem_2018)) > 0.95)

## Checks WMO gem_codes
gem_2015_wmo <- unique(wmo_demog_2015$gem_2015[wmo_demog_2015$wmo_gebruik == "ja"])
gem_2016_wmo <- unique(wmo_demog_2016$gem_2016[wmo_demog_2016$wmo_gebruik == "ja"])
gem_2017_wmo <- unique(wmo_demog_2017$gem_2017[wmo_demog_2017$wmo_gebruik == "ja"])
gem_2018_wmo <- unique(wmo_demog_2018$gem_2018[wmo_demog_2018$wmo_gebruik == "ja"])
gem_2019_wmo <- unique(wmo_demog_2019$gem_2019[wmo_demog_2019$wmo_gebruik == "ja"])

check_gem <- gem_2019_wmo

dim(wmo_demog_2015 %>%
      filter(wmo_gebruik == "ja") %>%
      filter(gem_2015 %in% check_gem))
dim(wmo_demog_2016 %>%
      filter(wmo_gebruik == "ja") %>%
      filter(gem_2016 %in% check_gem))
dim(wmo_demog_2017 %>%
      filter(wmo_gebruik == "ja") %>%
      filter(gem_2017 %in% check_gem))
dim(wmo_demog_2018 %>%
      filter(wmo_gebruik == "ja") %>%
      filter(gem_2018 %in% check_gem))
dim(wmo_demog_2019 %>%
      filter(wmo_gebruik == "ja") %>%
      filter(gem_2019 %in% check_gem))