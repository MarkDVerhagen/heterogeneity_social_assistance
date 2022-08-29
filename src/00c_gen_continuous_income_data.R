### -- 00c_gen_continuous_income_data.R
## Description: This script generates datasets with continuous
## household income data
## Input:
#' @input ["data/numeric_income/yyyy/rin_num_income.csv"]
#' @input Information on continuous rather than categorical income for the years 2016-2019
## Output:
#' @output ["data/numeric_income/yyyy/rin_num_income.rds"]
#' @output Formatted information on continuous rather than categorical income for the years 2016-2019
###

## Load data
library(tidyverse)

## Load functions
source("src/00_functions.R")

## Load raw income data (from stapelingsbestand stata subset to .csv)
income_2015 <- read_csv("H:/data/numeric_income/2015/rin_num_income.csv") %>%
  remove_numerics_from_names() %>%
  rename(rinpersoon = RINPERSOON,
         percsm = PERCSM)
income_2016 <- read_csv("H:/data/numeric_income/2016/rin_num_income.csv") %>%
  remove_numerics_from_names()
income_2017 <- read_csv("H:/data/numeric_income/2017/rin_num_income.csv") %>%
  remove_numerics_from_names()
income_2018 <- read_csv("H:/data/numeric_income/2018/rin_num_income.csv") %>%
  remove_numerics_from_names()
income_2019 <- read_csv("H:/data/numeric_income/2019/rin_num_income.csv") %>%
  remove_numerics_from_names()

## Transform character income to numeric income
income_2015 <- gen_numeric_income(income_2015)
income_2016 <- gen_numeric_income(income_2016)
income_2017 <- gen_numeric_income(income_2017)
income_2018 <- gen_numeric_income(income_2018)
income_2019 <- gen_numeric_income(income_2019)

## Save data
saveRDS(income_2015, "H:/data/numeric_income/2015/rin_num_income.rds")
saveRDS(income_2016, "H:/data/numeric_income/2016/rin_num_income.rds")
saveRDS(income_2016, "H:/data/numeric_income/2016/rin_num_income.rds")
saveRDS(income_2017, "H:/data/numeric_income/2017/rin_num_income.rds")
saveRDS(income_2018, "H:/data/numeric_income/2018/rin_num_income.rds")
saveRDS(income_2019, "H:/data/numeric_income/2019/rin_num_income.rds")
