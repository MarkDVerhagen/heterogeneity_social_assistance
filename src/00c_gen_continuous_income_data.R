### -- 00c_gen_continuous_income_data.R
## Description: This script generates datasets with continuous
## household income data
## Input:
## - ["data/numeric_income/yyyy/rin_num_income.csv"]
## - Information on continuous rather than categorical income for the years 2016-2019
## Output:
## - ["data/numeric_income/yyyy/rin_num_income.rds"]
## - Formatted information on continuous rather than categorical income for the years 2016-2019
###

library(tidyverse)

source("src/functions.R")

## Load raw income data (from stapelings bestand stata subset to .csv)
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

gen_numeric_income <- function(df) {
  ## Function to make a numeric version of the percsm variable
  #' @param df Dataframe including percsm in original form.

  df$lower_bound <- gsub("Van |% tot.*", "", df$percsm)
  df$lower_bound_num <- as.numeric(df$lower_bound)
  
  df$lower_bound_num <- case_when(
    df$percsm == "Minder dan 2%" ~ 1,
    df$percsm == "999% of meer" ~ 1000,
    df$lower_bound_num > 0 ~ df$lower_bound_num,
    df$percsm == c("Institutioneel huishouden") ~ -1,
    df$percsm == "999% of meer" ~ 1000,
    TRUE ~ -2
  )
  return(df)
}

income_2015 <- gen_numeric_income(income_2015)
income_2016 <- gen_numeric_income(income_2016)
income_2017 <- gen_numeric_income(income_2017)
income_2018 <- gen_numeric_income(income_2018)
income_2019 <- gen_numeric_income(income_2019)

saveRDS(income_2015, "H:/data/numeric_income/2015/rin_num_income.rds")
saveRDS(income_2016, "H:/data/numeric_income/2016/rin_num_income.rds")
saveRDS(income_2016, "H:/data/numeric_income/2016/rin_num_income.rds")
saveRDS(income_2017, "H:/data/numeric_income/2017/rin_num_income.rds")
saveRDS(income_2018, "H:/data/numeric_income/2018/rin_num_income.rds")
saveRDS(income_2019, "H:/data/numeric_income/2019/rin_num_income.rds")
