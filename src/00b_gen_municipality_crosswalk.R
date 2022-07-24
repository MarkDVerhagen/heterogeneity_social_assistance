### -- 00c_gen_municipality_crosswalk.R
## Description: This script generates a crosswalk across years to address
## mergers of municipalities across time
## Input:
## - ["data/raw/kwb-yyyy.rds"]
## - Spatial information provided by Statistics Netherlands for the years 2016-2019
## Output:
## - ["data/edit/cw_bu_gem_2019.rda"]
## - kwb_2016: all municipalities in 2016 including the 2019 equivalent
## - kwb_2017: all municipalities in 2017 including the 2019 equivalent
## - kwb_2018: all municipalities in 2018 including the 2019 equivalent
###

library(tidyverse)

kwb_2016 <- readRDS("data/edit/kwb-2016.rds")
kwb_2017 <- readRDS("data/edit/kwb-2017.rds") %>%
  mutate(gm_code = as.numeric(substr(gwb_code_8, 1, 4)))
kwb_2018 <- readRDS("data/edit/kwb-2018.rds") %>%
  mutate(gm_code = as.numeric(substr(gwb_code_8, 1, 4)))

## Herindelingen 2016
# Schijndel (844) + Sint-Oedenrode (846) + Veghel (860) >> Meierijstad (1948)

## Only use buurten and gemeenten
kwb_2016 <- kwb_2016 %>%
  filter(grepl("GM|BU", gwb_code))
kwb_2017 <- kwb_2017 %>%
  filter(grepl("GM|BU", gwb_code))
kwb_2018 <- kwb_2018 %>%
  filter(grepl("GM|BU", gwb_code))

kwb_2016$gem_2017 <- kwb_2016$gm_code

gm_2016_2017 <- function(df, var) {
  
  df[[var]] <- case_when(
    df[[var]] %in% c(844, 846, 860) ~ 1948,
    TRUE ~ as.numeric(df[[var]])
  )
  
  return(df)    
}

gm_2017_2018 <- function(df, var, litter) {
  df$gwb_code_8 <- as.numeric(df$gwb_code_8)
  litter$gwb_code_8 <- as.numeric(litter$gwb_code_8)
  
  df[[var]] <- case_when(
    df$gwb_code_8 %in% litter$gwb_code_8[litter$gm_2018 == 80] ~ 80, ## Deel van Littenseradeel
    df[[var]] == 81 ~ 80, ## Leeuwarderadeel
    df[[var]] %in% c(18, 1987, 40) ~ 1952,
    df$gwb_code_8 %in% litter$gwb_code_8[litter$gm_2018 == 1900] ~ 1900, ## Deel van Littenseradeel
    df[[var]] %in% c(70, 63, 1908) ~ 1949,
    df$gwb_code_8 %in% litter$gwb_code_8[litter$gm_2018 == 1949] ~ 1949, ## Deel van Littenseradeel
    df[[var]] %in% c(7, 48) ~ 1950,
    df[[var]] == 196 ~ 299,
    TRUE ~ as.numeric(df[[var]])
  )
  
  ## Omit the split up gemeente
  df <- df[df[[var]] != 140, ]
  
  return(df)    
}

gm_2018_2019 <- function(df, var, winsum) {
  df$gwb_code_8 <- as.numeric(df$gwb_code_8)
  winsum$gwb_code_8 <- as.numeric(winsum$gwb_code_8)
  
  df[[var]] <- case_when(
    df[[var]] %in% c(738, 870, 874) ~ 1959,
    df[[var]] %in% c(951, 881, 962) ~ 1954,
    df[[var]] %in% c(17, 9) ~ 14,
    df[[var]] == 393 ~ 394,
    df$gwb_code_8 %in% winsum$gwb_code_8[is.na(winsum$gm_2019)] ~ 1966, ## Deel van Winsum
    df[[var]] %in% c(5, 1663, 1651) ~ 1966,
    df[[var]] %in% c(585, 611, 588, 584, 617) ~ 1963,
    df[[var]] %in% c(689, 1927) ~ 1978,
    df[[var]] %in% c(58, 1722, 79) ~ 1970,
    df[[var]] %in% c(575, 576) ~ 575,
    df[[var]] %in% c(545, 707, 620) ~ 1961,
    df[[var]] %in% c(236, 733, 304) ~ 1960,
    df$gwb_code_8 %in% winsum$gwb_code_8[!is.na(winsum$gm_2019)] ~ 1969, ## Deel van Winsum
    df[[var]] %in% c(15, 22, 25, 56) ~ 1969,
    TRUE ~ as.numeric(df[[var]])
  )

  ## Omit the split up gemeente
  df <- df[df[[var]] != 53, ]
  return(df)    
}

kwb_2016 <- gm_2016_2017(kwb_2016, "gem_2017")
assertthat::assert_that(length(unique(kwb_2016$gem_2017)) == 388)

## Herindelingen 2017
# Leeuwarden + Leeuwarderadeel + deel van Littenseradeel >> Leeuwarden
# Hoogezand-Sappemeer + Menterwolde + Slochteren >> Midden-Groningen
# Sudwest-Fryslan + deel van Littenseradeel >> Sudwest-Fryslan
# Franekeradeel + Het Bildt + Menaldumadeel + deel van Littenseradeel >> Waadhoeke
# Bellingwedde + Vlagtwedde >> Westerwolde
# RIjnwaarden + Zevenaar >> Zevenaar

litter <- readxl::read_xlsx("H:/data/gemeente_crosswalk/litter_herindeling_2017.xlsx") %>%
  filter(!is.na(gm_2018)) %>%
  mutate(gm_2018 = as.numeric(gm_2018),
         gwb_code_8 = as.numeric(gwb_code_8))

kwb_2016$gem_2018 <- kwb_2016$gem_2017
kwb_2017$gem_2018 <- kwb_2017$gm_code

kwb_2016 <- gm_2017_2018(kwb_2016, "gem_2018", litter)
kwb_2017 <- gm_2017_2018(kwb_2017, "gem_2018", litter)

assertthat::assert_that(length(unique(kwb_2016$gem_2018)) == 380)
assertthat::assert_that(length(unique(kwb_2017$gem_2018)) == 380)

## Herindelingen 2018
# Aalburg + Werkendam + Woudrichem >> Altena
# Nuth + Onderbanken + Schinnen >> Beekdaelen
# Groningen + Haren + Ten Boer >> Groningen
# Haarlemmerliede + Spaarnwoude + Haarlemmermeer >> Haarlemmermeer
# Bedum + De Marne + Eemsmond + Winsum (ex. Ezinge, Feerwerd, Garnwerd) >> Het Hogeland
# Binnenmaas + Cromstrijen + Korendijk + Oud-Beijerland + Strijen >> Hoeksche Waard
# Giessenlanden + Molenwaard >> Molenlanden
# Dongeradeel + Ferwerderadeel + Kollumerland en Nieuwkruisland >> Noardeast-Frsylan
# Noordwijk + Noordwijkerhout >> Noordwijk
# Leerdam + Zederik + Vianen >> Vijfheerenlanden
# Geldermalsen + Lingewaal + Neerijnen >> West Betuwe
# Grootegast + Leek + Marum + Zuidhorn + Winsum (Ezinge, Feerwerd, Garnwerd) >> Westerkwartier

winsum <- readxl::read_xlsx("H:/data/gemeente_crosswalk/winsum_herinderling_2018.xlsx") %>%
  mutate(gwb_code_8 = as.numeric(gwb_code_8)) %>%
  filter(grepl("BU", gwb_code_10))

kwb_2016$gem_2019 <- kwb_2016$gem_2018
kwb_2017$gem_2019 <- kwb_2017$gem_2018
kwb_2018$gem_2019 <- kwb_2018$gm_code

kwb_2016 <- gm_2018_2019(kwb_2016, "gem_2019", winsum)
kwb_2017 <- gm_2018_2019(kwb_2017, "gem_2019", winsum)
kwb_2018 <- gm_2018_2019(kwb_2018, "gem_2019", winsum)

assertthat::assert_that(length(unique(kwb_2016$gem_2019)) == 355)
assertthat::assert_that(length(unique(kwb_2017$gem_2019)) == 355)
assertthat::assert_that(length(unique(kwb_2018$gem_2019)) == 355)

kwb_2019 <- readxl::read_xls("data/raw/Copy of kwb-2019.xls")
gm_2019 <- as.numeric(kwb_2019$gwb_code_8[grepl("GM", kwb_2019$gwb_code_10)])

assertthat::assert_that(length(gm_2019[!(gm_2019 %in% kwb_2016$gem_2019)]) == 0)
assertthat::assert_that(length(gm_2019[!(gm_2019 %in% kwb_2017$gem_2019)]) == 0)
assertthat::assert_that(length(gm_2019[!(gm_2019 %in% kwb_2018$gem_2019)]) == 0)

kwb_2016 <- kwb_2016 %>%
  filter(grepl("BU", gwb_code)) %>%
  mutate(gwb_code_8 = as.numeric(as.character(gwb_code_8)))
kwb_2017 <- kwb_2017 %>%
  filter(grepl("BU", gwb_code)) %>%
  mutate(gwb_code_8 = as.numeric(as.character(gwb_code_8)))
kwb_2018 <- kwb_2018 %>%
  filter(grepl("BU", gwb_code)) %>%
  mutate(gwb_code_8 = as.numeric(as.character(gwb_code_8)))

save(kwb_2016, kwb_2017, kwb_2018, file = "data/edit/cw_bu_gem_2019.rda")

          