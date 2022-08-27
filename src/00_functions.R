## Functions for scripts 00a/00b/00c

## 00a_gen_wmo_demog_data.R

gen_wmo_demog <- function(year, wmo_path = "H:/data/wmo/",
                          demog_path = "H:/data/demog/",
                          wmo_file = "/rin_wmo.rds",
                          demog_file = "/rin_demog.rds") {
  ## Function to merge wmo use and demography based on a year input
  #' @param year Year to generate the merged set for.
  #' @param wmo_path Path to wmo data.
  #' @param file File name at path.
  #' @return Merged set including both demographics and outcome data.

  wmo_file_path <- paste0(wmo_path, year, wmo_file)
  demog_file_path <- paste0(demog_path, year, demog_file)
  
  wmo_df <- read_rds(wmo_file_path) %>%
    dplyr::select(-contains("_b"), -contains("kosten"), -contains("kwal"))
  
  demog <- read_rds(demog_file_path)
  demog[, paste0("gem_", year)] <- as.numeric(demog[, paste0("gem_", year)])

  assertthat::assert_that(all(wmo_df$rinpersoon %in% demog$rinpersoon))
  wmo_demog <- demog %>%
    left_join(wmo_df)  
}

## 00b_gen_municipality_crosswalk.R

gm_2016_2017 <- function(df, var) {
  ## Manual encoding of mergers of municipalities from 2016 to 2017
  #' @param df Data with municipalities as rows
  #' @param var Variable reflecting municipality code
  #' @return Post-merger municipality coding

  df[[var]] <- case_when(
    df[[var]] %in% c(844, 846, 860) ~ 1948,
    TRUE ~ as.numeric(df[[var]])
  )
  return(df)
}

gm_2017_2018 <- function(df, var, litter) {
  ## Manual encoding of mergers of municipalities from 2017 to 2018
  #' @param df Data with municipalities as rows
  #' @param var Variable reflecting municipality code
  #' @param litter Hand-coded table for more complex mergers
  #' @return Post-merger municipality coding

  df$gwb_code_8 <- as.numeric(df$gwb_code_8)
  litter$gwb_code_8 <- as.numeric(litter$gwb_code_8)
  
  df[[var]] <- case_when(
    df$gwb_code_8 %in%
      litter$gwb_code_8[litter$gm_2018 == 80] ~ 80, ## Littenseradeel
    df[[var]] == 81 ~ 80, ## Leeuwarderadeel
    df[[var]] %in% c(18, 1987, 40) ~ 1952,
    df$gwb_code_8 %in%
      litter$gwb_code_8[litter$gm_2018 == 1900] ~ 1900, ## Littenseradeel
    df[[var]] %in% c(70, 63, 1908) ~ 1949,
    df$gwb_code_8 %in%
      litter$gwb_code_8[litter$gm_2018 == 1949] ~ 1949, ## Littenseradeel
    df[[var]] %in% c(7, 48) ~ 1950,
    df[[var]] == 196 ~ 299,
    TRUE ~ as.numeric(df[[var]])
  )

  ## Omit the split-up municipality
  df <- df[df[[var]] != 140, ]

  return(df)
}

gm_2018_2019 <- function(df, var, winsum) {
  ## Manual encoding of mergers of municipalities from 2018 to 2019
  #' @param df Data with municipalities as rows
  #' @param var Variable reflecting municipality code
  #' @param winsum Hand-coded table for more complex mergers
  #' @return Post-merger municipality coding

  df$gwb_code_8 <- as.numeric(df$gwb_code_8)
  winsum$gwb_code_8 <- as.numeric(winsum$gwb_code_8)
  
  df[[var]] <- case_when(
    df[[var]] %in% c(738, 870, 874) ~ 1959,
    df[[var]] %in% c(951, 881, 962) ~ 1954,
    df[[var]] %in% c(17, 9) ~ 14,
    df[[var]] == 393 ~ 394,
    df$gwb_code_8 %in%
      winsum$gwb_code_8[is.na(winsum$gm_2019)] ~ 1966, ## Part of Winsum
    df[[var]] %in% c(5, 1663, 1651) ~ 1966,
    df[[var]] %in% c(585, 611, 588, 584, 617) ~ 1963,
    df[[var]] %in% c(689, 1927) ~ 1978,
    df[[var]] %in% c(58, 1722, 79) ~ 1970,
    df[[var]] %in% c(575, 576) ~ 575,
    df[[var]] %in% c(545, 707, 620) ~ 1961,
    df[[var]] %in% c(236, 733, 304) ~ 1960,
    df$gwb_code_8 %in%
      winsum$gwb_code_8[!is.na(winsum$gm_2019)] ~ 1969, ## Part of Winsum
    df[[var]] %in% c(15, 22, 25, 56) ~ 1969,
    TRUE ~ as.numeric(df[[var]])
  )

  ## Omit the split-up municipality
  df <- df[df[[var]] != 53, ]
  return(df)
}

## 00c_gen_continuous_income_data.R

remove_numerics_from_names <- function(df) {
    ## Function to remove year numerics from variable names
    #' @param df Dataframe
    #' @return Dataframe with 4-digit numerics removed from all names

  names(df) <- gsub("_\\d{4}", "", names(df))
  return(df)
}

gen_numeric_income <- function(df) {
  ## Function to make a numeric version of the percsm variable
  #' @param df Dataframe including percsm in original form.
  #' @return Dataframe with numeric version of the char. income variable

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