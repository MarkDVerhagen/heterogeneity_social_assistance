recover_wmo_rin <- function(x) {
  ## Function to return those rinpersons who have used wmo.
  #' @param x Dataframe including both rinpersoon and wmo_gebruik.
  
  return(unique(x$rinpersoon[x$wmo_gebruik == "ja"]))
}


align_herkomst <- function(x) {
  ## Function to remap migrant background into three levels
  #' @param x Dataframe including a herkomst column.
  return(x %>%
           mutate(herkomst = case_when(
             herkomst == "Autochtoon" ~ "Nederlands",
             herkomst == "Nederland" ~ "Nederlands",
             herkomst == "Westers allochtoon" ~ "Westers",
             herkomst == "Overige westerse migratieachtergrond" ~ "Westers",
             herkomst == "Poolse migratieachtergrond of MOE-landers" ~ "Westers",
             TRUE ~ "Niet-Westers"
           ))
  )
}

merge_2019_gemeente <- function(df, kwb) {
  ## Function to merge the 2019 equivalent municipality codes
  ## onto earlier years
  #' @param df Wmo_demog datarfame
  #' @param kwb Municipality code information
  #' @param df WMO_demog including 2019 municipality
  
  ## Remove years indicators
  names(df) <- gsub("bc_\\d{4}", "bc", names(df))
  names(df) <- gsub("wc_\\d{4}", "wc", names(df))
  names(df) <- gsub("gem_\\d{4}", "gem", names(df))
  
  ## Merge in new codes
  df <- df %>%
    mutate(bc = as.numeric(as.character(bc))) %>%
    left_join(kwb %>%
                rename(bc = gwb_code_8) %>%
                dplyr::select(bc, gem_2019))
  
  ## Unit test that if GEM is missing, it is because BC is missing.
  assertthat::assert_that(all(is.na(df$gem_2019) == is.na(df$bc)))
  return(
    df
  )  
}