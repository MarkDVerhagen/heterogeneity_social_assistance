### Functions for 02_basic_descriptives.R
remove_numerics <- function(df) {
  names(df) <- gsub("_\\d{4}", "", names(df))
  return(df)
}

gen_desc_wmo_use <- function(df, group_vars = c("year")) {
  ## Function to make summary table
  #' @param df Dataframe with use on the individual level
  #' @param group_vars Grouping variables
  #' @return Descriptive table
  
  return(df %>%
           group_by_at(
             vars(group_vars)
           ) %>%
           summarise(
             n = n(),
             n_wmo = sum(wmo_gebruik == "ja", na.rm = T),
             n_wmo_hh = sum(wmo_huishoudelijkehulp == "ja", na.rm = T),
             n_wmo_hm = sum(wmo_hulpmiddel_diensten == "ja", na.rm = T),
             n_wmo_on = sum(wmo_ondersteuning == "ja", na.rm = T),
             n_wmo_vb = sum(wmo_verblijf_opvang == "ja", na.rm = T)
           ))
}


gen_mean_use_2016_2019 <- function(df, mean_var = "y") {
  ## Function to specify mean use and delta
  #' @param df Dataframe at individual level with WMO use
  #' @return Average use in 2016 and 2019 including delta
  full[[mean_var]] <- ifelse(full[[mean_var]] == "ja", 1,
                             ifelse(full[[mean_var]] == "nee", 0, NA))
  
  summ_df <- df %>%
    group_by(year, gem_2019) %>%
    summarise_at(vars(mean_var), funs(mean)) %>%
    ungroup() %>%
    pivot_wider(id_cols = "gem_2019", names_from = "year", values_from = mean_var) %>%
    mutate(delta = `2019` - `2016`)
  names(summ_df)[2:4] <- paste0(mean_var, "_", names(summ_df)[2:4])
  return(summ_df)
}



extract_gem_info <- function(data) {
  ## Function to extract number of municipalities and individuals
  ## in the registry for WMO
  #' @param df Individual level dataframe
  #' @return Descriptives
  
  names(data) <- gsub("_\\d{4}", "", names(data))
  return(data %>%
           summarise(n = n(),
                     n_registry = sum(!is.na(wmo_gebruik)),
                     n_gem = length(unique(gem)),
                     n_gem_registry = length(unique(gem[!is.na(wmo_gebruik)]))))
}


make_dummy_set <- function(df, pop = FALSE) {
  ## Function to make dummy set
  #' @param df Dataframe
  #' @return Dataframe with dummy-encoded descriptives
  
  if (pop) {
    return(df %>%
             dplyr::select(leeftijd, geslacht, herkomst, huishoudsamenstelling,
                           hbopl, year, lower_bound_num) %>%
             fastDummies::dummy_columns(select_columns =
                                          c("geslacht", "herkomst", "huishoudsamenstelling",
                                            "hbopl")) %>%
             dplyr::select(-geslacht, -herkomst, -hbopl,
                           -huishoudsamenstelling))  
  } else {
    return(df %>%
             dplyr::select(leeftijd, geslacht, herkomst, huishoudsamenstelling,
                           wmo_gebruik, year, lower_bound_num, hbopl) %>%
             fastDummies::dummy_columns(select_columns =
                                          c("geslacht", "herkomst", "huishoudsamenstelling",
                                            "hbopl", "wmo_gebruik")) %>%
             dplyr::select(-wmo_gebruik_nee, -geslacht, -herkomst,
                           -huishoudsamenstelling, -hbopl, -wmo_gebruik_, -wmo_gebruik))  
  }
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


make_desc_dummy <- function(data, group_vars = "year") {
  ## Make means from dummies including N
  #' @param data Dataframe with underlying individual data
  #' @return Summary of data including N
  
  desc <- data %>%
    group_by_at(vars(group_vars)) %>%
    summarise_all(.funs = mean)
  n <- data %>%
    group_by_at(vars(group_vars)) %>%
    summarise(n = n())
  return(desc %>%
           left_join(n, by = group_vars))
}