## Functions for scripts 03

## 03_model_descriptives.R
remove_numerics <- function(df) {
  ## Function to remove year indicators from column names
  #' @param df Dataframe
  #' @return Dataframe with year numerics removed from column names

  names(df) <- gsub("_\\d{4}", "", names(df))
  return(df)
}

gen_coef_df <- function(mod_obj) {
  ## Function to generate a dataframe from a model object
  #' @param mod_obj A model object
  #' @return A dataframe summarizing the model

  coef_df <- as.data.frame(summary(mod_obj)$coef)
  coef_df$var <- rownames(coef_df)
  coef_df$var_level <- ifelse(
    grepl("huishouds", coef_df$var), "Household",
      ifelse(grepl("income", coef_df$var), "Income",
              ifelse(grepl("herkomst", coef_df$var), "Migrant background",
                    ifelse(grepl("geslacht", coef_df$var), "Sex",
                            ifelse(grepl("leeftijd", coef_df$var), "Age",
                                  ifelse(grepl("hbopl", coef_df$var), "Education",
                                          "Other")))))
                              )
  coef_df$lower_ci <- coef_df$Estimate - 1.96 * coef_df$`Std. Error`
  coef_df$upper_ci <- coef_df$Estimate + 1.96 * coef_df$`Std. Error`
  rownames(coef_df) <- NULL
  return(coef_df)
}

gen_re_df <- function(mod_obj, slope = F, order = F) {
  ## Function to extract random elements of a lmer object
  #' @param mod_obj Lmer4 model object
  #' @param slope Random slopes included
  #' @param order Order by intercept
  #' @return Datarfame with random elements

  re_df <- data.frame(
    gem_2019 = rownames(coef(mod_obj)$gem_2019),
    coef_int = coef(mod_obj)$gem_2019[, 1],
    se_int = arm::se.ranef(mod_obj)$gem_2019[, 1]
  )

  re_df$ci_lower_int <- re_df$coef_int - 2 * re_df$se_int
  re_df$ci_upper_int <- re_df$coef_int + 2 * re_df$se_int

  re_df <- re_df %>%
    rowwise() %>%
    mutate(color_int = ifelse(between(0, ci_lower_int, ci_upper_int),
                              "Includes zero", "Excludes zero"))

  if (slope) {
    re_df$coef_slope <- coef(mod_obj)$gem_2019[, 2]
    re_df$se_slope <- arm::se.ranef(mod_obj)$gem_2019[, 2]

    re_df$ci_lower_slope <- re_df$coef_slope - 2 * re_df$se_slope
    re_df$ci_upper_slope <- re_df$coef_slope + 2 * re_df$se_slope

    re_df <- re_df %>%
      rowwise() %>%
      mutate(color_slope = ifelse(between(0, ci_lower_slope, ci_upper_slope),
                                  "Includes zero", "Excludes zero"))
  }

  if (order) {
    re_df <- re_df[order(re_df$coef_int), ]
    re_df$id <- 1 : nrow(re_df)  
  }

  return(re_df %>%
           ungroup())
}