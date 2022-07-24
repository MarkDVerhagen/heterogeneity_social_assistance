remove_numerics <- function(df) {
  names(df) <- gsub("_\\d{4}", "", names(df))
  return(df)
}

gen_coef_df <- function(mod_obj) {
  coef_df <- as.data.frame(summary(mod_obj)$coef)
  coef_df$var <- rownames(coef_df)
  coef_df$var_level <- ifelse(grepl("huishouds", coef_df$var), "Household",
                              ifelse(grepl("income", coef_df$var), "Income",
                                     ifelse(grepl("herkomst", coef_df$var), "Migrant background",
                                            ifelse(grepl("geslacht", coef_df$var), "Sex",
                                                   ifelse(grepl("leeftijd", coef_df$var), "Age",
                                                          ifelse(grepl("hbopl", coef_df$var), "Education",
                                                                 "Other"))))))
  coef_df$lower_ci <- coef_df$Estimate - 1.96 * coef_df$`Std. Error`
  coef_df$upper_ci <- coef_df$Estimate + 1.96 * coef_df$`Std. Error`
  rownames(coef_df) <- NULL
  return(coef_df)
}

format_labels <- function(coef_df) {
  coef_df$var <- gsub("inkomen_klasse|huishoudsamenstelling|geslacht|herkomst", "", coef_df$var)
  coef_df$var <- case_when(
    coef_df$var == "Niet-Westers" ~ "Non-Western",
    coef_df$var == "Westers" ~ "Western",
    coef_df$var == "Eenpersoonshuishouden" ~ "Single person",
    coef_df$var == "Institutioneel huishouden" ~ "Institution",
    coef_df$var == "Onbekend" ~ "Unknown",
    coef_df$var == "Overig meerpersoonshuishouden" ~ "Other multi-person",
    coef_df$var == "Paar zonder kinderen" ~ "Couple without kids",
    coef_df$var == "Paar met kinderen" ~ "Couple with kids",
    coef_df$var == "leeftijd" ~ "Age",
    coef_df$var == "leeftijd2" ~ "Age squared",
    coef_df$var == "vrouw" ~ "Female",
    coef_df$var == "(Intercept)" ~ "Intercept",
    TRUE ~ coef_df$var
  )
  coef_df$var <- gsub("hh inkomen |hh inkomen ", "", coef_df$var)
  coef_df$var <- gsub("tot ", "Below ", coef_df$var)
  coef_df$var <- gsub("tussen ", "Between ", coef_df$var)
  coef_df$var <- gsub(" %sm", "% of socl min.", coef_df$var)
  coef_df$var <- gsub(" en ", "-", coef_df$var)
  return(coef_df)
}

gen_coef_plot <- function(coef_df) {
  ggplot(coef_df, aes(y = Estimate, x = var, group = model, color = model)) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.6)) +
    geom_point(position = position_dodge(width=0.6)) + theme_bw() + geom_hline(yintercept = 0, linetype = "dashed") + xlab("Variable") +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(rows = vars(var_level), scales = "free") + coord_flip()
}

gen_age_effect <- function(coef_df, age = seq(18, 90, 1)) {
  age1 <- coef_df$Estimate[coef_df$var == "Age"]
  age2 <- coef_df$Estimate[coef_df$var == "Age squared"]
  return(age * age1 + age^2 * age2)
  
}

gen_re_df <- function(mod_obj, slope = F, order = F) {
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

write_lm_mod <- function(table, mod_obj, file="output/220711_Output_WMO3/group_multivariate_full.xlsx") {
  writexl::write_xlsx(
    rbind(
      table,
      c("N", stats::nobs(mod_obj), NA, NA, NA),
      c("Degrees of Freedom", summary(mod_obj)$df[2], NA, NA, NA),
      c("R-squared", summary(mod_obj)$r.squared, NA, NA, NA)),
    file
  )
}
