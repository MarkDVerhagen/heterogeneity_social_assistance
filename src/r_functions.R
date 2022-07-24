remove_numerics <- function(df) {
  names(df) <- gsub("_\\d{4}", "", names(df))
  return(df)
}


gen_samples_educ <- function(df) {
  ##
  #' @param df Dataframe with individual level data pooled across control, treat.
  #' @return List of samples for X, Y, W.
  
  X_sample <- df %>%
    select(gem_2019, huishoudsamenstelling, leeftijd,
           geslacht, lower_bound_num, herkomst, treat, hbopl)
  
  X_sample_dummies <- X_sample %>%
    dplyr::select(leeftijd, geslacht, herkomst, huishoudsamenstelling,
                  lower_bound_num, gem_2019, hbopl) %>%
    fastDummies::dummy_columns(select_columns =
                                 c("geslacht", "herkomst", "huishoudsamenstelling", "hbopl"),
                               remove_selected_columns = T)
  
  if (nrow(X_sample) > 2500000) {
    X_sample_dummies_comb <- rbind(
      X_sample_dummies[1 : 2500000, ] %>%
        fastDummies::dummy_cols(select_columns = c("gem_2019"),
                                remove_selected_columns = T),
      X_sample_dummies[2500001 : nrow(X_sample_dummies), ] %>%
        fastDummies::dummy_cols(select_columns = c("gem_2019"),
                                remove_selected_columns = T)
    )
  } else {
    X_sample_dummies_comb <- X_sample_dummies %>%
      fastDummies::dummy_cols(select_columns = c("gem_2019"),
                              remove_selected_columns = T)
  }
  
  X_sample_dummies_comb$geslacht_man <- NULL
  ## Sample analysis excluding gem
  W_sample <- X_sample$treat
  X_sample_matrix_ex_gm <- as.matrix(X_sample_dummies_comb %>%
                                       dplyr::select(-contains("gem_2019")))
  X_sample_matrix_inc_gm <- as.matrix(X_sample_dummies_comb)
  Y_sample <- df$y
  return(list(X_sample_matrix_ex_gm, X_sample_matrix_inc_gm, W_sample, Y_sample))
}

estimate_CT <- function(data_train, data_test, seed=1704, minsize = 2500) {
  ## Function to estimate a CausalTree
  #' @param data_train training data
  #' @param data_test testing data
  #' @param seed seed
  #' @return estimated CausalTree
  
  t1 <- Sys.time()
  set.seed(seed)
  honestTree <- honest.causalTree(y ~ .,
                                  data = data_train,
                                  treatment = data_train$treat,
                                  est_data = data_test,
                                  est_treatment = data_test$treat,
                                  split.Rule = "CT", split.Honest = T,
                                  HonestSampleSize = nrow(data_test), cv.option = "fit",
                                  cv.Honest = F, minsize = minsize)
  t2 <- Sys.time()
  print(t2 - t1)
  return(honestTree)
}

format_names <- function(df) {
  names(df) <- tolower(names(df))
  names(df) <- gsub("-", "_", names(df))
  return(df)
}

generate_result_df <- function(rules_df) {
  ## Function to parse a set or RPart rules into a dataframe
  #' @param rules Set of RPart rules
  #' @return Dataframe version of RPart rules
  
  result_df <- data.frame(node = 1:nrow(rules_df))
  
  for (row in c(1:nrow(rules_df))) {
    str <- tolower(paste(rules_df[row, ], collapse = " "))
    
    y_temp <- gsub(" when.*", "", str)
    
    str <- gsub(y_temp, "", str)
    
    extract_temp <- function(str) {
      ##
      #' @param str String to extract variable from
      
      var_temp <- gsub(" & .*", "", str)
      str <- gsub(paste0(var_temp, " & "), "", str)
      return(list(var_temp, str))
    }
    
    var_list <- c()
    
    while (grepl("&", str)) {
      extract <- extract_temp(str)
      var_list <- c(var_list, extract[[1]])
      str <- extract[[2]]
    }
    var_list <- c(var_list, gsub("  ", "", str))
    
    ## Assign to df
    result_df$y[row] <- as.numeric(y_temp)
    
    result_df$income_sm[row] <- assign_val(var_list, var = "income_sm")
    
    result_df$leeftijd[row] <- assign_val(var_list, var = "leeftijd")
    result_df$h_nederlands[row] <- assign_val(var_list, var = "h_nederlands")
    result_df$h_westers[row] <- assign_val(var_list, var = "h_westers")
    result_df$h_niet_westers[row] <- assign_val(var_list, var = "h_niet-westers")
    
    result_df$opl_unknown[row] <- assign_val(var_list, var = "opl_---")
    result_df$opl_100[row] <- assign_val(var_list, var = "opl_100")
    result_df$opl_200[row] <- assign_val(var_list, var = "opl_200")
    result_df$opl_300[row] <- assign_val(var_list, var = "opl_300")
    
    result_df$hh_eenpersoonshuishouden[row] <- assign_val(var_list, var = "hh_eenpersoonshuishouden")
    result_df$`hh_paar zonder kinderen`[row] <- assign_val(var_list, var = "hh_paar zonder kinderen")
    result_df$`hh_paar met kinderen`[row] <- assign_val(var_list, var = "hh_paar met kinderen")
    result_df$hh_eenouderhuishouden[row] <- assign_val(var_list, var = "hh_eenouderhuishouden")
    result_df$`hh_inst_onbekend`[row] <- assign_val(var_list, var = "hh_inst_onbekend")
    result_df$`hh_overig meerpersoonshuishouden`[row] <- assign_val(var_list, var = "hh_overig meerpersoonshuishouden")
    
    result_df$geslacht[row] <- assign_val(var_list, var = "geslacht")
  }
  
  for (col in names(result_df)[grepl("^h|geslacht|^opl_", names(result_df))]) {
    result_df[[col]] <- format_binary(result_df[[col]])
  }
  
  result_df$leeftijd <- format_age(result_df$leeftijd)
  result_df$income_sm <- format_income(result_df$income_sm)
  result_df <- add_min_max_age(result_df)
  result_df <- add_min_max_income(result_df)
  
  return(result_df)
}

assign_val <- function(var_list, var = "income_sm") {
  ##
  #' @param var_list String with parsed variable conditions
  #' @param var Variable of interest
  return_var <- var_list[grepl(var, var_list)]
  return(ifelse(is.null(return_var), NA, return_var))
  
}

format_age <- function(x) {
  #' @param x Column with unformatted age rules
  #' result_df$income_sm <- gsub(" to ", "-", result_df$income_sm)
  x <- gsub("when", "", x)
  x <- gsub(" <", "0-", x)
  x <- gsub("to", "-", x)
  x <- gsub(".* is ", "", x)
  x <- gsub("leeftijd is ", "", x)
  x <- ifelse(grepl(">=", x), paste0(x, "-999"), x)
  x <- gsub("[[:alpha:]]", "", x)
  x <- gsub(">=", "", x)
  x <- gsub("\\s+", "", x)
  return(x)
}

add_min_max_age <- function(df) {
  #' @param df Dataframe with "leeftijd" column
  #' @return Dataframe including parsed minimum and maximum leeftijd
  
  df$leeftijd_min <- as.numeric(gsub("-.*", "", df$leeftijd))
  df$leeftijd_min <- ifelse(is.na(df$leeftijd_min), 0,
                            df$leeftijd_min)
  df$leeftijd_max <- as.numeric(gsub(".*-", "", df$leeftijd))
  df$leeftijd_max <- ifelse(is.na(df$leeftijd_max), 999,
                            df$leeftijd_max)
  return(df)
}

add_min_max_income <- function(df) {
  #' @param df Dataframe with "income_sm" column
  #' @return Dataframe including parsed minimum and maximum income_sm
  
  df$income_sm_min <- gsub("to.*", "", df$income_sm)
  df$income_sm_min <- as.numeric(ifelse(df$income_sm_min == "min", -99, df$income_sm_min))
  df$income_sm_min <- round(ifelse(is.na(df$income_sm_min), -99, df$income_sm_min), 1)
  
  df$income_sm_max <- as.numeric(gsub(".*to", "", df$income_sm))
  df$income_sm_max <- round(ifelse(is.na(df$income_sm_max), 9999, df$income_sm_max), 1)
  return(df)
}

format_income <- function(x) {
  #' @param x Column with unformatted income rules
  #' result_df$income_sm <- gsub(" to ", "-", result_df$income_sm)
  x <- gsub("when", "", x)
  x <- gsub(" <", "min to", x)
  x <- gsub("- ", "-", x)
  x <- gsub(".* is", "", x)
  x <- gsub("income_sm", "", x)
  x <- gsub("\\s+", "", x)
  x <- gsub("_", "", x)
  x <- ifelse(grepl(">=", x), paste0(x, "to9999"), x)
  x <- gsub(">=", "", x)
  
  return(x)
}

format_binary <- function(x) {
  ## Function to format binary
  #' @param x Column with unformatted binary rules
  x <- ifelse(grepl(">=", x), 1, x)
  x <- ifelse(grepl("<", x), 0, x)
  return(as.numeric(x))
}

mean_diff <- function(check_df) {
  ## Function to calculate the ATE in a group
  #' @param check_df A dataframe with an outcome y and treatment indicator treat
  print(mean(check_df$y))
  print(mean(check_df$y[check_df$treat == 1]) - mean(check_df$y[check_df$treat == 0]))
}

gen_ate <- function(sample_df, sample_df2, result_df) {
  ## Function to add an ATE based on the estimated rules and a sample set
  #' @param sample_df Dataset to generate ATEs for
  #' @param result_df Dataset with rules
  #' @return result_df with inclusion of ate and n columns
  
  ids_list <- c()
  ids_list_2 <- c()
  nodes_list <- c()
  sample_df <- format_names(sample_df)
  result_df$ate <- result_df$n <- result_df$y_2016 <- result_df$y_2019 <- 0
  result_df$n_2016 <- result_df$n_2019 <- 0
  
  for (row in 1:nrow(result_df)) {
    check <- sample_df %>%
      filter(leeftijd >= result_df$leeftijd_min[row],
             leeftijd < result_df$leeftijd_max[row],
             income_sm >= result_df$income_sm_min[row],
             income_sm < result_df$income_sm_max[row])
    
    binary_cols <- names(result_df)[grepl("^h|geslacht", names(result_df))]
    for (col in binary_cols) {
      check <- align_binary(check, result_df, row=row, 
                            var=col)
    }
    ids_list <- c(ids_list, check$id)
    nodes_list <- c(nodes_list, rep(row, length(check$id)))
    
    result_df$ate[row] <- mean_diff(check)
    result_df$n[row] <- nrow(check)
    result_df$y_2016[row] <- mean(check$y[check$treat == 0])
    result_df$y_2019[row] <- mean(check$y[check$treat == 1])
    result_df$n_2016[row] <- nrow(check[check$treat == 0,])
    result_df$n_2019[row] <- nrow(check[check$treat == 1,])
  }
  
  if (!is.null(sample_df2)) {
    for (row in 1:nrow(result_df)) {
      check <- sample_df2 %>%
        filter(leeftijd >= result_df$leeftijd_min[row],
               leeftijd < result_df$leeftijd_max[row],
               income_sm >= result_df$income_sm_min[row],
               income_sm < result_df$income_sm_max[row])
      
      binary_cols <- names(result_df)[grepl("^h|geslacht", names(result_df))]
      for (col in binary_cols) {
        check <- align_binary(check, result_df, row=row, 
                              var=col)
      }
      ids_list_2 <- c(ids_list_2, check$id)
      nodes_list <- c(nodes_list, rep(row, length(check$id)))
      
      result_df$ate_2[row] <- mean_diff(check)
      result_df$n_2[row] <- nrow(check)
      result_df$y_2016_2[row] <- mean(check$y[check$treat == 0])
      result_df$y_2019_2[row] <- mean(check$y[check$treat == 1])
      result_df$n_2016_2[row] <- nrow(check[check$treat == 0,])
      result_df$n_2019_2[row] <- nrow(check[check$treat == 1,])
    }
    assertthat::assert_that(!any(duplicated(ids_list_2)))
  }
  
  assertthat::assert_that(!any(duplicated(ids_list)))
  dup_ids <- ids_list[duplicated(ids_list)]
  
  return(result_df)
}

gen_ate_y_plot <- function(result_df_ate, hline = 0.008) {
  ## Function to make a plot per node
  #' @param result_df_ate Set of data with row for each node, y, ate, and n
  #' @param hline Horizontal line yintercept to show ATE
  #' @return Plot incidating y versus ate sized by n
  
  if ("group" %in% names(result_df_ate)) {
    result_df_ate <- result_df_ate %>%
      rename(node = group) %>%
      rename(y = ate_sample)
  }
  
  return(result_df_ate[, c("y", "ate", "n")] %>%
           mutate(node = 1 : nrow(result_df_ate)) %>%
           reshape2::melt(id.vars = c("node", "n")) %>%
           ggplot(aes(y = value, x = node, color = variable, size = n / 100000)) +
           geom_point() + geom_hline(yintercept = hline, linetype = "dashed") +
           theme_bw())
}


align_binary <- function(df, rules_df, row=1, var = "H_Nederlands") {
  ## Function to align binary rule with datafame
  #' @param df Dataframe
  #' @param rules_df Rules_dataframe
  #' @param row Node row
  #' @param var Variable
  #' @retun Filtered dataframe based on variable and rule
  assertthat::assert_that(!is.null(df[[var]]))
  assertthat::assert_that(!is.null(rules_df[[var]]))
  if (!is.na(rules_df[row, var])) {
    df <- df[df[[var]] == rules_df[row, var], ]
  }
  return(df)
}


prune_ct <- function(ct_object, complexity) {
  ## Function to prune ct_object
  #' @param ct_object CausalTree
  #' @param complexity Complexity level
  
  opcp <- ct_object$cptable[, 1][complexity]
  op_ct_object <- prune(ct_object, opcp)
  return(op_ct_object)
}

output_ready <- function(df) {
  ## Function to make the result_df output-ready
  return(df %>%
           rename(ate_sample = y,
                  group = node) %>%
           dplyr::select(-income_sm)
  )
  
}

generate_output_ct <- function(op_ct_object, df_sample, df_sample2 = NULL) {
  ## Function to generate ct output
  #' @param op_ct_object CausalTree
  #' @param df_sample Data to generate sample for
  
  result_df <- generate_result_df(
    rpart.rules(op_ct_object, roundint = FALSE) %>%
      as.data.frame()
  )
  
  result_df_ate <- gen_ate(result_df = result_df, sample_df = df_sample, sample_df2 = df_sample2)
  
  return(output_ready(result_df_ate))
}

add_group_effects <- function(df) {
  df_groups <- df %>%
    group_by(treat, group) %>%
    summarise(mean_y = mean(y))
  
  df_groups_wide <- df_groups %>%
    pivot_wider(id_cols = "group", names_from = "treat", values_from = "mean_y")
  
  names(df_groups_wide) <- c("Group", "Prevalence_2016", "Prevalence_2019")
  df_groups_wide$effect <- df_groups_wide$Prevalence_2019 - df_groups_wide$Prevalence_2016
  ggplot(df_groups_wide, aes(y = effect, x = Prevalence_2016)) + geom_point()
  
  df_inc_effect <- df %>%
    left_join(
      df_groups_wide %>%
        dplyr::select(Group, effect) %>%
        rename(group = Group)
    )
  return(df_inc_effect)
}

estimate_effects <- function(df) {
  
  model_obj <- lm(effect ~ 1 + .,  data = df %>%
                    dplyr::select(-match_id, -gem_2019, -group, -y, -treat, -rinpersoon))
  return(model_obj)
}

make_age_effect <- function(model_obj) {
  coef_df <- as.data.frame(summary(model_obj)$coefficients)
  age_lin <- coef_df$Estimate[rownames(coef_df) == "leeftijd"]
  age_sq <- coef_df$Estimate[rownames(coef_df) == "leeftijd2"]
  age_cube <- coef_df$Estimate[rownames(coef_df) == "leeftijd3"]
  df_age <- data.frame(leeftijd = 1:100)
  df_age$effect <- df_age$leeftijd * age_lin +
    df_age$leeftijd^2 * age_sq +
    df_age$leeftijd^3 * age_cube
  
  return(df_age)
}

make_inc_effect <- function(model_obj) {
  coef_df <- as.data.frame(summary(model_obj)$coefficients)
  inc_lin <- coef_df$Estimate[rownames(coef_df) == "income_sm"]
  inc_sq <- coef_df$Estimate[rownames(coef_df) == "income_sm2"]
  inc_cube <- coef_df$Estimate[rownames(coef_df) == "income_sm3"]
  df_inc <- data.frame(income = 1:1000)
  df_inc$effect <- df_inc$income * inc_lin +
    df_inc$income^2 * inc_sq +
    df_inc$income^3 * inc_cube
  
  return(df_inc)
}

gen_grouped_df <- function(op_ct_obj, df) {
  result_df <- generate_result_df(
    rpart.rules(op_ct_obj, roundint = FALSE) %>%
      as.data.frame())
  names(result_df) <- gsub("opl_unknown", "opl____", names(result_df))
  df$group <- NA
  
  for (row in 1:nrow(result_df)) {
    print(row / nrow(result_df))
    check <- df %>%
      filter(leeftijd >= result_df$leeftijd_min[row],
             leeftijd < result_df$leeftijd_max[row],
             income_sm >= result_df$income_sm_min[row],
             income_sm < result_df$income_sm_max[row])
    
    binary_cols <- names(result_df)[grepl("^h|geslacht|^opl", names(result_df))]

    for (col in binary_cols) {
      check <- align_binary(check, result_df, row=row, 
                            var=col)
    }
    df$group[df$match_id %in% check$match_id] <- row
  }
  return(df)
}

make_summ_tables <- function(df) {
  summ_leeftijd <- df %>%
    group_by(leeftijd) %>%
    summarise(n = n(),
              mean_effect = mean(effect),
              sd_effect = sd(effect))
  summ_inc <- df %>%
    mutate(income = round(income_sm / 10)) %>%
    group_by(income) %>%
    summarise(n = n(),
              mean_effect = mean(effect),
              sd_effect = sd(effect))
  summ_herk <- df %>%
    mutate(herkomst3 = ifelse(h_nederlands == 1, "Nederlands",
                              ifelse(h_niet_westers == 1, "Niet-Westers", "Westers"))) %>%
    group_by(herkomst3) %>%
    summarise(n = n(),
              mean_effect = mean(effect),
              sd_effect = sd(effect))
  summ_hh <- df %>%
    mutate(huishouden = ifelse(hh_eenouderhuishouden == 1, "Eenouder",
                               ifelse(hh_eenpersoonshuishouden == 1, "Eenpersoons",
                                      ifelse(hh_inst_onbekend == 1, "Inst_Onbekend",
                                             ifelse(`hh_overig meerpersoonshuishouden` == 1, "Overig_meer",
                                                    ifelse(`hh_paar met kinderen` == 1, "Paar_kids",
                                                           ifelse(`hh_paar zonder kinderen` == 1, "Paar_no_kids", NA))))))) %>%
    group_by(huishouden) %>%
    summarise(n = n(),
              mean_effect = mean(effect),
              sd_effect = sd(effect))
  summ_geslacht <- df %>%
    group_by(geslacht) %>%
    summarise(n = n(),
              mean_effect = mean(effect),
              sd_effect = sd(effect))
  return(list(summ_geslacht, summ_herk, summ_hh, summ_inc, summ_leeftijd))
}

write_summ <- function(list, filename = "output/220711_Output_WMO3/group_summ_full.xlsx") {
  geslacht <- list[[1]]
  
  make_ready <- function(table) {
    table$n <- DescTools::RoundTo(table$n, 5)
    table <- table[table$n >= 10, ]
    return(table)
  }
  
  
  openxlsx::write.xlsx(
    list("geslacht" = list[[1]] %>% make_ready(),
         "herkomst" = list[[2]] %>% make_ready(),
         "huishouden" = list[[3]] %>% make_ready(),
         "income" = list[[4]] %>% make_ready(),
         "leeftijd" = list[[5]] %>% make_ready()),
    file = filename)
}

write_lm_mod <- function(mod_obj, file="output/220711_Output_WMO3/group_multivariate_full.xlsx") {
  writexl::write_xlsx(
    rbind(
      broom::tidy(mod_obj) %>% as.data.frame,
      c("N", stats::nobs(mod_obj), NA, NA, NA),
      c("Degrees of Freedom", summary(mod_obj)$df[2], NA, NA, NA),
      c("R-squared", summary(mod_obj)$r.squared, NA, NA, NA)),
    file
  )
}

make_group_descs <- function(df) {
  ## Function to make descriptives of each group
  #' @param df Dataframe on individual level with assigned group
  #' @return Summary table excluding small groups (< 10 obs.)
  
  desc <- df %>%
    dplyr::select(-rinpersoon,
                  -gem_2019, -treat, -y, -match_id,
                  -leeftijd2, -leeftijd3, -income_sm2,
                  -income_sm3) %>%
    group_by(group) %>%
    summarise_all(.funs = mean)
  
  n <- df %>%
    group_by(group) %>%
    summarise(n = n())
  
  n$n <- DescTools::RoundTo(n$n, 5)
  desc_n <- desc %>%
    left_join(n)
  perc_names <- names(desc_n)[grepl("hh_|h_|opl_|geslacht", names(desc_n))]
  desc_n[, perc_names][(desc_n[, perc_names]  * desc_n$n < 10) & (desc_n[, perc_names]  * desc_n$n > 0)] <- NA
  return(desc_n)
}
