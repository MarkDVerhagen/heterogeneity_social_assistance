library(WeightIt)
library(ebal)
library(cobalt)

bind_coefs <- function(coef_list, df, munic_set = NULL) {
  ## Function to generate a dataframe with coefficients
  #' @param coef_list List with the coefficients from model summaries.
  #' @param munic_set List with municipalities for which separate models were estimated
  #' @param df Total dataset
  if (!is.null(munic_set)) {
    df_coef <- bind_rows(coef_list)
    df_coef$gem_2019 <- munic_set
    df_coef <- df_coef %>%
      merge(as.data.frame(t(table(df_coef$gem_2019)))[, c('Var2', 'Freq')],
            by.x = 'gem_2019', by.y = 'Var2')
  } else {
    df_coef <- as.data.frame(t(coef_list[[1]]))
    df_coef$gem_2019 <- 'Full'
    df_coef$Freq <- nrow(df)
  }
  return(df_coef)
}

covariate_ate <- function(df_list, covariates=c('1 + .'), outcome = 'y', munic_set = NULL) {
  ##
  #' @param df_list List of pooled dataframe consisting of a treatment year and control year(s)
  #' @param covariates List with all covariates to add to linear regression
  #' @param outcome Name of the outcome variable
  
  fun_cov <- as.formula(paste0(outcome, " ~ ", paste(covariates, collapse = "+")))
  model_cov <- lapply(df_list, FUN = function(x) {lm(fun_cov, data = x)})
  coef_cov <- lapply(model_cov, FUN = function(x) {summary(x)$coefficients['treat', ]})
  df_cov <- bind_coefs(coef_cov, df, munic_set)
  return(df_cov)
}

ps_ebal_ate <- function(df_list, bal_fun, method = "propensity", munic_set = NULL, save_bal = T) {
  ## Function to generate an ATE from a reweighted sample
  #' @param dfs List with dataframes
  #' @param bal_fun Formula to estimate the probability of being treated
  #' @param method The method used to generate the ATE
  
  if (method == "propensity") {
    weights <- lapply(df_list, FUN = function(x) {
      weightit(formula(bal_fun),
               method = "ps", replace = TRUE, estimand = "ATE", data = x)
    })
  } else if (method == "entropy") {
    weights <- lapply(df_list, FUN = function(x) {
      weightit(formula(bal_fun), method = "ebal", moments = 1, replace = TRUE, estimand = "ATE", data = x)
    })  
  }
  if (save_bal) {
    bal_df <- bal.tab(weights[[1]], un = TRUE, m.threshold = 0.1)
    bal_df <- bal_df$Balance
    bal_df$vars <- rownames(bal_df)
    writexl::write_xlsx(bal_df[c("vars", "Type", "M.Threshold", "Diff.Un", "Diff.Adj")],
                        paste0("output/220711_Output_WMO3/", method, "_bal_table.xlsx"))
    }
  
  df_list_inc_w <- mapply(
    function(x, y) {
      x$weight <-get.w(y) 
      return(x)
    }, df_list, weights, SIMPLIFY = F)
  
  model <- lapply(df_list_inc_w, FUN = function(x) {lm(as.formula("y ~ 1 + treat"), data = x, weights = weight)})
  coef <- lapply(model, FUN = function(x) {summary(x)$coefficients['treat', ]})
  df <- bind_coefs(coef, df, munic_set)
  
  return(df)
}

gen_bal_plot <- function(w_object, data, w_fun) {
  ##
  #' @param w_object A weightit object
  #' @param data The data
  #' @param w_fun The weighting function
  
  bal.tab(w_object, un = TRUE, m.threshold = 0.1)
  love.plot(w_fun,
            data = data,
            weights = list(w1 = get.w(w_obj)),
            thresholds = c(m = 0.1)
  )  
}


generate_treatment_effect <- function(df, method = c("covariate", "entropy", "propensity"), seed=1704,
                                      sample = NULL, munic_set = NULL, covariates = "1 + .", outcome = "y",
                                      bal_fun = "treat ~ 1", omit_gem = TRUE, save_bal = T) {
  ##
  #' @param df Pooled dataframe consisting of a treatment year and control year(s)
  #' @param method String indicating the type of estimation used to generate the ATE
  #' @param seed Numeric seed for consistent RNG
  #' @param sample Set of RIN codes in case of a specific subset to be used
  #' @param munic_set Set of municipality code in case separate ATEs have to be calculated
  #' @return Dataframe including the estimated treatment effects for the specific groups
  #' including high and low bounds 
  
  set.seed(seed)
  results <- c()
  
  # subset data in case a custom set of rin codes is provided
  if (!is.null(sample)) {
    df <- df[df$rinpersoon %in% sample, ]
  }
  df$rinpersoon <- NULL
  
  # generate a list of datasets to estimate the ATE on
  if (!is.null(munic_set)) {
    df_list <- lapply(munic_set, FUN = function(x, omit_gem) {
      return(df %>%
               filter(gem_2019 %in% x) %>%
               dplyr::select(-gem_2019))})
  } else {
    df$gem_2019 <- NULL
    if (omit_gem) {
      df <- df %>%
        dplyr::select(-contains('gem_2019'))  
    }
    df_list <- list(df)
  }
  
  
  if ("covariate" %in% method) {
    df_cov <- covariate_ate(df_list, munic_set = munic_set)
    results <- append(results, list(covariate = df_cov))
  }
  if ("entropy" %in% method) {
    df_ebal <- ps_ebal_ate(df_list, bal_fun, method = "entropy", munic_set = munic_set, save_bal = save_bal)
    results <- append(results, list(entropy = df_ebal))
  }
  if ("propensity" %in% method) {
    df_ps <- ps_ebal_ate(df_list, bal_fun, method = "propensity", munic_set = munic_set, save_bal = save_bal)
    results <- append(results, list(propensity = df_ps))
  }
  return(results)
}

gen_samples <- function(df) {
  ##
  #' @param df Dataframe with individual level data pooled across control, treat.
  #' @return List of samples for X, Y, W.
  
  X_sample <- df %>%
    select(gem_2019, huishoudsamenstelling, leeftijd,
           geslacht, lower_bound_num, herkomst, treat)
  
  X_sample_dummies <- X_sample %>%
    dplyr::select(leeftijd, geslacht, herkomst, huishoudsamenstelling,
                  lower_bound_num, gem_2019) %>%
    fastDummies::dummy_columns(select_columns =
                                 c("geslacht", "herkomst", "huishoudsamenstelling"),
                               remove_selected_columns = T)
  
  if (nrow(X_sample) > 10000000) {
    X_sample_dummies_comb <- rbind(
      X_sample_dummies[1 : 10000000, ] %>%
        fastDummies::dummy_cols(select_columns = c("gem_2019"),
                                remove_selected_columns = T),
      X_sample_dummies[10000001 : nrow(X_sample_dummies), ] %>%
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