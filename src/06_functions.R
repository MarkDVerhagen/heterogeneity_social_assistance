modify_variables <- function(df_var) {
    ## Function to change names
    #' @param df_var Column with old names
    #' @return Column with new names

    df_var <- tolower(df_var)
    df_var <- gsub("herkomst_", "Migrant Background: ", df_var)
    df_var <- gsub("huishoudsamenstelling_",
                        "Household Type: ", df_var)
    df_var <- gsub("hbopl_", "Highest education: ", df_var)
    df_var <- gsub("geslacht_vrouw", "Sex: Female", df_var)
    df_var <- gsub("lower_bound_num", "Income: \\% of social minimum", df_var)
    df_var <- gsub("leeftijd", "Age", df_var)
    df_var <- gsub("---", "Unknown", df_var)
    df_var <- gsub("100", "Low", df_var)
    df_var <- gsub("200", "Middle", df_var)
    df_var <- gsub("300", "High", df_var)
    df_var <- gsub("nederlands", "Native Dutch", df_var)
    df_var <- gsub("niet-westers", "Non-Western", df_var)
    df_var <- gsub("westers", "Western", df_var)
    df_var <- gsub("eenouderhuishouden", "Single parent", df_var)    
    df_var <- gsub("eenpersoonshuishouden", "Single person", df_var)
    df_var <- gsub("inst_onbekend", "Institutional / unknown", df_var)
    df_var <- gsub("overig meerpersoonshuishouden", "Other", df_var)
    df_var <- gsub("paar met kinderen", "Couple with children", df_var)
    df_var <- gsub("paar zonder kinderen", "Couple without children", df_var)
    return(df_var)
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
