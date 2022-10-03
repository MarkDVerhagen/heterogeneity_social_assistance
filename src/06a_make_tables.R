library(tidyverse)
library(xtable)
library(knitr)

options(scipen = 10)

source("./src/06_functions.R")

## Municipalities
table_munic <- readxl::read_xlsx("./data/table_munic_submissions_full.xlsx")
table_munic <- table_munic[, c("n_gem", "N", "n_gem_wmo_registry", "n_registry",
                               "in_sample_n_gem", "in_sample_2019_gem", "in_sample_n")]
kable(table_munic, "latex", digits=0)
## Descriptives overall
table_desc <- readxl::read_xlsx("./data/table_desc_2016_2019.xlsx")
names(table_desc) <- c("Variable", "2016_sample_mean", "2016_sample_sd", "2019_sample_mean", "2019_sample_sd",
                       "2016_pop_mean", "2016_pop_sd", "2019_pop_mean", "2019_pop_sd")
table_desc$Variable <- modify_variables(table_desc$Variable)

kable(table_desc, "latex", digits = 3)

## Descriptives by WMO
table_by_wmo <- readxl::read_xlsx("./data/table_desc_by_wmo_2016_2019_inc_educ.xlsx")
names(table_by_wmo) <- c("Variable", "Non-users",  "Users", "Non-users",  "Users")

table_by_wmo$Variable <- modify_variables(table_by_wmo)
table_by_wmo <- table_by_wmo[table_by_wmo$Variable != "geslacht_man", ]

kable(table_by_wmo, "latex", digits = 3)

## ATEs
ates_ex_gm <- readRDS("models/ates_ex_gm_all_df.rds")
ates_grf <- readRDS("H:/wmo_heterogeneity/data/final/ate_non_gm_tau_forest_033_1250ms_500t_18plus.rds") %>%
  t() %>%
  as.data.frame() %>%
  rename(Estimate = estimate,
         `Std. Error` = std.err)
table_ates <- bind_rows(
  bind_rows(ates_ex_gm),
  ates_grf %>% mutate(gem_2019 = "Full", Freq = round(nrow(df) / 3))
)
kable(table_ates, "latex", digits = 5)

## ATEs per sample
table_ates_sample <- readxl::read_xlsx("./data/ates_per_sample.xlsx")
kable(table_ates_sample, "latex", digits = 5)
## VI metrics
vi_metrics <- readxl::read_xlsx("./data/variable_importance.xlsx")
vi_metrics$variable <- modify_variables(vi_metrics$variable)
vi_metrics$variable <- gsub("gem_2019_", "Municipality: ", vi_metrics$variable)
vi_metrics$variable <- gsub("344", "Utrecht", vi_metrics$variable)
vi_metrics$variable <- gsub("34", "Almere", vi_metrics$variable)
vi_metrics$variable <- gsub("599", "Rotterdam", vi_metrics$variable)
vi_metrics$variable <- gsub("363", "Amsterdam", vi_metrics$variable)
vi_metrics$variable <- gsub("518", "The Hague", vi_metrics$variable)
names(vi_metrics) <- c("Importance", "Variable")
kable(vi_metrics, "latex", digits =6)

## Group descriptives

gen_format_desc_table <- function(path = "./data/ex_sel_gm_desc.xlsx", sheet = NULL) {
    if (is.null(sheet)) {
    group_desc <- readxl::read_xlsx(path) %>%
        t() %>%
        as.data.frame()
    } else {
    group_desc <- readxl::read_xlsx(path, sheet = sheet) %>%
        t() %>%
        as.data.frame()
    }
    
    
    for (num in 2:13) {
        group_desc[num, ] <- formatC(as.numeric(group_desc[num, ]), format = "f", digits=2)    
    }
    group_desc[15, ] <- formatC(as.numeric(group_desc[15, ]), format = "f", digits=4) 
    group_desc$var <- rownames(group_desc)
    rownames(group_desc) <- NULL
    group_desc <- group_desc %>%
        dplyr::select(var, everything())

    group_desc$var <- gsub("hh_", "huishoudsamenstelling_", group_desc$var) 
    group_desc$var <- gsub("h_", "herkomst_", group_desc$var) 
    group_desc$var <- gsub("group", "Group", group_desc$var)
    group_desc$var <- gsub("income_sm", "Income: % of soc. min.", group_desc$var)
    group_desc$var <- gsub("unknown", "Other", group_desc$var)
    group_desc$var <- gsub("niet_Western", "Non-western", group_desc$var)
    group_desc$var <- modify_variables(group_desc$var)

    group_desc$var <- gsub("without", "w/o", group_desc$var)
    group_desc$var <- gsub("with", "w", group_desc$var)
    group_desc$var <- gsub("geslacht", "Sex", group_desc$var)
    group_desc$var <- gsub("y_effect", "Heterogenous Policy Effect", group_desc$var)
    group_desc <- group_desc %>%
        filter(var != "group")
    return(group_desc)
}

# ex_sel_gm
group_desc_ex_sel_gm <- gen_format_desc_table()
group_desc_ex_sel_gm <- group_desc_ex_sel_gm[, c(1, 3, 2, 7, 8, 10, 5, 4, 9, 13, 12, 6, 15, 11, 14)]

kable(group_desc_ex_sel_gm, "latex", digits = 2)

# 518
group_desc_ex_sel_gm <- gen_format_desc_table("./data/group_desc.xlsx", sheet = "518")
group_desc_ex_sel_gm <- group_desc_ex_sel_gm[, c(1, 2, 4, 3, 6, 8, 7, 5, 11, 10, 9, 12)]
kable(group_desc_ex_sel_gm, "latex")

# 363
group_desc_ex_sel_gm <- gen_format_desc_table("./data/group_desc.xlsx", sheet = "363")
group_desc_ex_sel_gm <- group_desc_ex_sel_gm[, c(1, 4, 7, 8, 3, 6, 10, 5,
                                                 9, 12, 2, 11, 13, 14)]
kable(group_desc_ex_sel_gm, "latex")

# 599
group_desc_ex_sel_gm <- gen_format_desc_table("./data/group_desc.xlsx", sheet = "599")
group_desc_ex_sel_gm <- group_desc_ex_sel_gm[, c(1, 3, 4, 5, 2, 7, 6, 8, 9, 10)]
kable(group_desc_ex_sel_gm, "latex")

# 34
group_desc_ex_sel_gm <- gen_format_desc_table("./data/group_desc.xlsx", sheet = "34")
group_desc_ex_sel_gm <- group_desc_ex_sel_gm[, c(1, 5, 2, 4, 3, 6, 7, 9, 8, 10, 11)]
kable(group_desc_ex_sel_gm, "latex")

# 344
group_desc_ex_sel_gm <- gen_format_desc_table("./data/group_desc.xlsx", sheet = "344")
group_desc_ex_sel_gm[13:14,]
group_desc_ex_sel_gm <- group_desc_ex_sel_gm[, c(1, 3, 4, 5, 9, 6, 8, 2, 7, 10, 13, 11, 12, 14)]

kable(group_desc_ex_sel_gm, "latex")

## Group regressions
group_reg <- readxl::read_xlsx("./data/group_regressions_v2.xlsx")

transform_column_sd <- function(df, col_name = "estimate (sd)") {
    return(as.numeric(gsub(".* \\(|\\).*", "", df[[col_name]])))
}

transform_column_est <- function(df, col_name = "estimate (sd)") {
    return(as.numeric(gsub(" \\(.*", "", df[[col_name]])))
}

group_reg$sd_ex_sel_gm <- transform_column_sd(group_reg, "estimate (sd)")
group_reg$sd_sel_gm_518 <- transform_column_sd(group_reg, "518_estimate (sd)")
group_reg$sd_sel_gm_363 <- transform_column_sd(group_reg, "363_estimate (sd)")
group_reg$sd_sel_gm_599 <- transform_column_sd(group_reg, "599_estimate (sd)")
group_reg$sd_sel_gm_34 <- transform_column_sd(group_reg, "34_estimate (sd)")
group_reg$sd_sel_gm_344 <- transform_column_sd(group_reg, "344_estimate (sd)")

group_reg$est_ex_sel_gm <- transform_column_est(group_reg, "estimate (sd)")
group_reg$est_sel_gm_518 <- transform_column_est(group_reg, "518_estimate (sd)")
group_reg$est_sel_gm_363 <- transform_column_est(group_reg, "363_estimate (sd)")
group_reg$est_sel_gm_599 <- transform_column_est(group_reg, "599_estimate (sd)")
group_reg$est_sel_gm_34 <- transform_column_est(group_reg, "34_estimate (sd)")
group_reg$est_sel_gm_344 <- transform_column_est(group_reg, "344_estimate (sd)")

group_reg <- group_reg %>%
    dplyr::select(-contains(" ("))

group_reg$term <- gsub("hh_", "huishoudsamenstelling_", group_reg$term)
group_reg$term <- gsub("h_", "herkomst_", group_reg$term)
group_reg$term <- modify_variables(group_reg$term)
group_reg$num_inc <- as.numeric(gsub("[^[:digit:]]", "", group_reg$term))
group_reg$num_inc_low <- group_reg$num_inc * 10 - 5
group_reg$num_inc_high <- group_reg$num_inc * 10 + 5
group_reg$term <- ifelse(grepl("income", group_reg$term),
                         paste0("Income: ", group_reg$num_inc_low, "% - ",
                                group_reg$num_inc_high, "% of soc. min."),
                         group_reg$term)
group_reg <- group_reg[, c(
    "term", "est_ex_sel_gm", "sd_ex_sel_gm", "est_sel_gm_518", "sd_sel_gm_518",
    "est_sel_gm_363", "sd_sel_gm_363", "est_sel_gm_599", "sd_sel_gm_599",
    "est_sel_gm_34", "sd_sel_gm_34", "est_sel_gm_344", "sd_sel_gm_344"
)]

kable(group_reg, "latex", digits = 5)
writexl::write_xlsx(group_reg, "./data/table_group_reg_v2_clean.xlsx")

## Robustness: 2015 to 2016
r_2015_2016 <- readxl::read_xlsx("./data/r_regression_2015_2016.xlsx")
r_2015_2016$term <- gsub("as.factor\\(herkomst3\\)", "h_", r_2015_2016$term)
r_2015_2016$term <- gsub("as.factor\\(huishoudsamenstelling\\)", "hh_", r_2015_2016$term)
r_2015_2016$term <- gsub("as.factor\\(income_cat\\)", "", r_2015_2016$term)
r_2015_2016$term[13:52] <- paste0(
    "Income: ", (as.numeric(r_2015_2016$term[13:52])) * 25 - 12.5,
    "-", (as.numeric(r_2015_2016$term[13:52])) * 25 + 12.5, " of soc. min."
)
r_2015_2016$term <- modify_variables(r_2015_2016$term)
r_2015_2016$estimate <- as.numeric(r_2015_2016$estimate)
r_2015_2016$std.error <- as.numeric(r_2015_2016$std.error)
kable(r_2015_2016[, c("term", "estimate", "std.error")], "latex",
    digits = 4
)

## Univariate descriptives
univ_table <- readxl::read_xlsx("./data/table_univ_wmo_2016_2019.xlsx")
univ_table$prop_2016 <- univ_table$y_2016 / univ_table$n_2016
univ_table$prop_2019 <- univ_table$y_2019 / univ_table$n_2019

kable(univ_table[, c("var", "n_2016", "prop_2016", "n_2019", "prop_2019")], "latex",
      digits = 4)
