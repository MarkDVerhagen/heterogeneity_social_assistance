library(tidyverse)
library(patchwork)
library(gridExtra)
source("./src/06_functions.R")
source("./src/styles.R")

wmo_palette <- MetBrewer::met.brewer("Hokusai1")[c(1, 2, 4, 5, 7)]
munic_palette <- MetBrewer::met.brewer("Juarez")
## -- Use per WMO type
df <- readxl::read_xlsx("./data/yearly_wmo_gem.xlsx")

nl_prop <- df %>%
    group_by(year) %>%
    summarise_all(funs(sum(., na.rm = T))) %>%
    mutate(prop_wmo = n_wmo / n,
           prop_wmo_hh = n_wmo_hh / n,
           prop_wmo_hm = n_wmo_hm / n,
           prop_wmo_on = n_wmo_on / n,
           prop_wmo_vb = n_wmo_vb / n,
           reg_pc = (prop_wmo_hh + prop_wmo_hm + prop_wmo_on + prop_wmo_vb) /
           prop_wmo) %>%
    dplyr::select(-starts_with("n_"))

nl_prop_melt <- nl_prop %>%
    dplyr::select(-gem_2019, -n, -reg_pc) %>%
    reshape2::melt(id.vars = "year")

nl_prop_melt <- nl_prop_melt %>%
    mutate(variable = case_when(
        nl_prop_melt$variable == "prop_wmo" ~ "Any",
        nl_prop_melt$variable == "prop_wmo_hh" ~ "Household",
        nl_prop_melt$variable == "prop_wmo_hm" ~ "Services",
        nl_prop_melt$variable == "prop_wmo_on" ~ "Support",
        nl_prop_melt$variable == "prop_wmo_vb" ~ "External"
    ))

nl_type_plot <- ggplot(nl_prop_melt, aes(y = value, x = as.factor(year), fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = paste0(round(value * 100, 2), "%")), vjust = -0.3) +
    scale_fill_manual(name = "Assistance type", values = wmo_palette) +
    facet_wrap(~variable) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 15, hor = T) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.1)) +
    ylab("Proportion use") + xlab("Year")

prop_df <- df %>%
    mutate(prop_wmo = n_wmo / n,
           prop_wmo_hh = n_wmo_hh / n,
           prop_wmo_hm = n_wmo_hm / n,
           prop_wmo_on = n_wmo_on / n,
           prop_wmo_vb = n_wmo_vb / n) %>%
    dplyr::select(-starts_with("n_"))

melt_df <- prop_df %>%
    reshape2::melt(id.vars = c("year", "gem_2019", "n"))
    
cast_df_year <- melt_df %>%
    pivot_wider(id_cols = c("gem_2019", "variable"), names_from = c("year"),
                    values_from = c("value"))

cast_df <- melt_df %>%
    pivot_wider(id_cols = c("gem_2019", "year"), names_from = c("variable"),
                values_from = c("value"))
cast_df_delta <- melt_df %>%
    pivot_wider(id_cols = c("gem_2019"), names_from = c("variable", "year"),
                values_from = c("value")) %>%
    mutate(delta_wmo = prop_wmo_2019 - prop_wmo_2016,
           delta_wmo_hh = prop_wmo_hh_2019 - prop_wmo_hh_2016,
           delta_wmo_hm = prop_wmo_hm_2019 - prop_wmo_hm_2016,
           delta_wmo_on = prop_wmo_on_2019 - prop_wmo_on_2016,
           delta_wmo_vb = prop_wmo_vb_2019 - prop_wmo_vb_2016) %>%
    dplyr::select(gem_2019, starts_with("delta_"))

cast_df_delta_melt <- cast_df_delta %>%
    reshape2::melt(id.vars = c("gem_2019"))


cast_df_delta_melt <- cast_df_delta_melt %>%
    mutate(variable = case_when(
        cast_df_delta_melt$variable == "delta_wmo" ~ "Any",
        cast_df_delta_melt$variable == "delta_wmo_hh" ~ "Household",
        cast_df_delta_melt$variable == "delta_wmo_hm" ~ "Services",
        cast_df_delta_melt$variable == "delta_wmo_on" ~ "Support",
        cast_df_delta_melt$variable == "delta_wmo_vb" ~ "External"
    ))

gem_type_plot <- ggplot(cast_df_delta_melt) +
    geom_density(aes(x = value, fill = variable)) +
    facet_wrap(~variable, scales = "free") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_fill_manual(name = "Assistance type", values = wmo_palette) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 15, hor = T) +
    scale_x_continuous(labels = scales::percent, name = "2019 use versus 2016 use") +
    ylab("Density") + theme(legend.position = "top")

(nl_type_plot / gem_type_plot) + plot_layout(guides = "collect") & theme(legend.position = 'top')

ggsave("./tex/figs/fig_wmo_types_2019_2016_delta.pdf", last_plot(), width = 12, height=14)

## Correlation plots of use delta per wmo type
deltas <- list(
    "delta_wmo", "delta_wmo_hh", "delta_wmo_hm",
    "delta_wmo_on", "delta_wmo_vb"
)
names(wmo_palette) <- deltas

axes_title <- c("Any", "Household", "Services", "Support", "Internal")
names(axes_title) <- deltas

make_scatter <- function(data, x, axes_title, y = "delta_wmo") {
    ## Function to make a scatter plot to show delta use for all municipalities
    #' @param data Data
    #' @param x X-axis variable
    #' @param y Y-axis variable
    #' @param axes_title List with axis names
    #' @return Scatterplot

    x_limit <- max(abs(min(data[[x]])), max(data[[x]]))
    y_limit <- max(abs(min(data[[y]])), max(data[[y]]))
    return(
        ggplot(data) +
            geom_point(aes(y = data[[y]], x = cast_df_delta[[x]]), color = wmo_palette[x]) +
            scale_y_continuous(labels = scales::percent, limits=c(-y_limit, y_limit)) +
            scale_x_continuous(labels = scales::percent, limits=c(-x_limit, x_limit)) +
            xlab(axes_title[x]) + ylab(axes_title[y]) +
            cowplot::theme_cowplot() +
            custom_theme(text_size = 14, hor = T, ver = T) +
            geom_hline(yintercept =  0, linetype = "dashed") +
            geom_vline(xintercept =  0, linetype = "dashed") +
            theme(axis.title=element_text(size=18))
    )
}

corr_plots_wmo <- lapply(deltas, function(x) make_scatter(cast_df_delta, x, axes_title))
corr_plots_wmo_hh <- lapply(deltas, function(x) make_scatter(cast_df_delta, x, axes_title, y = "delta_wmo_hh"))
corr_plots_wmo_hm <- lapply(deltas, function(x) make_scatter(cast_df_delta, x, axes_title, y = "delta_wmo_hm"))
corr_plots_wmo_on <- lapply(deltas, function(x) make_scatter(cast_df_delta, x, axes_title, y = "delta_wmo_on"))
corr_plots_wmo_vb <- lapply(deltas, function(x) make_scatter(cast_df_delta, x, axes_title, y = "delta_wmo_vb"))

corr_plots_comb <- append(corr_plots_wmo, corr_plots_wmo_hh)
corr_plots_comb <- append(corr_plots_comb, corr_plots_wmo_hm)
corr_plots_comb <- append(corr_plots_comb, corr_plots_wmo_on)
corr_plots_comb <- append(corr_plots_comb, corr_plots_wmo_vb)

ggsave(filename = "./tex/figs/fig_wmo_gem_delta_correlations.pdf",
       (do.call("grid.arrange", c(corr_plots_comb, ncol = 5, nrow = 5))),
       width = 19, height = 19)

## Use per municipality
use_gem <- readxl::read_xlsx("./data/yearly_wmo_gem.xlsx")
use_gem$prop_wmo <- use_gem$n_wmo / use_gem$n
use_gem_wide <- use_gem %>%
    select(gem_2019, year, prop_wmo) %>%
    pivot_wider(names_from = year, values_from = prop_wmo)
use_gem_wide_inc_n <- use_gem_wide %>%
    left_join(use_gem %>% filter(year == 2016) %>% select(gem_2019, n))

ggplot(use_gem_wide_inc_n, aes(y = `2019`, x = `2016`, size = n / 100000)) +
    geom_point(fill = wmo_palette[1], shape = 21, alpha = 0.9) +
    geom_point(aes(y = nl_prop_melt$value[(nl_prop_melt$year == "2019") & (nl_prop_melt$variable == "Any")],
                   x = nl_prop_melt$value[(nl_prop_melt$year == "2016") & (nl_prop_melt$variable == "Any")],
                   shape = "Netherlands"), size = 4) +
    geom_abline(slope = 1, linetype = "dashed") +
    geom_abline(slope = 1, intercept = 0.01, linetype = "dotted") +
    geom_label(data = use_gem_wide_inc_n[1, ], aes(x = 0.14125, label = "+1%", y = 0.15), size = 3) +
    geom_abline(slope = 1, intercept = 0.02, linetype = "dotted") +
    geom_label(data = use_gem_wide_inc_n[1, ], aes(x = 0.13, label = "+2%", y = 0.15), size = 3) +
    geom_abline(slope = 1, intercept = 0.03, linetype = "dotted") +
    geom_label(data = use_gem_wide_inc_n[1, ], aes(x = 0.1185, label = "+3%", y = 0.15), size = 3) +
    guides(size = guide_legend("Municipality\nsize ('00,000s)")) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 16, hor = T, ver = T) +
    theme(legend.position = "right") +
    scale_shape_manual(name = "", values = c(8)) +
    scale_y_continuous(labels = scales::percent, name = "Any social assistance use in 2019",
                       limits = c(0.015, 0.15)) +
    scale_x_continuous(labels = scales::percent, name = "Any social assistance use in 2016",
                       limits = c(0.015, 0.15))
ggsave("./tex/figs/fig_wmo_gem_2019_2016_delta.pdf", last_plot(), width = 8, height=6)

## Predictions on the municipality level
gem_preds <- readxl::read_xlsx("./data/gem_predictions.xlsx")
gem_preds_oos <- readxl::read_xlsx("./data/wmo_2016_predict_v3.xlsx")
gem_preds$size <- gem_preds$n / 10000
gem_preds <- gem_preds %>%
    left_join(gem_preds_oos[, c("gem_2019", "predicted")])

ggplot(gem_preds, aes(size = size, y = y_mean, x = predicted)) +
    geom_point(alpha = 0.6) +
    scale_y_continuous(labels = scales::percent, limits = c(0.01, 0.13),
                       name = "Observed use (2016 data)") +
    scale_x_continuous(labels = scales::percent, limits = c(0.01, 0.13),
                       name = "Predicted use (2016 data)") +
    guides(size = guide_legend("Municipality\nsize ('0,000s)")) +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T, text_size = 16) +
        theme(legend.position = "right") +
    geom_abline(slope = 1, linetype = "dashed")
ggsave("./tex/figs/fig_wmo_gem_2016_preds.pdf", last_plot(), width = 8, height=6)

## Balance plots
balance_prop <- readxl::read_xlsx("./data/balance_tables.xlsx", sheet = "Propensity matching")
balance_entrop <- readxl::read_xlsx("./data/balance_tables.xlsx", sheet = "Entropy balancing")
balance_tot <- rbind(
    balance_prop %>% mutate(type = "Propensity"),
    balance_entrop %>% mutate(type = "Entropy")
)
balance_tot$vars <- modify_variables(balance_tot$vars)
balance_tot$vars <- gsub("paar_zonder_kinderen", "Couple without children",
                         balance_tot$vars)
balance_tot$vars <- gsub("paar_met_kinderen", "Couple with children",
                         balance_tot$vars)
balance_tot$vars <- gsub("overig_meerpersoonshuishouden", "Other multi-person",
                         balance_tot$vars)
balance_tot$vars <- gsub("prop.score", "Propensity Score",
                         balance_tot$vars)
balance_tot$vars <- gsub("niet_Western", "Non-western",
                         balance_tot$vars)
balance_tot$vars <- gsub("Institutional / unknown", "Unconventional", balance_tot$vars)

balance_melt_adj <- balance_tot %>%
    dplyr::select(vars, Diff.Adj, type) %>%
    reshape2::melt(id.vars = c("vars", "type"))
balance_melt_unadj <- balance_tot %>%
    filter(type == "Propensity") %>%
    dplyr::select(vars, Diff.Un, type) %>%
    reshape2::melt(id.vars = c("vars", "type")) %>%
    mutate(type = "Unbalanced")

balance_comb <- rbind(balance_melt_adj %>% filter(type == "Entropy"),
                      balance_melt_unadj,
                      balance_melt_adj %>% filter(type == "Propensity"))
balance_comb$type <- factor(balance_comb$type,
                            levels = c("Entropy", "Unbalanced", "Propensity"))

ggplot(balance_comb, aes(y = value, x = vars, shape = type, color = type)) +
    geom_point(position = position_dodge2(width = 0.4), size =2.5) +
    coord_flip() +
    scale_color_manual(name = "Method", values = c(ggsci::pal_aaas("default")(2)[1],
                                                  "Black",
                                                  ggsci::pal_aaas("default")(2)[2])) +
    scale_shape_manual(name = "Method", values = c(16, 8, 16)) +
        scale_y_continuous(limits = c(-0.15, 0.15), name = "Difference between treatment and control") +
        xlab("") +
        cowplot::theme_cowplot() +
        custom_theme(text_size = 10, hor = T, ver = T) +
    theme(legend.position = "top")

ggsave(filename = "./tex/figs/fig_bal_plots.pdf", last_plot(), width = 8, height = 5)

## Model coefs 2016
coefs_2016 <- readxl::read_xlsx("./data/model_coefs_2016_lm_lmer.xlsx")
coefs_2016 <- coefs_2016 %>%
    filter(Model == "LM")
coefs_2016$income_num <- ifelse(grepl("income_cat", coefs_2016$var),
    as.numeric(gsub("as.factor\\(income_cat\\)", "", coefs_2016$var)),
    NA
)
coefs_2016$income_num <- coefs_2016$income_num / 4

coefs_2016$Estimate <- as.numeric(coefs_2016$Estimate)
income_plot <- ggplot(coefs_2016, aes(y = Estimate, x = income_num)) +
    geom_point(alpha = 0.8, size =2.5) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 11, hor = T, ver = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent, name = "Average policy effect") +
    scale_x_continuous(labels = scales::percent_format(big.mark = ","), name = "Household income relative to social minimum")

age_effects <- coefs_2016$Estimate[coefs_2016$Variable == "Age"]
df_age_lm <- data.frame(age = 18:95, model = "LM")
df_age_lm$effect <- df_age_lm$age * age_effects[1] +
    df_age_lm$age^2 * age_effects[2]

age_plot <- ggplot(rbind(df_age_lm), aes(y = effect, x = age)) +
    geom_point(alpha = 0.8, size =2.5) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 11, hor = T, ver = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent, name = "Average policy effect") +
    xlab("Age")

coefs_2016_dummy <- coefs_2016 %>%
    filter(Variable %in% c("Migrant background", "Household", "Sex"))
coefs_2016_dummy$var <- gsub("\\)", "_", coefs_2016_dummy$var)
coefs_2016_dummy$var <- gsub("as.factor\\(", "", coefs_2016_dummy$var)
coefs_2016_dummy$var <- modify_variables(coefs_2016_dummy$var)
coefs_2016_dummy$var <- gsub(": ", ":\n", coefs_2016_dummy$var)
coefs_2016_dummy$var <- gsub("Institutional / unknown", "Unconventional", coefs_2016_dummy$var)
coefs_2016_dummy$var <- gsub("Other", "Other multi-person", coefs_2016_dummy$var)

dummy_plot <- ggplot(coefs_2016_dummy, aes(y = Estimate, x = var)) +
    coord_flip() +
    geom_point(alpha = 0.8, size =2.5) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 11, hor = T, ver = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent, name = "Average policy effect") +
    xlab("")

cont_plots <- (income_plot / age_plot)
ggsave("./tex/figs/fig_coefs_2016.pdf", cont_plots | dummy_plot, width = 9, height = 7)

## Elbow tables
elbow <- readxl::read_xlsx("./data/elbow_tables.xlsx")
elbow$depth <- 1 : nrow(elbow)
elbow_melt <- elbow %>%
    reshape2::melt(id.vars = c("depth")) %>%
    filter(!is.na(value)) %>%
    filter(depth <= 15)
elbow_melt$variable <- gsub("NL", "Consistent\nMunicipalities", elbow_melt$variable)

selection_df <- data.frame(
    value = c(
        elbow_melt$value[(elbow_melt$variable == "Consistent\nMunicipalities") &
        (elbow_melt$depth == 12)],
    elbow_melt$value[(elbow_melt$variable == "Amsterdam") &
        (elbow_melt$depth == 12)],
    elbow_melt$value[(elbow_melt$variable == "The Hague") &
        (elbow_melt$depth == 8)],
    elbow_melt$value[(elbow_melt$variable == "Utrecht") &
        (elbow_melt$depth == 9)],
    elbow_melt$value[(elbow_melt$variable == "Almere") &
        (elbow_melt$depth == 9)],
    elbow_melt$value[(elbow_melt$variable == "Rotterdam") &
        (elbow_melt$depth == 7)]    
    ),
    variable = c("Consistent\nMunicipalities", "Amsterdam", "The Hague", "Utrecht", "Almere",
                 "Rotterdam"),
    type = "Selection",
    depth = c(12, 12, 8, 9, 9, 7)
)

ggplot(elbow_melt, aes(y = value, x = depth, color = variable)) +
    geom_point() +
    geom_line() +
    geom_point(data = selection_df, shape = 8, size = 5) +
    # ggsci::scale_color_aaas(name = "Sample") +
    scale_color_manual(name = "Sample", values = munic_palette) +
    cowplot::theme_cowplot() +
    custom_theme(text_size = 12, hor = T, ver = T) +
    ylab("Out-of-sample Error") + xlab("Tree Depth")

ggsave("./tex/figs/fig_elbow_graphs.pdf", last_plot(), width = 10, height = 6)


## Robustness with entire sample

gen_robust_df <- function(sheet = "ex_sel_gm_500K_2500ms_18plus") {
    ct_results <- readxl::read_xlsx("./data/ct_tables.xlsx", sheet = sheet)
    ct_results <- ct_results %>%
      rename(node = group) %>%
      rename(y = ate_sample)
    return(ct_results)
}

ct_results <- gen_robust_df()

gen_robust_plot <- function(data, hline, color = "red", inc_x = F, inc_y = F) {
    plot <- ggplot(data, aes(y = y, x = ate, size = n / 100000)) +
        geom_point(alpha = 0.8, color = color) +
        geom_hline(yintercept = hline, linetype = "dashed") +
        geom_vline(xintercept = hline, linetype = "dashed") +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T) +
        scale_x_continuous(name = "", labels = scales::percent) +
        scale_y_continuous(name = "", labels = scales::percent) +
        guides(size = guide_legend("Group size\n('00,000s)")) +
        theme(legend.position = "top") +
        geom_text(aes(label = node), hjust = -.15, vjust = -.15, size = 4)

    if (inc_x) {
        plot <- plot + scale_x_continuous(name = "Average Policy Effect (Entire Sample)",
                                          labels = scales::percent)
    }

    if (inc_y) {
        plot <- plot + scale_y_continuous(
            name = "Average Policy Effect (Estimation Sample)",
            labels = scales::percent
        )
    }
    return(plot)
}

plot_con <- gen_robust_plot(ct_results, hline = 0.0106, color = munic_palette[3],
                            inc_y=T) +
    ggtitle("Consistent Municipalities")
plot_518 <- gen_robust_plot(gen_robust_df("sel_gm_518_500K_2500ms_18plus"),
                color = munic_palette[5], hline = -0.0013, inc_y=T) +
    ggtitle("The Hague")
plot_363 <- gen_robust_plot(gen_robust_df("sel_gm_363_500K_2500ms_18plus"),
                color = munic_palette[2], hline = 0.01, inc_y=T, inc_x = T) +
    ggtitle("Amsterdam")
plot_599 <- gen_robust_plot(gen_robust_df("sel_gm_599_500K_2500ms_18plus"),
                color = munic_palette[4], hline = 0.0056) +
    ggtitle("Rotterdam")
plot_34 <- gen_robust_plot(gen_robust_df("sel_gm_34_500K_2500ms_18plus"),
                color = munic_palette[1], hline = 0.0037) +
    ggtitle("Almere")
plot_344 <- gen_robust_plot(gen_robust_df("sel_gm_344_500K_2500ms_18plus"),
    color = munic_palette[6], hline = 0.0026, inc_x = T
) +
    ggtitle("Utrecht")

(plot_con / plot_518 / plot_363) | (plot_599 / plot_34 / plot_344)

ggsave("./tex/figs/fig_robustness_groups_all.pdf", last_plot(), width = 11, height = 15)

ggplot(tot_results, aes(y = y, x = ate, size = n / 100000, color = Sample)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0.0079, linetype = "dashed") +
    geom_vline(xintercept = 0.0079, linetype = "dashed") +
    theme_bw() +
    scale_x_continuous(name = "Average Policy Effect (Entire Sample)",
                       labels = scales::percent) +
    scale_y_continuous(name = "Average Policy Effect (Estimation Sample)",
                       labels = scales::percent) +
    guides(size = guide_legend("Group size\n('00,000s)"))

ggsave("./tex/figs/fig_robustness_groups_nl.pdf", last_plot(), width = 8, height = 6)

## Plots of 2016 and 2019 use by group

gen_y_delta_plot <- function(data, hline = 0, color = "red", inc_x = F, inc_y = F) {
    vline <- weighted.mean(ct_results$y_2016, ct_results$n_2016)
    plot <- ggplot(data, aes(y = ate, x = y_2016, size = n / 100000)) +
        geom_point(alpha = 0.8, color = color) +
        geom_hline(yintercept = 0, color = "darkgrey") +
        geom_vline(xintercept = 0, color = "darkgrey") +
        geom_hline(yintercept = hline, linetype = "dashed") +
        geom_vline(xintercept = vline, linetype = "dashed") +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T) +
        scale_x_continuous(name = "", labels = scales::percent) +
        scale_y_continuous(name = "", labels = scales::percent) +
        guides(size = guide_legend("Group size\n('00,000s)")) +
        theme(legend.position = "top") +
        geom_text(aes(label = node), hjust = -.15, vjust = -.15, size = 4)

    if (inc_x) {
        plot <- plot + scale_x_continuous(name = "Pre-policy Prevalence (Entire Sample)",
                                          labels = scales::percent)
    }

    if (inc_y) {
        plot <- plot + scale_y_continuous(
            name = "Average Policy Effect (Entire Sample)",
            labels = scales::percent
        )
    }
    return(plot)
}

plot_con <- gen_y_delta_plot(ct_results, hline = 0.0106, color = munic_palette[3],
                            inc_y=T) +
    ggtitle("Consistent Municipalities")
plot_518 <- gen_y_delta_plot(gen_robust_df("sel_gm_518_500K_2500ms_18plus"),
                color = munic_palette[5], hline = -0.0013, inc_y=T) +
    ggtitle("The Hague")
plot_363 <- gen_y_delta_plot(gen_robust_df("sel_gm_363_500K_2500ms_18plus"),
                color = munic_palette[2], hline = 0.01, inc_y=T, inc_x = T) +
    ggtitle("Amsterdam")
plot_599 <- gen_y_delta_plot(gen_robust_df("sel_gm_599_500K_2500ms_18plus"),
                color = munic_palette[4], hline = 0.0056) +
    ggtitle("Rotterdam")
plot_34 <- gen_y_delta_plot(gen_robust_df("sel_gm_34_500K_2500ms_18plus"),
                color = munic_palette[1], hline = 0.0037) +
    ggtitle("Almere")
plot_344 <- gen_y_delta_plot(gen_robust_df("sel_gm_344_500K_2500ms_18plus"),
    color = munic_palette[6], hline = 0.0026, inc_x = T
) +
    ggtitle("Utrecht")

(plot_con / plot_518 / plot_363) | (plot_599 / plot_34 / plot_344)

ggsave("./tex/figs/fig_y_delta_groups_all.pdf", last_plot(), width = 11, height = 15)


## Group regressions
group_reg <- readxl::read_xlsx("./data/table_group_reg_v2_clean.xlsx")
group_reg$type <- case_when(
    grepl("Migrant Background", group_reg$term) ~ "Migrant Background",
    grepl("Household", group_reg$term) ~ "Household Type",
    grepl("geslacht", group_reg$term) ~ "Sex",
    grepl("Age", group_reg$term) ~ "Age",
    grepl("Income", group_reg$term) ~ "Income",
    TRUE ~ "Other"
)

group_reg_melt <- group_reg %>%
    filter(!(term %in% c("n", "degrees of freedom", "r-squared"))) %>%
    reshape2::melt(id.vars = c("term", "type"))
group_reg_melt_est <- group_reg_melt %>%
    filter(grepl("est", group_reg_melt$variable))
group_reg_melt_sd <- group_reg_melt %>%
    filter(grepl("sd", group_reg_melt$variable))
group_reg_melt_est$sd <- group_reg_melt_sd$value
group_reg_melt_est$Sample <- case_when(
    grepl("518", group_reg_melt_est$variable) ~ "The Hague",
    grepl("363", group_reg_melt_est$variable) ~ "Amsterdam",
    grepl("599", group_reg_melt_est$variable) ~ "Rotterdam",
    grepl("344", group_reg_melt_est$variable) ~ "Utrecht",
    grepl("34", group_reg_melt_est$variable) ~ "Almere",
    TRUE ~ "Consistent\nMunicipalities"
)

change_hh_term <- function(df_col) {
  df_col <- gsub("Institutional / unknown", "Unconventional", df_col)
  df_col <- gsub("Other", "Other multi-person", df_col)
  return(df_col)
}

group_reg_melt_est$term <- change_hh_term(group_reg_melt_est$term)

h_plot <- ggplot(group_reg_melt_est %>% filter(type == "Migrant Background"),
       aes(y = value, x = term, color = Sample, shape = Sample)) +
    geom_point(position = position_dodge(width = 0.2), size = 3) + coord_flip() +
        facet_wrap(~type) +
        geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() +
        scale_color_manual(values = munic_palette) +
        scale_y_continuous(name = "Average Policy Effect",
                       labels = scales::percent) +
        scale_x_discrete(labels = c("Native Dutch", "Non-western migrant")) +
        xlab("") +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T)

hh_plot <- ggplot(group_reg_melt_est %>% filter(type == "Household Type"),
       aes(y = value, x = term, color = Sample, shape = Sample)) +
    facet_grid(. ~ type, scales = "free") +
    geom_point(position = position_dodge(width = 0.2), size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() +
        scale_color_manual(values = munic_palette) + coord_flip() +
        scale_y_continuous(name = "Average Policy Effect",
                       labels = scales::percent) +
        scale_x_discrete(labels = c("Couple w children", "Other multi-person", "Unconventional",
                                    "Single parent", "Single person")) +
        xlab("") +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T)

age_effects <- group_reg_melt_est %>%
    filter(type == "Age") %>%
    dplyr::select(value, term, Sample) %>%
    reshape2::melt(id.vars = c("Sample", "term")) %>%
    pivot_wider(values_from = "value", id_cols = "Sample", names_from = "term")

age <- 18:95

df_age <- data.frame(age = rep(age, 6),
                     Sample = c(rep("Consistent\nMunicipalities", 78),
                                rep("The Hague", 78),
                                rep("Amsterdam", 78),
                                rep("Rotterdam", 78),
                                rep("Almere", 78),
                                rep("Utrecht", 78)))
df_age_inc_effects <- df_age %>%
    left_join(age_effects)
df_age_inc_effects$effect <- df_age_inc_effects$age * df_age_inc_effects$Age +
    df_age_inc_effects$age^2 * df_age_inc_effects$Age2
df_age_inc_effects$age <- as.numeric(df_age_inc_effects$age)

age_plot <- ggplot(df_age_inc_effects, aes(y = effect, x = age, color = Sample, shape = Sample)) +
    geom_point(size = 2.5) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() +
        scale_color_manual(values = munic_palette) +
        scale_y_continuous(name = "Average Policy Effect",
                       labels = scales::percent) +
        xlab("Age") +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T)


inc_effects <- group_reg_melt_est %>%
    filter(type == "Income")
inc_effects$term <- gsub("Income: | of.*", "", inc_effects$term)
inc_plot <- ggplot(inc_effects, aes(y = value, x = rep(1:40, 6), color = Sample, shape = Sample)) +
    geom_point(size = 2.5) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() +
        scale_color_manual(values = munic_palette) +
        scale_y_continuous(name = "Average Policy Effect",
                       labels = scales::percent) +
        scale_x_continuous(labels = paste0(c(seq(20, 380, 20), "400+"), "%"), breaks = seq(2,40, 2), "Household income") +
        cowplot::theme_cowplot() +
        custom_theme(hor = T, ver = T) +
            theme(axis.text.x = element_text(angle = 70, hjust = 1))


(((h_plot / hh_plot) +
    plot_layout(guides = "collect") & theme(legend.position = "top")) |
((inc_plot / age_plot) +
    plot_layout(guides = "collect") & theme(legend.position = "top"))) +
  plot_annotation(tag_levels = "A")

ggsave("./tex/figs/fig_group_regressions.pdf", last_plot(), width = 15, height = 12)

## Robustness: pre-treatment trends
r_pre_trends <- readxl::read_xlsx("./data/r_pre_2015_trends.xlsx")
ggplot(r_pre_trends, aes(y = use, x = year)) +
    geom_point() +
    geom_line() +
    geom_label(aes(label = paste0(round(use * 100, 2), "%")), vjust = -0.25) +
    scale_y_continuous(name = "Use of social assistance",
                       labels = scales::percent,
                       limits = c(0.05, 0.07)) +
    cowplot::theme_cowplot() +
    custom_theme(hor = T, ver = T) +
    xlab("Year")
ggsave("./tex/figs/fig_r_pre_2015_trends.pdf", last_plot(), width = 7, height = 7)
