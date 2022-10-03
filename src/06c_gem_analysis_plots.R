library(tidyverse)
library(gridExtra)

source("./src/styles.R")

## Read data
df <- readxl::read_xlsx("./data/yearly_wmo_gem.xlsx")

## Turn NA into 0s
# df[is.na(df)] <- 0

wmo_palette <- MetBrewer::met.brewer("Hokusai1")[c(1, 2, 4, 5, 7)]

## -- Findings on NL level
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
    custom_theme(hor = T) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.1)) +
    ylab("Proportion use") + xlab("Year") + theme(legend.position = "top")

## -- Findings on municipality level

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
    custom_theme(hor = T) +
    scale_x_continuous(labels = scales::percent, name = "2019 use versus 2016 use") +
    ylab("Density") + theme(legend.position = "top")

(nl_type_plot + gem_type_plot) + plot_layout(guides = "collect") & theme(legend.position = 'top')

## Correlation plots
deltas <- list(
    "delta_wmo", "delta_wmo_hh", "delta_wmo_hm",
    "delta_wmo_on", "delta_wmo_vb"
)
names(wmo_palette) <- deltas

axes_title <- c("Any", "Household", "Services", "Support", "Internal")
names(axes_title) <- deltas

make_scatter <- function(data, x, axes_title, y = "delta_wmo") {
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
            geom_vline(xintercept =  0, linetype = "dashed")
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

