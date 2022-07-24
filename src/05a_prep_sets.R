df_analysis <- readRDS("data/edit/df_analysis_2016_2019_inc.rds")
df_analysis <- df_analysis %>%
  filter(leeftijd >= 18)

df_analysis$leeftijd <- round(df_analysis$leeftijd / 2)*2
df_analysis$lower_bound_num <- ifelse(df_analysis$lower_bound_num > 0,
                                      round(df_analysis$lower_bound_num / 5) * 5,
                                      df_analysis$lower_bound_num)

df_analysis$huishoudsamenstelling <- ifelse(df_analysis$huishoudsamenstelling %in%
                                              c("Institutioneel huishouden", "Onbekend"), "Inst_Onbekend",
                                            as.character(df_analysis$huishoudsamenstelling))

## Merge all very small muicipalities (< 25,000 inhabitants)
small_gms <- names(table(df_analysis$gem_2019))[(table(df_analysis$gem_2019) < 25000)]
df_analysis$gem_2019 <- as.character(df_analysis$gem_2019)

df_analysis$gem_2019[df_analysis$gem_2019 %in% small_gms] <- "small"

## Encode
df_analysis_dummy <- df_analysis %>%
  mutate(geslacht = ifelse(geslacht == "vrouw", 1, 0)) %>%
  fastDummies::dummy_cols(select_columns = c("herkomst", "huishoudsamenstelling"))

names(df_analysis_dummy) <- case_when(
  grepl("herkomst_", names(df_analysis_dummy)) ~ gsub("herkomst_", "H_", names(df_analysis_dummy)),
  grepl("huishoudsamenstelling_", names(df_analysis_dummy)) ~ gsub("huishoudsamenstelling_", "HH_", names(df_analysis_dummy)),
  TRUE ~ names(df_analysis_dummy)
)

names(df_analysis_dummy) <- gsub("lower_bound_num", "Income_SM", names(df_analysis_dummy))

df_analysis_dummy <- df_analysis_dummy %>%
  dplyr::select(-huishoudsamenstelling, -inkomen_klasse, -herkomst, -contains("wmo_"), -year, -wc_2019, -percsm,
                -inkomen_pers, -wc, -bc)

df_analysis_sample <- df_analysis_dummy %>%
  rename(H_Niet_Westers = `H_Niet-Westers`,
         Geslacht = geslacht)
saveRDS(df_analysis_sample, "data/final/ct_analysis_set.rds")