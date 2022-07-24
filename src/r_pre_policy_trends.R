library(tidyverse)

## AWBZ analysis

df_2010 <- readRDS("data/raw/awbz_2010.rds")
df_2011 <- readRDS("data/raw/awbz_2011.rds")
df_2012 <- readRDS("data/raw/awbz_2012.rds")
df_2013 <- readRDS("data/raw/awbz_2013.rds")
df_2014 <- readRDS("data/raw/awbz_2014.rds")

rin_10 <- df_2010 %>% pull(rinpersoon) %>% unique()
rin_11 <- df_2011 %>% pull(rinpersoon) %>% unique()
rin_12 <- df_2012 %>% pull(rinpersoon) %>% unique()
rin_13 <- df_2013 %>% pull(rinpersoon) %>% unique()
rin_14 <- df_2014 %>% pull(rinpersoon) %>% unique()
pop_counts <- c(16.58, 16.66, 16.73, 16.78, 16.83) * 1000000
use <- c(length(rin_10) / pop_counts[1],
         length(rin_11) / pop_counts[2],
         length(rin_12) / pop_counts[3],
         length(rin_13) / pop_counts[4],
         length(rin_14) / pop_counts[5])
df_use_awbz <- data.frame(use = use, year = 2010:2014,
                          n = c(length(rin_10),
                                length(rin_11),
                                length(rin_12),
                                length(rin_13),
                                length(rin_14)
                                ))
writexl::write_xlsx(df_use_awbz, "output/220711_Output_WMO3/r_pre_2015_trends.xlsx")

ggplot(df_use_awbz, aes(y = use, x = year)) + geom_point() + geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.1))