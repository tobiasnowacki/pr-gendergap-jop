library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
theme_set(theme_tn())

source("code/utils/run_het_rd.R")

# Load data
all_dat = read.csv("data/no_all.csv") %>%
  filter(abs(winmargin_loc) < 0.5, winmargin_loc != 0,
         pop_loc < 250000,
         kname_loc != "Oslo") %>%
  filter(inconsistency_candidate_loc == 0,
    missing_votes_municipality_loc == 0)

#
votes_by_prerank <- all_dat %>%
    filter(sizeofcouncil_loc < 70 & sizeofcouncil_loc > 10) %>%
    group_by(sizeofcouncil_loc, rank_loc) %>%
    summarise(mean_pv = mean(personalvotes_total_loc, na.rm = TRUE),
              pct_elected = mean(elected_loc == 1, na.rm = TRUE),
              count = n())

fem_by_prerank <- all_dat %>%
    filter(sizeofcouncil_loc < 70 & sizeofcouncil_loc > 33) %>%
    group_by(sizeofcouncil_loc, rank_loc) %>%
    summarise(mean_pv = mean(female, na.rm = TRUE))

# how many obs per council size rank?
all_dat %>%
    filter(sizeofcouncil_loc < 70) %>%
    group_by(sizeofcouncil_loc) %>%
    summarise(count = n_distinct(knr_loc),
              mean_pop = mean(pop_loc)) %>%
    print(n = 60)

ggplot(votes_by_prerank,
    aes(x = rank_loc)) +
    geom_line(aes(y = mean_pv),
                  alpha = 1) +
    # geom_line(aes(y = mean_pv,
    #               colour = as.factor(female)),
    #               alpha = 1) +
    labs(x = "Pre-Vote Rank Position", y = "Personal Votes") +
    facet_wrap(. ~ sizeofcouncil_loc, scales = "free")

ggsave("output/figures/norway_pv_by_rank.pdf",
    width = 8, height = 6,
    device = cairo_pdf)
