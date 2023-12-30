library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(rddensity)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

# Load data
rd_dat = read.csv("data/no_rd.csv") %>%
  filter(abs(winmargin_loc) < 0.5, winmargin_loc != 0,
         year < 2019,
         pop_loc < 250000,
         kname_loc != "Oslo") %>%
  filter(inconsistency_candidate_loc == 0) %>%
  filter(party_loc %in% c("a", "frp", "h", "krf", "sp", "sv", "v"))

dplot = rdplotdensity(rdd = rddensity(rd_dat$winmargin_loc),
              X = rd_dat$winmargin_loc,
              CItype = "none",
              CIsimul = 10000,
              CIuniform = FALSE,
              plotN = c(20, 20),
              plotRange = c(-0.05, 0.05),
              histBreaks = seq(-0.05, 0.05, by = 0.0025),
              type = "both")$Estplot +
    geom_vline(xintercept = 0, lty = "dashed") +
    theme_tn()

ggsave(dplot,
       file = "output/figures/norway_density.pdf",
       width = 4,
       height = 2.5,
       device = cairo_pdf)
