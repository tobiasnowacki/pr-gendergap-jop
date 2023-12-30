library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(broom)
library(viridis)
library(scales)
library(rddensity)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

rd_dat = read.csv("data/spain_rd_ready.csv",
                  row.names = NULL) %>%
    filter(year < 2019) %>%
    filter(pop < 250000) %>%
    filter(abs(winmargin_loc) < 0.5) %>%
    filter(major_party == TRUE) %>%
    filter(winmargin_loc != 0)

dplot = rdplotdensity(rdd = rddensity(rd_dat$winmargin_loc),
              X = rd_dat$winmargin_loc,
              # CItype = "none",
              plotN = c(20, 20),
              plotRange = c(-0.05, 0.05),
              histBreaks = seq(-0.05, 0.05, by = 0.0025),
              type = "both")$Estplot +
    geom_vline(xintercept = 0, lty = "dashed") +
    theme_tn()

ggsave(dplot,
       file = "output/figures/spain_density.pdf",
       width = 4,
       height = 2.5,
       device = cairo_pdf)
