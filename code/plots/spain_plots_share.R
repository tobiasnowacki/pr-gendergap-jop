library(tidyverse)
library(rio)
library(rdrobust)
library(lfe)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/utils/het_by_sex_plot.R")

# Load data in
rd_dat = read.csv("data/spain_rd_ready.csv",
                  row.names = NULL) %>%
    filter(year < 2019) %>%
    filter(pop < 250000) %>%
    filter(abs(winmargin_loc) < 0.05) %>%
    filter(major_party == TRUE) %>%
    filter(winmargin_loc != 0)

# Share female
p1 <- rdplot(
  x = rd_dat$winmargin_loc,
  y = rd_dat$female,
  p = 2
)

ggplot(p1$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y)) +
  geom_point(alpha = 0.4) +
  geom_line(
    data = p1$vars_poly %>% filter(rdplot_x != 0),
    aes(x = rdplot_x, y = rdplot_y, group = rdplot_x > 0),
    lwd = 1.5, colour = "blue"
  ) +
  geom_vline(xintercept = 0, lty = "dotted") +
  labs(
    x = "Minimal Distance to Winning Threshold",
    y = "Share Women"
  ) +
  theme_tn()

ggsave(
  "output/figures/spain_share_female.pdf",
  device = cairo_pdf,
  width = 4,
  height = 2.5
  )
