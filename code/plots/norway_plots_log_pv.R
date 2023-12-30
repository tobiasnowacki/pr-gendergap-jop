library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(lfe)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/utils/run_het_rd.R")

library(devtools)

left <- c("a", "sv")
right = c("h", "frp", "krf", "v")

# Load data
rd_dat <- read.csv("data/no_rd.csv") %>%
  filter(
    abs(winmargin_loc) < 0.05,
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo"
  ) %>%
  filter(inconsistency_candidate_loc == 0)

# Change Gender label
rd_dat <- rd_dat %>%
  mutate(Gender = ifelse(female == 0, "Male", "Female"))

# Run plots
ggplot(rd_dat %>% filter(treat == 1 & run_again == TRUE)) +
  geom_density(aes(
    x = log(personalvotes_total_loc),
    fill = as.factor(Gender)
  ),
  alpha = 0.3
  ) +
  scale_fill_viridis_d(end = 0.7) +
  labs(
    x = "log(Preference Votes), t",
    y = "Density",
    fill = "Gender"
  ) +
  theme_tn()
ggsave("output/figures/pv_density_pre.pdf",
  device = cairo_pdf,
  width = 3.5, height = 3
)

ggplot(rd_dat %>% filter(treat == 1 & run_again == TRUE)) +
  geom_density(aes(
    x = log(pv_again),
    fill = as.factor(Gender)
  ),
  alpha = 0.3
  ) +
  scale_fill_viridis_d(end = 0.7) +
  labs(
    x = "log(Preference Votes), t + 1",
    y = "Density",
    fill = "Gender"
  ) +
  theme_tn()

ggsave("output/figures/pv_density_post.pdf",
  device = cairo_pdf,
  width = 3.5, height = 3
)

# Run plots
ggplot(rd_dat %>% filter(treat == 1 & run_again == TRUE)) +
  geom_density(aes(
    x = rankresult_loc,
    fill = as.factor(Gender)
  ),
  alpha = 0.3
  ) +
  scale_fill_viridis_d(end = 0.7) +
  labs(
    x = "Resulting Rank, t",
    y = "Density",
    fill = "Gender"
  ) +
  theme_tn()
ggsave("output/figures/rankresult_pre.pdf",
  device = cairo_pdf,
  width = 3.5, height = 3
)

ggplot(rd_dat %>% filter(treat == 1 & run_again == TRUE)) +
  geom_density(aes(
    x = post_rank_again,
    fill = as.factor(Gender)
  ),
  alpha = 0.3
  ) +
  scale_fill_viridis_d(end = 0.7) +
  labs(
    x = "Resulting Rank, t + 1",
    y = "Density",
    fill = "Gender"
  ) +
  theme_tn()

ggsave("output/figures/rankresult_post.pdf",
  device = cairo_pdf,
  width = 3.5, height = 3
)