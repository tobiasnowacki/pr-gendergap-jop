library(tidyverse)
library(rio)
library(rdrobust)
library(fabricatr)
library(lfe)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/utils/run_het_rd.R")

# Load data in
rd_dat <- read.csv("data/no_rd.csv") %>%
    filter(
        abs(winmargin_loc) < 0.5, winmargin_loc != 0,
        year < 2019,
        pop_loc < 250000,
        kname_loc != "Oslo"
    ) %>%
    filter(inconsistency_candidate_loc == 0) %>%
    filter(party_loc %in% c("a", "frp", "h", "krf", "sp", "sv", "v")) %>%
    mutate(pop_terc = split_quantile(pop_loc, 3))

# Function that takes upper pop threshold
result_pop_terc <- function(data, tercile) {
    trunc_dat <- data %>% filter(pop_terc == tercile)

    run_results <- run_het_rd(trunc_dat,
        "run_again",
        "winmargin_loc",
        "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(cnr_loc) + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc",
        bw_fac = c(1)
    )

    win_results <- run_het_rd(trunc_dat,
        "win_again",
        "winmargin_loc",
        "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(cnr_loc) + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc",
        bw_fac = c(1)
    )

    tab <- tibble(
        thresh = tercile,
        outcome = c("run_again", "win_again"),
        coef = c(run_results[[1]]$out$coef[6], win_results[[1]]$out$coef[6]),
        se = c(run_results[[1]]$out$se[6], win_results[[1]]$out$se[6]),
        ymin = coef - 1.96 * se,
        ymax = coef + 1.96 * se
    )

    return(tab)
}

thresh_tbl <- map_dfr(c(1:3), ~ result_pop_terc(rd_dat, .x))

thresh_tbl <- thresh_tbl %>%
    mutate(outcome = case_when(
        outcome == "run_again" ~ "Run (t+1)",
        outcome == "win_again" ~ "Win (t+1)"
    ))

ggplot(thresh_tbl, aes(x = as.factor(thresh))) +
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax, colour = outcome), width = 0.2) +
    geom_point(aes(y = coef, colour = outcome)) +
    facet_wrap(~outcome) +
    scale_colour_viridis_d(end = 0.7) +
    labs(x = "Population Tercile", y = "Gender Gap Coefficient") +
    theme_tn() +
    guides(colour = FALSE)

ggsave(
    "output/figures/norway_tercile_robust.pdf",
    device = cairo_pdf,
    width = 6,
    height = 3
)
