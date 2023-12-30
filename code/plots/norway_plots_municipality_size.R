library(tidyverse)
library(rio)
library(rdrobust)
library(lfe)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/utils/run_het_rd.R")

# Load data
rd_dat <- read.csv("data/no_rd.csv") %>%
    filter(
        abs(winmargin_loc) < 0.5, winmargin_loc != 0,
        year < 2019,
        pop_loc < 250000,
        kname_loc != "Oslo"
    ) %>%
    filter(inconsistency_candidate_loc == 0) %>%
    filter(party_loc %in% c("a", "frp", "h", "krf", "sp", "sv", "v"))

# Function that takes upper pop threshold
result_pop <- function(data, pop_limit) {
    trunc_dat <- data %>% filter(pop_loc < pop_limit)

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
        thresh = pop_limit,
        outcome = c("run_again", "win_again"),
        coef = c(run_results[[1]]$out$coef[6], win_results[[1]]$out$coef[6]),
        se = c(run_results[[1]]$out$se[6], win_results[[1]]$out$se[6]),
        ymin = coef - 1.96 * se,
        ymax = coef + 1.96 * se,
        share_women = mean(trunc_dat$female)
    )

    return(tab)
}


thresh_tbl <- map_dfr(seq(10000, 200000, by = 10000), ~ result_pop(rd_dat, .x))

thresh_tbl <- thresh_tbl %>%
    mutate(outcome = case_when(
        outcome == "run_again" ~ "Run (t+1)",
        outcome == "win_again" ~ "Win (t+1)"
    ))

ggplot(thresh_tbl, aes(x = thresh)) +
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_errorbar(aes(ymin = ymin, ymax = ymax, colour = outcome)) +
    geom_point(aes(y = coef, colour = outcome)) +
    facet_wrap(~outcome) +
    scale_colour_viridis_d(end = 0.7) +
    labs(x = "Sample Population Upper Bound", y = "Gender Gap Coefficient") +
    theme_tn() +
    guides(colour = FALSE)

ggsave(
    "output/figures/norway_muni_robust.pdf",
    device = cairo_pdf,
    width = 6,
    height = 3
)
