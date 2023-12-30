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
# Load data in
rd_dat <- read.csv("data/spain_rd_ready.csv",
    row.names = NULL
) %>%
    filter(pop < 250000) %>%
    filter(abs(winmargin_loc) < 0.5) %>%
    filter(major_party == TRUE) %>%
    filter(winmargin_loc != 0)

# Set formula
form_used <- "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(party_short):as.factor(year) | 0 | city_id"

# Set bandwidths
bw_seq <- seq(0.01, 0.15, by = 0.01)

# Running again
bw_run <- run_het_rd(rd_dat,
    "run_again",
    "winmargin_loc",
    form_used,
    bw_override = bw_seq
)

# Extract relevant coefficients
bw_run_df <- map_dfr(bw_run, ~ .x$out %>%
    tidy() %>%
    mutate(term = case_when(
        term == "treatTRUE" ~ "Elected",
        term == "treatTRUE:female" ~ "Elected x Female"
    )) %>%
    filter(term %in% c("Elected", "Elected x Female"))) %>%
    mutate(bw = rep(bw_seq, each = 2))

ggplot(bw_run_df, aes(bw, estimate)) +
    geom_pointrange(aes(
        ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error,
        colour = term, shape = term
    ),
    position = position_dodge(width = 0.005)
    ) +
    geom_hline(yintercept = 0, lty = "dotted", width = 0.5) +
    scale_colour_manual(values = c("dark red", "dark blue")) +
    theme_tn() +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
    labs(x = "Bandwidth", y = "Coefficient Estimate") +
    theme_tn()
ggsave("output/figures/spain_run_again_bw.pdf", device = cairo_pdf, width = 4, height = 2.5)


# Winning again
bw_win <- run_het_rd(rd_dat,
    "win_again",
    "winmargin_loc",
    form_used,
    bw_override = bw_seq
)

# Extract relevant coefficients
bw_win_df <- map_dfr(bw_win, ~ .x$out %>%
    tidy() %>%
    mutate(term = case_when(
        term == "treatTRUE" ~ "Elected",
        term == "treatTRUE:female" ~ "Elected x Female"
    )) %>%
    filter(term %in% c("Elected", "Elected x Female"))) %>%
    mutate(bw = rep(bw_seq, each = 2))

ggplot(bw_win_df, aes(bw, estimate)) +
    geom_pointrange(aes(
        ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error,
        colour = term, shape = term
    ),
    position = position_dodge(width = 0.005)
    ) +
    geom_hline(yintercept = 0, lty = "dotted", width = 0.5) +
    scale_colour_manual(values = c("dark red", "dark blue")) +
    theme_tn() +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
    labs(x = "Bandwidth", y = "Coefficient Estimate") +
    theme_tn()
ggsave("output/figures/spain_win_again_bw.pdf", device = cairo_pdf, width = 4, height = 2.5)
