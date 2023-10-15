library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/utils/run_het_rd.R")

data <- tibble(
    Country = c("Norway (open-list)", "Spain (closed-list)", "Poland (open-list)",
                "Norway (open-list)", "Spain (closed-list)", "Poland (open-list)"),
    Outcome = c(rep("Run again (t+1)", 3),
                rep("Win again (t+1)", 3)),
    GenderGap = c(0.002, 0.037, 0.030, -0.062, 0.058, -0.069),
    SE = c(0.033, 0.023, 0.065, 0.028, 0.019, 0.057),
    ymin = GenderGap - 1.96 * SE,
    ymax = GenderGap + 1.96 * SE,
    type = rep(c("Main", "Main", "Supplementary"), 2)
)

data$Country = factor(data$Country,
    levels = rev(c("Norway (open-list)", "Spain (closed-list)", "Poland (open-list)"))
)

ggplot(data, aes(x = Country)) +
    geom_point(aes(
        y = GenderGap,
        colour = type
    )) +
    geom_errorbar(
        aes(
            ymin = ymin,
            ymax = ymax,
            colour = type
        ),
        width = 0
    ) +
    geom_hline(
        yintercept = 0,
        lty = "dotted",
        colour = "grey60"
    ) +
    coord_flip() +
    labs(y = "Gender Gap (pp)") +
    facet_wrap(Outcome ~ ., nrow = 1) +
    scale_colour_manual(values = c("red", "grey50")) +
    theme_tn() + theme(legend.title = element_blank())

ggsave("output/figures/coef_summary.pdf",
    device = cairo_pdf,
    width = 6, height = 2.5
)
