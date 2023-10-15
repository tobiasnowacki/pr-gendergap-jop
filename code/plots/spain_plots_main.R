library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/utils/het_by_sex_plot.R")

# Load data in
rd_dat <- read.csv("data/spain_rd_ready.csv",
    row.names = NULL
) %>%
    filter(year < 2019) %>%
    filter(pop < 250000) %>%
    filter(abs(winmargin_loc) < 0.05) %>%
    filter(major_party == TRUE) %>%
    filter(year != 2019) %>%
    filter(winmargin_loc != 0)

# Plotting running again on rd
out_run <- het_by_sex_plot(rd_dat %>% filter(major_party == TRUE), "run_again")

ggplot(out_run[[1]], aes(rdplot_mean_x, rdplot_mean_y)) +
    geom_point(aes(colour = sex), alpha = 0.3) +
    geom_line(
        data = out_run[[2]],
        aes(rdplot_x, rdplot_y,
            colour = sex,
            group = interaction(below, sex)
        ), lwd = 1.3
    ) +
    geom_vline(xintercept = 0, lty = "dashed", colour = "grey50") +
    theme_tn() +
    labs(
        x = "Minimal Distance to Winning Threshold",
        y = "Pr(Running in t+1)",
        colour = "Sex"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    scale_colour_manual(values = c("red", "dark blue")) +
    theme(
        axis.line = element_line(colour = "grey40"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
    )
ggsave("output/figures/spain_run_again.pdf",
    device = cairo_pdf,
    width = 4,
    height = 2.5
)

# Plotting winning again on rd
out_win <- het_by_sex_plot(rd_dat %>% filter(major_party == TRUE), "win_again")

ggplot(out_win[[1]], aes(rdplot_mean_x, rdplot_mean_y)) +
    geom_point(aes(colour = sex), alpha = 0.3) +
    # geom_smooth(aes(group = interaction(rdplot_mean_x < 0, sex),
    #                 colour = sex)) +
    geom_line(
        data = out_win[[2]],
        aes(rdplot_x, rdplot_y,
            colour = sex,
            group = interaction(below, sex)
        ), lwd = 1.3
    ) +
    geom_vline(xintercept = 0, lty = "dashed", colour = "grey50") +
    theme_tn() +
    labs(
        x = "Minimal Distance to Winning Threshold",
        y = "Pr(Winning in t+1)",
        colour = "Sex"
    ) +
    scale_colour_manual(values = c("red", "dark blue")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    theme(
        axis.line = element_line(colour = "grey40"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
    )
ggsave("output/figures/spain_win_again.pdf",
    device = cairo_pdf,
    width = 4,
    height = 2.5
)