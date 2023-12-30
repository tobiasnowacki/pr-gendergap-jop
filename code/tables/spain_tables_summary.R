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
options(modelsummary_format_numeric_latex = "plain")

# Load data in
rd_dat <- read.csv("data/spain_rd_ready.csv",
       row.names = NULL
) %>%
       filter(year < 2019) %>%
       filter(pop < 250000) %>%
       filter(abs(winmargin_loc) < 0.05) %>%
       filter(major_party == TRUE) %>%
       filter(winmargin_loc != 0) %>%
       mutate(
              elected = as.numeric(elected == "S"),
              run_again = as.numeric(run_again),
              win_again = as.numeric(win_again)
       ) %>%
       dplyr::select(
              "Share Female" = female,
              "Elected" = elected,
              Population = pop,
              `Total Seats` = total_seats,
              `Running (t+1)` = run_again,
              `Winning (t+1)` = win_again
       )

out <- datasummary(
       `Share Female` + Elected + Population + `Total Seats` + `Running (t+1)` + `Winning (t+1)` ~ (Mean <- mean) + (SD <- sd) + (Min <- min) + (Max <- max),
       data = rd_dat,
       title = "Summary Statistics For Borderline Sample, Spain.",
       output = "output/tables/spain_summary.tex"
)
