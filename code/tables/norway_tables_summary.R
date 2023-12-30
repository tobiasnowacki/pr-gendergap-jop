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
options("modelsummary_format_numeric_latex" = "plain")

right = c("h", "frp", "krf", "v")

# Load data in
# Load data
rd_dat <- read.csv("data/no_rd.csv") %>%
  filter(
    abs(winmargin_loc) < 0.05,
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo"
  ) %>%
  filter(inconsistency_candidate_loc == 0) %>%
  mutate(
    elected = as.numeric(elected_loc),
    run_again = as.numeric(run_again),
    win_again = as.numeric(win_again)
  ) %>%
  dplyr::select(
    "Share Female" = female,
    "Elected" = elected,
    Population = pop_loc,
    `Total Seats` = sizeofcouncil_loc,
    `Running (t+1)` = run_again,
    `Winning (t+1)` = win_again
  )

out <- datasummary(`Share Female` + Elected + Population + `Total Seats` + `Running (t+1)` + `Winning (t+1)` ~ (Mean) + (SD) + (Min) + (Max),
  data = rd_dat,
  title = "Summary Statistics For Borderline Sample, Norway.",
  output = "output/tables/norway_summary.tex"
)

# Male-Female Balance in Norway
rd_dat <- read.csv("data/no_rd.csv") %>%
  filter(
    abs(winmargin_loc) < 0.05,
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo"
  ) %>%
  filter(inconsistency_candidate_loc == 0) %>%
  mutate(
    female = ifelse(female == 1, "Female", "Male"),
    elected = as.numeric(elected_loc),
    run_again = as.numeric(run_again),
    win_again = as.numeric(win_again)
  ) %>%
  dplyr::select(winmargin_loc, elected, female, pop_loc, age, sizeofcouncil_loc, run_again, win_again, no_run, candwin_loc,  personalvotes_total_loc, rank_loc, rankresult_loc, preadvantage_loc) %>%
  rename(
    Margin = winmargin_loc,
    Elected = elected,
    Population = pop_loc, `Council size` = sizeofcouncil_loc, `Run (t + 1)` = run_again, `Win (t + 1)` = win_again, `Times Running` = no_run, `Times Winning` = candwin_loc, Age = age, `Personal Votes` = personalvotes_total_loc, `List rank` = rank_loc, `Final list rank` = rankresult_loc, `Pre-Advantage` = preadvantage_loc
  )

# Bandwidth of h = 0.1
datasummary_balance(~female,
  rd_dat,
  caption = "Gender Balance Across Covariates",
  fmt = "%.2f"
  # output = "output/tables/norway_female_balance.tex"
)

Diff <- function(x) mean(x[rd_dat$female == "Female"]) - mean(x[rd_dat$female == "Male"])

DiffSE <- function(x) t.test(x[rd_dat$female == "Female"], x[rd_dat$female == "Male"])$stderr %>% sprintf("(%.2f)", .)

DiffPval <- function(x) t.test(x[rd_dat$female == "Female"], x[rd_dat$female == "Male"])$p.val %>% sprintf("[%.3f]", .)

datasummary(
  All(rd_dat) ~ female * (Mean + SD * Arguments(fmt = "(%.2f)")) + (Difference = Diff) + (`Diff SE` = DiffSE) + (`p-val` = DiffPval),
  data = rd_dat, output = "latex"
) %>%
  row_spec(5, extra_latex_after = "\\cmidrule(lr){2-8}") %>%
  column_spec(c(2, 4, 6), bold = TRUE) %>%
  column_spec(6:8, background = "#eeeeee") %>%
  save_kable(file = "output/tables/norway_female_balance_beamer.tex")


# Do this for the whole election sample
rd_all <- read.csv("data/no_all.csv") %>%
  filter(
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo"
  ) %>%
  filter(inconsistency_candidate_loc == 0) %>%
  mutate(
    female = ifelse(female == 1, "Female", "Male"),
    elected = as.numeric(elected_loc),
    run_again = as.numeric(run_again),
    win_again = as.numeric(win_again)
  ) %>%
  dplyr::select(winmargin_loc, elected, female, pop_loc, age, sizeofcouncil_loc, run_again, win_again, no_run, candwin_loc,  personalvotes_total_loc, rank_loc, rankresult_loc, preadvantage_loc) %>%
  rename(
    Margin = winmargin_loc,
    Elected = elected,
    Population = pop_loc, `Council size` = sizeofcouncil_loc, `Run (t + 1)` = run_again, `Win (t + 1)` = win_again, `Times Running` = no_run, `Times Winning` = candwin_loc, Age = age, `Personal Votes` = personalvotes_total_loc, `List rank` = rank_loc, `Final list rank` = rankresult_loc, `Pre-Advantage` = preadvantage_loc
  )

rd_all %>% filter(Elected == TRUE) %>% group_by(female) %>% summarise(mean(`Win (t + 1)`))

# Recast functions in terms of whole sample
Diff <- function(x) mean(x[rd_all$female == "Female"]) - mean(x[rd_all$female == "Male"])

DiffSE <- function(x) t.test(x[rd_all$female == "Female"], x[rd_all$female == "Male"])$stderr %>% sprintf("(%.2f)", .)

DiffPval <- function(x) t.test(x[rd_all$female == "Female"], x[rd_all$female == "Male"])$p.val %>% sprintf("[%.3f]", .)

datasummary(
  All(rd_all) ~ female * (Mean + SD * Arguments(fmt = "(%.2f)")) + (Difference = Diff) + (`Diff SE` = DiffSE) + (`p-val` = DiffPval),
  data = rd_all, output = "latex"
) %>%
  row_spec(5, extra_latex_after = "\\cmidrule(lr){2-8}") %>%
  column_spec(c(2, 4, 6), bold = TRUE) %>%
  column_spec(6:8, background = "#eeeeee") %>%
  save_kable(file = "output/tables/norway_female_balance_beamer_full.tex")
