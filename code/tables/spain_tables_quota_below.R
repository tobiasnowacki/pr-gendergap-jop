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
options("modelsummary_format_numeric_latex" = "plain")

# Load data in
rd_dat <- read.csv("data/spain_rd_ready.csv",
  row.names = NULL
) %>%
  filter(pop < 3000) %>%
  filter(abs(winmargin_loc) < 0.5) %>%
  filter(winmargin_loc != 0) %>%
  mutate(rank_diff = (rank_again - rank))

# Throw out cities that cross pop border...
city_cross <- rd_dat %>%
  select(city_id, year, pop) %>%
  distinct() %>%
  group_by(city_id) %>%
  arrange(city_id, year) %>%
  mutate(
    pop_next = lead(pop),
    crosses = case_when(
      pop < 3000 ~ pop_next > 3000,
      pop > 3000 ~ pop_next < 3000
    )
  ) %>%
  group_by(city_id) %>%
  summarise(ct = sum(crosses > 0, na.rm = TRUE))

rd_dat <- rd_dat %>%
  filter(city_id %in% city_cross$city_id[city_cross$ct == 0])

run_results_below <- run_het_rd(rd_dat,
  "run_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
  bw_fac = c(1)
)

win_results_below <- run_het_rd(rd_dat,
  "win_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
  bw_fac = c(1)
)

results <- list(
  run_results_below,
  win_results_below
) %>% flatten()


results_tidy <- map(results, ~ .x$out)
n_left <- map_dbl(results, ~ .x$n_left)
n_right <- map_dbl(results, ~ .x$n_right)
n_mean <- map_dbl(results, ~ .x$out_mean) %>% signif(3)
bwv <- map_dbl(results, ~ .x$bw) %>% signif(2)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc <- function(x) {
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}
names(results_tidy) <- paste0("(", 1:length(results_tidy), ")") %>% apply_mc()

row_mat <- matrix(
  ncol = length(n_left) + 1, byrow = TRUE,
  c(
    "Bandwidth", bwv,
    "Outcome Mean", n_mean,
    "N (left)", apply_mc(n_left),
    "N (right)", apply_mc(n_right)
  )
) %>%
  as.data.frame()

clist <- c(
  "treatTRUE" = "Elected",
  "female" = "Female",
  "treatTRUE:female" = "Elected x Female"
)

# This is adapted to Beamer Format
tex_out <- modelsummary(results_tidy,
  coef_map = clist,
  gof_omit = ".*",
  caption = "\\label{tab:spain_by_quota_below} Gender Gap in Incumbency Advantages, Spain, Non-Quota Municipalities (< 3,000 pop).",
  booktabs = TRUE,
  add_rows = row_mat,
  output = "latex"
) %>%
  kable_styling(
    font_size = 8,
    latex_options = "hold_position"
  ) %>%
  add_header_above(c(" " = 1, "Run (t+1)" = 1, "Win (t+1)" = 1)) %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:3, latex_column_spec = "S[
              input-symbols=(),
              table-format=-2.3,
              table-space-text-pre    = (,
              table-space-text-post   = ),
              input-open-uncertainty  =,
              input-close-uncertainty = ,
              table-align-text-post = false]") %>%
  gsub(pattern = "\\textbackslash{}", replacement = "\\", fixed = TRUE) %>%
  gsub(pattern = "\\{", replacement = "{", fixed = TRUE) %>%
  gsub(pattern = "\\}", replacement = "}", fixed = TRUE)

cat(tex_out, file = "output/tables/spain_by_quota_below.tex")