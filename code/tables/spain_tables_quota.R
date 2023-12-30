library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(lfe)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
options("modelsummary_format_numeric_latex" = "plain")


source("code/utils/run_het_rd.R")

# Load data in
rd_dat <- read.csv("data/spain_rd_ready.csv",
  row.names = NULL
) %>%
  filter(pop < 20000) %>%
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

# # Code numeric female indicator
# rd_dat = rd_dat %>%
#     mutate(female = case_when(
#     sex_imputed == "F" ~ 1,
#     sex_imputed == "M" ~ 0,
#     ),
#     treat = winmargin_loc > 0) %>%
#     filter(!is.na(female)) %>%
#     group_by(province_id, municipality_id) %>%
#     mutate(city_id = cur_group_id()) %>%
#     group_by(province_id, municipality_id, year) %>%
#     mutate(boot_id = cur_group_id()) %>%
#     rename(winmargin_loc = winmargin_loc)


run_results_below <- run_het_rd(rd_dat %>% filter(pop %in% 250:3000, year > 2007),
  "run_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
  bw_fac = c(1)
)

run_results_above <- run_het_rd(rd_dat %>% filter(pop %in% 3001:6000, year > 2007),
  "run_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
  bw_fac = c(1)
)

# run_results_before = run_het_rd(rd_dat %>% filter(pop < 5000, year < 2007),
#            "run_again",
#            "winmargin_loc",
#            "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(city_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
#            bw_fac = c(1))

win_results_below <- run_het_rd(rd_dat %>% filter(pop %in% 250:3000, year > 2007),
  "win_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
  bw_fac = c(1)
)

win_results_above <- run_het_rd(rd_dat %>% filter(pop %in% 3001:6000, year > 2007),
  "win_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
  bw_fac = c(1)
)

dd_df <- rd_dat %>%
  filter(abs(winmargin_loc) < 0.01) %>%
  filter(pop %in% 1000:5000, year > 2007)

felm(win_again ~ treat + female + treat:female + I(pop > 3000) + treat:I(pop > 3000) + female:I(pop > 3000) + treat:female:I(pop > 3000) | as.factor(province_id) + as.factor(year) + as.factor(year):as.factor(party_short) | 0 | city_id, data = dd_df) %>% summary()

rd_df1 <- rd_dat %>%
  filter(abs(winmargin_loc) < 0.05) %>%
  filter(pop %in% 1000:5000, year > 2007) %>%
  filter(treat == FALSE & female == 1)

rdplot(x = rd_df1$pop, y = rd_df1$win_again, c = 3000, p = 1)
rdrobust(x = rd_df1$pop, y = rd_df1$win_again, c = 3000, p = 1) %>% summary()



# win_results_before = run_het_rd(rd_dat %>% filter(pop < 5000, year < 2007),
#            "win_again",
#            "winmargin_loc",
#            "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(city_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
#            bw_fac = c(1))

# rank_results = run_het_rd(rd_dat,
#            "rank_again",
#            "winmargin_loc",
#            "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(city_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id")

results <- list(
  run_results_below,
  run_results_above,
  win_results_below,
  win_results_above
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
    "Pop", apply_mc(rep(c("0-3k", "3-6k"), times = 2)),
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

tex_out <- modelsummary(results_tidy,
  coef_map = clist,
  gof_omit = ".*",
  caption = "\\label{tab:spain_by_quota} \\textbf{Heterogeneity-in-DiscontinuityEstimates For Incumbency Advantage In Spanish Municipalities, By Quota Law.} The estimates for the gender gap are similar in municipalities with and without the gender quota on lists.",
  booktabs = TRUE,
  add_rows = row_mat,
  output = "latex"
) %>%
  kable_styling(
    font_size = 9,
    latex_options = "hold_position"
  ) %>%
  footnote(
    threeparttable = TRUE,
    fixed_small_size = FALSE,
    footnote_as_chunk = TRUE,
    general_title = "",
    general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Other coefficients not reported. `Pop' indicates the population of a candidate's municipality at time $t$. Regression run on all candidates in elections between 2011 and 2015. Observations in municipalities that crossed the 3,000 population threshold in between elections are excluded.")
  ) %>%
  add_header_above(c(" " = 1, "Run (t+1)" = 2, "Win (t+1)" = 2)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:5, latex_column_spec = "S[
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

cat(tex_out, file = "output/tables/spain_by_quota.tex")