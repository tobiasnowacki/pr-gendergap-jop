library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(lfe)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

options(modelsummary_format_numeric_latex = "plain")

source("code/utils/run_het_rd.R")

# Load data in
# Load data in
rd_dat = read.csv("data/spain_rd_ready.csv",
                  row.names = NULL) %>%
    filter(year < 2019) %>%
    filter(pop < 250000) %>%
    filter(abs(winmargin_loc) < 0.5) %>%
    filter(major_party == TRUE) %>%
    filter(winmargin_loc != 0)

left = "psoe"
right = "pp"

run_results_left = run_het_rd(rd_dat %>% filter(party_short %in% left),
           "run_again",
           "winmargin_loc",
           "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short) | 0 | city_id",
           bw_fac = c(1))

run_results_right = run_het_rd(rd_dat %>% filter(party_short %in% right),
           "run_again",
           "winmargin_loc",
           "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short) | 0 | city_id",
           bw_fac = c(1))

win_results_left = run_het_rd(rd_dat %>% filter(party_short %in% left),
           "win_again",
           "winmargin_loc",
           "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
           bw_fac = c(1))

win_results_right = run_het_rd(rd_dat %>% filter(party_short %in% right),
           "win_again",
           "winmargin_loc",
           "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
           bw_fac = c(1))

results_out = list(run_results_left, run_results_right, win_results_left, win_results_right) %>% flatten

results_tidy = map(results_out, ~ .x$out)
modelsummary(results_tidy)
n_left = map_dbl(results_out, ~ .x$n_left)
n_right = map_dbl(results_out, ~ .x$n_right)
n_mean = map_dbl(results_out, ~.x$out_mean) %>% signif(3)
bwv = map_dbl(results_out, ~.x$bw) %>% signif(2)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc = function(x){
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}
names(results_tidy) = paste0("(", 1:length(results_tidy), ")") %>% apply_mc

row_mat = matrix(ncol = length(n_left) + 1, byrow = TRUE,
                 c("Parties", rep(c("Left", "Right"), 2) %>% apply_mc,
                  "Bandwidth", bwv,
                 "Outcome Mean", n_mean,
                 "N (left)", apply_mc(n_left),
                 "N (right)", apply_mc(n_right))) %>%
          as.data.frame

clist = c("treatTRUE" = "Elected",
          "female" = "Female",
          "treatTRUE:female" = "Elected x Female")

tex_out = modelsummary(results_tidy,
             coef_map = clist,
             gof_omit = ".*",
             caption = "\\label{tab:spain_by_party} \\textbf{Heterogeneity-in-DiscontinuityEstimates For Incumbency Advantage In Spanish Municipalities, By Political Party Group.} The estimates for the gender gap are similar in both left- and right-wing parties.",
             booktabs = TRUE,
             add_rows = row_mat,
             output = "latex") %>%
  kable_styling(
    latex_options = "hold_position",
    font_size = 9
  ) %>%
  footnote(
      threeparttable = TRUE,
      fixed_small_size = FALSE,
      footnote_as_chunk = TRUE,
      general_title = "",
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Other coefficients not reported. Regression run on all candidates in elections between 2003 and 2015.")) %>%
  add_header_above(c(" " = 1, "Run (t + 1)" = 2, "Win (t + 1)" = 2)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:5, latex_column_spec = "S[
              input-symbols=(),
              table-format=-1.3,
              table-space-text-pre    = (,
              table-space-text-post   = ),
              input-open-uncertainty  =,
              input-close-uncertainty = ,
              table-align-text-post = false]") %>%
  gsub(pattern = "\\textbackslash{}", replacement= "\\", fixed = TRUE) %>%
  gsub(pattern = "\\{", replacement= "{", fixed = TRUE) %>%
  gsub(pattern = "\\}", replacement= "}", fixed = TRUE)

cat(tex_out, file = "output/tables/spain_tables_by_party.tex")
