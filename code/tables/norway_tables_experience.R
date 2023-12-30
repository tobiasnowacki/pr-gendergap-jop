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

# Load data
rd_dat = read.csv("data/no_rd.csv") %>%
  filter(abs(winmargin_loc) < 0.5, winmargin_loc != 0,
         year < 2019,
         pop_loc < 250000,
         kname_loc != "Oslo") %>%
  filter(inconsistency_candidate_loc == 0) %>%
  filter(party_loc %in% c("a", "frp", "h", "krf", "sp", "sv", "v")) %>%
  mutate(exper = no_run > 1)

run_results = run_het_rd(rd_dat,
           "run_again",
           "winmargin_loc",
           "winmargin_loc + treat + exper + winmargin_loc:treat + winmargin_loc:exper + treat:exper + winmargin_loc:treat:exper | as.factor(cnr_loc) + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc")

win_results = run_het_rd(rd_dat,
           "win_again",
           "winmargin_loc",
           "winmargin_loc + treat + exper + winmargin_loc:treat + winmargin_loc:exper + treat:exper + winmargin_loc:treat:exper | as.factor(cnr_loc) + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc")

results_out = append(run_results, win_results)

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
                 c("Bandwidth", bwv,
                 "BW Type", rep(c("Optimal", "2x Opt", "0.5x Opt"), 2) %>% apply_mc,
                 "Outcome Mean", n_mean,
                 "N (left)", apply_mc(n_left),
                 "N (right)", apply_mc(n_right))) %>%
          as.data.frame

clist = c("treatTRUE" = "Elected",
          "experTRUE" = "Experience",
          "treatTRUE:experTRUE" = "Elected x Experience")


tex_out = modelsummary(results_tidy,
             coef_map = clist,
             gof_omit = ".*",
             caption = "\\label{tab:norway_by_experience} \\textbf{Heterogeneity-in-DiscontinuityEstimates For Incumbency Advantage In Norwegian Municipalities, By Political Experience.} No Difference Between First-Time And Multiple-Time Candidates.",
             booktabs = TRUE,
             add_rows = row_mat,
             output = "latex") %>%
 kable_styling(
    # latex_options="scale_down"
    latex_options = "hold_position",
    font_size = 9
  ) %>%
  footnote(
      threeparttable = TRUE,
      fixed_small_size = FALSE,
      footnote_as_chunk = TRUE,
      general_title = "",
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Other coefficients not reported. Regression run on all candidates in elections between 2003 and 2015.")) %>%
  add_header_above(c(" " = 1, "Run (t + 1)" = 3, "Win (t + 1)" = 3)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:7, latex_column_spec = "S[
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

cat(tex_out, file = "output/tables/norway_by_experience.tex")