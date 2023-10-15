library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(lfe)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/utils/run_het_rd.R")
options(modelsummary_format_numeric_latex = "plain")

# Define which parties belong to which group
left = c("a", "sv")
right = c("h", "frp", "krf", "v")

# Load data
rd_dat = read.csv("data/no_rd.csv") %>%
  filter(abs(winmargin_loc) < 0.5,
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo") %>%
  filter(inconsistency_candidate_loc == 0, party_loc %in% c(left, right))

# Subset to municipalities that record full personal votes
rd_dat_votes <- rd_dat %>%
  filter(missing_votes_municipality_loc == 0) %>%
  mutate(pv_log = log(pv_again + 1))

# I'm measuring these unconditionally
rd_dat$rank_advanced[is.na(rd_dat$rank_advanced)] = 0
rd_dat$rank_postadvanced[is.na(rd_dat$rank_postadvanced)] = 0

# Set estimation formula
form_used = "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(cnr_loc)  + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc"

# Original rank advances
run_left = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "rank_advanced",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
run_right = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "rank_advanced",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Actual rank advances
win_left = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "rank_postadvanced",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
win_right = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "rank_postadvanced",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Preadvantage in t+1
rank_left = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "pread_again",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
rank_right = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "pread_again",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Personal Vote in t+1
pv_left = run_het_rd(rd_dat_votes %>% filter(party_loc %in% left),
           "pv_share",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
pv_right = run_het_rd(rd_dat_votes %>% filter(party_loc %in% right),
           "pv_share",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Wrangle results into table for export
results = list(run_left, run_right, win_left, win_right, rank_left, rank_right, pv_left, pv_right) %>% flatten

results_tidy = map(results, ~ .x$out)
n_left = map_dbl(results, ~ .x$n_left)
n_right = map_dbl(results, ~ .x$n_right)
n_mean = map_dbl(results, ~.x$out_mean) %>% signif(3)
bwv = map_dbl(results, ~.x$bw) %>% signif(2)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc = function(x){
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}
names(results_tidy) = paste0("(", 1:length(results_tidy), ")") %>% apply_mc

row_mat = matrix(ncol = length(n_left) + 1, byrow = TRUE,
                 c("Parties", apply_mc(rep(c("Left", "Right"), times = 4)),
                 "Bandwidth", bwv,
                 "Outcome Mean", n_mean,
                 "N (left)", apply_mc(n_left),
                 "N (right)", apply_mc(n_right))) %>%
          as.data.frame

clist = c("treatTRUE" = "Elected",
          "female" = "Female",
          "treatTRUE:female" = "Elected x Female")

# Export table
tex_out = modelsummary(results_tidy,
             coef_map = clist,
             gof_omit = ".*",
             caption = "\\label{tab:norway_by_party_mech} \\textbf{Mechanisms leading to lower incumbency advantage: Difference-in-Discontinuity Estimates On Additional Outcomes, Norway.}",
             booktabs = TRUE,
             add_rows = row_mat,
             output = "latex") %>%
  kable_styling(
    font_size = 9,
    latex_options = "hold_position"
  ) %>%
  footnote(
      threeparttable = TRUE,
      fixed_small_size = FALSE,
      footnote_as_chunk = TRUE,
      general_title = "",
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. Other coefficients reported in Table I4.")) %>%
  add_header_above(c(" " = 1, "Orig. Rank Advance" = 2, "Actual Rank Advance" = 2, "Pre-Ad. (t+1)" = 2, "Pers.V. Share (t+1)" = 2)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:9, latex_column_spec = "S[
              input-symbols=(),
              table-format=-2.3,
              table-space-text-pre    = (,
              table-space-text-post   = ),
              input-open-uncertainty  =,
              input-close-uncertainty = ,
              table-align-text-post = false]") %>%
  gsub(pattern = "\\textbackslash{}", replacement= "\\", fixed = TRUE) %>%
  gsub(pattern = "\\{", replacement= "{", fixed = TRUE) %>%
  gsub(pattern = "\\}", replacement= "}", fixed = TRUE)

  cat(tex_out, file = "output/tables/norway_by_party_mech.tex")