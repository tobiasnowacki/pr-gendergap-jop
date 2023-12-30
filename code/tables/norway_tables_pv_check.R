
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

options(modelsummary_format_numeric_latex = "plain")

# Load data
rd_dat = read.csv("data/no_rd.csv") %>%
  filter(abs(winmargin_loc) < 0.5,
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo") %>%
  filter(inconsistency_candidate_loc == 0,
         missing_votes_municipality_loc == 0) %>%
  mutate(pv_log = log(pv_again + 1))

# Categorise parties
left = c("a", "sv")
right = c("h", "frp", "krf", "v")

# Define RD specification
form_used = "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(cnr_loc)  + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc"

# Raw votes
mod1 = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "pv_again",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
mod2 = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "pv_again",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Share total votes
mod3 = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "pv_share",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
mod4 = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "pv_share",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Share party votes
mod5 = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "pv_share_p",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
mod6 = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "pv_share_p",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))

# Raw votes (unconditional)
mod7 = run_het_rd(rd_dat %>% filter(party_loc %in% left),
           "pv_log",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))
mod8 = run_het_rd(rd_dat %>% filter(party_loc %in% right),
           "pv_log",
           "winmargin_loc",
           form_used,
           bw_fac = c(1))


results = list(mod1, mod2, mod7, mod8, mod3, mod4, mod5, mod6) %>% flatten

results_tidy = map(results, ~ .x$out)

modelsummary(results_tidy)

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

tex_out = modelsummary(results_tidy,
             coef_map = clist,
             statistic = c(
                 "std.error"
             ),
             gof_omit = ".*",
             caption = "\\label{tab:norway_pv_check} \\textbf{Sensitivity of Results to Different Personal Vote Measures.} The gender gap estimate is consistent across preferred measures.",
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
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Other coefficients not reported. Regression run on all candidates in elections between 2003 and 2015.")) %>%
  add_header_above(
      c(" " = 1,
       "Raw PV" = 2,
       "log(Raw PV)" = 2,
       "PV / Total Votes" = 2,
       "PV / Party Votes" = 2
       )
    ) %>%
  row_spec(c(2, 4, 6), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  gsub(pattern = "\\textbackslash{}", replacement= "\\", fixed = TRUE) %>%
  gsub(pattern = "\\{", replacement= "{", fixed = TRUE) %>%
  gsub(pattern = "\\}", replacement= "}", fixed = TRUE)

  cat(tex_out, file = "output/tables/norway_pv_check.tex")
