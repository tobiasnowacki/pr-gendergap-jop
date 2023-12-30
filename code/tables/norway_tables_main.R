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

# Load data
rd_dat <- read.csv("data/no_rd.csv") %>%
  filter(
    abs(winmargin_loc) < 0.5, winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo"
  ) %>%
  filter(inconsistency_candidate_loc == 0) %>%
  filter(party_loc %in% c("a", "frp", "h", "krf", "sp", "sv", "v"))

# Estimate RD for running again
run_results <- run_het_rd(
  rd_dat,
  "run_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(cnr_loc) + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc"
)

# Estimate RD for winning again
win_results <- run_het_rd(
  rd_dat,
  "win_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(cnr_loc) + as.factor(year) + as.factor(party_loc) + as.factor(party_loc):as.factor(year) | 0 | knr_loc"
)

# Wrangle into exportable table
results_out <- append(run_results, win_results)

results_tidy <- map(results_out, ~ .x$out)
n_left <- map_dbl(results_out, ~ .x$n_left)
n_right <- map_dbl(results_out, ~ .x$n_right)
n_mean <- map_dbl(results_out, ~ .x$out_mean) %>% signif(3)
bwv <- map_dbl(results_out, ~ .x$bw) %>% signif(2)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc <- function(x) {
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}
names(results_tidy) <- paste0("(", 1:length(results_tidy), ")") %>% apply_mc()

# Prepare table elements
row_mat <- matrix(
  ncol = length(n_left) + 1, byrow = TRUE,
  c(
    "Bandwidth", bwv,
    "BW Type", rep(c("Optimal", "2x Opt", "0.5x Opt"), 2) %>% apply_mc(),
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

# Export table
tex_out <- modelsummary(results_tidy,
  coef_map = clist,
  gof_omit = ".*",
  caption = "\\label{tab:norway_main} \\textbf{Heterogeneity-in-DiscontinuityEstimates For Incumbency Advantage In Norwegian Municipalities.} Women face diminished incumbency effect on winning again.",
  booktabs = TRUE,
  add_rows = row_mat,
  kable_format = "latex",
  output = "latex"
) %>%
  kable_styling(
    font_size = 9
  ) %>%
  footnote(
    threeparttable = TRUE,
    fixed_small_size = FALSE,
    footnote_as_chunk = TRUE,
    general_title = "",
    general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Other coefficients (running variable) reported in Table I1. Regression run on all candidates in elections between 2003 and 2015. ")
  ) %>%
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
              table-align-text-post = false]")

tex_out <- tex_out %>%
  gsub(pattern = "\\textbackslash{}", replacement = "\\", fixed = TRUE) %>%
  gsub(pattern = "\\{", replacement = "{", fixed = TRUE) %>%
  gsub(pattern = "\\}", replacement = "}", fixed = TRUE)

cat(tex_out, file = "output/tables/norway_main.tex")


## Create full table w/ all coefficients
clist_extended <- c(
  "treatTRUE" = "Elected",
  "female" = "Female",
  "treatTRUE:female" = "Elected x Female",
  "winmargin_loc" = "Margin",
  "winmargin_loc:treatTRUE" = "Margin x Elected",
  "winmargin_loc:female" = "Margin x Female",
  "winmargin_loc:treatTRUE:female" = "Margin x Elected x Female"
)

tex_out_full <- modelsummary(results_tidy,
  coef_map = clist_extended,
  gof_omit = ".*",
  caption = "\\label{tab:norway_main_full} \\textbf{Heterogeneity-in-DiscontinuityEstimates For Incumbency Advantage In Norwegian Municipalities.} Women face diminished incumbency effect on winning again.",
  booktabs = TRUE,
  add_rows = row_mat,
  kable_format = "latex",
  output = "latex"
) %>%
  kable_styling(
    latex_options = "hold_position",
    font_size = 9
  ) %>%
  footnote(
    threeparttable = TRUE,
    fixed_small_size = FALSE,
    footnote_as_chunk = TRUE,
    general_title = "",
    general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Regression run on all candidates in elections between 2003 and 2015.")
  ) %>%
  add_header_above(c(" " = 1, "Run (t + 1)" = 3, "Win (t + 1)" = 3)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(c(6, 14), extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:7, latex_column_spec = "S[
              input-symbols=(),
              table-format=-1.3,
              table-space-text-pre    = (,
              table-space-text-post   = ),
              input-open-uncertainty  =,
              input-close-uncertainty = ,
              table-align-text-post = false]")

tex_out_full <- tex_out_full %>%
  gsub(pattern = "\\textbackslash{}", replacement = "\\", fixed = TRUE) %>%
  gsub(pattern = "\\{", replacement = "{", fixed = TRUE) %>%
  gsub(pattern = "\\}", replacement = "}", fixed = TRUE)

cat(tex_out_full, file = "output/tables/norway_main_full.tex")
