# Load the necessary packages
library(tidyverse)
library(rio)
library(lfe)
library(modelsummary)
library(kableExtra)

options(modelsummary_format_numeric_latex = "plain")

# Load data in
# Load data in
rd_dat = read.csv("data/spain_rd_ready.csv",
                  row.names = NULL) %>%
    filter(voteshare > 0.1) %>%
    filter(year < 2019) %>%
    filter(pop < 20000) %>%
    filter(abs(winmargin_loc) < 0.5) %>%
    filter(major_party == TRUE) %>%
    filter(year != 2019) %>%
    filter(winmargin_loc != 0)

# Functions for later
apply_mc = function(x){
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}

## --- SET UP MODEL PARAMS

# Bandwidths
bw_vec = c(0.01, 0.005, 0.0025)

# # Formulae
form1 = "run_again ~ treat + female + treat:female | 0 | 0 | city_id" %>% as.formula
form2 = "win_again ~ treat + female + treat:female | 0 | 0 | city_id" %>% as.formula
form3 = "rank_again ~ treat + female + treat:female | 0 | 0 | city_id" %>% as.formula

# Coef maps
cmap = c('treatTRUE' = 'Elected',
         'female' = 'Female',
         'treatTRUE:female' = 'Elected x Female')

# Avg no of votes from bandwidth edge to threshold
voteno = map_dbl(bw_vec, ~
        mean(unlist(rd_dat[abs(rd_dat$winmargin_loc) < .x, "votes_valid"]), na.rm = TRUE)*.x)

# Mean prop running again / winning again
mean_prop = map2_dbl(rep(bw_vec, 2), rep(c("run_again", "win_again"), each = 3),
     ~ mean(unlist(rd_dat[abs(rd_dat$winmargin_loc) < .x, .y]), na.rm = TRUE))

# info table
infotbl = matrix(ncol = 7,
    c("Bandwidth", rep(bw_vec, 2) %>% round(., 4),
      "Mean Out", round(mean_prop, 2),
      "Avg No. votes", rep(voteno, 2) %>% round(., 2)),
    byrow = TRUE
    ) %>% as.data.frame

## --- ESTIMATE MODELS

# Estimate models
mod_list = map2(rep(bw_vec, 2), rep(c(form1, form2), each = 3),
    ~ felm(.y,
        data = rd_dat %>% filter(abs(winmargin_loc) < .x)),
        exactDOF = TRUE)
names(mod_list) = paste0("(", 1:length(mod_list), ")") %>% apply_mc

tbl = modelsummary(mod_list,
    coef_map = cmap,
    caption = "\\label{tab:spain_did}. Estimates from Difference-in-Differences in Spanish Close Elections.",
    booktabs = TRUE,
    gof_omit=".R*",
    add_rows = infotbl,
    output = "latex") %>%
  kable_styling(
    # latex_options="scale_down"
    font_size = 9,
    latex_options = "hold_position"
  ) %>%
  footnote(
      threeparttable = TRUE,
      fixed_small_size = TRUE,
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. Bandwidth indicates the range of running variable margins included in the estimation. Avg. # Votes is the average number of votes between the extreme end of the selected bandwidth and the election threshold.")) %>%
  add_header_above(c(" " = 1, "Run Again" = 3, "Win Again" = 3)) %>%
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
cat(tbl, file = "output/tables/spain_did.tex")
