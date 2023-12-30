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

# Function for triangular kernel weights
weighting <- function(xx, kernel, bandw) {
  if (kernel > 0) {
    if (kernel == 1) {
      xx$wght <- bandw - abs(xx$winmargin_loc)
    }
    if (kernel == 2) {
      xx$wght <- 0.75 * (1 - ((xx$run / bandw)**2))
    }
    if (kernel == 3) {
      xx$wght <- (1 / ((2 * pi)**2)) * exp(-0.5 * ((abs(xx$run / bandw)**2)))
    }
    xx$wght[xx$t == 1] <- xx$wght[xx$t == 1] / sum(xx$wght[xx$t == 1])
    xx$wght[xx$t == 0] <- xx$wght[xx$t == 0] / sum(xx$wght[xx$t == 0])
  } else {
    xx$wght <- 1 / nrow(xx)
  }
  return(xx)
}


# Load data in
rd_dat = read.csv("data/spain_rd_ready.csv",
                  row.names = NULL) %>%
    filter(year < 2019) %>%
    filter(pop < 250000) %>%
    filter(abs(winmargin_loc) < 0.5) %>%
    filter(major_party == TRUE) %>%
    filter(winmargin_loc != 0)

mean(rd_dat$female)
table(rd_dat$female, rd_dat$cand_count) %>% prop.table(., margin = 1)

run_results = run_het_rd(
  rd_dat,
  "run_again",
  "winmargin_loc",
  "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id",
w = TRUE)

win_results = run_het_rd(rd_dat,
           "win_again",
           "winmargin_loc",
           "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(province_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id", w = TRUE)

# rank_results = run_het_rd(rd_dat,
#            "rank_again",
#            "winmargin_loc",
#            "winmargin_loc + treat + female + winmargin_loc:treat + winmargin_loc:female + treat:female + winmargin_loc:treat:female | as.factor(city_id) + as.factor(year) + as.factor(party_short) + as.factor(year):as.factor(party_short)| 0 | city_id")

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
          "female" = "Female",
          "treatTRUE:female" = "Elected x Female")


tex_out = modelsummary(results_tidy,
             coef_map = clist,
             gof_omit = ".*",
             caption = "\\label{tab:spain_main_tri} \\textbf{Heterogeneity-in-DiscontinuityEstimates For Incumbency Advantage In Spanish Municipalities, with triangular kernels}. Women enjoy a larger effect of winning on their probability to win again.",
             booktabs = TRUE,
             add_rows = row_mat,
             output = "latex") %>%
  kable_styling(
    latex_options="hold_position",
    # latex_options="scale_down"
    font_size = 9
  ) %>%
  footnote(
      threeparttable = TRUE,
      fixed_small_size = FALSE,
      footnote_as_chunk = TRUE,
      general_title = "",
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses.")) %>%
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

cat(tex_out, file = "output/tables/spain_main_tri.tex")
