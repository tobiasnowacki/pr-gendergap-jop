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
rd_dat = read.csv("data/spain_rd_ready.csv",
                  row.names = NULL) %>%
    filter(pop < 20000) %>%
    filter(abs(winmargin_loc) < 0.5) %>%
    filter(winmargin_loc != 0) %>%
    mutate(rank_diff = (rank_again - rank))

# Throw out cities that cross pop border...
city_cross = rd_dat %>%
  select(city_id, year, pop) %>%
  distinct() %>%
  group_by(city_id) %>%
  arrange(city_id, year) %>%
  mutate(
    pop_next = lead(pop),
    crosses = case_when(
    pop < 3000 ~ pop_next > 3000,
    pop > 3000 ~ pop_next < 3000
  )) %>%
  group_by(city_id) %>%
  summarise(ct = sum(crosses > 0, na.rm = TRUE))

rd_dat = rd_dat %>%
  filter(city_id %in% city_cross$city_id[city_cross$ct == 0])
# Set up four did models
f1 <- as.formula(win_again ~ treat + female + treat:female + I(pop > 3000) +    treat:I(pop > 3000) + female:I(pop > 3000) + treat:female:I(pop > 3000)  | as.factor(province_id) + as.factor(year) + as.factor(year):as.factor(party_short) | 0 | city_id)

f2 <- as.formula(win_again ~ treat + female + treat:female + I(pop > 3000) + treat:I(pop > 3000) + female:I(pop > 3000) + treat:female:I(pop > 3000) + pop + pop:I(pop > 3000)  | as.factor(province_id) + as.factor(year) + as.factor(year):as.factor(party_short) | 0 | city_id)

dd_df_25 = rd_dat %>%
  filter(abs(winmargin_loc) < 0.025) %>%
  filter(pop %in% 1000:5000, year > 2007)

dd_df_50 = rd_dat %>%
  filter(abs(winmargin_loc) < 0.05) %>%
  filter(pop %in% 1000:5000, year > 2007)

mod1 <- felm(f1, data = dd_df_25)
mod2 <- felm(f1, data = dd_df_50)
mod3 <- felm(f2, data = dd_df_25)
mod4 <- felm(f2, data = dd_df_50)

results = list(mod1, mod2, mod3, mod4)
n_obs = map_dbl(results, ~ .x$N)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc = function(x){
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}
names(results) = paste0("(", 1:length(results), ")") %>% apply_mc

row_mat = matrix(ncol = length(n_obs) + 1, byrow = TRUE,
                 c("Pop Window", apply_mc(rep(c("1-5k"), times = 4)),
                 "Margin Window", apply_mc(rep(c("0.025", "0.05"), times = 2)),
                 "Linear Pop Trend", apply_mc(rep(c("N", "Y"), each = 2)),
                 "N", apply_mc(n_obs)
                 )) %>%
          as.data.frame

clist = c("treatTRUE" = "Elected",
          "female" = "Female",
          "I(pop > 3000)TRUE" = "Quota",
          "treatTRUE:female" = "Elected x Female",
          "treatTRUE:I(pop > 3000)TRUE" = "Elected x Quota",
          "female:I(pop > 3000)TRUE" = "Female x Quota",
          "treatTRUE:female:I(pop > 3000)TRUE" = "Elected x Female x Quota"
          )

tex_out <- modelsummary(results,
             coef_map = clist,
             gof_omit = ".*",
             caption = "\\label{tab:spain_by_quota_did} \\textbf{Difference-in-Difference Estimates For Incumbency Advantage In Spanish Municipalities, By Quota Law.} The estimates for the gender gap are similar in municipalities with and without the gender quota on lists.",
             booktabs = TRUE,
             add_rows = row_mat,
             output = "latex") %>%
  kable_styling(
    font_size = 9,
    latex_options="hold_position"
  ) %>%
  footnote(
      threeparttable = TRUE,
      fixed_small_size = FALSE,
      footnote_as_chunk = TRUE,
      general_title = "",
      general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Quota` is an indicator for candidates in cities with a population greater than 3,000. Regression run on all candidates in elections between 2011 and 2015. Observations in municipalities that crossed the 3,000 population threshold in between elections are excluded.")) %>%
  add_header_above(c(" " = 1, "Win (t+1)" = 4)) %>%
  row_spec(c(2, 4, 6, 8, 10, 12), extra_latex_after = "\\addlinespace") %>%
  row_spec(14, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:5, latex_column_spec = "S[
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

cat(tex_out, file = "output/tables/spain_by_quota_did.tex")
