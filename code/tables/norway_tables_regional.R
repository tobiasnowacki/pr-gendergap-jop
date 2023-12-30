library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(lfe)
library(broom)
library(devtools)
library(fixest)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
options("modelsummary_format_numeric_latex" = "plain")

source("code/utils/run_het_rd.R")

dat <- read_csv("data/norway_reg.csv")

dat_left <- dat %>%
  filter(party_reg %in% c("a", "sv"))

dat_right <- dat %>%
  filter(party_reg %in% c("h", "frp", "krf", "v"))

# Get means
m1 <- mean(dat$win_again, na.rm = TRUE)
m2 <- mean(dat_left$win_again)
m3 <- mean(dat_right$win_again)

# Whole sample
mod1 <- feols(win_again ~ elected_reg + female + elected_reg:female | year + cnr_reg, dat, cluster = ~cnr_reg)
mod2 <- feols(win_again ~ elected_reg + female + elected_reg:female | year + cnr_reg + party_reg^year, dat, cluster = ~cnr_reg)

# Left parties
mod3 <- feols(win_again ~ elected_reg + female + elected_reg:female | year + cnr_reg, dat_left, cluster = ~cnr_reg)
mod4 <- feols(win_again ~ elected_reg + female + elected_reg:female | year + cnr_reg + party_reg^year, dat_left, cluster = ~cnr_reg)

# Right parties
mod5 <- feols(win_again ~ elected_reg + female + elected_reg:female | year + cnr_reg, dat_right, cluster = ~cnr_reg)
mod6 <- feols(win_again ~ elected_reg + female + elected_reg:female | year + cnr_reg + party_reg^year, dat_right, cluster = ~cnr_reg)

library(kableExtra)

results <- list(mod1, mod2, mod3, mod4, mod5, mod6)

results_tidy <- map(results, ~ .x$out)
library(modelsummary)
modelsummary(results)

n_mean <- rep(c(m1, m2, m3), each = 2) %>% signif(3)
n_obs <- rep(c(nrow(dat), nrow(dat_left), nrow(dat_right)), each = 2) %>% signif(3)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc <- function(x) {
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}


names(results) <- paste0("(", 1:length(results), ")") %>% apply_mc()

row_mat <- matrix(
  ncol = 7, byrow = TRUE,
  c(
    "Parties", apply_mc(rep(c("All", "Left", "Right"), each = 2)),
    "N", apply_mc(c(n_obs)),
    "Outcome Mean", apply_mc(c(n_mean)),
    "Region FE", apply_mc(rep("Y", 6)),
    "Year FE", apply_mc(rep(c("Y", "N"), 3)),
    "Year x Party FE", apply_mc(rep(c("N", "Y"), 3))
  )
) %>%
  as.data.frame()

clist <- c(
  "elected_reg" = "Elected",
  "female" = "Female",
  "elected_reg:female" = "Elected x Female"
)

f <- function(x) format(round(x, 3), big.mark = ",")
goflist <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f),
  list("raw" = "r.squared", "clean" = "R^2", "fmt" = f)
)

tex_out <- modelsummary(results,
  coef_map = clist,
  gof_omit = "N|Adj|Pseudo|Within|AIC|BIC|Log|FE|Std",
  caption = "\\label{tab:norway_regional} \\textbf{Probabilities of Winning Re-Election in Norway's Regional Elections.}. Female incumbents are unlikely to experience a smaller incumbency advantage than men in this context. ",
  booktabs = TRUE,
  add_rows = row_mat,
  output = "latex"
) %>%
  kable_styling(
    font_size = 9,
    # latex_options = "hold_position"
  ) %>%
  footnote(
    threeparttable = TRUE,
    fixed_small_size = FALSE,
    footnote_as_chunk = TRUE,
    general_title = "",
    general = c("All estimates are reported with robust standard errors clustered at the region level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Regression run on all candidates in regional elections between 2003 and 2015.")
  ) %>%
  add_header_above(c(" " = 1, "Win (in t+1)" = 6)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace") %>%
  column_spec(2:9, latex_column_spec = "S[
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

cat(tex_out, file = "output/tables/norway_regional.tex")
