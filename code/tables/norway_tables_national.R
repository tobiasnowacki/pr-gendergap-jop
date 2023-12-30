library(tidyverse)
library(haven)
library(rdrobust)
library(modelsummary)
library(kableExtra)
library(lfe)
library(broom)
library(devtools)
library(fixest)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

options(modelsummary_format_numeric_latex = "plain")

source("code/utils/run_het_rd.R")

big_party <- c("sv", "h", "v", "dna", "sp", "krf", "frp")

dat <- read_dta("data/DynastyPanel.dta") %>%
    mutate(win_again = seat_next1, treatment = margin > 0) %>%
    filter(party %in% big_party) %>%
    filter(!is.na(margin), year > 2000, year < 2013)

dat_full <- dat
dat_ten <- dat %>% filter(abs(margin) < 0.1)
dat_five <- dat %>% filter(abs(margin) < 0.05)

# Get means
m1 <- mean(dat$win_again, na.rm = TRUE)
m2 <- mean(dat_ten$win_again, na.rm = TRUE)
m3 <- mean(dat_five$win_again, na.rm = TRUE)

# Whole sample
mod1 <- feols(win_again ~ treatment + female + treatment:female | year + districtid, dat_full, cluster = ~districtid)
mod2 <- feols(win_again ~ treatment + female + treatment:female | year^party + districtid, dat_full, cluster = ~districtid)

# Left parties
mod3 <- feols(win_again ~ treatment + female + treatment:female | year + districtid, dat_ten, cluster = ~districtid)
mod4 <- feols(win_again ~ treatment + female + treatment:female | year^party + districtid, dat_ten, cluster = ~districtid)

# Right parties
mod5 <- feols(win_again ~ treatment + female + treatment:female | year + districtid, dat_five, cluster = ~districtid)
mod6 <- feols(win_again ~ treatment + female + treatment:female | year^party + districtid, dat_five, cluster = ~districtid)


results <- list(mod1, mod2, mod3, mod4, mod5, mod6)

results_tidy <- map(results, ~ .x$out)
library(modelsummary)
modelsummary(results)

n_mean <- rep(c(m1, m2, m3), each = 2) %>% signif(3)
n_obs <- rep(c(nrow(dat), nrow(dat_ten), nrow(dat_five)), each = 2) %>% signif(3)

# Wrap multicolumn around header (to make it centred with decimal alignment)
apply_mc <- function(x) {
    return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}


names(results) <- paste0("(", 1:length(results), ")") %>% apply_mc()

row_mat <- matrix(
    ncol = 7, byrow = TRUE,
    c(
        "Bandwidth", apply_mc(rep(c("All", "0.10", "0.05"), each = 2)),
        "N", apply_mc(c(n_obs)),
        "Outcome Mean", apply_mc(c(n_mean)),
        "Region FE", apply_mc(rep("Y", 6)),
        "Year FE", apply_mc(rep(c("Y", "N"), 3)),
        "Year x Party FE", apply_mc(rep(c("N", "Y"), 3))
    )
) %>%
    as.data.frame()

clist <- c(
    "treatmentTRUE" = "Elected",
    "female" = "Female",
    "treatmentTRUE:female" = "Elected x Female"
)

f <- function(x) format(round(x, 3), big.mark = ",")
goflist <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = f),
    list("raw" = "r.squared", "clean" = "R^2", "fmt" = f)
)

tex_out <- modelsummary(results,
    coef_map = clist,
    gof_omit = "N|Adj|Pseudo|Within|AIC|BIC|Log|FE|Std",
    caption = "\\label{tab:norway_national} \\textbf{Probabilities of Winning Re-Election in Norway's National Elections (Difference-in-Differences).} ",
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
        general = c("All estimates are reported with robust standard errors clustered at the election district level in parentheses. Each observation is a candidate's election attempt. Regression run on all candidates in national elections between 2003 and 2009.")
    ) %>%
    add_header_above(c(" " = 1, "Win (t+1)" = 6)) %>%
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

cat(tex_out, file = "output/tables/norway_national.tex")
