# Load dependencies
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

# Parties to keep
parties_keep <- c(
    "Komitet Wyborczy Platforma Obywatelska RP",
    "Komitet Wyborczy Prawo i Sprawiedliwość",
    "koalicyjny komitet wyborczy platforma.nowoczesna koalicja obywatelska"
) %>% tolower()

# Load data
rd_dat <- read.csv("data/poland_county.csv") %>%
    filter(
        abs(running) < 0.5,
        running != 0,
        year < 2014
    ) %>%
    mutate(
        winmargin_loc = running,
        female = ifelse(gender == "K", 1, 0),
        treat = ifelse(elected == "T", 1, 0)
    ) %>%
    filter(listname %in% parties_keep)

# Estimate RD running again
run_results <- run_het_rd(
    rd_dat,
    "run_again",
    "running",
    "running + treat + female + running:treat + running:female + treat:female + running:treat:female | as.factor(unitid) + as.factor(year) + as.factor(listname):as.factor(year) | 0 | unitid"
)

# Estimate RD winning again
win_results <- run_het_rd(
    rd_dat,
    "win_again",
    "running",
    "running + treat + female + running:treat + running:female + treat:female + running:treat:female | as.factor(unitid) + as.factor(year) + as.factor(listname):as.factor(year) | 0 | unitid"
)

# Wrangle results into table for export
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
    "treat" = "Elected",
    "female" = "Female",
    "treat:female" = "Elected x Female"
)

# Export table
tex_out <- modelsummary(results_tidy,
    coef_map = clist,
    gof_omit = ".*",
    caption = "\\label{tab:poland_main} \\textbf{Difference-in-Discontinuity Estimates For Incumbency Advantage In Polish Counties and County-Like Cities.} The gender gap is similar in magnitude to that of Norwegian municipalities.",
    booktabs = TRUE,
    add_rows = row_mat,
    output = "latex"
) %>%
    kable_styling(
        # latex_options="scale_down"
        font_size = 9
    ) %>%
    footnote(
        threeparttable = TRUE,
        fixed_small_size = FALSE,
        footnote_as_chunk = TRUE,
        general_title = "",
        general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. Each observation is a candidate's election attempt. 'Elected' is an indicator for observations where the candidate obtained a seat in the municipal council. 'Female' is an indicator for observations identified as female. `Elected` times `Female` is the interaction between the two variables. Other coefficients not reported. Regression run on all candidates from two main parties (PO, PiS) in county-level elections in 2010.")
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
              table-align-text-post = false]") %>%
    gsub(pattern = "\\textbackslash{}", replacement = "\\", fixed = TRUE) %>%
    gsub(pattern = "\\{", replacement = "{", fixed = TRUE) %>%
    gsub(pattern = "\\}", replacement = "}", fixed = TRUE)

cat(tex_out, file = "output/tables/poland_main.tex")