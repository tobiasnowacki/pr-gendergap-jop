# Load dependencies
library(tidyverse)
library(rio)
library(rdrobust)
library(modelsummary)
library(fastDummies)
library(lfe)
library(broom)
library(fixest)
library(gbm)
library(kableExtra)
library(devtools)
library(Matching)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/utils/run_het_rd.R")
options(modelsummary_format_numeric_latex = "plain")


# Define functions for script -------------------------

#' Function to estimate propensity scores (Pr = Female)
#' on data given vector of covariates W
estimate_prop_scores <- function(data, W, depth_param = 4) {
  train.dat <- data[, c("female", "winmargin_loc", W)]
  depths <- seq_len(depth_param)
  bt.models <- list()
  set.seed(2019207)
  for (i in 1:length(depths)) {
    bt.models[[i]] <- gbm(female ~ .,
      data = train.dat, shrinkage = 0.01,
      distribution = "bernoulli", n.trees = 1000,
      interaction.depth = depths[i],
      cv.folds = 10
    )
    print(i)
  }

  n.depths <- length(bt.models)
  depths <- rep(NA, n.depths)
  min.cv.error <- rep(NA, n.depths)
  best.n.trees <- rep(NA, n.depths)
  for (i in 1:n.depths) {
    bt.curr <- bt.models[[i]]
    depths[i] <- bt.curr$interaction.depth
    min.cv.error[i] <- min(bt.curr$cv.error)
    best.n.trees[i] <- which.min(bt.curr$cv.error)
    rm(bt.curr)
  }
  m <- which.min(min.cv.error)
  final.ntrees <- best.n.trees[m]
  final.depth <- depths[m]

  data$prop.scores <- predict(bt.models[[m]],
    newdata = data,
    n.trees = final.ntrees,
    type = "response"
  )

  return(data)
}

#' Produce distribution of propensity scores
#' `data` object needs prop.scores variable!
propensity_plot <- function(data, mod_name) {
  plotobj <- ggplot(
    data,
    aes(x = prop.scores, fill = as.factor(female), group = female)
  ) +
    geom_density(alpha = 0.4) +
    labs(
      x = "Propensity Scores",
      y = "Density",
      fill = paste0("Moderator (", mod_name, ")")
    ) +
    scale_color_viridis_d(labels = c("Male", "Female"), end = 0.7) +
    theme_tn()
  return(plotobj)
}

# Package everything in one function
return_matched_estimates <- function(data, W, bw = 0.05, return_balanceplot = FALSE) {
  rd_dat_restrict <- estimate_prop_scores(data, W)

  # Invert to produce exact matches on all columns NOT listed here
  exact_vec <- !c("winmargin_loc", W) %in% c("winmargin_loc", "age", "no_run", "year", year_cols)

  rd_dat_left <- rd_dat_restrict %>% filter(winmargin_loc <= 0)
  rd_dat_right <- rd_dat_restrict %>% filter(winmargin_loc > 0)

  matchmod <- Match(
    Tr = rd_dat_left[, c("female")],
    X = as.matrix(rd_dat_left[, c("winmargin_loc", W)]),
    exact = exact_vec,
    M = 1, estimand = "ATT", replace = FALSE, Weight = 2
  )

  mdat_left <- rd_dat_left[
    c(matchmod$index.treated, matchmod$index.control),
  ]

  matchmod <- Match(
    Tr = rd_dat_right[, c("female")],
    X = as.matrix(rd_dat_right[, c("winmargin_loc", W)]),
    exact = exact_vec,
    M = 1, estimand = "ATT", replace = FALSE, Weight = 2
  )

  mdat_right <- rd_dat_right[
    c(matchmod$index.treated, matchmod$index.control),
  ]

  mdat <- rbind(mdat_left, mdat_right)

  matchplot <- propensity_plot(mdat, "Sex")

  matchreg <- feols(
    win_again ~ winmargin_loc * treat * female,
    cluster = "knr_loc",
    data = mdat %>% filter(abs(winmargin_loc) < bw)
  )

  if (return_balanceplot) {
    return(
      list(
        reg = matchreg,
        bplot = matchplot,
        mdat = mdat
      )
    )
  } else {
    return(matchreg %>% summary())
  }
}


# Execute script ----------------------------------

# Load data
rd_dat <- read.csv("data/no_rd.csv") %>%
  filter(
    abs(winmargin_loc) < 0.5,
    winmargin_loc != 0,
    year < 2019,
    pop_loc < 250000,
    kname_loc != "Oslo"
  ) %>%
  filter(inconsistency_candidate_loc == 0) %>%
  filter(party_loc %in% c("a", "frp", "h", "krf", "sp", "sv", "v"))

# Bin age into median
rd_dat$age_bin <- 0
rd_dat$age_bin[rd_dat$age > 47] <- 1

# Restrict sample to near threshold (bw = 0.05)
rd_dat_restrict <- rd_dat %>%
  filter(abs(winmargin_loc) < 0.05) %>%
  mutate(party_loc = as.factor(party_loc))

# Create dummies for categorical variables
rd_dat_restrict <- cbind(
  rd_dat_restrict,
  dummy_cols(rd_dat_restrict$party_loc)[, -1],
  dummy_cols(rd_dat_restrict$cnr_loc)[, -1],
  dummy_cols(rd_dat_restrict$year)[, -1]
)


# Generate vectors of dummy columns for matching
year_cols <- c(".data_2003", ".data_2007", ".data_2011", ".data_2015")
party_cols <- str_subset(names(rd_dat_restrict), "^(\\.data\\_[a-z]+)")
prov_cols <- str_subset(names(rd_dat_restrict), "^(\\.data\\_[0-9]+)")
prov_cols <- prov_cols[!prov_cols %in% year_cols]

# Define matching vectors
matchvecs <- list(
  c("age", "no_run", "year"),
  c("age", "no_run", year_cols),
  c("age", "no_run", "year", party_cols),
  c("age", "no_run", "year", prov_cols),
  c("age", "no_run", "year", party_cols, prov_cols)
)

# Fit matched diff-in-disc for different specifications
result_list <- list()
for (i in 1:length(matchvecs)) {
  print(i)
  result_list[[i]] <- return_matched_estimates(
    rd_dat_restrict, matchvecs[[i]],
    bw = 0.05,
    return_balanceplot = TRUE
  )
}

# Save overlap plots
ggsave("output/figures/matching_overlap_1.pdf",
  result_list[[1]]$bplot,
  width = 6, height = 4, device = cairo_pdf
)

ggsave("output/figures/matching_overlap_5.pdf",
  result_list[[5]]$bplot,
  width = 6, height = 4, device = cairo_pdf
)

# Create table with estimates
regs <- map(result_list, ~ .x$reg)

clist <- c(
  "treatTRUE" = "Elected",
  "female" = "Female",
  "treatTRUE:female" = "Elected x Female"
)

apply_mc <- function(x) {
  return(c(paste0("\\multicolumn{1}{c}{", x, "}")))
}
names(regs) <- paste0("(", 1:length(regs), ")") %>% apply_mc()

row_mat <- matrix(
  ncol = length(regs) + 1, byrow = TRUE,
  c(
    "Bandwidth", "0.05", "0.05", "0.05", "0.05", "0.05",
    "N", map_dbl(regs, ~ .x$nobs),
    "Individual Match", "Y", "Y", "Y", "Y", "Y",
    "Year Match", "Y", "", "Y", "Y", "Y",
    "Year (Factor) Match", "", "Y", "", "", "",
    "Party Match", "", "", "Y", "", "Y",
    "Province Match", "", "", "", "Y", "Y"
  )
) %>%
  as.data.frame()

# Export table
tex_out <- modelsummary(regs,
  coef_map = clist,
  gof_omit = ".*",
  caption = "\\label{tab:norway_match} \\textbf{Matched Estimates.}",
  booktabs = TRUE,
  add_rows = row_mat,
  kable_format = "latex",
  output = "latex"
) %>%
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
    general = c("All estimates are reported with robust standard errors clustered at the municipality level in parentheses. 'Individual Match' indicates 1:1 distance-based matching on age and experience; 'Year' match indicates 1:1 distance-based matching on election year; 'Year (Factor)' treats election year as a dummy variable; 'Party Match' and 'Province Match' indicates 1:1 exact matching on party and province, respectively.")
  ) %>%
  add_header_above(c(" " = 1, "Win (t + 1)" = 5)) %>%
  row_spec(c(2, 4), extra_latex_after = "\\addlinespace") %>%
  row_spec(6, extra_latex_after = "\\addlinespace \\midrule \\addlinespace") %>%
  column_spec(2:6, latex_column_spec = "S[
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

cat(tex_out, file = "output/tables/norway_matched.tex")