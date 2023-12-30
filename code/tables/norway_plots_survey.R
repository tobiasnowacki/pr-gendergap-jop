library(tidyverse)
library(rio)
library(readxl)
library(rdrobust)
library(lfe)
library(modelsummary)
library(kableExtra)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

options(modelsummary_format_numeric_latex = "plain")
source("code/utils/het_by_sex_plot.R")

df <- read_excel("data/localsurvey_crosstab.xlsx") %>%
    mutate(party_en = factor(c("Soc Left", "Labour", "ChrDem", "Liberal", "Conserv", "Progress \n (right-wing pop)", "Green")),
    bloc = c("left", "left", rep("right", 4), "left")) %>%
    filter(!is.na(party))

df$party_en = factor(df$party_en, levels = c("Green", "Soc Left", "Labour", "ChrDem", "Liberal", "Conserv", "Progress \n (right-wing pop)"))

ggplot(df, aes(x = party_en, y = more)) +
    geom_hline(yintercept = 50, lty = "dashed", lwd = 0.5) +
    geom_bar(stat = "identity", aes(fill = bloc)) +
    scale_fill_manual(values = c("#c20606", "#0f0fbd")) +
    labs(x = "Party", y = "% Agree", fill = "Party Group") +
    theme_tn()

ggsave(
  "output/figures/norway_survey_support.pdf",
  device = cairo_pdf,
  width = 6,
  height = 4
  )
