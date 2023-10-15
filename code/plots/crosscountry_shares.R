library(tidyverse)
library(rio)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

# Load data
df <- read_csv("data/world_bank_women.csv", skip = 4) |>
    pivot_longer(`1960`:`2021`, names_to = "year") |>
    dplyr::select(`Country Name`, year, value) |>
    mutate(year = as.numeric(year)) |>
    filter(year > 2000, `Country Name` %in% c(
        "Austria",
        "Belgium",
        "Bulgaria",
        "Cyprus",
        "Czechia",
        "Denmark",
        "Estonia",
        "Finland",
        "France",
        "Germany",
        "Greece",
        "Hungary",
        "Iceland",
        "Ireland",
        "Italy",
        "Latvia",
        "Lithuania",
        "Luxembourg",
        "Netherlands",
        "Norway",
        "Poland",
        "Portugal",
        "Romania",
        "Slovak Republic",
        "Slovenia",
        "Spain",
        "Sweden",
        "Switzerland",
        "United Kingdom"
    ))

# Compute European average
df_aggregate <- df |>
    group_by(year) |>
    summarise(
        value = mean(value, na.rm = TRUE)
    ) |>
    mutate(
        `Country Name` = "European Average"
    ) |>
    rbind(df %>% filter(`Country Name` %in% c("Norway", "Spain")))

# Produce plot
p1 <- ggplot(df, aes(x = year, y = value)) +
    geom_hline(yintercept = 50, lty = "dotted") +
    geom_line(data = df_aggregate, aes(y = value, lty = `Country Name`)) +
    geom_line(
        data = df %>% filter(!`Country Name` %in% c("Norway", "Spain")),
        aes(group = `Country Name`), alpha = 0.1
    ) +
    theme_tn() +
    labs(x = "Year", y = "% Women in Nat'l Legislature")

ggsave(
    filename = "output/figures/csts_plot.pdf", plot = p1,
    device = cairo_pdf,
    width = 6,
    height = 3
)