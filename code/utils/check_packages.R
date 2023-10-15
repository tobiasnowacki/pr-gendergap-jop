pkgs <- c(
    "tidyverse",
    "rio",
    "devtools",
    "rdrobust",
    "modelsummary",
    "kableExtra",
    "broom",
    "lfe"
)

for (x in pkgs){
    # install.packages(x, repos = "https://cloud.r-project.org")
    library(x, character.only = TRUE)
    print(
        paste0(x, ": ", packageVersion(x))
    )
}