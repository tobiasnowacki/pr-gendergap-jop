## Overview

This repository contains all data and code necessary to replicate the tables and plots contained in Nowacki (2023), "The Gender Gap in Political Careers Under Proportional Representation" (Journal of Politics).

The code was successfully run using 4.1.2 (''Bird Hippie'') on MacOS 14.0 Sonoma. You will need to install the following packages (e.g. using `install.packages()`) as of 01-December-2023:

```
tidyverse
rio
devtools
rdrobust
modelsummary
kableExtra
fixest
broom
lfe
gbm
Matching
fabricatr
fastDummies
readxl
haven
```

To replicate all code run the following terminal command from the root folder of the repository:

```
make R
```

To then embed the generated results into the `.tex` file (to check against the previously submitted version), compile `draft/pr_gendergap.tex`


### Versions of packages used

To consult the version of packages installed in your local `R` version, run `code/utils/check_packages.R`.

```
[1] "tidyverse: 2.0.0"
[1] "rio: 1.0.1"
[1] "devtools: 2.4.5"
[1] "rdrobust: 2.1.1"
[1] "modelsummary: 1.4.2"
[1] "kableExtra: 1.3.4"
[1] "broom: 1.0.5"
[1] "lfe: 2.8.6"
[1] "gbm: 2.1.8"
[1] "Matching: 4.9.9"
[1] "fabricatr: 1.0.0"
[1] "fastDummies: 1.6.3"
[1] "readxl: 1.4.3"
[1] "haven: 2.5.3"
```