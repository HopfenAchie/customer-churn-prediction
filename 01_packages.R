# Packages

# install/load required packages ----

# select libraries
packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "tidymodels",
  "stacks",
  "purrr",
  "readr",
  "fs",
  "stringr",
  "ggcorrplot",
  "skimr",
  "doParallel",
  "LiblineaR",
  "xgboost",
  "forcats"
)

# initial installation of new packages and loading
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
  lapply(packages, FUN = require, character.only = TRUE)
} else {
  lapply(packages, FUN = require, character.only = TRUE)
}