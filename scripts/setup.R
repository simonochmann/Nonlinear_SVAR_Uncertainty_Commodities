# scripts/setup.R

# List of required packages
required_packages <- c(
  "dplyr", "tidyr", "readr", "lubridate",
  "zoo", "xts", "tsDyn", "vars", "svars", "ggplot2",
  "readxl", "glue", "janitor", "tibble", "stringdist", "stringr",
  "digest", "purrr", "jsonlite", "here", "base64enc", "openssl",
  "ggthemes", "scales", "moments", "patchwork", "future", "future.apply",
  "cli", "fs"
  )

# Install missing packages
installed  <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Load all packages
lapply(required_packages, library, character.only = TRUE)

message("All packages loaded successfully.")