# scripts/setup.R

# scripts/setup.R (top or after library() calls)
Sys.setenv(
  OMP_NUM_THREADS        = "1",
  OPENBLAS_NUM_THREADS   = "1",
  MKL_NUM_THREADS        = "1",
  VECLIB_MAXIMUM_THREADS = "1"  # Apple Accelerate on macOS
)
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)
if (requireNamespace("data.table", quietly = TRUE))  data.table::setDTthreads(1)
options(mc.cores = 1)

# List of required packages
required_packages <- c(
  "dplyr", "tidyr", "readr", "lubridate",
  "zoo", "xts", "tsDyn", "vars", "svars", "ggplot2",
  "readxl", "glue", "janitor", "tibble", "stringdist", "stringr",
  "digest", "purrr", "jsonlite", "here", "base64enc", "openssl",
  "ggthemes", "scales", "moments", "patchwork", "future", "future.apply",
  "cli", "fs", "jsonvalidate", "furrr", "gtools", "kableExtra", "quarto"
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