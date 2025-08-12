# functions/parse_fred/clean/standardize_fred_indexes.R

#' Bind indexes and produce wide + standardized (z) versions
standardize_fred_indexes <- function(idx_list, verbose = TRUE) {
  suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
  
  long <- dplyr::bind_rows(idx_list) %>%
    dplyr::arrange(date, index)
  
  # Wide panel: date, vix, vxo, jln
  wide <- tidyr::pivot_wider(long, names_from = index, values_from = value)
  
  # Add z-scores as extra columns (vix_z, vxo_z, jln_z)
  out <- wide %>%
    dplyr::mutate(dplyr::across(-date, ~ as.numeric(scale(.x)[, 1]), .names = "{.col}_z"))
  
  if (verbose) {
    covg <- sapply(dplyr::select(out, -date), function(x) mean(!is.na(x)))
    cat("Coverage (share non-NA):\n"); print(round(covg, 3))
  }
  
  out
}