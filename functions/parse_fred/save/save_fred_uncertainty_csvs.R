save_fred_uncertainty_csvs <- function(df, out_dir, verbose = TRUE) {
  suppressPackageStartupMessages(library(readr))
  long <- tidyr::pivot_longer(df, -date, names_to = "index", values_to = "value")
  p1 <- here::here(out_dir, "uncertainty_fred_wide.csv")
  p2 <- here::here(out_dir, "uncertainty_fred_long.csv")
  readr::write_csv(df, p1)
  readr::write_csv(long, p2)
  if (verbose) cat(" Saved:", p1, "\n        ", p2, "\n")
  c(p1, p2)
}
