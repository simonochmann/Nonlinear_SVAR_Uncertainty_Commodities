suppressPackageStartupMessages({library(here); library(fs); library(readr); library(tibble)})
OUT <- here::here("models","tvar","artifacts","tables"); fs::dir_create(OUT)
info <- tibble::tibble(
  item   = c("Release timestamp","FRED retrieval","Pink Sheet snapshot"),
  value  = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             "2025-08-27 (VIXCLS, VXOCLS, JLNUM12M)",
             "2025-06 (World Bank Pink Sheet)")
)
readr::write_csv(info, fs::path(OUT, "run_info.csv"))
