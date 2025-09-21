# scripts/08_export_appendix_commodities_table.R
suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr); library(tidyr); library(stringr); library(tibble)
})

DATA_DIR <- here::here("data","tvar")
MODELS_DIR <- here::here("models","tvar")
OUT_DIR <- fs::path(MODELS_DIR, "artifacts","tables")
fs::dir_create(OUT_DIR)

# 18 Joëts commodities 
JOETS18 <- c(
  "aluminium","cocoa","coffee_arabic","copper","cotton_a_indx","crude_wti",
  "gold","lead","maize","ngas_us","nickel","platinum","silver",
  "soybeans","sugar_wld","tin","wheat_us_hrw","zinc"
)

# Map id -> group
map_group <- function(id){
  v <- tolower(id)
  dplyr::case_when(
    v %in% c("crude_wti","ngas_us") ~ "Energy",
    v %in% c("gold","silver","platinum") ~ "Precious",
    v %in% c("aluminium","copper","lead","nickel","tin","zinc") ~ "Industrial",
    v %in% c("cocoa","coffee_arabic","cotton_a_indx","maize","soybeans","sugar_wld","wheat_us_hrw") ~ "Agriculture",
    TRUE ~ "Other"
  )
}

# display names 
pretty_name <- function(id){
  dplyr::case_when(
    id == "crude_wti"       ~ "Crude oil (WTI)",
    id == "ngas_us"         ~ "Natural gas (US)",
    id == "coffee_arabic"   ~ "Coffee (Arabica)",
    id == "cotton_a_indx"   ~ "Cotton A Index",
    id == "sugar_wld"       ~ "Sugar (World)",
    id == "wheat_us_hrw"    ~ "Wheat (US HRW)",
    TRUE ~ stringr::str_to_title(gsub("_", " ", id))
  )
}

cand <- c("tvar_input_vix.csv", "tvar_input_vxo.csv", "tvar_input_jln.csv")
files <- fs::path(DATA_DIR, cand)
files <- files[fs::file_exists(files)]
stopifnot(length(files) >= 1)

read_one <- function(f) {
  x <- readr::read_csv(f, show_col_types = FALSE)
  need <- c("date","commodity_id")
  if (!all(need %in% names(x))) return(tibble::tibble(date=as.Date(character()), commodity_id=character()))
  if (!inherits(x$date, "Date")) suppressWarnings(x$date <- as.Date(x$date))
  x %>%
    dplyr::select(date, commodity_id) %>%
    dplyr::filter(commodity_id %in% JOETS18) %>%
    dplyr::distinct()
}

ALL <- dplyr::bind_rows(lapply(files, read_one))

COV <- ALL %>%
  dplyr::group_by(commodity_id) %>%
  dplyr::summarise(
    start = min(date, na.rm = TRUE),
    end   = max(date, na.rm = TRUE),
    months= dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Group     = map_group(commodity_id),
    Commodity = pretty_name(commodity_id),
    Start     = format(start, "%Y-%m"),
    End       = format(end,   "%Y-%m")
  ) %>%
  dplyr::select(Group, Commodity, Start, End, months) %>%
  dplyr::arrange(factor(Group, levels=c("Energy","Industrial","Precious","Agriculture","Other")), Commodity)

out_csv <- fs::path(OUT_DIR, "appendix_commodities_coverage.csv")
readr::write_csv(COV, out_csv)
message("Wrote: ", out_csv)
