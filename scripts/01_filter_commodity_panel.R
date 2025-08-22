# scripts/01_filter_commodity_panel.R
# Filter the Pink Sheet long panel to your study set,
# compute log returns & a volatility proxy, and save outputs.
# Uses case-insensitive matching for commodity names.

source("scripts/setup.R")

source(here::here("functions/filter/filter_complete_series.R"))
source(here::here("functions/filter/compute_log_returns.R"))
source(here::here("functions/filter/compute_volatility_proxy.R"))
source(here::here("functions/filter/save_filtered_panel.R"))

# logs
source(here::here("functions/filter/logs/log_dropped_commodities.R"))
source(here::here("functions/filter/logs/log_retained_commodities.R"))
source(here::here("functions/filter/logs/log_filtering_activity.R"))
source(here::here("functions/filter/logs/log_filtering_metadata_json.R"))

source(here::here("functions/filter/validate/validate_filtered_panel.R"))

# Parameters
INPUT_FILE   <- here::here("data","raw","commodity_prices_long.csv")
OUT_DIR      <- here::here("data","processed")
LOG_DIR      <- here::here("logs","filtering")
LOG_TXT_PATH <- here::here("logs","filtering","filtering_pipeline.log")

MIN_OBS   <- 180
VOL_METHOD <- "squared"  # "squared", "abs", etc.
TAGS       <- c("monthly","macro","filtered")
NOTE       <- "Baseline filtering after long-format parse"

# Study set
KEEP <- c(
  "crude_wti","ngas_us",
  "gold","platinum","silver",
  "cocoa","coffee_arabic","maize","cotton_a_indx","soybeans","sugar_wld","wheat_us_hrw",
  "aluminium","copper","lead","nickel","tin","zinc"
)

# Ensure dirs
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

# Load input
stopifnot(file.exists(INPUT_FILE))
df <- readr::read_csv(INPUT_FILE, show_col_types = FALSE)
stopifnot(all(c("date","commodity","price") %in% names(df)))
df$date <- as.Date(df$date)

# Case-insensitive filter to the study set
KEEP_LC <- tolower(KEEP)
df <- df %>% mutate(commodity_lc = tolower(commodity))
n_before <- n_distinct(df$commodity)

df_keep <- df %>%
  filter(commodity_lc %in% KEEP_LC) %>%
  arrange(commodity_lc, date)

if (nrow(df_keep) == 0) {
  stop("After case-insensitive filtering, no study-set commodities were found. ",
       "Check names in data/raw/commodity_prices_long.csv.")
}

cat("\nStudy-set presence (case-insensitive):\n")
present <- sort(unique(df_keep$commodity))
missing <- setdiff(sort(KEEP), sort(unique(df_keep$commodity_lc)))
cat("  Present:", paste(present, collapse = ", "), "\n")
if (length(missing)) cat("  Missing:", paste(missing, collapse = ", "), "\n")

# Filter complete series
df_keep <- df_keep %>% dplyr::select(date, commodity, price)# drop helper col
df_filtered <- filter_complete_series(df_keep, min_obs = MIN_OBS)

# Compute log returns
df_returns <- compute_log_returns(df_filtered)

# Normalize after compute_log_returns, but keep log_return 
# Ensure ln_price exists
if (!"ln_price" %in% names(df_returns)) {
  df_returns <- dplyr::mutate(df_returns, ln_price = log(price))
}

# Ensure ret exists, but do not drop log_return
if (!"ret" %in% names(df_returns)) {
  # Prefer log_return if present; otherwise compute from ln_price
  if ("log_return" %in% names(df_returns)) {
    df_returns <- dplyr::mutate(df_returns, ret = log_return)
  } else {
    df_returns <- df_returns %>%
      dplyr::arrange(commodity, date) %>%
      dplyr::group_by(commodity) %>%
      dplyr::mutate(ret = ln_price - dplyr::lag(ln_price)) %>%
      dplyr::ungroup()
  }
}

# stable set of columns but include log_return 
df_returns <- df_returns %>%
  dplyr::select(date, commodity, price, ln_price, ret, log_return) %>%
  dplyr::arrange(commodity, date)

# Compute volatility proxy (requires log_return present)
df_vol <- compute_volatility_proxy(df_returns, method = VOL_METHOD)

# Canonicalize to downstream schema 
filtered_panel <- df_vol %>%
  dplyr::mutate(
    # Canonical mappings
    commodity_id   = gsub("[^a-z0-9]+", "_", tolower(commodity)),
    commodity_name = commodity,
    unit           = NA_character_
  ) %>%
  dplyr::select(
    date, commodity_id, commodity_name, unit,
    price, ln_price, ret, vol_proxy
  ) %>%
  dplyr::arrange(commodity_id, date)

# Save canonical output for 02_merge_uncertainty_index.R 
CLEAN_DIR <- here::here("data", "clean")
dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_FILE_CANON <- here::here("data", "clean", "commodity_panel.csv")
readr::write_csv(filtered_panel, OUT_FILE_CANON)

# keep processed outputs as before 
paths <- save_filtered_panel(
  df_vol,              
  out_dir = OUT_DIR,
  overwrite = TRUE,
  return_paths = TRUE
)

# Retained vs dropped (from canonical object) 
retained <- sort(unique(filtered_panel$commodity_name))
dropped  <- setdiff(sort(unique(df_keep$commodity)), retained)

cat("\nSummary after filtering:\n")
cat("  Retained commodities (", length(retained), "): ",
    paste(retained, collapse = ", "), "\n", sep = "")
if (length(dropped)) {
  cat("  Dropped commodities (insufficient obs): ",
      paste(dropped, collapse = ", "), "\n", sep = "")
}

# Prepare dropped_df for logging
dropped_df <- df_keep %>%
  dplyr::group_by(commodity) %>%
  dplyr::summarise(n_obs = sum(!is.na(price)), .groups = "drop") %>%
  dplyr::filter(commodity %in% dropped)

# Logs 
try({
  log_dropped_commodities(
    dropped_df,
    out_dir = LOG_DIR,
    tags    = TAGS,
    note    = NOTE
  )
}, silent = TRUE)

try({
  log_retained_commodities(
    data.frame(commodity = retained),
    out_dir = LOG_DIR,
    tags    = TAGS,
    note    = NOTE
  )
}, silent = TRUE)

log_filtering_activity(
  out_log = LOG_TXT_PATH,
  n_commodities_before = n_before,
  n_commodities_after  = length(retained),
  min_obs              = MIN_OBS,
  method_vol_proxy     = VOL_METHOD,
  out_dir              = OUT_DIR,
  tags                 = TAGS,
  note                 = NOTE
)

log_filtering_metadata_json(
  df        = filtered_panel,  
  out_dir   = OUT_DIR,
  vol_method= VOL_METHOD,
  min_obs   = MIN_OBS,
  tags      = TAGS,
  note      = NOTE
)

cat("\nCanonical panel written to: ", OUT_FILE_CANON, "\n")
cat("Processed panel(s) saved to: ", paste(paths, collapse = ", "), "\n")

# Validate canonical object
validate_filtered_panel(filtered_panel)

# Schema snapshot
schema_dir  <- here::here("schemas")
schema_path <- here::here("schemas", "commodity_panel_schema.json")
dir.create(schema_dir, recursive = TRUE, showWarnings = FALSE)

schema_obj <- list(
  columns = c("date","commodity_id","commodity_name","unit","price","ln_price","ret","vol_proxy"),
  types   = c("Date","chr","chr","chr","num","num","num","num"),
  key     = c("commodity_id","date")
)

if (!file.exists(schema_path)) {
  jsonlite::write_json(schema_obj, schema_path, auto_unbox = TRUE, pretty = TRUE)
  cat("Schema snapshot written to:", schema_path, "\n")
} else {
  # validate against existing snapshot
  snap <- jsonlite::read_json(schema_path, simplifyVector = TRUE)
  # column check (order-insensitive)
  if (!setequal(names(filtered_panel), snap$columns)) {
    stop("Schema drift: columns differ from snapshot.\n",
         "Expected: ", paste(snap$columns, collapse=", "),
         "\nGot:      ", paste(names(filtered_panel), collapse=", "))
  }
  # light type guard (coerces R classes to short labels for comparison)
  to_short <- function(x) {
    if (inherits(x, "Date")) "Date"
    else if (is.numeric(x)) "num"
    else if (is.character(x)) "chr"
    else if (is.integer(x)) "int"
    else class(x)[1]
  }
  got_types <- vapply(filtered_panel, to_short, character(1))
  # Align order to snapshot columns
  got_types <- got_types[snap$columns]
  if (!identical(unname(got_types), snap$types)) {
    stop("Schema drift: types differ from snapshot.\n",
         "Expected: ", paste(snap$types, collapse=", "),
         "\nGot:      ", paste(unname(got_types), collapse=", "))
  }
  cat("Schema validated against snapshot:", schema_path, "\n")
}


jsonlite::write_json(
  list(
    columns = c("date","commodity_id","commodity_name","unit","price","ln_price","ret","vol_proxy"),
    types   = c("Date","chr","chr","chr","num","num","num","num"),
    key     = c("commodity_id","date")
  ),
  "schemas/commodity_panel_schema.json",
  auto_unbox = TRUE, pretty = TRUE
)