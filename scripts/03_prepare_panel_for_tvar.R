# scripts/03_prepare_panel_for_tvar.R
# TVAR input builder: production-grade, config-driven, auditable.

source(here::here("scripts/setup.R"))

source(here::here("functions/tvar/validate/validate_tvar_input.R"))
source(here::here("functions/tvar/clean/standardize_panel_variables.R"))
source(here::here("functions/tvar/clean/filter_common_sample.R"))  # not relied on for vars
source(here::here("functions/tvar/save/save_tvar_input_dataset.R"))
source(here::here("functions/tvar/logs/log_tvar_preprocessing.R"))
source(here::here("functions/tvar/logs/log_tvar_metadata_json.R"))
source(here::here("functions/tvar/logs/log_standardization_activity.R"))
source(here::here("functions/tvar/logs/log_standardization_metadata_json.R"))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Config 
config_path <- here::here("config", "tvar.yaml")

# defaults
cfg <- list(
  indexes           = c("VIX","VXO","JLN"),
  expected_frequency= "monthly",
  required_vars     = c("ret","vol_proxy"),
  missing_allowed   = 0,                
  window            = list(start = NULL, end = NULL),
  out_dir           = here::here("data","tvar")
)

if (file.exists(config_path)) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Found ", config_path, " but package 'yaml' is not installed. Install it or remove config.")
  }
  y <- yaml::read_yaml(config_path)
  for (nm in intersect(names(y), names(cfg))) cfg[[nm]] <- y[[nm]]
  if (!is.null(y$window)) {
    for (nm in intersect(names(y$window), names(cfg$window))) cfg$window[[nm]] <- y$window[[nm]]
  }
} else {
  warning("No config/tvar.yaml found. Using built-in defaults.")
}

OUT_DIR <- cfg$out_dir %||% here::here("data","tvar")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Utility for logs
non_numeric_vars_of <- function(df) {
  keep <- setdiff(names(df), "date")
  keep[vapply(df[keep], function(x) !(is.numeric(x) || inherits(x, "Date")), logical(1))]
}

# Core function 
build_tvar_input <- function(input_path, index_name, cfg, out_dir) {
  stopifnot(file.exists(input_path))
  IDX <- index_name
  tag <- tolower(IDX)
  
  df_raw <- readr::read_csv(input_path, show_col_types = FALSE)
  
  # Light structure checks
  needed <- c("date", cfg$required_vars)
  if (!inherits(df_raw$date, "Date")) stop("Column 'date' must be class Date in ", basename(input_path))
  if (!all(needed %in% names(df_raw))) {
    stop("[", IDX, "] missing required columns: ", paste(setdiff(needed, names(df_raw)), collapse = ", "))
  }
  idx_col_raw <- if (IDX %in% names(df_raw)) IDX else tolower(IDX)
  if (!idx_col_raw %in% names(df_raw)) stop("[", IDX, "] index column not found: ", IDX, " / ", tolower(IDX))
  
  # Standardize names/scales as TVAR expects
  standardized <- standardize_panel_variables(df_raw)
  df_clean     <- standardized$data
  std_meta     <- standardized$meta
  
  # Restore ID columns
  for (nm in c("commodity_id","commodity_name","unit")) {
    if (nm %in% names(df_raw)) df_clean[[nm]] <- df_raw[[nm]]
  }
  
  # Explicit windowing 
  if (!is.null(cfg$window$start)) df_clean <- dplyr::filter(df_clean, date >= as.Date(cfg$window$start))
  if (!is.null(cfg$window$end))   df_clean <- dplyr::filter(df_clean, date <= as.Date(cfg$window$end))
  
  if (nrow(df_clean) == 0) stop("[", IDX, "] no rows remain after windowing.")
  
  # Resolve index col name after standardization
  idx_col_std <- if (IDX %in% names(df_clean)) IDX else tolower(IDX)
  if (!idx_col_std %in% names(df_clean)) {
    stop("[", IDX, "] standardized data missing index col: looked for ", IDX, " and ", tolower(IDX))
  }
  
  # Filter to common sample on numeric drivers only
  required_drivers <- c(cfg$required_vars, idx_col_std)
  numeric_vars     <- names(df_clean)[vapply(df_clean, is.numeric, logical(1))]
  use_vars         <- intersect(required_drivers, numeric_vars)
  if (length(use_vars) < length(required_drivers)) {
    stop("[", IDX, "] required numeric vars not found: ",
         paste(setdiff(required_drivers, use_vars), collapse = ", "))
  }
  
  na_count <- rowSums(is.na(df_clean[, use_vars, drop = FALSE]))
  miss_allowed <- as.integer(cfg$missing_allowed %||% 0)
  if (miss_allowed > 0) {
    message("[", IDX, "] allowing up to ", miss_allowed, " missing in drivers per row.")
  }
  mask    <- na_count <= miss_allowed
  n_before <- nrow(df_clean)
  df_tvar  <- df_clean[mask, , drop = FALSE]
  n_after  <- nrow(df_tvar)
  
  cat("\nCommon-sample filter for ", IDX, " using vars: ",
      paste(use_vars, collapse = ", "),
      "\nKept ", n_after, "/", n_before, " rows (",
      round(100 * n_after / max(1, n_before), 1), "%)\n", sep = "")
  
  if (n_after == 0) {
    stop("[", IDX, "] zero rows after filtering. Consider tightening window or relaxing missing_allowed.")
  }
  
  # Per-commodity coverage & drop reasons
  drop_reason <- apply(
    is.na(df_clean[, use_vars, drop = FALSE]),
    1,
    function(mis) if (!any(mis)) NA_character_ else use_vars[which(mis)[1]]
  )
  
  df_tmp <- dplyr::mutate(df_clean, `_mask` = mask, `_reason` = drop_reason)
  df_cov <- dplyr::group_by(df_tmp, commodity_id)
  df_cov <- dplyr::summarise(
    df_cov,
    coverage = mean(`_mask`),
    top_reason = {
      rr <- `_reason`[!is.na(`_reason`)]
      if (length(rr)) names(sort(table(rr), decreasing = TRUE))[1] else NA_character_
    },
    .groups = "drop"
  )
  
  cov_path <- here::here(out_dir, paste0("tvar_input_", tag, "_coverage_by_commodity.csv"))
  readr::write_csv(df_cov, cov_path)
  
  # Strict validation after filtering
  val_dates <- df_tvar |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_obs    = dplyr::n(),
      ret_mean = mean(ret, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::arrange(date)
  
  validate_tvar_input(val_dates, expected_frequency = cfg$expected_frequency)
  
  # Save TVAR-ready panel
  out_file <- here::here(out_dir, paste0("tvar_input_", tag, ".csv"))
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  save_out <- save_tvar_input_dataset(df_tvar, path = out_file, hash = TRUE)
  
  # Logs
  log_tvar_preprocessing(
    n_obs      = nrow(df_tvar),
    n_vars     = ncol(df_tvar) - 1,
    date_range = range(df_tvar$date),
    tag        = tag,
    sha256     = save_out$sha256,
    path       = out_file
  )
  log_tvar_metadata_json(df = df_tvar, path = out_file, tag = tag)
  
  nnv <- non_numeric_vars_of(df_clean)
  log_standardization_activity(
    original_names   = names(df_raw),
    cleaned_names    = std_meta$colnames,
    non_numeric_vars = nnv,
    tag  = tag
  )
  log_standardization_metadata_json(
    original_names   = names(df_raw),
    cleaned_names    = std_meta$colnames,
    non_numeric_vars = nnv,
    output_path      = out_file,
    tag              = tag
  )
  
  message(sprintf(" TVAR input panel for '%s' written to: %s", tag, out_file))
  
  list(
    index         = IDX,
    out_file      = out_file,
    sha256        = save_out$sha256,
    kept_rows     = n_after,
    kept_share    = n_after / max(1, n_before),
    coverage_csv  = cov_path,
    window        = cfg$window,
    missing_allowed = miss_allowed
  )
}

#  Driver 
inputs <- setNames(
  object = paste0("data/merged/commodity_panel_with_", tolower(cfg$indexes), ".csv"),
  nm     = cfg$indexes
)

results <- vector("list", length(inputs))
names(results) <- names(inputs)

for (IDX in names(inputs)) {
  input_path <- here::here(inputs[[IDX]])
  results[[IDX]] <- build_tvar_input(input_path, IDX, cfg, OUT_DIR)
}

message("All requested indices processed.")
