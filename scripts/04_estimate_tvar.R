# scripts/04_estimate_tvar.R  — MULTIVARIATE (ALL COMMODITIES) PER INDEX

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(glue); library(fs); library(yaml)
})

source("scripts/setup.R")  # prints "All packages loaded successfully."

source(here("functions/tvar/estimate/select_tvar_threshold.R"), local = TRUE)
source(here("functions/tvar/estimate/estimate_tvar_model.R"),   local = TRUE)
source(here("functions/tvar/validate/validate_estimated_tvar.R"), local = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# -------------------------------------------------------------------
# 0) Load BOTH configs: custom (tvar_estimate.yaml) + paths.yml
# -------------------------------------------------------------------
cfg_est_path <- here("config","tvar_estimate.yaml")
cfg_est <- list(
  indexes                 = c("VIX","VXO","JLN"),
  commodities             = NULL,
  lags_set                = 2:6,
  delays_set              = 1:3,
  trim                    = 0.15,
  ngrid                   = 100,
  criterion               = "BIC",
  min_regime_share        = 0.15,
  use_given_split         = FALSE,
  smooth_ma               = 3,      # 1=no smoothing; 3≈MA(3)
  window                  = list(start=NULL, end=NULL),
  out_dir                 = NULL,   # we'll override using paths.yml
  verbose                 = TRUE,
  overwrite               = FALSE,
  max_commodities         = NULL,
  require_both_stable     = TRUE,
  min_obs_per_regime      = 40,
  standardize_y           = TRUE,
  report_theta_percentile = TRUE
)
if (file.exists(cfg_est_path)) {
  y <- yaml::read_yaml(cfg_est_path)
  for (nm in intersect(names(y), names(cfg_est))) cfg_est[[nm]] <- y[[nm]]
  if (!is.null(y$window)) {
    for (nm in intersect(names(y$window), names(cfg_est$window))) cfg_est$window[[nm]] <- y$window[[nm]]
  }
} else {
  message("No config/tvar_estimate.yaml found — using built-in defaults.")
}
cfg_est$standardize_y           <- isTRUE(cfg_est$standardize_y)
cfg_est$report_theta_percentile <- isTRUE(cfg_est$report_theta_percentile)
cfg_est$require_both_stable     <- isTRUE(cfg_est$require_both_stable)

# paths.yml (authoritative for model dir & inputs used elsewhere)
cfg_paths <- if (file.exists(here("config","paths.yml"))) {
  yaml::read_yaml(here("config","paths.yml"))
  } else {
  list(models = list(dir = "models/tvar", default_save_notes = "analysis_ready"))
}
MODELS_DIR <- here(cfg_paths$models$dir %||% "models/tvar")   # <- unify with 05/07
fs::dir_create(MODELS_DIR)

OUT_DIR <- MODELS_DIR  # keep summary/error CSVs next to models so everything’s together
summary_path <- fs::path(OUT_DIR, "tvar_summary.csv")
error_path   <- fs::path(OUT_DIR, "tvar_errors.csv")

# CSV schemas
SUMMARY_COLS <- c("index","commodity_id","p","delay","theta","theta_pct","share_low","share_high",
                  "n_low","n_high","sumBIC","sumAIC","standardized","model_path","run_at")
ERROR_COLS   <- c("index","commodity_id","stage","message","run_at")

ensure_csv_schema <- function(path, cols){
  recreate <- !file.exists(path)
  if (!recreate) {
    x <- try(readr::read_csv(path, n_max = 5, show_col_types = FALSE), silent = TRUE)
    if (inherits(x, "try-error") || !setequal(names(x), cols) || ncol(x) != length(cols)) {
      file.copy(path, paste0(path, ".bak"), overwrite = TRUE); recreate <- TRUE
    }
  }
  if (recreate) {
    df <- as.data.frame(setNames(rep(list(logical(0)), length(cols)), cols))
    if (identical(cols, SUMMARY_COLS)) {
      df[] <- list(character(), character(), integer(), integer(), double(), double(),
                   double(), double(), integer(), integer(), double(), double(),
                   logical(), character(), as.POSIXct(character()))
    } else if (identical(cols, ERROR_COLS)) {
      df[] <- list(character(), character(), character(), character(), as.POSIXct(character()))
    }
    readr::write_csv(df, path)
  }
}
append_summary <- function(row) readr::write_csv(row, summary_path, append = TRUE)
append_error   <- function(index, cid, stage, msg) {
  readr::write_csv(
    data.frame(index=index, commodity_id=cid, stage=stage, message=msg, run_at=Sys.time()),
    error_path, append = TRUE
  )
}

ensure_csv_schema(summary_path, SUMMARY_COLS)
ensure_csv_schema(error_path,   ERROR_COLS)

roll_mean_right <- function(x, n=1L) { if (n<=1L) return(as.numeric(x)); as.numeric(stats::filter(x, rep(1/n,n), sides=1)) }

# -------------------------------------------------------------------
# 1) Driver: ONE multivariate TVAR per index (VIX / VXO / JLN)
#    Input: data/tvar/tvar_input_<index>.csv (LONG panel)
#    Required columns: date, commodity_id, ret, <index> (VIX/VXO/JLN)
# -------------------------------------------------------------------
for (IDX in cfg_est$indexes) {
  tag <- tolower(IDX)
  in_file <- here("data","tvar", sprintf("tvar_input_%s.csv", tag))
  message(glue("\n=== {IDX}: looking for {fs::path_rel(in_file)}"))
  if (!file.exists(in_file)) {
    append_error(IDX, NA, "load_input", paste0("Missing TVAR input: ", in_file))
    message("  ✗ Missing input → logged to tvar_errors.csv"); next
  }
  
  df <- readr::read_csv(in_file, show_col_types = FALSE)
  if (!inherits(df$date, "Date")) suppressWarnings(df$date <- as.Date(df$date))
  
  idx_col <- if (IDX %in% names(df)) IDX else tolower(IDX)
  if (!idx_col %in% names(df)) {
    append_error(IDX, NA, "check_index_col", paste0("Index column not found in input: ", idx_col))
    message("  ✗ Index column not found → logged"); next
  }
  
  # Optional sample window
  if (!is.null(cfg_est$window$start)) df <- dplyr::filter(df, date >= as.Date(cfg_est$window$start))
  if (!is.null(cfg_est$window$end))   df <- dplyr::filter(df, date <= as.Date(cfg_est$window$end))
  df <- dplyr::arrange(df, date)
  
  # Commodities
  JOETS18 <- c("aluminium","cocoa","coffee_arabic","copper","cotton_a_indx","crude_wti",
               "gold","lead","maize","ngas_us","nickel","platinum","silver","soybeans",
               "sugar_wld","tin","wheat_us_hrw","zinc")
  commod_all <- unique(df$commodity_id)
  commod_sel <- if (is.null(cfg_est$commodities)) JOETS18 else cfg_est$commodities
  commod_sel <- intersect(commod_sel, commod_all)
  message(glue("  → {IDX}: using {length(commod_sel)}/{length(JOETS18)} Joets commodities"))
  
  if (length(commod_sel) < 2L) {
    append_error(IDX, "PANEL", "precheck", "Need at least 2 commodities for a VAR.")
    message("  ✗ Too few commodities → logged"); next
  }
  
  # Audit list
  fs::dir_create(here("logs","tvar"))
  writeLines(sort(commod_sel), here("logs","tvar", sprintf("commodities_selected_%s.txt", tag)))
  
  # ---- BUILD WIDE PANEL + ATTACH INDEX COLUMN ----
  # 1) long -> wide returns (one col per commodity)
  df_wide <- df |>
    dplyr::filter(commodity_id %in% commod_sel) |>
    dplyr::select(date, commodity_id, ret) |>
    tidyr::pivot_wider(names_from = commodity_id, values_from = ret) |>
    dplyr::arrange(date)
  
  # 2) grab the index series (VIX/VXO/JLN) by date and join
  idx_tbl <- df[, c("date", idx_col), drop = FALSE] |> dplyr::distinct()
  df_wide <- dplyr::left_join(df_wide, idx_tbl, by = "date")
  
  # sanity: ensure the index column is present after joining
  stopifnot(idx_col %in% names(df_wide))
  
  # ---- CLEAN & PREP Y ----
  # 1) choose Y columns and filter complete cases jointly with index
  y_cols <- setdiff(names(df_wide), c("date", idx_col))
  stopifnot(length(y_cols) >= 2)
  
  keep   <- stats::complete.cases(df_wide[, c(y_cols, idx_col)])
  df_use <- df_wide[keep, c("date", y_cols), drop = FALSE]
  th_use <- df_wide[[idx_col]][keep]
  
  # 2) enforce numeric + safe names
  df_use[y_cols] <- lapply(df_use[y_cols], function(v) as.numeric(v))
  names(df_use)[match(y_cols, names(df_use))] <- make.names(y_cols, unique = TRUE)
  y_cols <- setdiff(names(df_use), "date")  # refresh after rename
  
  # 3) drop zero-variance series
  nzv <- vapply(df_use[y_cols], function(v) stats::var(v, na.rm = TRUE) > 0, logical(1))
  if (!all(nzv)) {
    message("  • Dropping zero-variance: ", paste(y_cols[!nzv], collapse=", "))
    y_cols <- y_cols[nzv]
    df_use <- df_use[, c("date", y_cols), drop = FALSE]
  }
  
  # 4) drop exact duplicates (SAFE LOOP BOUNDS)
  if (length(y_cols) >= 2) {
    keep_idx <- rep(TRUE, length(y_cols))
    for (i in seq_len(length(y_cols) - 1L)) {           # <= safe upper bound
      if (!keep_idx[i]) next
      for (j in seq.int(i + 1L, length(y_cols))) {      # <= safe sequence
        if (!keep_idx[j]) next
        if (isTRUE(all.equal(df_use[[ y_cols[i] ]],
                             df_use[[ y_cols[j] ]],
                             check.attributes = FALSE))) {
          keep_idx[j] <- FALSE
        }
      }
    }
    if (any(!keep_idx)) {
      message("  • Dropping duplicates: ", paste(y_cols[!keep_idx], collapse=", "))
      y_cols <- y_cols[keep_idx]
      df_use <- df_use[, c("date", y_cols), drop = FALSE]
    }
  }
  
  # 5) optional standardization
  if (isTRUE(cfg_est$standardize_y)) {
    df_use[y_cols] <- lapply(df_use[y_cols], scale)
  }
  
  # 5.1) optional smoothing of threshold series
  if (isTRUE(cfg_est$smooth_ma > 1)) {
    th_use <- roll_mean_right(th_use, n = cfg_est$smooth_ma)
  }
  
  # 6) final Y-only for estimator
  df_est <- df_use[, y_cols, drop = FALSE]
  
  # 7) guards
  stopifnot(ncol(df_est) >= 2)
  stopifnot(!anyNA(df_est))
  stopifnot(sum(!is.finite(as.matrix(df_est))) == 0)
  message("  final K = ", ncol(df_est), " | T = ", nrow(df_est))
  
  # THRESHOLD: fixed median split for balance 
  theta_val <- stats::median(th_use, na.rm = TRUE)
  threshold_info <- list(threshold_value = theta_val, percentile = 0.5, variable = paste0(idx_col, "_L1"))
  
  message(glue("  Using fixed threshold θ={round(theta_val,3)} (median)"))
  
  # Sanity prints (remove later)
  message("  df_est dims: ", paste(dim(df_est), collapse=" x "))
  message("  any NA in Y? ", anyNA(df_est))
  message("  non-finite count: ", sum(!is.finite(as.matrix(df_est))))
  
  stopifnot(is.data.frame(df_est))
  stopifnot(ncol(df_est) >= 2)
  stopifnot(!anyNA(df_est))
  stopifnot(sum(!is.finite(as.matrix(df_est))) == 0)
  
  # Guard to be explicit
  stopifnot("date" %in% names(df_use))
  
  est <- tryCatch(
    estimate_tvar_model(
      df               = df_use,       # includes 'date'
      th_series        = th_use,
      threshold_info   = list(threshold_value = theta_val,
                              percentile = 0.5,
                              variable   = paste0(idx_col, "_L1")),
      lags_set         = cfg_est$lags_set,
      delays_set       = cfg_est$delays_set,
      trim             = cfg_est$trim,
      ngrid            = cfg_est$ngrid,
      criterion        = cfg_est$criterion,
      min_regime_share = cfg_est$min_regime_share,
      use_given_split  = FALSE,        # <-- change this
      standardize_y    = FALSE,
      require_both_stable = cfg_est$require_both_stable,
      verbose          = cfg_est$verbose
    ),
    silent = TRUE
  )
  
  if (inherits(est, "try-error")) {
    append_error(IDX, "PANEL", "estimate_all", as.character(est))
    message("  ✗ Estimation failed → logged"); next
  }
  
  # enrich metadata (keep as you had it)
  est$metadata <- est$metadata %||% list()
  est$metadata$dates            <- df_use$date
  est$metadata$threshold_var    <- paste0(idx_col, "_L1")
  est$metadata$threshold_series <- as.numeric(th_use)
  est$metadata$standardized     <- isTRUE(cfg_est$standardize_y)
  est$regime_variable           <- paste0(idx_col, "_L1")
  if (!is.null(est$threshold_value) && !is.list(est$threshold_value)) {
    est$threshold_value <- list(as.numeric(est$threshold_value))
  }
  est$variables <- y_cols
  
  # Save (STAMPED + LATEST) into MODELS_DIR from paths.yml
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stamped <- fs::path(MODELS_DIR, sprintf("%s_tvar_model_%s_%s.rds",
                                          tolower(IDX), ts,
                                          cfg_paths$models$default_save_notes %||% "withRegim_wr"))
  latest  <- fs::path(MODELS_DIR, sprintf("%s_tvar_model_latest.rds", tolower(IDX)))
  
  readr::write_rds(est, stamped, compress = "gz")
  if (fs::file_exists(latest)) try(fs::file_delete(latest), silent = TRUE)
  fs::file_copy(stamped, latest)
  
  message(glue("  ✓ saved ALL‑COMMODITIES TVAR for {IDX} → {fs::path_rel(stamped)} (K={length(y_cols)})"))
  append_summary(data.frame(
    index        = as.character(IDX),
    commodity_id = "PANEL",
    p            = as.integer(est$spec$p),
    delay        = as.integer(est$spec$delay),
    theta        = as.numeric(est$threshold_value),
    theta_pct    = as.numeric(if (isTRUE(cfg_est$report_theta_percentile)) est$theta_percentile else NA_real_),
    share_low    = as.numeric(unname(est$regime_share["low"])),
    share_high   = as.numeric(unname(est$regime_share["high"])),
    n_low        = as.integer(est$regimes$low$n_obs),
    n_high       = as.integer(est$regimes$high$n_obs),
    sumBIC       = as.numeric(sum(est$regimes$low$BIC) + sum(est$regimes$high$BIC)),
    sumAIC       = as.numeric(sum(est$regimes$low$AIC) + sum(est$regimes$high$AIC)),
    standardized = isTRUE(cfg_est$standardize_y),
    model_path   = as.character(stamped),
    run_at       = Sys.time(),
    stringsAsFactors = FALSE
  ))
}

message("\nAll requested ALL‑COMMODITIES TVAR estimations completed.\n")