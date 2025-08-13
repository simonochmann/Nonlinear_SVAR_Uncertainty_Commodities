# scripts/04_estimate_tvar.R

source("scripts/setup.R")

source(here("functions/tvar/estimate/select_tvar_threshold.R"))
source(here("functions/tvar/estimate/estimate_tvar_model.R"))
source(here("functions/tvar/validate/validate_estimated_tvar.R"))
source(here("functions/tvar/save/save_tvar_model_object.R"))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# 1) Read config 
cfg_path <- here::here("config", "tvar_estimate.yaml")

# Defaults if no YAML is present
cfg <- list(
  indexes              = c("VIX","VXO","JLN"),
  commodities          = NULL,
  lags_set             = 2:6,
  delays_set           = 1:3,
  trim                 = 0.15,
  ngrid                = 100,
  criterion            = "BIC",   # "BIC" or "AIC"
  min_regime_share     = 0.15,
  use_given_split      = FALSE,
  smooth_ma            = 3,       # 1 = no smoothing; 3 mirrors MA(3)
  window               = list(start=NULL, end=NULL),
  out_dir              = "data/models/tvar",
  verbose              = TRUE,
  overwrite            = FALSE,
  max_commodities      = NULL,    # e.g. 5 for tests
  require_both_stable  = TRUE,
  min_obs_per_regime   = 40,
  standardize_y        = TRUE,
  report_theta_percentile = TRUE
)

if (file.exists(cfg_path)) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Found ", cfg_path, " but package 'yaml' is not installed. Install it or remove config.")
  }
  y <- yaml::read_yaml(cfg_path)
  for (nm in intersect(names(y), names(cfg))) cfg[[nm]] <- y[[nm]]
  if (!is.null(y$window)) {
    for (nm in intersect(names(y$window), names(cfg$window))) cfg$window[[nm]] <- y$window[[nm]]
  }
} else {
  message("No config/tvar_estimate.yaml found — using built-in defaults.")
}

# Normalize booleans from YAML (or defaults)
cfg$standardize_y            <- isTRUE(cfg$standardize_y)
cfg$report_theta_percentile  <- isTRUE(cfg$report_theta_percentile)
cfg$require_both_stable      <- isTRUE(cfg$require_both_stable)

# Output root for all logs/models
OUT_DIR <- here::here(cfg$out_dir %||% "data/models/tvar")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

summary_path <- here::here(OUT_DIR, "tvar_summary.csv")
error_path   <- here::here(OUT_DIR, "tvar_errors.csv")

# Expected schemas
SUMMARY_COLS <- c(
  "index","commodity_id","p","delay","theta","theta_pct","share_low","share_high",
  "n_low","n_high","sumBIC","sumAIC","standardized","model_path","run_at"
)
ERROR_COLS <- c("index","commodity_id","stage","message","run_at")

ensure_csv_schema <- function(path, cols) {
  recreate <- FALSE
  if (file.exists(path)) {
    x <- try(readr::read_csv(path, n_max = 100, show_col_types = FALSE), silent = TRUE)
    if (inherits(x, "try-error") || !setequal(names(x), cols) || ncol(x) != length(cols) ||
        nrow(readr::problems(x)) > 0) {
      file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
      recreate <- TRUE
    }
  } else {
    recreate <- TRUE
  }
  if (recreate) {
    tmpl <- as.list(rep(NA, length(cols))); names(tmpl) <- cols
    df <- as.data.frame(tmpl)[FALSE, , drop = FALSE]
    # fix column classes
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

# call before appends
ensure_csv_schema(summary_path, SUMMARY_COLS)
ensure_csv_schema(error_path,   ERROR_COLS)

# 2) Helpers 
roll_mean_right <- function(x, n = 1L) {
  if (n <= 1L) return(as.numeric(x))
  as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))
}

append_summary <- function(row) {
  readr::write_csv(row, summary_path, append = TRUE)
}
append_error <- function(index, cid, stage, msg) {
  readr::write_csv(
    data.frame(index=index, commodity_id=cid, stage=stage, message=msg, run_at=Sys.time()),
    error_path, append = TRUE
  )
}

# 3) Driver 
for (IDX in cfg$indexes) {
  tag <- tolower(IDX)
  in_file <- here::here("data", "tvar", paste0("tvar_input_", tag, ".csv"))
  if (!file.exists(in_file)) {
    append_error(IDX, NA, "load_input", paste0("Missing TVAR input: ", in_file))
    next
  }
  
  df <- readr::read_csv(in_file, show_col_types = FALSE)
  
  # Ensure date is Date
  if (!inherits(df$date, "Date")) {
    suppressWarnings(df$date <- as.Date(df$date))
  }
  
  # Index column name (tolerate upper/lower)
  idx_col <- if (IDX %in% names(df)) IDX else tolower(IDX)
  if (!idx_col %in% names(df)) {
    append_error(IDX, NA, "check_index_col", paste0("Index col not in file: ", idx_col))
    next
  }
  
  # Optional sample window
  if (!is.null(cfg$window$start)) df <- dplyr::filter(df, date >= as.Date(cfg$window$start))
  if (!is.null(cfg$window$end))   df <- dplyr::filter(df, date <= as.Date(cfg$window$end))
  df <- dplyr::arrange(df, date)
  
  # Commodity list
  all_ids <- unique(df$commodity_id)
  ids <- cfg$commodities %||% all_ids
  ids <- ids[ids %in% all_ids]
  if (!is.null(cfg$max_commodities)) ids <- head(ids, cfg$max_commodities)
  
  message(sprintf("\n=== %s: %d commodities ===", IDX, length(ids)))
  
  # Output subdir for this index
  out_dir_idx <- file.path(OUT_DIR, tag)
  dir.create(out_dir_idx, recursive = TRUE, showWarnings = FALSE)
  
  for (cid in ids) {
    cat("  •", IDX, "— commodity", cid, "...\n")
    df_c <- df[df$commodity_id == cid, , drop = FALSE]
    df_c <- dplyr::arrange(df_c, date)
    
    # y-variables for bivariate TVAR
    y_df <- df_c[, c("date", "ret", "vol_proxy")]
    
    # threshold series (optionally smoothed MA)
    th_raw <- df_c[[idx_col]]
    th_ser <- roll_mean_right(th_raw, n = as.integer(cfg$smooth_ma %||% 1))
    
    # Drop rows with NA on y or threshold before estimation
    keep_mask <- stats::complete.cases(y_df[, c("ret","vol_proxy")]) & !is.na(th_ser)
    y_df_use  <- y_df[keep_mask, , drop = FALSE]
    th_use    <- th_ser[keep_mask]
    
    if (nrow(y_df_use) < 40) {
      append_error(IDX, cid, "precheck", "Too few usable observations (<40) after NA/smoothing drop.")
      next
    }
    
    # Model file path
    model_file <- file.path(out_dir_idx, paste0("tvar_", tag, "_cid_", cid, ".rds"))
    if (file.exists(model_file) && isTRUE(cfg$overwrite) == FALSE) {
      # already estimated — skip
      next
    }
    
    # Estimate
    est <- try(
      estimate_tvar_model(
        df               = y_df_use,
        th_series        = th_use,         # keep threshold in RAW units
        threshold_info   = NULL,
        lags_set         = cfg$lags_set,
        delays_set       = cfg$delays_set,
        trim             = cfg$trim,
        ngrid            = cfg$ngrid,
        criterion        = cfg$criterion,
        min_regime_share = cfg$min_regime_share,
        use_given_split  = cfg$use_given_split,
        standardize_y    = cfg$standardize_y,
        verbose          = cfg$verbose
      ),
      silent = TRUE
    )
    
    if (inherits(est, "try-error")) {
      append_error(IDX, cid, "estimate", as.character(est))
      next
    }
    
    # Optional sanity check
    chk <- try(validate_estimated_tvar(est, verbose = TRUE), silent = TRUE)
    
    if (!inherits(chk, "try-error") && isTRUE(cfg$require_both_stable)) {
      det <- as.data.frame(chk$details)
      enough_obs <- det$enough_obs & det$n_obs >= cfg$min_obs_per_regime
      ok <- isTRUE(all(det$stable)) && isTRUE(all(enough_obs))
      if (!ok) {
        append_error(IDX, cid, "validate", "Unstable or too few obs in a regime — skipping save.")
        next
      }
    }
    
    # Save model object (RDS) with provenance
    cfg_hash <- tryCatch(digest::digest(file = cfg_path, algo = "sha256"), error = function(e) NA)
    sess     <- utils::sessionInfo()
    
    save_tvar_model_object(
      model = est,
      path  = model_file,
      meta  = list(
        index         = IDX,
        commodity_id  = cid,
        input_file    = in_file,
        window        = cfg$window,
        smooth_ma     = cfg$smooth_ma,
        selected      = est$spec,
        config_sha256 = cfg_hash,
        R_version     = R.version.string,
        packages      = lapply(sess$otherPkgs, function(p) p$Version)
      )
    )
    
    message(sprintf("    saved: %s | p=%d d=%d θ=%.3f low=%.2f high=%.2f",
                    model_file, est$spec$p, est$spec$delay, est$threshold_value,
                    est$regime_share["low"], est$regime_share["high"]))
    
    # θ percentile is computed inside the estimator on the aligned (delay-trimmed) series
    theta_pct <- if (isTRUE(cfg$report_theta_percentile)) as.numeric(est$theta_percentile) else NA_real_
    
    # Append summary row (cast explicitly to keep CSV schema stable)
    n_low  <- as.integer(est$regimes$low$n_obs)
    n_high <- as.integer(est$regimes$high$n_obs)
    sumBIC <- as.numeric(sum(est$regimes$low$BIC) + sum(est$regimes$high$BIC))
    sumAIC <- as.numeric(sum(est$regimes$low$AIC) + sum(est$regimes$high$AIC))
    
    append_summary(data.frame(
      index        = as.character(IDX),
      commodity_id = as.character(cid),
      p            = as.integer(est$spec$p),
      delay        = as.integer(est$spec$delay),
      theta        = as.numeric(est$threshold_value),
      theta_pct    = as.numeric(theta_pct),
      share_low    = as.numeric(unname(est$regime_share["low"])),
      share_high   = as.numeric(unname(est$regime_share["high"])),
      n_low        = n_low,
      n_high       = n_high,
      sumBIC       = sumBIC,
      sumAIC       = sumAIC,
      standardized = isTRUE(est$metadata$standardized),
      model_path   = as.character(model_file),
      run_at       = Sys.time(),
      stringsAsFactors = FALSE
    ))
  }
}

message("\nAll requested TVAR estimations completed.\n")