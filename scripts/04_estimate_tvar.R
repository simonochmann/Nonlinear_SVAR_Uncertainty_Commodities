# scripts/04_estimate_tvar.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(glue); library(fs); library(yaml)
})

source("scripts/setup.R")  

source(here("functions/tvar/estimate/select_tvar_threshold.R"), local = TRUE)
source(here("functions/tvar/estimate/estimate_tvar_model.R"),   local = TRUE)
source(here("functions/tvar/validate/validate_estimated_tvar.R"), local = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

pluck_or <- function(x, path, default = NULL) {
  out <- tryCatch({
    for (nm in path) x <- x[[nm]]
    x
  }, error = function(e) NULL)
  if (is.null(out)) default else out
}

# Rebuild residuals
get_residuals <- function(est, reg = c("low","high")){
  reg <- match.arg(reg)
  E <- pluck_or(est, c("regimes", reg, "residuals"), NULL)
  if (!is.null(E)) return(as.matrix(E))
  Y <- pluck_or(est, c("regimes", reg, "Y"), NULL)
  F <- pluck_or(est, c("regimes", reg, "fitted"), NULL)
  if (!is.null(Y) && !is.null(F)) return(as.matrix(Y) - as.matrix(F))
  NULL
}

# Conservative Ljung–Box p (min across series) with smart lag and guards
lb_p_min <- function(E, lag = NULL) {
  if (is.null(E)) return(NA_real_)
  M <- as.matrix(E); n <- nrow(M)
  if (n < 10) return(NA_real_)
  if (is.null(lag)) lag <- max(4L, min(12L, floor(n/4)))
  ps <- apply(M, 2, function(z) {
    z <- as.numeric(z)
    if (!all(is.finite(z)) || var(z) == 0 || length(z) <= (lag + 1)) return(NA_real_)
    suppressWarnings(stats::Box.test(z, lag = lag, type = "Ljung-Box")$p.value)
  })
  ps <- ps[is.finite(ps)]
  if (length(ps)) min(ps) else NA_real_
}

safe_logdet <- function(S, ridge = 1e-10) {
  if (is.null(S)) return(NA_real_)
  ev <- suppressWarnings(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
  if (!length(ev)) return(NA_real_)
  ev[ev < ridge] <- ridge
  sum(log(ev))
}

# Helpers for Table 1 metrics 
spectral_radius_companion <- function(A_list){
  if (is.null(A_list) || !length(A_list)) return(NA_real_)
  k <- nrow(A_list[[1]]); p <- length(A_list)
  if (p == 1L) return(max(Mod(eigen(A_list[[1]], only.values=TRUE)$values)))
  Ctop <- do.call(cbind, A_list)
  Cbot <- cbind(diag(k*(p-1)), matrix(0, nrow=k*(p-1), ncol=k))
  C    <- rbind(Ctop, Cbot)
  max(Mod(eigen(C, only.values=TRUE)$values))
}

normalize_A_list <- function(A, K){
  if (is.null(A)) return(NULL)
  if (is.list(A)) return(A)
  dims <- dim(A)
  if (length(dims)==3) return(lapply(seq_len(dims[3]), function(j) A[,,j,drop=FALSE]))
  if (length(dims)==2){
    nr <- dims[1]; nc <- dims[2]
    if (nr==K && (nc %% K)==0){           
      p <- nc %/% K
      return(lapply(seq_len(p), function(j) A[, ((j-1)*K+1):(j*K), drop=FALSE]))
    }
    if ((nr %% K)==0 && nc==K){           
      p <- nr %/% K
      return(lapply(seq_len(p), function(j) A[((j-1)*K+1):(j*K), , drop=FALSE]))
    }
  }
  NULL
}

estimate_var_ols <- function(Y, p){
  Y <- as.matrix(Y); Tn <- nrow(Y); K <- ncol(Y)
  Z <- NULL
  for (lag in 1:p) Z <- cbind(Z, Y[(p-lag+1):(Tn-lag), , drop=FALSE])
  Z  <- cbind(1, Z)
  Yt <- Y[(p+1):Tn, , drop=FALSE]
  B  <- solve(crossprod(Z), crossprod(Z, Yt))          # (1+Kp) x K
  E  <- Yt - Z %*% B
  Sigma <- crossprod(E) / (nrow(E) - (1 + K*p))
  A_list <- lapply(seq_len(p), function(k) t(B[(2 + (k-1)*K):(1 + k*K), , drop=FALSE]))
  list(A_list=A_list, Sigma=Sigma, E=E)
}

# Load configs: custom (tvar_estimate.yaml) + paths.yml
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

# paths.yml
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

# one multivariate TVAR per index

for (IDX in cfg_est$indexes) {
  tag <- tolower(IDX)
  in_file <- here("data","tvar", sprintf("tvar_input_%s.csv", tag))
  message(glue("\n=== {IDX}: looking for {fs::path_rel(in_file)}"))
  if (!file.exists(in_file)) {
    append_error(IDX, NA, "load_input", paste0("Missing TVAR input: ", in_file))
    message("Missing input → logged to tvar_errors.csv"); next
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
    message("Too few commodities → logged"); next
  }
  
  # Audit list
  fs::dir_create(here("logs","tvar"))
  writeLines(sort(commod_sel), here("logs","tvar", sprintf("commodities_selected_%s.txt", tag)))
  
  df_wide <- df |>
    dplyr::filter(commodity_id %in% commod_sel) |>
    dplyr::select(date, commodity_id, ret) |>
    tidyr::pivot_wider(names_from = commodity_id, values_from = ret) |>
    dplyr::arrange(date)
  
  # grab the index series (VIX/VXO/JLN) by date and join
  idx_tbl <- df[, c("date", idx_col), drop = FALSE] |> dplyr::distinct()
  df_wide <- dplyr::left_join(df_wide, idx_tbl, by = "date")
  
  # ensure the index column is present after joining
  stopifnot(idx_col %in% names(df_wide))
  
  # CLEAN & PREP Y 
  y_cols <- setdiff(names(df_wide), c("date", idx_col))
  stopifnot(length(y_cols) >= 2)
  
  keep   <- stats::complete.cases(df_wide[, c(y_cols, idx_col)])
  df_use <- df_wide[keep, c("date", y_cols), drop = FALSE]
  th_use <- df_wide[[idx_col]][keep]
  
  # enforce numeric + safe names
  df_use[y_cols] <- lapply(df_use[y_cols], function(v) as.numeric(v))
  names(df_use)[match(y_cols, names(df_use))] <- make.names(y_cols, unique = TRUE)
  y_cols <- setdiff(names(df_use), "date")  
  
  # drop zero-variance series
  nzv <- vapply(df_use[y_cols], function(v) stats::var(v, na.rm = TRUE) > 0, logical(1))
  if (!all(nzv)) {
    message("Dropping zero-variance: ", paste(y_cols[!nzv], collapse=", "))
    y_cols <- y_cols[nzv]
    df_use <- df_use[, c("date", y_cols), drop = FALSE]
  }
  
  # drop exact duplicates
  if (length(y_cols) >= 2) {
    keep_idx <- rep(TRUE, length(y_cols))
    for (i in seq_len(length(y_cols) - 1L)) {           # safe upper bound
      if (!keep_idx[i]) next
      for (j in seq.int(i + 1L, length(y_cols))) {      # safe sequence
        if (!keep_idx[j]) next
        if (isTRUE(all.equal(df_use[[ y_cols[i] ]],
                             df_use[[ y_cols[j] ]],
                             check.attributes = FALSE))) {
          keep_idx[j] <- FALSE
        }
      }
    }
    if (any(!keep_idx)) {
      message("Dropping duplicates: ", paste(y_cols[!keep_idx], collapse=", "))
      y_cols <- y_cols[keep_idx]
      df_use <- df_use[, c("date", y_cols), drop = FALSE]
    }
  }
  
  # standardization
  if (isTRUE(cfg_est$standardize_y)) {
    df_use[y_cols] <- lapply(df_use[y_cols], scale)
  }
  
  # smoothing of threshold series
  if (isTRUE(cfg_est$smooth_ma > 1)) {
    th_use <- roll_mean_right(th_use, n = cfg_est$smooth_ma)
  }
  
  # final Y-only for estimator
  df_est <- df_use[, y_cols, drop = FALSE]
  
  # guards
  stopifnot(ncol(df_est) >= 2)
  stopifnot(!anyNA(df_est))
  stopifnot(sum(!is.finite(as.matrix(df_est))) == 0)
  message("  final K = ", ncol(df_est), " | T = ", nrow(df_est))
  
  # fixed median split for balance 
  theta_val <- stats::median(th_use, na.rm = TRUE)
  threshold_info <- list(threshold_value = theta_val, percentile = 0.5, variable = paste0(idx_col, "_L1"))
  
  message(glue("  Using fixed threshold θ={round(theta_val,3)} (median)"))
  
  # Sanity prints
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
      df               = df_use,       
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
      use_given_split  = FALSE,        
      standardize_y    = FALSE,
      require_both_stable = cfg_est$require_both_stable,
      verbose          = cfg_est$verbose
    ),
    silent = TRUE
  )
  
  if (inherits(est, "try-error")) {
    append_error(IDX, "PANEL", "estimate_all", as.character(est))
    message("Estimation failed → logged"); next
  }
  
  # enrich metadata 
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
  
  # Build Table 1 summary row from `est` (robust)
  K <- length(y_cols)
  
  p_star <- as.integer(pluck_or(est, c("spec","p"), NA_integer_))
  d_star <- as.integer(pluck_or(est, c("spec","delay"), NA_integer_))
  
  # threshold
  c_star <- pluck_or(est, c("threshold_value"), NA_real_)
  if (is.list(c_star)) c_star <- as.numeric(c_star[[1]])
  c_star <- as.numeric(c_star)
  
  # regime counts/shares
  T_L <- as.integer(pluck_or(est, c("regimes","low","n_obs"),  NA_integer_))
  T_H <- as.integer(pluck_or(est, c("regimes","high","n_obs"), NA_integer_))
  s_H <- suppressWarnings(as.numeric(pluck_or(est, c("regime_share","high"), NA_real_)))
  if (!is.finite(s_H) && is.finite(T_L) && is.finite(T_H) && (T_L+T_H)>0) s_H <- T_H/(T_L+T_H)
  
  # Stability: use A (stacked) OR Ak (list), normalize to list and use it 
  K <- length(y_cols)
  A_obj_L <- pluck_or(est, c("regimes","low","Ak"), NULL) %||% pluck_or(est, c("regimes","low","A"), NULL)
  A_obj_H <- pluck_or(est, c("regimes","high","Ak"),NULL) %||% pluck_or(est, c("regimes","high","A"),NULL)
  A_list_L <- normalize_A_list(A_obj_L, K)
  A_list_H <- normalize_A_list(A_obj_H, K)
  lambda_max_L <- tryCatch(spectral_radius_companion(A_list_L), error = function(e) NA_real_)
  lambda_max_H <- tryCatch(spectral_radius_companion(A_list_H), error = function(e) NA_real_)
  
  # Residual whiteness: conservative min Ljung–Box p across series
  res_L <- get_residuals(est, "low")
  res_H <- get_residuals(est, "high")
  lb_p_L <- lb_p_min(res_L)   
  lb_p_H <- lb_p_min(res_H)
  
  # Descriptive sup-LM (likelihood-ratio) vs pooled VAR
  supLM_stat <- NA_real_; supLM_p <- NA_real_
  try({
    Ymat <- as.matrix(df_est[, y_cols, drop = FALSE])
    var_lin <- estimate_var_ols(Ymat, p = p_star)
    
    SigL <- pluck_or(est, c("regimes","low","Sigma"),  NULL)
    SigH <- pluck_or(est, c("regimes","high","Sigma"), NULL)
    if (is.null(SigL) && !is.null(res_L)) SigL <- stats::cov(res_L)
    if (is.null(SigH) && !is.null(res_H)) SigH <- stats::cov(res_H)
    
    if (!is.null(SigL) && !is.null(SigH)) {
      LL_pool  <- -(nrow(var_lin$E)) * safe_logdet(var_lin$Sigma)
      LL_split <- -T_L * safe_logdet(SigL) - T_H * safe_logdet(SigH)
      supLM_stat <- 2 * (LL_split - LL_pool)
      
      df_diff <- K * (K * p_star + 1)  # rough df for contrast
      if (is.finite(supLM_stat) && supLM_stat >= 0 && df_diff > 0) {
        supLM_p <- stats::pchisq(supLM_stat, df = df_diff, lower.tail = FALSE)
        # avoid printing exact 0 due to underflow
        supLM_p <- max(supLM_p, .Machine$double.xmin)
      }
    }
  }, silent = TRUE)
  
  # window label
  end_date <- max(df_use$date, na.rm=TRUE)
  window_label <- if (end_date <= as.Date("2015-04-30")) "1986-2015"
  else if (end_date <= as.Date("2025-04-30")) "1986-2025"
  else paste0(format(min(df_use$date), "%Y-%m"), " to ", format(end_date, "%Y-%m"))
  
  # proxy label
  proxy_label <- if (IDX %in% c("VIX","VXO")) "vxo_vix" else "jln"
  
  summ_row <- tibble::tibble(
    window = window_label, proxy = proxy_label,
    p = p_star, d = d_star, c_star = c_star,
    T_L = T_L, T_H = T_H, s_H = s_H,
    lambda_max_L = lambda_max_L, lambda_max_H = lambda_max_H,
    lb_p_L = lb_p_L, lb_p_H = lb_p_H,
    supLM_stat = supLM_stat, supLM_p = supLM_p
  )
  
  # Write summary row where the Rmd can find it (models/tvar/artifacts/tables)
  out_csv <- fs::path(MODELS_DIR, "artifacts", "tables", "model_selection_summary.csv")
  fs::dir_create(fs::path_dir(out_csv))
  
  # add a timestamp 
  summ_row$run_at <- Sys.time()
  
  if (fs::file_exists(out_csv)) {
    old <- readr::read_csv(out_csv, show_col_types = FALSE)
    old <- dplyr::filter(old, !(window == window_label & proxy == proxy_label))
    new <- dplyr::bind_rows(old, summ_row)
    readr::write_csv(new, out_csv)            
  } else {
    readr::write_csv(summ_row, out_csv)
  }
  message(glue("Table 1 metrics updated in {fs::path_rel(out_csv)}"))
  
  # Save into MODELS_DIR from paths.yml
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stamped <- fs::path(MODELS_DIR, sprintf("%s_tvar_model_%s_%s.rds",
                                          tolower(IDX), ts,
                                          cfg_paths$models$default_save_notes %||% "withRegim_wr"))
  latest  <- fs::path(MODELS_DIR, sprintf("%s_tvar_model_latest.rds", tolower(IDX)))
  
  readr::write_rds(est, stamped, compress = "gz")
  if (fs::file_exists(latest)) try(fs::file_delete(latest), silent = TRUE)
  fs::file_copy(stamped, latest)
  
  message(glue("saved ALL‑COMMODITIES TVAR for {IDX} → {fs::path_rel(stamped)} (K={length(y_cols)})"))
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