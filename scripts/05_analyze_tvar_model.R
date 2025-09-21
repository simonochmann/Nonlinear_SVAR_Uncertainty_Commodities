# scripts/05_analyze_tvar_model.R
# Purpose: Analyze a fitted (multivariate) TVAR model with diagnostics, IRFs, and logging

suppressPackageStartupMessages({
  library(here); library(glue); library(dplyr); library(fs); library(readr)
  library(jsonlite); library(purrr); library(yaml)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

.normalize_id <- function(x) tolower(gsub("[^a-z0-9]+", "_", x))

.extract_threshold_info <- function(m, th_series = NULL) {
  theta <- suppressWarnings(as.numeric(m$spec$theta %||% m$threshold_value))
  tpct  <- suppressWarnings(as.numeric(m$theta_percentile))
  if (!is.finite(tpct) && length(th_series)) {
    tpct <- stats::ecdf(th_series[is.finite(th_series)])(theta)
  }
  list(theta = theta, theta_pct = if (is.finite(tpct)) tpct else NA_real_)
}

# Sources
source(here("scripts/setup.R"))

# logs
source(here("functions/tvar/logs/log_tvar_model_summary.R"))
source(here("functions/tvar/logs/log_tvar_irf_metadata.R"))

# diagnostics
source(here("functions/tvar/diagnostics/plot_tvar_threshold_split.R"))
source(here("functions/tvar/diagnostics/plot_tvar_regime_paths.R"))
source(here("functions/tvar/diagnostics/plot_tvar_fitted_vs_actual.R"))
source(here("functions/tvar/diagnostics/plot_tvar_irf_regimes.R"))

# validate
source(here("functions/tvar/validate/validate_tvar_coefficient_matrix.R"))

# simulate / orchestrate
source(here("functions/tvar/simulate/compute_tvar_irf.R"))
source(here("functions/tvar/run/run_tvar_irf_analysis.R"))

# Config & paths
cfg <- yaml::read_yaml(here("config/paths.yml"))
`%||%` <- function(a,b) if (is.null(a)) b else a
ts_tag <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

root_dir      <- here()
model_dir     <- fs::path(root_dir, "models", "tvar")
output_dir    <- fs::path(root_dir, "output")
log_dir       <- fs::path(output_dir, "logs")
plots_dir     <- fs::path(output_dir, "plots", "tvar")
irf_plot_dir  <- fs::path(output_dir, "plots", "irfs")
irf_obj_dir   <- fs::path(output_dir, "irf")
meta_dir      <- fs::path(output_dir, "metadata")
fs::dir_create(c(output_dir, log_dir, plots_dir, irf_plot_dir, irf_obj_dir, meta_dir, model_dir))

active_unc <- tolower(cfg$uncertainty$active %||% "vix")

# Helpers
has_regimes <- function(x) {
  is.list(x) && ((!is.null(x$regimes) && is.list(x$regimes)) ||
                   (!is.null(x$model) && !is.null(x$model$regimes) && is.list(x$model$regimes)))
}
unwrap_model <- function(x) {
  if (!is.list(x)) return(NULL)
  if (!is.null(x$regimes)) return(x)
  if (!is.null(x$model) && !is.null(x$model$regimes)) return(x$model)
  NULL
}
first_nonempty <- function(...) { xs <- list(...); for (x in xs) if (!is.null(x) && length(x) && all(nzchar(x))) return(x); NULL }
as_df_preserve <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (is.data.frame(obj)) return(obj)
  cn <- colnames(obj); df <- as.data.frame(obj)
  if (is.null(colnames(df))) colnames(df) <- if (!is.null(cn)) cn else paste0("series_", seq_len(ncol(df)))
  df
}
infer_lag_from_name <- function(nm) { if (is.null(nm) || !nzchar(nm)) return(0L); m <- regexpr("_L(\\d+)$", nm, perl = TRUE); if (m[1] < 0) return(0L); as.integer(sub(".*_L(\\d+)$", "\\1", nm)) }
align_threshold_series_len <- function(s, n_total, name_hint=NULL, p_hint=NULL) {
  s <- as.numeric(s); p <- as.integer(p_hint %||% infer_lag_from_name(name_hint) %||% 0L)
  if (p > 0L && length(s) == (n_total - p)) s <- c(rep(NA_real_, p), s)
  if (length(s) < n_total) s <- c(rep(NA_real_, n_total - length(s)), s)
  if (length(s) > n_total) s <- utils::tail(s, n_total)
  stopifnot(length(s) == n_total); s
}
build_regime_index_safe <- function(thr_series, thr_value) {
  split <- ifelse(thr_series <= as.numeric(thr_value), 1L, 2L)
  if (anyNA(split)) {
    for (i in seq_along(split)) if (is.na(split[i]) && i > 1L) split[i] <- split[i-1L]
    for (i in seq_along(split)) { j <- length(split) - i + 1L; if (is.na(split[j]) && j < length(split)) split[j] <- split[j+1L] }
    if (all(is.na(split))) split[] <- 1L
  }
  as.integer(split)
}
infer_h_from_obj <- function(obj) {
  if (is.null(obj)) return(0L)
  if (is.numeric(obj)) return(length(obj))
  if (is.matrix(obj) || inherits(obj, "data.frame")) return(nrow(obj))
  if (is.list(obj) && length(obj)) for (el in obj) { h <- infer_h_from_obj(el); if (h > 0L) return(h) }
  0L
}
locate_irf_blob <- function(m) {
  candidates <- list(m$irf, m$IRF, m$irfs, m$regimes$low$irf, m$regimes$high$irf, m$regimes$low$IRF, m$regimes$high$IRF)
  for (blob in candidates) if (!is.null(blob) && infer_h_from_obj(blob) > 1L) return(blob)
  NULL
}
canonicalize_irf <- function(m) {
  blob_low  <- m$regimes$low$irf  %||% m$regimes$low$IRF
  blob_high <- m$regimes$high$irf %||% m$regimes$high$IRF
  if (is.null(blob_low) && is.null(blob_high)) {
    blob <- locate_irf_blob(m)
    if (!is.null(blob) && is.list(blob) && all(c("low","high") %in% names(blob))) { blob_low <- blob$low; blob_high <- blob$high } else { blob_low <- blob_high <- blob }
  }
  to_vec <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x)) return(as.numeric(x))
    if (is.matrix(x) || inherits(x, "data.frame")) return(as.numeric(x[,1]))
    NULL
  }
  canon_side <- function(sb) {
    if (is.null(sb)) return(NULL)
    out <- list()
    if (is.list(sb)) {
      for (imp in names(sb)) {
        if (!nzchar(imp)) next
        if (is.list(sb[[imp]])) {
          for (rsp in names(sb[[imp]])) {
            v <- to_vec(sb[[imp]][[rsp]])
            if (!is.null(v)) {
              out[[imp]] <- out[[imp]] %||% list()
              out[[imp]][[rsp]] <- v
            }
          }
        } else {
          v <- to_vec(sb[[imp]]); if (!is.null(v)) out[[imp]] <- list(!!imp := v)
        }
      }
    }
    if (length(out)) out else NULL
  }
  m$irf <- list(low = canon_side(blob_low), high = canon_side(blob_high)); m
}
h_from_canonical <- function(m) {
  x <- tryCatch(m$irf$low[[1]][[1]], error = function(e) NULL)
  if (is.null(x)) x <- tryCatch(m$irf$high[[1]][[1]], error = function(e) NULL)
  if (is.null(x)) return(0L) else length(x)
}

# Smarter CSV Harvest 
.is_integerish_seq <- function(v) {
  v <- as.numeric(v)
  if (!length(v) || any(!is.finite(v))) return(FALSE)
  seq0 <- seq(from = round(v[1]), by = 1, length.out = length(v))
  all(abs(v - seq0) < 1e-8)
}
.pick_value_col <- function(d) {
  num <- names(d)[sapply(d, is.numeric)]
  if (!length(num)) return(NULL)
  lower <- tolower(num)
  pref_order <- c("value","irf","response","y")
  hit <- intersect(pref_order, lower)
  if (length(hit)) return(num[match(hit[1], lower)])
  non_h <- num[!vapply(d[num], .is_integerish_seq, logical(1))]
  if (length(non_h)) return(non_h[1])
  if (length(num) >= 2) {
    idx <- seq_len(nrow(d))
    cors <- vapply(d[num], function(col)
      suppressWarnings(abs(cor(idx, as.numeric(col), use = "complete.obs"))),
      numeric(1))
    return(num[which.min(cors)])
  }
  num[1]
}
harvest_irf_from_csv <- function(m, csv_dir, Hreq = NA_integer_) {
  if (!fs::dir_exists(csv_dir)) return(m)
  files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) return(m)
  
  parse_meta <- function(fn) {
    b <- fs::path_file(fn)
    regime <- if (grepl("(^|_)low(_|\\.)", b, TRUE)) "low"
    else if (grepl("(^|_)high(_|\\.)", b, TRUE)) "high"
    else NA_character_
    imp <- sub(".*?([A-Za-z0-9_]+)_to_([A-Za-z0-9_]+)\\.csv$", "\\1", b)
    rsp <- sub(".*?([A-Za-z0-9_]+)_to_([A-Za-z0-9_]+)\\.csv$", "\\2", b)
    list(regime = regime, impulse = imp, response = rsp)
  }
  
  irf_build <- list(low=list(), high=list())
  
  for (f in files) {
    meta <- parse_meta(f)
    if (is.na(meta$regime) || !nzchar(meta$impulse) || !nzchar(meta$response)) next
    
    d <- tryCatch(readr::read_csv(f, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    
    val_col <- .pick_value_col(d)
    if (is.null(val_col)) next
    
    v <- as.numeric(d[[val_col]])
    if (.is_integerish_seq(v)) next
    if (is.finite(Hreq) && length(v) >= Hreq) v <- v[seq_len(Hreq)]
    if (!is.numeric(v) || length(v) < 2 || all(!is.finite(v))) next
    if (sd(v[is.finite(v)]) < 1e-12) next
    
    irf_build[[meta$regime]][[meta$impulse]] <- irf_build[[meta$regime]][[meta$impulse]] %||% list()
    irf_build[[meta$regime]][[meta$impulse]][[meta$response]] <- v
    
    if (getOption("tvar.debug.harvest", TRUE)) {
      rng <- range(v, na.rm = TRUE)
      message(sprintf("[05][CSV] %s  pick='%s'  len=%d  min=%.4g  max=%.4g",
                      fs::path_file(f), val_col, length(v), rng[1], rng[2]))
    }
  }
  
  if (length(irf_build$low) || length(irf_build$high)) m$irf <- irf_build
  m
}

# Name utils
.norm_name <- function(x) { x <- tolower(x); gsub("[^a-z0-9]+", "", x) }
.strip_regime_prefix <- function(x) sub("^(tvar_)?irf_(low|high)_", "", x, perl = TRUE)

# Build plot-ready item and return raw vectors
build_plot_ready_irf <- function(irf, H, verbose = TRUE) {
  stopifnot(is.list(irf$low), is.list(irf$high))
  imps_low  <- names(irf$low);  imps_high <- names(irf$high)
  base_low  <- setNames(sapply(imps_low,  .strip_regime_prefix), imps_low)
  base_high <- setNames(sapply(imps_high, .strip_regime_prefix), imps_high)
  base_common <- intersect(unname(base_low), unname(base_high))
  if (!length(base_common)) return(NULL)
  for (b in base_common) {
    il <- names(base_low )[which(base_low  == b)][1]
    ih <- names(base_high)[which(base_high == b)][1]
    rs_l <- names(irf$low [[il]] %||% list())
    rs_h <- names(irf$high[[ih]] %||% list())
    rs_common <- intersect(rs_l, rs_h)
    if (!length(rs_common) && b %in% union(rs_l, rs_h)) rs_common <- b
    if (!length(rs_common)) next
    rsp <- if (b %in% rs_common) b else rs_common[1]
    v1 <- irf$low [[il]][[rsp]]; v2 <- irf$high[[ih]][[rsp]]
    if (!(is.numeric(v1) && length(v1) > 1 && is.numeric(v2) && length(v2) > 1)) next
    if (is.finite(H)) {
      if (length(v1) > H) v1 <- v1[seq_len(H)]
      if (length(v2) > H) v2 <- v2[seq_len(H)]
    }
    if (verbose) message(glue("[05] IRF plot pair → base='{b}', response='{rsp}'"))
    irf_compat <- list(
      regimes = list(
        low  = list(summary = setNames(list(setNames(list(v1), rsp)), b)),
        high = list(summary = setNames(list(setNames(list(v2), rsp)), b))
      )
    )
    return(list(compat = irf_compat, impulse = b, response = rsp, v_low = v1, v_high = v2))
  }
  NULL
}

# Robust fallback plotter
plot_irf_regimes_fallback <- function(v_low, v_high, impulse, response, horizon, save_path) {
  v_low  <- as.numeric(v_low)
  v_high <- as.numeric(v_high)
  
  trim <- function(v) {
    i <- which(is.finite(v))
    if (!length(i)) return(numeric(0))
    v[min(i):max(i)]
  }
  v_low  <- trim(v_low)
  v_high <- trim(v_high)
  
  if (length(v_low)  > horizon) v_low  <- v_low[ seq_len(horizon) ]
  if (length(v_high) > horizon) v_high <- v_high[seq_len(horizon)]
  
  bad <- function(v) !length(v) || all(!is.finite(v)) || sd(v[is.finite(v)]) < 1e-12 || .is_integerish_seq(v)
  if (bad(v_low) || bad(v_high)) {
    message("[05][fallback] Skipping degenerate IRF series for ", impulse, "→", response)
    return(invisible(FALSE))
  }
  
  H <- max(length(v_low), length(v_high))
  x <- seq(0, H - 1)
  
  extend_to <- function(v, H) { c(v, rep(NA_real_, max(0, H - length(v)))) }
  v_low  <- extend_to(v_low,  H)
  v_high <- extend_to(v_high, H)
  
  dir.create(fs::path_dir(save_path), showWarnings = FALSE, recursive = TRUE)
  grDevices::png(save_path, width = 1280, height = 720, res = 120)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  y_all <- c(v_low, v_high); y_all <- y_all[is.finite(y_all)]
  y_pad <- 0.05 * diff(range(y_all)); if (!is.finite(y_pad)) y_pad <- 0
  
  oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
  par(mar = c(4.2, 4.8, 3.2, 1.2))
  plot(x, v_low, type = "n",
       xlab = "Horizon (periods)", ylab = "IRF",
       main = sprintf("IRF: %s → %s (low vs high regime)", impulse, response),
       ylim = range(y_all) + c(-y_pad, y_pad))
  abline(h = 0, lty = 3)
  lines(x, v_low,  lwd = 2)
  lines(x, v_high, lwd = 2, lty = 2)
  legend("topright", legend = c("Low regime", "High regime"),
         lwd = 2, lty = c(1, 2), bty = "n")
  
  message("Saved fallback IRF plot to: ", fs::path_abs(save_path))
  invisible(TRUE)
}

save_model_variants <- function(model, cfg, notes = NULL, verbose = TRUE) {
  dir_models <- fs::path_abs(cfg$models$dir %||% fs::path("models","tvar"))
  fs::dir_create(dir_models); stopifnot(fs::dir_exists(dir_models))
  active <- tolower(cfg$uncertainty$active %||% "vix")
  notes  <- notes %||% cfg$models$default_save_notes %||% "analysis_ready"
  ts     <- ts_tag()
  stamped_path <- fs::path(dir_models, sprintf("%s_tvar_model_%s_%s.rds", active, ts, notes))
  latest_path  <- fs::path(dir_models, sprintf("%s_tvar_model_latest.rds", active))
  ar_latest    <- fs::path(dir_models, sprintf("%s_tvar_model_analysis_ready_latest.rds", active))
  model$metadata <- c(model$metadata %||% list(),
                      list(created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                           uncertainty_active = active, save_notes = notes))
  readr::write_rds(model, stamped_path, compress = "gz"); stopifnot(fs::file_exists(stamped_path))
  refresh_pointer <- function(target) {
    if (fs::file_exists(target)) try(fs::file_delete(target), silent = TRUE)
    tmp <- fs::path_dir(target) |> fs::path(sprintf(".tmp_%s_%s.rds", active, ts))
    readr::write_rds(model, tmp, compress = "gz"); fs::file_move(tmp, target)
  }
  refresh_pointer(latest_path); refresh_pointer(ar_latest)
  if (isTRUE(verbose)) {
    message("[05] Saved stamped: ", fs::path_rel(stamped_path))
    message("[05] Latest      → ", fs::path_rel(latest_path))
    message("[05] AnalysisReady→ ", fs::path_rel(ar_latest))
  }
  invisible(list(stamped = stamped_path, latest = latest_path, analysis_ready = ar_latest))
}

# Load newest compatible TVAR model
pattern <- sprintf("^%s_tvar_model_.*\\.rds$|^%s_tvar_model_latest\\.rds$", active_unc, active_unc)
candidate_paths <- list.files(model_dir, pattern = pattern, full.names = TRUE)
if (!length(candidate_paths)) stop("No TVAR model found for active=", active_unc, " in models/tvar.")
candidate_paths <- candidate_paths[order(file.info(candidate_paths)$mtime, decreasing = TRUE)]

picked_path <- NA_character_; raw_obj <- NULL; model_core <- NULL; skipped <- character(0)
for (p in candidate_paths) {
  obj <- try(readRDS(p), silent = TRUE)
  if (inherits(obj, "try-error")) { skipped <- c(skipped, basename(p)); next }
  if (!has_regimes(obj))         { skipped <- c(skipped, basename(p)); next }
  mc <- unwrap_model(obj); if (is.null(mc)) { skipped <- c(skipped, basename(p)); next }
  picked_path <- p; raw_obj <- obj; model_core <- mc; break
}
if (is.na(picked_path)) stop("No *compatible* TVAR RDS (with $regimes) found. Skipped: ", paste(skipped, collapse=", "))
message("[05] Using model: ", fs::path_file(picked_path))
if (length(skipped)) message("[05] Skipped incompatible: ", paste(skipped, collapse=", "))

tvar_model <- raw_obj

# Normalize schema, names, dates
model_core <- if (!is.null(tvar_model$regimes)) tvar_model else tvar_model$model
wish_names <- first_nonempty(tvar_model$variables, model_core$variables)

normalize_Y <- function(Y, k, prefer) {
  cur <- colnames(Y)
  if (!is.null(prefer) && length(prefer) == k) {
    colnames(Y) <- prefer
  } else if (is.null(cur) || any(!nzchar(cur)) || length(cur) != k) {
    colnames(Y) <- paste0("series_", seq_len(k))
  }
  Y
}
if (!is.null(model_core$regimes$low$Y)) {
  model_core$regimes$low$Y <- as_df_preserve(model_core$regimes$low$Y)
  k_low <- ncol(model_core$regimes$low$Y)
  model_core$regimes$low$Y <- normalize_Y(model_core$regimes$low$Y, k_low, wish_names)
}
if (!is.null(model_core$regimes$high$Y)) {
  model_core$regimes$high$Y <- as_df_preserve(model_core$regimes$high$Y)
  k_high <- ncol(model_core$regimes$high$Y)
  low_names <- if (!is.null(model_core$regimes$low$Y)) colnames(model_core$regimes$low$Y) else wish_names
  model_core$regimes$high$Y <- normalize_Y(model_core$regimes$high$Y, k_high, low_names)
}
if (!is.null(model_core$Y)) {
  model_core$Y <- as_df_preserve(model_core$Y)
  k <- ncol(model_core$Y)
  prefer <- first_nonempty(if (!is.null(model_core$regimes$low$Y)) colnames(model_core$regimes$low$Y) else NULL, wish_names)
  model_core$Y <- normalize_Y(model_core$Y, k, prefer)
}
if (!is.null(model_core$regimes$low$fitted))  model_core$regimes$low$fitted  <- as_df_preserve(model_core$regimes$low$fitted)
if (!is.null(model_core$regimes$high$fitted)) model_core$regimes$high$fitted <- as_df_preserve(model_core$regimes$high$fitted)

if (is.null(model_core$variables)) {
  model_core$variables <- first_nonempty(
    if (!is.null(model_core$regimes$low$Y))  colnames(model_core$regimes$low$Y)  else NULL,
    if (!is.null(model_core$regimes$high$Y)) colnames(model_core$regimes$high$Y) else NULL
  )
}
stopifnot(length(model_core$variables) >= 1)

n_total <- (if (!is.null(model_core$regimes$low$Y))  nrow(model_core$regimes$low$Y)  else 0L) +
  (if (!is.null(model_core$regimes$high$Y)) nrow(model_core$regimes$high$Y) else 0L)
if (is.null(model_core$metadata$dates) || length(model_core$metadata$dates) != n_total) {
  model_core$metadata$dates <- seq_len(n_total)
}
time_index <- model_core$metadata$dates

# Threshold metadata
lowX  <- if (!is.null(model_core$regimes$low$X))  as_df_preserve(model_core$regimes$low$X)  else NULL
highX <- if (!is.null(model_core$regimes$high$X)) as_df_preserve(model_core$regimes$high$X) else NULL
X_all <- bind_rows(lowX, highX)
regime_var_hint <- first_nonempty(
  model_core$metadata$threshold_var, tvar_model$regime_variable, model_core$regime_variable,
  paste0(toupper(active_unc), "_L1"), paste0(tolower(active_unc), "_L1"), active_unc
)
choose_threshold_column <- function(df, hint) {
  if (is.null(df) || !nrow(df) || !ncol(df)) return(NULL)
  cn <- colnames(df)
  cand <- c(hint, paste0(hint,"_L1"), tolower(hint), paste0(tolower(hint),"_L1"),
            toupper(hint), paste0(toupper(hint),"_L1"),
            "value_L1","threshold","thresh","regime_var")
  pick <- intersect(unique(cand), cn)
  if (!length(pick)) return(NULL)
  list(name = pick[1], series = df[[pick[1]]])
}
thr_pick <- choose_threshold_column(X_all, regime_var_hint)

threshold_val_raw <- tryCatch({
  if (!is.null(tvar_model$threshold_value)) {
    if (is.list(tvar_model$threshold_value)) as.numeric(tvar_model$threshold_value[[1]]) else as.numeric(tvar_model$threshold_value)
  } else if (!is.null(tvar_model$model) && !is.null(tvar_model$model$Thresh)) {
    as.numeric(tvar_model$model$Thresh)
  } else NA_real_
}, error = function(e) NA_real_)

align_threshold_value <- function(series, tv_raw) {
  if (!length(series)) return(list(tv = tv_raw, space = "unknown"))
  c0 <- suppressWarnings(attr(series, "scaled:center"))
  s0 <- suppressWarnings(attr(series, "scaled:scale"))
  if (is.null(c0) || is.null(s0) || !is.finite(c0) || !is.finite(s0) || s0 == 0) return(list(tv = tv_raw, space = "raw"))
  list(tv = (tv_raw - c0) / s0, space = "scaled")
}

thr_series <- NULL; thr_name <- NULL; thr_space <- "unknown"; thr_value <- threshold_val_raw
if (!is.null(thr_pick)) {
  thr_series <- thr_pick$series; thr_name <- thr_pick$name
  aligned <- align_threshold_value(thr_series, threshold_val_raw)
  thr_value <- aligned$tv; thr_space <- aligned$space
  rng <- range(thr_series, na.rm = TRUE)
  if (is.finite(thr_value) && (thr_value <= min(rng) || thr_value >= max(rng))) {
    message(glue("[05] Aligned threshold {round(thr_value,4)} outside [{round(rng[1],4)}, {round(rng[2],4)}]; using median."))
    thr_value <- stats::median(thr_series, na.rm = TRUE); thr_space <- paste0(thr_space, "+median_fallback")
  }
} else if (!is.null(model_core$metadata$threshold_series)) {
  thr_series <- model_core$metadata$threshold_series
  thr_name   <- model_core$metadata$threshold_var %||% regime_var_hint %||% "threshold_var"
  thr_space  <- "metadata"
  rng <- range(thr_series, na.rm = TRUE)
  tv_raw <- suppressWarnings(as.numeric(unlist(model_core$threshold_value)[1]))
  thr_value <- if (!is.finite(tv_raw) || tv_raw <= min(rng) || tv_raw >= max(rng)) stats::median(thr_series, na.rm = TRUE) else tv_raw
  model_core$threshold_value <- list(as.numeric(thr_value))
}

if (!is.null(thr_series)) {
  thr_num <- as.numeric(thr_series)
  invisible(plot_tvar_threshold_split(
    df = setNames(data.frame(value = thr_num), thr_name),
    regime_var = thr_name, threshold = thr_value,
    threshold_percentile = if (is.finite(thr_value)) NULL else "50%",
    bins = 30, log_stats = TRUE, show_density = TRUE, show_rug = TRUE,
    save_path = fs::path(plots_dir, glue("tvar_threshold_split_{thr_name}.png"))
  ))
  model_core$metadata <- model_core$metadata %||% list()
  model_core$metadata$dates           <- model_core$metadata$dates %||% time_index
  model_core$metadata$threshold_var   <- thr_name
  model_core$metadata$threshold_space <- thr_space
  model_core$threshold_value          <- list(as.numeric(thr_value))
  n_tot  <- length(model_core$metadata$dates)
  p_hint <- model_core$p %||% model_core$lags %||% NULL
  model_core$metadata$threshold_series <- align_threshold_series_len(thr_num, n_total = n_tot, name_hint = thr_name, p_hint = p_hint)
  model_core$metadata$regime_index <- build_regime_index_safe(
    thr_series = model_core$metadata$threshold_series,
    thr_value  = as.numeric(unlist(model_core$threshold_value)[1])
  )
}

# Ensure logger sees threshold
tvar_model$threshold_value <- model_core$threshold_value
tvar_model$regime_variable <- model_core$metadata$threshold_var

# Log model summary
log_tvar_model_summary(
  model     = tvar_model,
  log_path  = fs::path(log_dir, "tvar_model_summary.md"),
  log_level = "info",
  style     = "markdown",
  verbose   = TRUE
)

# Core diagnostics
names_low  <- if (!is.null(model_core$regimes$low$Y))  colnames(model_core$regimes$low$Y)  else character(0)
names_high <- if (!is.null(model_core$regimes$high$Y)) colnames(model_core$regimes$high$Y) else character(0)
common_vars <- intersect(names_low, names_high)
var_name <- dplyr::coalesce(common_vars[1], names_low[1], names_high[1])
stopifnot(!is.na(var_name), nzchar(var_name))

invisible(plot_tvar_regime_paths(
  model       = model_core, variable = var_name, time_index = NULL,
  show_fitted = TRUE, show_residuals = FALSE,
  save_path   = fs::path(plots_dir, glue("tvar_regime_paths_{var_name}.png")),
  title       = glue("TVAR Regime Path for '{var_name}'"),
  subtitle    = "With Fitted Overlay and Regime Shading",
  verbose     = TRUE
))
invisible(plot_tvar_fitted_vs_actual(
  model         = model_core, variable = var_name,
  show_fit_lines= TRUE, show_residuals= TRUE,
  save_path     = fs::path(plots_dir, glue("tvar_fitted_vs_actual_{var_name}.png")),
  verbose       = TRUE
))

ok_low <- try({
  if (!is.null(model_core$regimes$low$A)) {
    validate_tvar_coefficient_matrix(model_core$regimes$low$A, expected_k = NULL, expected_p = NULL)
  } else TRUE
}, silent = TRUE)
if (inherits(ok_low, "try-error")) message("[05] Coefficient matrix check (low) failed (non-fatal).") else message("[05] Coefficient matrix check (low) passed.")

# IRFs + canonicalize + clamp + plot
HORIZON  <- 12L
N_DRAWS  <- 1000L
CI_LEVEL <- 0.90
SHOCK_TYPE <- "unit"
SHOCK_SIZE <- 1

if (exists("run_tvar_irf_analysis")) {
  model_core <- run_tvar_irf_analysis(
    model       = model_core,
    horizon     = HORIZON,
    n_draws     = N_DRAWS,
    ci_level    = CI_LEVEL,
    shock_type  = SHOCK_TYPE,
    shock_size  = SHOCK_SIZE,
    output_dir  = irf_plot_dir,
    save_csv    = TRUE,
    verbose     = TRUE
  )
} else {
  model_core <- compute_tvar_irf(
    model            = model_core,
    horizon          = HORIZON,
    n_draws          = N_DRAWS,
    shock_type       = SHOCK_TYPE,
    shock_size       = SHOCK_SIZE,
    impulse_variable = NULL,
    verbose          = TRUE
  )
}

# canonicalize & harvest if needed
model_core <- canonicalize_irf(model_core)
H_now <- h_from_canonical(model_core)
if (H_now < 2L) {
  message(glue("[05] IRF horizon detected as {H_now}. CSV harvest attempt..."))
  model_core <- harvest_irf_from_csv(model_core, csv_dir = irf_plot_dir, Hreq = HORIZON)
  model_core <- canonicalize_irf(model_core)
  H_now <- h_from_canonical(model_core)
}

# Quick audit line 
if (is.list(model_core$irf$low) && length(model_core$irf$low)) {
  imp0 <- names(model_core$irf$low)[1]
  rsp0 <- names(model_core$irf$low[[imp0]])[1]
  v0l <- model_core$irf$low [[imp0]][[rsp0]]
  v0h <- model_core$irf$high[[imp0]][[rsp0]]
  message(glue("[05][audit] first pair {imp0}->{rsp0}: len(low)={length(v0l)}, len(high)={length(v0h)} ",
               "min(low)={round(min(v0l, na.rm=TRUE),4)} max(low)={round(max(v0l, na.rm=TRUE),4)} ",
               "min(high)={round(min(v0h,na.rm=TRUE),4)} max(high)={round(max(v0h,na.rm=TRUE),4)}"))
}

# clamp to horizon
clamp_irf <- function(irf, H) {
  if (is.null(irf)) return(irf)
  for (side in c("low","high")) if (is.list(irf[[side]]))
    for (imp in names(irf[[side]]))
      for (rsp in names(irf[[side]][[imp]])) {
        v <- irf[[side]][[imp]][[rsp]]
        if (is.numeric(v) && length(v) > H) irf[[side]][[imp]][[rsp]] <- v[seq_len(H)]
      }
  irf
}
model_core$irf <- clamp_irf(model_core$irf, HORIZON)
H_now <- min(H_now, HORIZON)
message(glue("[05] Canonical IRF horizon = {H_now}"))

# pick a pair and try plotting
plot_pkg <- build_plot_ready_irf(model_core$irf, H = HORIZON, verbose = TRUE)
if (is.null(plot_pkg)) {
  warning("[05] No common impulse/response pair found; skipping IRF plot.")
} else {
  save_path <- fs::path(irf_plot_dir, glue("tvar_irf_regimes_{plot_pkg$impulse}_to_{plot_pkg$response}.png"))
  ok <- TRUE
  tryCatch({
    # Try native plotter first
    invisible(plot_tvar_irf_regimes(
      model     = list(irf = plot_pkg$compat),
      impulse   = plot_pkg$impulse,
      response  = plot_pkg$response,
      horizon   = HORIZON,
      ci_level  = CI_LEVEL,
      save_path = save_path,
      verbose   = TRUE
    ))
  }, error = function(e) {
    message("[05] plot_tvar_irf_regimes failed: ", conditionMessage(e), " — using fallback.")
    ok <<- FALSE
  })
  if (!ok) {
    plot_irf_regimes_fallback(
      v_low    = plot_pkg$v_low,
      v_high   = plot_pkg$v_high,
      impulse  = plot_pkg$impulse,
      response = plot_pkg$response,
      horizon  = HORIZON,
      save_path = save_path
    )
  }
}

# Export all impulse/response pairs with safe fallback
export_all_irf_pairs_safe <- function(irf, horizon, ci_level, out_dir) {
  stopifnot(is.list(irf$low), is.list(irf$high))
  .strip <- function(x) sub("^(tvar_)?irf_(low|high)_", "", x, perl = TRUE)
  
  imps_low  <- names(irf$low);   imps_high <- names(irf$high)
  base_low  <- setNames(sapply(imps_low,  .strip), imps_low)
  base_high <- setNames(sapply(imps_high, .strip), imps_high)
  base_imps <- intersect(unname(base_low), unname(base_high))
  if (!length(base_imps)) { message("[05] No common impulses across regimes."); return(invisible()) }
  
  .clamp <- function(v, H) if (is.numeric(v) && length(v) > H) v[seq_len(H)] else v
  
  n_saved <- 0L
  for (b in base_imps) {
    il <- names(base_low )[which(base_low  == b)][1]
    ih <- names(base_high)[which(base_high == b)][1]
    rs_l <- names(irf$low [[il]] %||% list())
    rs_h <- names(irf$high[[ih]] %||% list())
    rs_common <- intersect(rs_l, rs_h)
    if (b %in% union(rs_l, rs_h)) rs_common <- union(b, rs_common)
    if (!length(rs_common)) next
    
    for (rsp in rs_common) {
      v1 <- irf$low [[il]][[rsp]]; v2 <- irf$high[[ih]][[rsp]]
      if (!(is.numeric(v1) && is.numeric(v2) && length(v1) > 1 && length(v2) > 1)) next
      v1 <- .clamp(v1, horizon); v2 <- .clamp(v2, horizon)
      
      compat <- list(
        regimes = list(
          low  = list(summary = setNames(list(setNames(list(v1), rsp)), b)),
          high = list(summary = setNames(list(setNames(list(v2), rsp)), b))
        )
      )
      save_path <- fs::path(out_dir, glue("tvar_irf_regimes_{b}_to_{rsp}.png"))
      
      ok <- TRUE
      tryCatch({
        invisible(plot_tvar_irf_regimes(
          model     = list(irf = compat),
          impulse   = b,
          response  = rsp,
          horizon   = horizon,
          ci_level  = ci_level,
          save_path = save_path,
          verbose   = FALSE
        ))
      }, error = function(e) { ok <<- FALSE })
      
      if (!ok) {
        plot_irf_regimes_fallback(
          v_low    = v1,
          v_high   = v2,
          impulse  = b,
          response = rsp,
          horizon  = horizon,
          save_path = save_path
        )
      }
      n_saved <- n_saved + 1L
    }
  }
  message(glue("[05] Exported {n_saved} IRF plot(s) with safe fallback."))
  invisible(n_saved)
}

# Save IRF-embedded snapshot + metadata
readr::write_rds(model_core, fs::path(irf_obj_dir, glue("tvar_with_irfs_{ts_tag()}.rds")), compress = "gz")
log_tvar_irf_metadata(
  model         = model_core,
  output_dir    = log_dir,
  log_file_json = "tvar_irf_metadata.json",
  log_file_md   = "tvar_irf_metadata.md",
  verbose       = TRUE
)

invisible(export_all_irf_pairs_safe(
  irf      = model_core$irf,
  horizon  = HORIZON,
  ci_level = CI_LEVEL,
  out_dir  = irf_plot_dir
))

# Readiness stamp + persist normalized / analysis-ready model
validate_irf_h <- function(irf) {
  if (is.null(irf)) return(0L)
  any_resp <- tryCatch(irf[[1]][[1]], error = function(e) NULL)
  if (is.null(any_resp)) return(0L)
  if (is.matrix(any_resp) || is.data.frame(any_resp)) nrow(any_resp) else length(any_resp)
}
validate_ready_for_06_09 <- function(m) {
  lowY  <- as_df_preserve(m$regimes$low$Y);  highY <- as_df_preserve(m$regimes$high$Y)
  n_low <- if (!is.null(lowY)) nrow(lowY) else 0L; n_high <- if (!is.null(highY)) nrow(highY) else 0L
  n_tot <- n_low + n_high
  ok_basic <- all(
    length(m$variables) >= 1, !is.null(lowY), !is.null(highY),
    identical(colnames(lowY), colnames(highY)),
    length(m$metadata$dates) == n_tot,
    length(m$metadata$threshold_series) == n_tot,
    !is.null(m$threshold_value), length(m$threshold_value) >= 1
  )
  if (!ok_basic) return(list(ok=FALSE, reason="basic_schema"))
  ri <- m$metadata$regime_index %||% integer()
  if (length(ri) != n_tot || !all(sort(unique(ri)) == c(1L,2L))) return(list(ok=FALSE, reason="regime_index"))
  H <- validate_irf_h(m$irf); if (H < 2L) return(list(ok=FALSE, reason="irf_horizon", H=H))
  list(ok=TRUE, reason="ready", H=H)
}
stamp_readiness <- function(m, out_dir, name) {
  chk <- validate_ready_for_06_09(m)
  obj <- list(created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
              uncertainty = active_unc, ok = chk$ok, reason = chk$reason, horizon = chk$H %||% NA_integer_)
  write_json(obj, fs::path(out_dir, sprintf("tvar_readiness_%s.json", name)), pretty = TRUE, auto_unbox = TRUE)
  writeLines(c(paste0("# TVAR Readiness (", name, ")"),
               paste0("- ok: ", obj$ok),
               paste0("- reason: ", obj$reason),
               paste0("- horizon: ", obj$horizon),
               paste0("- timestamp: ", obj$created_at)),
             fs::path(out_dir, sprintf("tvar_readiness_%s.md", name)))
  invisible(obj)
}
ready <- stamp_readiness(model_core, out_dir = meta_dir, name = active_unc)
message(glue("[05] Readiness → ok={ready$ok}, reason={ready$reason}, horizon={ready$horizon}"))

norm_path <- fs::path(model_dir, glue("{tools::file_path_sans_ext(fs::path_file(picked_path))}_normalized_{ts_tag()}.rds"))
readr::write_rds(model_core, norm_path, compress = "gz")
message("[05] Saved normalized copy: ", fs::path_rel(norm_path))
save_model_variants(model_core, cfg = cfg, notes = cfg$models$default_save_notes %||% "analysis_ready")

message("[05] TVAR analysis complete.")