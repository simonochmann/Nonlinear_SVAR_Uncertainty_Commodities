#!/usr/bin/env Rscript
# scripts/06_analyze_regime_dynamics.R
# Quantify regime dynamics: path, durations, transitions, stationary dist + logs/plots
# Multivariate-ready; consumes analysis-ready models from 05
# Enhancements:
# 1) Hysteresis split option (band in SD units)
# 2) Minimum spell length smoothing
# 3) Block bootstrap with block length from median spell length
# 4) QC report (p11/p22, implied durations, persistence)

suppressPackageStartupMessages({
  library(here); library(fs); library(glue); library(dplyr); library(readr)
  library(yaml); library(jsonlite)
})

source("scripts/setup.R")

# Regime helpers
safe_source <- function(path) if (file.exists(path)) source(path)
source(here("functions/tvar/regime/compute_regime_path.R"))
source(here("functions/tvar/regime/compute_regime_durations.R"))
source(here("functions/tvar/regime/compute_transition_matrix.R"))
source(here("functions/tvar/regime/compute_regime_probabilities.R"))
source(here("functions/tvar/regime/summarize_regime_statistics.R"))
source(here("functions/tvar/regime/bootstrap_transition_matrix.R"))

# Plots
source(here("functions/tvar/diagnostics/plot_transition_ci.R"))
source(here("functions/tvar/diagnostics/plot_regime_duration_histogram.R"))
source(here("functions/tvar/diagnostics/plot_transition_matrix.R"))
source(here("functions/tvar/diagnostics/plot_regime_overlay_timeseries.R"))

# Logs
source(here("functions/tvar/logs/log_regime_dynamics_summary.R"))

`%||%` <- function(a,b) if (is.null(a)) b else a

# ------------------------------ Config toggles --------------------------------
cfg <- yaml::read_yaml(here("config/paths.yml"))
active <- tolower(cfg$uncertainty$active %||% "vix")

# IR-related options
CI_BOOTSTRAP_B <- getOption("tvar06.bootstrap_B", 2000L)
CI_BLOCK_LEN   <- getOption("tvar06.block_len",   NULL)   # if NULL, set from median spell length

# New: regime smoothing options
HYS_ON    <- isTRUE(getOption("tvar06.hysteresis_on", FALSE))
HYS_BAND  <- as.numeric(getOption("tvar06.hysteresis_band", 0.15))  # in SD units
MS_ON     <- isTRUE(getOption("tvar06.min_spell_on", FALSE))
MS_LEN    <- as.integer(getOption("tvar06.min_spell_len", 2L))

# ------------------------------ Paths -----------------------------------------
root_dir   <- here::here()
model_dir  <- file.path(root_dir, "models", "tvar")
output_dir <- file.path(root_dir, "output")
reg_dir    <- file.path(output_dir, "regime", active)
plot_dir   <- file.path(output_dir, "plots", "tvar")
log_dir    <- file.path(output_dir, "logs")
fs::dir_create(c(output_dir, reg_dir, plot_dir, log_dir))

# ------------------------------ Internal helpers ------------------------------
unwrap_model <- function(x) {
  if (!is.list(x)) return(NULL)
  if (!is.null(x$regimes)) return(x)
  if (!is.null(x$model) && !is.null(x$model$regimes)) return(x$model)
  NULL
}

# Hysteresis split on a continuous series around threshold
apply_hysteresis_split <- function(series, thr, band_width = 0.15) {
  s <- as.numeric(series)
  s_sd <- stats::sd(s, na.rm = TRUE)
  if (!is.finite(s_sd) || s_sd == 0) s_sd <- 1
  band <- band_width * s_sd
  low_to_high  <- thr + band
  high_to_low  <- thr - band
  idx <- rep(NA_integer_, length(s))
  # initialize
  first <- which(!is.na(s))[1]
  if (length(first)) {
    idx[first] <- if (s[first] <= thr) 1L else 2L
  } else {
    idx[] <- 1L
    return(idx)
  }
  for (t in seq_len(length(s))) {
    if (is.na(s[t])) { idx[t] <- if (t>1) idx[t-1] else 1L; next }
    if (t == 1L || is.na(idx[t-1])) { idx[t] <- if (s[t] <= thr) 1L else 2L; next }
    if (idx[t-1] == 1L) {
      idx[t] <- if (s[t] >  low_to_high) 2L else 1L
    } else {
      idx[t] <- if (s[t] <  high_to_low) 1L else 2L
    }
  }
  idx
}

# Minimum spell length enforcement (push micro-spells to neighbors)
enforce_min_spell_length <- function(idx, min_len = 2L) {
  if (min_len <= 1L) return(idx)
  r <- rle(idx)
  too_short <- r$lengths < min_len
  if (!any(too_short)) return(idx)
  for (i in which(too_short)) {
    if (i == 1L && length(r$lengths) > 1L) {
      r$values[i] <- r$values[i+1L]
    } else if (i == length(r$lengths)) {
      r$values[i] <- r$values[i-1L]
    } else {
      left  <- r$lengths[i-1L]
      right <- r$lengths[i+1L]
      r$values[i] <- if (right >= left) r$values[i+1L] else r$values[i-1L]
    }
  }
  inverse.rle(r)
}

# --- Rebuild TM directly from regime_index when needed ---
rebuild_transition_from_index <- function(idx) {
  lv <- c(1L, 2L)
  x <- as.integer(idx)
  x <- x[is.finite(x)]
  x[!(x %in% lv)] <- NA_integer_
  x <- stats::na.omit(x)
  if (length(x) < 2L) {
    M <- matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2, dimnames = list(c("low","high"), c("low","high")))
    return(M)
  }
  from <- x[-length(x)]
  to   <- x[-1L]
  # counts
  C <- matrix(0, 2, 2, dimnames = list(c("low","high"), c("low","high")))
  for (i in seq_along(from)) {
    fi <- from[i]; ti <- to[i]
    if (fi %in% lv && ti %in% lv) {
      C[c("low","high")[fi], c("low","high")[ti]] <- C[c("low","high")[fi], c("low","high")[ti]] + 1
    }
  }
  # row-normalize to probabilities; handle zero rows
  rs <- rowSums(C)
  P <- C
  for (r in rownames(C)) {
    if (rs[r] > 0) {
      P[r, ] <- C[r, ] / rs[r]
    } else {
      P[r, ] <- 0.5  # neutral fallback
    }
  }
  P
}

is_bad_tm <- function(M) {
  is.null(M) || any(!is.finite(M)) || any(abs(rowSums(M) - 1) > 1e-6)
}

# ---- Transition-matrix coercion & QC-safe access ----
coerce_transition_matrix <- function(P) {
  # Accepts:
  #  - 2x2 matrix/data.frame (with/without dimnames)
  #  - tidy data.frame with columns: from, to, p
  # Returns a 2x2 numeric matrix with dimnames c("low","high")
  if (is.null(P)) return(NULL)
  
  as_mat <- function(x) {
    m <- as.matrix(x)
    storage.mode(m) <- "double"
    m
  }
  
  # Tidy long-form?
  if (is.data.frame(P) && all(c("from","to","p") %in% names(P))) {
    lv <- c("low","high")
    # normalize labels
    norm <- function(v) {
      v <- tolower(as.character(v))
      v[v %in% c("1","low","l","regime1")]  <- "low"
      v[v %in% c("2","high","h","regime2")] <- "high"
      factor(v, levels = lv)
    }
    P$from <- norm(P$from); P$to <- norm(P$to)
    M <- matrix(0, nrow = 2, ncol = 2, dimnames = list(lv, lv))
    for (i in seq_len(nrow(P))) {
      fi <- as.character(P$from[i]); ti <- as.character(P$to[i])
      if (fi %in% lv && ti %in% lv && is.finite(P$p[i])) {
        M[fi, ti] <- as.numeric(P$p[i])
      }
    }
    return(M)
  }
  
  # Already matrix-like?
  if (is.matrix(P) || (is.data.frame(P) && ncol(P) == 2L && nrow(P) == 2L) ||
      (is.data.frame(P) && ncol(P) == 2L && nrow(P) >= 2L)) {
    M <- as_mat(P)[1:2, 1:2, drop = FALSE]  # ensure 2x2
    rn <- rownames(M); cn <- colnames(M)
    
    fix_labels <- function(nms) {
      if (is.null(nms)) return(NULL)
      v <- tolower(nms)
      v[v %in% c("1","low","l","regime1")]  <- "low"
      v[v %in% c("2","high","h","regime2")] <- "high"
      v
    }
    
    rn2 <- fix_labels(rn); cn2 <- fix_labels(cn)
    
    # If either side is still NULL or not recognizable, assume order 1=low,2=high
    if (is.null(rn2) || !all(rn2 %in% c("low","high"))) rn2 <- c("low","high")
    if (is.null(cn2) || !all(cn2 %in% c("low","high"))) cn2 <- c("low","high")
    
    rownames(M) <- rn2; colnames(M) <- cn2
    # Reorder to standard layout
    M <- M[c("low","high"), c("low","high"), drop = FALSE]
    return(M)
  }
  
  # Unknown type → try best-effort coercion
  M <- suppressWarnings(as.matrix(P))
  if (is.null(dim(M)) || any(dim(M) < 2)) return(NULL)
  M <- M[1:2, 1:2, drop = FALSE]
  rownames(M) <- colnames(M) <- c("low","high")
  M
}

safe_p_diag <- function(P) {
  P <- coerce_transition_matrix(P)
  if (is.null(P)) return(c(p11 = NA_real_, p22 = NA_real_))
  c(p11 = as.numeric(P["low","low"]), p22 = as.numeric(P["high","high"]))
}

report_tm <- function(P) {
  P <- coerce_transition_matrix(P)
  if (is.null(P)) { cat("[QC] Transition matrix unavailable.\n"); return(invisible()) }
  pd <- safe_p_diag(P)
  if (!is.finite(pd["p11"]) || !is.finite(pd["p22"])) { cat("[QC] Diagonals not finite.\n"); return(invisible()) }
  d1 <- 1/(1 - pd["p11"]); d2 <- 1/(1 - pd["p22"])
  cat(sprintf("[QC] p11=%.3f p22=%.3f | implied mean durations: low=%.2f, high=%.2f\n",
              pd["p11"], pd["p22"], d1, d2))
  cat(sprintf("[QC] Persistence metric p11+p22=%.3f (stationary if > 1)\n",
              pd["p11"] + pd["p22"]))
  invisible(P)
}

# ------------------------------ Load model ------------------------------------
prefer_path <- fs::path(model_dir, sprintf("%s_tvar_model_analysis_ready_latest.rds", active))
stamped <- list.files(model_dir, pattern = sprintf("^%s_tvar_model_.*\\.rds$", active), full.names = TRUE)

if (fs::file_exists(prefer_path)) {
  candidate_paths <- c(prefer_path, setdiff(stamped, prefer_path)[order(file.info(setdiff(stamped, prefer_path))$mtime, decreasing = TRUE)])
} else {
  candidate_paths <- stamped[order(file.info(stamped)$mtime, decreasing = TRUE)]
}
stopifnot(length(candidate_paths) > 0)

picked_path <- NA_character_
model <- NULL
for (p in candidate_paths) {
  obj <- try(readRDS(p), silent = TRUE)
  if (inherits(obj, "try-error")) next
  m <- unwrap_model(obj)
  if (is.null(m) || is.null(m$regimes$low$Y) || is.null(m$regimes$high$Y)) next
  picked_path <- p; model <- m; break
}
if (is.na(picked_path)) stop("[06] No compatible TVAR model found for active = ", active)
message("[06] Using model: ", fs::path_file(picked_path))

# ------------------------------ Normalize dates -------------------------------
n_total <- (if (!is.null(model$regimes$low$Y))  nrow(model$regimes$low$Y)  else 0L) +
  (if (!is.null(model$regimes$high$Y)) nrow(model$regimes$high$Y) else 0L)

if (is.null(model$metadata$dates) || length(model$metadata$dates) != n_total) {
  model$metadata$dates <- seq_len(n_total)
}

if (!is.null(model$metadata$regime_index)) {
  idx <- as.integer(model$metadata$regime_index)
  uniq <- unique(idx)
  if (length(idx) != n_total || length(uniq) < 2 || any(!idx %in% c(1L, 2L))) {
    message("[06] Discarding stale regime_index (len=", length(idx),
            ", uniq=", paste(uniq, collapse=","), "); will recompute from threshold series.")
    model$metadata$regime_index <- NULL
  }
}

# ------------------------------ Build regime path -----------------------------
# compute_regime_path uses threshold_series + threshold_value (median fallback)
model <- compute_regime_path(model, inject = TRUE, verbose = TRUE)

# Guard against leftover length mismatches
n_total <- (if (!is.null(model$regimes$low$Y))  nrow(model$regimes$low$Y)  else 0L) +
  (if (!is.null(model$regimes$high$Y)) nrow(model$regimes$high$Y) else 0L)

if (is.null(model$metadata$dates) || length(model$metadata$dates) != n_total) {
  model$metadata$dates <- seq_len(n_total)
}

if (length(model$metadata$regime_index) != n_total) {
  thr <- as.numeric(model$metadata$threshold_series %||% numeric())
  thr_val <- tryCatch(
    if (length(model$threshold_value)) as.numeric(unlist(model$threshold_value)[1]) else NA_real_,
    error = function(e) NA_real_
  )
  if (length(thr) == 0L) {
    model$metadata$regime_index <- rep(1L, n_total)
  } else {
    thr <- thr[seq_len(min(length(thr), n_total))]
    if (!is.finite(thr_val)) thr_val <- stats::median(thr, na.rm = TRUE)
    idx <- ifelse(thr <= thr_val, 1L, 2L)
    if (length(idx) < n_total) idx <- c(idx, rep(idx[length(idx)], n_total - length(idx)))
    if (length(idx) > n_total) idx <- idx[seq_len(n_total)]
    model$metadata$regime_index <- idx
  }
  message("[06] Rebuilt regime_index to length n_total = ", n_total, " from threshold series.")
}
stopifnot(length(model$metadata$regime_index) == n_total)

# ------------------------------ Enhancements: Hysteresis & Min-Spell ----------
# Start from baseline regime_index and optionally refine
if (HYS_ON) {
  thr_series <- model$metadata$threshold_series %||% numeric()
  thr_value  <- tryCatch(as.numeric(unlist(model$threshold_value)[1]), error = function(e) NA_real_)
  if (length(thr_series) == n_total && is.finite(thr_value)) {
    idx_hyst <- apply_hysteresis_split(thr_series, thr = thr_value, band_width = HYS_BAND)
    if (length(idx_hyst) == n_total) {
      model$metadata$regime_index <- idx_hyst
      message(glue("[06] Applied hysteresis split (band = {HYS_BAND} × SD)."))
    }
  } else {
    message("[06] Hysteresis requested but missing threshold series/value; skipping.")
  }
}

if (MS_ON && MS_LEN > 1L) {
  idx_raw <- model$metadata$regime_index
  idx_smooth <- enforce_min_spell_length(idx_raw, min_len = MS_LEN)
  model$metadata$regime_index <- idx_smooth
  message(glue("[06] Enforced minimum spell length = {MS_LEN}."))
}

# ------------------------------ Persist run metadata --------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_meta <- list(
  active        = active,
  model_path    = picked_path,
  script        = "scripts/06_analyze_regime_dynamics.R",
  stamp         = stamp,
  R             = R.version.string,
  n_low         = nrow(model$regimes$low$Y),
  n_high        = nrow(model$regimes$high$Y),
  variables     = model$variables,
  hysteresis_on = HYS_ON, hysteresis_band = HYS_BAND,
  min_spell_on  = MS_ON,  min_spell_len  = MS_LEN
)
jsonlite::write_json(run_meta, file.path(reg_dir, glue("regime_runmeta_{active}_{stamp}.json")),
                     auto_unbox = TRUE, pretty = TRUE)

# ------------------------------ Durations / Transitions -----------------------
dur_out   <- compute_regime_durations(model = model, verbose = TRUE)

reg_trans_raw <- compute_transition_matrix(model = model)
reg_trans     <- coerce_transition_matrix(reg_trans_raw)

if (is_bad_tm(reg_trans)) {
  message("[06] Transition matrix malformed/empty — rebuilding from regime_index.")
  reg_trans <- rebuild_transition_from_index(model$metadata$regime_index)
}

# Final sanity (force exact row-stochastic form)
reg_trans[,"low"]  <- pmax(0, reg_trans[,"low"])
reg_trans[,"high"] <- pmax(0, reg_trans[,"high"])
reg_trans <- sweep(reg_trans, 1, rowSums(reg_trans), FUN = "/")
reg_trans[!is.finite(reg_trans)] <- 0.5  # covers any 0/0 rows

reg_prob <- compute_regime_probabilities(transition_matrix = reg_trans)

# QC report
report_tm(reg_trans)

# ------------------------------ Bootstrap CIs --------------------------------
boot <- NULL
if (!is.null(model$metadata$regime_index)) {
  # Choose block length:
  #  - If user supplied CI_BLOCK_LEN, use it.
  #  - Else set to median spell length (at least 2) for persistence-aware bootstrap.
  block_len_auto <- tryCatch(
    {
      dt <- dur_out$durations_tbl %||% dur_out$spells
      ml <- median(dt$duration, na.rm = TRUE)
      ml <- if (!is.finite(ml)) 2L else max(2L, as.integer(round(ml)))
      ml
    },
    error = function(e) 2L
  )
  block_len <- CI_BLOCK_LEN %||% block_len_auto
  message(glue("[06] Bootstrap: B={CI_BOOTSTRAP_B}, block_len={block_len} ",
               if (!is.null(CI_BLOCK_LEN)) "(user)" else "(auto from median spell)"))
  boot <- bootstrap_transition_matrix(
    model$metadata$regime_index,
    B = CI_BOOTSTRAP_B, block_len = block_len, seed = 123, verbose = TRUE
  )
}

# ------------------------------ One-row summary -------------------------------
reg_sum <- summarize_regime_statistics(
  model                 = model,
  durations             = list(spells = dur_out$durations_tbl %||% dur_out$spells),
  transition_matrix     = if (!is.null(boot)) boot$tm_tidy else reg_trans,
  regime_probabilities  = reg_prob
)

# ------------------------------ Exports ---------------------------------------
# 1) Transition matrix (tidy)
if (!is.null(boot)) {
  readr::write_csv(boot$tm_tidy, file.path(reg_dir, glue("transition_matrix_{active}_{stamp}.csv")))
} else if (!is.null(reg_trans)) {
  trans_df <- tibble::as_tibble(reg_trans, rownames = "from") |>
    tidyr::pivot_longer(-from, names_to = "to", values_to = "p")
  readr::write_csv(trans_df, file.path(reg_dir, glue("transition_matrix_{active}_{stamp}.csv")))
}

# 2) Transition CIs
if (!is.null(boot)) {
  readr::write_csv(boot$ci, file.path(reg_dir, glue("transition_ci_{active}_{stamp}.csv")))
  try(plot_transition_ci(boot$ci,
                         save_path = file.path(plot_dir, glue("transition_ci_{active}_{stamp}.png"))),
      silent = TRUE)
}

# 3) One-row summary
readr::write_csv(reg_sum, file.path(reg_dir, glue("regime_summary_{active}_{stamp}.csv")))

# ------------------------------ Plots -----------------------------------------
try(plot_regime_duration_histogram(
  (dur_out$durations_tbl %||% dur_out$spells),
  save_path = file.path(plot_dir, glue("regime_duration_hist_{active}_{stamp}.png"))
), silent = TRUE)

try(plot_transition_matrix(
  reg_trans,
  save_path = file.path(plot_dir, glue("transition_matrix_{active}_{stamp}.png"))
), silent = TRUE)

if (!is.null(boot) && !is.null(reg_trans)) {
  try(plot_transition_ci(
    reg_trans, boot,
    save_path = file.path(plot_dir, glue("transition_ci_overlay_{active}_{stamp}.png"))
  ), silent = TRUE)
}

# Overlay timeseries (pick a safe variable from the multivariate set)
try({
  series_name <- model$variables[1]
  low_df  <- as.data.frame(model$regimes$low$Y)
  high_df <- as.data.frame(model$regimes$high$Y)
  y <- c(as.numeric(low_df[[series_name]]), as.numeric(high_df[[series_name]]))
  plot_regime_overlay_timeseries(
    y,
    dates = model$metadata$dates,
    regime_index = model$metadata$regime_index,
    title = glue("{toupper(active)} — {series_name}: regimes over time"),
    save_path = file.path(plot_dir, glue("regime_overlay_{active}_{series_name}_{stamp}.png"))
  )
}, silent = TRUE)

# ------------------------------ Log (Markdown) --------------------------------
log_regime_dynamics_summary(
  output_dir        = log_dir,
  filename_md       = glue("regime_dynamics_{active}_{stamp}.md"),
  model_path        = picked_path,
  regime_summary    = reg_sum,
  transition_matrix = reg_trans
)

# ------------------------------ Persist model ---------------------------------
short <- substr(tools::file_path_sans_ext(fs::path_file(picked_path)), 1, 50)
out_norm <- file.path(model_dir, glue("{short}_regdyn_{active}_{stamp}.rds"))
saveRDS(model, out_norm)

message("[06] Regime dynamics complete → ", reg_dir)