# functions/tvar/utils/summarize_tvar_models.R

# Optional, but handy if something else defined it already
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Scalar guards -----------------------------------------------------------
.safe_num1 <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  out <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(out)) return(default)
  out
}
.safe_int1 <- function(x, default = NA_integer_) {
  as.integer(.safe_num1(x, default))
}
.safe_lgl1 <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  val <- as.logical(x)[1]
  if (is.na(val)) default else val
}

# ---- Spectral radius from VAR(p) coeffs -------------------------------------
# A_flat: k x (k*p) stacked by lag blocks horizontally
.comp_rho_from_A <- function(A_flat, k, p) {
  if (is.null(A_flat)) return(NA_real_)
  A_flat <- as.matrix(A_flat)
  stopifnot(ncol(A_flat) == k * p, nrow(A_flat) == k)
  
  # top block: k x (k*p)
  A_blocks <- lapply(seq_len(p), function(L) {
    cols <- ((L - 1) * k + 1):(L * k)
    A_flat[, cols, drop = FALSE]
  })
  A_top <- do.call(cbind, A_blocks)
  
  if (p == 1) {
    M <- A_top
  } else {
    lower <- cbind(diag(k * (p - 1)), matrix(0, nrow = k * (p - 1), ncol = k))
    M <- rbind(A_top, lower)   # (k*p) x (k*p)
  }
  vals <- eigen(M, only.values = TRUE)$values
  max(Mod(vals))
}

# ---- Summarize a single TVAR model file -------------------------------------
summarize_tvar <- function(path) {
  m <- readRDS(path)
  
  # Some wrappers save as list(model=..., meta=...). Unwrap if needed.
  if (!is.null(m$model) && is.list(m$model)) m <- m$model
  
  # K, p, delay
  k <- if (!is.null(m$metadata$variables)) {
    length(m$metadata$variables)
  } else if (!is.null(m$regimes$low$A)) {
    nrow(m$regimes$low$A)
  } else NA_integer_
  
  p <- if (!is.null(m$spec$p)) {
    .safe_int1(m$spec$p)
  } else {
    .safe_int1(m$metadata$lag)
  }
  
  delay <- .safe_int1(m$spec$delay)
  
  # Theta (threshold) and percentile (be very defensive)
  theta <- if (!is.null(m$spec$theta)) {
    .safe_num1(m$spec$theta)
  } else {
    .safe_num1(m$threshold_value)
  }
  theta_pct <- .safe_num1(m$theta_percentile)  # older models may not have it
  
  # Regime sample sizes
  low_obs  <- .safe_int1(m$regimes$low$n_obs)
  high_obs <- .safe_int1(m$regimes$high$n_obs)
  
  # Params per equation: intercept + k*p
  kpar <- 1L + as.integer(k) * as.integer(p)
  
  # Stability (use stored if available else compute)
  if (!is.null(m$stability)) {
    low_rho     <- .safe_num1(m$stability$low$rho)
    high_rho    <- .safe_num1(m$stability$high$rho)
    low_stable  <- .safe_lgl1(m$stability$low$stable)
    high_stable <- .safe_lgl1(m$stability$high$stable)
  } else {
    low_rho  <- if (!is.na(k) && !is.na(p)) .safe_num1(.comp_rho_from_A(m$regimes$low$A,  k, p)) else NA_real_
    high_rho <- if (!is.na(k) && !is.na(p)) .safe_num1(.comp_rho_from_A(m$regimes$high$A, k, p)) else NA_real_
    low_stable  <- is.finite(low_rho)  && low_rho  < 1
    high_stable <- is.finite(high_rho) && high_rho < 1
  }
  
  data.frame(
    file            = basename(path),
    K               = .safe_int1(k),
    p               = .safe_int1(p),
    delay           = .safe_int1(delay),
    theta           = .safe_num1(theta),
    theta_pct       = .safe_num1(theta_pct),
    low_obs         = .safe_int1(low_obs),
    high_obs        = .safe_int1(high_obs),
    df_low_per_eq   = .safe_int1(low_obs)  - .safe_int1(kpar),
    df_high_per_eq  = .safe_int1(high_obs) - .safe_int1(kpar),
    low_rho         = round(.safe_num1(low_rho),  3),
    high_rho        = round(.safe_num1(high_rho), 3),
    low_stable      = .safe_lgl1(low_stable),
    high_stable     = .safe_lgl1(high_stable),
    stringsAsFactors = FALSE
  )
}

# ---- Batch: find models, summarize, write CSV --------------------------------
summarize_all_tvar_models <- function() {
  paths <- sort(Sys.glob("models/tvar/*_tvar_model_latest.rds"))
  if (!length(paths)) {
    paths <- sort(Sys.glob("models/tvar/*_tvar_model_*_withRegim*.rds"))
  }
  if (!length(paths)) {
    message("No TVAR model files found in models/tvar/")
    return(invisible(NULL))
  }
  
  rows <- lapply(paths, function(p) {
    tryCatch(
      summarize_tvar(p),
      error = function(e) {
        message("! summarize_tvar failed for ", basename(p), ": ", conditionMessage(e))
        NULL
      }
    )
  })
  
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    message("No summaries produced.")
    return(invisible(NULL))
  }
  
  summ_df <- do.call(rbind, rows)
  print(summ_df, row.names = FALSE)
  
  dir.create("logs/models", recursive = TRUE, showWarnings = FALSE)
  out_csv <- "logs/models/tvar_summary_latest.csv"
  utils::write.csv(summ_df, out_csv, row.names = FALSE)
  message("✓ Wrote TVAR summary → ", out_csv)
  
  invisible(summ_df)
}

# If sourced interactively, run once:
if (sys.nframe() == 0L) {
  summarize_all_tvar_models()
}
