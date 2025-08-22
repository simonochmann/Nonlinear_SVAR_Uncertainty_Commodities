# scripts/patch_analysis_ready_models_for_07.R
suppressPackageStartupMessages({
  library(here); library(glue); library(readr); library(dplyr); library(purrr)
})

`%||%` <- function(x,y) if (is.null(x)) y else x

# --- helpers ---------------------------------------------------------
infer_regime_index <- function(m) {
  tv  <- as.numeric(unlist(m$threshold_value)[1])
  thr <- as.numeric(m$metadata$threshold_series)
  stopifnot(length(thr) == length(m$metadata$dates))
  as.integer(ifelse(thr <= tv, 1L, 2L))  # 1=low, 2=high
}

# Expect your TVAR IRF engine is available:
source(here("functions","tvar","simulate","compute_tvar_irf.R"))

recompute_irfs <- function(m, horizon = 12L, n_draws = 500L,
                           shock_type = "unit", shock_size = 1, verbose = FALSE) {
  # Compute and attach regime IRFs with full horizon
  # compute_tvar_irf() should return a nested list per regime with H-length vectors/matrices.
  m <- compute_tvar_irf(
    model = m,
    horizon = horizon,
    n_draws = n_draws,
    shock_type = shock_type,
    shock_size = shock_size,
    impulse_variable = NULL,   # let your function loop or store all pairs
    verbose = verbose
  )
  m
}

validate_irf_h <- function(irf) {
  # Grab any one impulse-response to infer H
  x <- tryCatch(irf[[1]][[1]], error = function(e) NULL)
  if (is.null(x)) return(0L)
  if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
}

validate_ready_for_07 <- function(m) {
  n_low  <- nrow(as.data.frame(m$regimes$low$Y))
  n_high <- nrow(as.data.frame(m$regimes$high$Y))
  n_tot  <- n_low + n_high
  
  req <- all(
    length(m$variables) >= 1,
    identical(colnames(m$regimes$low$Y), colnames(m$regimes$high$Y)),
    length(m$metadata$dates) == n_tot,
    length(m$metadata$threshold_series) == n_tot,
    !is.null(m$threshold_value), length(m$threshold_value) >= 1
  )
  if (!req) return(FALSE)
  
  ri <- m$metadata$regime_index %||% integer()
  if (length(ri) != n_tot) return(FALSE)
  if (length(unique(ri)) != 2L || !all(sort(unique(ri)) == c(1L,2L))) return(FALSE)
  
  irf <- m$irf %||% m$regimes$low$irf %||% NULL
  if (is.null(irf)) return(FALSE)
  H <- validate_irf_h(irf)
  H >= 2L
}

# --- patch loop ------------------------------------------------------
patch_one <- function(key, horizon = 12L, n_draws = 500L) {
  path <- here("models","tvar", sprintf("%s_tvar_model_analysis_ready_latest.rds", key))
  stopifnot(file.exists(path))
  m <- readRDS(path)
  
  # 1) regime_index
  if (length(m$metadata$regime_index %||% integer()) != length(m$metadata$dates)) {
    m$metadata$regime_index <- infer_regime_index(m)
  }
  
  # 2) IRFs (recompute if H < 2)
  current_irf <- m$irf %||% m$regimes$low$irf %||% NULL
  H_now <- if (is.null(current_irf)) 0L else validate_irf_h(current_irf)
  if (H_now < 2L) {
    m <- recompute_irfs(m, horizon = horizon, n_draws = n_draws, shock_type = "unit", shock_size = 1)
  }
  
  # 3) save & report
  saveRDS(m, path)
  ok <- validate_ready_for_07(m)
  data.frame(index = key, ok = ok, horizon = validate_irf_h(m$irf %||% m$regimes$low$irf %||% NULL))
}

idx <- c("vix","vxo","jln")
res <- purrr::map_dfr(idx, patch_one, horizon = 12L, n_draws = 1000L)
print(res)
