# functions/tvar/utils/get_resid_cov.R
# Robust covariance retriever for TVAR workflows with regime-aware combination.
# Order of attempts:
#   1) model$Sigma_u (or similar top-level) → validate
#   2) Regime residuals stacked (low+high) → cov() → validate
#   3) Regime Σ matrices (low/high) → size-weighted average → validate
#   4) Shallow residuals at top level → cov() → validate
#   5) Deep (nested) residuals by name → cov() → validate
#   6) Identity fallback

get_resid_cov <- function(model,
                          var_names = NULL,
                          prefer_fields = c("Sigma_u", "resid_cov", "cov", "covariance"),
                          resid_fields   = c("residuals", "u", "errors", "e", "innovations", "eps", "epsilon"),
                          allow_identity = TRUE,
                          min_eig = 1e-8,
                          verbose = TRUE) {
  
  nv   <- function(x) is.null(x) || length(x) == 0
  .msg <- function(...) if (isTRUE(verbose)) message("[get_resid_cov] ", sprintf(...))
  
  # Resolve variable order
  vars <- var_names
  if (nv(vars) && !is.null(model$variables)) vars <- as.character(model$variables)
  if (!nv(vars)) vars <- as.character(vars)
  
  # --- helpers ----------------------------------------------------------------
  to_mat <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.vector(x) && !is.matrix(x)) {
      n <- length(x); d <- as.integer(sqrt(n))
      if (d * d != n) return(NULL)
      x <- matrix(x, d, d)
    }
    if (!is.matrix(x)) return(NULL)
    mode(x) <- "numeric"; x
  }
  sym  <- function(S) (S + t(S)) / 2
  fix_psd <- function(S, eps = min_eig) {
    S <- sym(S)
    if (any(!is.finite(S))) { .msg("Non-finite entries; setting to 0 before PSD fix."); S[!is.finite(S)] <- 0 }
    ev <- try(eigen(S, symmetric = TRUE), silent = TRUE)
    if (inherits(ev, "try-error")) return(S)
    vals <- pmax(ev$values, eps)
    S2 <- ev$vectors %*% diag(vals, length(vals)) %*% t(ev$vectors)
    dimnames(S2) <- dimnames(S)
    sym(S2)
  }
  align <- function(S, vars) {
    if (nv(S)) return(NULL)
    if (!is.null(rownames(S))) {
      if (is.null(colnames(S))) colnames(S) <- rownames(S)
    } else if (!is.null(colnames(S))) {
      rownames(S) <- colnames(S)
    }
    if (!nv(vars)) {
      if (!is.null(colnames(S))) {
        keep <- vars[vars %in% colnames(S)]
        if (length(keep)) {
          S <- S[keep, keep, drop = FALSE]
          if (length(keep) < length(vars)) .msg("Missing vars in covariance: %s", paste(setdiff(vars, keep), collapse = ", "))
          rownames(S) <- colnames(S) <- keep
        } else if (ncol(S) == length(vars)) {
          rownames(S) <- colnames(S) <- vars
        }
      } else if (ncol(S) == length(vars)) {
        rownames(S) <- colnames(S) <- vars
      }
    }
    S
  }
  cov_of <- function(R) {
    R <- to_mat(R); if (is.null(R)) return(NULL)
    stats::cov(R, use = "pairwise.complete.obs")
  }
  
  # --- 1) top-level stored covariances ---------------------------------------
  for (fld in prefer_fields) if (!is.null(model[[fld]])) {
    S <- align(to_mat(model[[fld]]), vars)
    if (!nv(S) && any(is.finite(S))) {
      S <- fix_psd(S); .msg("Using stored '%s' (k=%d).", fld, ncol(S)); return(S)
    }
  }
  
  # --- 2) regime residuals stacked (preferred for 'combined') -----------------
  stacked <- NULL; n_low <- n_high <- NA_integer_
  if (!nv(model$regimes) && is.list(model$regimes)) {
    Rlow  <- to_mat(model$regimes$low$residuals)
    Rhigh <- to_mat(model$regimes$high$residuals)
    if (!nv(Rlow) || !nv(Rhigh)) {
      # align columns if names exist
      if (!nv(vars)) {
        if (!nv(colnames(Rlow)))  Rlow  <- Rlow[,  vars[vars %in% colnames(Rlow)],  drop = FALSE]
        if (!nv(colnames(Rhigh))) Rhigh <- Rhigh[, vars[vars %in% colnames(Rhigh)], drop = FALSE]
        if (!nv(colnames(Rlow)))  Rlow  <- Rlow[,  match(colnames(Rlow),  vars), drop = FALSE]
        if (!nv(colnames(Rhigh))) Rhigh <- Rhigh[, match(colnames(Rhigh), vars), drop = FALSE]
      }
      n_low  <- if (!nv(Rlow))  nrow(Rlow)  else 0L
      n_high <- if (!nv(Rhigh)) nrow(Rhigh) else 0L
      stacked <- rbind(Rlow, Rhigh)
      S <- cov_of(stacked)
      S <- align(to_mat(S), vars)
      if (!nv(S)) { S <- fix_psd(S); .msg("Using cov(residuals) from regimes (n_low=%s, n_high=%s).", n_low, n_high); return(S) }
    }
  }
  
  # --- 3) regime Σ matrices weighted by sample size --------------------------
  if (!nv(model$regimes) && is.list(model$regimes)) {
    Slow  <- align(to_mat(model$regimes$low$Sigma ), vars)
    Shigh <- align(to_mat(model$regimes$high$Sigma), vars)
    if (!nv(Slow) || !nv(Shigh)) {
      if (is.na(n_low))  n_low  <- if (!nv(model$regimes$low$residuals))  nrow(model$regimes$low$residuals)  else 0L
      if (is.na(n_high)) n_high <- if (!nv(model$regimes$high$residuals)) nrow(model$regimes$high$residuals) else 0L
      wlow  <- max(n_low - 1L, 0L)
      whigh <- max(n_high - 1L, 0L)
      S <- NULL
      if (!nv(Slow) && !nv(Shigh)) {
        denom <- max(wlow + whigh, 1L)
        S <- (wlow * Slow + whigh * Shigh) / denom
      } else if (!nv(Slow)) {
        S <- Slow
      } else if (!nv(Shigh)) {
        S <- Shigh
      }
      if (!nv(S)) { S <- fix_psd(S); .msg("Using weighted Σ from regimes (wlow=%s, whigh=%s).", wlow, whigh); return(S) }
    }
  }
  
  # --- 4) shallow residuals on the model root --------------------------------
  for (rf in resid_fields) if (!nv(model[[rf]])) {
    S <- cov_of(model[[rf]]); S <- align(to_mat(S), vars)
    if (!nv(S)) { S <- fix_psd(S); .msg("Using cov(%s) at root.", rf); return(S) }
  }
  
  # --- 5) deep (named) residuals anywhere ------------------------------------
  find_named_residuals <- function(x, keys = tolower(resid_fields)) {
    if (!is.list(x)) return(NULL)
    nms <- names(x); if (is.null(nms)) return(NULL)
    # only return when the NAME matches a residual key
    for (i in seq_along(x)) {
      nm <- tolower(nms[i]); xi <- x[[i]]
      if (!is.null(nm) && nm %in% keys && (is.matrix(xi) || is.data.frame(xi))) return(xi)
      if (is.list(xi)) {
        got <- find_named_residuals(xi, keys)
        if (!is.null(got)) return(got)
      }
    }
    NULL
  }
  Rdeep <- find_named_residuals(model)
  if (!nv(Rdeep)) {
    S <- cov_of(Rdeep); S <- align(to_mat(S), vars)
    if (!nv(S)) { S <- fix_psd(S); .msg("Using cov(residuals) from deep search."); return(S) }
  }
  
  # --- 6) identity ------------------------------------------------------------
  if (isTRUE(allow_identity)) {
    k <- if (!nv(vars)) length(vars) else ncol(to_mat(model$regimes$low$Sigma) %||% to_mat(model$regimes$high$Sigma) %||% diag(1))
    I <- diag(1, k); if (!nv(vars) && length(vars) == k) { rownames(I) <- colnames(I) <- vars }
    .msg("Falling back to identity covariance (k=%d). Consider storing model$Sigma_u.", k)
    return(I)
  }
  stop("[get_resid_cov] Could not determine a residual covariance and allow_identity = FALSE.")
}