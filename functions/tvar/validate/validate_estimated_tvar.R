# functions/tvar/validate/validate_estimated_tvar.R
#' Validate an estimated TVAR object
#'
#' Checks: Sigma positive-definite, stability (roots inside unit circle),
#' and min obs per regime.
#' @param model list returned by estimate_tvar_model()
#' @param tol   max allowed root modulus (< 1). Default 1 - 1e-6
#' @param verbose print summary
#' @return list(ok = TRUE/FALSE, details = data.frame(...), meta = list)
validate_estimated_tvar <- function(model, tol = 1 - 1e-6, verbose = TRUE) {
  stopifnot(is.list(model), "regimes" %in% names(model))
  regs <- model$regimes
  rows <- list()
  
  # companion roots for VAR(p)
  companion_roots <- function(A, k, p) {
    Fm <- matrix(0, nrow = k * p, ncol = k * p)
    Fm[1:k, ] <- A
    if (p > 1) {
      Fm[(k + 1):(k * p), 1:(k * (p - 1))] <- diag(k * (p - 1))
    }
    eigen(Fm, only.values = TRUE)$values
  }
  
  meta <- model$metadata
  vars <- meta$variables
  k    <- length(vars)
  p    <- meta$lag
  
  all_ok <- TRUE
  for (rg in names(regs)) {
    obj <- regs[[rg]]
    if (is.null(obj)) next
    
    # Sigma PD
    Sigma <- obj$Sigma
    eigS  <- try(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values, silent = TRUE)
    sigma_pd <- is.numeric(eigS) && all(eigS > 0, na.rm = TRUE)
    
    # stability
    A <- obj$A
    if (!is.matrix(A) || nrow(A) != k || ncol(A) != (k * p)) {
      return(list(ok = FALSE, details = data.frame(regime = rg, msg = "A matrix dims invalid"),
                  meta = meta))
    }
    vals    <- try(companion_roots(A, k, p), silent = TRUE)
    max_mod <- if (inherits(vals, "try-error")) Inf else max(Mod(vals))
    stable  <- is.finite(max_mod) && (max_mod < tol)
    
    # obs
    nobs    <- obj$n_obs
    enough  <- is.finite(nobs) && (nobs >= max(10, k * p + 5))
    
    ok_rg   <- sigma_pd && stable && enough
    all_ok  <- all_ok && ok_rg
    
    rows[[rg]] <- data.frame(
      regime      = rg,
      n_obs       = as.integer(nobs),
      sigma_pd    = as.logical(sigma_pd),
      max_root    = as.numeric(round(max_mod, 6)),
      stable      = as.logical(stable),
      enough_obs  = as.logical(enough),
      stringsAsFactors = FALSE
    )
  }
  
  details <- if (length(rows)) do.call(rbind, rows) else
    data.frame(regime=character(), n_obs=integer(), sigma_pd=logical(),
               max_root=numeric(), stable=logical(), enough_obs=logical(),
               stringsAsFactors = FALSE)
  
  if (verbose && nrow(details)) print(details)
  list(ok = isTRUE(all(details$stable)) && isTRUE(all(details$enough_obs)),
       details = details, meta = meta)
}