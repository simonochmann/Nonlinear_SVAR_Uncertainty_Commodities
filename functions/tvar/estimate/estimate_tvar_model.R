#' Estimate Threshold VAR (TVAR) Model with Grid Selection
#'
#' Fits separate VAR(p) models for each regime defined by a threshold split.
#' Selects (p, delay, theta) on a grid using summed BIC across equations/regimes,
#' with trimming and minimum regime share constraints.
#'
#' @param df           Data frame with a Date column `date` and k numeric variables (e.g., ret, vol_proxy).
#' @param th_series    Numeric vector (same length as df) with the threshold series q_t (before delay).
#' @param threshold_info Optional list from `select_tvar_threshold()`; if it contains
#'                       $split, that split value is used when `use_given_split = TRUE`.
#' @param lags_set     Integer vector of VAR lags p to try (default 2:6).
#' @param delays_set   Integer vector of delays d for q_{t-d} (default 1:3).
#' @param trim         Trimming proportion for split search (default 0.15).
#' @param ngrid        Number of candidate splits to evaluate (default 100).
#' @param criterion    Selection criterion ("BIC" or "AIC"; default "BIC").
#' @param min_regime_share Minimum share per regime (0-1) required at the selected split (default 0.15).
#' @param use_given_split If TRUE and threshold_info$split is present, do not search theta; use the provided split.
#' @param standardize_y If TRUE, standardize Y columns (mean 0, sd 1) before estimation. Threshold stays in raw units.
#' @param verbose      Print progress and brief summaries (default TRUE).
#'
#' @return A list:
#'   - regimes: list(low=..., high=...) each with A, intercept, residuals, Sigma, Y, X, n_obs, AIC, BIC, etc.
#'   - threshold_value: selected theta (raw units)
#'   - regime_variable: tag from threshold_info$tag if present, else NA
#'   - spec: list(p, delay, criterion, theta)
#'   - regime_share: c(low=., high=.)
#'   - metadata: variables, lag, total_obs_used, total_obs_full, y_means, y_sds, standardized
estimate_tvar_model <- function(
    df,
    th_series,
    threshold_info   = NULL,
    lags_set         = 2:6,
    delays_set       = 1:3,
    trim             = 0.15,
    ngrid            = 100,
    criterion        = c("BIC","AIC"),
    min_regime_share = 0.15,
    use_given_split  = FALSE,
    standardize_y    = FALSE,
    verbose          = TRUE
) {
  criterion <- match.arg(criterion)
  
  # basic checks
  stopifnot("date" %in% names(df))
  vars <- setdiff(names(df), "date")
  if (length(vars) < 2) stop("Need at least two y-variables in df (besides 'date').")
  if (!all(vapply(df[vars], is.numeric, logical(1L))))
    stop("All non-'date' columns in df must be numeric.")
  if (!is.numeric(th_series) || length(th_series) != nrow(df))
    stop("th_series must be numeric vector with same length as nrow(df).")
  
  # standardize Y (threshold stays raw)
  y_means <- vapply(df[vars], function(x) mean(x, na.rm = TRUE), numeric(1))
  y_sds   <- vapply(df[vars], function(x) stats::sd(x, na.rm = TRUE), numeric(1))
  y_sds[!is.finite(y_sds) | y_sds <= .Machine$double.eps] <- 1
  
  df_work <- df
  if (isTRUE(standardize_y)) {
    df_work[vars] <- sweep(sweep(df_work[vars], 2, y_means, "-"), 2, y_sds, "/")
  }
  
  # build lagged matrices on STANDARDIZED (or raw) Y
  Y0   <- as.matrix(df_work[vars])
  N0   <- nrow(Y0)
  k    <- ncol(Y0)
  pmax <- max(lags_set)
  dmax <- max(delays_set)
  
  if (N0 <= (pmax + dmax + 5)) stop("Too few observations for requested pmax/delay.")
  
  # Embed once: rows correspond to t = (pmax+1) ... N0
  Z <- stats::embed(Y0, pmax + 1)
  Y_full <- Z[, 1:k, drop = FALSE]
  colnames(Y_full) <- vars
  
  # Build RHS blocks for lags 1..pmax with deterministic names
  X_blocks <- vector("list", length = pmax)
  rhs_all_names <- character(0)
  for (L in 1:pmax) {
    block <- Z[, (k*L + 1):(k*(L + 1)), drop = FALSE]  # k cols for lag L
    colnames(block) <- paste0(vars, "_L", L)
    X_blocks[[L]] <- block
    rhs_all_names <- c(rhs_all_names, colnames(block))
  }
  X_full <- do.call(cbind, X_blocks)
  
  # Helper: lag threshold by 'd' so we use q_{t-d} to split regimes at time t 
  lag_threshold_for_Y <- function(d) {
    th_lag <- c(rep(NA_real_, d), th_series[1:(N0 - d)])
    th_lag[(pmax + 1):N0]  # align with rows of Y_full/X_full
  }
  
  # Sum of IC across equations for a regime
  ic_sum <- function(Yreg, Xreg, crit) {
    n <- nrow(Yreg); if (n <= 2) return(Inf)
    Xc <- cbind(Intercept = 1, as.matrix(Xreg))
    kpar <- ncol(Xc)  # includes intercept
    RSS <- numeric(k)
    for (i in 1:k) {
      fit <- .lm.fit(Xc, Yreg[, i])
      RSS[i] <- sum(fit$residuals^2)
    }
    sigma2 <- RSS / n
    if (any(!is.finite(sigma2)) || any(sigma2 <= 0)) return(Inf)
    if (crit == "BIC") sum(n*log(sigma2) + kpar*log(n)) else sum(n*log(sigma2) + 2*kpar)
  }
  
  # grid search over (delay, theta, p)
  best_score <- Inf
  best_spec  <- NULL
  best_env   <- NULL
  best_rhs   <- NULL
  
  for (delay in delays_set) {
    thY <- lag_threshold_for_Y(delay)
    keep <- !is.na(thY)
    if (!any(keep)) next
    
    Yd  <- Y_full[keep, , drop = FALSE]
    Xd  <- X_full[keep, , drop = FALSE]
    thd <- thY[keep]
    
    # candidate theta grid (trimmed quantiles)
    theta_grid <- NULL
    if (!isTRUE(use_given_split)) {
      probs <- seq(trim, 1 - trim, length.out = ngrid)
      theta_grid <- unique(stats::quantile(thd, probs = probs, na.rm = TRUE, type = 8))
    } else {
      if (is.null(threshold_info) || is.null(threshold_info$split))
        stop("use_given_split=TRUE but threshold_info$split is missing.")
      theta_grid <- threshold_info$split
    }
    
    for (theta in theta_grid) {
      reg_low  <- thd <= theta
      share_lo <- mean(reg_low)
      share_hi <- 1 - share_lo
      if (share_lo < min_regime_share || share_hi < min_regime_share) next
      
      idx_lo <- which(reg_low)
      idx_hi <- which(!reg_low)
      
      for (p in lags_set) {
        rhs_names_p <- unlist(lapply(1:p, function(L) paste0(vars, "_L", L)))
        if (!all(rhs_names_p %in% colnames(Xd))) next
        
        Xp <- Xd[, rhs_names_p, drop = FALSE]
        
        score_lo <- ic_sum(Yd[idx_lo, , drop = FALSE], Xp[idx_lo, , drop = FALSE], criterion)
        score_hi <- ic_sum(Yd[idx_hi, , drop = FALSE], Xp[idx_hi, , drop = FALSE], criterion)
        score <- score_lo + score_hi
        if (!is.finite(score)) next
        
        if (score < best_score) {
          best_score <- score
          best_spec  <- list(p = p, delay = delay, theta = theta,
                             share_low = share_lo, share_high = share_hi)
          best_env   <- list(Yd = Yd, Xd = Xd, thd = thd,
                             idx_lo = idx_lo, idx_hi = idx_hi)
          best_rhs   <- rhs_names_p
        }
      }
    }
  }
  
  if (is.null(best_spec)) stop("No admissible (p, delay, theta) satisfied min_regime_share and data length.")
  
  # final fit with selected spec
  p        <- best_spec$p
  delay    <- best_spec$delay
  theta    <- best_spec$theta
  Yd       <- best_env$Yd
  Xd       <- best_env$Xd
  idx_lo   <- best_env$idx_lo
  idx_hi   <- best_env$idx_hi
  Xp       <- Xd[, best_rhs, drop = FALSE]
  
  fit_regime <- function(row_idx) {
    Yreg <- Yd[row_idx, , drop = FALSE]
    Xreg <- Xp[row_idx, , drop = FALSE]
    n    <- nrow(Yreg)
    
    Xc <- cbind(Intercept = 1, as.matrix(Xreg))
    A_names <- unlist(lapply(1:p, function(L) paste0(vars, "_L", L)))
    
    A   <- matrix(NA_real_, nrow = k, ncol = k * p,
                  dimnames = list(vars, A_names))
    intercept <- numeric(k); names(intercept) <- vars
    fitted    <- matrix(NA_real_, nrow = n, ncol = k, dimnames = list(NULL, vars))
    resid     <- matrix(NA_real_, nrow = n, ncol = k, dimnames = list(NULL, vars))
    r2        <- numeric(k); aic <- numeric(k); bic <- numeric(k)
    
    for (i in seq_len(k)) {
      fit <- .lm.fit(Xc, Yreg[, i])
      b   <- as.numeric(fit$coefficients)
      intercept[i] <- b[1]
      beta <- b[-1]
      A[i, ] <- beta
      fitted[, i] <- Xc %*% b
      resid[, i]  <- Yreg[, i] - fitted[, i]
      
      sst <- sum((Yreg[, i] - mean(Yreg[, i]))^2)
      rss <- sum(resid[, i]^2)
      r2[i] <- if (sst > 0) 1 - rss/sst else NA_real_
      kpar  <- length(b)
      sigma2<- rss / n
      aic[i] <- n * log(sigma2) + 2 * kpar
      bic[i] <- n * log(sigma2) + kpar * log(n)
    }
    
    list(
      A          = A,
      intercept  = intercept,
      residuals  = resid,
      fitted     = fitted,
      Sigma      = stats::cov(resid),
      Y          = Yreg,
      X          = Xreg,
      n_obs      = n,
      r_squared  = r2,
      AIC        = aic,
      BIC        = bic
    )
  }
  
  low_fit  <- fit_regime(idx_lo)
  high_fit <- fit_regime(idx_hi)
  
  if (verbose) {
    message(sprintf("Selected p=%d, delay=%d, θ=%.3f | shares: low=%.2f, high=%.2f",
                    p, delay, theta, best_spec$share_low, best_spec$share_high))
    message(sprintf("Obs per regime: low=%d, high=%d", low_fit$n_obs, high_fit$n_obs))
  }
  
  regime_tag <- if (!is.null(threshold_info) && !is.null(threshold_info$tag)) {
    threshold_info$tag
  } else NA_character_
  
  regimes <- list(low = low_fit, high = high_fit)
  
  theta_pct <- stats::ecdf(best_env$thd)(best_spec$theta)
  
  res <- list(
    regimes          = regimes,
    threshold_value  = theta,                 
    regime_share     = c(low = best_spec$share_low, high = best_spec$share_high),
    theta_percentile = as.numeric(theta_pct), 
    metadata = list(
      lag           = p,
      variables     = vars,
      threshold_tag = threshold_info$tag %||% NA,
      total_obs_full= nrow(df),
      total_obs_used= nrow(Yd),
      y_means       = y_means,
      y_sds         = y_sds,
      standardized  = standardize_y
    ),
    spec = list(p = p, delay = delay, criterion = criterion, theta = theta)
  )
  return(res)
}