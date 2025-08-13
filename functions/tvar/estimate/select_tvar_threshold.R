#' Select threshold (split) for a TVAR by IC grid search
#'
#' @param df Data frame with 'date' and k numeric series (already standardized for TVAR).
#' @param index_name Name of the threshold variable column (e.g., "VIX", "VXO", "JLN").
#' @param lags_set Integer vector of candidate VAR lags p.
#' @param delays_set Integer vector of candidate delays d (how far the threshold is lagged).
#' @param trim Proportion to trim from each tail when forming the threshold grid (e.g., 0.15).
#' @param ngrid Number of grid points between trimmed quantiles.
#' @param criterion "BIC" (default) or "AIC".
#' @param min_regime_share Minimum share of obs per regime in the estimation sample (e.g., 0.15).
#' @param smooth_ma Integer; moving-average window for the threshold (1 = none).
#' @param verbose Print progress.
#' @return A list with: values (vector aligned to df rows), split (numeric),
#'         best_lag, best_delay, shares, name, tag, grid, criterion, score.
select_tvar_threshold <- function(
    df,
    index_name,
    lags_set       = 2:6,
    delays_set     = 1:3,
    trim           = 0.15,
    ngrid          = 100,
    criterion      = c("BIC","AIC"),
    min_regime_share = 0.15,
    smooth_ma      = 3,
    verbose        = TRUE
) {
  stopifnot("date" %in% names(df))
  criterion <- toupper(match.arg(criterion))
  # Vars used in the VAR
  vars <- setdiff(names(df), "date")
  
  # Auto-drop non-numeric columns (except 'date'), but keep threshold column
  num_mask <- vapply(df[vars], is.numeric, logical(1))
  if (!all(num_mask)) {
    drop <- vars[!num_mask]
    if (verbose) message("select_tvar_threshold(): dropping non-numeric columns: ",
                         paste(drop, collapse = ", "))
    keep <- c("date", vars[num_mask])
    df <- df[, keep, drop = FALSE]
    vars <- setdiff(names(df), "date")
  }
  
  # Ensure threshold column exists (case-insensitive) and is numeric
  idx_col <- if (index_name %in% names(df)) index_name else tolower(index_name)
  if (!idx_col %in% names(df)) stop("select_tvar_threshold(): index column not found: ", index_name)
  
  if (!is.numeric(df[[idx_col]])) {
    df[[idx_col]] <- suppressWarnings(as.numeric(df[[idx_col]]))
    if (all(is.na(df[[idx_col]]))) {
      stop("select_tvar_threshold(): threshold column '", idx_col, "' is non-numeric and cannot be coerced.")
    }
  }
  
  # Guard: need all numeric
  if (any(!vapply(df[vars], is.numeric, logical(1)))) {
    stop("select_tvar_threshold(): all non-'date' columns must be numeric.")
  }
  
  # Helper: simple right-aligned moving average (no external deps)
  ma_right <- function(x, n) {
    if (n <= 1) return(as.numeric(x))
    f <- stats::filter(x, rep(1/n, n), sides = 1)
    as.numeric(f)
  }
  
  best <- list(score = Inf, split = NA_real_, best_lag = NA_integer_, best_delay = NA_integer_)
  best_values <- rep(NA_real_, nrow(df))
  best_grid   <- NULL
  best_shares <- c(low = NA_real_, high = NA_real_)
  
  # Build once per (p, d): lagged design mask function
  design_mask <- function(n, p, d, ma_win) {
    # rows unusable due to MA smoothing and threshold delay and VAR lags
    drop <- max(0, (ma_win - 1)) + d + p
    mask <- rep(TRUE, n)
    if (drop > 0) mask[seq_len(drop)] <- FALSE
    mask
  }
  
  # IC calculator for a given split
  calc_ic <- function(Y, X, regime_flag) {
    # Separate OLS per regime and equation; sum ICs
    reg_levels <- c("low","high")
    total_AIC <- 0; total_BIC <- 0
    for (rg in reg_levels) {
      idx <- which(regime_flag == rg)
      if (length(idx) < 5) return(list(AIC = Inf, BIC = Inf))
      Yg <- Y[idx, , drop = FALSE]
      Xg <- X[idx, , drop = FALSE]
      # OLS per equation
      k <- ncol(Yg); aic_sum <- 0; bic_sum <- 0
      for (j in seq_len(k)) {
        fit <- try(stats::lm.fit(x = Xg, y = Yg[, j]), silent = TRUE)
        if (inherits(fit, "try-error")) return(list(AIC = Inf, BIC = Inf))
        # Extract ICs from lm.fit:
        rss <- sum(fit$residuals^2)
        n   <- nrow(Xg)
        p   <- ncol(Xg)               # parameters
        sigma2 <- rss / n
        # Gaussian OLS IC:
        aic_sum <- aic_sum + (n * log(sigma2) + 2 * p)
        bic_sum <- bic_sum + (n * log(sigma2) + log(n) * p)
      }
      total_AIC <- total_AIC + aic_sum
      total_BIC <- total_BIC + bic_sum
    }
    list(AIC = total_AIC, BIC = total_BIC)
  }
  
  n <- nrow(df)
  y_mat <- as.matrix(df[vars])
  
  for (p in lags_set) {
    for (d in delays_set) {
      # Build threshold series (smooth then delay)
      th_raw <- df[[idx_col]]
      th_sm  <- ma_right(th_raw, smooth_ma)
      th_lag <- c(rep(NA_real_, d), head(th_sm, n - d))
      
      # Determine valid rows after lags/delay/MA
      mask_valid <- design_mask(n, p, d, smooth_ma)
      idx_keep   <- which(mask_valid)
      
      th_keep <- th_lag[idx_keep]
      y_keep  <- y_mat[idx_keep, , drop = FALSE]
      
      # Build lagged regressors for VAR(p)
      build_X <- function(Y, lagp) {
        # Y is T x k; return T - p x (1 + k*p) design (intercept + stacked lags)
        Tn <- nrow(Y); k <- ncol(Y)
        if (Tn <= lagp) return(list(Y = NULL, X = NULL))
        Yt <- Y[(lagp + 1):Tn, , drop = FALSE]
        Xl <- matrix(NA_real_, nrow = Tn - lagp, ncol = k * lagp)
        colnames(Xl) <- as.vector(sapply(1:lagp, function(L) paste0(colnames(Y), "_L", L)))
        for (L in 1:lagp) {
          Xl[, ((L - 1) * k + 1):(L * k)] <- Y[(lagp + 1 - L):(Tn - L), , drop = FALSE]
        }
        X <- cbind(Intercept = 1, Xl)
        list(Y = Yt, X = X)
      }
      
      des <- build_X(y_keep, p)
      if (is.null(des$Y)) next
      Yd  <- des$Y
      Xd  <- des$X
      
      # Align threshold to Yd rows (drop first p cells)
      th_aligned <- th_keep[(p + 1):length(th_keep)]
      # Trim grid
      ql <- stats::quantile(th_aligned, probs = trim, na.rm = TRUE, type = 8)
      qh <- stats::quantile(th_aligned, probs = 1 - trim, na.rm = TRUE, type = 8)
      if (!is.finite(ql) || !is.finite(qh) || ql >= qh) next
      grid <- seq(ql, qh, length.out = max(10, ngrid))
      
      for (s in grid) {
        regime_flag <- ifelse(th_aligned <= s, "low", "high")
        share_low   <- mean(regime_flag == "low")
        share_high  <- 1 - share_low
        if (share_low < min_regime_share || share_high < min_regime_share) next
        
        ic <- calc_ic(Yd, Xd, regime_flag)
        score <- if (criterion == "BIC") ic$BIC else ic$AIC
        if (is.finite(score) && score < best$score) {
          best$score  <- score
          best$split  <- s
          best$best_lag   <- p
          best$best_delay <- d
          best_values <- th_lag
          best_grid   <- grid
          best_shares <- c(low = share_low, high = share_high)
          if (verbose) {
            message(sprintf("New best: p=%d d=%d split=%.4f %s=%.1f (shares L/H=%.2f/%.2f)",
                            p, d, s, criterion, score, share_low, share_high))
          }
        }
      }
    }
  }
  
  # Fallback if nothing valid
  if (!is.finite(best$score)) {
    warning("select_tvar_threshold(): no valid split found. Falling back to median of lagged series.")
    d_fallback <- delays_set[1]
    th_sm <- ma_right(df[[idx_col]], smooth_ma)
    th_lag <- c(rep(NA_real_, d_fallback), head(th_sm, n - d_fallback))
    med <- stats::median(th_lag, na.rm = TRUE)
    best_values <- th_lag
    best$split <- med
    best$best_lag <- lags_set[1]
    best$best_delay <- d_fallback
    best_grid <- med
    # Rough shares ignoring initial NAs
    v <- th_lag[!is.na(th_lag)]
    best_shares <- c(low = mean(v <= med), high = mean(v > med))
  }
  
  list(
    values     = best_values,
    split      = best$split,
    best_lag   = best$best_lag,
    best_delay = best$best_delay,
    shares     = best_shares,
    name       = index_name,
    tag        = tolower(index_name),
    grid       = best_grid,
    criterion  = criterion,
    score      = best$score
  )
}