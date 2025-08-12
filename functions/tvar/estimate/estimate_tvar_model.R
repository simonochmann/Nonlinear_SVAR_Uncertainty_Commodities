#' Estimate Threshold VAR (TVAR) Model
#'
#' Estimates separate VAR(p) models for each regime defined by a threshold split.
#'
#' @param df A data frame with a `date` column and standardized input variables.
#' @param threshold_info Output from `select_tvar_threshold()`.
#' @param lag Number of lags (p) for VAR.
#' @param verbose If TRUE, prints summary per regime.
#'
#' @return A list with regime-specific models, residuals, coefficients, A matrix, and metadata.

estimate_tvar_model <- function(df, threshold_info, lag = 1, verbose = TRUE) {
  stopifnot("date" %in% names(df))
  stopifnot(is.numeric(lag), length(lag) == 1, lag >= 1)
  
  vars <- setdiff(names(df), "date")
  k <- length(vars)
  p <- lag
  
  # Filter threshold values
  valid_idx <- which(!is.na(threshold_info$values))
  df <- df[valid_idx, , drop = FALSE]
  threshold_var <- threshold_info$values[valid_idx]
  split_value <- threshold_info$split
  
  # Assign regimes
  df$threshold <- threshold_var
  df$regime <- ifelse(df$threshold <= split_value, "low", "high")
  regime_names <- c("low", "high")
  
  # Internal function: Create lagged data
  create_lagged_df <- function(df_regime, lag_order) {
    n <- nrow(df_regime)
    lagged <- df_regime
    for (var in vars) {
      for (lag_i in 1:lag_order) {
        lagged[[paste0(var, "_L", lag_i)]] <- dplyr::lag(df_regime[[var]], lag_i)
      }
    }
    lagged <- lagged[(lag_order + 1):n, , drop = FALSE]
    return(na.omit(lagged))
  }
  
  models <- list()
  
  for (regime in regime_names) {
    df_r <- df[df$regime == regime, ]
    df_r_lagged <- create_lagged_df(df_r, p)
    
    if (nrow(df_r_lagged) < 5) {
      warning(glue::glue("Regime '{regime}' has too few observations. Skipping."))
      next
    }
    
    Y <- df_r_lagged[, vars, drop = FALSE]
    X <- df_r_lagged[, grepl("_L", names(df_r_lagged)), drop = FALSE]
    X <- cbind(Intercept = 1, X)
    
    residuals <- matrix(NA, nrow = nrow(Y), ncol = k)
    fitted    <- matrix(NA, nrow = nrow(Y), ncol = k)
    coefs     <- list()
    r2_vals   <- numeric(k)
    aic_vals  <- numeric(k)
    bic_vals  <- numeric(k)
    
    colnames(fitted) <- vars
    colnames(residuals) <- vars
    
    lagged_varnames <- unlist(lapply(1:p, function(lag_i) {
      paste0(vars, "_L", lag_i)
    }))
    
    for (i in seq_along(vars)) {
      fit <- lm(Y[[i]] ~ . - 1, data = as.data.frame(X))
      coefs_i <- coef(fit)[lagged_varnames]
      
      if (any(is.na(coefs_i)) || length(coefs_i) != k * p) {
        stop(glue::glue("Missing or mismatched coefficients in regression for '{vars[i]}'"))
      }
      
      coefs[[vars[i]]] <- coefs_i
      fitted[, i]      <- fitted(fit)
      residuals[, i]   <- resid(fit)
      r2_vals[i]       <- summary(fit)$r.squared
      aic_vals[i]      <- AIC(fit)
      bic_vals[i]      <- BIC(fit)
    }
    
    # Construct A matrix
    A_mat <- matrix(NA, nrow = k, ncol = k * p)
    rownames(A_mat) <- vars
    colnames(A_mat) <- lagged_varnames
    
    for (i in seq_along(vars)) {
      A_mat[i, ] <- as.numeric(coefs[[vars[i]]])
    }
    
    models[[regime]] <- list(
      A = A_mat,
      coefficients = coefs,
      residuals = residuals,
      fitted = fitted,
      Sigma = cov(residuals, use = "pairwise.complete.obs"),  
      Y = Y,
      X = X,
      n_obs = nrow(Y),
      r_squared = r2_vals,
      AIC = aic_vals,
      BIC = bic_vals
    )
    
    if (verbose) {
      message(glue::glue("Regime: {regime}"))
      print(round(data.frame(R2 = r2_vals, AIC = aic_vals, BIC = bic_vals), 3))
    }
  }
  
  return(list(
    regimes         = models,
    threshold_value = split_value,
    regime_variable = threshold_info$name %||% "threshold_var",
    metadata        = list(
      lag            = lag,
      variables      = vars,
      threshold_tag  = threshold_info$tag %||% NA,
      total_obs      = nrow(df),
      regime_counts  = table(df$regime)
    )
  ))
}