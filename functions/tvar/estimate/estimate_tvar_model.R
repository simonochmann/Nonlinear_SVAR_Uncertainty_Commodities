#' Estimate Threshold VAR (TVAR) Model
#'
#' Estimates separate VAR(p) models for each regime defined by a threshold split.
#'
#' @param df A data frame with a `date` column and standardized input variables.
#' @param threshold_info Output from `select_tvar_threshold()`.
#' @param lag Number of lags (p) for VAR.
#' @param verbose If TRUE, prints summary per regime.
#'
#' @return A list with regime-specific models, residuals, coefficients, and metadata.
estimate_tvar_model <- function(df, threshold_info, lag = 1, verbose = TRUE) {
  stopifnot("date" %in% names(df))
  
  # Setup
  # Setup
  vars         <- setdiff(names(df), "date")
  split_value  <- threshold_info$split
  regime_names <- c("low", "high")
  
  # Align df and threshold_var to same length after lag
  valid_idx    <- which(!is.na(threshold_info$values))
  df           <- df[valid_idx, , drop = FALSE]
  threshold_var <- threshold_info$values[valid_idx]
  df$threshold <- threshold_var
  df$regime    <- ifelse(df$threshold <= split_value, regime_names[1], regime_names[2])
  
  # Lagging function
  create_lagged_df <- function(df_regime, p) {
    n <- nrow(df_regime)
    lagged <- df_regime
    for (var in vars) {
      for (l in 1:p) {
        lagged[[paste0(var, "_L", l)]] <- dplyr::lag(df_regime[[var]], l)
      }
    }
    lagged <- lagged[(p + 1):n, , drop = FALSE]
    return(na.omit(lagged))
  }
  
  models <- list()
  
  for (regime in regime_names) {
    df_r <- df[df$regime == regime, ]
    df_r_lagged <- create_lagged_df(df_r, lag)
    
    if (nrow(df_r_lagged) < 5) {
      warning(paste("Regime", regime, "has too few observations. Skipping."))
      next
    }
    
    Y <- df_r_lagged[, vars, drop = FALSE]
    X <- df_r_lagged[, grepl("_L", names(df_r_lagged)), drop = FALSE]
    X <- cbind(Intercept = 1, X)
    
    coefs     <- list()
    residuals <- matrix(NA, nrow = nrow(Y), ncol = length(vars))
    fitted    <- matrix(NA, nrow = nrow(Y), ncol = length(vars))
    r2_vals   <- numeric(length(vars))
    aic_vals  <- numeric(length(vars))
    bic_vals  <- numeric(length(vars))
    
    colnames(fitted) <- vars
    colnames(residuals) <- vars
    
    for (i in seq_along(vars)) {
      fit <- lm(Y[[i]] ~ . -1, data = as.data.frame(X))
      coefs[[vars[i]]]     <- coef(fit)
      fitted[, i]          <- fitted(fit)
      residuals[, i]       <- resid(fit)
      r2_vals[i]           <- summary(fit)$r.squared
      aic_vals[i]          <- AIC(fit)
      bic_vals[i]          <- BIC(fit)
    }
    
    models[[regime]] <- list(
      coefficients = coefs,
      residuals    = residuals,
      fitted       = fitted,
      Y            = Y,
      X            = X,
      n_obs        = nrow(Y),
      r_squared    = r2_vals,
      AIC          = aic_vals,
      BIC          = bic_vals
    )
    
    if (verbose) {
      message(paste0("Regime: ", regime))
      print(round(data.frame(R2 = r2_vals, AIC = aic_vals, BIC = bic_vals), 3))
    }
  }
  
  return(list(
    regimes          = models,
    threshold_value  = split_value,
    regime_variable  = threshold_info$name %||% "threshold_var",  # fallback
    metadata         = list(
      lag           = lag,
      variables     = vars,
      threshold_tag = threshold_info$tag %||% NA,
      total_obs     = nrow(df),
      regime_counts = table(df$regime)
    )
  ))
}