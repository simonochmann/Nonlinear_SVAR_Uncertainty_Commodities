# functions/tvar/regime/compute_regime_path.R
# Compute Regime Path from Threshold Rule in TVAR Model (robust, side‑effect friendly)
# Order of truth:
#   1) model$metadata$regime_index (already present)
#   2) metadata$threshold_series + threshold_value
#   3) metadata$input_df[[regime_variable]] (or metadata$threshold_var) + threshold_value
#   4) best‑guess column in input_df (common names) + threshold_value
#   5) fallback: concatenate low/high block sizes from regimes
#
# Returns the MODEL (not a vector). If inject=TRUE, writes model$metadata$regime_index
# and model$regime_path.

compute_regime_path <- function(model, inject = TRUE, verbose = TRUE) {
  stopifnot(is.list(model))
  
  vmessage <- function(...) if (isTRUE(verbose)) message(...)
  
  # 0) Already present 
  if (!is.null(model$metadata$regime_index)) {
    vmessage("[regime_path] Using existing metadata$regime_index (n=",
             length(model$metadata$regime_index), ")")
    if (isTRUE(inject)) model$regime_path <- model$metadata$regime_index
    return(model)
  }
  
  # helper: normalize threshold value (can be list or scalar; ignore NA)
  get_threshold_value <- function(m) {
    tv <- NULL
    if (!is.null(m$threshold_value)) {
      tv <- if (is.list(m$threshold_value)) m$threshold_value[[1]] else m$threshold_value
    } else if (!is.null(m$model) && !is.null(m$model$Thresh)) {
      tv <- as.numeric(m$model$Thresh)
    }
    if (length(tv) == 0 || is.na(tv)) return(NULL)
    as.numeric(tv)
  }
  
  thr_value <- get_threshold_value(model)
  
  # 1) metadata$threshold_series + threshold_value 
  thr_series <- NULL
  if (!is.null(model$metadata$threshold_series)) thr_series <- model$metadata$threshold_series
  if (is.null(thr_series) && !is.null(model$threshold_series)) thr_series <- model$threshold_series
  
  if (!is.null(thr_series) && !is.null(thr_value)) {
    x <- as.numeric(thr_series)
    idx <- ifelse(is.na(x), NA_integer_, ifelse(x <= thr_value, 1L, 2L))
    vmessage("[regime_path] Derived from metadata threshold_series vs threshold_value = ",
             signif(thr_value, 6))
    if (isTRUE(inject)) {
      model$metadata$regime_index <- idx
      model$regime_path <- idx
    }
    return(model)
  }
  
  # 2) input_df + regime_variable (or metadata$threshold_var) 
  df <- NULL
  if (!is.null(model$metadata$input_df)) df <- model$metadata$input_df
  if (is.null(df) && !is.null(model$input_df)) df <- model$input_df
  
  regime_var <- NULL
  if (!is.null(model$regime_variable) && is.character(model$regime_variable)) {
    regime_var <- model$regime_variable
  } else if (!is.null(model$metadata$threshold_var)) {
    regime_var <- model$metadata$threshold_var
  }
  
  if (!is.null(df) && !is.null(regime_var) && regime_var %in% colnames(df) && !is.null(thr_value)) {
    x <- df[[regime_var]]
    x <- as.numeric(x)
    idx <- ifelse(is.na(x), NA_integer_, ifelse(x <= thr_value, 1L, 2L))
    vmessage("[regime_path] Built from input_df[['", regime_var,
             "']] using threshold_value = ", signif(thr_value, 6))
    if (isTRUE(inject)) {
      model$metadata$regime_index <- idx
      model$regime_path <- idx
    }
    return(model)
  }
  
  # 3) Best‑guess column in input_df
  if (!is.null(df) && !is.null(thr_value)) {
    candidates <- c("value_L1", "uncertainty_idx", "VIX", "VXO", "vix", "vxo", "jln", "ciss")
    pick <- intersect(candidates, colnames(df))
    if (length(pick) >= 1) {
      gvar <- pick[1]
      x <- as.numeric(df[[gvar]])
      idx <- ifelse(is.na(x), NA_integer_, ifelse(x <= thr_value, 1L, 2L))
      vmessage("[regime_path] Best‑guess input_df[['", gvar, "']] used (threshold = ",
               signif(thr_value, 6), ")")
      if (isTRUE(inject)) {
        model$metadata$threshold_var <- gvar
        model$metadata$regime_index <- idx
        model$regime_path <- idx
      }
      return(model)
    }
  }
  
  # 4) Fallback: concatenate low/high block sizes 
  n_low  <- if (!is.null(model$regimes$low$Y))  nrow(model$regimes$low$Y)  else 0L
  n_high <- if (!is.null(model$regimes$high$Y)) nrow(model$regimes$high$Y) else 0L
  if ((n_low + n_high) > 0) {
    idx <- c(rep(1L, n_low), rep(2L, n_high))
    vmessage("[regime_path] Fallback from block sizes: low=", n_low,
             ", high=", n_high, " (no threshold series/var available)")
    if (isTRUE(inject)) {
      model$metadata$regime_index <- idx
      model$regime_path <- idx
    }
    return(model)
  }
  
  # Give up 
  stop("Could not infer regime path: no regime_index, no usable threshold series/value, ",
       "no input_df match, and no regime block sizes.")
}