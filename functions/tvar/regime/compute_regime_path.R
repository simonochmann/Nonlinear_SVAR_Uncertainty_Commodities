#' Compute Regime Path from Threshold Rule in TVAR Model
#'
#' This function classifies each observation into a regime (1 = low, 2 = high)
#' based on the regime variable and threshold value stored in the TVAR model.
#'
#' @param model A fitted TVAR model with threshold_value, regime_variable, and metadata$input_df.
#' @param verbose Logical, whether to print diagnostics to console.
#' @param return_full Logical, whether to return full metadata or only the regime path vector.
#' @param inject Logical, whether to inject the regime_path into the model object (for chaining).
#'
#' @return If return_full = TRUE: A list with regime_path, metadata, summary.
#'         If return_full = FALSE: Just the regime_path vector.
#'
#' @export
compute_regime_path <- function(model, verbose = TRUE, return_full = TRUE, inject = FALSE) {
  
  # 1. Input Validation
  if (is.null(model$threshold_value) || is.na(model$threshold_value[[1]])) {
    cli::cli_abort("Missing or invalid {.field threshold_value} in model.")
  }
  if (is.null(model$regime_variable) || !is.character(model$regime_variable)) {
    cli::cli_abort("Missing or invalid {.field regime_variable} in model.")
  }
  if (is.null(model$metadata$input_df)) {
    cli::cli_abort("Missing {.field metadata$input_df} in model.")
  }
  
  threshold <- model$threshold_value[[1]]
  var       <- model$regime_variable
  df        <- model$metadata$input_df
  
  if (!(var %in% colnames(df))) {
    cli::cli_abort("Threshold variable {.val {var}} not found in input data.")
  }
  
  x <- df[[var]]
  total_obs <- length(x)
  
  # 2. Compute Regime Path
  regime_path <- dplyr::case_when(
    is.na(x)           ~ NA_integer_,
    x <= threshold     ~ 1L,
    x > threshold      ~ 2L,
    TRUE               ~ NA_integer_
  )
  
  # 3. Build Metadata
  summary_tbl <- tibble::tibble(
    regime = as.character(1:2),
    count = purrr::map_int(1:2, ~sum(regime_path == .x, na.rm = TRUE)),
    pct   = purrr::map_dbl(1:2, ~round(100 * mean(regime_path == .x, na.rm = TRUE), 2))
  )
  
  full_metadata <- list(
    threshold_value     = threshold,
    threshold_variable  = var,
    total_observations  = total_obs,
    missing_values      = sum(is.na(x)),
    timestamp           = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    model_hash          = tryCatch(digest::digest(model), error = function(e) NA),
    summary             = summary_tbl
  )
  
  # 4. Optional Verbose Diagnostics
  if (verbose) {
    cli::cli_h1("TVAR Regime Path Computation")
    cli::cli_text("Variable used     : {.strong {var}}")
    cli::cli_text("Threshold value   : {.strong {round(threshold, 6)}}")
    cli::cli_text("Total observations: {.strong {total_obs}}")
    cli::cli_text("Missing values    : {.strong {sum(is.na(x))}}")
    print(summary_tbl)
  }
  
  # 5. Injection into Model
  if (inject) {
    model$regime_path <- regime_path
    if (return_full) {
      model$regime_metadata <- full_metadata
    }
  }
  
  # 6. Return
  if (return_full) {
    return(list(
      regime_path = regime_path,
      metadata = full_metadata
    ))
  } else {
    return(regime_path)
  }
}
