#' Log a concise summary of a threshold VAR (TVAR) model
#'
#' @param model An estimated TVAR model object.
#' @param log_path Optional file path to save the summary log.
#' @param log_level Logging verbosity: "info" (default), or "warn".
#' @param style Output style: "plain" (default), "markdown", or "latex".
#' @param verbose Print the summary to console. Default is TRUE.
#'
#' @return A list containing threshold, regimes, and stats (invisibly).
#' @export
log_tvar_model_summary <- function(model,
                                   log_path = "output/logs/tvar_model_summary.txt",
                                   log_level = "info",
                                   style = c("markdown", "plain"),
                                   verbose = TRUE) {
  style <- match.arg(style)
  
  # Safe rounding fallback
  safe_round <- function(x, digits = 3) {
    if (is.null(x) || is.na(x) || !is.numeric(x)) return("NA")
    round(x, digits)
  }
  
  # Build summary content
  threshold <- model$threshold_value
  quantile <- names(threshold)
  variable <- model$regime_variable
  
  summary_lines <- c()
  
  summary_lines <- c(
    summary_lines,
    if (style == "markdown") {
      c(
        paste0("### Threshold TVAR Summary"),
        "",
        paste0("- **Regime Variable**: `", variable, "`"),
        paste0("- **Threshold Value**: `", safe_round(threshold[1], 4), "` (Quantile: ", quantile, ")"),
        ""
      )
    } else {
      c(
        "=== Threshold TVAR Summary ===",
        paste0("Regime Variable: ", variable),
        paste0("Threshold Value: ", safe_round(threshold[1], 4), " (Quantile: ", quantile, ")"),
        ""
      )
    }
  )
  
  # Summary table header
  summary_lines <- c(
    summary_lines,
    if (style == "markdown") {
      c(
        "| Regime | Obs | R² | AIC | BIC |",
        "|--------|-----|-----|------|------|"
      )
    } else {
      c("Regime\tObs\tR2\tAIC\tBIC")
    }
  )
  
  # Summary table rows
  regime_table <- purrr::imap_chr(model$regimes, function(reg, name) {
    obs <- reg$n_obs
    r2  <- safe_round(reg$r_squared[1])
    aic <- safe_round(reg$AIC[1])
    bic <- safe_round(reg$BIC[1])
    
    if (style == "markdown") {
      paste0("| ", name, " | ", obs, " | ", r2, " | ", aic, " | ", bic, " |")
    } else {
      paste(name, obs, r2, aic, bic, sep = "\t")
    }
  })
  
  summary_lines <- c(summary_lines, regime_table)
  
  # Save log
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(summary_lines, log_path)
  
  # Print if verbose
  if (verbose) cat(paste0(summary_lines, collapse = "\n"), "\n")
  
  # Return object (invisible, but testable)
  invisible(list(
    threshold_value = threshold,
    regime_variable = variable,
    regimes = purrr::map(model$regimes, ~ list(
      n_obs = .$n_obs,
      r_squared = .$r_squared,
      AIC = .$AIC,
      BIC = .$BIC
    ))
  ))
}