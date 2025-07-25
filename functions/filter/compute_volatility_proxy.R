#' Compute Volatility Proxy from Log Returns
#'
#' Computes a volatility proxy from log returns. Supports multiple methods
#' ("squared", "abs", [future: "rv_rolling"]), robust NA handling, optional
#' metadata summary, and outlier clipping for stability.
#'
#' @param df A tibble with column `log_return`.
#' @param method "squared" (default), "abs". Future: "rv_rolling".
#' @param clip_outliers Logical. If TRUE, clips vol_proxy above 99.9th percentile. Default = FALSE.
#' @param return_metadata Logical. If TRUE, returns list with data and diagnostics. Default = FALSE.
#' @param verbose Logical. Whether to print summary stats. Default = TRUE.
#'
#' @return Tibble with `vol_proxy` column or list(data = ..., meta = ...) if `return_metadata = TRUE`.
#' @export
compute_volatility_proxy <- function(df,
                                     method = "squared",
                                     clip_outliers = FALSE,
                                     return_metadata = FALSE,
                                     verbose = TRUE) {
  stopifnot(is.data.frame(df))
  if (!"log_return" %in% names(df)) stop("`log_return` column not found.")
  
  # Method Validation
  valid_methods <- c("squared", "abs")
  if (!method %in% valid_methods) {
    stop("Invalid method. Choose one of: ", paste(valid_methods, collapse = ", "))
  }
  
  # Compute Vol Proxy 
  proxy <- dplyr::case_when(
    is.na(df$log_return) ~ NA_real_,
    method == "squared" ~ df$log_return^2,
    method == "abs"     ~ abs(df$log_return)
  )
  
  # Outlier Clipping 
  if (clip_outliers) {
    clip_val <- quantile(proxy, 0.999, na.rm = TRUE)
    proxy <- pmin(proxy, clip_val)
    if (verbose) cat("Clipped vol_proxy at 99.9th percentile =", round(clip_val, 4), "\n")
  }
  
  df_out <- dplyr::mutate(df, vol_proxy = proxy)
  
  # Metadata Summary 
  if (return_metadata) {
    meta <- list(
      method = method,
      formula = if (method == "squared") "log_return^2" else "abs(log_return)",
      n_obs = nrow(df_out),
      pct_missing_vol_proxy = round(mean(is.na(df_out$vol_proxy)) * 100, 2),
      vol_proxy_summary = summary(df_out$vol_proxy),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      clipped = clip_outliers
    )
    if (verbose) {
      cat("Volatility proxy computed using method:", method, "\n")
      cat("Missing vol_proxy values:", meta$pct_missing_vol_proxy, "%\n")
    }
    return(list(data = df_out, meta = meta))
  }
  
  return(df_out)
}
