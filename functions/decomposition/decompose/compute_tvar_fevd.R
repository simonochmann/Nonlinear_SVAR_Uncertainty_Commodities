#' Regime-aware FEVD from IRFs (orthogonalized unit shocks)
#'
#' Computes FEVD shares over horizon 0..H per (regime, response, impulse).
#' If IRFs are provided per regime, decomposition is done regime-by-regime.
#'
#' @param irf_df tibble with columns:
#'        regime, impulse, response, h, irf
#'        (if 'regime' missing, a "mixed" regime is assumed)
#' @param horizon integer H (if NULL uses max(h) in irf_df)
#' @param shock_var optional named numeric vector of shock variances (default 1)
#'
#' @return tibble: regime, response, impulse, t, fevd_share ∈ [0,1]
#'
compute_tvar_fevd <- function(irf_df, horizon = NULL, shock_var = NULL) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  
  needed <- c("impulse","response","h","irf")
  if (!all(needed %in% names(irf_df))) {
    stop("irf_df must contain columns: impulse, response, h, irf (and optionally regime).")
  }
  
  if (!"regime" %in% names(irf_df)) {
    irf_df$regime <- "mixed"
  }
  
  if (is.null(horizon)) {
    horizon <- max(irf_df$h, na.rm = TRUE)
  }
  
  # Shock variances (default 1)
  if (is.null(shock_var)) {
    shock_var <- rep(1, length(unique(irf_df$impulse)))
    names(shock_var) <- unique(irf_df$impulse)
  }
  
  w <- tibble::tibble(impulse = names(shock_var), var_w = as.numeric(shock_var))
  
  # Keep horizons 0..H
  irf_df <- irf_df %>% dplyr::filter(.data$h >= 0L, .data$h <= horizon)
  
  # Cumulative sums of squared IRFs × variance per (regime, response, impulse)
  fevd_cum <- irf_df %>%
    dplyr::inner_join(w, by = "impulse") %>%
    dplyr::mutate(term = (.data$irf^2) * .data$var_w) %>%
    dplyr::arrange(.data$regime, .data$response, .data$impulse, .data$h) %>%
    dplyr::group_by(.data$regime, .data$response, .data$impulse) %>%
    dplyr::mutate(t = .data$h, numer = cumsum(.data$term)) %>%
    dplyr::ungroup()
  
  # Denominator: sum across impulses per (regime, response, t)
  denom <- fevd_cum %>%
    dplyr::group_by(.data$regime, .data$response, .data$t) %>%
    dplyr::summarise(denom = sum(.data$numer, na.rm = TRUE), .groups = "drop")
  
  out <- fevd_cum %>%
    dplyr::inner_join(denom, by = c("regime","response","t")) %>%
    dplyr::mutate(fevd_share = dplyr::if_else(.data$denom > 0, .data$numer / .data$denom, 0)) %>%
    dplyr::select(.data$regime, .data$response, .data$impulse, .data$t, .data$fevd_share)
  
  out
}