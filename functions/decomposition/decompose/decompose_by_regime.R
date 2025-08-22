#' Share of Δ realized in each regime (per scenario × variable)
#'
#' @param delta_tbl tibble: scenario, regime, variable, t, delta
#' @param metric "abs" (default; L1 share) or "signed" (signed sums share)
#'
#' @return tibble: scenario, variable, regime, total, share
#'
decompose_by_regime <- function(delta_tbl, metric = c("abs","signed")) {
  requireNamespace("dplyr", quietly = TRUE)
  metric <- match.arg(metric)
  
  stopifnot(all(c("scenario","regime","variable","t","delta") %in% names(delta_tbl)))
  
  agg <- delta_tbl %>%
    dplyr::mutate(measure = if (metric == "abs") abs(.data$delta) else .data$delta) %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$regime) %>%
    dplyr::summarise(total = sum(.data$measure, na.rm = TRUE), .groups = "drop")
  
  totals <- agg %>%
    dplyr::group_by(.data$scenario, .data$variable) %>%
    dplyr::summarise(grand = sum(.data$total, na.rm = TRUE), .groups = "drop")
  
  agg %>%
    dplyr::inner_join(totals, by = c("scenario","variable")) %>%
    dplyr::mutate(share = dplyr::if_else(.data$grand > 0, .data$total / .data$grand, 0)) %>%
    dplyr::select(.data$scenario, .data$variable, .data$regime, .data$total, .data$share)
}