#' Orchestrate forecast decomposition: Δ = shocked - baseline = Σ_k contributions_k (+ residual)
#'
#' @param baseline tibble: regime, variable, t, value
#' @param shocked  tibble: scenario, regime, variable, t, value
#' @param single_shock_paths optional tibble: shock_id, regime, variable, t, value
#' @param scenario_shock_map  optional tibble: scenario, shock_id, weight (default 1)
#' @param fevd_shares optional tibble from compute_tvar_fevd(): regime, response, impulse, t, fevd_share
#' @param method character: "auto" | "pathwise" | "fevd"
#'   - "auto": uses "pathwise" if single_shock_paths present; otherwise "fevd"
#' @param reconcile character: "residual_bucket" | "proportional_scale"
#' @param tol numeric tolerance for additivity reconciliation
#'
#' @return list with:
#'   - delta_tbl: scenario, regime, variable, t, delta
#'   - contrib_tbl: scenario, shock_id, regime, variable, t, contribution, method
#'   - reconciled_tbl: same as contrib_tbl (+ 'shock_id="residual"' if residual bucket used)
#'   - summary: per scenario × variable aggregates
#'
decompose_forecast <- function(
    baseline,
    shocked,
    single_shock_paths = NULL,
    scenario_shock_map = NULL,
    fevd_shares = NULL,
    method = c("auto","pathwise","fevd"),
    reconcile = c("residual_bucket","proportional_scale"),
    tol = 1e-10
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  method    <- match.arg(method)
  reconcile <- match.arg(reconcile)
  
  # --- Δ table
  stopifnot(all(c("regime","variable","t","value") %in% names(baseline)))
  stopifnot(all(c("scenario","regime","variable","t","value") %in% names(shocked)))
  
  delta_tbl <- shocked %>%
    dplyr::left_join(
      baseline %>% dplyr::rename(baseline = value),
      by = c("regime","variable","t")
    ) %>%
    dplyr::mutate(
      baseline = dplyr::coalesce(.data$baseline, 0),
      delta    = .data$value - .data$baseline
    ) %>%
    dplyr::select(.data$scenario, .data$regime, .data$variable, .data$t, .data$delta)
  
  # --- Choose method
  chosen <- if (method == "auto") {
    if (!is.null(single_shock_paths)) "pathwise" else "fevd"
  } else method
  
  # --- Compute raw contributions
  contrib_tbl <- switch(
    chosen,
    "pathwise" = decompose_by_shock(
      baseline            = baseline,
      shocked_delta       = delta_tbl,
      single_shock_paths  = single_shock_paths,
      scenario_shock_map  = scenario_shock_map
    ) %>% dplyr::mutate(method = "pathwise"),
    "fevd" = {
      stopifnot(!is.null(fevd_shares))
      decompose_by_shock(
        baseline            = baseline,
        shocked_delta       = delta_tbl,
        fevd_shares         = fevd_shares
      ) %>% dplyr::mutate(method = "fevd")
    }
  )
  
  # --- Reconcile additivity (ensure Σ_k contrib == Δ)
  reconciled_tbl <- reconcile_additivity(
    contrib_tbl = contrib_tbl,
    delta_tbl   = delta_tbl,
    method      = reconcile,
    tol         = tol
  )
  
  # --- Compact scenario × variable summary (L1 and signed totals)
  summary_tbl <- reconciled_tbl %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id) %>%
    dplyr::summarise(
      sum_contribution      = sum(.data$contribution, na.rm = TRUE),
      l1_contribution       = sum(abs(.data$contribution), na.rm = TRUE),
      max_abs_contribution  = suppressWarnings(max(abs(.data$contribution), na.rm = TRUE)),
      .groups = "drop"
    )
  
  list(
    delta_tbl     = delta_tbl,
    contrib_tbl   = contrib_tbl,
    reconciled_tbl= reconciled_tbl,
    summary       = summary_tbl
  )
}
