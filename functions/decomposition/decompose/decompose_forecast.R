#' Orchestrate forecast decomposition: Δ = shocked - baseline = Σ_k contributions_k (+ residual)
#'
#' @param baseline tibble: regime, variable, t, value
#' @param shocked  tibble: scenario, regime, variable, t, value
#' @param single_shock_paths optional tibble: shock_id, regime, variable, t, value
#' @param scenario_shock_map  optional tibble: scenario, shock_id, weight (default 1)
#' @param fevd_shares optional tibble: [uncertainty?], regime, response, impulse, t, fevd_share
#' @param method "auto" | "pathwise" | "fevd"
#' @param reconcile "residual_bucket" | "proportional_scale"
#' @param tol numeric tolerance for additivity reconciliation
#'
#' @return list of tibbles: delta_tbl, contrib_tbl, reconciled_tbl, summary
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
  
  # --- Δ table ------------------------------------------------------------------
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
  
  # ensure uniqueness per (scenario, regime, variable, t)
  delta_tbl <- delta_tbl %>%
    dplyr::group_by(.data$scenario, .data$regime, .data$variable, .data$t) %>%
    dplyr::summarise(delta = sum(.data$delta, na.rm = TRUE), .groups = "drop")
  
  # --- Choose method -------------------------------------------------------------
  chosen <- if (method == "auto") {
    if (!is.null(single_shock_paths)) "pathwise" else "fevd"
  } else method
  
  # --- Compute raw contributions -------------------------------------------------
  if (identical(chosen, "pathwise")) {
    contrib_tbl <- decompose_by_shock(
      baseline            = baseline,
      shocked_delta       = delta_tbl,
      single_shock_paths  = single_shock_paths,
      scenario_shock_map  = scenario_shock_map
    ) %>% dplyr::mutate(method = "pathwise")
    
  } else if (identical(chosen, "fevd")) {
    # FEVD-based allocation: Δ_{s,r,v,t} * share_{r,v,t,shock}
    if (is.null(fevd_shares))
      stop("[decompose_forecast] fevd_shares must be provided for method='fevd'.")
    
    # standardise & normalise FEVD shares
    fevd_norm <- fevd_shares %>%
      dplyr::rename(variable = response, shock_id = impulse) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(
        intersect(c("regime","variable","t","shock_id"), names(.))
      ))) %>%
      dplyr::summarise(fevd_share = sum(.data$fevd_share, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(.data$regime, .data$variable, .data$t) %>%
      dplyr::mutate(total = sum(.data$fevd_share, na.rm = TRUE),
                    fevd_share = dplyr::if_else(.data$total > 0, .data$fevd_share/.data$total, 0)) %>%
      dplyr::select(-.data$total) %>%
      dplyr::ungroup()
    
    # replicate FEVD across scenarios so the join is one-to-many (no many-to-many warning)
    scenarios <- unique(delta_tbl$scenario)
    fevd_exp  <- tidyr::crossing(scenario = scenarios, fevd_norm)
    
    contrib_tbl <- delta_tbl %>%
      dplyr::inner_join(fevd_exp, by = c("scenario","regime","variable","t")) %>%
      dplyr::transmute(
        scenario, shock_id, regime, variable, t,
        contribution = delta * fevd_share,
        method = "fevd"
      )
  } else {
    stop("[decompose_forecast] Unknown method: ", chosen)
  }
  
  # --- Reconcile additivity (ensure Σ_k contrib == Δ) ---------------------------
  reconciled_tbl <- reconcile_additivity(
    contrib_tbl = contrib_tbl,
    delta_tbl   = delta_tbl,
    method      = reconcile,
    tol         = tol
  )
  
  # --- Compact scenario × variable summary --------------------------------------
  summary_tbl <- reconciled_tbl %>%
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id) %>%
    dplyr::summarise(
      sum_contribution     = sum(.data$contribution, na.rm = TRUE),
      l1_contribution      = sum(abs(.data$contribution), na.rm = TRUE),
      max_abs_contribution = suppressWarnings(max(abs(.data$contribution), na.rm = TRUE)),
      .groups = "drop"
    )
  
  list(
    delta_tbl       = delta_tbl,
    contrib_tbl     = contrib_tbl,
    reconciled_tbl  = reconciled_tbl,
    summary         = summary_tbl
  )
}