#' Allocate Δ across shocks, either by pathwise singles or FEVD shares
#'
#' Two modes:
#' 1) Pathwise: requires `single_shock_paths` and (optionally) `scenario_shock_map`.
#'    Contribution_k = weight_k * (single_shock_path_k - baseline).
#'    Works even for nonlinear models; non-additivity handled later.
#' 2) FEVD: requires `fevd_shares` (regime-aware). Contribution_k(t) = fevd_share_k(t) * Δ(t).
#'
#' @param baseline tibble: regime, variable, t, value
#' @param shocked_delta tibble: scenario, regime, variable, t, delta  (precomputed)
#' @param single_shock_paths optional tibble: shock_id, regime, variable, t, value
#' @param scenario_shock_map optional tibble: scenario, shock_id, weight (defaults to 1)
#' @param fevd_shares optional tibble: regime, response, impulse, t, fevd_share
#'
#' @return tibble: scenario, shock_id, regime, variable, t, contribution
#'
decompose_by_shock <- function(
    baseline,
    shocked_delta,
    single_shock_paths = NULL,
    scenario_shock_map = NULL,
    fevd_shares = NULL
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  # Inputs
  stopifnot(all(c("regime","variable","t","value") %in% names(baseline)))
  stopifnot(all(c("scenario","regime","variable","t","delta") %in% names(shocked_delta)))
  
  # ---- Mode A: Pathwise (preferred if singles provided)
  if (!is.null(single_shock_paths)) {
    stopifnot(all(c("shock_id","regime","variable","t","value") %in% names(single_shock_paths)))
    
    # Map scenarios to shocks (default: each scenario == one shock id with weight 1)
    if (is.null(scenario_shock_map)) {
      scenario_shock_map <- shocked_delta %>%
        dplyr::distinct(.data$scenario) %>%
        dplyr::mutate(shock_id = .data$scenario, weight = 1)
    } else {
      if (!all(c("scenario","shock_id") %in% names(scenario_shock_map))) {
        stop("scenario_shock_map must have columns: scenario, shock_id, (optional) weight")
      }
      if (!"weight" %in% names(scenario_shock_map)) {
        scenario_shock_map$weight <- 1
      }
    }
    
    # Precompute Δ for each single shock relative to the common baseline
    single_delta <- single_shock_paths %>%
      dplyr::left_join(
        baseline %>% dplyr::rename(baseline = value),
        by = c("regime","variable","t")
      ) %>%
      dplyr::mutate(
        baseline = dplyr::coalesce(.data$baseline, 0),
        delta    = .data$value - .data$baseline
      ) %>%
      dplyr::select(.data$shock_id, .data$regime, .data$variable, .data$t, .data$delta)
    
    # Expand by scenario map and weight
    contrib <- scenario_shock_map %>%
      dplyr::inner_join(single_delta, by = "shock_id") %>%
      dplyr::mutate(contribution = .data$weight * .data$delta) %>%
      dplyr::select(.data$scenario, .data$shock_id, .data$regime, .data$variable, .data$t, .data$contribution)
    
    # Keep only scenarios present in shocked_delta
    contrib <- contrib %>%
      dplyr::semi_join(shocked_delta %>% dplyr::distinct(.data$scenario), by = "scenario")
    
    return(contrib)
  }
  
  # ---- Mode B: FEVD shares
  if (is.null(fevd_shares)) {
    stop("Provide either single_shock_paths (pathwise) or fevd_shares (FEVD) to allocate Δ across shocks.")
  }
  stopifnot(all(c("regime","response","impulse","t","fevd_share") %in% names(fevd_shares)))
  
  # Normalize FEVD so that Σ_shock fevd_share = 1 for each (regime, response, t)
  fevd_norm <- fevd_shares %>%
    dplyr::group_by(.data$regime, .data$response, .data$t) %>%
    dplyr::mutate(total = sum(.data$fevd_share, na.rm = TRUE),
                  fevd_share = dplyr::if_else(total > 0, .data$fevd_share / total, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(variable = response, shock_id = impulse) %>%
    dplyr::select(.data$regime, .data$variable, .data$t, .data$shock_id, .data$fevd_share)
  
  # Join Δ and allocate by shares
  contrib <- shocked_delta %>%
    dplyr::inner_join(fevd_norm, by = c("regime","variable","t")) %>%
    dplyr::mutate(contribution = .data$fevd_share * .data$delta) %>%
    dplyr::select(.data$scenario, .data$shock_id, .data$regime, .data$variable, .data$t, .data$contribution)
  
  contrib
}
