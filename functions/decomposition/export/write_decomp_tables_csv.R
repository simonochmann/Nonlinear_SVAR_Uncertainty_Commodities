#' Write decomposition tables to CSV (tidy + summaries)
#'
#' @param delta_tbl tibble: scenario, regime, variable, t, delta
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param out_dir directory to write CSVs
#' @param include_residual logical: include "residual" rows in contrib exports
#' @param by_horizon_summary logical: write per-(scenario,variable,t) summaries
#' @return list of written file paths (named)
write_decomp_tables_csv <- function(
    delta_tbl,
    contrib_tbl,
    out_dir = here::here("data","scenarios"),
    include_residual = TRUE,
    by_horizon_summary = TRUE
) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  
  fs::dir_create(out_dir)
  
  # --- sanitize
  stopifnot(all(c("scenario","regime","variable","t","delta") %in% names(delta_tbl)))
  stopifnot(all(c("scenario","shock_id","regime","variable","t","contribution") %in% names(contrib_tbl)))
  
  if (!include_residual) {
    contrib_tbl <- dplyr::filter(contrib_tbl, .data$shock_id != "residual")
  }
  
  paths <- list()
  
  # 1) Tidy delta
  paths$delta_tidy <- fs::path(out_dir, "decomposition_delta_tidy.csv")
  readr::write_csv(delta_tbl, paths$delta_tidy)
  
  # 2) Tidy contributions
  paths$contrib_tidy <- fs::path(out_dir, "decomposition_contributions_tidy.csv")
  readr::write_csv(contrib_tbl, paths$contrib_tidy)
  
  # 3) Wide contributions (shock_id → columns) for quick Excel inspection
  contrib_wide <- contrib_tbl |>
    tidyr::pivot_wider(names_from = shock_id, values_from = contribution)
  paths$contrib_wide <- fs::path(out_dir, "decomposition_contributions_wide.csv")
  readr::write_csv(contrib_wide, paths$contrib_wide)
  
  # 4) Per horizon summary (Σ contrib, residual = Δ − Σ)
  if (by_horizon_summary) {
    sums <- contrib_tbl |>
      dplyr::group_by(.data$scenario, .data$regime, .data$variable, .data$t) |>
      dplyr::summarise(sum_contrib = sum(.data$contribution, na.rm = TRUE), .groups = "drop") |>
      dplyr::inner_join(delta_tbl, by = c("scenario","regime","variable","t")) |>
      dplyr::mutate(residual = .data$delta - .data$sum_contrib)
    paths$horizon_summary <- fs::path(out_dir, "decomposition_by_horizon_summary.csv")
    readr::write_csv(sums, paths$horizon_summary)
  }
  
  # 5) Per scenario × variable totals by shock
  totals <- contrib_tbl |>
    dplyr::group_by(.data$scenario, .data$variable, .data$shock_id) |>
    dplyr::summarise(
      total = sum(.data$contribution, na.rm = TRUE),
      l1    = sum(abs(.data$contribution), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$scenario, .data$variable, dplyr::desc(abs(.data$total)))
  paths$totals_by_shock <- fs::path(out_dir, "decomposition_totals_by_shock.csv")
  readr::write_csv(totals, paths$totals_by_shock)
  
  paths
}