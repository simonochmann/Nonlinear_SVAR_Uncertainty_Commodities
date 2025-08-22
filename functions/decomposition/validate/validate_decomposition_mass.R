#' Validate mass balance: sum(contributions) ≈ Δ within tolerance
#'
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param delta_tbl   tibble: scenario, regime, variable, t, delta
#' @param tol numeric absolute tolerance for residual at each key
#' @param mode "strict" (stop on violations), "warn" (warning), "report" (no throws)
#' @param include_residual logical: if TRUE, ignore rows with shock_id == "residual" in sum()
#' @return invisible(list(ok, n_violations, max_abs_residual, residuals_tbl))
validate_decomposition_mass <- function(
    contrib_tbl,
    delta_tbl,
    tol = 1e-10,
    mode = c("strict","warn","report"),
    include_residual = FALSE
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  
  mode <- match.arg(mode)
  
  # Columns
  needed_c <- c("scenario","shock_id","regime","variable","t","contribution")
  needed_d <- c("scenario","regime","variable","t","delta")
  miss_c <- setdiff(needed_c, names(contrib_tbl))
  miss_d <- setdiff(needed_d, names(delta_tbl))
  if (length(miss_c)) stop(glue::glue("contrib_tbl missing columns: {paste(miss_c, collapse=', ')}"))
  if (length(miss_d)) stop(glue::glue("delta_tbl missing columns: {paste(miss_d, collapse=', ')}"))
  
  # Optionally drop residual bucket from the sum
  sum_tbl <- contrib_tbl
  if (!include_residual && "residual" %in% contrib_tbl$shock_id) {
    sum_tbl <- dplyr::filter(sum_tbl, .data$shock_id != "residual")
  }
  
  # Sum contributions on keys
  sums <- sum_tbl |>
    dplyr::group_by(.data$scenario, .data$regime, .data$variable, .data$t) |>
    dplyr::summarise(sum_contrib = sum(.data$contribution, na.rm = TRUE), .groups = "drop")
  
  # Join with Δ
  res_tbl <- delta_tbl |>
    dplyr::inner_join(sums, by = c("scenario","regime","variable","t")) |>
    dplyr::mutate(residual = .data$delta - .data$sum_contrib,
                  abs_residual = abs(.data$residual),
                  within_tol = .data$abs_residual <= tol)
  
  n_viol <- sum(!res_tbl$within_tol, na.rm = TRUE)
  max_abs <- suppressWarnings(max(res_tbl$abs_residual, na.rm = TRUE))
  ok <- n_viol == 0
  
  # Act per mode
  if (!ok) {
    msg <- glue::glue("Mass-balance violations: {n_viol} rows exceed tol={tol}; max |residual| = {signif(max_abs, 4)}")
    if (mode == "strict") stop(msg)
    if (mode == "warn")   warning(msg, call. = FALSE)
  }
  
  invisible(list(
    ok = ok,
    n_violations = n_viol,
    max_abs_residual = max_abs,
    residuals_tbl = res_tbl
  ))
}
