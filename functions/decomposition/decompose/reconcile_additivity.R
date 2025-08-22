#' Reconcile Σ_k contributions with Δ (per [uncertainty], scenario, regime, variable, t)
#'
#' @param contrib_tbl tibble: [uncertainty?], scenario, shock_id, regime, variable, t, contribution
#' @param delta_tbl   tibble:  [uncertainty?], scenario, regime, variable, t, delta
#' @param method      "residual_bucket" (default) or "proportional_scale"
#' @param tol         numeric tolerance below which we treat sums as zero
#'
#' @return tibble: [uncertainty?], scenario, shock_id, regime, variable, t, contribution (adjusted or with residual)
#'
reconcile_additivity <- function(
    contrib_tbl,
    delta_tbl,
    method = c("residual_bucket","proportional_scale"),
    tol = 1e-10
) {
  requireNamespace("dplyr", quietly = TRUE)
  
  method <- match.arg(method)
  
  # ---- Flexible column checks (uncertainty is optional) -----------------------
  must_c <- c("scenario","shock_id","regime","variable","t","contribution")
  must_d <- c("scenario","regime","variable","t","delta")
  stopifnot(all(must_c %in% names(contrib_tbl)))
  stopifnot(all(must_d %in% names(delta_tbl)))
  
  # Composite key: include 'uncertainty' if both tables have it
  base_key <- c("scenario","regime","variable","t")
  has_unc  <- "uncertainty" %in% names(contrib_tbl) && "uncertainty" %in% names(delta_tbl)
  key_cols <- if (has_unc) c("uncertainty", base_key) else base_key
  
  # ---- Totals & residuals -----------------------------------------------------
  sums <- contrib_tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(total = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
    dplyr::inner_join(delta_tbl, by = key_cols) %>%
    dplyr::mutate(residual = delta - total)
  
  if (identical(method, "residual_bucket")) {
    # Put any gap into a "residual" pseudo-shock
    res_rows <- sums %>%
      dplyr::filter(abs(residual) > tol) %>%
      dplyr::transmute(
        dplyr::across(dplyr::all_of(key_cols)),
        shock_id     = "residual",
        contribution = residual
      )
    
    out <- dplyr::bind_rows(
      contrib_tbl %>% dplyr::select(dplyr::all_of(c(key_cols, "shock_id", "contribution"))),
      res_rows
    ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(key_cols, "shock_id"))))
    
    return(out)
  }
  
  # ---- proportional_scale -----------------------------------------------------
  # Spread the residual proportionally to existing contributions
  scaled <- contrib_tbl %>%
    dplyr::left_join(
      sums %>% dplyr::select(dplyr::all_of(c(key_cols, "total", "residual", "delta"))),
      by = key_cols
    ) %>%
    dplyr::mutate(
      # If total ~ 0, we can't distribute proportionally here (handled below)
      share        = dplyr::if_else(abs(total) > tol, contribution / total, 0),
      contribution = dplyr::if_else(abs(total) > tol,
                                    contribution + share * residual,
                                    contribution)
    ) %>%
    dplyr::select(dplyr::all_of(c(key_cols, "shock_id", "contribution")))
  
  # For cells with |total| ≤ tol but |delta| > tol, create a residual bucket row
  zeros <- sums %>% dplyr::filter(abs(total) <= tol & abs(delta) > tol)
  if (nrow(zeros) > 0) {
    res_rows <- zeros %>%
      dplyr::transmute(
        dplyr::across(dplyr::all_of(key_cols)),
        shock_id     = "residual",
        contribution = delta
      )
    scaled <- dplyr::bind_rows(scaled, res_rows)
  }
  
  scaled %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(key_cols, "shock_id"))))
}