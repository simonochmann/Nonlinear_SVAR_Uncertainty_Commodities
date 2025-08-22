#' Write compact LaTeX tables (per scenario × variable) of shock contributions
#'
#' Creates one table per (scenario, variable) at a selected horizon, with top_n
#' shocks by |contribution| and optional residual row.
#'
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param save_dir directory for .tex files
#' @param horizon integer (t). If NULL, uses max t per (scenario, variable)
#' @param top_n integer number of shocks to show (by |sum over regimes|)
#' @param include_residual logical
#' @param digits integer rounding digits
#' @param booktabs logical: use \\toprule/\\midrule/\\bottomrule if kableExtra present
#'
#' @return vector of file paths (one .tex per (scenario, variable))
write_decomp_tables_tex <- function(
    contrib_tbl,
    save_dir = here::here("output","tables"),
    horizon = NULL,
    top_n = 8,
    include_residual = TRUE,
    digits = 3,
    booktabs = TRUE
) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("knitr", quietly = TRUE)
  
  has_kableExtra <- requireNamespace("kableExtra", quietly = TRUE)
  fs::dir_create(save_dir)
  
  stopifnot(all(c("scenario","shock_id","regime","variable","t","contribution") %in% names(contrib_tbl)))
  
  # collapse over regimes at chosen horizon per facet
  base <- contrib_tbl
  
  if (!include_residual) base <- dplyr::filter(base, .data$shock_id != "residual")
  
  # determine horizon per (scenario, variable) if not provided globally
  if (is.null(horizon)) {
    h_tbl <- base |>
      dplyr::group_by(.data$scenario, .data$variable) |>
      dplyr::summarise(h = max(.data$t, na.rm = TRUE), .groups = "drop")
  } else {
    h_tbl <- base |>
      dplyr::distinct(.data$scenario, .data$variable) |>
      dplyr::mutate(h = horizon)
  }
  
  out_paths <- c()
  
  split_keys <- base |>
    dplyr::distinct(.data$scenario, .data$variable) |>
    dplyr::arrange(.data$scenario, .data$variable)
  
  for (i in seq_len(nrow(split_keys))) {
    scen <- split_keys$scenario[i]
    var  <- split_keys$variable[i]
    h    <- h_tbl |>
      dplyr::filter(.data$scenario == scen, .data$variable == var) |>
      dplyr::pull(.data$h)
    if (length(h) == 0) next
    h <- h[1]
    
    tbl <- base |>
      dplyr::filter(.data$scenario == scen, .data$variable == var, .data$t == h) |>
      dplyr::group_by(.data$shock_id) |>
      dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(abs(.data$contribution)))
    
    if (!is.null(top_n) && nrow(tbl) > top_n) {
      tbl <- dplyr::slice_head(tbl, n = top_n)
    }
    
    # prettify
    tab_title <- paste0("Contributions at horizon t = ", h, " (", scen, " – ", var, ")")
    latex_path <- fs::path(save_dir, paste0("decomp_", gsub("[^A-Za-z0-9]+","_", paste(scen, var, sep="_")), "_t", h, ".tex"))
    
    # build kable
    kbl <- knitr::kable(
      tbl,
      format = "latex",
      booktabs = has_kableExtra && booktabs,
      digits = digits,
      col.names = c("Shock", "Contribution"),
      caption = tab_title,
      escape = TRUE,
      align = c("l","r")
    )
    
    if (has_kableExtra) {
      kbl <- kableExtra::kable_styling(kbl, latex_options = c("hold_position"))
    }
    
    cat(kbl, file = latex_path)
    out_paths <- c(out_paths, latex_path)
  }
  
  out_paths
}