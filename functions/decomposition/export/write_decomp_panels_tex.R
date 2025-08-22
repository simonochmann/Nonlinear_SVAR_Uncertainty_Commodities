#' Write LaTeX appendix panels: stacked (scenario × variable) multi-tables
#'
#' Creates a single .tex file that groups multiple (scenario, variable) tables
#' into a compact appendix panel using `longtable`. Good for SSRN/BIS appendices.
#'
#' @param contrib_tbl tibble: scenario, shock_id, regime, variable, t, contribution
#' @param save_path .tex output path
#' @param top_n integer shocks per subtable (by |sum over regimes| at terminal horizon)
#' @param include_residual logical
#' @param digits rounding
#' @param caption main caption for the panel
#'
#' @return save_path (invisibly)
write_decomp_panels_tex <- function(
    contrib_tbl,
    save_path = here::here("output","tables","decomposition_panels.tex"),
    top_n = 8,
    include_residual = TRUE,
    digits = 3,
    caption = "Decomposition of forecast deltas by shock (terminal horizon)."
) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("knitr", quietly = TRUE)
  
  has_kableExtra <- requireNamespace("kableExtra", quietly = TRUE)
  fs::dir_create(fs::path_dir(save_path))
  
  df <- contrib_tbl
  if (!include_residual) df <- dplyr::filter(df, .data$shock_id != "residual")
  
  stopifnot(all(c("scenario","shock_id","regime","variable","t","contribution") %in% names(df)))
  
  # terminal horizon per facet
  term_h <- df |>
    dplyr::group_by(.data$scenario, .data$variable) |>
    dplyr::summarise(h = max(.data$t, na.rm = TRUE), .groups = "drop")
  
  # build all subtables first
  subtables <- list()
  
  keys <- df |>
    dplyr::distinct(.data$scenario, .data$variable) |>
    dplyr::arrange(.data$scenario, .data$variable)
  
  for (i in seq_len(nrow(keys))) {
    scen <- keys$scenario[i]; var <- keys$variable[i]
    h <- term_h |>
      dplyr::filter(.data$scenario == scen, .data$variable == var) |>
      dplyr::pull(.data$h)
    if (!length(h)) next
    h <- h[1]
    
    tbl <- df |>
      dplyr::filter(.data$scenario == scen, .data$variable == var, .data$t == h) |>
      dplyr::group_by(.data$shock_id) |>
      dplyr::summarise(contribution = sum(.data$contribution, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(abs(.data$contribution))) |>
      dplyr::slice_head(n = top_n)
    
    kbl <- knitr::kable(
      tbl,
      format = "latex",
      booktabs = has_kableExtra,
      digits = digits,
      col.names = c("Shock", "Contribution"),
      caption = paste0(scen, " – ", var, " (t = ", h, ")"),
      escape = TRUE,
      align = c("l","r")
    )
    if (has_kableExtra) {
      kbl <- kableExtra::kable_styling(kbl, latex_options = c("hold_position"))
    }
    subtables[[paste(scen, var, sep = " / ")]] <- kbl
  }
  
  # stitch into a longtable panel
  header <- "\\begin{center}\n\\setlength{\\tabcolsep}{6pt}\n"
  header <- paste0(header, "\\begin{longtable}{@{} l r @{}}\n\\caption{", caption, "}\\\\\n")
  header <- paste0(header, "\\toprule\nShock & Contribution\\\\\n\\midrule\n\\endfirsthead\n")
  header <- paste0(header, "\\multicolumn{2}{@{}l}{\\emph{(continued)}}\\\\\n\\toprule\nShock & Contribution\\\\\n\\midrule\n\\endhead\n")
  
  footer <- "\\bottomrule\n\\end{longtable}\n\\end{center}\n"
  
  body <- ""
  sep_line <- "\n\\addlinespace[0.5em]\n\\midrule\n\\addlinespace[0.5em]\n"
  
  # Insert a titled row before each subtable (scenario – variable)
  for (nm in names(subtables)) {
    body <- paste0(
      body,
      "\\multicolumn{2}{@{}l}{\\textbf{", nm, "}}\\\\\n",
      "\\addlinespace[0.25em]\n",
      subtables[[nm]],  # each is its own small table body
      sep_line
    )
  }
  
  panel_tex <- paste0(header, body, footer)
  
  cat(panel_tex, file = save_path)
  invisible(save_path)
}
