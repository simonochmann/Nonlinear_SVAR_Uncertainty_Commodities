#' Markdown summary with interpretation-ready bullets
#' @export
log_regime_dynamics_summary <- function(output_dir, filename_md, model_path, regime_summary, transition_matrix) {
  fs::dir_create(output_dir)
  md_path <- file.path(output_dir, filename_md)
  
  fmt <- function(x, d=3) ifelse(is.finite(x), format(round(x, d), nsmall = d), "NA")
  
  lines <- c(
    "# Regime Dynamics Summary",
    "",
    glue::glue("- **Model**: `{basename(model_path)}`"),
    glue::glue("- **Obs by regime**: low={regime_summary$n_low}, high={regime_summary$n_high}"),
    glue::glue("- **Shares**: low={fmt(regime_summary$share_low)}, high={fmt(regime_summary$share_high)}"),
    glue::glue("- **Spectral radius (stability)**: low={fmt(regime_summary$rho_low,6)}, high={fmt(regime_summary$rho_high,6)}"),
    glue::glue("- **Median spell durations**: low={fmt(regime_summary$median_duration_low,1)}, high={fmt(regime_summary$median_duration_high,1)}"),
    glue::glue("- **Max spell durations**: low={regime_summary$max_duration_low}, high={regime_summary$max_duration_high}"),
    glue::glue("- **Transition p_ii**: p11={fmt(regime_summary$p11)}, p22={fmt(regime_summary$p22)}"),
    glue::glue("- **Stationary dist.**: π_low={fmt(regime_summary$pi_low)}, π_high={fmt(regime_summary$pi_high)}"),
    glue::glue("- **Expected durations**: E_low={fmt(regime_summary$E_low,2)}, E_high={fmt(regime_summary$E_high,2)}"),
    glue::glue("- **Half-life (periods)**: HL_low={fmt(regime_summary$HL_low,2)}, HL_high={fmt(regime_summary$HL_high,2)}"),
    glue::glue("- **Mixing rate (|λ₂|)**: {fmt(regime_summary$mixing_rate,3)}"),
    glue::glue("- **Ergodic chain**: {regime_summary$ergodic}"),
    ""
  )
  
  if (!is.null(transition_matrix)) {
    M <- if (is.list(transition_matrix)) transition_matrix$matrix else transition_matrix
    lines <- c(lines,
               "## Transition Matrix",
               "",
               "| from \\ to | low | high |",
               "|---|---:|---:|",
               glue::glue("| low  | {fmt(M[1,1])} | {fmt(M[1,2])} |"),
               glue::glue("| high | {fmt(M[2,1])} | {fmt(M[2,2])} |"),
               ""
    )
  }
  writeLines(lines, md_path)
  invisible(md_path)
}