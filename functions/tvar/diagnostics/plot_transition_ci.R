# Plot bootstrap CIs for p11 and p22
# Accepts either:
#  - ci_df with columns (param, lo, hi)   [what bootstrap_transition_matrix() returns]
#  - or a long tidy with columns (param, q, val) where q in {lo, hi}
# Saves optional PNG and returns ggplot object.

plot_transition_ci <- function(ci_df, save_path = NULL, title = "Transition persistence (CIs)") {
  if (is.null(ci_df) || !nrow(ci_df)) return(invisible(NULL))
  
  # Normalize to (param, lo, hi)
  norm <- NULL
  cols <- names(ci_df)
  if (all(c("param","lo","hi") %in% cols)) {
    norm <- ci_df[, c("param","lo","hi")]
  } else if (all(c("param","q","val") %in% cols)) {
    wide <- tidyr::pivot_wider(ci_df, names_from = q, values_from = val)
    stopifnot(all(c("lo","hi") %in% names(wide)))
    norm <- wide[, c("param","lo","hi")]
  } else {
    stop("ci_df must have (param, lo, hi) or (param, q, val) columns.")
  }
  
  # Order and label
  norm$param <- factor(norm$param, levels = c("p11","p22"), labels = c("low→low (p11)","high→high (p22)"))
  
  df <- data.frame(
    param = norm$param,
    mid   = (norm$lo + norm$hi)/2,
    lo    = norm$lo,
    hi    = norm$hi
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = param, y = mid, ymin = lo, ymax = hi)) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "Probability", title = title) +
    ggplot2::theme_minimal(base_size = 12)
  
  if (!is.null(save_path)) {
    fs::dir_create(dirname(save_path))
    ggplot2::ggsave(save_path, p, width = 5.5, height = 4, dpi = 300)
  }
  invisible(p)
}