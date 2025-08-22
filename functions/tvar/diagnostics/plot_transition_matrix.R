#' Heatmap for 2x2 transition matrix with clean labels and implicit QA
#' @export
plot_transition_matrix <- function(M, save_path = NULL) {
  if (is.list(M)) M <- M$matrix
  if (is.null(M)) return(invisible(NULL))
  stopifnot(is.matrix(M), all(dim(M) == c(2,2)))
  
  df <- tibble::tibble(
    from = rep(c("low","high"), each = 2),
    to   = rep(c("low","high"), times = 2),
    p    = c(M[1,1], M[1,2], M[2,1], M[2,2])
  )
  row_sum_ok <- all(abs(rowsums <- tapply(df$p, df$from, sum) - 1) < 1e-6)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = to, y = from, fill = p, label = sprintf("%.2f", p))) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(size = 5) +
    ggplot2::scale_fill_gradient(limits = c(0,1), low = "#f0f0f0", high = "#1b1b1b") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Transition matrix (row-stochastic)",
      subtitle = if (row_sum_ok) "Row sums ≈ 1 ✓" else "Check row-normalization!",
      x = "to", y = "from", fill = "p"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right")
  
  if (!is.null(save_path)) {
    fs::dir_create(dirname(save_path))
    ggplot2::ggsave(save_path, p, width = 4.8, height = 4.2, dpi = 300)
  }
  invisible(p)
}
