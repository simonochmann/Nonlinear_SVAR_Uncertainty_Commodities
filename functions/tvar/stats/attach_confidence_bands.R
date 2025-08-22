attach_confidence_bands <- function(dpaths, level = 0.90) {
  # dpaths: [B x H x k]; return list(median, lo, hi) as [H x k]
  B <- dim(dpaths)[1]; H <- dim(dpaths)[2]; k <- dim(dpaths)[3]
  probs <- c((1-level)/2, 0.5, 1-(1-level)/2)
  qfun <- function(x) stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  out <- apply(dpaths, c(2,3), qfun)  # [3 x H x k]
  list(
    lo     = t(out[1, , , drop=TRUE]),
    median = t(out[2, , , drop=TRUE]),
    hi     = t(out[3, , , drop=TRUE])
  )
}
