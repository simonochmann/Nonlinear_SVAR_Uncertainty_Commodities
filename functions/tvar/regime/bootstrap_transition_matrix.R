# Bootstrap CIs for a 2x2 transition matrix from a regime index (1=low, 2=high)
# Robust: supports IID (default) or simple moving-block bootstrap for dependence.
# Returns:
#   list(
#     tm_hat  = 2x2 matrix with dimnames (low/high),
#     tm_tidy = data.frame(from,to,p)   # normalized rows
#     ci      = data.frame(param, lo, hi, n_row_low, n_row_high),
#     draws   = data.frame(p11, p22)
#   )

bootstrap_transition_matrix <- function(
    regime_index,
    B = 2000L,
    block_len = NULL,   # e.g. 10L for block bootstrap; NULL => IID
    conf_level = 0.95,
    seed = NULL,
    verbose = TRUE
) {
  stopifnot(is.numeric(B), B >= 100)
  idx <- as.integer(regime_index)
  if (length(idx) < 3) stop("regime_index too short for transitions.")
  
  # Build empirical transition matrix once (point estimate)
  from0 <- idx[-length(idx)]
  to0   <- idx[-1]
  tab0  <- table(factor(from0, levels = c(1,2)), factor(to0, levels = c(1,2)))
  Mhat  <- matrix(0, 2, 2, dimnames = list(from = c("low","high"), to = c("low","high")))
  for (i in 1:2) {
    s <- sum(tab0[i, ])
    if (s > 0) Mhat[i, ] <- tab0[i, ] / s
  }
  
  # Prepare vector of transition pairs (for resampling)
  trans_pairs <- cbind(from = from0, to = to0)
  n_pairs <- nrow(trans_pairs)
  
  # Helper: compute row-normalized 2x2 from a pair matrix
  tm_from_pairs <- function(pairs) {
    if (nrow(pairs) == 0) return(Mhat*NA_real_)
    tb <- table(factor(pairs[,1], levels = c(1,2)), factor(pairs[,2], levels = c(1,2)))
    M <- matrix(0, 2, 2, dimnames = list(from = c("low","high"), to = c("low","high")))
    for (i in 1:2) {
      s <- sum(tb[i, ])
      if (s > 0) M[i, ] <- tb[i, ] / s
    }
    M
  }
  
  # Resampler: IID or simple moving-block bootstrap on transitions
  sample_pairs <- function() {
    if (is.null(block_len) || !is.finite(block_len) || block_len < 2) {
      # IID bootstrap on pairs
      trans_pairs[sample.int(n_pairs, size = n_pairs, replace = TRUE), , drop = FALSE]
    } else {
      L <- as.integer(block_len)
      # Build start indices for blocks
      starts <- sample.int(n_pairs - L + 1L, size = ceiling(n_pairs / L), replace = TRUE)
      out <- do.call(rbind, lapply(starts, function(s) trans_pairs[s:(s+L-1L), , drop = FALSE]))
      out[seq_len(n_pairs), , drop = FALSE]
    }
  }
  
  if (!is.null(seed)) set.seed(seed)
  if (isTRUE(verbose)) message("[bootstrap] B=", B, " block_len=", if (is.null(block_len)) "IID" else block_len)
  
  draws <- matrix(NA_real_, nrow = B, ncol = 2, dimnames = list(NULL, c("p11","p22")))
  for (b in seq_len(B)) {
    pb <- sample_pairs()
    Mb <- tm_from_pairs(pb)
    draws[b, "p11"] <- Mb["low",  "low" ]
    draws[b, "p22"] <- Mb["high", "high"]
  }
  draws_df <- as.data.frame(draws)
  
  # Percentile CIs
  qlo <- (1 - conf_level)/2
  qhi <- 1 - qlo
  ci <- data.frame(
    param      = c("p11","p22"),
    lo         = c(quantile(draws_df$p11, qlo, na.rm = TRUE),
                   quantile(draws_df$p22, qlo, na.rm = TRUE)),
    hi         = c(quantile(draws_df$p11, qhi, na.rm = TRUE),
                   quantile(draws_df$p22, qhi, na.rm = TRUE)),
    n_row_low  = sum(from0 == 1L),
    n_row_high = sum(from0 == 2L),
    row.names  = NULL
  )
  
  # Tidy matrix (from/to/p) with standard names
  tidy <- tibble::tibble(
    from = rep(c("low","high"), each = 2),
    to   = rep(c("low","high"), times = 2),
    p    = as.numeric(c(Mhat["low","low"], Mhat["low","high"],
                        Mhat["high","low"], Mhat["high","high"]))
  )
  
  list(tm_hat = Mhat, tm_tidy = tidy, ci = ci, draws = draws_df)
}