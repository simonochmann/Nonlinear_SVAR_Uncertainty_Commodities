#' Compute a 2x2 Markov transition matrix from a regime index
#'
#' Robust to NA runs and allows Laplace smoothing to avoid 0/NaN rows.
#' Returns both the matrix and row-wise standard errors/CI (Wald) for p_ii.
#'
#' @param model list with metadata$regime_index OR
#' @param regime_index optional integer/factor vector (1=low, 2=high)
#' @param laplace numeric Laplace add-k smoothing for row counts (default 0)
#' @param conf numeric, Wald CI level for p_ii (default 0.95)
#' @return list(matrix, counts, n_row, se, ci, note)
#' @export
compute_transition_matrix <- function(model = NULL,
                                      regime_index = NULL,
                                      laplace = 0,
                                      conf = 0.95) {
  if (is.null(regime_index)) {
    regime_index <- tryCatch(model$metadata$regime_index, error = function(e) NULL)
  }
  if (is.null(regime_index) || length(regime_index) < 2)
    return(list(matrix = NULL, counts = NULL, n_row = c(low = 0L, high = 0L),
                se = c(low = NA_real_, high = NA_real_), ci = matrix(NA_real_, 2, 2),
                note = "insufficient length"))
  
  x <- as.integer(regime_index)
  # Drop NA transitions
  keep <- !is.na(x[-length(x)]) & !is.na(x[-1])
  if (!any(keep)) {
    return(list(matrix = NULL, counts = NULL, n_row = c(low = 0L, high = 0L),
                se = c(low = NA_real_, high = NA_real_), ci = matrix(NA_real_, 2, 2),
                note = "all transitions NA"))
  }
  from <- x[-length(x)][keep]
  to   <- x[-1][keep]
  
  # Count table on {1,2}
  tab <- table(factor(from, levels = c(1,2)),
               factor(to,   levels = c(1,2)))
  # Laplace smoothing (add-k per cell in row)
  if (laplace > 0) tab <- tab + laplace
  
  n1 <- sum(tab[1, ])
  n2 <- sum(tab[2, ])
  M  <- matrix(0, 2, 2, dimnames = list(from = c("low","high"), to = c("low","high")))
  if (n1 > 0) M[1, ] <- tab[1, ] / n1
  if (n2 > 0) M[2, ] <- tab[2, ] / n2
  
  # Row-wise Wald SE for p_ii = Binomial(n_i, p_ii)
  p11 <- if (n1 > 0) M[1,1] else NA_real_
  p22 <- if (n2 > 0) M[2,2] else NA_real_
  se1 <- if (n1 > 0) sqrt(p11 * (1 - p11) / n1) else NA_real_
  se2 <- if (n2 > 0) sqrt(p22 * (1 - p22) / n2) else NA_real_
  
  z  <- stats::qnorm(0.5 + conf/2)
  ci <- matrix(NA_real_, 2, 2, dimnames = list(param = c("p11","p22"), bound = c("lo","hi")))
  ci["p11","lo"] <- max(0, p11 - z * se1); ci["p11","hi"] <- min(1, p11 + z * se1)
  ci["p22","lo"] <- max(0, p22 - z * se2); ci["p22","hi"] <- min(1, p22 + z * se2)
  
  list(
    matrix = M,
    counts = tab,
    n_row  = c(low = n1, high = n2),
    se     = c(low = se1, high = se2),
    ci     = ci,
    note   = if (laplace > 0) glue::glue("Laplace smoothing add-k={laplace}") else "no smoothing"
  )
}