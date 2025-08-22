#' Stationary distribution, expected durations, half-lives and ergodicity checks
#'
#' @param transition_matrix 2x2 row-stochastic matrix OR a list from compute_transition_matrix()
#' @return list(stationary, expected_duration, half_life, mixing_rate, ergodic, reversible)
#' @export
compute_regime_probabilities <- function(transition_matrix) {
  M <- if (is.list(transition_matrix)) transition_matrix$matrix else transition_matrix
  if (is.null(M)) return(NULL)
  stopifnot(is.matrix(M), all(dim(M) == c(2,2)))
  
  # Ergodicity (aperiodic + irreducible) for 2-state chain: need positive off-diagonals
  irreducible <- (M[1,2] > 0 && M[2,1] > 0)
  aperiodic   <- (M[1,1] > 0 && M[2,2] > 0)
  ergodic     <- irreducible && aperiodic
  
  # Stationary π solves πM=π
  ev <- eigen(t(M))
  i1 <- which.min(abs(ev$values - 1))
  pi <- Re(ev$vectors[, i1]); pi <- pi / sum(pi); names(pi) <- c("low","high")
  
  # Expected duration in each regime: 1 / (1 - p_ii)
  E_low  <- if (M[1,1] < 1) 1 / (1 - M[1,1]) else Inf
  E_high <- if (M[2,2] < 1) 1 / (1 - M[2,2]) else Inf
  
  # Half-life of persistence (geometric spells), per regime: log(0.5)/log(p_ii), if 0<p_ii<1
  hl <- function(pii) if (pii > 0 && pii < 1) log(0.5) / log(pii) else NA_real_
  HL_low  <- hl(M[1,1])
  HL_high <- hl(M[2,2])
  
  # Mixing rate (second eigenvalue magnitude)
  lam <- sort(Mod(ev$values), decreasing = TRUE)
  mixing <- if (length(lam) >= 2) lam[2] else NA_real_
  
  list(
    stationary        = pi,
    expected_duration = c(low = E_low, high = E_high),
    half_life         = c(low = HL_low, high = HL_high),
    mixing_rate       = mixing,
    ergodic           = ergodic,
    reversible        = isTRUE(abs(M[1,2]*pi["low"] - M[2,1]*pi["high"]) < 1e-10)  # detailed balance
  )
}