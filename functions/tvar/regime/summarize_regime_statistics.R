# functions/tvar/regime/summarize_regime_statistics.R
# Tidy one-row summary for logging and CSV export (robust TM coercion)

summarize_regime_statistics <- function(model, durations, transition_matrix, regime_probabilities) {
  
  `%||%` <- function(a,b) if (is.null(a)) b else a
  
  # Coerce transition_matrix to 2x2 matrix in order (low, high)
  to_matrix <- function(TM) {
    if (is.null(TM)) return(NULL)
    
    # unwrap common list containers
    if (is.list(TM)) {
      pick <- NULL
      for (nm in c("M","matrix","tm","trans","transition","transition_matrix")) {
        if (!is.null(TM[[nm]])) { pick <- TM[[nm]]; break }
      }
      if (!is.null(pick)) return(to_matrix(pick))
      # if it's a data.frame, fall through
      if (!is.data.frame(TM)) {
        stop("Unsupported list form for transition_matrix; expected list(M=...) or a data.frame.")
      }
    }
    
    # already a matrix?
    if (is.matrix(TM)) {
      M <- TM
    } else if (inherits(TM, "table") || (is.array(TM) && length(dim(TM)) == 2)) {
      M <- as.matrix(TM)
    } else if (is.data.frame(TM)) {
      cols <- names(TM)
      if (all(c("from","to","p") %in% cols)) {
        df <- TM[, c("from","to","p")]
      } else if (all(c("tm_from","tm_to","tm_p") %in% cols)) {
        df <- TM[, c("tm_from","tm_to","tm_p")]; names(df) <- c("from","to","p")
      } else {
        stop("transition_matrix data.frame must have (from,to,p) or (tm_from,tm_to,tm_p).")
      }
      df$from <- as.character(df$from); df$to <- as.character(df$to)
      lev <- c("low","high")
      # map numeric labels 1/2 to low/high if needed
      map_lab <- function(v) ifelse(v %in% c("1", 1), "low",
                                    ifelse(v %in% c("2", 2), "high", v))
      df$from <- map_lab(df$from); df$to <- map_lab(df$to)
      
      # fill grid
      grid <- expand.grid(from = lev, to = lev, stringsAsFactors = FALSE)
      df <- merge(grid, df, by = c("from","to"), all.x = TRUE)
      df$p[is.na(df$p)] <- 0
      M <- matrix(0, 2, 2, dimnames = list(from = lev, to = lev))
      for (i in seq_len(nrow(df))) M[df$from[i], df$to[i]] <- df$p[i]
    } else if (is.numeric(TM) && length(TM) == 4) {
      # assume row-major c(p11, p12, p21, p22)
      M <- matrix(as.numeric(TM), nrow = 2, byrow = TRUE,
                  dimnames = list(from = c("low","high"), to = c("low","high")))
    } else {
      stop("Unsupported transition_matrix type: provide 2x2 matrix, table/array, tidy df, or length-4 numeric.")
    }
    
    # ensure ordering & names
    rn <- rownames(M); cn <- colnames(M)
    # map 1/2 names to low/high
    map_nc <- function(v) {
      v <- as.character(v)
      v[v %in% c("1")] <- "low"
      v[v %in% c("2")] <- "high"
      v
    }
    if (!is.null(rn)) rownames(M) <- map_nc(rn)
    if (!is.null(cn)) colnames(M) <- map_nc(cn)
    
    # reorder if possible
    if (!is.null(rownames(M)) && !is.null(colnames(M))) {
      rord <- match(c("low","high"), rownames(M))
      cord <- match(c("low","high"), colnames(M))
      if (all(is.finite(rord))) M <- M[rord, , drop = FALSE]
      if (all(is.finite(cord))) M <- M[, cord, drop = FALSE]
    } else {
      # set names if absent
      dimnames(M) <- list(from = c("low","high"), to = c("low","high"))
    }
    
    # row-normalize if not already
    for (i in 1:2) {
      s <- sum(M[i, ], na.rm = TRUE)
      if (is.finite(s) && s > 0) M[i, ] <- M[i, ] / s
    }
    M
  }
  
  M <- to_matrix(transition_matrix)
  
  n_low  <- if (!is.null(model$regimes$low$Y))  nrow(model$regimes$low$Y)  else NA_integer_
  n_high <- if (!is.null(model$regimes$high$Y)) nrow(model$regimes$high$Y) else NA_integer_
  n_total <- sum(c(n_low, n_high), na.rm = TRUE)
  share_low  <- if (is.finite(n_total) && n_total > 0) n_low  / n_total else NA_real_
  share_high <- if (is.finite(n_total) && n_total > 0) n_high / n_total else NA_real_
  
  # spectral radius per regime
  rho_of <- function(A) {
    if (is.null(A)) return(NA_real_)
    k <- nrow(A); p <- ncol(A)/k
    C <- matrix(0, k*p, k*p); C[1:k,] <- A
    if (p > 1) C[(k+1):(k*p), 1:(k*(p-1))] <- diag(k*(p-1))
    max(Mod(eigen(C, only.values = TRUE)$values))
  }
  rho_low  <- rho_of(model$regimes$low$A)
  rho_high <- rho_of(model$regimes$high$A)
  
  # spells
  n_sp_low <- n_sp_high <- med_low <- med_high <- max_low <- max_high <- NA_real_
  if (!is.null(durations$spells) && nrow(durations$spells)) {
    ds <- durations$spells
    n_sp_low  <- sum(ds$regime == "low",  na.rm = TRUE)
    n_sp_high <- sum(ds$regime == "high", na.rm = TRUE)
    med_low   <- stats::median(ds$duration[ds$regime == "low"],  na.rm = TRUE)
    med_high  <- stats::median(ds$duration[ds$regime == "high"], na.rm = TRUE)
    max_low   <- max(ds$duration[ds$regime == "low"],  na.rm = TRUE)
    max_high  <- max(ds$duration[ds$regime == "high"], na.rm = TRUE)
  }
  
  # transition probs
  p11 <- p12 <- p21 <- p22 <- NA_real_
  if (!is.null(M)) {
    p11 <- M["low","low"]; p12 <- M["low","high"]
    p21 <- M["high","low"]; p22 <- M["high","high"]
  }
  
  # stationary & expected durations
  pi_low  <- pi_high <- E_low <- E_high <- NA_real_
  if (!is.null(regime_probabilities)) {
    pi_low  <- as.numeric(regime_probabilities$stationary["low"])
    pi_high <- as.numeric(regime_probabilities$stationary["high"])
    E_low   <- as.numeric(regime_probabilities$expected_duration["low"])
    E_high  <- as.numeric(regime_probabilities$expected_duration["high"])
  }
  
  # half-life & mixing via 2nd eigenvalue
  HL_low <- HL_high <- mixing_rate <- NA_real_
  ergodic <- NA
  if (!is.null(M)) {
    ev <- eigen(M)$values
    lam <- sort(Mod(ev), decreasing = TRUE)
    lam2 <- if (length(lam) >= 2) lam[2] else NA_real_
    if (is.finite(lam2) && lam2 > 0 && lam2 < 1) {
      HL_low <- HL_high <- log(0.5)/log(lam2)
      mixing_rate <- 1 - lam2
    }
    ergodic <- isTRUE(all(M > 0)) # sufficient (not necessary)
  }
  
  # empirical vs stationary shares
  emp_low <- emp_high <- delta_low <- delta_high <- NA_real_
  if (!is.null(model$metadata$regime_index)) {
    emp_low  <- mean(model$metadata$regime_index == 1L, na.rm = TRUE)
    emp_high <- 1 - emp_low
    if (is.finite(pi_low))  delta_low  <- emp_low  - pi_low
    if (is.finite(pi_high)) delta_high <- emp_high - pi_high
  }
  
  near_abs_low  <- if (is.finite(p11)) (p11 > 0.98) else NA
  near_abs_high <- if (is.finite(p22)) (p22 > 0.98) else NA
  
  tibble::tibble(
    n_low = n_low, n_high = n_high,
    share_low = share_low, share_high = share_high,
    rho_low = rho_low, rho_high = rho_high,
    n_spells_low = n_sp_low, n_spells_high = n_sp_high,
    median_duration_low = med_low, median_duration_high = med_high,
    max_duration_low = max_low, max_duration_high = max_high,
    p11 = p11, p12 = p12, p21 = p21, p22 = p22,
    pi_low = pi_low, pi_high = pi_high,
    E_low = E_low, E_high = E_high,
    HL_low = HL_low, HL_high = HL_high,
    mixing_rate = mixing_rate, ergodic = ergodic,
    emp_share_low = emp_low, emp_share_high = emp_high,
    delta_pi_low = delta_low, delta_pi_high = delta_high,
    near_absorbing_low = near_abs_low, near_absorbing_high = near_abs_high
  )
}