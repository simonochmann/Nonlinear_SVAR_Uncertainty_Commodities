# functions/scenarios/shocks/build_contemporaneous_A.R

#' Build contemporaneous impact matrix A for TVAR scenarios
#'
#' Constructs A such that epsilon_t = A * u_t with Var(u_t) = I, hence
#' Sigma = Var(epsilon_t) = A %*% t(A).
#'
#' Identification options:
#' - "unit":     A = I_K (unit shocks; useful for pure IRF superposition).
#' - "cholesky": A = chol(Sigma) (upper-triangular; pivoted fallback), optionally with
#'               shrinkage and near-PD repair.
#' - "sign":     Start from Cholesky then flip shock column signs to satisfy
#'               impact sign constraints at horizon 0 (simple, transparent).
#' - "custom":   Use user-supplied A (from `A_custom` arg or `model$A[[regime]]`).
#'
#' Sigma source (in order of preference):
#'  1) `Sigma_arg` (if provided),
#'  2) `model$regimes[[regime]]$residual_cov`,
#'  3) `model$Sigma[[regime]]`,
#'  4) `model$Sigma$combined` or first available.
#'
#' Robustness:
#' - Optional diagonal shrinkage: S* = (1-λ)S + λ*diag(diag(S)).
#' - If S is not PD, attempt Matrix::nearPD, else jitter the diagonal.
#' - Optional pivoted Cholesky.
#'
#' @param model   TVAR model list carrying residual covariances and/or custom A.
#' @param identification One of c("unit","cholesky","sign","custom").
#' @param variables Character vector of model variables in the desired order (length K).
#' @param regime  Character in c("combined","low","high") or any key present in model.
#' @param ordering Optional character vector to re-order variables before factorization.
#'                 By default, uses `variables`.
#' @param Sigma_arg Optional K×K covariance matrix to override model-provided Sigma.
#' @param A_custom Optional K×K matrix used when `identification="custom"`.
#' @param sign_constraints Optional data.frame for `identification="sign"` with columns:
#'        response (chr), shock (chr), sign (chr; one of "+","-","0").
#'        Interpreted at horizon 0 (impact). Only column sign flips are applied.
#' @param shrink_lambda Numeric in [0,1). Diagonal shrinkage intensity. Default 0.
#' @param min_eig       Numeric >= 0. Minimum eigenvalue enforced after repair. Default 1e-8.
#' @param pivoted_chol  Logical. Allow pivoted Cholesky fallback. Default TRUE.
#' @param verbose       Logical. Emit informative messages. Default FALSE.
#'
#' @return A K×K numeric matrix with row/colnames = `variables`.
#'         Attributes:
#'           - method, regime, ordering_used, shrink_lambda, min_eig,
#'             sigma_source, pd_fixed (logical), chol_pivoted (logical),
#'             constraints_applied (logical), constraints_satisfied (logical).
#' @export
build_contemporaneous_A <- function(
    model,
    identification = c("unit","cholesky","sign","custom"),
    variables,
    regime = c("combined","low","high"),
    ordering = NULL,
    Sigma_arg = NULL,
    A_custom  = NULL,
    sign_constraints = NULL,
    shrink_lambda = 0,
    min_eig = 1e-8,
    pivoted_chol = TRUE,
    verbose = FALSE
) {
  identification <- match.arg(identification)
  regime <- as.character(regime)[1]
  if (missing(variables) || !is.character(variables) || length(variables) == 0L) {
    stop("`variables` must be a non-empty character vector.")
  }
  K <- length(variables)
  
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # (0) unit/custom fast paths
  if (identification == "unit") {
    A <- diag(K)
    dimnames(A) <- list(variables, variables)
    attr(A,"meta") <- .A_meta("unit", regime, variables, variables,
                              shrink_lambda, min_eig, sigma_source = "none",
                              pd_fixed = FALSE, chol_pivoted = FALSE,
                              constraints_applied = FALSE, constraints_satisfied = NA)
    return(A)
  }
  if (identification == "custom") {
    A <- A_custom %||% (model$A[[regime]] %||% model$A$combined)
    if (is.null(A)) stop("Custom identification selected but no `A_custom` or `model$A[[regime]]` found.")
    .validate_A(A, K)
    # Reorder/rename rows/cols to match `variables`
    A <- .align_matrix(A, variables)
    attr(A,"meta") <- .A_meta("custom", regime, variables, colnames(A),
                              shrink_lambda, min_eig, sigma_source = "user",
                              pd_fixed = FALSE, chol_pivoted = FALSE,
                              constraints_applied = FALSE, constraints_satisfied = NA)
    return(A)
  }
  
  # (1) choose Sigma 
  S <- NULL; sigma_source <- NULL
  if (!is.null(Sigma_arg)) {
    S <- Sigma_arg; sigma_source <- "arg"
  } else if (!is.null(model$regimes[[regime]]$residual_cov)) {
    S <- model$regimes[[regime]]$residual_cov; sigma_source <- paste0("model$regimes$", regime)
  } else if (!is.null(model$Sigma[[regime]])) {
    S <- model$Sigma[[regime]]; sigma_source <- paste0("model$Sigma$", regime)
  } else if (!is.null(model$Sigma$combined)) {
    S <- model$Sigma$combined; sigma_source <- "model$Sigma$combined"
  } else if (length(model$Sigma)) {
    # first available
    key <- names(model$Sigma)[1]
    S <- model$Sigma[[key]]; sigma_source <- paste0("model$Sigma$", key)
  }
  
  if (is.null(S)) stop("Could not locate residual covariance `Sigma` in model; provide `Sigma_arg`.")
  
  .validate_S(S)
  # align S to `ordering` then to `variables`
  ordering_used <- ordering %||% variables
  if (!setequal(colnames(S), ordering_used)) {
    # try to align S by its dimnames
    if (is.null(colnames(S)) || is.null(rownames(S))) {
      stop("Sigma lacks dimnames; unable to align to requested ordering.")
    }
  }
  # reorder S to ordering_used
  S_ord <- .reorder_S(S, ordering_used)
  
  # (2) shrinkage / PD repair
  pd_fixed <- FALSE
  if (shrink_lambda > 0) {
    if (shrink_lambda >= 1 || shrink_lambda < 0) stop("`shrink_lambda` must be in [0,1).")
    D <- diag(diag(S_ord))
    S_ord <- (1 - shrink_lambda) * S_ord + shrink_lambda * D
  }
  
  # ensure PD
  eigvals <- tryCatch(eigen(S_ord, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NA)
  needs_pd_fix <- any(!is.finite(eigvals)) || min(eigvals, na.rm = TRUE) < min_eig
  if (needs_pd_fix) {
    if (requireNamespace("Matrix", quietly = TRUE)) {
      pd <- suppressWarnings(Matrix::nearPD(S_ord, corr = FALSE))
      S_ord <- as.matrix(pd$mat)
      pd_fixed <- TRUE
    } else {
      # jitter diagonal
      jitter <- (abs(min(eigvals, na.rm = TRUE)) + min_eig) %||% min_eig
      S_ord <- S_ord + diag(jitter, nrow(S_ord))
      pd_fixed <- TRUE
    }
  }
  
  # (3) factorization -> A0
  chol_pivoted <- FALSE
  A0 <- NULL
  # We use upper-triangular chol so that S = R'R; set A = R.
  # (Many VAR codes use lower-tri; either convention works if consistent.)
  ch_try <- try(chol(S_ord), silent = TRUE)
  if (inherits(ch_try, "try-error") && isTRUE(pivoted_chol)) {
    # pivoted chol fallback
    ch_piv <- try(chol(S_ord, pivot = TRUE), silent = TRUE)
    if (inherits(ch_piv, "try-error")) {
      stop("Cholesky factorization failed, even with pivoting. Check Sigma.")
    }
    piv <- attr(ch_piv, "pivot")
    chol_pivoted <- TRUE
    # unpivot: R_piv %*% P' with permutation P; but easier is to permute S before chol.
    # We'll instead redo with explicit permutation to keep labels:
    P <- diag(1, nrow = nrow(S_ord))[piv, , drop = FALSE]
    S_perm <- t(P) %*% S_ord %*% P
    R <- chol(S_perm)
    # Map back to original order:
    A0 <- R %*% t(P)
  } else if (!inherits(ch_try, "try-error")) {
    A0 <- ch_try
  } else {
    stop("Cholesky factorization failed. Consider increasing `shrink_lambda` or checking Sigma.")
  }
  
  # align A0's dimnames to ordering_used
  dimnames(A0) <- list(ordering_used, ordering_used)
  
  # map A0 to final variable order if ordering != variables
  if (!identical(ordering_used, variables)) {
    A0 <- .permute_A(A0, from = ordering_used, to = variables)
  }
  
  # (4) apply sign constraints if requested
  constraints_applied <- FALSE
  constraints_satisfied <- NA
  A <- A0
  
  if (identification == "sign") {
    constraints_applied <- TRUE
    if (is.null(sign_constraints) || !is.data.frame(sign_constraints)) {
      stop("`sign_constraints` data.frame required for identification='sign'.")
    }
    req_cols <- c("response","shock","sign")
    if (!all(req_cols %in% names(sign_constraints))) {
      stop("`sign_constraints` must have columns: response, shock, sign.")
    }
    # flip columns of A (shock columns) to satisfy sign at impact: sign(response,shock) * A[response, shock] >= 0
    sc <- sign_constraints
    sc$response <- as.character(sc$response)
    sc$shock    <- as.character(sc$shock)
    sc$sign     <- as.character(sc$sign)
    
    # validate names exist
    if (!all(sc$response %in% variables)) {
      missing <- setdiff(sc$response, variables)
      stop("Unknown response variables in sign_constraints: ", paste(missing, collapse = ", "))
    }
    if (!all(sc$shock %in% variables)) {
      missing <- setdiff(sc$shock, variables)
      stop("Unknown shock variables in sign_constraints: ", paste(missing, collapse = ", "))
    }
    # Reduce to a simple column flip problem:
    # For each shock j, if majority of required signs for A[resp, j] disagree, flip column j.
    unique_shocks <- unique(sc$shock)
    for (sh in unique_shocks) {
      rows <- sc$response[sc$shock == sh]
      signs <- sc$sign[sc$shock == sh]
      sgn_num <- ifelse(signs == "+",  1,
                        ifelse(signs == "-", -1,
                               ifelse(signs == "0",  0, NA_real_)))
      if (any(is.na(sgn_num))) stop("sign must be one of '+','-','0'.")
      col_idx <- match(sh, variables)
      vals <- A[rows, col_idx, drop = TRUE]
      score_noflip <- sum(sign(vals) == sgn_num | (sgn_num == 0 & abs(vals) < 1e-12))
      score_flip   <- sum(sign(-vals) == sgn_num | (sgn_num == 0 & abs(-vals) < 1e-12))
      if (score_flip > score_noflip) {
        A[, col_idx] <- -A[, col_idx]
      }
    }
    # final check
    ok_list <- mapply(function(r, c, s) {
      want <- if (s == "+")  1 else if (s == "-") -1 else 0
      val <- A[r, c]
      if (want == 0) return(abs(val) < 1e-12)
      return(sign(val) == want)
    }, r = sc$response, c = sc$shock, s = sc$sign)
    constraints_satisfied <- all(ok_list)
    if (verbose && !constraints_satisfied) {
      message("Sign constraints not fully satisfied by column flips; consider advanced rotation methods.")
    }
  }
  
  # finalize metadata/labels
  dimnames(A) <- list(variables, variables)
  attr(A,"meta") <- .A_meta(
    method = identification,
    regime = regime,
    variables = variables,
    ordering_used = ordering_used,
    shrink_lambda = shrink_lambda,
    min_eig = min_eig,
    sigma_source = sigma_source,
    pd_fixed = pd_fixed,
    chol_pivoted = chol_pivoted,
    constraints_applied = constraints_applied,
    constraints_satisfied = constraints_satisfied
  )
  A
}

.A_meta <- function(method, regime, variables, ordering_used,
                    shrink_lambda, min_eig, sigma_source,
                    pd_fixed, chol_pivoted,
                    constraints_applied, constraints_satisfied) {
  list(
    method = method,
    regime = regime,
    ordering_used = ordering_used,
    variables = variables,
    shrink_lambda = shrink_lambda,
    min_eig = min_eig,
    sigma_source = sigma_source,
    pd_fixed = pd_fixed,
    chol_pivoted = chol_pivoted,
    constraints_applied = constraints_applied,
    constraints_satisfied = constraints_satisfied
  )
}

.validate_S <- function(S) {
  if (!is.matrix(S) || nrow(S) != ncol(S))
    stop("Sigma must be a square numeric matrix.")
  if (!is.numeric(S)) stop("Sigma must be numeric.")
  if (any(!is.finite(S))) stop("Sigma contains non-finite entries.")
  if (is.null(colnames(S)) || is.null(rownames(S)))
    warning("Sigma has no dimnames; alignment will rely on provided `ordering`.")
  invisible(TRUE)
}

.validate_A <- function(A, K) {
  if (!is.matrix(A) || nrow(A) != K || ncol(A) != K)
    stop("A must be a KxK numeric matrix.")
  if (!is.numeric(A) || any(!is.finite(A)))
    stop("A must be numeric with all finite entries.")
  invisible(TRUE)
}

.reorder_S <- function(S, order) {
  if (!is.null(colnames(S)) && !is.null(rownames(S))) {
    missing <- setdiff(order, colnames(S))
    if (length(missing))
      stop("Sigma is missing variables: ", paste(missing, collapse = ", "))
    S[order, order, drop = FALSE]
  } else {
    S
  }
}

.permute_A <- function(A, from, to) {
  if (!setequal(from, to)) {
    P <- diag(1, length(from))
    dimnames(P) <- list(from, from)
    P <- P[to, from, drop = FALSE]  # rows=to, cols=from
    A <- P %*% A %*% t(P)
  }
  dimnames(A) <- list(to, to)
  A
}