# functions/scenarios/simulate/generate_counterfactual_baseline.R

#' Generate a counterfactual "no-shock" baseline path
#'
#' Modes:
#' - method = "irf_zero" (default): deterministic zero baseline (IRF superposition style).
#' - method = "stochastic": build baseline by convolving user-supplied structural innovations
#'   with unit-shock IRFs (so baseline uses the *same innovations* as your scenario, but with
#'   *no scheduled impulses*). This makes Δ paths strictly comparable.
#' - method = "conditional_mean": if `model$mu` exists (named numeric by variable), use it as a
#'   flat baseline across the horizon; otherwise falls back to zeros.
#'
#' Assumptions for "stochastic":
#' - `innovations` is an H×K matrix of *structural* shocks u_t (Var(u_t)=I), columns in `variables` order.
#' - `model$irf[[regime_or_combined]][[impulse]][[response]]` returns a numeric vector (length >= H).
#'
#' @param model    TVAR/VAR model object carrying `variables` and `irf`.
#' @param horizon  Integer forecast horizon H (>=1).
#' @param variables Optional character vector (order) to use; defaults to `model$variables`.
#' @param method   "irf_zero" | "stochastic" | "conditional_mean". Default "irf_zero".
#' @param innovations Optional numeric matrix (H×K) of *structural* shocks for method "stochastic".
#' @param regime   Which IRF set to use. Defaults to "combined", then falls back to "low".
#' @param responses Optional subset of response variables to return (default: all).
#' @param attach_meta Logical; attach rich metadata attributes. Default TRUE.
#'
#' @return tibble with columns: t, variable, value, is_baseline=TRUE
#'         (and attributes "meta" when attach_meta=TRUE).
#' @export
generate_counterfactual_baseline <- function(
    model,
    horizon,
    variables    = NULL,
    method       = c("irf_zero","stochastic","conditional_mean"),
    innovations  = NULL,
    regime       = c("combined","low","high"),
    responses    = NULL,
    attach_meta  = TRUE
) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # validate basics 
  method <- match.arg(method)
  regime <- as.character(regime)[1]
  if (!is.numeric(horizon) || length(horizon)!=1L || !is.finite(horizon) || horizon < 1)
    stop("`horizon` must be a finite numeric scalar >= 1.")
  H <- as.integer(round(horizon))
  
  # variables
  variables <- variables %||% model$variables
  if (is.null(variables) || !is.character(variables) || !length(variables))
    stop("`variables` must be provided or present as model$variables.")
  K <- length(variables)
  
  # optional response subset
  if (!is.null(responses)) {
    if (!all(responses %in% variables))
      stop("Unknown `responses`: ", paste(setdiff(responses, variables), collapse=", "))
  } else {
    responses <- variables
  }
  
  # helper: build tidy tibble
  .to_tidy <- function(M) {
    # M is H × K matrix with colnames = variables
    tibble::tibble(
      t        = rep(seq_len(H), each = length(responses)),
      variable = rep(responses, times = H),
      value    = c(M[, responses, drop = FALSE]),
      is_baseline = TRUE
    )
  }
  
  meta <- list(
    method   = method,
    regime   = regime,
    variables= variables,
    responses= responses,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  
  # mode 1: deterministic zero (IRF style)
  if (method == "irf_zero") {
    M <- matrix(0, nrow = H, ncol = K, dimnames = list(NULL, variables))
    out <- .to_tidy(M)
    if (attach_meta) attr(out, "meta") <- meta
    return(out)
  }
  
  # mode 2: conditional mean (flat) 
  if (method == "conditional_mean") {
    mu <- model$mu %||% rep(0, K)
    if (is.null(names(mu))) names(mu) <- variables
    mu <- mu[variables]
    M <- matrix(rep(mu, each = H), nrow = H, ncol = K, byrow = FALSE,
                dimnames = list(NULL, variables))
    out <- .to_tidy(M)
    meta$mu_used <- mu
    if (attach_meta) attr(out, "meta") <- meta
    return(out)
  }
  
  # ---- mode 3: stochastic baseline via IRF convolution ----
  # y_t = sum_{tau=1..t} Phi(t - tau + 1) * u_tau, where Phi(k) is the unit-shock MA coefficient
  if (method == "stochastic") {
    if (is.null(innovations)) stop("`innovations` (H×K structural shocks) required for method='stochastic'.")
    if (!is.matrix(innovations) || nrow(innovations) < H || ncol(innovations) != K)
      stop("`innovations` must be an H×K numeric matrix with columns in `variables` order.")
    if (is.null(colnames(innovations))) colnames(innovations) <- variables
    # pick IRF source: prefer combined, then regime specific fallback
    irf_src <- model$irf[[regime]] %||% model$irf$combined %||% model$irf$low
    if (is.null(irf_src)) stop("IRF structure not found in model (tried: regime, combined, low).")
    
    # Validate IRF structure presence for each impulse/response
    .get_irf <- function(impulse, response, k) {
      # Expect list of vectors: irf_src[[impulse]][[response]][k]
      vec <- try(irf_src[[impulse]][[response]], silent = TRUE)
      if (inherits(vec, "try-error") || is.null(vec))
        stop("IRF missing for impulse='", impulse, "' -> response='", response, "'.")
      if (length(vec) < k) {
        # pad with last value or with zeros; choose zeros for safety
        c(vec, rep(0, k - length(vec)))
      } else {
        vec
      }
    }
    
    # Convolve: for each t, response j = sum_{i in variables} sum_{tau<=t} IRF_{i→j}[t - tau + 1] * u_{tau,i}
    Y <- matrix(0, nrow = H, ncol = K, dimnames = list(NULL, variables))
    for (t in 1:H) {
      for (i in seq_len(K)) {                # impulse variable index
        imp <- variables[i]
        u_path <- innovations[1:t, i, drop = TRUE]  # length t
        # collect IRF slices for all responses at lags 1..t
        for (j in seq_len(K)) {              # response variable index
          resp <- variables[j]
          irf_ij <- .get_irf(imp, resp, k = t)
          # contribution at each lag ℓ = 1..t multiplies u_{t-ℓ+1, i}
          # equivalently: sum_{τ=1..t} irf_ij[ t - τ + 1 ] * u_{τ, i}
          Y[t, j] <- Y[t, j] + sum(irf_ij[seq_len(t)] * rev(u_path))
        }
      }
    }
    
    out <- .to_tidy(Y)
    meta$innovations_summary <- list(
      dim = dim(innovations),
      colnames = colnames(innovations),
      sd = stats::setNames(apply(innovations, 2, stats::sd), colnames(innovations))
    )
    if (attach_meta) attr(out, "meta") <- meta
    return(out)
  }
  
  stop("Unknown `method` value: ", method)
}