# functions/scenarios/shocks/build_shock_vector.R

#' Build a structural shock vector aligned to model variable order
#'
#' Flexible constructor for K-length shock vectors (one entry per model variable).
#' Supports:
#' - Single-variable shocks: `impulse = "uncertainty", size = 1.5`
#' - Baskets with (optional) weights: `impulse = c("uncertainty","oil_vol"), size=1, weights=c(0.7,0.3)`
#' - Named custom vectors (pre-scaled): `impulse = c(uncertainty=0.8, oil_vol=0.2)` (ignore `size`)
#' - Aliases mapping (e.g., "vix" -> "uncertainty")
#' - Case-insensitive matching, strict/lenient behavior, and normalization: none|l1|l2|sum1
#'
#' @param variables Character vector of model variables, in the exact order of the VAR/TVAR.
#' @param impulse Either:
#'   - character scalar: the variable to shock,
#'   - character vector: basket of variables,
#'   - named numeric vector: custom per-variable magnitudes (pre-scaled; `size` ignored).
#' @param size Numeric scalar magnitude for single/basket modes (ignored if `impulse` is a named numeric vector).
#'   Interpretation depends on `normalize`:
#'     * "none": weights are used as-is; each selected variable gets `size * weight`.
#'     * "sum1": abs(weights) are scaled to sum to 1 ⇒ total L1 equals `abs(size)`.
#'     * "l1":   weights scaled so sum(abs(weights)) = 1, identical to "sum1" (alias).
#'     * "l2":   weights scaled to unit L2 norm ⇒ ||vector||_2 = |size|.
#' @param weights Optional numeric vector of weights for basket mode; must match length of `impulse` (character).
#'                If NULL in basket mode, equal weights are used.
#' @param aliases Optional named character vector mapping aliases -> canonical variable names,
#'                e.g., c(vix = "uncertainty", oil = "oil_ret").
#' @param match Character, one of c("exact","ci") for exact vs case-insensitive matching. Default "exact".
#' @param normalize Character, one of c("none","sum1","l1","l2"). Default "sum1".
#' @param allow_partial Logical. If TRUE, unknown names are dropped with a warning; if FALSE, error on first unknown. Default FALSE.
#' @param strict Logical. If TRUE, disallow duplicates in `impulse` after alias resolution (error). Default TRUE.
#'
#' @return Named numeric vector of length K (length(variables)), aligned to `variables`.
#'         Attributes `attr(v, "meta")` provide: list(
#'           kind = "single"|"basket"|"custom",
#'           matched = <character>,
#'           unmatched = <character>,
#'           normalize = <character>,
#'           size_input = <numeric|NULL>,
#'           weights_input = <numeric|NULL>
#'         )
#' @examples
#' build_shock_vector(c("uncertainty","oil_vol","oil_ret"), "uncertainty", size = 1.5)
#' build_shock_vector(c("uncertainty","oil_vol","oil_ret"),
#'                    impulse = c("uncertainty","oil_vol"), size = 1, weights = c(0.7,0.3))
#' build_shock_vector(c("uncertainty","oil_vol","oil_ret"),
#'                    impulse = c(vix = 0.8, oil_vol = 0.2), aliases = c(vix = "uncertainty"))
#' @export
build_shock_vector <- function(
    variables,
    impulse,
    size      = NULL,
    weights   = NULL,
    aliases   = NULL,
    match     = c("exact","ci"),
    normalize = c("sum1","none","l1","l2"),
    allow_partial = FALSE,
    strict        = TRUE
) {
  # ---- setup & guards --------------------------------------------------------
  match     <- match.arg(match)
  normalize <- match.arg(normalize)
  if (normalize == "l1") normalize <- "sum1"
  
  if (is.null(variables) || !is.character(variables) || length(variables) == 0L) {
    stop("`variables` must be a non-empty character vector in model order.")
  }
  
  # helper: case-insensitive resolver
  resolve_names <- function(x, pool, mode = "exact") {
    if (mode == "exact") {
      m <- match(x, pool)
      return(list(idx = m, found = !is.na(m), map = setNames(pool[m], x)))
    } else {
      # case-insensitive
      pool_l <- tolower(pool)
      x_l    <- tolower(x)
      m <- match(x_l, pool_l)
      return(list(idx = m, found = !is.na(m), map = setNames(pool[m], x)))
    }
  }
  
  # alias resolution
  if (!is.null(aliases)) {
    if (!is.character(aliases) || is.null(names(aliases)))
      stop("`aliases` must be a named character vector: alias -> canonical_name")
    # apply aliases for character impulses or names(impulse) if named numeric
    if (is.character(impulse)) {
      impulse <- unname(ifelse(!is.na(aliases[impulse]), aliases[impulse], impulse))
    } else if (is.numeric(impulse) && !is.null(names(impulse))) {
      nm <- names(impulse)
      nm2 <- ifelse(!is.na(aliases[nm]), aliases[nm], nm)
      names(impulse) <- nm2
    }
  }
  
  # classify mode
  mode <- if (is.numeric(impulse) && !is.null(names(impulse))) "custom"
  else if (is.character(impulse) && length(impulse) == 1L) "single"
  else if (is.character(impulse) && length(impulse) >= 1L) "basket"
  else stop("`impulse` must be: character (1 or more) or a named numeric vector.")
  
  # initialize zero vector
  v <- setNames(numeric(length(variables)), variables)
  
  # ---- CUSTOM MODE -----------------------------------------------------------
  if (mode == "custom") {
    # validate names against variables
    nm <- names(impulse)
    res <- resolve_names(nm, variables, match)
    unknown <- nm[!res$found]
    if (length(unknown)) {
      if (allow_partial) {
        warning("Dropping unknown impulse names (custom): ", paste(unknown, collapse = ", "))
        keep <- res$found
        impulse <- impulse[keep]
        nm <- nm[keep]
        res$idx <- res$idx[keep]
      } else {
        stop("Unknown impulse names in custom vector: ", paste(unknown, collapse = ", "))
      }
    }
    # assign directly (pre-scaled)
    if (length(impulse)) v[res$map[nm]] <- as.numeric(impulse)
    attr(v, "meta") <- list(
      kind = "custom",
      matched = unname(res$map[nm]),
      unmatched = unknown,
      normalize = "none",
      size_input = NULL,
      weights_input = NULL
    )
    return(v)
  }
  
  # ---- SINGLE / BASKET MODE --------------------------------------------------
  # sanity on size
  if (is.null(size) || !is.numeric(size) || length(size) != 1L || !is.finite(size)) {
    stop("`size` must be a finite numeric scalar for single/basket modes.")
  }
  size <- as.numeric(size)
  
  # resolve impulse names to variables
  res <- resolve_names(impulse, variables, match)
  unknown <- impulse[!res$found]
  if (length(unknown)) {
    if (allow_partial) {
      warning("Dropping unknown impulse names: ", paste(unknown, collapse = ", "))
      impulse <- impulse[res$found]
      res$idx <- res$idx[res$found]
    } else {
      stop("Unknown impulse names: ", paste(unknown, collapse = ", "))
    }
  }
  if (strict && anyDuplicated(res$map[impulse])) {
    dup <- unique(impulse[duplicated(res$map[impulse])])
    stop("Duplicate impulse names after alias/case resolution: ", paste(dup, collapse = ", "))
  }
  
  # weights
  if (mode == "single") {
    w <- 1
  } else {
    if (is.null(weights)) {
      w <- rep(1, length(impulse))
    } else {
      if (!is.numeric(weights) || length(weights) != length(impulse))
        stop("`weights` must be numeric and have the same length as `impulse` (basket mode).")
      w <- as.numeric(weights)
    }
    if (all(w == 0)) stop("All basket weights are zero.")
  }
  
  # normalize weights as requested
  if (normalize == "none") {
    w_norm <- w
    scale_factor <- 1
  } else if (normalize == "sum1") {
    s <- sum(abs(w))
    if (s == 0) stop("Cannot normalize by L1: sum(abs(weights)) == 0.")
    w_norm <- w / s
    scale_factor <- size
  } else if (normalize == "l2") {
    s <- sqrt(sum(w^2))
    if (s == 0) stop("Cannot normalize by L2: sqrt(sum(w^2)) == 0.")
    w_norm <- w / s
    scale_factor <- size
  } else {
    stop("Unknown `normalize` mode.")
  }
  
  # compose vector
  if (mode == "single") {
    v[res$map[impulse]] <- size
  } else {
    # Apply scale_factor to normalized weights
    contrib <- as.numeric(w_norm) * scale_factor
    names(contrib) <- res$map[impulse]
    # if normalize == "none", contrib = weights * size (raw scaling)
    if (normalize == "none") contrib <- as.numeric(w) * size
    # assign
    v[names(contrib)] <- contrib
  }
  
  attr(v, "meta") <- list(
    kind = mode,
    matched = unname(res$map[impulse]),
    unmatched = unknown,
    normalize = normalize,
    size_input = size,
    weights_input = if (mode == "single") NULL else w
  )
  v
}