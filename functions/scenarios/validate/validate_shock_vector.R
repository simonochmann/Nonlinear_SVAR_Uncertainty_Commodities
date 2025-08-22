# functions/scenarios/validate/validate_shock_vector.R

#' Validate (and align) a structural shock vector against model variables
#'
#' Ensures a numeric vector of shocks is well-formed for simulation:
#' - numeric, finite values
#' - names match `variables` (optionally allow filling missing with 0, dropping extras)
#' - exactly length K and ordered as `variables` (optionally reorder)
#' - not identically zero unless `allow_all_zero=TRUE`
#'
#' Typical use: call after constructing shocks via `build_shock_vector()`.
#'
#' @param shock_vec Numeric vector of shocks. May be fully named length-K or a
#'   (named) subset to be aligned (see `fill_missing`/`drop_extra`).
#' @param variables Character vector of model variables (length K, desired order).
#' @param reorder Logical; if TRUE, reorder `shock_vec` to `variables` order. Default TRUE.
#' @param fill_missing One of c("error","zero"). If named `shock_vec` is missing
#'   some variables, "zero" fills them with 0; "error" stops. Default "error".
#' @param drop_extra One of c("error","warn","drop"). If `shock_vec` contains names
#'   not in `variables`, choose behavior. Default "error".
#' @param deduplicate One of c("error","sum","mean"). When duplicate names are found
#'   in `shock_vec`, either stop or aggregate. Default "error".
#' @param require_names Logical; if TRUE and `shock_vec` lacks names, only allowed
#'   case is length == length(variables), in which case names are assumed to be `variables`.
#'   Default TRUE.
#' @param allow_all_zero Logical; allow an all-zero vector (within `zero_tol`). Default FALSE.
#' @param zero_tol Numeric tolerance for zero checks. Default 1e-12.
#' @param finite_only Logical; require all entries to be finite. Default TRUE.
#'
#' @return A named numeric vector of length K in `variables` order. Carries
#'   attribute "meta" with fields: ok, K, variables, support_size, l1, l2, max_abs,
#'   all_zero, reordered, filled_missing, dropped_extra, deduplicated.
#' @examples
#' vars <- c("y","x","z")
#' v <- c(x = 0.5, y = -1)
#' validate_shock_vector(v, vars, fill_missing="zero", reorder=TRUE, drop_extra="error")
#' @export
validate_shock_vector <- function(
    shock_vec,
    variables,
    reorder       = TRUE,
    fill_missing  = c("error","zero"),
    drop_extra    = c("error","warn","drop"),
    deduplicate   = c("error","sum","mean"),
    require_names = TRUE,
    allow_all_zero= FALSE,
    zero_tol      = 1e-12,
    finite_only   = TRUE
) {
  fill_missing <- match.arg(fill_missing)
  drop_extra   <- match.arg(drop_extra)
  deduplicate  <- match.arg(deduplicate)
  
  # ---- guards ----
  if (is.null(variables) || !is.character(variables) || !length(variables)) {
    stop("`variables` must be a non-empty character vector (model variable order).")
  }
  K <- length(variables)
  
  if (!is.numeric(shock_vec) || !length(shock_vec)) {
    stop("`shock_vec` must be a non-empty numeric vector.")
  }
  
  # handle names requirement
  nm <- names(shock_vec)
  if (isTRUE(require_names) && (is.null(nm) || all(nm == ""))) {
    if (length(shock_vec) == K) {
      # assume order matches `variables`; assign names
      names(shock_vec) <- variables
      nm <- variables
      warning("`shock_vec` had no names; assuming it is already in `variables` order and naming accordingly.")
    } else {
      stop("`shock_vec` lacks names and length != length(variables). Provide names or a full-length vector.")
    }
  }
  
  # deduplicate names if any
  deduplicated <- FALSE
  if (!is.null(nm)) {
    dup <- nm[duplicated(nm)]
    if (length(dup)) {
      if (deduplicate == "error") {
        stop("`shock_vec` contains duplicate names: ", paste(unique(dup), collapse = ", "))
      } else {
        # aggregate by name
        split_idx <- split(seq_along(shock_vec), nm)
        shock_vec <- vapply(split_idx, function(idx) {
          if (deduplicate == "sum") sum(shock_vec[idx])
          else mean(shock_vec[idx])
        }, numeric(1))
        nm <- names(shock_vec)
        deduplicated <- TRUE
      }
    }
  }
  
  # drop/handle extras
  dropped_extra <- character(0)
  if (!is.null(nm)) {
    extra <- setdiff(nm, variables)
    if (length(extra)) {
      if (drop_extra == "error") {
        stop("`shock_vec` has names not in `variables`: ", paste(extra, collapse=", "))
      } else if (drop_extra == "warn") {
        warning("Dropping extra names not in `variables`: ", paste(extra, collapse=", "))
        shock_vec <- shock_vec[setdiff(nm, extra)]
        dropped_extra <- extra
      } else { # drop
        shock_vec <- shock_vec[setdiff(nm, extra)]
        dropped_extra <- extra
      }
      nm <- names(shock_vec)
    }
  }
  
  # fill missing variables
  filled_missing <- character(0)
  if (!is.null(nm)) {
    missing <- setdiff(variables, nm)
    if (length(missing)) {
      if (fill_missing == "error") {
        stop("`shock_vec` is missing variables: ", paste(missing, collapse=", "),
             ". Consider `fill_missing='zero'` if intended.")
      } else { # zero-fill
        add <- stats::setNames(rep(0, length(missing)), missing)
        shock_vec <- c(shock_vec, add)
        filled_missing <- missing
      }
    }
  } else {
    # unnamed vector case handled earlier (only allowed if length==K)
    if (length(shock_vec) != K) {
      stop("Unnamed `shock_vec` must have length equal to length(variables) (K).")
    }
    names(shock_vec) <- variables
  }
  
  # reorder if requested (and possible)
  reordered <- FALSE
  if (isTRUE(reorder)) {
    # ensure all variables are present now
    if (!all(variables %in% names(shock_vec))) {
      stop("Internal error: after fill/drop, not all `variables` present in `shock_vec`.")
    }
    if (!identical(names(shock_vec), variables)) {
      shock_vec <- shock_vec[variables]
      reordered <- TRUE
    }
  } else {
    # still ensure length matches K
    if (length(shock_vec) != K) {
      stop("`shock_vec` length (", length(shock_vec), ") does not match K=", K, " and `reorder=FALSE`.")
    }
  }
  
  # type/finite checks
  if (isTRUE(finite_only) && any(!is.finite(shock_vec))) {
    bad <- which(!is.finite(shock_vec))
    stop("`shock_vec` contains non-finite values at positions: ",
         paste0(names(shock_vec)[bad], collapse = ", "))
  }
  shock_vec[is.na(shock_vec)] <- NA_real_  # keep NA if finite_only=FALSE
  if (any(is.na(shock_vec))) stop("`shock_vec` contains NA values; replace or drop them.")
  
  # zero-vector check
  all_zero <- all(abs(shock_vec) <= zero_tol)
  if (all_zero && !isTRUE(allow_all_zero)) {
    stop("`shock_vec` is (near) all zeros within tolerance ", zero_tol,
         ". If intentional, set `allow_all_zero=TRUE`.")
  }
  
  # summary stats
  l1 <- sum(abs(shock_vec))
  l2 <- sqrt(sum(shock_vec^2))
  max_abs <- max(abs(shock_vec))
  support_size <- sum(abs(shock_vec) > zero_tol)
  
  attr(shock_vec, "meta") <- list(
    ok              = TRUE,
    K               = K,
    variables       = variables,
    support_size    = support_size,
    l1              = l1,
    l2              = l2,
    max_abs         = max_abs,
    all_zero        = all_zero,
    reordered       = reordered,
    filled_missing  = filled_missing,
    dropped_extra   = dropped_extra,
    deduplicated    = deduplicated,
    zero_tol        = zero_tol,
    finite_only     = finite_only,
    created_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  
  return(shock_vec)
}