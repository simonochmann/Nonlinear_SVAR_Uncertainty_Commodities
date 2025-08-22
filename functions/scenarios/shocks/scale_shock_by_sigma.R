# functions/scenarios/shocks/scale_shock_by_sigma.R

#' Scale a scenario shock by a volatility measure (σ)
#'
#' Flexible scaler for converting a user-specified shock magnitude into
#' "absolute units" using a per-variable volatility proxy. Designed for
#' TVAR scenarios where \code{sigma} may vary by regime or be robustified.
#'
#' Modes:
#' - scale = "abs": return `size` unchanged (already in absolute units).
#' - scale = "sd":  multiply by standard deviation sigma[var].
#' - scale = "mad": multiply by robust sigma = 1.4826 * MAD.
#' - scale = "iqr": multiply by robust sigma = IQR / 1.349 (≈ sd under normality).
#'
#' The `sigma` argument can be:
#' - a **named numeric vector**: sigma[var] is used;
#' - a **list of named numeric vectors** with elements "low","high","combined":
#'   pass `regime` to select; will fall back to "combined" -> "low" -> "high".
#'
#' Safety features:
#' - `min_sigma` to prevent zero/near-zero scaling;
#' - optional winsorization of the retrieved sigma value;
#' - optional caps on the final scaled shock magnitude.
#'
#' @param size Numeric scalar. The shock magnitude in units of `scale`.
#' @param scale Character. One of c("abs","sd","mad","iqr"). Default "sd".
#' @param sigma Named numeric vector **or** list(low=,high=,combined=) of such vectors.
#'              Names must include the variable(s) used in scenarios.
#' @param var Character scalar. The canonical model variable to be scaled.
#' @param regime Optional character in c("low","high","combined"). Used only if `sigma` is a list.
#' @param aliases Optional named character vector mapping aliases -> canonical names (e.g. c(vix="uncertainty")).
#' @param match Character, one of c("exact","ci") for name matching. Default "exact".
#' @param min_sigma Numeric scalar lower bound for sigma before multiplication. Default 1e-8.
#' @param winsor Numeric in [0,0.5). If >0, winsorize the fetched sigma value at
#'               the given two-sided tail probability by requiring `sigma_in`
#'               to be within [quantile_lower, quantile_upper] **if** `sigma` was
#'               supplied as a length>1 vector (ignored for single value lookups).
#'               Default 0 (no winsorization).
#' @param cap_abs Optional numeric scalar. If provided, cap |scaled| at this value.
#' @param cap_sd Optional numeric scalar. If provided and scale != "abs",
#'               cap |size| (in σ units) at this value before scaling.
#'
#' @return Numeric scalar: the shock in absolute units. Carries attribute
#'         "meta" with fields: method, var, regime, sigma_used, size_input,
#'         size_sigma_units, scaled, capped, winsor, source ("vector"|"list").
#'
#' @examples
#' sigma <- c(uncertainty = 2.0, oil_vol = 0.5)
#' scale_shock_by_sigma(size = 1.5, scale = "sd", sigma = sigma, var = "uncertainty")
#'
#' # regime list
#' sig_list <- list(
#'   low = c(uncertainty=1.8, oil_vol=0.4),
#'   high= c(uncertainty=2.6, oil_vol=0.7),
#'   combined = c(uncertainty=2.2, oil_vol=0.55)
#' )
#' scale_shock_by_sigma(2, "sd", sig_list, "oil_vol", regime="high")
#' @export
scale_shock_by_sigma <- function(
    size,
    scale = c("sd","abs","mad","iqr"),
    sigma,
    var,
    regime  = NULL,
    aliases = NULL,
    match   = c("exact","ci"),
    min_sigma = 1e-8,
    winsor   = 0,
    cap_abs  = NULL,
    cap_sd   = NULL
) {
  # ----------------- guards -----------------
  scale <- match.arg(scale)
  match <- match.arg(match)
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size)) {
    stop("`size` must be a finite numeric scalar.")
  }
  if (is.null(var) || !is.character(var) || length(var) != 1L || !nzchar(var)) {
    stop("`var` must be a non-empty character scalar (canonical variable name).")
  }
  if (is.null(sigma)) stop("`sigma` must be provided (named numeric vector or regime list).")
  if (!is.null(winsor) && (winsor < 0 || winsor >= 0.5)) stop("`winsor` must be in [0, 0.5).")
  if (!is.null(cap_sd) && (scale == "abs")) warning("`cap_sd` ignored when scale='abs'.")
  # alias resolution
  if (!is.null(aliases)) {
    if (!is.character(aliases) || is.null(names(aliases))) {
      stop("`aliases` must be a named character vector: alias -> canonical.")
    }
    if (!is.null(aliases[[var]])) var <- aliases[[var]]
  }
  # helper to resolve name
  resolve_name <- function(x, pool) {
    if (identical(match, "exact")) {
      idx <- match(x, names(pool)); return(idx)
    } else {
      idx <- match(tolower(x), tolower(names(pool))); return(idx)
    }
  }
  
  # ----------------- choose sigma source -----------------
  sigma_vec <- NULL
  sigma_source <- NULL
  used_regime <- NULL
  
  if (is.list(sigma)) {
    # regime-aware selection: prefer requested, else combined, else low, else high (first available)
    reg_order <- c(regime, "combined", "low", "high")
    reg_order <- unique(reg_order[!is.na(reg_order)])
    avail <- intersect(reg_order, names(sigma))
    if (!length(avail)) {
      stop("`sigma` list has no matching regimes. Available: ",
           paste(names(sigma), collapse=", "), "; requested: ", paste(reg_order, collapse=", "))
    }
    used_regime <- avail[1]
    sigma_vec <- sigma[[used_regime]]
    sigma_source <- "list"
  } else if (is.numeric(sigma)) {
    sigma_vec <- sigma
    sigma_source <- "vector"
  } else {
    stop("`sigma` must be a named numeric vector or a list of such vectors (by regime).")
  }
  
  if (is.null(names(sigma_vec))) stop("`sigma` vector must be named by variables.")
  
  idx <- resolve_name(var, sigma_vec)
  if (is.na(idx)) {
    stop("Variable '", var, "' not found in names(sigma). Available: ",
         paste(names(sigma_vec), collapse=", "))
  }
  
  sigma_val <- as.numeric(sigma_vec[[idx]])
  if (!is.finite(sigma_val)) stop("sigma[", var, "] is not finite.")
  
  # winsorize sigma value if requested and vector has breadth
  if (winsor > 0 && length(sigma_vec) >= 10L) {
    lo <- stats::quantile(sigma_vec, probs = winsor, names = FALSE, na.rm = TRUE)
    hi <- stats::quantile(sigma_vec, probs = 1 - winsor, names = FALSE, na.rm = TRUE)
    sigma_val <- min(max(sigma_val, lo), hi)
  }
  
  # enforce minimum sigma
  sigma_val <- max(sigma_val, min_sigma)
  
  # ----------------- scale -----------------
  # optionally cap the *sigma-units* size before scaling
  size_sigma_units <- size
  if (!is.null(cap_sd) && is.finite(cap_sd)) {
    size_sigma_units <- sign(size) * min(abs(size), abs(cap_sd))
  }
  
  if (scale == "abs") {
    scaled <- size
  } else if (scale %in% c("sd","mad","iqr")) {
    scaled <- size_sigma_units * sigma_val
  } else {
    stop("Unknown scale: ", scale)
  }
  
  # final absolute cap (post-scaling)
  capped <- FALSE
  if (!is.null(cap_abs) && is.finite(cap_abs)) {
    if (abs(scaled) > abs(cap_abs)) {
      scaled <- sign(scaled) * abs(cap_abs)
      capped <- TRUE
    }
  }
  
  # ----------------- attach metadata -----------------
  attr(scaled, "meta") <- list(
    method            = scale,
    var               = var,
    regime            = used_regime %||% NA_character_,
    sigma_used        = sigma_val,
    size_input        = size,
    size_sigma_units  = if (scale == "abs") NA_real_ else size_sigma_units,
    scaled            = scaled,
    capped            = capped,
    winsor            = winsor,
    min_sigma         = min_sigma,
    cap_abs           = cap_abs,
    cap_sd            = cap_sd,
    source            = sigma_source
  )
  
  return(scaled)
}