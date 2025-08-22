# functions/scenarios/shocks/schedule_shocks_over_horizon.R

#' Schedule shock impulses over a forecast horizon
#'
#' Creates a time path of shock multipliers (length = horizon).
#' Supports:
#' - constant (step) shocks
#' - ramp (linear fade-in / fade-out)
#' - exponential decay
#' - sinusoidal cycles
#' - custom user-specified numeric profile
#'
#' @param horizon Integer >= 1. Number of forecast periods.
#' @param start Integer >= 1. First period shock applies (1 = contemporaneous).
#' @param length Integer >= 1. Duration in periods (ignored if profile supplied).
#' @param type Character: "constant","ramp","expdecay","sinus","custom". Default "constant".
#' @param decay Numeric scalar (0<decay<1) for exponential decay, default 0.5.
#' @param profile Optional numeric vector for type="custom". Length must be <= horizon.
#'                Values are written starting at `start`. Overrides length/decay/type params.
#' @param normalize Logical. If TRUE, scale profile so max=1. Default FALSE.
#' @param allow_truncate Logical. If TRUE, schedule that extends beyond horizon is truncated
#'                       with a warning. If FALSE, error. Default TRUE.
#'
#' @return Numeric vector of length horizon with multipliers (0 outside shock window).
#'         Attributes include:
#'           - type
#'           - start
#'           - length
#'           - decay
#'           - normalized
#'           - truncated (logical)
#'
#' @examples
#' schedule_shocks_over_horizon(12, start=1, length=4, type="constant")
#' schedule_shocks_over_horizon(12, start=2, length=5, type="ramp")
#' schedule_shocks_over_horizon(12, start=1, length=6, type="expdecay", decay=0.7)
#' schedule_shocks_over_horizon(12, start=1, profile=c(1,0.8,0.5,0.2), type="custom")
#'
#' @export
schedule_shocks_over_horizon <- function(
    horizon,
    start,
    length   = 1L,
    type     = c("constant","ramp","expdecay","sinus","custom"),
    decay    = 0.5,
    profile  = NULL,
    normalize= FALSE,
    allow_truncate = TRUE
) {
  type <- match.arg(type)
  
  # ---- validate inputs ----
  if (!is.numeric(horizon) || length(horizon)!=1L || horizon < 1)
    stop("`horizon` must be integer >=1")
  horizon <- as.integer(horizon)
  
  if (!is.numeric(start) || length(start)!=1L || start < 1)
    stop("`start` must be integer >=1")
  start <- as.integer(start)
  
  if (!is.null(profile)) type <- "custom"  # override
  
  if (type != "custom") {
    if (!is.numeric(length) || length(length)!=1L || length < 1)
      stop("`length` must be integer >=1")
    length <- as.integer(length)
  }
  
  # initialize
  sched <- rep(0, horizon)
  truncated <- FALSE
  
  # ---- build profile ----
  if (type == "custom") {
    if (!is.numeric(profile) || !length(profile))
      stop("Custom profile must be a non-empty numeric vector.")
    L <- length(profile)
    end <- start + L - 1L
    if (end > horizon) {
      if (allow_truncate) {
        profile <- profile[seq_len(horizon - start + 1L)]
        end <- horizon; truncated <- TRUE
        warning("Custom profile truncated to fit horizon.")
      } else {
        stop("Custom profile exceeds horizon. Increase horizon or allow_truncate=TRUE.")
      }
    }
    idx <- start:end
    sched[idx] <- profile
  } else if (type == "constant") {
    end <- start + length - 1L
    if (end > horizon) {
      if (allow_truncate) { end <- horizon; truncated <- TRUE }
      else stop("Shock schedule exceeds horizon.")
    }
    sched[start:end] <- 1
  } else if (type == "ramp") {
    end <- start + length - 1L
    if (end > horizon) {
      if (allow_truncate) { end <- horizon; length <- end-start+1; truncated <- TRUE }
      else stop("Ramp schedule exceeds horizon.")
    }
    ramp <- seq(0,1,length.out=length)
    sched[start:end] <- ramp
  } else if (type == "expdecay") {
    end <- start + length - 1L
    if (end > horizon) {
      if (allow_truncate) { end <- horizon; length <- end-start+1; truncated <- TRUE }
      else stop("Expdecay schedule exceeds horizon.")
    }
    sched[start:end] <- decay^(0:(length-1))
  } else if (type == "sinus") {
    end <- start + length - 1L
    if (end > horizon) {
      if (allow_truncate) { end <- horizon; length <- end-start+1; truncated <- TRUE }
      else stop("Sinus schedule exceeds horizon.")
    }
    t <- seq(0, pi, length.out = length)
    sched[start:end] <- sin(t)
  }
  
  # normalization
  if (isTRUE(normalize) && any(sched != 0)) {
    sched <- sched / max(abs(sched), na.rm=TRUE)
  }
  
  attr(sched, "meta") <- list(
    type       = type,
    start      = start,
    length     = if (type=="custom") length(profile) else length,
    decay      = if (type=="expdecay") decay else NA_real_,
    normalized = normalize,
    truncated  = truncated
  )
  sched
}