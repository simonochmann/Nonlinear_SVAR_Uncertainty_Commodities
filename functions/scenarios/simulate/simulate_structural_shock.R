# functions/scenarios/simulate/simulate_structural_shock.R

#' Simulate a scenario's structural-shock response path via IRF superposition
#'
#' Given a validated scenario spec, builds the contemporaneous impact matrix A,
#' converts human-sized shocks into absolute units (via sigma), schedules them
#' over the horizon, and linearly combines the appropriate IRFs to produce the
#' shocked path. Designed for TVAR objects with regime-aware IRFs.
#'
#' Expected IRF structure:
#'   model$irf[[regime_key]][[impulse]][[response]] = numeric vector (length >= horizon)
#' where `regime_key` is usually "combined", with "low"/"high" as fallbacks.
#'
#' @param model     TVAR/VAR model object carrying at least:
#'                    - $variables (character K)
#'                    - $irf (list as above)
#'                    - optionally $Sigma[[regime]] or $regimes[[regime]]$residual_cov
#'                    - optionally $sigma (named numeric) for scale='sd'
#' @param variables Character vector of model variables in order (defaults to model$variables).
#' @param scenario  Validated scenario spec (see validate_scenario_spec()).
#' @param sigma     Named numeric vector OR list(low/high/combined) of such vectors for scale='sd'.
#'                  If NULL, falls back to model$sigma or unit vector.
#' @param responses Optional subset of response variable names to return (default: all variables).
#' @param regime_override Optional regime key to force IRF set (e.g. "low" or "high").
#'                        If NULL, uses scenario$regime_conditioning: "none" → "combined" IRFs,
#'                        "force_low" → "low", "force_high" → "high" (fallbacks applied if missing).
#' @param identification Optional override of scenario$identification ("unit","cholesky","sign","custom").
#' @param A_custom Optional custom A to use when identification="custom".
#' @param verbose  Logical; print informational messages.
#'
#' @return tibble with columns:
#'         t, variable, regime, scenario, value, is_baseline (=FALSE)
#'         Attribute "meta" includes A metadata, shock summaries, and IRF source info.
#' @export
simulate_structural_shock <- function(
    model,
    variables         = NULL,
    scenario,
    sigma             = NULL,
    responses         = NULL,
    regime_override   = NULL,
    identification    = NULL,
    A_custom          = NULL,
    verbose           = FALSE
) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  # --------------------- validate & setup -------------------------------------
  stopifnot(is.list(model), is.list(scenario))
  variables <- variables %||% model$variables
  if (is.null(variables) || !is.character(variables) || !length(variables)) {
    stop("`variables` must be provided or present as model$variables.")
  }
  K <- length(variables)
  
  H <- as.integer(scenario$horizon %||% stop("Scenario missing `horizon`."))
  if (H < 1L) stop("horizon must be >= 1")
  
  if (!is.null(responses)) {
    if (!all(responses %in% variables))
      stop("Unknown responses: ", paste(setdiff(responses, variables), collapse=", "))
  } else {
    responses <- variables
  }
  
  # sigma: allow vector or regime-list; fallback to model$sigma or 1s
  sigma <- sigma %||% model$sigma %||% stats::setNames(rep(1, K), variables)
  if (is.numeric(sigma) && is.null(names(sigma))) names(sigma) <- variables
  
  # choose regime source for IRFs
  regime_key <- regime_override %||% switch(
    tolower(scenario$regime_conditioning %||% "none"),
    "force_low"  = "low",
    "force_high" = "high",
    "none"       = "combined",
    "combined"
  )
  
  # pick IRF list with graceful fallbacks
  irf_src <- model$irf[[regime_key]] %||% model$irf$combined %||% model$irf$low %||% model$irf$high
  if (is.null(irf_src)) stop("IRF source not found in model for regime '", regime_key, "' (and fallbacks).")
  
  # identification (A matrix)
  ident_used <- identification %||% scenario$identification %||% "unit"
  A <- build_contemporaneous_A(
    model           = model,
    identification  = ident_used,
    variables       = variables,
    regime          = regime_key,
    A_custom        = A_custom,
    verbose         = verbose
  )
  A_meta <- attr(A, "meta")
  
  # --------------------- core superposition engine ----------------------------
  # Helper to fetch IRF(impulse -> response) as a vector length >= H (pad with zeros)
  .get_irf_vec <- function(impulse, response) {
    vec <- try(irf_src[[impulse]][[response]], silent = TRUE)
    if (inherits(vec, "try-error") || is.null(vec)) {
      stop("IRF missing for impulse='", impulse, "' -> response='", response, "' in regime '", regime_key, "'.")
    }
    v <- as.numeric(vec)
    if (length(v) < H) v <- c(v, rep(0, H - length(v)))
    v[seq_len(H)]
  }
  
  # Initialize response matrix Y (H x K)
  Y <- matrix(0, nrow = H, ncol = K, dimnames = list(NULL, variables))
  
  # Loop over impulses in the scenario
  if (is.null(scenario$impulses) || !length(scenario$impulses)) {
    stop("Scenario has no `impulses`.")
  }
  
  # build a convenient alias map if scenario provided one; otherwise NULL
  aliases <- scenario$aliases %||% NULL
  
  for (imp in scenario$impulses) {
    # --- resolve impulse target(s) & magnitude ---
    # support single variable or basket (character vector); also named numeric custom handled by build_shock_vector()
    impulse_names <- imp$variable
    weights       <- imp$weights %||% NULL
    
    # compute size in absolute units for each targeted variable (per-name scaling)
    # If basket: we first construct a unit vector via build_shock_vector(..., size=1)
    # then scale the overall size using the designated `scale` relative to the *primary* variable (first in list)
    # or apply per-variable scaling if you prefer; we choose per-variable scaling (cleaner).
    if (is.character(impulse_names)) {
      # per-variable scaled sizes
      sizes_abs <- vapply(impulse_names, function(vn) {
        scale_shock_by_sigma(
          size   = as.numeric(imp$size),
          scale  = tolower(imp$scale %||% "sd"),
          sigma  = sigma,
          var    = vn,
          aliases= aliases
        )
      }, numeric(1))
      names(sizes_abs) <- impulse_names
      # weights handling: compose final custom vector across variables using build_shock_vector
      # Here we pass size=NULL and a named numeric vector -> "custom" path
      custom_vec <- sizes_abs
      if (!is.null(weights)) {
        # when weights supplied, distribute signs/magnitudes accordingly:
        # normalize weights to sum1 on abs and multiply by |sizes_abs|max to keep magnitude reasonable
        w <- as.numeric(weights)
        if (length(w) != length(impulse_names)) stop("weights length must match impulse variable vector.")
        s <- sum(abs(w))
        if (s == 0) stop("All weights are zero in impulse basket.")
        w <- w / s
        # scale basket to overall |size_abs_max|; sign follows weights
        # choose reference magnitude as mean abs(sizes_abs) to avoid dominance by outlier sigma
        ref_mag <- mean(abs(sizes_abs))
        custom_vec <- stats::setNames(w * ref_mag * sign(sizes_abs[1]), impulse_names)
      }
      shock_vec <- build_shock_vector(
        variables = variables,
        impulse   = custom_vec,  # named numeric → custom mode
        normalize = "none"
      )
    } else if (is.numeric(impulse_names) && !is.null(names(impulse_names))) {
      # already a named numeric vector → treat as absolute magnitudes
      shock_vec <- build_shock_vector(
        variables = variables,
        impulse   = impulse_names,
        normalize = "none"
      )
    } else {
      stop("Impulse `variable` must be a character vector or a named numeric vector.")
    }
    
    # validate schedule and build multiplier path
    sch <- imp$schedule
    if (!is.list(sch) || is.null(sch$start) || is.null(sch$length)) {
      stop("Impulse schedule must be a list with at least {start, length}.")
    }
    sched <- schedule_shocks_over_horizon(
      horizon  = H,
      start    = sch$start,
      length   = sch$length,
      type     = tolower(sch$type %||% "constant"),
      decay    = as.numeric(sch$decay %||% 0.5),
      profile  = sch$profile %||% NULL,
      normalize= isTRUE(sch$normalize),
      allow_truncate = TRUE
    )
    
    # map structural shocks through A to reduced-form impact at each t where shock applies
    # contribution at time h from an impulse at tau:  y[h] += (A %*% shock_vec) * IRF_k for k = h - tau + 1
    impact0 <- as.numeric(A %*% shock_vec)  # K-length vector aligned to `variables`
    
    # superpose across time
    for (tau in which(sched != 0)) {
      mult <- sched[tau]  # schedule multiplier at this tau (often 1)
      for (h in tau:H) {
        k <- h - tau + 1L
        # add contributions for each response variable j
        # y_{h,j} += sum_i ( impact0[i] * IRF_{i→j}[k] ) * mult
        # compute once the IRF column sum_i(impact0[i] * IRF_{i→j}[k]) for each j
        for (j in seq_len(K)) {
          resp <- variables[j]
          contrib_j <- 0.0
          # accumulate from all impulse channels i
          for (i in seq_len(K)) {
            imp_name <- variables[i]
            irf_ij <- .get_irf_vec(imp_name, resp)[k]
            contrib_j <- contrib_j + impact0[i] * irf_ij
          }
          Y[h, j] <- Y[h, j] + mult * contrib_j
        }
      }
    }
  } # end impulses loop
  
  # --------------------- tidy output + metadata --------------------------------
  out <- tibble::tibble(
    t        = rep(seq_len(H), each = length(responses)),
    variable = rep(responses, times = H),
    regime   = regime_key,
    scenario = scenario$name %||% NA_character_,
    value    = c(Y[, responses, drop = FALSE]),
    is_baseline = FALSE
  )
  
  meta <- list(
    scenario_name   = scenario$name %||% NA_character_,
    regime_key      = regime_key,
    identification  = ident_used,
    A_meta          = A_meta,
    horizon         = H,
    K               = K,
    variables       = variables,
    responses       = responses,
    impulses        = scenario$impulses,
    created_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  attr(out, "meta") <- meta
  out
}