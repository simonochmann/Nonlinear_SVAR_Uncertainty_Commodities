# functions/scenarios/validate/validate_identification_choice.R

#' Validate and normalize the identification choice for scenario simulation
#'
#' Supported identification modes:
#'   - "unit":     A = I_K
#'   - "cholesky": A = chol(Sigma) under `ordering` (upper-tri by convention)
#'   - "sign":     start from Cholesky and flip shock-column signs to satisfy
#'                 horizon-0 sign constraints (simple column flips)
#'   - "custom":   user-supplied A (K×K)
#'
#' This validator:
#'   * checks the method and regime selection,
#'   * ensures `ordering` is a full permutation of `variables`,
#'   * verifies Sigma availability in `model` if needed,
#'   * validates a provided `A_custom`,
#'   * validates & normalizes sign-constraints (response, shock, sign ∈ {+,-,0}),
#'   * collects optional identification parameters (shrink_lambda, min_eig, pivoted_chol).
#'
#' @param scenario List (validated YAML spec preferred). Fields used:
#'        $identification, $ordering (optional), $regime_conditioning (optional),
#'        $sign_constraints (optional data.frame), $A_custom (optional matrix),
#'        $ident_params (optional list: shrink_lambda, min_eig, pivoted_chol).
#' @param model Optional model object for consistency checks. If provided, used to:
#'        - pull `variables` when not supplied,
#'        - check Sigma availability for Cholesky/Sign under the chosen regime.
#'        Expected fields (best-effort): $variables, $Sigma, $regimes.
#' @param variables Character vector of model variables in canonical order.
#'        If NULL, falls back to `model$variables`.
#' @param regime_override Optional regime key ("combined","low","high") to force
#'        identification regime. If NULL, derived from scenario$regime_conditioning:
#'          "none" → "combined", "force_low" → "low", "force_high" → "high".
#' @param require_sigma Logical. If TRUE (default), error when Sigma is not
#'        found for "cholesky"/"sign". If FALSE, only warn.
#' @param verbose Logical. Emit informative messages. Default FALSE.
#'
#' @return A list with class "validated_identification":
#'   - identification    (chr) one of "unit","cholesky","sign","custom"
#'   - variables         (chr) canonical order used
#'   - ordering          (chr) the ordering used for factorization (perm of variables)
#'   - regime_key        (chr) regime used for identification
#'   - sign_constraints  (data.frame or NULL) normalized constraints
#'   - A_custom          (matrix or NULL) validated K×K (not reordered here)
#'   - ident_params      (list) subset of {shrink_lambda, min_eig, pivoted_chol}
#'   - notes             (character vector) warnings/decisions taken
#'   - created_at        (chr) ISO timestamp
#' @export
validate_identification_choice <- function(
    scenario,
    model            = NULL,
    variables        = NULL,
    regime_override  = NULL,
    require_sigma    = TRUE,
    verbose          = FALSE
) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  .iso <- function(t = Sys.time()) format(t, "%Y-%m-%dT%H:%M:%S%z")
  
  if (is.null(scenario) || !is.list(scenario)) {
    stop("`scenario` must be a list-like spec (from YAML or constructed).")
  }
  
  # ---- method ----------------------------------------------------------------
  method <- scenario$identification %||% "unit"
  allowed <- c("unit","cholesky","sign","custom")
  if (!method %in% allowed) {
    stop("Unknown `identification`='", method, "'. Must be one of: ",
         paste(allowed, collapse = ", "), ".")
  }
  
  # ---- variables & ordering --------------------------------------------------
  variables <- variables %||% (if (!is.null(model)) model$variables else NULL)
  if (is.null(variables) || !is.character(variables) || !length(variables)) {
    stop("`variables` must be supplied or available at `model$variables` (non-empty character vector).")
  }
  K <- length(variables)
  
  ordering <- scenario$ordering %||% variables
  if (!is.character(ordering) || length(ordering) != K || !setequal(ordering, variables)) {
    stop("`ordering` must be a full permutation of `variables` (length K). ",
         "Got length=", length(ordering), ", setequal=", setequal(ordering, variables), ".")
  }
  
  # ---- regime selection ------------------------------------------------------
  regime_key <- regime_override %||% switch(
    tolower(scenario$regime_conditioning %||% "none"),
    "force_low"  = "low",
    "force_high" = "high",
    "none"       = "combined",
    "combined"
  )
  
  # ---- Sigma availability for cholesky/sign ---------------------------------
  notes <- character(0)
  if (method %in% c("cholesky","sign")) {
    if (!is.null(model)) {
      has_sigma <- .detect_sigma(model, regime_key)
      if (!has_sigma) {
        msg <- paste0("No residual covariance `Sigma` found in model for regime '",
                      regime_key, "' (checked model$regimes[[regime]]$residual_cov, ",
                      "model$Sigma[[regime]], model$Sigma$combined).")
        if (isTRUE(require_sigma)) stop(msg) else { warning(msg); notes <- c(notes, msg) }
      }
    } else {
      msg <- "Model not provided; cannot verify Sigma availability for Cholesky/Sign."
      if (isTRUE(require_sigma)) warning(msg)
      notes <- c(notes, msg)
    }
  }
  
  # ---- ident_params (optional tuning for A building) -------------------------
  ip <- scenario$ident_params %||% list()
  # sanitize
  ip$shrink_lambda <- ip$shrink_lambda %||% 0
  ip$min_eig       <- ip$min_eig       %||% 1e-8
  ip$pivoted_chol  <- ip$pivoted_chol  %||% TRUE
  
  if (!is.numeric(ip$shrink_lambda) || length(ip$shrink_lambda) != 1L || ip$shrink_lambda < 0 || ip$shrink_lambda >= 1) {
    stop("`ident_params$shrink_lambda` must be a numeric scalar in [0,1).")
  }
  if (!is.numeric(ip$min_eig) || length(ip$min_eig) != 1L || ip$min_eig < 0) {
    stop("`ident_params$min_eig` must be a non-negative numeric scalar.")
  }
  ip$pivoted_chol <- isTRUE(ip$pivoted_chol)
  
  # ---- sign constraints (if needed) ------------------------------------------
  sc_norm <- NULL
  if (method == "sign") {
    sc <- scenario$sign_constraints %||% NULL
    if (is.null(sc)) {
      stop("`identification='sign'` requires `scenario$sign_constraints` (data.frame with columns response, shock, sign).")
    }
    if (is.data.frame(sc)) {
      req_cols <- c("response","shock","sign")
      missing <- setdiff(req_cols, names(sc))
      if (length(missing)) stop("`sign_constraints` missing columns: ", paste(missing, collapse = ", "))
      sc$response <- as.character(sc$response)
      sc$shock    <- as.character(sc$shock)
      sc$sign     <- as.character(sc$sign)
    } else if (is.list(sc)) {
      # allow list of triplets
      sc <- do.call(rbind, lapply(sc, function(x)
        data.frame(response = as.character(x$response),
                   shock    = as.character(x$shock),
                   sign     = as.character(x$sign),
                   stringsAsFactors = FALSE)))
    } else {
      stop("`sign_constraints` must be a data.frame or list of {response,shock,sign}.")
    }
    
    # validate names exist in variables
    if (!all(sc$response %in% variables)) {
      bad <- setdiff(sc$response, variables)
      stop("sign_constraints$response contains unknown variables: ", paste(bad, collapse = ", "))
    }
    if (!all(sc$shock %in% variables)) {
      bad <- setdiff(sc$shock, variables)
      stop("sign_constraints$shock contains unknown variables: ", paste(bad, collapse = ", "))
    }
    # validate signs
    ok_signs <- c("+","-","0")
    if (!all(sc$sign %in% ok_signs)) {
      bad <- unique(sc$sign[!sc$sign %in% ok_signs])
      stop("sign_constraints$sign must be one of '+','-','0'. Offenders: ", paste(bad, collapse = ", "))
    }
    # contradictions: same (response,shock) assigned incompatible signs
    dup_pairs <- unique(sc[duplicated(sc[c("response","shock")]) | duplicated(sc[c("response","shock")], fromLast = TRUE), c("response","shock")])
    if (nrow(dup_pairs)) {
      # for each pair, ensure all signs identical
      for (k in seq_len(nrow(dup_pairs))) {
        r <- dup_pairs$response[k]; s <- dup_pairs$shock[k]
        vals <- unique(sc$sign[sc$response == r & sc$shock == s])
        if (length(vals) > 1L) {
          stop("Contradictory sign constraints for response='", r, "', shock='", s, "': ", paste(vals, collapse = ", "))
        }
      }
      # deduplicate rows
      sc <- unique(sc)
      notes <- c(notes, "Duplicate sign-constraint rows collapsed to unique set.")
    }
    sc_norm <- sc[, c("response","shock","sign")]
  }
  
  # ---- custom A (if provided) ------------------------------------------------
  A_custom <- NULL
  if (method == "custom") {
    # accept from scenario or from model$A[[regime]]
    A_custom <- scenario$A_custom %||% (if (!is.null(model) && !is.null(model$A)) model$A[[regime_key]] %||% model$A$combined else NULL)
    if (is.null(A_custom)) {
      stop("`identification='custom'` but no `scenario$A_custom` (or model$A[[regime]]) provided.")
    }
    if (!is.matrix(A_custom) || !is.numeric(A_custom) || nrow(A_custom) != K || ncol(A_custom) != K) {
      stop("`A_custom` must be a numeric K×K matrix. Expected K=", K, ", got (", nrow(A_custom), "×", ncol(A_custom), ").")
    }
    if (any(!is.finite(A_custom))) stop("`A_custom` contains non-finite values.")
    # (we do NOT reorder here; `build_contemporaneous_A()` aligns if needed)
  }
  
  # ---- finalize --------------------------------------------------------------
  out <- list(
    identification   = method,
    variables        = variables,
    ordering         = ordering,
    regime_key       = regime_key,
    sign_constraints = sc_norm,
    A_custom         = A_custom,
    ident_params     = ip,
    notes            = unique(notes),
    created_at       = .iso()
  )
  class(out) <- c("validated_identification","list")
  
  if (isTRUE(verbose)) {
    msg <- paste0("[validate_identification_choice] method=", method,
                  ", regime=", regime_key,
                  ", ordering=", paste(ordering, collapse = ", "),
                  if (!is.null(sc_norm)) paste0(", sign_constraints=", nrow(sc_norm), " row(s)") else "",
                  if (!is.null(A_custom)) ", custom A provided" else "")
    message(msg)
  }
  
  out
}

# ---------- helpers ------------------------------------------------------------

# Best-effort check: does model provide Sigma for the requested regime?
.detect_sigma <- function(model, regime_key) {
  # try regimes list first
  if (!is.null(model$regimes) &&
      !is.null(model$regimes[[regime_key]]) &&
      !is.null(model$regimes[[regime_key]]$residual_cov)) return(TRUE)
  
  # then Sigma list by regime
  if (!is.null(model$Sigma) && !is.null(model$Sigma[[regime_key]])) return(TRUE)
  
  # finally combined Sigma
  if (!is.null(model$Sigma) && !is.null(model$Sigma$combined)) return(TRUE)
  
  FALSE
}
