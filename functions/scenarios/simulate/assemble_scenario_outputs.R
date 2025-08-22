# functions/scenarios/simulate/assemble_scenario_outputs.R

#' Assemble tidy outputs from multiple scenario runs
#'
#' Accepts a list of per-scenario results (from `simulate_scenarios_batch()` or
#' equivalent) and returns a single tidy data frame. Handles inputs in either form:
#'   - list of lists with fields {spec, baseline, shocked, tidy} (preferred)
#'   - list of already-tidy tibbles (each containing both baseline & shocked rows)
#'
#' Guarantees consistent columns: t, variable, regime, scenario, value, is_baseline
#' (plus any optional "draw" or extra columns that were present).
#'
#' Extras:
#' - `compute_delta`: add Δ = shocked − baseline (per scenario × regime × variable × t)
#' - `format`: "long" (default) or "wide" (adds columns `baseline`, `shocked`, `delta`)
#' - `complete_grid`: ensure full grid over {scenario, regime, variable, t} (fills missing with NA)
#'
#' @param results List. Each element is either:
#'   * a list with $spec (list), $baseline (tibble), $shocked (tibble), optional $tidy
#'   * a tibble already containing both baselines and shocked rows
#' @param compute_delta Logical. If TRUE, compute Δ = shocked − baseline. Default TRUE.
#' @param format "long" or "wide". In "wide", returns columns: baseline, shocked, delta. Default "long".
#' @param complete_grid Logical. If TRUE, complete {scenario, regime, variable, t} grid. Default FALSE.
#' @param keep_columns Character vector of additional columns to keep if present (e.g., "draw").
#' @param validate Logical. If TRUE, run light validations (type/NA checks). Default TRUE.
#'
#' @return A tibble. Attributes:
#'   - "meta": list(total_scenarios, scenarios, variables, regimes, t_min, t_max, created_at)
#' @export
assemble_scenario_outputs <- function(
    results,
    compute_delta  = TRUE,
    format         = c("long","wide"),
    complete_grid  = FALSE,
    keep_columns   = c("draw"),
    validate       = TRUE
) {
  format <- match.arg(format)
  `%||%` <- function(x,y) if (is.null(x)) y else x
  
  if (!length(results)) stop("`results` is empty.")
  
  # ---- Normalize each element to a tidy tibble with required cols -------------
  normalize_one <- function(x) {
    # Case A: already a tibble
    if (inherits(x, "data.frame")) {
      tbl <- tibble::as_tibble(x)
      req <- c("t","variable","regime","scenario","value","is_baseline")
      missing <- setdiff(req, names(tbl))
      if (length(missing)) {
        stop("Tidy scenario is missing required columns: ", paste(missing, collapse = ", "))
      }
      return(tbl)
    }
    # Case B: list with baseline + shocked (preferred)
    if (is.list(x) && !is.null(x$baseline) && !is.null(x$shocked)) {
      b <- tibble::as_tibble(x$baseline)
      s <- tibble::as_tibble(x$shocked)
      # Ensure common columns
      add_if_missing <- function(df, col, val) { if (!col %in% names(df)) df[[col]] <- val; df }
      b <- add_if_missing(b, "scenario", x$spec$name %||% NA_character_)
      b <- add_if_missing(b, "regime",   attr(s, "meta")$regime_key %||% "combined")
      b <- add_if_missing(b, "is_baseline", TRUE)
      
      s <- add_if_missing(s, "scenario", x$spec$name %||% NA_character_)
      s <- add_if_missing(s, "is_baseline", FALSE)
      
      # Align columns (variable, t, regime, value exist by design in prior functions)
      common <- union(names(b), names(s))
      b[setdiff(common, names(b))] <- NA
      s[setdiff(common, names(s))] <- NA
      
      out <- dplyr::bind_rows(b, s)
      return(out)
    }
    # Case C: list with $tidy
    if (is.list(x) && !is.null(x$tidy)) {
      return(tibble::as_tibble(x$tidy))
    }
    stop("Each element of `results` must be a tibble or a list with {baseline, shocked} or {tidy}.")
  }
  
  tidies <- purrr::map(results, normalize_one)
  tidy   <- dplyr::bind_rows(tidies)
  
  # Keep additional columns if requested and present
  extra_cols <- intersect(keep_columns, names(tidy))
  
  # ---- Light validation -------------------------------------------------------
  if (isTRUE(validate)) {
    must_num  <- c("t","value")
    must_chr  <- c("variable","regime","scenario")
    must_lgl  <- c("is_baseline")
    if (any(!sapply(tidy[must_num], is.numeric))) stop("Columns `t` and `value` must be numeric.")
    if (any(!sapply(tidy[must_chr], is.character))) stop("Columns `variable`, `regime`, `scenario` must be character.")
    if (!is.logical(tidy$is_baseline)) {
      tidy$is_baseline <- as.logical(tidy$is_baseline)
      if (any(is.na(tidy$is_baseline))) stop("`is_baseline` must be logical (TRUE/FALSE).")
    }
    if (any(is.na(tidy$t))) stop("`t` contains NA.")
    if (any(tidy$t < 1))   stop("`t` must be >= 1.")
  }
  
  # ---- Optionally complete grid ----------------------------------------------
  if (isTRUE(complete_grid)) {
    scen <- sort(unique(tidy$scenario))
    reg  <- sort(unique(tidy$regime))
    vars <- sort(unique(tidy$variable))
    tmin <- min(tidy$t, na.rm = TRUE)
    tmax <- max(tidy$t, na.rm = TRUE)
    grid <- tidyr::expand_grid(
      scenario = scen,
      regime   = reg,
      variable = vars,
      t        = seq.int(tmin, tmax),
      is_baseline = c(TRUE, FALSE)
    )
    # left join to preserve existing values, fill missing with NA
    keep <- c("scenario","regime","variable","t","is_baseline","value", extra_cols)
    tidy <- grid |>
      dplyr::left_join(tidy[, intersect(names(tidy), keep), drop = FALSE],
                       by = c("scenario","regime","variable","t","is_baseline"))
  }
  
  # ---- Compute delta (shocked - baseline) ------------------------------------
  # Note: we compute deltas per scenario × regime × variable × t. Extra columns (e.g., draw)
  # are not retained in delta calc; deltas summarize across them implicitly.
  if (isTRUE(compute_delta) || identical(format, "wide")) {
    base <- tidy |>
      dplyr::filter(is_baseline) |>
      dplyr::select(scenario, regime, variable, t, baseline = value)
    
    shock <- tidy |>
      dplyr::filter(!is_baseline) |>
      dplyr::select(scenario, regime, variable, t, shocked = value)
    
    joined <- dplyr::full_join(base, shock, by = c("scenario","regime","variable","t"))
    joined <- joined |>
      dplyr::mutate(delta = shocked - baseline)
    
    if (identical(format, "wide")) {
      # return wide with baseline, shocked, delta
      wide <- joined |>
        dplyr::arrange(scenario, regime, variable, t)
      # attach meta and return
      attr(wide, "meta") <- .assemble_meta(wide)
      return(wide)
    } else {
      # long: append delta rows with is_baseline = NA (or keep separate?)
      # Prefer to keep tidy output as original plus a separate Δ frame if needed.
      # We'll instead merge Δ back into the shocked rows as additional columns lo_/hi_ remain intact.
      tidy <- tidy |>
        dplyr::left_join(
          joined[, c("scenario","regime","variable","t","baseline","shocked","delta")],
          by = c("scenario","regime","variable","t")
        )
    }
  }
  
  # ---- Final ordering & metadata ---------------------------------------------
  tidy <- tidy |>
    dplyr::arrange(scenario, regime, variable, t, is_baseline)
  
  attr(tidy, "meta") <- .assemble_meta(tidy)
  tidy
}

# ---- helper to build attr(meta) ----------------------------------------------
.assemble_meta <- function(df) {
  list(
    total_scenarios = dplyr::n_distinct(df$scenario),
    scenarios       = sort(unique(df$scenario)),
    variables       = sort(unique(df$variable)),
    regimes         = sort(unique(df$regime)),
    t_min           = suppressWarnings(min(df$t, na.rm = TRUE)),
    t_max           = suppressWarnings(max(df$t, na.rm = TRUE)),
    created_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}
