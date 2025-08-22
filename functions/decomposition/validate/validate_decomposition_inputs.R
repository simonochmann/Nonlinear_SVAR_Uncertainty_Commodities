#' Validate decomposition inputs (baseline & shocked)
#'
#' Checks:
#' - Required columns present
#' - Keys are unique (no duplicate rows)
#' - Overlapping variables/regimes exist
#' - Horizon alignment on (regime, variable) keys
#' - t is integerish and non-negative
#'
#' @param baseline tibble: regime, variable, t, value
#' @param shocked  tibble: scenario, regime, variable, t, value
#' @param require_same_horizon logical: enforce identical t sets per (regime,variable)
#' @param allow_empty_overlap logical: if FALSE, stop when no overlap between sets
#' @param report_only logical: if TRUE, never stop; return a list report
#'
#' @return invisible(list(ok=TRUE/FALSE, issues=tibble(...), meta=list(...))) if report_only
#'         otherwise returns invisible(TRUE) or throws an error
validate_decomposition_inputs <- function(
    baseline,
    shocked,
    require_same_horizon = TRUE,
    allow_empty_overlap  = FALSE,
    report_only          = FALSE
) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  
  issues <- tibble::tibble(level=character(), where=character(), what=character())
  
  .need_cols <- function(df, cols, name) {
    miss <- setdiff(cols, names(df))
    if (length(miss)) {
      msg <- glue::glue("{name}: missing columns: {paste(miss, collapse=', ')}")
      if (report_only) {
        issues <<- dplyr::bind_rows(issues, tibble::tibble(level="error", where=name, what=msg))
      } else stop(msg)
    }
  }
  .integerish_nonneg <- function(x) {
    if (!is.numeric(x)) return(FALSE)
    if (any(!is.finite(x))) return(FALSE)
    all(abs(x - round(x)) < 1e-10 & x >= 0)
  }
  .dup_count <- function(df, keys) {
    df |>
      dplyr::count(dplyr::across(dplyr::all_of(keys)), name="n") |>
      dplyr::filter(.data$n > 1) |> nrow()
  }
  .add_issue <- function(level, where, what) {
    issues <<- dplyr::bind_rows(issues, tibble::tibble(level=level, where=where, what=what))
  }
  
  # 1) Columns
  .need_cols(baseline, c("regime","variable","t","value"), "baseline")
  .need_cols(shocked,  c("scenario","regime","variable","t","value"), "shocked")
  
  # 2) t integerish & non-negative
  if (!.integerish_nonneg(baseline$t)) .add_issue("error","baseline","t must be integerish and non-negative")
  if (!.integerish_nonneg(shocked$t))  .add_issue("error","shocked","t must be integerish and non-negative")
  
  # 3) Unique keys
  dups_base <- .dup_count(baseline, c("regime","variable","t"))
  dups_shck <- .dup_count(shocked,  c("scenario","regime","variable","t"))
  if (dups_base > 0) .add_issue("error","baseline", glue::glue("duplicate (regime,variable,t) rows: {dups_base}"))
  if (dups_shck > 0) .add_issue("error","shocked",  glue::glue("duplicate (scenario,regime,variable,t) rows: {dups_shck}"))
  
  # 4) Overlap in variables & regimes
  vars_overlap   <- intersect(unique(baseline$variable), unique(shocked$variable))
  regimes_overlap<- intersect(unique(baseline$regime),   unique(shocked$regime))
  if (!length(vars_overlap) || !length(regimes_overlap)) {
    .add_issue("error","overlap","no common variables and/or regimes between baseline and shocked")
  }
  
  # 5) Horizon alignment per (regime, variable)
  if (require_same_horizon) {
    base_keys <- baseline |>
      dplyr::group_by(.data$regime, .data$variable) |>
      dplyr::summarise(t_set = list(sort(unique(.data$t))), .groups="drop")
    
    shck_keys <- shocked |>
      dplyr::group_by(.data$regime, .data$variable) |>
      dplyr::summarise(t_set = list(sort(unique(.data$t))), .groups="drop") |>
      dplyr::distinct()
    
    joint <- dplyr::inner_join(base_keys, shck_keys,
                               by=c("regime","variable"),
                               suffix=c("_base","_shck"))
    
    if (nrow(joint) == 0 && !allow_empty_overlap) {
      .add_issue("error","alignment","no overlapping (regime,variable) keys to compare")
    } else {
      misaligned <- joint |>
        dplyr::mutate(ok = purrr::map2_lgl(.data$t_set_base, .data$t_set_shck, ~identical(.x, .y))) |>
        dplyr::filter(!.data$ok)
      if (nrow(misaligned) > 0) {
        .add_issue("error","alignment", glue::glue("mismatched t sets for {nrow(misaligned)} (regime,variable) pairs"))
      }
    }
  }
  
  ok <- nrow(issues) == 0
  if (report_only) return(invisible(list(ok=ok, issues=issues, meta=list(
    variables_overlap = sort(vars_overlap),
    regimes_overlap   = sort(regimes_overlap),
    max_t_baseline    = suppressWarnings(max(baseline$t, na.rm=TRUE)),
    max_t_shocked     = suppressWarnings(max(shocked$t,  na.rm=TRUE))
  ))))
  
  if (!ok) {
    msg <- paste(paste0(issues$where, ": ", issues$what), collapse = "\n")
    stop(msg)
  }
  invisible(TRUE)
}
