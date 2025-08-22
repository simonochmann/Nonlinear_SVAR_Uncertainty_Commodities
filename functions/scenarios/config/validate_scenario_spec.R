# functions/scenarios/config/validate_scenario_spec.R
`%||%` <- function(x, y) if (is.null(x)) y else x

validate_scenario_spec <- function(spec, model_vars = NULL) {
  stopifnot(is.list(spec))
  errs <- character(); wns <- character()
  add_err <- function(...) errs <<- c(errs, paste0(...))
  add_wn  <- function(...) wns  <<- c(wns,  paste0(...))
  
  # ---- Top-level ----
  if (is.null(spec$name) || !nzchar(spec$name)) add_err("Missing 'name'.")
  if (!is.null(spec$horizon) && (!is.numeric(spec$horizon) || length(spec$horizon) != 1))
    add_err("'horizon' must be a scalar number.")
  if (!is.null(spec$regime) && !spec$regime %in% c("combined","low","high"))
    add_wn("Unknown 'regime' = ", spec$regime, " (allowed: combined/low/high).")
  
  # ---- Identification (method/type) ----
  if (!is.null(spec$identification)) {
    id <- spec$identification
    id_method <- id$method %||% id$type
    if (is.null(id_method) || !nzchar(id_method))
      add_err("identification$method (or $type) is required.")
    if (!is.null(id$ordering) && !is.vector(id$ordering))
      add_err("identification$ordering must be a character vector.")
  }
  
  # ---- Scale (optional) ----
  if (!is.null(spec$scale)) {
    sc <- spec$scale
    if (!is.null(sc$by_sigma) && !is.logical(sc$by_sigma)) add_err("scale$by_sigma must be TRUE/FALSE.")
    if (!is.null(sc$sigma_source) && !sc$sigma_source %in% c("model","data"))
      add_wn("scale$sigma_source is usually 'model' or 'data'.")
  }
  
  # ---- Shocks ----
  if (!is.null(spec$shocks)) {
    if (!is.list(spec$shocks) && !is.numeric(spec$shocks))
      add_err("'shocks' must be a named list or named numeric vector.")
    if (is.null(names(spec$shocks)) || any(!nzchar(names(spec$shocks))))
      add_err("'shocks' must have non-empty names.")
    if (any(!is.finite(unlist(spec$shocks))))
      add_err("'shocks' values must be finite.")
  }
  
  # ---- Schedule (optional) ----
  if (!is.null(spec$schedule)) {
    if (!is.list(spec$schedule)) add_err("'schedule' must be a list of items.")
    for (i in seq_along(spec$schedule)) {
      s <- spec$schedule[[i]]
      if (is.null(s$impulse) && is.null(s$shock)) add_err("schedule[[",i,"]] needs 'impulse' (or 'shock').")
      if (!is.null(s$t_start) && !is.numeric(s$t_start)) add_err("schedule[[",i,"]] t_start must be numeric.")
      if (!is.null(s$t_end)   && !is.numeric(s$t_end))   add_err("schedule[[",i,"]] t_end must be numeric.")
      if (!is.null(s$size)    && !is.numeric(s$size))    add_err("schedule[[",i,"]] size must be numeric.")
    }
  }
  
  # ---- Responses include (optional) ----
  if (!is.null(spec$responses) && !is.null(spec$responses$include)) {
    inc <- spec$responses$include
    if (!is.vector(inc)) add_err("responses.include must be a character vector.")
  }
  
  # ---- Cross-check vs model_vars ----
  if (!is.null(model_vars)) {
    mv <- as.character(model_vars)
    
    # identification ordering
    ord <- (spec$identification %||% list())$ordering %||% character(0)
    if (length(ord)) {
      miss <- setdiff(ord, mv)
      if (length(miss)) add_err("identification$ordering has unknown variables: ", paste(miss, collapse=", "))
    }
    
    # shocks names
    if (!is.null(spec$shocks)) {
      shock_names <- names(spec$shocks)
      miss <- setdiff(shock_names, mv)
      if (length(miss)) add_err("shocks include unknown variables: ", paste(miss, collapse=", "))
    }
    
    # schedule impulses
    if (!is.null(spec$schedule)) {
      for (i in seq_along(spec$schedule)) {
        imp <- spec$schedule[[i]]$impulse %||% spec$schedule[[i]]$shock
        if (!is.null(imp) && !(imp %in% mv)) add_err("schedule[[",i,"]] impulse '", imp, "' not in model variables.")
      }
    }
    
    # responses include
    inc <- (spec$responses %||% list())$include %||% character(0)
    if (length(inc)) {
      miss <- setdiff(inc, mv)
      if (length(miss)) add_err("responses.include unknown variables: ", paste(miss, collapse=", "))
    }
  }
  
  if (length(errs)) stop(paste(errs, collapse = "\n"), call. = FALSE)
  if (length(wns))  message("[validate_scenario_spec] Warnings:\n- ", paste(wns, collapse = "\n- "))
  TRUE
}