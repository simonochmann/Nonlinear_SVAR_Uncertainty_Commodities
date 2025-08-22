# functions/scenarios/config/load_scenario_yaml.R
`%||%` <- function(x, y) if (is.null(x)) y else x
.is_abs <- function(p) grepl("^(/|[A-Za-z]:[/\\]|\\\\\\\\)", p)

.load_yaml_with_context <- function(path) {
  # Try parsing; if it fails, show a code frame (±3 lines) with a caret
  tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      msg <- conditionMessage(e)
      lines <- try(readLines(path, warn = FALSE), silent = TRUE)
      if (!inherits(lines, "try-error")) {
        m <- regexpr("line ([0-9]+), column ([0-9]+)", msg, perl = TRUE)
        if (m > 0) {
          nums <- regmatches(msg, m)
          ln <- as.integer(sub(".*line ([0-9]+).*", "\\1", nums))
          cn <- as.integer(sub(".*column ([0-9]+).*", "\\1", nums))
          from <- max(1L, ln - 3L); to <- min(length(lines), ln + 3L)
          snippet <- paste0(
            "\n--- YAML context (lines ", from, ":", to, ") ---\n",
            paste(sprintf("%5d | %s", seq(from, to), lines[from:to]), collapse = "\n"),
            "\n", sprintf("%5s   %s^", "", paste0(rep(" ", cn - 1L), collapse = "")), "\n",
            "-----------------------------------------------\n"
          )
          msg <- paste0(msg, snippet)
        }
      }
      stop(msg, call. = FALSE)
    }
  )
}

#' Load a scenario YAML and attach metadata
#' @param path path to YAML (relative to project root OK)
#' @param root_dir project root (default here::here())
#' @return list spec with attr(meta = list(file_abs, file_rel))
load_scenario_yaml <- function(path, root_dir = here::here()) {
  if (is.null(path) || !nzchar(path)) stop("Empty scenario path.")
  root_dir <- root_dir %||% here::here()
  
  p_abs <- if (.is_abs(path)) path else file.path(root_dir, path)
  p_abs <- normalizePath(p_abs, winslash = "/", mustWork = FALSE)
  if (!file.exists(p_abs)) stop("Scenario YAML not found: ", p_abs)
  
  spec <- .load_yaml_with_context(p_abs)
  if (is.null(spec) || !is.list(spec)) stop("YAML didn't parse into a list: ", p_abs)
  
  if (is.null(spec$name) || !nzchar(spec$name)) {
    spec$name <- tools::file_path_sans_ext(basename(p_abs))
  }
  if (!is.null(spec$horizon)) spec$horizon <- as.integer(spec$horizon[1])
  
  meta <- list(
    file_abs = p_abs,
    file_rel = tryCatch(fs::path_rel(p_abs, start = root_dir), error = function(e) basename(p_abs))
  )
  attr(spec, "meta") <- meta
  spec
}
