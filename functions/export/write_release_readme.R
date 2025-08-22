# functions/export/write_release_readme.R
#' Compose README_RELEASE.md using collected artifacts, with quick lists and hashes
write_release_readme <- function(
    release_dir,
    active_ix,
    stamp,
    art,
    appendix_info = NULL,
    bib_info = NULL
) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  
  have <- function(x) {
    if (is.null(x) || !length(x)) "(none)" else paste0("- ", paste(basename(x), collapse = "\n- "))
  }
  
  # ---- Manifest summary (robust) ---------------------------------------------
  man_path <- fs::path(release_dir, "MANIFEST.json")
  man_lines_vec <- character(0)
  if (file.exists(man_path)) {
    man <- tryCatch(
      jsonlite::read_json(man_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.data.frame(man) && nrow(man)) {
      if (!"bytes" %in% names(man) && "size" %in% names(man)) man$bytes <- man$size
      man$bytes_num <- suppressWarnings(as.numeric(man$bytes))
      man$file      <- as.character(man$file)
      ord <- order(man$bytes_num, decreasing = TRUE, na.last = TRUE)
      top <- head(ord, 8L)
      if (length(top)) {
        man_lines_vec <- paste0(
          "- ", man$file[top], " (",
          format(man$bytes_num[top], big.mark = ",", scientific = FALSE), " bytes)"
        )
      }
    }
  }
  
  appendix_lines <- if (!is.null(appendix_info) && length(appendix_info$outputs)) {
    have(appendix_info$outputs)
  } else "(none)"
  
  bib_line <- if (!is.null(bib_info) && length(bib_info$merged_bib)) {
    fs::path_file(bib_info$merged_bib)
  } else "(none)"
  
  header <- glue::glue(
    "# Nonlinear SVAR – Scenario & Decomposition Release

**Active uncertainty index:** {active_ix}  
**Created:** {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}  
**Stamp:** {stamp}

## What’s inside

**Tables**
{have(art$rel_tabs)}

**Data (CSV)**
{have(art$rel_csv)}

**Figures**
- Decomposition:
{have(art$rel_png_d)}

- Scenarios:
{have(art$rel_png_s)}

**Logs**
{have(art$rel_logs)}

**Environment**
- ENVIRONMENT.md
- MANIFEST.json

**Config snapshot**
{have(art$rel_renv)}

**Appendix (optional)**
{appendix_lines}

**Bibliography (optional)**
- {bib_line}

## Manifest (largest files)
{if (length(man_lines_vec) == 0) '(none)' else paste(man_lines_vec, collapse = '\n')}

## Reproduce (fast path)
")
  
  # Build the code block separately to avoid tricky quoting inside glue strings
  code_block <- paste0(
    "```r\n",
    "# 1) Run scenario simulation (fast mode)\n",
    "source('scripts/07_shock_scenario_simulations.R')\n\n",
    "# 2) Run forecast decomposition (fast mode)\n",
    "source('scripts/08_forecast_decomposition.R')\n\n",
    "# 3) Package and export\n",
    "source('scripts/09_package_export.R')\n",
    "```"
  )
  
  readme_txt <- paste0(header, "\n", code_block, "\n")
  
  out <- fs::path(release_dir, "README_RELEASE.md")
  dir.create(fs::path_dir(out), recursive = TRUE, showWarnings = FALSE)
  cat(readme_txt, file = out)
  message("[09] Wrote ", fs::path_rel(out))
  out
}
