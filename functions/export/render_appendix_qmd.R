#' Render a Quarto (or R Markdown) appendix and copy outputs into release_dir/docs
render_appendix_qmd <- function(qmd_path, release_dir) {
  requireNamespace("fs", quietly = TRUE)
  docs_dir <- fs::path(release_dir, "docs")
  fs::dir_create(docs_dir)
  
  out_files <- character(0)
  # Prefer quarto if available
  has_quarto <- requireNamespace("quarto", quietly = TRUE) && !is.na(quarto::quarto_path())
  if (has_quarto) {
    message("[09] Rendering with Quarto: ", qmd_path)
    quarto::quarto_render(input = qmd_path, output_dir = docs_dir, quiet = TRUE)
    # collect top-level outputs (html/pdf) created in docs_dir
    out_files <- fs::dir_ls(docs_dir, type = "file", recurse = FALSE)
  } else if (grepl("\\.Rmd$", qmd_path, ignore.case = TRUE) && requireNamespace("rmarkdown", quietly = TRUE)) {
    message("[09] Rendering with rmarkdown: ", qmd_path)
    of <- rmarkdown::render(qmd_path, output_dir = docs_dir, quiet = TRUE)
    out_files <- c(out_files, of)
  } else {
    message("[09] Skipping appendix render (Quarto/Rmd not available).")
  }
  
  list(docs_dir = docs_dir, outputs = out_files)
}
