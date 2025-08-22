#' Merge all .bib files under search_dir into release_dir/docs/refs.bib (dedup by key)
synthesize_bibliography <- function(search_dir, release_dir) {
  requireNamespace("fs", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  
  bibs <- fs::dir_ls(search_dir, glob = "*.bib", recurse = TRUE, type = "file")
  if (!length(bibs)) {
    message("[09] No .bib files found under: ", search_dir)
    return(list(merged_bib = NULL, sources = character(0)))
  }
  
  read_lines <- function(p) readLines(p, warn = FALSE, encoding = "UTF-8")
  extract_key <- function(line) {
    # tries to extract the BibTeX key from lines like: @article{Key2020,
    m <- regexpr("^\\s*@\\w+\\{\\s*([^,\\s]+)", line, perl = TRUE)
    if (m > 0) sub("^\\s*@\\w+\\{\\s*([^,\\s]+).*", "\\1", line) else NA_character_
  }
  
  seen <- new.env(parent = emptyenv())
  merged <- character()
  for (b in bibs) {
    lines <- read_lines(b)
    i <- 1
    while (i <= length(lines)) {
      if (grepl("^\\s*@\\w+\\{", lines[i])) {
        key <- extract_key(lines[i])
        # collect until closing brace of entry
        j <- i
        brace <- 0L
        entry <- character()
        repeat {
          l <- lines[j]
          brace <- brace + stringr::str_count(l, "\\{") - stringr::str_count(l, "\\}")
          entry <- c(entry, l)
          j <- j + 1L
          if (brace <= 0L || j > length(lines)) break
        }
        if (!exists(key, envir = seen, inherits = FALSE)) {
          assign(key, TRUE, envir = seen)
          merged <- c(merged, entry, "")  # blank line between entries
        }
        i <- j
      } else {
        i <- i + 1L
      }
    }
  }
  
  docs_dir <- fs::path(release_dir, "docs"); fs::dir_create(docs_dir)
  out_bib <- fs::path(docs_dir, "refs.bib")
  cat(paste(merged, collapse = "\n"), file = out_bib)
  message("[09] Wrote merged bibliography: ", fs::path_rel(out_bib))
  list(merged_bib = out_bib, sources = bibs)
}
