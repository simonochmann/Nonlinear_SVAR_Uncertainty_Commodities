#' Standardize Raw Index Name to Canonical Form
#'
#' @param raw_name A single character string (raw index name).
#' @param to_upper Logical. Convert output to uppercase? (default = TRUE)
#' @return Canonical name if recognized; raw input otherwise.
#' @export
standardize_index_name <- function(raw_name, to_upper = TRUE) {
  stopifnot(is.character(raw_name), length(raw_name) == 1)
  
  normalize <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]", "", x)
    trimws(x)
  }
  
  raw_variants <- c(
    # VIX → 4
    "vix", "vixindex", "vix_index", "vix index",
    # VXO → 4
    "vxo", "vxoindex", "vxo_index", "vxo index",
    # JLN → 7
    "jln", "jlnindex", "jln_index", "jln index",
    "jlnum1m", "jlnum3m", "jlnum12m",
    # CISS → 6
    "ciss", "cissindex", "ciss_index", "ciss index",
    "cissindexecb", "ecbciss",
    # EPU → 4
    "eueconomicpolicyuncertainty", "epu", "epueu", "eupolicyuncertainty"
  )
  
  canonical <- c(
    rep("VIX", 4),
    rep("VXO", 4),
    rep("JLN", 7),      
    rep("CISS", 6),
    rep("EPU", 4)
  )
  
  stopifnot(length(raw_variants) == length(canonical))
  
  name_map <- setNames(canonical, normalize(raw_variants))
  key <- normalize(raw_name)
  
  if (key %in% names(name_map)) {
    out <- name_map[[key]]
  } else {
    warning(sprintf("Unknown index name: '%s'. Returning raw input.", raw_name))
    out <- raw_name
  }
  
  if (!to_upper) out <- tolower(out)
  unname(out)
}
