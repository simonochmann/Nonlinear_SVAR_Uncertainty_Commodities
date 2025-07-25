#' Filter Commodities by Completeness 
#'
#' Filters a long-format commodity panel to retain only commodities with
#' at least `min_obs` non-missing price values. Also returns dropped commodities
#' and logs key filtering stats for reproducibility.
#'
#' @param df A data.frame or tibble with columns `commodity`, `date`, and `price`.
#' @param min_obs Integer. Minimum number of non-NA price values required to retain a series.
#' @param verbose Logical. Whether to print filtering summary.
#' @param return_metadata Logical. If TRUE, returns a list with both filtered data and diagnostics.
#'
#' @return Tibble (default) or named list (if `return_metadata = TRUE`) with:
#'   - $data: filtered data
#'   - $dropped: tibble of dropped commodities and counts
#'   - $summary: list with metadata
#' @export
filter_complete_series <- function(df, min_obs = 180, verbose = TRUE, return_metadata = FALSE) {
  stopifnot(is.data.frame(df))
  
  # ---- Input Checks ----
  required_cols <- c("commodity", "date", "price")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  if (!inherits(df$date, "Date")) stop("`date` column must be of class Date.")
  if (!is.numeric(df$price)) stop("`price` column must be numeric.")
  if (!is.numeric(min_obs) || min_obs <= 0) stop("`min_obs` must be a positive number.")
  
  # ---- Count Valid Observations ----
  counts <- df %>%
    dplyr::group_by(commodity) %>%
    dplyr::summarise(
      n_obs = sum(!is.na(price)),
      .groups = "drop"
    )
  
  # Filter
  keep <- counts %>% dplyr::filter(n_obs >= min_obs) %>% dplyr::pull(commodity)
  dropped <- counts %>% dplyr::filter(!commodity %in% keep)
  
  filtered <- df %>% dplyr::filter(commodity %in% keep)
  
  # Summary
  summary <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    total_commodities = nrow(counts),
    retained = length(keep),
    dropped = nrow(dropped),
    min_obs_threshold = min_obs,
    retained_pct = round(100 * length(keep) / nrow(counts), 1),
    date_range = range(df$date, na.rm = TRUE)
  )
  
  if (verbose) {
    message(glue::glue("
                       Retained {summary$retained}/{summary$total_commodities} commodities with ≥ {min_obs} non-NA prices."))
  }
  
  if (return_metadata) {
    return(list(
      data = filtered,
      dropped = dropped,
      summary = summary
    ))
  } else {
    return(filtered)
  }
}
