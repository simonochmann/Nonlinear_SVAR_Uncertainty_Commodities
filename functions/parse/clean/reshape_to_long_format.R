#' Reshape Commodity Data to Long Format
#'
#' Converts wide-format commodity price data into tidy long format.
#' Returns one row per date–commodity observation. Suitable for ggplot2,
#' time series diagnostics, or returns computation.
#'
#' @param df A data.frame or tibble with column 'date' and wide price columns.
#' @param verbose Logical. If TRUE, prints reshaping diagnostics (default: TRUE).
#' @param drop_na Logical. If TRUE, drops rows with NA prices (default: TRUE).
#' @param return_metadata Logical. If TRUE, returns a list with `data` and `summary`.
#'
#' @return A tibble (or list) with columns: date, commodity, price.
#' @export
#'
#' @examples
#' df_long <- reshape_to_long_format(df)
#' out <- reshape_to_long_format(df, return_metadata = TRUE)

reshape_to_long_format <- function(df,
                                   verbose = TRUE,
                                   drop_na = TRUE,
                                   return_metadata = FALSE) {
  stopifnot("date" %in% names(df), is.data.frame(df))
  
  df <- tibble::as_tibble(df)
  df <- dplyr::arrange(df, date)
  
  # Validate column types
  numeric_cols <- sapply(df[-which(names(df) == "date")], is.numeric)
  if (!all(numeric_cols)) {
    warning("Some non-numeric columns detected in price columns. They will be ignored.")
    df <- df[, c("date", names(df)[-1][numeric_cols])]
  }
  
  # Reshape to long
  df_long <- tidyr::pivot_longer(
    data = df,
    cols = -date,
    names_to = "commodity",
    values_to = "price"
  )
  
  if (drop_na) df_long <- dplyr::filter(df_long, !is.na(price))
  
  # Verbose diagnostic
  summary_stats <- df_long %>%
    dplyr::summarise(
      commodities = dplyr::n_distinct(commodity),
      observations = dplyr::n(),
      start_date = min(date, na.rm = TRUE),
      end_date = max(date, na.rm = TRUE)
    )
  
  if (verbose) {
    cat("reshape_to_long_format(): Reshaping Summary\n")
    print(summary_stats)
  }
  
  if (return_metadata) {
    return(list(data = df_long, summary = summary_stats))
  } else {
    return(df_long)
  }
}
