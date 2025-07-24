#' Clean Commodity Price Data into Log Returns
#'
#' Converts a wide-format dataset of commodity prices into log return series.
#' Accepts either a CSV path or a `pink_sheet_raw` object. Handles frequency aggregation,
#' standardization, and output formatting.
#'
#' @param input Either a path to CSV or a pink_sheet_raw object.
#' @param date_column Name of the date column (default: "date").
#' @param frequency Aggregation: "monthly" (default), or "none".
#' @param return_type Output: "data.frame" (default), "xts", or "ts".
#' @param standardize Logical, whether to apply z-score standardization (default: TRUE).
#' @param drop_na Logical, drop NAs after differencing (default: TRUE).
#' @param verbose Print diagnostics (default: TRUE).
#'
#' @return S3 object of class `commodity_returns`, with elements:
#'   - data: log returns
#'   - meta: list with assets, dates, rows, columns
#'
#' @export
clean_commodity_returns <- function(input,
                                    date_column = "date",
                                    frequency = "monthly",
                                    return_type = "data.frame",
                                    standardize = TRUE,
                                    drop_na = TRUE,
                                    verbose = TRUE) {
  # Load input depending on class 
  if (is.character(input)) {
    if (!file.exists(input)) stop("File does not exist: ", input)
    raw_df <- readr::read_csv(input, show_col_types = FALSE)
    source_name <- basename(input)
  } else if (inherits(input, "pink_sheet_raw")) {
    raw_df <- input$data
    source_name <- input$meta$file
  } else {
    stop("Unsupported input: must be file path or pink_sheet_raw object.")
  }
  
  # Validate and order date column 
  if (!(date_column %in% names(raw_df))) stop("Date column not found: ", date_column)
  raw_df[[date_column]] <- lubridate::ymd(raw_df[[date_column]])
  raw_df <- raw_df[order(raw_df[[date_column]]), ]
  
  # Filter numeric columns only
  price_df <- raw_df %>%
    dplyr::select(where(is.numeric), all_of(date_column)) %>%
    dplyr::relocate(all_of(date_column))
  
  xts_prices <- xts::xts(price_df[, -1], order.by = price_df[[date_column]])
  
  # Frequency aggregation
  if (frequency == "monthly") {
    xts_prices <- xts::apply.monthly(xts_prices, zoo::last)
  }
  
  # Compute log returns 
  log_ret <- diff(log(xts_prices))
  if (drop_na) log_ret <- na.omit(log_ret)
  
  # Standardize if needed 
  if (standardize) {
    log_ret <- scale(log_ret)
  }
  
  # Return format handling 
  result_data <- switch(return_type,
                        "xts" = log_ret,
                        "ts" = ts(log_ret, start = c(lubridate::year(start(log_ret)),
                                                     lubridate::month(start(log_ret))), frequency = 12),
                        "data.frame" = data.frame(date = zoo::index(log_ret),
                                                  coredata(log_ret)),
                        stop("Invalid return_type: choose 'data.frame', 'xts', or 'ts'"))
  
  # Metadata for diagnostics/logging
  meta <- list(
    input_source = source_name,
    frequency = frequency,
    date_start = start(log_ret),
    date_end = end(log_ret),
    n_obs = nrow(log_ret),
    n_assets = ncol(log_ret),
    standardized = standardize,
    generated = Sys.time()
  )
  
  if (verbose) {
    message("clean_commodity_returns()")
    message(glue::glue("Source: {source_name}"))
    message(glue::glue("Assets: {meta$n_assets} | {meta$date_start} to {meta$date_end}"))
    message(glue::glue("Frequency: {meta$frequency} | Standardized: {meta$standardized}"))
    if (drop_na && any(is.na(log_ret))) {
      warning("NAs remain after differencing.")
    }
  }
  
  # Return structured object
  structure(
    list(data = result_data, meta = meta),
    class = "commodity_returns"
  )
}
