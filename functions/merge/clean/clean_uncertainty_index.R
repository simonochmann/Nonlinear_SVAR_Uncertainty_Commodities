#' Clean Raw Uncertainty Index Data into Standard Format
#'
#' Cleans a raw uncertainty index file into a standardized two-column tibble
#' (`date`, `value`) depending on the `index_type`. Supported: "ciss", "vix", "vxo", "jln".
#'
#' @param df_raw A raw tibble loaded from CSV.
#' @param index_type One of: "ciss", "vix", "vxo", "jln".
#' @return A tibble with `date` and `value` columns, arranged by date.
#' @export
clean_uncertainty_index <- function(df_raw, index_type) {
  index_type <- tolower(index_type)
  
  cleaned_df <- switch(index_type,
                       "ciss" = clean_ciss(df_raw),
                       "vix"  = clean_vix(df_raw),
                       "vxo"  = clean_vxo(df_raw),
                       "jln"  = clean_jln(df_raw),
                       stop(glue::glue("Unsupported index_type '{index_type}'"))
  )
  
  cleaned_df <- cleaned_df |>
    dplyr::select(date, value) |>
    dplyr::arrange(date)
  
  if (nrow(cleaned_df) == 0) stop("Cleaning resulted in empty output.")
  if (!all(c("date", "value") %in% names(cleaned_df))) stop("Output must contain `date` and `value` columns.")
  
  attr(cleaned_df, "index_type") <- index_type
  attr(cleaned_df, "n_obs") <- nrow(cleaned_df)
  attr(cleaned_df, "date_range") <- range(cleaned_df$date)
  
  return(cleaned_df)
}
