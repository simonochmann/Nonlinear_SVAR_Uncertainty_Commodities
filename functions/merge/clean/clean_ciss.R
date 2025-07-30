clean_ciss <- function(df) {
  df <- df |> dplyr::rename_with(tolower)
  
  if (all(c("date", "ciss_value") %in% names(df))) {
    return(
      df |>
        dplyr::mutate(date = as.Date(date), value = ciss_value) |>
        dplyr::select(date, value) |>
        dplyr::filter(!is.na(value)) |>
        tibble::as_tibble()   # <-- ensure output is a tibble
    )
  }
  
  value_col <- names(df)[grepl("ciss", names(df))][1]
  if (!"time_period" %in% names(df) || is.na(value_col)) {
    stop("CISS data must contain 'time_period' and a CISS value column.")
  }
  
  df |>
    dplyr::rename(date = time_period, value = !!rlang::sym(value_col)) |>
    dplyr::mutate(date = as.Date(date), value = as.numeric(value)) |>
    dplyr::select(date, value) |>
    dplyr::filter(!is.na(value)) |>
    tibble::as_tibble() 
}
