clean_vix <- function(df) {
  df <- df |> dplyr::rename_with(tolower)
  
  if (!"observation_date" %in% names(df) || !"vixcls" %in% names(df)) {
    stop("VIX file must contain 'observation_date' and 'vixcls'.")
  }
  
  df |>
    dplyr::rename(date = observation_date, value = vixcls) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(!is.na(value))
}