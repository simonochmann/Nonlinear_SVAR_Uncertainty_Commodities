clean_vxo <- function(df) {
  df <- df |> dplyr::rename_with(tolower)
  
  if (!"observation_date" %in% names(df) || !"vxocls" %in% names(df)) {
    stop("VXO file must contain 'observation_date' and 'vxocls'.")
  }
  
  df |>
    dplyr::rename(date = observation_date, value = vxocls) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(!is.na(value))
}