clean_jln <- function(df, series = "jlnum12m") {
  df <- janitor::clean_names(df)
  
  if (!"observation_date" %in% names(df) || !series %in% names(df)) {
    stop(glue::glue("JLN data must contain 'observation_date' and the selected series '{series}'"))
  }
  
  df |>
    dplyr::transmute(
      date = as.Date(observation_date),
      value = !!rlang::sym(series)
    ) |>
    dplyr::filter(!is.na(value))
}