#' Compute durations of each regime spell in a TVAR model
#'
#' @param model A TVAR model object with a `regime_path` vector and `metadata$dates`
#' @param verbose Logical, if TRUE prints summary to console
#' @return A list with three elements:
#'   - durations_tbl: Regime spells with start/end, duration, ID
#'   - summary_tbl: Summary stats by regime
#'   - regime_spell_vector: Vector of same length as regime_path mapping each obs to a spell ID
#' @export
compute_regime_durations <- function(model, verbose = TRUE) {
  stopifnot(!is.null(model$regime_path), "Model must contain a regime_path vector")
  stopifnot(!is.null(model$metadata$dates), "Model must contain metadata$dates")
  
  regime_vec <- model$regime_path
  dates_vec  <- model$metadata$dates
  rle_out    <- rle(regime_vec)
  
  spell_ids  <- seq_along(rle_out$lengths)
  regime_spell_vector <- rep(spell_ids, rle_out$lengths)
  
  start_idx <- cumsum(c(1, head(rle_out$lengths, -1)))
  end_idx   <- cumsum(rle_out$lengths)
  
  durations_tbl <- tibble::tibble(
    spell_id = spell_ids,
    regime   = rle_out$values,
    start_idx = start_idx,
    end_idx   = end_idx,
    start_date = dates_vec[start_idx],
    end_date   = dates_vec[end_idx],
    duration   = rle_out$lengths
  )
  
  summary_tbl <- durations_tbl |>
    dplyr::group_by(regime) |>
    dplyr::summarise(
      n_spells = dplyr::n(),
      mean_duration = mean(duration),
      median_duration = median(duration),
      max_duration = max(duration),
      .groups = "drop"
    )
  
  if (verbose) {
    cli::cli_rule("Regime Spell Summary")
    print(summary_tbl)
    cli::cli_rule("Recent Spells")
    print(utils::tail(durations_tbl, 5))
  }
  
  # inject into model for later use (only in #)
  # model$regime_durations <- list(
  #   durations_tbl = durations_tbl,
  #   summary_tbl = summary_tbl,
  #   spell_vector = regime_spell_vector
  # )
  
  return(list(
    durations_tbl = durations_tbl,
    summary_tbl = summary_tbl,
    regime_spell_vector = regime_spell_vector
  ))
}