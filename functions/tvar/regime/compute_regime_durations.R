#' Compute durations of each regime spell in a TVAR model
#'
#' Robust spell extractor that tolerates different schema variants, NA gaps, and
#' either numeric (1/2) or character regime encodings. Returns tidy tables and a
#' spell-id vector aligned to the input series.
#'
#' @param model   A TVAR-like list. Regime vector is searched in order:
#'                (i) `regime_vector` (if provided),
#'                (ii) `model$metadata$regime_index`,
#'                (iii) `model$regime_path`.
#' @param regime_vector Optional integer/character vector of regimes (overrides model).
#' @param dates         Optional vector of dates; defaults to `model$metadata$dates`
#'                      else `seq_along(regime)`.
#' @param inject        If TRUE, injects results into `model$metadata$regime_durations`.
#' @param min_spell     Minimum allowed spell length (shorter spells are merged left).
#'                      Use 1 for no merging.
#' @param verbose       Print console summary.
#'
#' @return A list with:
#'   - durations_tbl: tibble of spells {spell_id, regime, start_idx, end_idx, start_date, end_date, duration}
#'   - summary_tbl:   tibble by regime {n_spells, mean_duration, median_duration, sd_duration, min, max, time_share}
#'   - regime_spell_vector: integer vector (length T) mapping obs -> spell_id (NA preserved)
#'   - model (only if inject=TRUE): model with results attached
#'
#' @examples
#' # res <- compute_regime_durations(model, verbose = TRUE)
#'
#' @export
compute_regime_durations <- function(model,
                                     regime_vector = NULL,
                                     dates = NULL,
                                     inject = FALSE,
                                     min_spell = 1L,
                                     verbose = TRUE) {
  # fetch / validate regime vector 
  if (!is.null(regime_vector)) {
    regime <- regime_vector
  } else if (!is.null(model$metadata$regime_index)) {
    regime <- model$metadata$regime_index
  } else if (!is.null(model$regime_path)) {
    regime <- model$regime_path
  } else {
    stop("No regime vector found. Provide `regime_vector` or ensure model$metadata$regime_index / model$regime_path exists.")
  }
  
  if (!is.atomic(regime)) stop("Regime vector must be an atomic vector.")
  n <- length(regime)
  if (n < 1) stop("Regime vector is empty.")
  
  # dates
  if (!is.null(dates)) {
    idx_dates <- dates
  } else if (!is.null(model$metadata$dates)) {
    idx_dates <- model$metadata$dates
  } else {
    idx_dates <- seq_len(n)
  }
  if (length(idx_dates) != n) {
    warning("Length of `dates` does not match regime length; falling back to seq_len(n).")
    idx_dates <- seq_len(n)
  }
  
  # canonicalize regimes 
  # allow numeric {1,2} or character {"low","high"} etc.
  canonize <- function(v) {
    if (is.numeric(v)) {
      # Map 1->low, 2->high when possible
      u <- sort(unique(stats::na.omit(v)))
      if (all(u %in% c(1, 2))) {
        lab <- ifelse(v == 1, "low",
                      ifelse(v == 2, "high", NA_character_))
        return(list(val = v, lab = lab, is_numeric = TRUE))
      } else {
        # keep numeric labels as character strings
        return(list(val = v, lab = as.character(v), is_numeric = TRUE))
      }
    } else {
      # character / factor
      vv <- as.character(v)
      # try to coerce common names
      vv_low  <- tolower(vv) %in% c("low", "l", "1")
      vv_high <- tolower(vv) %in% c("high", "h", "2")
      lab <- ifelse(vv_low, "low", ifelse(vv_high, "high", vv))
      return(list(val = v, lab = lab, is_numeric = FALSE))
    }
  }
  can <- canonize(regime)
  regime_lab <- can$lab
  
  # handle NAs as breaks 
  # We compute spells on contiguous non-NA stretches. NA positions receive NA spell ids.
  is_na <- is.na(regime_lab)
  # indices of non-NA runs
  blocks <- if (all(is_na)) list() else {
    # positions where regime is not NA
    pos <- which(!is_na)
    # split pos into consecutive runs
    split(pos, cumsum(c(1, diff(pos) != 1)))
  }
  
  spell_id_vec <- rep(NA_integer_, n)
  spells_list  <- list()
  spell_counter <- 0L
  
  # helper to merge very short spells into the previous spell when min_spell > 1
  merge_short_spells <- function(vals, runs) {
    if (min_spell <= 1L || length(runs$lengths) <= 1L) return(runs)
    lens <- runs$lengths
    valsv <- runs$values
    i <- 1L
    while (i <= length(lens)) {
      if (lens[i] < min_spell && i > 1L) {
        # merge into previous
        lens[i - 1L] <- lens[i - 1L] + lens[i]
        lens <- lens[-i]
        valsv <- valsv[-i]
        next
      }
      i <- i + 1L
    }
    list(lengths = lens, values = valsv)
  }
  
  # build spells per non-NA block
  for (b in blocks) {
    v <- regime_lab[b]
    r <- rle(v)
    r <- merge_short_spells(v, r)
    
    # compute start/end indices within global index
    block_starts <- cumsum(c(b[1], head(r$lengths, -1)))
    block_starts <- c(b[1], head(b, -1)[cumsum(r$lengths)] - r$lengths + 1L)
    # More robust start/end computation:
    starts <- integer(length(r$lengths))
    ends   <- integer(length(r$lengths))
    cur <- b[1]
    for (j in seq_along(r$lengths)) {
      starts[j] <- cur
      ends[j]   <- cur + r$lengths[j] - 1L
      cur <- ends[j] + 1L
    }
    
    for (j in seq_along(r$lengths)) {
      spell_counter <- spell_counter + 1L
      idx_range <- starts[j]:ends[j]
      spell_id_vec[idx_range] <- spell_counter
      
      spells_list[[length(spells_list) + 1L]] <- tibble::tibble(
        spell_id   = spell_counter,
        regime     = r$values[j],
        start_idx  = starts[j],
        end_idx    = ends[j],
        start_date = idx_dates[starts[j]],
        end_date   = idx_dates[ends[j]],
        duration   = r$lengths[j]
      )
    }
  }
  
  durations_tbl <- if (length(spells_list)) dplyr::bind_rows(spells_list) else {
    tibble::tibble(spell_id = integer(), regime = character(),
                   start_idx = integer(), end_idx = integer(),
                   start_date = as.vector(idx_dates)[0],
                   end_date   = as.vector(idx_dates)[0],
                   duration   = integer())
  }
  
  # enforce ordered factor for regime if we have low/high
  if (nrow(durations_tbl)) {
    if (all(unique(durations_tbl$regime) %in% c("low", "high"))) {
      durations_tbl$regime <- factor(durations_tbl$regime, levels = c("low", "high"))
    } else {
      durations_tbl$regime <- factor(durations_tbl$regime)
    }
  }
  
  # summary by regime 
  if (nrow(durations_tbl)) {
    total_T <- sum(durations_tbl$duration)
    summary_tbl <- durations_tbl |>
      dplyr::group_by(regime) |>
      dplyr::summarise(
        n_spells        = dplyr::n(),
        mean_duration   = mean(duration),
        median_duration = stats::median(duration),
        sd_duration     = stats::sd(duration),
        min_duration    = min(duration),
        max_duration    = max(duration),
        time_share      = sum(duration) / total_T,
        .groups = "drop"
      )
  } else {
    summary_tbl <- tibble::tibble(
      regime = factor(character()), n_spells = integer(),
      mean_duration = numeric(), median_duration = numeric(),
      sd_duration = numeric(), min_duration = numeric(),
      max_duration = numeric(), time_share = numeric()
    )
  }
  
  if (isTRUE(verbose)) {
    cli::cli_rule("Regime Spell Summary")
    print(summary_tbl)
    if (nrow(durations_tbl)) {
      cli::cli_rule("Recent Spells")
      print(utils::tail(durations_tbl, 5))
    } else {
      message("No spells computed (all NA or empty).")
    }
  }
  
  out <- list(
    durations_tbl        = durations_tbl,
    summary_tbl          = summary_tbl,
    regime_spell_vector  = spell_id_vec
  )
  
  if (isTRUE(inject)) {
    model$metadata$regime_durations <- out
    out$model <- model
  }
  
  out
}