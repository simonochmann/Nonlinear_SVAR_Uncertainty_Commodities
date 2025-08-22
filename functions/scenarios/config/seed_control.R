# functions/scenarios/config/seed_control.R

#' Deterministic RNG control with metadata and optional parallel streams
#'
#' Features:
#' - Accepts integer seeds or string keys (hashed → integer vector).
#' - Supports RNGkind switching (Mersenne-Twister or L'Ecuyer-CMRG).
#' - Optional parallel-safe streams via L'Ecuyer-CMRG (reproducible substreams).
#' - Captures previous RNG state; provides a restore() closure.
#' - Returns rich metadata (digest, RNGkind, date, scenario key).
#'
#' @param seed Integer, character, or NULL. If character, a SHA-256 based seed is derived.
#' @param key Optional label (e.g., scenario name) stored in metadata; if seed is character and key is NULL, key <- seed.
#' @param rng_kind One of c("Mersenne-Twister","L'Ecuyer-CMRG"). Default "L'Ecuyer-CMRG".
#' @param normal_kind Passed to RNGkind(normal.kind=...). Default "Inversion".
#' @param sample_kind Passed to RNGkind(sample.kind=...) when available. Default "Rejection".
#' @param streams Integer number of parallel streams to prepare (>=1). Only used when rng_kind = "L'Ecuyer-CMRG".
#' @param stream_index Select a specific stream (1..streams). Ignored if streams=1.
#' @param verbose Logical; message() basic info.
#'
#' @return A list with:
#'   - meta: list with seed_input, seed_vec, rng_kind, normal_kind, sample_kind, stream_index,
#'           created_at, sha256_seed, sha256_state_before, sha256_state_after, key
#'   - restore: function() to restore the previous RNG state/kind
#'   - set_stream: function(i) to move to stream i (L'Ecuyer only)
#'
#' @examples
#' sc <- seed_control(seed = "Oil_Supply_Shock_x1.5sd", key = "scenario_oil", streams = 4, stream_index = 2)
#' runif(3)
#' sc$restore()
#'
#' @export
seed_control <- function(
    seed         = NULL,
    key          = NULL,
    rng_kind     = c("L'Ecuyer-CMRG","Mersenne-Twister"),
    normal_kind  = "Inversion",
    sample_kind  = "Rejection",
    streams      = 1L,
    stream_index = 1L,
    verbose      = FALSE
) {
  `%||%` <- function(x,y) if (is.null(x)) y else x
  rng_kind <- match.arg(rng_kind)
  
  # ---- capture prior state/kind ---------------------------------------------
  old_kind <- tryCatch(suppressWarnings(RNGkind()), error = function(e) NULL)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  sha_state_before <- .state_digest(old_seed)
  
  # ---- set RNGkind safely ----------------------------------------------------
  .set_rngkind_safe(rng_kind, normal_kind, sample_kind)
  
  # ---- derive seed vector ----------------------------------------------------
  if (is.null(seed)) {
    # Leave current state as-is but still record metadata
    if (verbose) message("[seed_control] No seed supplied; leaving RNG state unchanged.")
    seed_vec <- old_seed
  } else if (is.character(seed)) {
    key <- key %||% seed
    seed_vec <- .seed_from_string(seed, rng_kind = rng_kind)
  } else if (is.numeric(seed)) {
    seed_vec <- .seed_from_integer(seed, rng_kind = rng_kind)
  } else {
    stop("`seed` must be NULL, character, or numeric.")
  }
  
  # If a seed was provided, set it now
  if (!is.null(seed_vec)) {
    suppressWarnings(assign(".Random.seed", seed_vec, envir = .GlobalEnv))
  }
  
  # ---- L'Ecuyer parallel streams --------------------------------------------
  set_stream <- function(i) invisible(NULL)
  if (identical(rng_kind, "L'Ecuyer-CMRG") && (streams <- as.integer(streams)) > 1L) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available; cannot initialize multiple L'Ecuyer streams. Proceeding with single stream.")
      streams <- 1L
    } else {
      # advance to stream_index deterministically
      stream_index <- max(1L, as.integer(stream_index))
      for (k in seq_len(stream_index - 1L)) parallel::nextRNGStream()
      set_stream <- function(i) {
        i <- as.integer(i)
        if (i < 1L) stop("stream index must be >= 1")
        # reset to original seeded state then advance (i-1) times
        suppressWarnings(assign(".Random.seed", seed_vec, envir = .GlobalEnv))
        if (i > 1L) {
          for (k in seq_len(i - 1L)) parallel::nextRNGStream()
        }
        invisible(TRUE)
      }
    }
  }
  
  sha_state_after <- .state_digest(get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  
  # ---- build metadata --------------------------------------------------------
  meta <- list(
    key                  = key %||% NA_character_,
    seed_input           = if (is.null(seed)) NA else seed,
    seed_vec             = seed_vec,
    rng_kind             = rng_kind,
    normal_kind          = normal_kind,
    sample_kind          = .sample_kind_or_na(),
    streams              = as.integer(streams),
    stream_index         = as.integer(stream_index),
    created_at           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    sha256_seed          = if (is.null(seed)) NA_character_ else .seed_digest(seed, rng_kind),
    sha256_state_before  = sha_state_before,
    sha256_state_after   = sha_state_after
  )
  if (isTRUE(verbose)) message(sprintf("[seed_control] RNG=%s streams=%d idx=%d", rng_kind, meta$streams, meta$stream_index))
  
  # ---- restore closure -------------------------------------------------------
  restore <- function() {
    if (!is.null(old_kind)) {
      # old_kind is a length-3 vector: c(rng, normal, sample?) depending on R version
      .restore_rngkind(old_kind)
    }
    if (!is.null(old_seed)) {
      suppressWarnings(assign(".Random.seed", old_seed, envir = .GlobalEnv))
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      # if previously no seed existed, remove it to truly restore "no state"
      rm(".Random.seed", envir = .GlobalEnv)
    }
    invisible(TRUE)
  }
  
  structure(
    list(meta = meta, restore = restore, set_stream = set_stream),
    class = "rng_control"
  )
}

#' Run an expression inside a temporary RNG scope
#' Restores previous RNG state and kind afterwards.
#' @param ... expressions
#' @inheritParams seed_control
#' @return the value of the last expression
#' @export
rng_with <- function(seed = NULL, key = NULL, rng_kind = c("L'Ecuyer-CMRG","Mersenne-Twister"),
                     normal_kind = "Inversion", sample_kind = "Rejection",
                     streams = 1L, stream_index = 1L, verbose = FALSE, ...) {
  ctrl <- seed_control(seed, key, rng_kind, normal_kind, sample_kind, streams, stream_index, verbose)
  on.exit(ctrl$restore(), add = TRUE)
  force(list(...)) # evaluate promises
  eval.parent(substitute({ ... }))
}

# ---- helpers ----------------------------------------------------------------

# choose RNGkind safely across R versions
.set_rngkind_safe <- function(rng_kind, normal_kind, sample_kind) {
  # sample.kind is only available in R >= 3.6.0
  has_sample_kind <- "sample.kind" %in% names(formals(RNGkind))
  if (has_sample_kind) {
    RNGkind(kind = rng_kind, normal.kind = normal_kind, sample.kind = sample_kind)
  } else {
    RNGkind(kind = rng_kind, normal.kind = normal_kind)
  }
}

.restore_rngkind <- function(old_kind_vec) {
  # old_kind_vec like c(kind, normal, sample?) depending on R
  kind <- old_kind_vec[1]
  normal <- old_kind_vec[2] %||% NULL
  sample <- old_kind_vec[3] %||% NULL
  has_sample_kind <- "sample.kind" %in% names(formals(RNGkind))
  if (has_sample_kind) {
    RNGkind(kind = kind, normal.kind = normal, sample.kind = sample %||% "Rejection")
  } else {
    RNGkind(kind = kind, normal.kind = normal %||% "Inversion")
  }
}

.sample_kind_or_na <- function() {
  if ("sample.kind" %in% names(formals(RNGkind))) RNGkind()[3] else NA_character_
}

# derive seed vector from integer
.seed_from_integer <- function(seed, rng_kind = "L'Ecuyer-CMRG") {
  seed <- as.integer(round(seed))[1]
  if (identical(rng_kind, "L'Ecuyer-CMRG")) {
    # L'Ecuyer requires a 7-int seed; we derive via hash for stability
    .seed_7_from_any(seed)
  } else {
    set.seed(seed); get(".Random.seed", envir = .GlobalEnv)
  }
}

# derive seed vector from string deterministically
.seed_from_string <- function(s, rng_kind = "L'Ecuyer-CMRG") {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required for string seeds.")
  h <- digest::digest(s, algo = "sha256", serialize = FALSE)
  # convert hex → integers
  ints <- .hex_to_ints(h)
  if (identical(rng_kind, "L'Ecuyer-CMRG")) {
    .seed_7_from_any(ints)
  } else {
    # fold into a single 32-bit int for MT
    seed_int <- abs(as.integer(ints[1] %% .Machine$integer.max))
    .seed_from_integer(seed_int, rng_kind = "Mersenne-Twister")
  }
}

.seed_7_from_any <- function(x) {
  # produce a valid 7-int seed for L'Ecuyer-CMRG
  v <- as.integer(abs(as.numeric(x)))
  if (length(v) < 7) v <- rep(v, length.out = 7)
  v <- v[1:7]
  # ensure not all zeros and within 0..2^31-1
  v[v < 0] <- -v[v < 0]
  v <- v %% .Machine$integer.max
  if (all(v == 0L)) v[1] <- 12345L
  # set and return full state
  RNGkind("L'Ecuyer-CMRG")
  suppressWarnings(assign(".Random.seed", c(407L, v), envir = .GlobalEnv))
  get(".Random.seed", envir = .GlobalEnv)
}

.hex_to_ints <- function(hex) {
  # split hex string into 8-char chunks → 32-bit ints
  n <- nchar(hex)
  if (n %% 8 != 0) {
    hex <- paste0(hex, paste(rep("0", 8 - (n %% 8)), collapse = ""))
    n <- nchar(hex)
  }
  parts <- substring(hex, first = seq(1, n, by = 8), last = seq(8, n, by = 8))
  as.numeric(strtoi(parts, base = 16L))
}

.state_digest <- function(state) {
  if (is.null(state)) return(NA_character_)
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  digest::digest(state, algo = "sha256")
}

.seed_digest <- function(seed_input, rng_kind) {
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  digest::digest(list(seed = seed_input, rng_kind = rng_kind), algo = "sha256")
}
