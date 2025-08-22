# tests/testthat/test_regime_durations.R

suppressPackageStartupMessages({
  library(testthat)
  library(tibble)
  library(dplyr)
  library(lubridate)
  library(here)
})

# Source the function under test
source(here("functions/tvar/regime/compute_regime_durations.R"))

# Minimal mock model helper
mk_model <- function(regime = NULL, dates = NULL) {
  if (is.null(regime)) regime <- integer(0)
  if (is.null(dates))  dates  <- seq_len(length(regime))
  list(
    metadata = list(
      regime_index = regime,
      dates = dates
    )
  )
}

test_that("label canonicalization: numeric 1/2 → 'low'/'high' and spell stats correct", {
  regime <- c(1,1,1, 2,2, 1, 2,2,2)
  dates  <- as.Date("2000-01-01") + months(0:(length(regime)-1))
  m <- mk_model(regime, dates)
  
  res <- compute_regime_durations(m, verbose = FALSE)
  
  # contract basics
  expect_true(is.list(res))
  expect_true(all(c("durations_tbl","summary_tbl","regime_spell_vector") %in% names(res)))
  
  dt <- res$durations_tbl
  expect_s3_class(dt, "tbl_df")
  expect_true(all(c("spell_id","regime","start_idx","end_idx","start_date","end_date","duration") %in% names(dt)))
  expect_setequal(levels(dt$regime), c("low","high"))
  
  # check spell structure: 4 spells (3x low, 2x high, 1x low, 3x high)
  expect_equal(nrow(dt), 4)
  expect_equal(as.character(dt$regime), c("low","high","low","high"))
  expect_equal(dt$duration, c(3,2,1,3))
  
  # summary shares add to 1
  sm <- res$summary_tbl
  expect_equal(round(sum(sm$time_share), 6), 1)
})

test_that("NA handling: NA breaks spells and yields NA spell IDs at those positions", {
  regime <- c(1,1, NA, 2,2, NA, 1)
  dates  <- as.Date("2010-01-01") + months(0:(length(regime)-1))
  m <- mk_model(regime, dates)
  
  res <- compute_regime_durations(m, verbose = FALSE)
  
  # NA positions in spell vector remain NA
  expect_true(all(is.na(res$regime_spell_vector[which(is.na(regime))])))
  
  # durations sum equals count of non-NA observations
  total_non_na <- sum(!is.na(regime))
  expect_equal(sum(res$durations_tbl$duration), total_non_na)
  
  # spells should be: low(2), high(2), low(1) → 3 spells
  expect_equal(nrow(res$durations_tbl), 3)
  expect_equal(as.character(res$durations_tbl$regime), c("low","high","low"))
  expect_equal(res$durations_tbl$duration, c(2,2,1))
})

test_that("spell-ID alignment: constant within spells, length equals T", {
  regime <- c(1,1,1, 2,2, 2, 1,1)
  dates  <- seq_len(length(regime))
  m <- mk_model(regime, dates)
  res <- compute_regime_durations(m, verbose = FALSE)
  
  idvec <- res$regime_spell_vector
  expect_equal(length(idvec), length(regime))
  # IDs should be non-decreasing, with contiguous equal chunks
  diffs <- diff(idvec[!is.na(idvec)])
  expect_true(all(diffs >= 0))
  
  # where id changes, regime changes too
  changes <- which(c(FALSE, diff(idvec, lag = 1) != 0))
  if (length(changes)) {
    expect_true(all(regime[changes] != regime[pmax(1, changes - 1)], na.rm = TRUE))
  }
})

test_that("date alignment: start/end indices map to provided dates", {
  regime <- c(1,1, 2, 2,2)
  dates  <- as.Date("2020-01-01") + months(0:(length(regime)-1))
  m <- mk_model(regime, dates)
  res <- compute_regime_durations(m, verbose = FALSE)
  
  dt <- res$durations_tbl
  # first spell: idx 1..2 → dates 2020-01-01 .. 2020-02-01
  expect_equal(dt$start_idx[1], 1)
  expect_equal(dt$end_idx[1],   2)
  expect_equal(dt$start_date[1], as.Date("2020-01-01"))
  expect_equal(dt$end_date[1],   as.Date("2020-02-01"))
})

test_that("min_spell merges internal singletons into previous spell", {
  # singleton 'high' between 'low's; ensure it gets merged with min_spell=2
  regime <- c(1,1, 2, 1,1,1)
  dates  <- seq_len(length(regime))
  m <- mk_model(regime, dates)
  
  # direct override via regime_vector argument, min_spell=2
  res <- compute_regime_durations(m, regime_vector = regime, dates = dates,
                                  min_spell = 2L, verbose = FALSE)
  
  dt <- res$durations_tbl
  # Expect no spell with duration < 2 (except possibly the first spell if it were short; here it's not)
  expect_true(all(dt$duration >= 2))
  
  # With merging, we should have 2 spells: low (merged: 2+1 = 3), low (3)  → because the singleton 'high' merged into previous 'low'
  # Our implementation merges INTO PREVIOUS spell, preserving labels; so both spells end up "low".
  expect_equal(nrow(dt), 2)
  expect_equal(as.character(dt$regime), c("low","low"))
  expect_equal(dt$duration, c(3,3))
})

test_that("character labels ('low'/'high') are accepted and preserved", {
  regime <- c("low","low","high","high","low")
  dates  <- seq_len(length(regime))
  m <- mk_model(regime, dates)
  
  res <- compute_regime_durations(m, verbose = FALSE)
  dt <- res$durations_tbl
  
  expect_s3_class(dt$regime, "factor")
  expect_setequal(levels(dt$regime), c("low","high"))
  expect_equal(as.character(dt$regime), c("low","high","low"))
  expect_equal(dt$duration, c(2,2,1))
})

test_that("all-NA regime yields empty outputs gracefully", {
  regime <- c(NA, NA, NA)
  dates  <- seq_len(length(regime))
  m <- mk_model(regime, dates)
  
  res <- compute_regime_durations(m, verbose = FALSE)
  expect_equal(nrow(res$durations_tbl), 0)
  expect_equal(nrow(res$summary_tbl), 0)
  expect_true(all(is.na(res$regime_spell_vector)))
})
