library(testthat)
library(here)
source(here("functions/merge/utils/standardize_index_name.R"))

test_that("standard index names are correctly mapped", {
  expect_equal(standardize_index_name("vix index"), "VIX")
  expect_equal(standardize_index_name("VIXINDEX"), "VIX")
  expect_equal(standardize_index_name("jlnum12m"), "JLN")
  expect_equal(standardize_index_name("JLNum3M"), "JLN")
  expect_equal(standardize_index_name("CISS_Index"), "CISS")
  expect_equal(standardize_index_name("ecbciss"), "CISS")
})
