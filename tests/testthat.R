library(testthat)
test_check <- function(pkg = NULL) testthat::test_dir("tests/testthat", reporter = "summary")
test_check()
