test_that("Validator warns on unstable dynamics", {
  m <- build_mock_tvar()
  A_unstable <- m$regimes$low$A
  A_unstable[1,1] <- 1.05  # push spectral radius > 1
  
  expect_warning(
    validate_tvar_coefficient_matrix(A_unstable, Sigma = m$regimes$low$Sigma),
    regexp = "Unstable VAR dynamics"
  )
  
  expect_true(
    validate_tvar_coefficient_matrix(m$regimes$low$A, Sigma = m$regimes$low$Sigma)
  )
})
