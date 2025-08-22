test_that("IRF contract/schema is respected", {
  m <- build_mock_tvar()
  m  <- compute_tvar_irf(m, horizon = 4, n_draws = 128, seed = 42)
  ir <- m$irf
  
  expect_true(is.list(ir$settings))
  expect_true(is.list(ir$regimes$low))
  expect_true(is.list(ir$regimes$high))
  expect_true(is.numeric(ir$settings$horizon))
  expect_true(is.numeric(ir$settings$n_draws))
  expect_true(is.numeric(ir$settings$ci_level))
  expect_true(length(ir$settings$impulses) >= 1)
  expect_true(length(ir$settings$responses) >= 1)
  
  # impulses/responses align with data cols
  expect_setequal(ir$settings$impulses, colnames(m$regimes$low$Y))
  expect_setequal(ir$settings$responses, colnames(m$regimes$low$Y))
})
