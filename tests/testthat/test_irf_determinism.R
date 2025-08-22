test_that("IRFs are deterministic given a fixed seed", {
  m <- build_mock_tvar()
  m1 <- compute_tvar_irf(m, horizon = 6, n_draws = 256, seed = 999, save_dir = NULL)
  m2 <- compute_tvar_irf(m, horizon = 6, n_draws = 256, seed = 999, save_dir = NULL)
  
  s1_low  <- m1$irf$regimes$low$summary
  s2_low  <- m2$irf$regimes$low$summary
  s1_high <- m1$irf$regimes$high$summary
  s2_high <- m2$irf$regimes$high$summary
  
  expect_equal(s1_low,  s2_low,  tolerance = 0)
  expect_equal(s1_high, s2_high, tolerance = 0)
})
