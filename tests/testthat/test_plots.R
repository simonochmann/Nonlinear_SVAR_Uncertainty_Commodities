test_that("IRF regime plot renders and saves", {
  out <- tmp_outdir("plots")
  m <- build_mock_tvar()
  m <- compute_tvar_irf(m, horizon = 6, n_draws = 128, seed = 123)
  
  pth <- file.path(out, "irf_panel.png")
  p <- plot_tvar_irf_regimes(
    model = m, impulse = "oil", response = "oil",
    save_path = pth, verbose = FALSE
  )
  expect_true(inherits(p, "ggplot"))
  expect_true(file.exists(pth))
  expect_gt(file.size(pth), 1000)  # > 1KB: sanity check non-empty
})
