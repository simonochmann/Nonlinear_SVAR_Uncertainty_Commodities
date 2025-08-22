test_that("IRF metadata logger writes JSON and Markdown with expected keys", {
  out <- tmp_outdir("logs")
  m <- build_mock_tvar()
  m <- compute_tvar_irf(m, horizon = 4, n_draws = 128, seed = 7)
  
  res <- log_tvar_irf_metadata(
    model = m, output_dir = out,
    log_file_json = "meta.json", log_file_md = "meta.md", verbose = FALSE
  )
  
  expect_true(file.exists(res$json_path))
  expect_true(file.exists(res$md_path))
  
  js <- jsonlite::read_json(res$json_path, simplifyVector = TRUE)
  expect_true("settings"  %in% names(js))
  expect_true("variables" %in% names(js))
  expect_true("regimes"   %in% names(js))
  expect_true("pairs"     %in% names(js))
  expect_true(nrow(js$pairs) >= 1)
  expect_true(all(c("impulse","response","pair") %in% colnames(js$pairs)))
})
