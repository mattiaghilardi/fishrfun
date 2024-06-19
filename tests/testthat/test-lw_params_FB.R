test_that("We can retrieve lw parametersh", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  lw <- lw_params_FB(df)
  expect_s3_class(lw, "data.frame")
  expect_equal(ncol(lw), 6)
  expect_equal(colnames(lw),
               c("original_name", "a", "sd_log10a", "b", "sd_b", "method_ab"))

})

