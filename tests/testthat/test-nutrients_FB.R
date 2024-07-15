test_that("We can retrieve nutrients", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  nut <- nutrients_FB(df)
  expect_s3_class(nut, "data.frame")
  expect_equal(ncol(nut), 24)
  expect_equal(colnames(nut)[23:24], c("nutrients.rank",
                                       "nutrients.method"))
  expect_type(nut$Calcium, "double")
  expect_type(nut$nutrients.rank, "character")
  expect_type(nut$nutrients.method, "character")

})
