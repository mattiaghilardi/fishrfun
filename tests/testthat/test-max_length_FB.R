test_that("We can retrieve max length", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  maxtl <- max_length_FB(df)
  expect_s3_class(maxtl, "data.frame")
  expect_type(maxtl$original_name, "character")
  expect_type(maxtl$MaxLengthTL, "double")

  maxl <- max_length_FB(df, length_type = "both")
  expect_equal(ncol(maxl), 3)
  expect_equal(colnames(maxl), c("original_name", "MaxLengthTL", "MaxLengthSL"))

})
