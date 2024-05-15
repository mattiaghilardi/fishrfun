test_that("We can retrieve fish morphometry", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks = c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  ar <- aspect_ratio_FB(df)
  expect_s3_class(ar, "data.frame")
  expect_equal(ncol(ar), 3)

  eye <- morphometric_traits_FB(df, eye_diameter = ED/HL)
  expect_s3_class(eye, "data.frame")
  expect_equal(colnames(eye), c("original_name", "eye_diameter", "eye_diameter.rank"))
})
