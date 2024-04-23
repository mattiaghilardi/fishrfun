test_that("We can load fish taxonomy", {

  skip_if_offline()
  df <- load_fish_taxonomy(db = "FB")
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 8)

  df <- load_fish_taxonomy(db = "ECoF")
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 7)

})

test_that("We can build fish taxonomy", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks = c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 7)

  df <- build_fish_taxonomy(species)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 7)
})
