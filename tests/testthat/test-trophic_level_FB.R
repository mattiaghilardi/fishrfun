test_that("We can retrieve trophic level", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  troph <- trophic_level_FB(df)
  expect_s3_class(troph, "data.frame")
  expect_equal(ncol(troph), 5)
  expect_equal(colnames(troph), c("original_name",
                                  "trophic_level",
                                  "trophic_level.se",
                                  "trophic_level.remark",
                                  "trophic_level.rank"))

  troph_food <- trophic_level_FB(df, type = "food items")
  expect_type(troph_food$trophic_level, "double")
  expect_type(troph_food$trophic_level, "double")
  expect_type(troph_food$trophic_level.remark, "character")

})
