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
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 7)

  df <- build_fish_taxonomy(species)
  expect_s3_class(df, "data.frame")
  expect_equal(colnames(df), c("original_name",
                               "id.rank",
                               "species",
                               "genus",
                               "family",
                               "order",
                               "class"))

  # test that throws error if rank names are wrong
  expect_error(build_fish_taxonomy(species,
                                   c("s", "g", "f")))
  # test that throws message if one or more names are wrong
  expect_message(df <- build_fish_taxonomy(c("Dentex dente", "Mullus sp.", "Balistidae spp.")),
                 "Building taxonomy only for valid names")
  # and wrong names are removed
  expect_equal(nrow(df), 2)

  # test that throws error if all names are wrong
  expect_error(build_fish_taxonomy("Dentex dente", "species"))
})
