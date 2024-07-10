test_that("We can build a trait table", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  fns <- list(aspect_ratio_FB, max_length_FB)
  args <- list(list(), list(length_type = "both"))
  traits <- build_trait_table(df, fns, args)
  expect_s3_class(traits, "data.frame")
  expect_equal(ncol(traits), 7)

  # test that throws error if some elements in fns are not functions
  fns[[2]] <- 1
  expect_error(build_trait_table(taxa, fns, args))

})
