test_that("We can retrieve fish morphometry", {

  skip_if_offline()
  species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.")
  ranks <- c("species", "genus", "family")
  df <- build_fish_taxonomy(species, ranks)
  ar <- aspect_ratio_FB(df)
  expect_s3_class(ar, "data.frame")
  expect_equal(ncol(ar), 5)
  elon <- elongation_FB(df)
  expect_type(elon$elongation, "double")
  bd <- body_depth_FB(df)
  expect_type(bd$body_depth.rank, "character")

  eye <- morphometric_traits_FB(df, eye_diameter = ED/HL, return_pics = TRUE)
  expect_equal(ncol(eye), 6)
  expect_equal(colnames(eye),
               c("original_name", "eye_diameter",
                 "eye_diameter.sd", "eye_diameter.n",
                 "eye_diameter.rank", "eye_diameter.pics"))
  expect_type(eye$eye_diameter.pics, "list")

  # test that throws error if no traits are requested
  expect_error(morphometric_traits_FB(df), "No traits requested")

})
