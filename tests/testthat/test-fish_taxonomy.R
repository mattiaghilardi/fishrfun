test_that("We can load fish taxonomy", {

  skip_if_offline()
  df <- load_fish_taxonomy(db = "FB")
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 8)

  df <- load_fish_taxonomy(db = "ECoF")
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 6)

})
