test_that("We can load available releases", {

  skip_if_offline()
  expect_vector(available_ECoF_releases(), ptype = character())
  show_failure(expect_vector(available_ECoF_releases(), ptype = integer()))

  expect_vector(latest_release(db = "FB"), ptype = character())
  expect_equal(length(latest_release(db = "FB")), 1)
  expect_equal(length(latest_release(db = "ECoF")), 1)
})
