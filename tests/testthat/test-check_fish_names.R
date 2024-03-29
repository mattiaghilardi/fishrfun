test_that("We can validate fish names in ECoF", {

  skip_if_offline()
  valid <- validate_names_ECoF("Dentex dentex")
  expect_vector(valid, ptype = character())
  expect_equal(length(valid), 1)

})


test_that("Name checking works with all databases", {

  skip_if_offline()

  expect_message(errors <- check_fish_names(c("Boops boops", "Sparus aurata")))
  expect_null(errors)

  errors <- check_fish_names(c("Scarus ghobban", "Ostracion cubicus"),
                                            db = "ECoF")
  expect_s3_class(errors, "data.frame")
  expect_equal(ncol(errors), 3)

  expect_message(errors <- check_fish_names_FTOL("Scaridae", rank = "Family"))
  expect_s3_class(errors, "data.frame")
  expect_equal(ncol(errors), 2)

})
