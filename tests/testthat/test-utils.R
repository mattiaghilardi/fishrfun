test_that("We can load ECoF database", {

  skip_if_offline()
  db <- load_ECoF_db()
  expect_type(db, "list")
  expect_equal(length(db), 2)

})

test_that("We can find the best match", {

  expect_equal(find_best_match("Seranidae",
                               c("Serranidae", "Siganidae", "Sparidae")),
               "Serranidae")

})

test_that("Argument checks work", {

  expect_error(check_string(1))
  expect_error(check_character(1))
  expect_error(check_logical(1))
  expect_error(check_list(1))
  expect_error(check_df(1))
  expect_error(check_equal_length(1:3, 1:5))
  expect_error(check_different_vars(1, 1))
  expect_error(check_version("FB", "23"))
  expect_error(check_names_arg(names = NULL))

  skip_if_offline()
  df <- check_names_arg(c("Dentex dentex", "Mullus sp.", "Balistidae spp."),
                        version = "latest")
  expect_s3_class(df, "data.frame")
})
