test_that("We can create community composition matrices", {

  data <- tibble::tribble(
    ~site, ~species, ~abundance,
    "a", "sp1", 10,
    "a", "sp2", 8,
    "a", "sp3", 1,
    "b", "sp1", 8,
    "b", "sp2", 5,
  )
  df <- create_comm_matrix(data, "site", "species", NULL)
  expect_true(is.matrix(df))
  expect_equal(dim(df), c(2, 3))

  df <- create_comm_matrix(data, "site", "species", "abundance",
                           relative = TRUE, as_matrix = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(rowSums(df), c(a = 1, b = 1))

  # test that correctly throws errors
  expect_error(create_comm_matrix(data, "site", "species", "biomass"))
  expect_error(create_comm_matrix(data, "abundance", "site", "species"))
  expect_error(create_comm_matrix(data, "site", "abundance", "species"))
  expect_error(create_comm_matrix(data, "site", "species", "site"))
  # test that correctly throws message
  expect_message(create_comm_matrix(data, "site", "species",
                                    values_var = NULL, relative = TRUE),
                 "Relative values are not calculated if `values_var = NULL`")

})
