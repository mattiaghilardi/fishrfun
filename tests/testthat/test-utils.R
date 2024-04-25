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
