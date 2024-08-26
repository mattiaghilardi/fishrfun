test_that("We can build tree from taxonomy", {

  taxa <- tibble::tribble(
    ~"species", ~"genus", ~"family", ~"order", ~"class",
    "sp1",      "gen1",   "fam1",    "ord1",   "cla1",
    "sp2",      "gen2",   "fam1",    "ord1",   "cla1",
    "sp3",      "gen3",   "fam2",    "ord1",   "cla1",
    "fam3 sp.", "",       "fam3",    "ord2",   "cla1",
    "sp5",      "gen5",   "fam4",    "ord2",   "cla1",
    "sp6",      "gen6",   "fam5",    "ord3",   "cla1",
    "sp7",      "gen7",   "fam6",    "ord4",   "cla1",
    "gen7 sp.", "gen7",   "fam6",    "ord4",   "cla1"
  )
  tree <- build_tree_from_taxonomy(taxa)
  expect_s3_class(tree, "phylo")
  expect_equal(tree$edge.length, rep(1, length(tree$edge.length)))

  # check that throws error if taxonomy not a data frame
  taxa <- as.matrix(taxa)
  expect_error(build_tree_from_taxonomy(taxa))

})
