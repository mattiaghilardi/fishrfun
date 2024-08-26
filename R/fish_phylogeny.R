#' Convert taxonomy into a phylogenetic tree
#'
#' @description
#' `build_tree_from_taxonomy` is a wrapper around [`ape::as.phylo.formula()`]
#' and converts taxonomic hierarchy into a phylogenetic tree.
#' Following [Thorson et al. 2023](https://doi.org/10.1111/2041-210X.14076),
#' all edge lengths are set to 1.
#'
#' @param taxonomy A data frame with one column for each taxonomic variable in `formula`
#' @param formula A right-side formula describing the taxonomic relationship.
#' See [`ape::as.phylo.formula()`] for details
#'
#' @return An object of class phylo
#'
#' @inherit load_fish_taxonomy author
#'
#' @importFrom ape as.phylo
#' @importFrom stringi stri_split
#'
#' @export
#'
#' @references
#' Thorson J. T. et al. (2023) Identifying direct and indirect associations
#' among traits by merging phylogenetic comparative methods and structural
#' equation models. Methods Ecol Evol, 14(5): 1259-1275.
#' <https://doi.org/10.1111/2041-210X.14076>
#'
#' @examples
#' taxa <- tibble::tribble(
#'   ~"species", ~"genus", ~"family", ~"order", ~"class",
#'   "sp1",      "gen1",   "fam1",    "ord1",   "cla1",
#'   "sp2",      "gen2",   "fam1",    "ord1",   "cla1",
#'   "sp3",      "gen3",   "fam2",    "ord1",   "cla1",
#'   "fam3 sp.", "",       "fam3",    "ord2",   "cla1",
#'   "sp5",      "gen5",   "fam4",    "ord2",   "cla1",
#'   "sp6",      "gen6",   "fam5",    "ord3",   "cla1",
#'   "sp7",      "gen7",   "fam6",    "ord4",   "cla1",
#'   "gen7 sp.", "gen7",   "fam6",    "ord4",   "cla1"
#' )
#' tree <- build_tree_from_taxonomy(taxa)
#' tree
#' plot(tree)
build_tree_from_taxonomy <- function(taxonomy,
                                     formula = ~class/order/family/genus/species) {

  # check that all variables in formula are present in taxonomy
  taxo_colnames <- stringi::stri_split(as.character(formula), fixed = "/")[[2]]
  if(!is.data.frame(taxonomy) |
     !all(taxo_colnames %in% colnames(taxonomy))
  ) {
    cli::cli_abort("{.arg taxonomy} must be a data frame with one column
                   for each taxonomic variable in {.arg formula}:
                   {.val {taxo_colnames}}.",
                   call = rlang::caller_env())
  }

  # replace potential NAs with empty character and change to factor
  df <- taxonomy %>%
    dplyr::select(dplyr::all_of(taxo_colnames)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~tidyr::replace_na(.x, "") %>%
                                  as.factor()))

  # make tree
  tree <- ape::as.phylo(formula, data = df, collapse = FALSE)
  tree$edge.length <- rep(1, nrow(tree$edge))
  tree
}
