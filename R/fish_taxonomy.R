#' Load fish taxonomy
#'
#' @param db Either "FB" (FishBase; default) or "ECoF" (Eschmeyer's Catalog of Fishes)
#' @param version A string; the database version to use. Will default to the latest
#' available; see [rfishbase::available_releases()] for FishBase
#' and [available_ECoF_releases()] for the Eschmeyer's Catalog of Fishes
#'
#' @importFrom rfishbase load_taxa
#' @importFrom rlang is_null arg_match
#'
#' @return A data frame
#'
#' @author Mattia Ghilardi, \email{mattia.ghilardi91@@gmail.com}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_fish_taxonomy()
#' load_fish_taxonomy("ECoF")
#' }
load_fish_taxonomy <- function(db = c("FB", "ECoF"),
                               version = "latest") {

  db <- rlang::arg_match(db)
  check_string(version)
  version <- check_version(db, version)

  if (db == "FB") {
    as.data.frame(rfishbase::load_taxa(version = version))
  } else if (db == "ECoF") {
    db <- load_ECoF_db(version)
    if (!rlang::is_null(db)) db$taxonomy
  }
}
