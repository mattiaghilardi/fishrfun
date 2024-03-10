#' List available releases of the Eschmeyer's Catalog of Fishes
#'
#' Lists all available releases (year.month format).
#'
#' @return A character vector
#'
#' @inherit load_fish_taxonomy author
#'
#' @importFrom RCurl getURLContent
#' @importFrom stringr str_extract_all
#'
#' @export
#'
#' @examples
#' \dontrun{
#' available_ECoF_releases()
#' }
available_ECoF_releases <- function() {
  url <- "https://github.com/mattiaghilardi/ECoFarchive/tree/main/archive"
  filenames <- RCurl::getURLContent(url) |>
    stringr::str_extract_all('(?<="ECoF_).+?(?=.rds)')
  sort(filenames[[1]], decreasing = TRUE) # latest first
}

#' Get latest release of FishBase or ECoF
#'
#' @inheritParams load_fish_taxonomy
#' @importFrom rfishbase available_releases
#' @noRd
latest_release <- function(db = c("FB", "ECoF")) {
  if (db == "FB") rfishbase::available_releases()[1]
  else if (db == "ECoF") available_ECoF_releases()[1]
}
