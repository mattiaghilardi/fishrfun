#' List available releases of the Eschmeyer's Catalog of Fishes
#'
#' Lists all available releases (year.month format).
#'
#' @return A character vector
#'
#' @inherit load_fish_taxonomy author
#'
#' @importFrom RCurl getURLContent
#' @importFrom stringi stri_extract_all_regex
#' @importFrom memoise memoise
#'
#' @export
#'
#' @examplesIf interactive()
#' available_ECoF_releases()
available_ECoF_releases <- memoise::memoise(
  function() {
    check_internet()
    url <- "https://github.com/mattiaghilardi/ECoFarchive/tree/main/archive"
    call = rlang::caller_env()
    tryCatch(
      RCurl::getURLContent(url) %>%
        stringi::stri_extract_all_regex('(?<="ECoF_).+?(?=.rds)') %>%
        unlist() %>%
        unique() %>%
        sort(decreasing = TRUE), # latest first
      error = function(e) {
        cli::cli_abort(c(
          "!" = "Listing of ECoF releases failed with error:",
          "{e}"),
          call = call
        )
      }
    )
  }
)

#' Get latest release of FishBase or ECoF
#'
#' @inheritParams load_fish_taxonomy
#' @importFrom rfishbase available_releases
#' @noRd
latest_release <- function(db = c("FB", "ECoF")) {
  if (db == "FB") rfishbase::available_releases()[1]
  else if (db == "ECoF") available_ECoF_releases()[1]
}
