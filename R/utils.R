#' Checks
#'
#' @importFrom rlang caller_arg caller_env is_string is_character is_logical
#' @importFrom cli cli_abort
#' @importFrom curl has_internet
#' @noRd
check_string <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!rlang::is_string(x)) {
    cli::cli_abort("{.arg {arg}} must be a string.", call = call)
  }
}

check_character <- function(x,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  if (!rlang::is_character(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a character vector.",
      "x" = "You've supplied a {.cls {class(x)}} vector."
    ), call = call)
  }
}

check_logical <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!rlang::is_logical(x)) {
    cli::cli_abort("{.arg {arg}} must be logical.", call = call)
  }
}

check_length <- function(x,
                         y,
                         arg1 = rlang::caller_arg(x),
                         arg2 = rlang::caller_arg(y),
                         call = rlang::caller_env()) {
  if (length(x) != length(y)) {
    cli::cli_abort("{.arg {arg1}} and {.arg {arg2}} must have the same length.",
                   call = call)
  }
}

check_version <- function(db = c("FB", "ECoF"),
                          version = "latest",
                          call = rlang::caller_env()) {
  if (version == "latest") {
    version <- latest_release(db)
  } else {
    releases <-
      if (db == "FB") {
        rfishbase::available_releases()
      } else if (db == "ECoF") {
        available_ECoF_releases()
      }
    version %in% releases ||
      cli::cli_abort("Version {version} is not available.
                     See rfishbase::available_releases() for FB
                     or available_ECoF_releases() for ECoF.",
                     call = call)
  }
  version
}

check_internet <- function(call = rlang::caller_env()) {
  if (!curl::has_internet()) {
    cli::cli_abort("No internet connection.", call = call)
  }
}

# Check the `names` argument of functions that retrieve data from FishBase
# Return the built taxonomy
check_names_arg <- function(names,
                            version,
                            arg = rlang::caller_arg(names),
                            call = rlang::caller_env()) {
  taxo_colnames <- c("id.rank", "species", "genus", "family", "order", "class")
  if(!is.data.frame(names) |
     !length(names(names)) == 7 |
     !all(colnames(names)[2:7] == taxo_colnames)
  ) {
    if (rlang::is_character(names)) {
      # Build taxonomy
      names <- build_fish_taxonomy(names = names,
                                   id.rank = NULL,
                                   check_names = TRUE,
                                   colname = "names",
                                   db = "FB",
                                   version = version)
    } else {
      cli::cli_abort("{.arg {arg}} must be a data frame created with
                     {.fn build_fish_taxonomy} or a character vector
                     of taxonomic names.",
                     call = call)
    }
  }
  names
}

#' Load Eschmeyer's Catalog of Fishes database
#'
#' @param version The database version to use
#'
#' @return The database
#' @noRd
load_ECoF_db <- memoise::memoise(
  function(version = latest_release("ECoF")) {
    check_internet()
    url_dir <- "https://raw.githubusercontent.com/mattiaghilardi/ECoFarchive/main/archive"
    url_file <- paste0(url_dir, "/ECoF_", version, ".rds")
    call = rlang::caller_env()
    tryCatch({
      con <- url(url_file)
      suppressWarnings(readRDS(con))},
      error = function(e) {
        cli::cli_abort(c(
          "!" = "Download of ECoF database failed with error:",
          "{e}"),
          call = call
        )
      },
      finally = close(con)
    )
  }
)

#' Find best match for misspelled scientific names
#'
#' @param name_errors A character vector of non valid names
#' @param valid_names A character vector of valid names
#' @param maxDist Maximum distance for approximate matching. See [stringdist::amatch()]
#' @param ... Other arguments passed to [stringdist::amatch()],
#' such as `method` (default to "osa")
#'
#' @importFrom stringdist amatch
#'
#' @return A character vector
#' @noRd
find_best_match <- function(name_errors, valid_names, maxDist = 3, ...) {
  # Note: default method may not always be the best
  # e.g. "Blenidae" would return "Belonidae" instead of "Blenniidae"
  # method = "lcs" (longest common substring) may be more appropriate for misspelled names
  valid_names[stringdist::amatch(name_errors,
                                 valid_names,
                                 maxDist = maxDist,
                                 ...)]
}
