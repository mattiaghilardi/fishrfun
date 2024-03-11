#' Checks
#'
#' @importFrom rlang caller_arg caller_env is_string is_character is_logical
#' @importFrom cli cli_abort
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

#' Load Eschmeyer's Catalog of Fishes database
#'
#' @param version The database version to use
#'
#' @importFrom cli cli_alert_warning
#'
#' @return The database
#' @noRd
load_ECoF_db <- function(version = latest_release("ECoF")) {
  url_dir <- "https://raw.githubusercontent.com/mattiaghilardi/ECoFarchive/main/archive"
  url_file <- paste0(url_dir, "/ECoF_", version, ".rds")
  con <- url(url_file)
  on.exit(close(con))
  db <- try(readRDS(con), silent = TRUE)
  if (inherits(db, "try-error")) {
    cli::cli_alert_warning("Failed to load ECoF database", class = "try-error")
    return(invisible(NULL))
  }
  db
}

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
