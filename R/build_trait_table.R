#' Build a fish trait table
#'
#' @description
#' A wrapper function to build a table with multiple traits extracted from FishBase
#'
#' @inheritParams morphometric_traits_FB
#' @param fns List of `fishrfun` functions that return species-level traits
#' @param args List of same length as `fns`. Each element must be a list of
#' arguments, other than "names" and "FB_version", to be passed on to the
#' respective function in `fns`. The element must be an empty list if no
#' additional arguments are required. See *Examples*
#'
#' @return A data frame
#'
#' @inherit load_fish_taxonomy author
#'
#' @importFrom rlang is_function
#'
#' @examplesIf interactive()
#' # List of functions for the traits of interest
#' fns <- list(aspect_ratio_FB, max_length_FB, trophic_level_FB)
#' # Pass an additional argument to `max_length_FB` and `trophic_level_FB`
#' args <- list(list(), list(length_type = "both"), list(type = "food items"))
#' # List of taxa identified to species, genus or family level
#' species <- c("Boops boops", "Serranus sp.", "Sparidae spp.")
#' ranks <- c("species", "genus", "family")
#' taxa <- build_fish_taxonomy(species, ranks)
#' build_trait_table(taxa, fns, args)
#'
#' @export
build_trait_table <- function(names,
                              fns,
                              args = vector("list", length(fns)),
                              FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)
  check_list(fns)
  check_list(args)
  check_equal_length(args, fns)
  if (!all(purrr::map_lgl(fns, rlang::is_function))) {
    cli::cli_abort("All elements in {.arg fns} must be functions",
                   call = rlang::caller_env())
  }
  # if (!all(purrr::map_lgl(args, rlang::is_list))) {
  #   cli::cli_abort("All elements in {.arg args} must be lists",
  #                  call = rlang::caller_env())
  # }

  id <- colnames(names)[1]
  purrr::map2(
    fns,
    args,
    ~ do.call(.x, c(list(names = names, FB_version = FB_version), .y))
  ) %>%
    purrr::reduce(dplyr::left_join, by = id)
}

