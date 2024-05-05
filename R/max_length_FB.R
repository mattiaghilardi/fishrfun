#' Get maximum length from FishBase
#'
#' @description
#' `max_length_FB` extracts the observed or estimated species maximum length
#' (total, standard, or both). It also works with taxa identified at the
#' genus or family level. In this case it returns the maximum length among
#' all the species belonging to the genus or family.
#'
#' @inheritParams morphometric_traits_FB
#' @param length_type A string; the type of length to return.
#' "TL" (default) or "SL", for total and standard length respectively,
#' or "both" if both are to be returned
#'
#' @return A data frame with the provided name(s) and maximum length(s).
#'
#' @importFrom rfishbase estimate
#' @importFrom dplyr summarise across
#'
#' @inherit load_fish_taxonomy author
#'
#' @examplesIf interactive()
#' # Max total length of fishes identified to species, genus or family level
#' max_length_FB(c("Dentex dentex", "Scombridae spp.", "Diplodus sargus", "Labrus sp."))
#' # Both max TL and SL
#' max_length_FB(c("Pagellus acarne", "Mullus spp."), length_type = "both")
#'
#' @export
max_length_FB <- function(names,
                          length_type = c("TL", "SL", "both"),
                          FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)

  length_type <- rlang::arg_match(length_type)

  # Load taxonomy
  taxo <- load_fish_taxonomy("FB", version = version)

  # Get max length for all species
  maxL_spe <- taxo %>%
    dplyr::left_join(rfishbase::estimate(fields = c("SpecCode", "MaxLengthTL", "MaxLengthSL"),
                                         version = version),
                     by = "SpecCode")

  # Join
  maxL <- names %>%
    dplyr::left_join(maxL_spe %>%
                       dplyr::select("Species", "MaxLengthTL", "MaxLengthSL"),
                     by = c("species" = "Species"))

  # Max length at genus level
  if ("genus" %in% names$id.rank) {

    # Keep max value for each genus
    maxL_gen <- maxL_spe %>%
      dplyr::group_by(Genus) %>%
      dplyr::summarise(maxTL_g = base::max(MaxLengthTL, na.rm = TRUE),
                       maxSL_g = base::max(MaxLengthSL, na.rm = TRUE))

    # Join
    maxL <- maxL %>%
      dplyr::left_join(maxL_gen,
                       by = c("genus" = "Genus")) %>%
      dplyr::mutate(MaxLengthTL = ifelse(.data$id.rank == "genus",
                                         .data$maxTL_g,
                                         MaxLengthTL),
                    MaxLengthSL = ifelse(.data$id.rank == "genus",
                                         .data$maxSL_g,
                                         MaxLengthSL))
  }

  # Max length at family level
  if ("family" %in% names$id.rank) {

    # Keep max value for each family
    maxL_fam <- maxL_spe %>%
      dplyr::group_by(Family) %>%
      dplyr::summarise(maxTL_f = base::max(MaxLengthTL, na.rm = TRUE),
                       maxSL_f = base::max(MaxLengthSL, na.rm = TRUE))

    # Join
    maxL <- maxL %>%
      dplyr::left_join(maxL_fam,
                       by = c("family" = "Family")) %>%
      dplyr::mutate(MaxLengthTL = ifelse(.data$id.rank == "family",
                                         .data$maxTL_f,
                                         MaxLengthTL),
                    MaxLengthSL = ifelse(.data$id.rank == "family",
                                         .data$maxSL_f,
                                         MaxLengthSL))
  }

  # Out
  if (length_type == "both") length_type <- c("TL", "SL")
  maxL %>%
    dplyr::select(1, paste0("MaxLength", length_type)) %>%
    # Round max length
    dplyr::mutate(dplyr::across(is.double,
                                ~ round(.x, 2)))

}
