#' Get maximum length from FishBase
#'
#' @description
#' `max_length_FB` extracts the observed or estimated species maximum length
#' (total, standard, or both). It also works with taxa identified at different
#' taxonomic ranks. In this case it returns the maximum length among
#' all the species belonging to the rank.
#'
#' @inheritParams morphometric_traits_FB
#' @param length_type A string; the type of length to return.
#' "TL" (default) or "SL", for total and standard length respectively,
#' or "both" if both are to be returned
#'
#' @return A data frame with the provided name(s) and maximum length(s).
#'
#' @importFrom rfishbase estimate
#' @importFrom dplyr summarise across where all_of
#'
#' @inherit load_fish_taxonomy author
#'
#' @examplesIf interactive()
#' # List of taxa identified to species, genus or family level
#' species <- c("Dentex dentex", "Labrus sp.", "Scombridae spp.")
#' ranks <- c("species", "genus", "family")
#' taxa <- build_fish_taxonomy(species, ranks)
#' # Max total length of fishes identified to species, genus or family level
#' max_length_FB(taxa)
#' # Both max TL and SL
#' max_length_FB(taxa, length_type = "both")
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

  # Max length at higher ranks
  for (i in c("Genus", "Family", "Order", "Class")) {
    i_low <- tolower(i)
    if (i_low %in% names$id.rank) {

      # Keep max value for each genus
      maxL_i <- maxL_spe %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(i))) %>%
        dplyr::summarise(maxTL_i = base::max(MaxLengthTL, na.rm = TRUE),
                         maxSL_i = base::max(MaxLengthSL, na.rm = TRUE)) %>%
        dplyr::rename("{i_low}" := dplyr::all_of(i))

      # Join
      maxL <- maxL %>%
        dplyr::left_join(maxL_i, by = i_low) %>%
        dplyr::mutate(MaxLengthTL = ifelse(.data$id.rank == i_low,
                                           .data$maxTL_i,
                                           MaxLengthTL),
                      MaxLengthSL = ifelse(.data$id.rank == i_low,
                                           .data$maxSL_i,
                                           MaxLengthSL)) %>%
        dplyr::select(-c("maxTL_i", "maxSL_i"))
    }
  }

  # Out
  if (length_type == "both") length_type <- c("TL", "SL")
  maxL %>%
    dplyr::select(1, paste0("MaxLength", length_type)) %>%
    # Round max length
    dplyr::mutate(dplyr::across(dplyr::where(is.double),
                                ~ round(.x, 2)))

}
