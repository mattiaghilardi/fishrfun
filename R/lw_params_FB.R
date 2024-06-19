#' Get length-weight parameters from FishBase
#'
#' @description
#' `lw_params_FB` extracts the *a* and *b* parameters of the length-weight
#' relationship (\eqn{W = a * L^b}) as estimated by
#' [Froese et al. 2014](https://doi.org/10.1111/jai.12299).
#' It works with taxa identified at different taxonomic ranks.
#'
#' @inheritParams morphometric_traits_FB
#'
#' @return A data frame with the provided name(s), means and standard deviations
#' of *a* and *b*, and the method used. Further information about the applied
#' method can be found in the [paper](https://doi.org/10.1111/jai.12299) and
#' [here](https://fishbase.se/fishonline/english/fol_fishbasegoesfishbayes.htm).
#'
#' @inherit load_fish_taxonomy author
#'
#' @references
#' Froese R., Thorson J. T. and Reyes R. B. Jr. (2014) A Bayesian approach for
#' estimating length-weight relationships in fishes. J Appl Ichthyol, 30(1):78-85
#' <https://doi.org/10.1111/jai.12299>
#'
#' @examplesIf interactive()
#' # List of taxa identified to species, genus or family level
#' species <- c("Dentex dentex", "Labrus sp.", "Scombridae spp.")
#' ranks <- c("species", "genus", "family")
#' taxa <- build_fish_taxonomy(species, ranks)
#' #  of fishes identified to species, genus or family level
#' lw_params_FB(taxa)
#'
#' @export
lw_params_FB <- function(names,
                         FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)

  # Load taxonomy
  taxo <- load_fish_taxonomy("FB", version = version)

  # Get lw params for all species
  ab_spe <- taxo %>%
    dplyr::left_join(rfishbase::estimate(fields = c("SpecCode",
                                                    "a", "sd_log10a",
                                                    "b", "sd_b",
                                                    "Method_ab"),
                                         version = version),
                     by = "SpecCode")

  # Join
  ab <- names %>%
    dplyr::left_join(
      ab_spe %>%
        dplyr::select("Species", "a", "sd_log10a", "b", "sd_b", "Method_ab"),
      by = c("species" = "Species"))

  # LW params at higher ranks

  ### The following code retrieves mean values for each taxonomic group.
  ### However, this is suboptimal because many values are already estimated
  ### at the genus or family level based on body shape.
  ### I haven't find any solution to directly get model estimates for a taxonomic group.

  for (i in c("Genus", "Family", "Order", "Class")) {
    i_low <- tolower(i)
    if (i_low %in% names$id.rank) {

      # Get mean value for each taxonomic group
      ab_i <- ab_spe %>%
        dplyr::filter(!is.na(.data$a)) %>% # need to remove NAs to get correct n
        dplyr::group_by(dplyr::across(dplyr::all_of(i))) %>%
        dplyr::summarise(n = dplyr::n(),
                         a_i = mean(.data$a, na.rm = TRUE),
                         sd_log10a_i = sqrt(sum(.data$sd_log10a^2, na.rm = TRUE) / .data$n),
                         b_i = mean(.data$b, na.rm = TRUE),
                         sd_b_i = sqrt(sum(.data$sd_b^2, na.rm = TRUE) / .data$n),
                         Method_ab_i = paste("Average parameters based on data from",
                                             .data$n, "species within this", i_low)) %>%
        dplyr::rename("{i_low}" := dplyr::all_of(i)) %>%
        dplyr::select(-"n")

      # Join
      ab <- ab %>%
        dplyr::left_join(ab_i, by = i_low) %>%
        dplyr::mutate(a = ifelse(.data$id.rank == i_low,
                                 .data$a_i,
                                 .data$a),
                      sd_log10a = ifelse(.data$id.rank == i_low,
                                         .data$sd_log10a_i,
                                         .data$sd_log10a),
                      b = ifelse(.data$id.rank == i_low,
                                 .data$b_i,
                                 .data$b),
                      sd_b = ifelse(.data$id.rank == i_low,
                                    .data$sd_b_i,
                                    .data$sd_b),
                      Method_ab = ifelse(.data$id.rank == i_low,
                                         .data$Method_ab_i,
                                         .data$Method_ab)) %>%
        dplyr::select(-dplyr::ends_with("_i"))
    }
  }

  # Out
  ab %>%
    dplyr::select(1, "a", "sd_log10a", "b", "sd_b", "method_ab" = "Method_ab")

}
