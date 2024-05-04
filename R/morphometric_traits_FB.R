#' Get morphometric traits from FishBase
#'
#' @description
#' `morphometric_traits_FB` extracts average morphometric traits at
#' the lowest available taxonomic rank, either species, genus, or family.
#' It works with taxa identified at different taxonomic ranks.
#' Some trait-specific functions are also provided:
#' - `aspect_ratio_FB`: returns the aspect ratio of caudal fin
#' - `elongation_FB`: returns the body elongation (i.e. standard length / body depth)
#' - `body_depth_FB`: returns the body depth (i.e. the inverse of elongation)
#'
#' @details
#' If you want a specific variable from the FishBase morphometric table,
#' use the expression "`user_trait_name = variable_name`". For instance,
#' if you want the aspect ratio of the caudal fin, you can use
#' "`aspect_ratio = AspectRatio`", where `aspect_ratio` is the trait name
#' that you will find in the output and `AspectRatio` is the variable
#' name in the FishBase morphometric table. This is exactly what
#' `aspect_ratio_FB` does.
#'
#' If you want a composite trait, use the expression "`user_trait_name = formula`",
#' where the formula must only use variable names present in the FishBase
#' morphometric table. For instance, if you want the eye diameter standardised
#' by head length (as presented in FishBase), use "`eye_diameter = ED/HL`",
#' where `ED` and `HL` are the variable names for eye diameter and head length
#' respectively.
#'
#' Multiple expressions can be used to extract multiple traits simultaneously.
#'
#' Please run `names(rfishbase::morphometrics())` to see all variable names in
#' the table.
#'
#' @param names A data frame created by [build_fish_taxonomy()] or a
#' character vector of taxonomic names to be passed on to [build_fish_taxonomy()]
#' @param ... Expressions in the form of `user_trait_name = variable_name`, or
#' `user_trait_name = formula`. See *Details*
#' @param FB_version A string; the FishBase version to use.
#' Will default to the latest available; see [rfishbase::available_releases()]
#'
#' @return A data frame with the provided name(s), and two columns for
#' each trait, the first with the trait values and the second with the
#' taxonomic rank at which the values are extracted.
#'
#' @importFrom rfishbase morphometrics
#' @importFrom rlang quos
#' @importFrom dplyr group_by ungroup ends_with
#' @importFrom rlang `!!!`
#'
#' @inherit load_fish_taxonomy author
#'
#' @examplesIf interactive()
#' # List of taxa identified to species, genus or family level
#' taxa <- c("Caesio cuning", "Acanthurus sp.", "Chaetodontidae spp.")
#' # Aspect ratio
#' aspect_ratio_FB(taxa)
#' # User-specified traits, e.g. eye diameter standardised by head length (ED/HL)
#' morphometric_traits_FB(taxa, eye_diameter = ED/HL)
#'
#' @name morphometric_traits_FB
#' @export
morphometric_traits_FB <- function(names,
                                   ...,
                                   FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)

  # Load taxonomy
  taxo <- load_fish_taxonomy("FB", version = version)

  # Morphometry table
  morph <- rfishbase::morphometrics(version = version)
  morph$SL <- as.numeric(morph$SL) # for version 3.1.9
  morph <- dplyr::left_join(taxo, morph) %>%
    suppressMessages()

  # Add trait(s)
  args <- rlang::quos(...)
  trait_names <- names(args)
  n_traits <- length(args)

  out <- names

  for (i in 1:n_traits) {
    morph2 <- morph %>%
      dplyr::mutate(!!! args[i]) %>%
      dplyr::rename("trait" = trait_names[i])

    # Get trait at the species, genus or family level for each species
    morph2 <- morph2 %>%
      dplyr::group_by(Species) %>%
      dplyr::mutate(trait_s = mean(trait, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Genus) %>%
      dplyr::mutate(trait_g = mean(trait_s, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Family) %>%
      dplyr::mutate(trait_f = mean(trait_s, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Species
    trait_spe <- morph2 %>%
      dplyr::select("Species", "trait_s", "trait_g", "trait_f") %>%
      dplyr::distinct() %>%
      # Retain the trait at the lowest possible taxonomic rank and add rank
      dplyr::mutate(
        trait_value = dplyr::case_when(
          !is.na(trait_s) ~ trait_s,
          is.na(trait_s) & !is.na(trait_g) ~ trait_g,
          is.na(trait_s) & is.na(trait_g) & !is.na(trait_f) ~ trait_f,
          is.na(trait_s) & is.na(trait_g) & is.na(trait_f) ~ NA
        ),
        trait_rank = dplyr::case_when(
          !is.na(trait_s) ~ "species",
          is.na(trait_s) & !is.na(trait_g) ~ "genus",
          is.na(trait_s) & is.na(trait_g) & !is.na(trait_f) ~ "family",
          is.na(trait_s) & is.na(trait_g) & is.na(trait_f) ~ NA
        )
      ) %>%
      dplyr::select("Species", "trait_value", "trait_rank")

    # Join
    out <- out %>%
      dplyr::left_join(trait_spe,
                       by = c("species" = "Species"))

    # Genus
    if ("genus" %in% out$id.rank) {

      trait_gen <- morph2 %>%
        dplyr::select("Genus", "trait_g", "trait_f") %>%
        dplyr::distinct() %>%
        # Retain the trait at the lowest possible taxonomic rank and add rank
        dplyr::mutate(
          trait_value = ifelse(!is.na(trait_g), trait_g, trait_f),
          trait_rank = dplyr::case_when(!is.na(trait_g) ~ "genus",
                                        is.na(trait_g) & !is.na(trait_f) ~ "family",
                                        is.na(trait_g) & is.na(trait_f) ~ NA)
          ) %>%
        dplyr::select("Genus", "trait_value", "trait_rank")

      # Join
      out <- out %>%
        dplyr::left_join(trait_gen,
                         by = c("genus" = "Genus"),
                         suffix = c("", ".g")) %>%
        dplyr::mutate(
          trait_value = ifelse(is.na(trait_value),
                               trait_value.g,
                               trait_value),
          trait_rank = ifelse(is.na(trait_rank),
                              trait_rank.g,
                              trait_rank))
    }

    # Family
    if ("family" %in% out$id.rank) {

      trait_fam <- morph2 %>%
        dplyr::select("Family", "trait_value" = "trait_f") %>%
        dplyr::distinct() %>%
        dplyr::mutate(trait_rank = ifelse(is.na(trait_value), NA, "family"))

      # Join
      out <- out %>%
        dplyr::left_join(trait_fam,
                         by = c("family" = "Family"),
                         suffix = c("", ".f")) %>%
        dplyr::mutate(
          trait_value = ifelse(is.na(trait_value),
                               trait_value.f,
                               trait_value),
          trait_rank = ifelse(is.na(trait_rank),
                              trait_rank.f,
                              trait_rank))
    }

    out <- out %>%
      dplyr::select(-c(dplyr::ends_with(c(".s", ".g", ".f")))) %>%
      dplyr::mutate(trait_value = round(trait_value, 2)) %>%
      dplyr::rename("{trait_names[1]}" := "trait_value",
                    "{paste0(trait_names[1], '.rank')}" := "trait_rank")
  }

  out %>% dplyr::select(-c(2:7))
}

#' @rdname morphometric_traits_FB
#' @export
aspect_ratio_FB <- function(names,
                            FB_version = latest_release("FB")) {
  morphometric_traits_FB(names,
                         aspect_ratio = AspectRatio,
                         FB_version = FB_version)
}

#' @rdname morphometric_traits_FB
#' @export
elongation_FB <- function(names,
                          FB_version = latest_release("FB")) {
  morphometric_traits_FB(names,
                         elongation = SL/BD,
                         FB_version = FB_version)
}

#' @rdname morphometric_traits_FB
#' @export
body_depth_FB <- function(names,
                          FB_version = latest_release("FB")) {
  morphometric_traits_FB(names,
                         body_depth = BD/SL,
                         FB_version = FB_version)
}
