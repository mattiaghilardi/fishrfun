#' Get morphometric traits from FishBase
#'
#' @description
#' `morphometric_traits_FB` is a general function to extract average
#' morphometric traits at the lowest available taxonomic rank.
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
#' `aspect_ratio_FB()` does.
#'
#' If you want a composite trait, use the expression "`user_trait_name = formula`",
#' where the formula must only use variable names present in the FishBase
#' morphometric table. For instance, if you want the eye diameter standardised
#' by head length (as presented in FishBase), use "`eye_diameter = ED/HL`",
#' where `ED` and `HL` are the variable names for eye diameter and head length,
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
#' @param return_pics Logical; if the names of pictures used to extract traits
#' should be returned. This might be useful if you find some issues with the
#' returned trait values and would like to check the pictures, otherwise it
#' slows down execution. Default to FALSE
#' @param FB_version A string; the FishBase version to use.
#' Will default to the latest available; see [rfishbase::available_releases()]
#'
#' @return A data frame with the provided name(s), and four columns for each
#' trait, or five if `return_pics = TRUE`. The first column reports the
#' average trait values, while the other columns include the standard
#' deviation ("sd"), the number of individuals or species (for taxa identified
#' at the species level or at higher taxonomic ranks, respectively)
#' with data ("n"), the taxonomic rank at which the values are extracted
#' ("rank"), and, if `return_pics = TRUE`, the picture names ("pics") as a list.
#'
#' @importFrom rfishbase morphometrics
#' @importFrom rlang quos `!!!`
#' @importFrom dplyr group_by ungroup rowwise
#' @importFrom stats sd
#'
#' @inherit load_fish_taxonomy author
#'
#' @examplesIf interactive()
#' # List of taxa identified to species, genus or family level
#' species <- c("Caesio cuning", "Acanthurus sp.", "Chaetodontidae spp.")
#' ranks <- c("species", "genus", "family")
#' taxa <- build_fish_taxonomy(species, ranks)
#' # Aspect ratio
#' aspect_ratio_FB(taxa)
#' # User-specified traits, e.g. eye diameter standardised by head length (ED/HL)
#' morphometric_traits_FB(taxa, eye_diameter = ED/HL)
#'
#' @name morphometric_traits_FB
#' @export
morphometric_traits_FB <- function(names,
                                   ...,
                                   return_pics = FALSE,
                                   FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)

  # Load taxonomy
  taxo <- load_fish_taxonomy("FB", version = version) %>%
    dplyr::filter(Genus != "Leptocephalus") # current issue in FB taxonomy

  # Morphometry table
  morph <- rfishbase::morphometrics(version = version) %>%
    dplyr::mutate(SL = as.numeric(SL)) # for version 3.1.9

  # Add trait(s)
  args <- rlang::quos(...)
  n_traits <- length(args)

  if (n_traits == 0) cli::cli_abort("No traits requested")

  out <- purrr::map(
    1:n_traits,
    function(i) {

      morph2 <- dplyr::mutate(morph, !!! args[i])
      # Get trait name, also for unnamed traits which are renamed by mutate
      trait_name <- colnames(morph2)[ncol(morph2)]
      morph2 <- dplyr::rename(morph2, "trait" = dplyr::all_of(trait_name))

      # Get average trait for each species
      morph2 <- taxo %>%
        dplyr::left_join(
          morph2 %>%
            dplyr::filter(!is.na(.data$trait)) %>% # need to remove NAs to get correct n
            dplyr::group_by(SpecCode) %>%
            dplyr::summarise(trait_mean.s = mean(.data$trait, na.rm = TRUE),
                             trait_sd.s = stats::sd(.data$trait, na.rm = TRUE),
                             trait_n.s = dplyr::n(),
                             trait_pics.s = ifelse(return_pics, list(PicName), NA)),
          by = "SpecCode")

      # Get average trait for all higher ranks
      morph2 <- purrr::map2(
        c("Genus", "Family", "Order", "Class"),
        c("g", "f", "o", "c"),
        ~ morph2 %>%
          dplyr::left_join(
            morph2 %>%
              dplyr::filter(!is.na(.data$trait_mean.s)) %>% # need to remove NAs to get correct n
              dplyr::select(dplyr::all_of(c(.x, "Species", "trait_mean.s", "trait_pics.s"))) %>%
              dplyr::group_by(dplyr::across(dplyr::all_of(.x))) %>%
              dplyr::summarise(trait_mean = mean(.data$trait_mean.s, na.rm = TRUE),
                               trait_sd = stats::sd(.data$trait_mean.s, na.rm = TRUE),
                               trait_n = dplyr::n(),
                               trait_pics = ifelse(return_pics, list(.data$trait_pics.s), NA)) %>%
              rlang::set_names(.x,
                               paste(c("trait_mean", "trait_sd", "trait_n", "trait_pics"),
                                     .y,
                                     sep = '.')),
            by = .x)
      ) %>%
        purrr::reduce(dplyr::left_join) %>%
        suppressMessages()

      ranks <- list("Species" = c("s", "g", "f", "o", "c"),
                    "Genus" = c("g", "f", "o", "c"),
                    "Family" = c("f", "o", "c"),
                    "Order" = c("o", "c"),
                    "Class" = "c")

      ranks <- ranks[unique(stringi::stri_trans_totitle(names$id.rank))]

      morph2 <- purrr::map(
        1:length(ranks),
        function(x) {
          trait_summary <- morph2 %>%
            dplyr::select(dplyr::all_of(
              c(names(ranks)[x],
                paste0("trait_",
                       rep(c("mean", "sd", "n"), each = length(ranks[[x]])),
                       ".",
                       ranks[[x]]))
            )) %>%
            # retain only requested taxa to speed up
            dplyr::filter(
              .data[[names(ranks)[x]]] %in% (names %>%
                                               dplyr::filter(.data$id.rank == tolower(names(ranks)[x])) %>%
                                               dplyr::pull(tolower(names(ranks)[x])))
            ) %>%
            unique() %>%
            tidyr::pivot_longer(cols = dplyr::starts_with("trait_"),
                                names_to = "name",
                                values_to = "value") %>%
            dplyr::mutate(trait_rank = gsub(".*\\.", "", .data$name),
                          trait_rank = factor(.data$trait_rank,
                                              levels = c("s", "g", "f", "o", "c"),
                                              labels = c("species", "genus", "family", "order", "class"),
                                              ordered = TRUE),
                          name = gsub("\\..*", "", .data$name)) %>%
            dplyr::filter(!is.na(.data$value)) %>%
            tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(names(ranks)[x]))) %>%
            dplyr::filter(.data$trait_rank == min(.data$trait_rank)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(trait_rank = as.character(.data$trait_rank))

          if (return_pics) {
            trait_summary <- trait_summary %>%
              dplyr::left_join(
                morph2 %>%
                  dplyr::select(dplyr::all_of(
                    c(names(ranks)[x],
                      paste0("trait_",
                             rep(c("pics"), each = length(ranks[[x]])),
                             ".",
                             ranks[[x]]))
                  )) %>%
                  dplyr::filter(
                    .data[[names(ranks)[x]]] %in% (names %>%
                                                     dplyr::filter(.data$id.rank == tolower(names(ranks)[x])) %>%
                                                     dplyr::pull(tolower(names(ranks)[x])))
                  ) %>%
                  unique(),
                by = c(names(ranks)[x])
              ) %>%
              dplyr::rowwise() %>%
              dplyr::mutate(
                trait_pics = ifelse(!is.na(.data$trait_rank),
                                    list(get(paste0("trait_pics.",
                                                    stringi::stri_sub(.data$trait_rank, 0, 1)))),
                                    NA)
              ) %>%
              dplyr::ungroup()
          }

          if (return_pics) {
            trait_summary %>%
              dplyr::select("submitted_name" = 1, "trait_mean", "trait_sd", "trait_n", "trait_rank", "trait_pics")
          } else {
            trait_summary %>%
              dplyr::select("submitted_name" = 1, "trait_mean", "trait_sd", "trait_n", "trait_rank")
          }
        }) %>%
        dplyr::bind_rows()

      morph2 %>%
        dplyr::rename("{trait_name}" := "trait_mean") %>%
        dplyr::rename_with(~ gsub("trait_", paste0(trait_name, "."), .x, fixed = TRUE))
    }) %>%
    purrr::reduce(dplyr::left_join, by = "submitted_name")

  names %>%
    dplyr::mutate(
      submitted_name = dplyr::case_when(
        .data$id.rank == "species" ~ species,
        .data$id.rank == "genus" ~ genus,
        .data$id.rank == "family" ~ family,
        .data$id.rank == "order" ~ order,
        .data$id.rank == "class" ~ class)) %>%
    dplyr::left_join(out, by = "submitted_name") %>%
    dplyr::select(-c(2:8))
}

#' @rdname morphometric_traits_FB
#' @export
aspect_ratio_FB <- function(names,
                            return_pics = FALSE,
                            FB_version = latest_release("FB")) {
  morphometric_traits_FB(names,
                         aspect_ratio = AspectRatio,
                         return_pics = return_pics,
                         FB_version = FB_version)
}

#' @rdname morphometric_traits_FB
#' @export
elongation_FB <- function(names,
                          return_pics = FALSE,
                          FB_version = latest_release("FB")) {
  morphometric_traits_FB(names,
                         elongation = SL/BD,
                         return_pics = return_pics,
                         FB_version = FB_version)
}

#' @rdname morphometric_traits_FB
#' @export
body_depth_FB <- function(names,
                          return_pics = FALSE,
                          FB_version = latest_release("FB")) {
  morphometric_traits_FB(names,
                         body_depth = BD/SL,
                         return_pics = return_pics,
                         FB_version = FB_version)
}
