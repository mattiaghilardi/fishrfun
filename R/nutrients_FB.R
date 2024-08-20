#' Get nutrients estimates from FishBase
#'
#' @description
#' `nutrients_FB` extracts the concentrations of seven key nutrients in fish:
#' calcium (mg per 100 g), iron (mg per 100 g), omega-3 fatty acids (g per 100 g),
#' protein (%), selenium (&mu;g per 100 g), vitamin A (&mu;g per 100 g), and
#' zinc (mg per 100 g). These are estimated by a Bayesian model (available
#' [here](https://github.com/mamacneil/NutrientFishbase)) which includes
#' phylogenetic and traits information, an update of the model in
#' [Hicks et al. 2019](https://doi.org/10.1038/s41586-019-1592-6).
#' It works with taxa identified at different taxonomic ranks.
#'
#' @inheritParams morphometric_traits_FB
#'
#' @return A data frame with the provided name(s), medians and 95% Highest
#' Posterior Densities of seven nutrients, the taxonomic rank at which the values are extracted
#' and a column ("method") describing how the values were obtained.
#'
#' @importFrom stringi stri_sub
#'
#' @inherit load_fish_taxonomy author
#'
#' @references
#' Hicks C et al. (2019) Harnessing global fisheries to tackle micronutrient
#' deficiencies. Nature, 574: 95-98. <https://doi.org/10.1038/s41586-019-1592-6>
#'
#' @examplesIf interactive()
#' # Nutrients concentrations of fishes identified to species, genus or family level
#' species <- c("Scarus ghobban", "Caranx sp.", "Lutjanidae spp.")
#' ranks <- c("species", "genus", "family")
#' taxa <- build_fish_taxonomy(species, ranks)
#' nutrients_FB(taxa)
#'
#' @export
nutrients_FB <- function(names,
                         FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)

  # Load taxonomy
  taxo <- load_fish_taxonomy("FB", version = version)

  # Get nutrients for all species
  nut <- taxo %>%
    dplyr::left_join(
      rfishbase::estimate(version = version) %>%
        dplyr::select("SpecCode",
                      dplyr::starts_with(c("Calcium", "Iron", "Omega", "Protein",
                                           "Selenium", "Vitamin", "Zinc"))) %>%
        dplyr::rename_with(~paste(.x, "s", sep = '.'), -"SpecCode"),
      by = "SpecCode")

  # Nutrients at higher ranks
  nut <- purrr::map2(
    c("Genus", "Family", "Order", "Class"),
    c("g", "f", "o", "c"),
    ~ nut %>%
      dplyr::mutate(method.s = ifelse(is.na(Calcium.s),
                                      NA,
                                      "Nutrients estimates for this species")) %>%
      dplyr::left_join(
        nut %>%
          dplyr::rename_with(~gsub(".s", "", .x, fixed = TRUE)) %>%
          dplyr::filter(!is.na(.data$Calcium)) %>%
          dplyr::select(dplyr::all_of(dplyr::starts_with(c(.x, "Calcium", "Iron", "Omega", "Protein",
                                                           "Selenium", "Vitamin", "Zinc")))) %>%
          tidyr::pivot_longer(
            cols = -dplyr::all_of(.x),
            names_to = "nutrient",
            values_to = "value"
          ) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(.x, "nutrient")))) %>%
          dplyr::summarise(n = dplyr::n(),
                           mean = median(value, na.rm = TRUE),
                           method = paste("Median nutrients concentrations",
                                          "based on estimates for",
                                          n, "species within this", tolower(.x))) %>%
          dplyr::select(-n) %>%
          dplyr::mutate(nutrient = paste(nutrient, .y, sep = '.')) %>%
          dplyr::rename_with(~paste(.x, .y, sep = '.'), "method", .y) %>%
          tidyr::pivot_wider(names_from = "nutrient", values_from = "mean"),
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

  nut <- purrr::map(
    1:length(ranks),
    function(x) {
      nut %>%
        dplyr::select(dplyr::all_of(
          c(names(ranks)[x],
            paste0("method.", ranks[[x]]),
            paste0(rep(c("Calcium", "Iron", "Omega3", "Protein",
                         "Selenium", "VitaminA", "Zinc"),
                       3 * length(ranks[[x]])),
                   rep(c("", "_l95", "_u95"), each = 7 * length(ranks[[x]])),
                   ".",
                   rep(rep(ranks[[x]], each = 7), 3))
          )
        )) %>%
        # retain only requested taxa to speed up
        dplyr::filter(
          .data[[names(ranks)[x]]] %in% (names %>%
                                           dplyr::filter(.data$id.rank == tolower(names(ranks)[x])) %>%
                                           dplyr::pull(tolower(names(ranks)[x])))
        ) %>%
        unique() %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with(c("Calcium", "Iron", "Omega3", "Protein",
                                      "Selenium", "VitaminA", "Zinc")),
          names_to = "name",
          values_to = "value") %>%
        dplyr::mutate(rank = gsub(".*\\.", "", .data$name),
                      rank = factor(.data$rank,
                                    levels = c("s", "g", "f", "o", "c"),
                                    labels = c("species", "genus", "family", "order", "class"),
                                    ordered = TRUE),
                      name = gsub("\\..*", "", .data$name)) %>%
        dplyr::filter(!is.na(.data$value)) %>%
        tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(names(ranks)[x]))) %>%
        dplyr::filter(.data$rank == min(.data$rank)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(method = get(paste0("method.", stringi::stri_sub(.data$rank, 0, 1))),
                      rank = as.character(.data$rank)) %>%
        dplyr::ungroup() %>%
        dplyr::select("submitted_name" = 1,
                      dplyr::starts_with(c("Calcium", "Iron", "Omega3", "Protein",
                                           "Selenium", "VitaminA", "Zinc")),
                      "nutrients.rank" = "rank",
                      "nutrients.method" = "method")
    }) %>%
    dplyr::bind_rows()

  # Out
  names %>%
    dplyr::mutate(
      submitted_name = dplyr::case_when(
        .data$id.rank == "species" ~ species,
        .data$id.rank == "genus" ~ genus,
        .data$id.rank == "family" ~ family,
        .data$id.rank == "order" ~ order,
        .data$id.rank == "class" ~ class)) %>%
    dplyr::left_join(nut, by = "submitted_name") %>%
    dplyr::select(-c(2:8))
}
