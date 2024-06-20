#' Get trophic level from FishBase
#'
#' @description
#' `trophic_level_FB` extracts the average trophic level at the lowest
#' available taxonomic rank. It works with taxa identified at different
#' taxonomic ranks.
#'
#' @details
#' ## Type of trophic level estimates
#' This function can return three different trophic level estimates:
#'  - `type = "diet"`: the trophic level based on diet composition only.
#'  These estimates are available only for some species (~5%).
#'  Note that for many families there are no diet studies available.
#'  - `type = "food items"`: the trophic level based on food items only.
#'  These estimates are available for many more species (>20%) and almost all
#'  families.
#'  - `type = "both"`: the average of the two when both are available,
#'  otherwise the available estimate.
#'
#' ## Standard errors
#' The function also returns a standard error (SE) of the estimate. This is the
#' SE from FishBase when values are extracted at species level and `type` is
#' "diet" or "food items". When `type = "both"` and only one estimate is
#' available, the corresponding SE from FishBase is returned. Instead, if
#' the diet- and food-based estimates are both available, the average SE is
#' computed using the formula:
#' \deqn{\sqrt{\frac{SE_{diet}^2 + SE_{food}^2}{2}}}
#' Similarly, for values extracted at higher taxonomic ranks, the SE is
#' averaged across the *N* species with available data belonging to the rank as:
#' \deqn{\sqrt{\frac{\sum_{i=1}^{N} SE_{i}^2}{N}}}
#'
#' @inheritParams morphometric_traits_FB
#' @param type A string; the type of estimate to return. Possible values are:
#' "both" (default), "diet", "food items". See *Details*
#'
#' @return A data frame with the provided name(s), the trophic level(s) and
#' associated SE, and the taxonomic rank at which the values are extracted.
#' The output also includes a column ("remark") describing how the values
#' were calculated (i.e. the method applied by FishBase when data are
#' available at the species level, or the number of species used to get
#' average values at higher ranks).
#'
#' @importFrom rfishbase ecology
#' @importFrom dplyr slice pick starts_with rename_with n
#' @importFrom purrr map map2 reduce
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang set_names
#'
#' @inherit load_fish_taxonomy author
#'
#' @examplesIf interactive()
#' # List of taxa identified to species, genus or family level
#' species <- c("Acanthurus achilles", "Lethrinus sp.", "Carangidae spp.")
#' ranks <- c("species", "genus", "family")
#' taxa <- build_fish_taxonomy(species, ranks)
#' # Trophic level of fishes identified to species, genus or family level
#' trophic_level_FB(taxa)
#' # Trophic level based only on food items
#' trophic_level_FB(taxa, type = "food items")
#'
#' @export
trophic_level_FB <- function(names,
                             type = c("both", "diet", "food items"),
                             FB_version = latest_release("FB")) {

  # Checks
  check_string(FB_version)
  version <- check_version("FB", FB_version)
  names <- check_names_arg(names, version = version)
  type <- rlang::arg_match(type)
  type <- switch(type,
                 "both",
                 "diet" = "Diet",
                 "food items" = "Food")

  # Load taxonomy
  taxo <- load_fish_taxonomy("FB", version = version)

  # Trophic level for all species
  troph <- rfishbase::ecology(version = version) %>%
    dplyr::select(SpecCode, dplyr::starts_with("Diet"), dplyr::starts_with("Food")) %>%
    # Drop NAs
    dplyr::filter(!is.na(DietTroph) | !is.na(FoodTroph)) %>% # | !is.na(DietTLu)
    # Some species have different entries for different stocks
    # Retain only the first, which is the one reported in FishBase
    # Data is typically either identical to the first or simply missing in the additional stocks
    dplyr::group_by(SpecCode) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Get trophic level for each species based on the selected type
  if (type == "both") {
    troph <- troph %>%
      dplyr::mutate(
        troph_mean.s = rowMeans(dplyr::pick(DietTroph, FoodTroph), na.rm = TRUE),
        troph_se.s = dplyr::case_when(
          is.na(DietTroph) ~ FoodSeTroph,
          is.na(FoodTroph) ~ DietSeTroph,
          # Else get average SE using formula of average SD with equal sample size
          .default = sqrt(rowSums(dplyr::pick(DietSeTroph, FoodSeTroph)^2, na.rm = TRUE)/2)),
        # If instead we want to calculate SE of the two estimates
        # sqrt(((DietTroph - troph_mean.s)^2 + (FoodTroph - troph_mean.s)^2)/2) / sqrt(2)),
        troph_remark.s = dplyr::case_when(
          is.na(DietTroph) ~ FoodRemark,
          is.na(FoodTroph) ~ DietRemark,
          .default = "Average trophic level for this species based on diet and food items.")
      ) %>%
      dplyr::select("SpecCode", "troph_mean.s", "troph_se.s", "troph_remark.s")
  } else {
    troph <- troph %>%
      dplyr::select("SpecCode",
                    troph_mean.s = paste0(type, "Troph"),
                    troph_se.s = paste0(type, "SeTroph"),
                    troph_remark.s = paste0(type, "Remark"))
  }

  # Join taxonomic info
  troph <- taxo %>%
    dplyr::left_join(troph, by = "SpecCode")

  # Get average trophic level for all higher ranks
  troph <- purrr::map2(
    c("Genus", "Family", "Order", "Class"),
    c("g", "f", "o", "c"),
    ~ troph %>%
      dplyr::left_join(
        troph %>%
          dplyr::filter(!is.na(troph_mean.s)) %>% # need to remove NAs to get correct n
          dplyr::select(dplyr::all_of(c(.x, "Species", "troph_mean.s", "troph_se.s"))) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(.x))) %>%
          dplyr::summarise(troph_mean = mean(troph_mean.s, na.rm = TRUE),
                           troph_n = dplyr::n(),
                           # Average SE across species
                           troph_se = sqrt(sum(troph_se.s^2, na.rm = TRUE) / troph_n),
                           # If instead we want to calculate SE from species estimates
                           # troph_se = sd(troph_mean.s) / sqrt(troph_n),
                           troph_remark = paste0("Average trophic level based on data from ",
                                                 troph_n, " species within this ",
                                                 tolower(.x), ".")) %>%
          dplyr::select(-troph_n) %>%
          rlang::set_names(.x,
                           paste(c("troph_mean", "troph_se", "troph_remark"),
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

  troph <- purrr::map(
    1:length(ranks),
    function(x) {
      troph %>%
        dplyr::select(dplyr::all_of(
          c(names(ranks)[x],
            paste0("troph_",
                   rep(c("mean", "se", "remark"), each = length(ranks[[x]])),
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
        tidyr::pivot_longer(cols = dplyr::starts_with("troph_mean"),
                            names_to = "name",
                            values_to = "value") %>%
        dplyr::mutate(troph_rank = gsub(".*\\.", "", .data$name),
                      troph_rank = factor(.data$troph_rank,
                                          levels = c("s", "g", "f", "o", "c"),
                                          labels = c("species", "genus", "family", "order", "class"),
                                          ordered = TRUE),
                      name = gsub("\\..*", "", .data$name)) %>%
        dplyr::filter(!is.na(.data$value)) %>%
        tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(names(ranks)[x]))) %>%
        dplyr::filter(.data$troph_rank == min(.data$troph_rank)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(troph_se = get(paste0("troph_se.",
                                            stringi::stri_sub(.data$troph_rank, 0, 1))),
                      troph_remark = get(paste0("troph_remark.",
                                                stringi::stri_sub(.data$troph_rank, 0, 1))),
                      troph_rank = as.character(.data$troph_rank)) %>%
        dplyr::ungroup() %>%
        dplyr::select("submitted_name" = 1,
                      "trophic_level" = "troph_mean",
                      "troph_se", "troph_remark", "troph_rank") %>%
        dplyr::rename_with(~ gsub("troph_", "trophic_level.", .x, fixed = TRUE))
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
    dplyr::left_join(troph, by = "submitted_name") %>%
    dplyr::select(-c(2:8))
  # %>%
  #   # Round trophic level
  #   dplyr::mutate(trophic_level = round(trophic_level, 2),
  #                 trophic_level.se = round(trophic_level.se, 2))
}
