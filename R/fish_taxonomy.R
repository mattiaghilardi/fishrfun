#' Load fish taxonomy
#'
#' @param db Either "FB" (FishBase; default) or "ECoF" (Eschmeyer's Catalog of Fishes)
#' @param version A string; the database version to use. Will default to the latest
#' available; see [rfishbase::available_releases()] for FishBase
#' and [available_ECoF_releases()] for the Eschmeyer's Catalog of Fishes
#'
#' @importFrom rfishbase load_taxa
#' @importFrom rlang is_null arg_match
#' @importFrom dplyr collect
#' @importFrom magrittr `%>%`
#'
#' @return A tibble
#'
#' @author Mattia Ghilardi, \email{mattia.ghilardi91@@gmail.com}
#'
#' @export
#'
#' @examplesIf interactive()
#' load_fish_taxonomy()
#' load_fish_taxonomy("ECoF")
load_fish_taxonomy <- function(db = c("FB", "ECoF"),
                               version = "latest") {

  db <- rlang::arg_match(db)
  check_string(version)
  version <- check_version(db, version)

  if (db == "FB") {
    # added collect only for version<4 as apparently the argument didn't work
    rfishbase::load_taxa(version = version, collect = TRUE) %>%
      suppressMessages() %>%
      dplyr::collect() %>%
      # remove SpecCode 0 and 1 which correspond to "Genus Species" and "Genus Sp"
      dplyr::filter(!is.na(Class))
  } else if (db == "ECoF") {
    db <- load_ECoF_db(version)
    db$taxonomy %>%
      dplyr::mutate(spid = as.numeric(.data$spid))
  }
}

#' Build fish taxonomy
#'
#' Build taxonomy for a list of fish taxa identified at different taxonomic ranks
#'
#' @param names A character vector of taxonomic names. The function accepts
#' both species names and names of fish identified at higher ranks
#' @param id.rank A character vector of taxonomic ranks describing the level
#' at which taxa are identified. Must be the same length as 'names'.
#' Accepted ranks are: "species", "genus", "family", "order", and "class".
#' If NULL (default), the function tries to guess the ranks from the names
#' @param check_names Logical, check whether taxonomic names are correct at
#' the respective taxonomic rank. Default to TRUE
#' @param colname A name for the column containing the supplied names
#' in the returned data frame
#' @inheritParams load_fish_taxonomy
#'
#' @return A data frame including the supplied names, ranks and taxonomy
#'
#' @inherit load_fish_taxonomy author
#'
#' @importFrom tidyr separate
#' @importFrom taxadb clean_names
#' @importFrom stringi stri_trans_totitle
#' @importFrom dplyr mutate case_when left_join pull select
#' @importFrom dplyr bind_cols bind_rows distinct rename
#' @importFrom cli cli_text
#' @importFrom rlang `:=`
#'
#' @export
#'
#' @examplesIf interactive()
#' species <- c("Dentex dentex", "Mullus sp.", "Balistidae spp.",
#'              "Carangiformes spp.", "Elasmobranchii spp.")
#' # Declaring level of identification
#' ranks = c("species", "genus", "family", "order", "class")
#' build_fish_taxonomy(species, id.rank = ranks)
#' # Letting the function guess the level of identification
#' build_fish_taxonomy(species)
build_fish_taxonomy <- function(names,
                                id.rank = NULL,
                                check_names = TRUE,
                                colname = "original_name",
                                db = c("FB", "ECoF"),
                                version = "latest") {

  # Checks
  check_character(names)
  db <- rlang::arg_match(db)
  check_string(version)
  version <- check_version(db, version)

  # Clean species names
  cleaned_names <- taxadb::clean_names(names) %>%
    stringi::stri_trans_totitle(type = "sentence")

  # Make data frame
  df <- data.frame(original_name = names,
                   cleaned_name = cleaned_names)

  # Open Nomenclature qualifiers relevant to fish
  ON <- c("sp", "spp", "ssp", "aff", "cf", "cfr", "conf", "?", "inc", "ind", "indet", "nov")
  # the "?" is already removed by taxadb::clean_names(binomial_only = TRUE),
  # thus uncertain species with "?" will be considered identified to species level
  # if `id.rank = NULL`

  # Split species names
  df <- df %>%
    tidyr::separate(.data$cleaned_name,
                    into = c("gen", "spe"),
                    sep = " ",
                    remove = FALSE,
                    fill = "right") %>%
    dplyr::mutate(
      # Fix names such as "labrid", "pomacentrid" to match family names
      gen = ifelse(endsWith(.data$gen, "id"),
                   paste0(.data$gen, "ae"),
                   .data$gen),
      # Remove potential open nomenclature qualifiers
      spe = ifelse(.data$spe %in% ON, NA, .data$spe))

  taxo <- load_fish_taxonomy(db, version)
  names(taxo) <- tolower(names(taxo))

  # Add genus, family, order, class and rank
  if (!rlang::is_null(id.rank)) {

    # Check id.rank
    check_character(id.rank)
    check_equal_length(id.rank, names)

    id.values <- c("species", "genus", "family", "order", "class")
    if (!all(unique(id.rank) %in% id.values)) {
      cli::cli_abort("{.arg id.rank} can only include
                     the following values: {.val {id.values}}",
                     call = rlang::caller_env())
    }

    df <- df %>%
      dplyr::mutate(
        id.rank = id.rank,
        species = ifelse(id.rank == "species",
                         .data$cleaned_name,
                         NA),
        genus = ifelse(id.rank %in% c("species", "genus"),
                       .data$gen,
                       NA),
        family = ifelse(id.rank == "family",
                        .data$gen,
                        NA),
        order = ifelse(id.rank == "order",
                       .data$gen,
                       NA),
        class = ifelse(id.rank == "class",
                       .data$gen,
                       NA)
      )
  } else {
    # Try to guess rank
    df <- df %>%
      dplyr::mutate(
        family = ifelse(endsWith(.data$gen, "dae"),
                        .data$gen,
                        NA),
        order = ifelse(endsWith(.data$gen, "formes"),
                       .data$gen,
                       NA),
        class = ifelse(.data$gen %in% unique(taxo$class) |
                         startsWith(.data$gen, "Actino") |
                         startsWith(.data$gen, "Elasmo") |
                         startsWith(.data$gen, "Teleost"),
                       .data$gen,
                       NA),
        genus = ifelse(is.na(.data$family) & is.na(.data$order) & is.na(.data$class),
                       .data$gen,
                       NA),
        id.rank = dplyr::case_when(!is.na(.data$class) ~ "class",
                                   !is.na(.data$order) ~ "order",
                                   !is.na(.data$family) ~ "family",
                                   !is.na(.data$genus) & is.na(.data$spe) ~ "genus",
                                   !is.na(.data$genus) & !is.na(.data$spe) ~ "species"),
        species = ifelse(id.rank == "species",
                         .data$cleaned_name,
                         NA)
      )
  }

  # Check names
  if (check_names) {
    error <- lapply(
      c("Species", "Genus", "Family", "Order", "Class"),
      function(x) {
        x_low <- tolower(x)
        if (x_low %in% df$id.rank) {
          error_x <- df %>%
            dplyr::filter(id.rank == x_low) %>%
            dplyr::pull(.data[[x_low]]) %>%
            check_fish_names(rank = x,
                             db = db,
                             version = version) %>%
            suppressMessages()
          if (!rlang::is_null(error_x)) {
            error_x <- df %>%
              dplyr::filter(id.rank == x_low &
                              .data[[x_low]] %in% error_x$supplied_name) %>%
              dplyr::select("original_name", "id.rank") %>%
              dplyr::bind_cols(error_x)
          }
          error_x
        }
      }
    ) %>%
      dplyr::bind_rows()

    if (nrow(error) > 0) {

      # Remove errors
      df <- df %>%
        dplyr::filter(!.data$original_name %in% error$original_name)

      f <- function() {
        if (nrow(df) == 0)
          cli::cli_abort("All names are incorrect or misspelled,
                          not possible to build the taxonomy\n
                          {.fn check_fish_names} may help",
                         call = rlang::caller_env())
        cli::cli_alert_danger("{.val {nrow(error)}} {?name/names} {?is/are}
                            incorrect or misspelled:",
                              wrap = TRUE)
        cli::cli_text("")
        print(error)
        cli::cli_text("")
        cli::cli_alert_info("{.fn check_fish_names} may help")
        cli::cli_alert_info("Building taxonomy only for valid names")
      }
      f()
    }
  }

  # Fill in taxonomy
  df <- df %>%
    dplyr::left_join(taxo %>%
                       dplyr::select("genus", "family") %>%
                       dplyr::distinct(),
                     by = "genus",
                     suffix = c("", ".db")) %>%
    dplyr::mutate(family = ifelse(is.na(.data$family),
                                  .data$family.db,
                                  .data$family)) %>%
    dplyr::select(-"family.db") %>%
    dplyr::left_join(taxo %>%
                       dplyr::select("family", "order") %>%
                       dplyr::distinct(),
                     by = "family",
                     suffix = c("", ".db")) %>%
    dplyr::mutate(order = ifelse(is.na(.data$order),
                                 .data$order.db,
                                 .data$order)) %>%
    dplyr::select(-"order.db") %>%
    dplyr::left_join(taxo %>%
                       dplyr::select("order", "class") %>%
                       dplyr::distinct(),
                     by = "order",
                     suffix = c("", ".db")) %>%
    dplyr::mutate(class = ifelse(is.na(.data$class),
                                 .data$class.db,
                                 .data$class)) %>%
    dplyr::select(-"class.db")

  # Out
  df %>%
    dplyr::select("original_name", "id.rank", "species", "genus", "family", "order", "class") %>%
    dplyr::rename({{colname}} := "original_name")
}
