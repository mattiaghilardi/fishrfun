#' Check fish scientific names
#'
#' @param names A string or character vector of names
#' @param rank A string; the taxonomic rank at which names should be checked.
#'             Must be one of "Species" (default), "Genus", "Family",
#'             "Order", or "Class"
#' @inheritParams load_fish_taxonomy
#'
#' @importFrom cli cli_inform cli_alert_danger cli_alert_info
#'
#' @return
#' A message. If any non-valid name is found, a data frame is also returned
#' invisibly, including non-valid supplied names and suggested names.
#' If `rank = "Species"`, the data frame also includes valid names returned by
#' [rfishbase::validate_names()] or [validate_names_ECoF()].
#'
#' @inherit load_fish_taxonomy author
#'
#' @export
#'
#' @examplesIf interactive()
#' # Correct names in FishBase
#' check_fish_names(c("Boops boops", "Sparus aurata"))
#' # Misspelled family name and suggested name
#' # using the Eschmeyer's Catalog of Fishes
#' errors <- check_fish_names(c("Balistidae", "Seranidae"),
#'                            rank = "Family",
#'                            db = "ECoF")
#' errors
#' # Non-valid species name, suggested name and valid name
#' errors <- check_fish_names(c("Scarus ghobban", "Ostracion cubicus"))
#' errors
check_fish_names <- function(names,
                             rank = "Species",
                             db = c("FB", "ECoF"),
                             version = "latest") {

  # Checks
  check_character(names)
  rank <- rlang::arg_match(rank,
                           c("Species", "Genus", "Family", "Order", "Class"))
  db <- rlang::arg_match(db)
  check_string(version)
  version <- check_version(db, version)

  # Rank lower case in ECoF
  if (db == "ECoF") rank <- tolower(rank)

  # Check names
  taxo <- load_fish_taxonomy(db, version)
  unique_taxa <- unique(taxo[, rank, drop = TRUE])
  error <- names[! names %in% unique_taxa]

  if (length(error) == 0) {
    cli::cli_inform(paste("All", rank, "names are correct"))
  } else {
    #error
    msg1 <- paste("{length(error)}", rank, "{?name/names} {?is/are} incorrect:
                  {.val {error}}")
    cli::cli_alert_danger(msg1,
                          wrap = TRUE)

    # Find best match for misspelled names
    suggested <- find_best_match(error, unique_taxa)

    msg2 <- paste("Approximate string matching for misspelled names returned
                    the following {length(suggested)} potential", rank,
                  "{?name/names}: {.val {suggested}}")
    cli::cli_alert_info(msg2,
                        wrap = TRUE)

    out <- data.frame(supplied_name = error,
                      suggested_name = suggested)

    if (rank %in% c("Species", "species")) {
      valid <- validate_fish_names(error, db, version)
      fun <- if (db == "FB") "rfishbase::validate_names()"
      else if (db == "ECoF") "validate_names_ECoF()"
      msg3 <- "A call to `{fun}` returned the
              following {length(valid)} valid {?name/names}: {.val {valid}}"
      cli::cli_alert_info(msg3,
                          wrap = TRUE)
      out$valid_name <- valid
    }

    invisible(out)
  }
}

validate_fish_names <- function(species_list,
                                db = c("FB", "ECoF"),
                                version = "latest") {

  if (db == "FB") {
    rfishbase::validate_names(species_list, version = version)
  } else if (db == "ECoF") {
    validate_names_ECoF(species_list, USE.NAMES = FALSE, version = version)
  }
}

#' Validate species names in the Eschmeyer's Catalog of Fishes
#'
#' @param species_list A string or character vector of species names
#' @param USE.NAMES Logical; if TRUE use `species_list` as names for
#'                  the returned object
#' @param version A string; the database version to use. Will default
#'                to the latest available; see [available_ECoF_releases()]
#'
#' @return A vector of validated names, or a list if multiple
#' valid species match a single name
#'
#' @inherit load_fish_taxonomy author
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
#'
#' @examplesIf interactive()
#' validate_names_ECoF("Ostracion cubicus")
validate_names_ECoF <- function(species_list,
                                USE.NAMES = FALSE,
                                version = latest_release("ECoF")) {

  check_character(species_list)
  check_string(version)
  version <- check_version("ECoF", version)

  db <- load_ECoF_db(version)
  sp <- gsub(",.*", "", db$all_species$query)
  gen <- gsub(".*, ", "", db$all_species$query)
  db$all_species$query <- paste(gen, sp)

  sapply(species_list, function(x) {
    if (x %in% db$taxonomy$species) {
      x
    } else if (x %in% db$all_species$query) {
      df <- dplyr::filter(db$all_species, .data$query == x)
      unique(df$species)
    } else {
      NA
    }
  }, USE.NAMES = USE.NAMES)
}

#' Check scientific names and sampled species in the Fish Tree of Life
#'
#' @inheritParams check_fish_names
#' @param sampled Logical, only if `rank = "Species"`.
#'                If names have to be checked for unsampled species
#'                (i.e. those without genetic data,
#'                see Rabosky et al. 2018 for detailed methodology)
#'
#' @return
#' A message. If any non-valid name is found, a data frame is also returned
#' invisibly, including non-valid supplied names and suggested names.
#' If `rank = "Species"` and `sampled = TRUE`, and any non-valid name
#' or unsampled species is found, a list is returned invisibly with the
#' following two elements:
#'  - `name_error`: data frame with non valid supplied names and suggested names
#'  - `not_sampled`: vector of supplied names without genetic data
#'
#' @importFrom fishtree fishtree_taxonomy
#'
#' @inherit load_fish_taxonomy author
#'
#' @export
#'
#' @examplesIf interactive()
#' # Correct names with genetic data
#' check_fish_names_FTOL(c("Balistapus undulatus", "Plectropomus leopardus"),
#'                       sampled = TRUE)
#' # Misspelled family name and suggested name
#' errors <- check_fish_names_FTOL(c("Gobidae", "Sparidae"), rank = "Family")
#' errors
#'
#' @references
#' Rabosky D. L. et al. (2018) An inverse latitudinal gradient in
#' speciation rate for marine fishes. Nature, 559, 392–395.
#' <https://doi.org/10.1038/s41586-018-0273-1>
check_fish_names_FTOL <- function(names,
                                  rank = "Species",
                                  sampled = FALSE) {

  check_character(names)
  rank <- rlang::arg_match(rank,
                           c("Species", "Genus", "Family", "Order", "Class"))

  # Rank lower case in FTOL
  rank <- tolower(rank)

  # Retrieve all ranks in the Fish Tree of Life
  taxo_ftol <- fishtree::fishtree_taxonomy()

  if (rank == "species") {
    check_logical(sampled)
    # Retrieve all species
    sp_ftol <- fishtree::fishtree_taxonomy(taxo_ftol[taxo_ftol$rank == "class", "name"])[[1]]
    unique_taxa <- unique(sp_ftol$species)
    # Check names
    error <- names[! names %in% unique_taxa]

    # If sampled=TRUE check sampled species
    if (isTRUE(sampled)) {
      not_sampled <- names[! names %in% sp_ftol$sampled_species]
    }
  } else if (rank == "genus") {
    # Retrieve all genus from species names
    sp_ftol <- fishtree::fishtree_taxonomy(taxo_ftol[taxo_ftol$rank == "class", "name"])[[1]]
    unique_taxa <- unique(gsub(" .*", "", sp_ftol$species))
    # Check names
    error <- names[! names %in% unique_taxa]
  } else {
    # For all other ranks
    unique_taxa <- unique(taxo_ftol[taxo_ftol$rank == rank, "name"])
    error <- names[! names %in% unique_taxa]
  }

  if (length(error) == 0) {
    cli::cli_inform(paste("All", rank, "names are correct"))
    out <- error
  } else {
    msg1 <-paste("{length(error)}", rank, "{?name/names} {?is/are} incorrect
                 or not present in the Fish Tree of Life: {.val {error}}")
    cli::cli_alert_danger(msg1,
                          wrap = TRUE)

    # Find best match for misspelled names
    suggested <- find_best_match(error, unique_taxa)

    msg2 <- paste("Approximate string matching for misspelled names returned the
                  following {length(suggested)} potential", rank,
                  "{?name/names}: {.val {suggested}}")
    cli::cli_alert_info(msg2,
                        wrap = TRUE)

    out <- data.frame(supplied_name = error,
                      suggested_name = suggested)
  }

  # If sampled species were checked return a message
  if (exists("not_sampled")) {

    if (length(not_sampled) == 0) {
      cli::cli_inform("All species have genetic data")
    } else {
      msg3 <- "{length(not_sampled)} species do not have genetic
               data: {.val {not_sampled}}"
      cli::cli_alert_danger(msg3,
                            wrap = TRUE)
    }

    # If errors invisibly return a list
    if (length(error) > 0 | length(not_sampled) > 0) {
      invisible(list(name_error = out,
                     not_sampled = not_sampled))
    }
  } else {
    invisible(out)
  }
}
