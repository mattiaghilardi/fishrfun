#' Create community composition matrices
#'
#' A generic function to create matrices of community composition, with
#' flexibility in the choice of variables to use as rows, columns and values
#'
#' @param data A long-format data frame
#' @param rows_var A string; the variable in `data` to be used as rows
#' @param cols_var A string; the variable in `data` to be used as columns
#' @param values_var A string; the variable in `data` to be used as values
#' @param relative Logical; if values should be converted into relative
#' to the sum in each row. Default to `FALSE`
#' @param as_matrix Logical; if the function should return a matrix.
#' Default to `TRUE`, if `FALSE` will return a data frame
#'
#' @return A matrix or data frame
#'
#' @importFrom tibble column_to_rownames
#' @importFrom rlang is_double
#' @importFrom dplyr any_of everything
#' @importFrom tidyr replace_na
#'
#' @export
#'
#' @examples
#' # Example data including length, abundance and length-weight parameters
#' # Note that species may have multiple entries per site, which are then summarised
#' data <- tibble::tribble(
#'   ~site, ~species, ~length, ~abundance, ~a, ~b,
#'   "site1", "sp1", 10, 3, 0.02, 2.97,
#'   "site1", "sp2", 8, 5, 0.01, 2.95,
#'   "site1", "sp3", 15, 1, 0.02, 2.94,
#'   "site2", "sp1", 8, 3, 0.02, 2.97,
#'   "site2", "sp2", 5, 7, 0.01, 2.95,
#'   "site3", "sp1", 5, 3, 0.02, 2.97,
#'   "site3", "sp1", 10, 2, 0.02, 2.97,
#'   "site3", "sp3", 10, 1, 0.02, 2.94,
#'   "site3", "sp3", 15, 1, 0.02, 2.94
#' )
#' # Compute biomass for each row
#' data <- dplyr::mutate(data,
#'                       biomass = a*length^b * abundance)
#' # Incidence
#' create_comm_matrix(data, "site", "species", NULL)
#' # Absolute abundance
#' create_comm_matrix(data, "site", "species", "abundance")
#' # Relative abundance
#' create_comm_matrix(data, "site", "species", "abundance", relative = TRUE)
#' # Absolute biomass
#' create_comm_matrix(data, "site", "species", "biomass")
#' # Relative biomass
#' create_comm_matrix(data, "site", "species", "biomass", relative = TRUE)
create_comm_matrix <- function(data,
                               rows_var,
                               cols_var,
                               values_var = NULL,
                               relative = FALSE,
                               as_matrix = TRUE) {

  # TO DO: check data
  check_string(rows_var)
  check_string(cols_var)
  if (!rlang::is_null(values_var)) check_string(values_var)
  vars <- c(rows_var, cols_var, values_var)
  wrong_vars <- vars[!vars %in% colnames(data)]
  if (length(wrong_vars) > 0)
    cli::cli_abort("{.val {wrong_vars}} {?is/are} not present in {.arg data}.")

  if (!rlang::is_character(data[[rows_var]]))
    cli::cli_abort("{.arg rows_var} must be a character column in {.arg data}.")
  if (!rlang::is_character(data[[cols_var]]))
    cli::cli_abort("{.arg cols_var} must be a character column in {.arg data}.")
  if (!rlang::is_null(values_var))
    if (!rlang::is_double(data[[values_var]]))
    cli::cli_abort("{.arg values_var} must be a numeric column in {.arg data}.")

  check_logical(relative)
  check_logical(as_matrix)

  data <- data |>
    dplyr::select(dplyr::any_of(vars)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(vars[1:2]))) |>
    dplyr::summarise(n = ifelse(rlang::is_null(values_var),
                                1,
                                sum(.data[[values_var]], na.rm = TRUE)),
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = dplyr::all_of(cols_var), values_from = "n") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) |>
    tibble::column_to_rownames(rows_var)

  if (relative) {
    if (rlang::is_null(values_var)) {
      cli::cli_inform(
        c("Relative values are not calculated if `values_var = NULL`",
          "i" = "Return incidence matrix")
        )
    } else {
      data <- data |>
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    ~.x / rowSums(data, na.rm = TRUE)))
    }
  }

  if (as_matrix) data <- as.matrix(data)

  data
}
