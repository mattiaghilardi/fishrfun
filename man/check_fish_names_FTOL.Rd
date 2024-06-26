% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_fish_names.R
\name{check_fish_names_FTOL}
\alias{check_fish_names_FTOL}
\title{Check scientific names and sampled species in the Fish Tree of Life}
\usage{
check_fish_names_FTOL(names, rank = "Species", sampled = FALSE)
}
\arguments{
\item{names}{A string or character vector of names}

\item{rank}{A string; the taxonomic rank at which names should be checked.
Must be one of "Species" (default), "Genus", "Family", "Order", or "Class"}

\item{sampled}{Logical, only if \code{rank = "Species"}. If names have to be
checked for unsampled species (i.e. those without genetic data, see
\href{https://doi.org/10.1038/s41586-018-0273-1}{Rabosky et al. 2018}
for detailed methodology)}
}
\value{
A message. If any non-valid name is found, a data frame is also returned
invisibly, including non-valid supplied names and suggested names.
If \code{rank = "Species"} and \code{sampled = TRUE}, and any non-valid name
or unsampled species is found, a list is returned invisibly with the
following two elements:
\itemize{
\item \code{name_error}: data frame with non valid supplied names and suggested names
\item \code{not_sampled}: vector of supplied names without genetic data
}
}
\description{
Check scientific names and sampled species in the Fish Tree of Life
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Correct names with genetic data
check_fish_names_FTOL(c("Balistapus undulatus", "Plectropomus leopardus"),
                      sampled = TRUE)
# Misspelled family name and suggested name
errors <- check_fish_names_FTOL(c("Gobidae", "Sparidae"), rank = "Family")
errors
\dontshow{\}) # examplesIf}
}
\references{
Rabosky D. L. et al. (2018) An inverse latitudinal gradient in
speciation rate for marine fishes. Nature, 559, 392-395.
\url{https://doi.org/10.1038/s41586-018-0273-1}
}
\author{
Mattia Ghilardi, \email{mattia.ghilardi91@gmail.com}
}
