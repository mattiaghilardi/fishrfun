% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trophic_level_FB.R
\name{trophic_level_FB}
\alias{trophic_level_FB}
\title{Get trophic level from FishBase}
\usage{
trophic_level_FB(
  names,
  type = c("both", "diet", "food items"),
  FB_version = latest_release("FB")
)
}
\arguments{
\item{names}{A data frame created by \code{\link[=build_fish_taxonomy]{build_fish_taxonomy()}} or a
character vector of taxonomic names to be passed on to \code{\link[=build_fish_taxonomy]{build_fish_taxonomy()}}}

\item{type}{A string; the type of estimate to return. Possible values are:
"both" (default), "diet", "food items". See \emph{Details}}

\item{FB_version}{A string; the FishBase version to use.
Will default to the latest available; see \code{\link[rfishbase:available_releases]{rfishbase::available_releases()}}}
}
\value{
A data frame with the provided name(s), the trophic level(s) and
associated SE, and the taxonomic rank at which the values are extracted.
The output also includes a column ("remark") describing how the values
were calculated (i.e. the method applied by FishBase when data are
available at the species level, or the number of species used to get
average values at higher ranks).
}
\description{
\code{trophic_level_FB} extracts the average trophic level at the lowest
available taxonomic rank. It works with taxa identified at different
taxonomic ranks.
}
\details{
\subsection{Type of trophic level estimates}{

This function can return three different trophic level estimates:
\itemize{
\item \code{type = "diet"}: the trophic level based on diet composition only.
These estimates are available only for some species (~5\%).
Note that for many families there are no diet studies available.
\item \code{type = "food items"}: the trophic level based on food items only.
These estimates are available for many more species (>20\%) and almost all
families.
\item \code{type = "both"}: the average of the two when both are available,
otherwise the available estimate.
}
}

\subsection{Standard errors}{

The function also returns a standard error (SE) of the estimate. This is the
SE from FishBase when values are extracted at species level and \code{type} is
"diet" or "food items". When \code{type = "both"} and only one estimate is
available, the corresponding SE from FishBase is returned. Instead, if
the diet- and food-based estimates are both available, the average SE is
computed using the formula:
\deqn{\sqrt{\frac{SE_{diet}^2 + SE_{food}^2}{2}}}
Similarly, for values extracted at higher taxonomic ranks, the SE is
averaged across the \emph{N} species with available data belonging to the rank as:
\deqn{\sqrt{\frac{\sum_{i=1}^{N} SE_{i}^2}{N}}}
}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# List of taxa identified to species, genus or family level
species <- c("Acanthurus achilles", "Lethrinus sp.", "Carangidae spp.")
ranks <- c("species", "genus", "family")
taxa <- build_fish_taxonomy(species, ranks)
# Trophic level of fishes identified to species, genus or family level
trophic_level_FB(taxa)
# Trophic level based only on food items
trophic_level_FB(taxa, type = "food items")
\dontshow{\}) # examplesIf}
}
\author{
Mattia Ghilardi, \email{mattia.ghilardi91@gmail.com}
}
