#' Morphological Data
#'
#' A dataset containing morphological traits and metadata for a variety of specimens.
#' This dataset is used for analyzing and documenting variation across groups.
#'
#' @format A data frame with the following columns:
#' \itemize{
#'   \item \code{Clade} {Character. The clade name or designation.}
#'   \item \code{Species} {Character. The species name or designation.}
#'   \item \code{Species_updated} {Character. An updated or corrected name, if applicable.}
#'   \item \code{Institution} {Character. The institution responsible for the specimen.}
#'   \item \code{Number} {Integer. A unique identifier for the specimen.}
#'   \item \code{Country} {Character. The country where the specimen was found.}
#'   \item \code{Region} {Character. The region of the country where the specimen was found.}
#'   \item \code{Locality} {Character. The specific locality within the country where the specimen was found.}
#'   \item \code{Latitude} {Numeric. The latitude of the specimen's location.}
#'   \item \code{Longitude} {Numeric. The longitude of the specimen's location.}
#'   \item \code{Elevation} {Numeric. The elevation of the specimen's location in meters.}
#'   \item \code{trait1-trait8} {Numeric. The first eight trait measurements, designed to represent continuious variables.}
#'   \item \code{trait9} {Numeric. The ninth trait measurement, designed to represent a categorical variable.}
#'   \item \code{trait10} {Numeric. The tenth trait measurement, designed to represent a binary variable.}
#' }
#'
#' @details
#' The `data_morphodata` dataset contains morphological measurements for different specimens, which are
#' useful for comparative analysis and visualization of trait variation across groups (e.g., "species", "clades"). Trait9 shows
#' how a categorical variable may be scored and trait10 shows
#' how a binary trait may be scored.
#'
#' @examples
#' data(data_morphodata)
#' head(data_morphodata)
"data_morphodata"

