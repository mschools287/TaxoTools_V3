#' Standardize Morphological Traits by a Reference Trait
#'
#' This function standardizes multiple morphological traits relative to a reference trait, creating new columns with the standardized values.
#'
#' @param data A data frame containing the morphological data.
#' @param reference_trait A character string specifying the name of the reference trait to which others will be standardized.
#' @param traits_to_standardize A character vector of column names representing the morphological traits to be standardized.
#'
#' @return A data frame with the original data and new columns containing the standardized values of the specified traits. The new columns are named by appending "_standardized" to the original trait names.
#'
#' @details
#' The function divides each value in the \code{traits_to_standardize} columns by the corresponding value in the \code{reference_trait} column for each row. This creates new columns that show the standardized values of the traits relative to the reference trait.
#'
#' @examples
#' data(data_morphodata)
#' process_standardize_traits(
#'   data = data_morphodata,
#'   reference_trait = "trait1",
#'   traits_to_standardize = c("trait2", "trait3", "trait4", "trait5"))
#'
#' @importFrom dplyr mutate across
#' @export
process_standardize_traits <- function(data, reference_trait, traits_to_standardize) {
  # Standardize the traits relative to the reference trait
  data <- data %>%
    mutate(across(
      .cols = traits_to_standardize,
      .fns = ~ . / data[[reference_trait]],
      .names = "{.col}_standardized_{reference_trait}"
    ))
  return(data)
}

