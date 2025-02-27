#' Filter Individuals by Trait Threshold
#'
#' This function filters a dataset to identify individuals that meet a specified threshold
#' for a given trait. The filtering can be done for values above or below the threshold.
#'
#' @param data A data frame containing the individual and trait data.
#' @param individuals_column A string specifying the name of the column containing unique individual identifiers.
#' @param trait_column A string specifying the name of the column containing the trait values.
#' @param threshold A numeric value specifying the threshold for filtering.
#' @param above A logical value. If TRUE, filters individuals with trait values above the threshold.
#'        If FALSE, filters individuals with trait values below the threshold. Default is TRUE.
#'
#' @return A vector of individual identifiers that meet the specified trait threshold.
#' @export
#'
#' @examples
#' # Filter individuals with trait values below 8
#' data(data_morphodata)
#' trait_filter(
#'   data = data_morphodata,
#'   individuals_column = "Species_updated",
#'   trait_column = "trait1",
#'   threshold = 8,
#'   above = FALSE)
#'
#' @export
trait_filter <- function(data, individuals_column, trait_column, threshold, above = TRUE) {
  if (above) {
    filtered_data <- data %>% filter(.data[[trait_column]] > threshold)
  } else {
    filtered_data <- data %>% filter(.data[[trait_column]] < threshold)
  }
  return(filtered_data[[individuals_column]])
}
