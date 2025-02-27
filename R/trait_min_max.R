#' Identify Groups with Minimum and Maximum Trait Values
#'
#' This function identifies the groups with the minimum and maximum values for a specified trait, returning the groups and their corresponding trait values. If multiple groups share the minimum or maximum value, all of them will be included in the result.
#'
#' @param data A data frame containing group data with columns for names and trait values.
#' @param grouped_by A character string specifying the name of the column containing group identifiers.
#' @param trait_column A character string specifying the name of the column containing the trait values to be analyzed.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{Min_Group} A data frame containing the groups with the minimum trait value and the corresponding trait value.
#'   \item \code{Max_Group} A data frame containing the groups with the maximum trait value and the corresponding trait value.
#' }
#'
#' @details
#' The function uses the \code{min} and \code{max} functions to find the groups with the smallest and largest trait values, respectively. In the event of ties (i.e., multiple groups with identical minimum or maximum trait values), all groups with the tied values are returned. The result is a list containing two data frames: one for the groups with the minimum value and another for those with the maximum value.
#'
#' @examples
#' data(data_morphodata)
#' trait_min_max(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   trait_column = "trait1")
#'
#' @importFrom dplyr filter select rename
#' @export
trait_min_max <- function(data, grouped_by, trait_column) {
  min_species <- data %>%
    filter(.data[[trait_column]] == min(.data[[trait_column]], na.rm = TRUE)) %>%
    select(.data[[grouped_by]], .data[[trait_column]]) %>%
    rename(Group = .data[[grouped_by]], Trait_Value = .data[[trait_column]])

  max_species <- data %>%
    filter(.data[[trait_column]] == max(.data[[trait_column]], na.rm = TRUE)) %>%
    select(.data[[grouped_by]], .data[[trait_column]]) %>%
    rename(Group = .data[[grouped_by]], Trait_Value = .data[[trait_column]])

  return(list(Min_Group = min_species, Max_Group = max_species))
}
