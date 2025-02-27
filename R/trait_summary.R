#' Calculate Summary Statistics for a Trait
#'
#' This function calculates the mean, median, and standard deviation for a specified trait, grouped by a grouping variable (e.g., "species", "clade").
#'
#' @param data A data frame containing the data to be analyzed.
#' @param grouped_by A character string specifying the name of the column containing the grouping variable.
#' @param trait_column A character string specifying the name of the column containing the trait data to summarize.
#'
#' @return A data frame with one row per group, containing the mean, median, and standard deviation for the specified trait.
#'
#' @details
#' The function groups the data by the specified grouping variable and computes summary statistics (mean, median, and standard deviation) for the specified trait. Missing values are ignored in the calculations.
#'
#' @examples
#' data(data_morphodata)
#' trait_summary(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   trait_column = "trait1")
#'
#' @importFrom dplyr group_by summarise
#' @export
trait_summary <- function(data, grouped_by, trait_column) {
  summary_stats <- data %>%
    group_by(.data[[grouped_by]]) %>%
    summarise(
      Mean = mean(.data[[trait_column]], na.rm = TRUE),
      Median = median(.data[[trait_column]], na.rm = TRUE),
      SD = sd(.data[[trait_column]], na.rm = TRUE),
      .groups = "drop"
    )
  return(summary_stats)
}
