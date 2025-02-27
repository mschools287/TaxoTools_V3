#' Count the Number of Occurrences per Group
#'
#' This function counts the number of occurrences (specimens) for each group (e.g., "species", "clade") in a dataset.
#' It also computes a total count for all groups combined.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param grouped_by A character string specifying the name of the column containing the grouping variable.
#'
#' @return A data frame with two columns:
#' \itemize{
#'   \item \code{Group}: The group names (e.g., "species", "clade").
#'   \item \code{Specimen_Count}: The number of occurrences (specimens) for each group. An additional row labeled "Total" is included at the end, representing the sum of all specimen counts.}
#' }
#'
#' @details
#' The function groups the data by the specified grouping variable and counts the number of occurrences for each group.
#' It then adds a "Total" row with the sum of all the counts. The output data frame contains the group names, their corresponding counts, and the total count at the end.
#'
#' @examples
#' data(data_morphodata)
#' process_specimen_count(
#'   data = data_morphodata,
#'   grouped_by = "Species")
#'
#' @importFrom dplyr group_by summarize add_row
#' @export
process_specimen_count <- function(data, grouped_by) {
  # Count the number of occurrences for each group
  specimen_counts <- data %>%
    group_by(.data[[grouped_by]]) %>%
    summarize(count = n(), .groups = "drop")

  # Rename the columns for clarity
  colnames(specimen_counts) <- c("Group", "Specimen_Count")

  # Calculate the total count and add it as a final row
  total_count <- sum(specimen_counts$Specimen_Count)
  specimen_counts <- specimen_counts %>%
    add_row(Group = "Total", Specimen_Count = total_count)

  return(specimen_counts)
}

