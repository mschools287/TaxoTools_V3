#' Identify Standout Traits Based on Range Comparisons
#'
#' This function identifies traits with significantly larger or smaller ranges compared to other groups in the dataset.
#' It computes the minimum and maximum values for each trait and compares the range of each group to the ranges of all others.
#'
#' @param data A data frame containing trait and grouping variable data.
#' @param grouped_by A character string specifying the grouping column (e.g., "Species", "Clade").
#' @param trait_columns A character vector of trait columns to analyze.
#' @param cutoff A numeric value between 0 and 1 (default: 2/3). Defines the threshold for standout traits.
#'
#' @return A data frame listing the traits that have standout ranges for each group, with the maximum and minimum values for each trait.
#' If no standout traits are identified at the specified cutoff a message is printed.
#'
#' @examples
#' data(data_morphodata)
#' document_standout_traits(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   trait_columns = c("trait1", "trait2"),
#'   cutoff = 2/3)
#'
#' @importFrom dplyr group_by summarise across ungroup mutate select
#' @export
document_standout_traits <- function(data, trait_columns, grouped_by = "Species", cutoff = 2/3) {

  # Check if the grouping column exists
  if (!grouped_by %in% colnames(data)) {
    stop(paste("The column", grouped_by, "is not present in the dataset. Please specify a valid column."))
  }

  # Check if the trait columns exist
  if (!all(trait_columns %in% colnames(data))) {
    missing_cols <- trait_columns[!trait_columns %in% colnames(data)]
    stop(paste("The following trait columns are missing:", paste(missing_cols, collapse = ", ")))
  }

  # Summarize data to calculate ranges
  summaries <- data %>%
    group_by(across(all_of(grouped_by))) %>%
    summarise(across(all_of(trait_columns),
                     list(min = ~ signif(min(., na.rm = TRUE), 3),
                          max = ~ signif(max(., na.rm = TRUE), 3)),
                     .names = "{.col}_{.fn}"),
              .groups = "drop")

  # Prepare diagnostic messages
  messages <- list()

  for (trait in trait_columns) {
    # Extract min and max for the current trait
    min_col <- paste0(trait, "_min")
    max_col <- paste0(trait, "_max")
    trait_ranges <- summaries %>% select(all_of(grouped_by), all_of(min_col), all_of(max_col))

    for (group in trait_ranges[[grouped_by]]) {
      # Get the range for the current group
      group_range <- trait_ranges %>%
        filter(!!sym(grouped_by) == group)

      group_min <- group_range[[min_col]]
      group_max <- group_range[[max_col]]

      # Compare with other groups
      other_ranges <- trait_ranges %>%
        filter(!!sym(grouped_by) != group)

      # Check if the group's range is fully larger or smaller than other groups (no overlap)
      larger_than <- sum(group_min > other_ranges[[max_col]] & group_max > other_ranges[[max_col]])
      smaller_than <- sum(group_max < other_ranges[[min_col]] & group_min < other_ranges[[min_col]])
      total_groups <- nrow(other_ranges)

      # Check if the group range is fully larger or smaller than the cutoff
      if (larger_than / total_groups >= cutoff) {
        percentage <- round((larger_than / total_groups) * 100, 0)  # Convert to percentage
        messages <- append(messages, paste0(
          group, " has a larger range in ", trait,
          " than ", percentage, "% of the other groups."
        ))
      }

      if (smaller_than / total_groups >= cutoff) {
        # Replace "1" with "all" when smaller_than is exactly 1
        smaller_message <- ifelse(smaller_than == total_groups,
                                  paste0(group, " has a smaller range in ", trait, " than all of the other groups."),
                                  paste0(group, " has a smaller range in ", trait,
                                         " than ", round((smaller_than / total_groups) * 100, 0), "% of the other groups."))
        messages <- append(messages, smaller_message)
      }
    }
  }

  # Check if no standout traits were found and add the message to the list
  if (length(messages) == 0) {
    messages <- append(messages, "No standout traits were found at the specified cutoff.")
  }

  # Return results
  list(
    range_summaries = summaries,
    diagnostic_messages = messages
  )
}


