#' Summarize Trait Ranges by Grouping Variable
#'
#' This function calculates the minimum and maximum values (or single value) for each specified trait,
#' grouped by a specified column (e.g., "Species", "Clade"). It can optionally include the average value
#' and the count of observations for each trait.
#'
#' @param data A data frame containing the data to be analyzed.
#' @param grouped_by A character string specifying the name of the column containing the grouping variable (default: `"Species"`).
#' @param trait_columns A character vector of trait column names for which the range or single value will be summarized.
#' @param include_counts Logical. If `TRUE`, includes the count of non-missing observations for each trait (default: `FALSE`).
#' @param include_averages Logical. If `TRUE`, includes the average value for each trait (default: `FALSE`).
#'
#' @return A data frame with one row per group, containing the range (or single value) for each trait, and optionally the average value and/or the count of non-missing observations.
#' Prior to publishing, all en dashes in ranges should be replaced with em dashes.
#'
#' @details
#' The function calculates the minimum and maximum values for each trait within each grouping variable. If the minimum and maximum are the same, it returns a single value. If all values for a trait are missing, it returns `"N/A"`.
#'
#' If `include_averages = TRUE`, the average value for each trait is appended in parentheses. If `include_counts = TRUE`, the count of non-missing observations is appended in parentheses.
#'
#' @examples
#' data(data_morphodata)
#' document_trait_range_table(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   trait_columns = c("trait1", "trait2", "trait3"))
#' document_trait_range_table(
#'   data = data_morphodata,
#'   grouped_by = "Clade",
#'   trait_columns = c("trait1", "trait2", "trait3"),
#'   include_counts = TRUE,
#'   include_averages = TRUE)
#'
#' @importFrom dplyr group_by summarize across all_of ungroup relocate n select
#' @export
document_trait_range_table <- function(data, grouped_by, trait_columns, include_counts = FALSE, include_averages = FALSE) {
  # Helper function to calculate min-max, average, and optional specimen count
  min_max_table <- function(x) {
    valid_count <- sum(!is.na(x))  # Count of non-NA values
    if (valid_count == 0) {
      return("N/A")  # Return "N/A" if all values are NA
    }
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    avg_val <- mean(x, na.rm = TRUE)

    # Construct output based on options
    if (min_val == max_val) {
      result <- as.character(min_val)  # Single value if min == max
    } else {
      result <- paste0(min_val, "-", max_val)  # Range if min != max
    }

    # Append average and/or count based on user options
    if (include_averages) {
      result <- paste0(result, " (", round(avg_val, 2), ")")  # Add average
    }

    if (include_counts) {
      result <- paste0(result, " (n=", valid_count, ")")  # Add count
    }

    return(result)
  }

  # Apply the helper function to each trait grouped by the specified grouping variable and optionally include counts
  trait_summary <- data %>%
    dplyr::group_by(.data[[grouped_by]]) %>%
    dplyr::summarize(
      across(
        .cols = dplyr::all_of(trait_columns),
        .fns = min_max_table,
        .names = "{.col}_range"
      )
    ) %>%
    dplyr::ungroup()

  # Optionally add the count column if counts are included
  if (include_counts) {
    counts <- data %>%
      dplyr::group_by(.data[[grouped_by]]) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      dplyr::ungroup()
    trait_summary <- trait_summary %>%
      dplyr::left_join(counts, by = grouped_by) %>%
      dplyr::relocate(count, .after = grouped_by)  # Move the count column
  }

  return(trait_summary)
}
