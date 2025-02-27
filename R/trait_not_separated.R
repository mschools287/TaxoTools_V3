#' Identify Groups That Cannot Be Distinguished by Any Trait
#'
#' This function identifies groups (e.g., "Species", "Clade") that cannot be distinguished from each other based on the given morphological traits.
#' It compares the minimum and maximum values for each trait across groups and checks if there is any overlap in the ranges.
#' If overlap is found for any trait, the pair is considered indistinguishable by that trait.
#'
#' @param data A data frame containing group names and their respective trait values.
#' @param trait_columns A character vector specifying the columns of traits to be compared between groups.
#' @param grouped_by A character vector specifying the column with group designations (e.g., "Species", "Clade").
#'
#' @details
#' The function works by calculating the minimum and maximum values for each trait per group, then comparing these ranges across groups.
#' If the ranges of a particular trait overlap between two groups, the pair is considered non-separated by that trait. The function prints the
#' groups that could not be separated by any trait in pairs.
#'
#' @return The function does not return any value but prints a list of groups that could not be separated by any trait.
#'
#' @examples
#' data(data_morphodata)
#' trait_not_separated(
#'   data = data_morphodata,
#'   trait_columns = c("trait1", "trait2"),
#'   grouped_by = "Species")
#'
#' @import dplyr
#' @export
trait_not_separated <- function(data, trait_columns, grouped_by) {
  # Ensure required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but is not installed. Please install it.")
  }

  # Ensure the specified 'grouped_by' column exists in the data
  if (!(grouped_by %in% colnames(data))) {
    stop(paste("The specified 'grouped_by' column", grouped_by, "does not exist in the data."))
  }

  # Calculate the min and max for each trait by the grouping column
  range_check <- data %>%
    dplyr::select(dplyr::all_of(grouped_by), dplyr::all_of(trait_columns)) %>%
    dplyr::group_by(.data[[grouped_by]]) %>%
    dplyr::summarise(dplyr::across(
      .cols = dplyr::everything(),
      .fns = list(Min = ~ ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)),
                  Max = ~ ifelse(all(is.na(.)), NA, max(., na.rm = TRUE))),
      .names = "{.col}_{.fn}"
    )) %>%
    dplyr::ungroup()

  # Initialize a list for non-separated pairs
  non_separated_pairs <- list()

  # Loop through each pair of groups and compare their min-max ranges
  group_combinations <- combn(unique(range_check[[grouped_by]]), 2, simplify = FALSE)

  for (pair in group_combinations) {
    group_a <- pair[1]
    group_b <- pair[2]

    non_overlap_a_to_b <- list()
    non_overlap_b_to_a <- list()

    for (trait in trait_columns) {
      # Extract min and max values for both groups
      min_a <- range_check[range_check[[grouped_by]] == group_a, paste0(trait, "_Min")] %>% as.numeric()
      max_a <- range_check[range_check[[grouped_by]] == group_a, paste0(trait, "_Max")] %>% as.numeric()
      min_b <- range_check[range_check[[grouped_by]] == group_b, paste0(trait, "_Min")] %>% as.numeric()
      max_b <- range_check[range_check[[grouped_by]] == group_b, paste0(trait, "_Max")] %>% as.numeric()

      # Skip comparison if any value is missing
      if (is.na(min_a) || is.na(max_a) || is.na(min_b) || is.na(max_b)) next

      # Check for non-overlapping ranges
      if (max_a < min_b || max_b < min_a) {
        description_a_to_b <- paste0(
          trait, " (",
          ifelse(min_a == max_a, round(min_a, 3), paste0(round(min_a, 3), "-", round(max_a, 3))),
          " vs ",
          ifelse(min_b == max_b, round(min_b, 3), paste0(round(min_b, 3), "-", round(max_b, 3))),
          ")"
        )

        description_b_to_a <- paste0(
          trait, " (",
          ifelse(min_b == max_b, round(min_b, 3), paste0(round(min_b, 3), "-", round(max_b, 3))),
          " vs ",
          ifelse(min_a == max_a, round(min_a, 3), paste0(round(min_a, 3), "-", round(max_a, 3))),
          ")"
        )

        # Avoid duplicates
        if (!description_a_to_b %in% non_overlap_a_to_b) {
          non_overlap_a_to_b <- c(non_overlap_a_to_b, description_a_to_b)
        }
        if (!description_b_to_a %in% non_overlap_b_to_a) {
          non_overlap_b_to_a <- c(non_overlap_b_to_a, description_b_to_a)
        }
      }
    }

    # If no non-overlapping traits were found, add to non_separated_pairs
    if (length(non_overlap_a_to_b) == 0 && length(non_overlap_b_to_a) == 0) {
      non_separated_pairs <- c(non_separated_pairs, paste(group_a, "vs", group_b))
    }
  }

  # Print the result in the desired format
  if (length(non_separated_pairs) > 0) {
    cat("The following pairs could not be separated by any trait:\n")
    for (pair in non_separated_pairs) {
      cat(pair, "\n")
    }
  } else {
    cat("All pairs can be distinguished by at least one trait.\n")
  }

  # Return NULL so that the list is not printed
  return(invisible(NULL))
}
