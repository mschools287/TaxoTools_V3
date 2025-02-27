#' Generate "Diagnoses" Sections Based on Trait Ranges
#'
#' This function generates diagnostic descriptions distinguishing groups based on non-overlapping ranges of specified traits. It compares the minimum and maximum values for each trait across groups and produces a summary of traits where the ranges do not overlap.
#'
#' @param data A data frame containing specimen data, with columns for group designations (e.g., "Species", "Clade", ect.) and trait values.
#' @param trait_columns A character vector of column names corresponding to the traits to be analyzed for non-overlapping ranges.
#' @param grouped_by A character string specifying the column name used to group specimens.
#'
#' @return A list of character strings, each representing a description that distinguishes two groups based on non-overlapping trait ranges. If no non-overlapping ranges are found, a message is printed.
#'
#' @details
#' The function calculates the minimum and maximum values for each trait by group, compares these ranges across groups, and generates descriptions for trait comparisons that do not overlap. If no non-overlapping ranges are found, a message will indicate this and list the groups that could not be separated.
#' Categorical and binary traits will be printed as a range if multiple values are present within the same group. Prior to publishing, all en dashes in ranges should be replaced with em dashes.
#'
#' @examples
#' data(data_morphodata)
#' document_diagnoses_section(
#'   data = data_morphodata,
#'   trait_columns = c("trait1", "trait2"),
#'   grouped_by = "Species")
#'
#' @importFrom dplyr select group_by summarise across ungroup
#' @importFrom purrr map_chr
#' @importFrom rlang .data
#' @export
document_diagnoses_section <- function(data, trait_columns, grouped_by) {
  # Ensure required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but is not installed. Please install it.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("The 'purrr' package is required but is not installed. Please install it.")
  }

  # Ensure the specified 'grouped_by' column exists in the data
  if (!(grouped_by %in% colnames(data))) {
    stop(paste("The specified 'grouped_by' column", grouped_by, "does not exist in the data."))
  }

  # Calculate the min and max for each trait by the grouping column
  range_check <- data %>%
    dplyr::select(!!rlang::sym(grouped_by), dplyr::all_of(trait_columns)) %>%
    dplyr::group_by(!!rlang::sym(grouped_by)) %>%
    dplyr::summarise(dplyr::across(
      .cols = dplyr::everything(),
      .fns = list(Min = ~ ifelse(all(is.na(.)), NA, min(., na.rm = TRUE)),
                  Max = ~ ifelse(all(is.na(.)), NA, max(., na.rm = TRUE))),
      .names = "{.col}_{.fn}"
    )) %>%
    dplyr::ungroup()

  # Initialize a list to hold descriptions and a list for non-separated pairs
  descriptions <- list()
  non_separated_pairs <- list()

  # Loop through each grouping pair and compare their min-max ranges
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

    # Add descriptions for valid comparisons
    if (length(non_overlap_a_to_b) > 0) {
      description_text_a_to_b <- if (length(non_overlap_a_to_b) > 1) {
        paste0(
          "From ", group_a, ", we distinguish ", group_b, " by ",
          paste(non_overlap_a_to_b[-length(non_overlap_a_to_b)], collapse = ", "), ", and ",
          non_overlap_a_to_b[length(non_overlap_a_to_b)], "."
        )
      } else {
        paste0(
          "From ", group_a, ", we distinguish ", group_b, " by ", non_overlap_a_to_b[[1]], "."
        )
      }
      descriptions[[paste(group_a, "and", group_b)]] <- description_text_a_to_b
    }

    if (length(non_overlap_b_to_a) > 0) {
      description_text_b_to_a <- if (length(non_overlap_b_to_a) > 1) {
        paste0(
          "From ", group_b, ", we distinguish ", group_a, " by ",
          paste(non_overlap_b_to_a[-length(non_overlap_b_to_a)], collapse = ", "), ", and ",
          non_overlap_b_to_a[length(non_overlap_b_to_a)], "."
        )
      } else {
        paste0(
          "From ", group_b, ", we distinguish ", group_a, " by ", non_overlap_b_to_a[[1]], "."
        )
      }
      descriptions[[paste(group_b, "and", group_a)]] <- description_text_b_to_a
    }

    # If no non-overlapping traits were found, add to non_separated_pairs
    if (length(non_overlap_a_to_b) == 0 && length(non_overlap_b_to_a) == 0) {
      non_separated_pairs <- c(non_separated_pairs, paste(group_a, "vs", group_b))
    }
  }

  if (length(descriptions) == 0) {
    cat("No non-overlapping ranges found between any groups.\n")
  } else {
    for (desc in descriptions) {
      cat(desc, "\n\n")
    }
  }

  if (length(non_separated_pairs) > 0) {
    cat("\nThe following groups could not be separated by any trait:\n")
    for (pair in non_separated_pairs) {
      cat(pair, "\n")
    }
  }

  # Invisibly return the descriptions for further use if needed
  invisible(descriptions)
}
