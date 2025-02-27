#' Generate "Description" Sections for Groups
#'
#' This function generates description sections for each group based on the minimum and maximum values for a set of traits. The descriptions are formatted for each group, listing trait ranges or single values.
#'
#' @param data A data frame containing specimen data, with columns for group designations (e.g., "Species", "Clade", ect.) and trait values.
#' @param trait_columns A character vector of column names corresponding to the traits to summarize.
#' @param grouped_by A character string specifying the column name used to group specimens (default: `"Species"`).
#'
#' @return A character vector of group descriptions, each formatted to list the traits and their corresponding range or value.
#'
#' @details
#' The function computes the minimum and maximum values for each trait and constructs a description for each group that includes the ranges or single values of the traits. If a trait has missing values for all individuals in a group, it will be marked as `"N/A"`. Ranges are reported as `"min-max"`, and single values are reported as is. Categorical and binary traits will be printed as a range if multiple values are present within the same species.
#' Prior to publishing, all en dashes in ranges should be replaced with em dashes.
#'
#' @examples
#' data(data_morphodata)
#' document_description_section(
#'   data = data_morphodata,
#'   trait_columns = c("trait1", "trait2", "trait3", "trait4"),
#'   grouped_by = "Clade")
#'
#' @importFrom dplyr group_by summarise across ungroup rowwise mutate
#' @importFrom purrr walk
#' @importFrom rlang c_across
#' @export
document_description_section <- function(data, trait_columns, grouped_by = "Species") {
  # Check if the specified grouping column exists in the data
  if (!grouped_by %in% colnames(data)) {
    stop(paste("The column", grouped_by, "is not present in the dataset. Please specify a valid column."))
  }

  # Summarize the data to get a single value or range for each trait per grouping
  summaries <- data %>%
    group_by(!!sym(grouped_by)) %>%
    summarise(across(all_of(trait_columns),
                     ~ if (all(is.na(.))) {
                       "N/A"  # Explicitly mark missing traits as "N/A"
                     } else {
                       min_val <- min(., na.rm = TRUE)
                       max_val <- max(., na.rm = TRUE)

                       if (is.na(min_val) | is.na(max_val)) {
                         "N/A"  # If range calculation fails due to missing values
                       } else if (min_val == max_val) {
                         as.character(signif(min_val, 3))
                       } else {
                         paste0(signif(min_val, 3), "-", signif(max_val, 3))  # Use em dash for range
                       }
                     },
                     .names = "range_{col}")) %>%
    ungroup()

  # Create descriptions for each grouping
  descriptions <- summaries %>%
    rowwise() %>%
    mutate(description = {
      trait_values <- c_across(starts_with("range_"))

      # Ensure missing values are represented as "N/A" in the output
      trait_descriptions <- paste0(
        "(", seq_along(trait_columns), ") ",
        trait_columns, ", ",
        ifelse(trait_values == "", "N/A", trait_values)
      )

      # Construct sentence with "and" before the last trait
      full_description <- paste0(
        trait_descriptions[-length(trait_descriptions)],
        collapse = ", "
      )
      full_description <- paste0(full_description,
                                 ", and ",
                                 trait_descriptions[length(trait_descriptions)])

      paste0("Members of ", !!sym(grouped_by), " have ", full_description, ".")
    }) %>%
    pull(description)

  # Print each description with spacing
  purrr::walk(descriptions, function(desc) {
    cat(desc, "\n\n")
  })
}
