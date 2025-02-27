#' Filter Out Juvenile Specimens Based on Trait Size
#'
#' This function removes individuals from a dataset that do not fall within a given percentage
#' of the largest specimen's trait value in each group (e.g., "species", "clade", ect.). It helps
#' filter out potential juveniles based on a specified trait.
#'
#' @param data A data frame containing morphological measurements.
#' @param trait A character string specifying the column name of the trait used for filtering (e.g., body size).
#' @param group_by A character string specifying the column name to group by.
#' @param cutoff A numeric value (default = 0.50) representing the proportion of the maximum trait
#' value in each group that individuals must meet or exceed to be retained. For example, a `cutoff = 0.90`
#' removes individuals that are smaller than 90% of the largest individual in their group.
#'
#' @return A list containing:
#' \item{filtered_data}{A data frame with only individuals that meet the cutoff criteria.}
#' \item{removed_summary}{A data frame summarizing the number of individuals removed per group.}
#'
#' @examples
#' # Remove individuals that are less than 85% of the largest trait1 value per species
#' process_juveniles(data_morphodata, trait = "trait1", group_by = "Species", cutoff = 0.85)
#'
#' @export
process_juveniles <- function(data, trait, group_by, cutoff = 0.50) {
  # Check if the provided columns exist in the dataset
  if (!group_by %in% colnames(data)) {
    stop(paste("The column", group_by, "is not present in the dataset. Please specify a valid grouping column."))
  }
  if (!trait %in% colnames(data)) {
    stop(paste("The trait column", trait, "is not present in the dataset. Please specify a valid trait column."))
  }

  # Compute the max trait value for each group
  max_values <- data %>%
    group_by(across(all_of(group_by))) %>%
    summarise(max_trait = max(!!sym(trait), na.rm = TRUE), .groups = "drop")

  # Merge max values back to the original dataset
  data_with_max <- data %>%
    left_join(max_values, by = group_by)

  # Apply the cutoff: Keep individuals that are at least (cutoff * max) in their group
  filtered_data <- data_with_max %>%
    filter(!!sym(trait) >= (cutoff * max_trait)) %>%
    select(-max_trait)  # Remove extra column after filtering

  # Count removed individuals per group
  removed_counts <- data_with_max %>%
    mutate(removed = !!sym(trait) < (cutoff * max_trait)) %>%
    group_by(across(all_of(group_by))) %>%
    summarise(removed = sum(removed, na.rm = TRUE), .groups = "drop")

  # Return the filtered dataset and the summary of removed individuals
  list(
    filtered_data = filtered_data,
    removed_summary = removed_counts
  )
}
