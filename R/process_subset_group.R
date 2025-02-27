#' Subset Dataset by Groups and Save to Environment
#'
#' This function subsets a dataset by unique values in a specified grouping column (e.g., "Species", "Clade")
#' and assigns each subset as a new variable in the R environment. The names of
#' the new variables are prefixed with a user-specified string.
#'
#' @param data A data frame to be subset.
#' @param grouped_by A character string specifying the column name used for grouping.
#' @param prefix A character string to prefix the names of the subset variables. Default is `"subset_"`.
#'
#' @return No direct return value. Subsets are saved as variables in the R environment.
#'
#' @examples
#' data(data_morphodata)
#' process_subset_group(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   prefix = "Subset_")
#'
#' @export
process_subset_group <- function(data, grouped_by, prefix = "subset_") {
  # Check if the grouped_by column exists in the dataset
  if (!(grouped_by %in% colnames(data))) {
    stop(paste("The specified 'grouped_by' column", grouped_by, "does not exist in the dataset."))
  }

  # Get unique groups
  unique_groups <- unique(data[[grouped_by]])

  # Loop through each group and assign subsets to the environment
  for (group in unique_groups) {
    # Subset the data for the current group
    subset_data <- data[data[[grouped_by]] == group, ]

    # Create a variable name for the subset
    var_name <- paste0(prefix, group)

    # Assign the subset to the global environment
    assign(var_name, subset_data, envir = .GlobalEnv)
  }
}
