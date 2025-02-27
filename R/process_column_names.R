#' Rename Columns in a Dataset
#'
#' This function allows you to rename all columns in a given dataset. It ensures that the number of
#' new column names matches the number of columns in the dataset before applying the changes.
#'
#' @param data A data frame whose columns need to be renamed.
#' @param new_column_names A character vector containing the new column names. The length of this vector
#'                         should match the number of columns in the dataset.
#'
#' @return The updated data frame with renamed columns.
#'
#' @examples
#' # Rename columns in the dataset
#' process_column_names(
#'   data_morphodata,
#'   new_column_names = c("Clade", "Species", "Species Updated",
#'    "Institution", "Number", "Country", "Region", "Locality",
#'    "Latitude", "Longitude", "Elevation", "Trait 1", "Trait 2",
#'        "Trait 3", "Trait 4", "Trait 5", "Trait 6", "Trait 7",
#'        "Trait 8", "Trait 9", "Trait 10"))
#'
#' @note The number of new column names must exactly match the number of columns in the data frame.
#'       If the lengths do not match, the function will print an error message and not perform any changes.
#' @export
process_column_names <- function(data, new_column_names) {
  # Ensure the length of new_column_names matches the number of columns in the data
  if (length(new_column_names) == ncol(data)) {
    colnames(data) <- new_column_names
    cat("Columns have been successfully renamed.\n")
  } else {
    cat("Error: The number of new column names does not match the number of columns in the dataset.\n")
  }
  return(data)
}

