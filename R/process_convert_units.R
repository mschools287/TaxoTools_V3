#' Convert Trait Units and Add a New Column
#'
#' This function applies a specified conversion factor to the values of a chosen trait column
#' and then adds the converted values as a new column in the dataset.
#'
#' @param data A data frame containing the trait data.
#' @param trait_column A character string specifying the name of the column to convert.
#' @param conversion_factor A numeric value to multiply the trait column values by for conversion.
#'
#' @details
#' The function generates a new column in the dataset with the converted trait values.
#' The name of the new column is derived from the original trait column name,
#' appended with \code{"_converted"}.
#'
#' @return A modified data frame containing the original data and the new column with converted values.
#'
#' @examples
#' # Convert trait1 from cm to mm (conversion factor: 10)
#' data(data_morphodata)
#' process_convert_units(
#'   data = data_morphodata,
#'   trait_column = "trait1",
#'   conversion_factor = 10)
#'
#' @export
process_convert_units <- function(data, trait_column, conversion_factor) {
  # Check if the trait_column exists in the data
  if (!(trait_column %in% colnames(data))) {
    stop(paste("The specified 'trait_column' column", trait_column, "does not exist in the data."))
  }

  # Create the new column name with the conversion factor included
  new_column_name <- paste0(trait_column, "_converted_", conversion_factor)

  # Apply the conversion
  data[[new_column_name]] <- data[[trait_column]] * conversion_factor

  # Return the updated data
  return(data)
}
