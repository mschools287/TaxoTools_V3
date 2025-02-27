#' Calculate Geographic Range for Groups
#'
#' This function calculates the minimum and maximum latitude and longitude values for each species (or group) in the dataset,
#' and also counts the number of individuals per group.
#'
#' @param data A data frame containing species and geographic data.
#' @param grouped_by A string specifying the column name for grouping identifiers (e.g., "Species" or "Clade").
#' @param lat_column A string specifying the name of the column containing latitude values.
#' @param long_column A string specifying the name of the column containing longitude values.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{grouped_by}: Unique species or clades.
#'   \item \code{Count}: Number of individuals for the group.
#'   \item \code{Min_Lat}: Minimum latitude for the group.
#'   \item \code{Max_Lat}: Maximum latitude for the group.
#'   \item \code{Min_Long}: Minimum longitude for the group.
#'   \item \code{Max_Long}: Maximum longitude for the group.
#' }
#' @export
#'
#' @examples
#' data(data_morphodata)
#' geo_range(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   lat_column = "Latitude",
#'   long_column = "Longitude")
#' @export
geo_range <- function(data, grouped_by, lat_column, long_column) {

  # Ensure the specified 'grouped_by' column exists in the data
  if (!(grouped_by %in% colnames(data))) {
    stop(paste("The specified 'grouped_by' column", grouped_by, "does not exist in the data."))
  }

  # Ensure the specified 'lat_column' and 'long_column' exist in the data
  if (!(lat_column %in% colnames(data))) {
    stop(paste("The specified 'lat_column' column", lat_column, "does not exist in the data."))
  }
  if (!(long_column %in% colnames(data))) {
    stop(paste("The specified 'long_column' column", long_column, "does not exist in the data."))
  }

  # Filter data to include only rows with non-missing latitude and longitude
  valid_data <- data %>%
    dplyr::filter(!is.na(.data[[lat_column]]) & !is.na(.data[[long_column]]))

  # Group by the specified 'grouped_by' column and summarize the geographic range
  range_df <- valid_data %>%
    dplyr::group_by(.data[[grouped_by]]) %>%
    dplyr::summarise(
      Count = dplyr::n(),  # Count only valid records
      Min_Lat = format(min(!!rlang::sym(lat_column), na.rm = TRUE), digits = 8),
      Max_Lat = format(max(!!rlang::sym(lat_column), na.rm = TRUE), digits = 8),
      Min_Long = format(min(!!rlang::sym(long_column), na.rm = TRUE), digits = 8),
      Max_Long = format(max(!!rlang::sym(long_column), na.rm = TRUE), digits = 8),
      .groups = "drop"
    )

  return(range_df)
}
