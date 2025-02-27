#' Check Missing Data
#'
#' This function analyzes a dataset and provides a summary of missing data for each column.
#' It calculates both the raw count and the proportion of missing values for each trait.
#'
#' @param data A data frame to analyze for missing values.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Column} The name of each column in the dataset.
#'   \item \code{Missing_raw} The raw count of missing values in each column.
#'   \item \code{Missing_proportion} The proportion of missing values relative to the total number of rows in the dataset.}
#' }
#'
#' @examples
#' data(data_morphodata)
#' process_check_missing(data = data_morphodata)
#'
#' @export
process_check_missing <- function(data) {
  # Count missing values for each column
  missing_counts <- sapply(data, function(col) sum(is.na(col)))

  # Calculate the proportion of missing values for each column
  total_rows <- nrow(data)
  missing_proportion <- missing_counts / total_rows

  # Create a data frame with the updated column names and proportions
  missing_df <- data.frame(
    Column = names(missing_counts),
    Missing_raw = as.integer(missing_counts),
    Missing_proportion = round(missing_proportion, 4), # Round to 4 decimal places for readability
    row.names = NULL
  )

  return(missing_df)
}

