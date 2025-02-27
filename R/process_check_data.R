#' Process Data Issues in Dataset
#'
#' This function checks for common data issues, including:
#' 1) Date formatting
#' 2) Coordinate formatting
#' 3) Coordinate validity
#' 4) Elevation formatting
#' 5) Missing metadata columns
#' 6) Missing trait values
#' 7) Character values in numeric (trait) columns
#'
#' @param data A data frame containing the dataset to process.
#' @param lat_col A string specifying the name of the column containing latitude values.
#' @param long_col A string specifying the name of the column containing longitude values.
#' @param elevation_col A string specifying the name of the column containing elevation values.
#' @param trait_columns A vector of strings specifying the names of columns containing trait data.
#' @param date_col A string specifying the name of the column containing date values. Defaults to NULL.
#'
#' @return This function does not return a value. It prints notifications to the console about any data issues found.
#'
#' @examples
#' # Bad example: Dataset with inconsistent date formats, invalid coordinates, inconsistent elevation values, and missing trait data.
#' data_bad <- data.frame(
#'   Species = c("Species A", "Species B", "Species C"),
#'   Date = c("01/01/2020", "2020-02-01", "March 3, 2020"),
#'   Latitude = c("12°34'56.78\" N", "34.9352", "100.00"),
#'   Longitude = c("78°34'56.78\" W", "12.5689", "200.00"),
#'   Elevation = c("100", "200ft", "300m"),
#'   trait1 = c("50mm", "unsure", "55mm"),
#'   trait2 = c("80cm", "damaged", "75mm"),
#'   trait3 = c("30mm", "40mm", "unsure"),
#'   trait4 = c(NA, NA, "45mm"))
#'
#' process_check_data(
#'   data = data_bad,
#'   lat_col = "Latitude",
#'   long_col = "Longitude",
#'   elevation_col = "Elevation",
#'   trait_columns = c("trait1", "trait2", "trait3", "trait4"),
#'   date_col = "Date")
#'
#' # Good example: Dataset with consistent date formats, valid coordinates, consistent elevation values, and less than 50% missing trait data.
#' data_good <- data.frame(
#'   Species = c("SpeciesA", "SpeciesB", "SpeciesC", "SpeciesD"),
#'   Institution = c("Institution1", "Institution2", "Institution3", "Institution4"),
#'   Number = c(1, 2, 3, 4),
#'   Country = c("USA", "Canada", "Mexico", "Brazil"),
#'   Region = c("North", "East", "South", "West"),
#'   Locality = c("Locality1", "Locality2", "Locality3", "Locality4"),
#'   Latitude = c(34.05, 45.42, -23.55, -15.78),
#'   Longitude = c(-118.25, -75.69, -46.63, -47.92),
#'   Elevation = c(100, 200, 300, 400),
#'   DateCollected = c("2023-05-10", "2023-06-15", "2023-07-20", "2023-08-25"),
#'   trait1 = c(1.5, 2.3, 3.1, 4.2),
#'   trait2 = c(0.5, 0.8, 1.0, 1.2),
#'   trait3 = c(NA, 2.5, 3.5, 4.5))
#'
#' process_check_data(
#'   data = data_good,
#'   lat_col = "Latitude",
#'   long_col = "Longitude",
#'   elevation_col = "Elevation",
#'   trait_columns = c("trait1", "trait2", "trait3"),
#'   date_col = "DateCollected")
#'
#' @export
process_check_data <- function(data, lat_col, long_col, elevation_col, trait_columns, date_col = NULL) {

  # 1) Date parsing: Standardize date columns into a consistent Date format
  check_date_parsing <- function(date_col) {
    if (!is.null(date_col) && !all(is.na(data[[date_col]]))) {
      # Attempt to parse dates in multiple formats
      parsed_dates <- as.Date(data[[date_col]], format = "%m/%d/%Y")  # Try MM/DD/YYYY format
      if (any(is.na(parsed_dates))) {
        parsed_dates <- as.Date(data[[date_col]], format = "%Y-%m-%d")  # Try YYYY-MM-DD format
      }
      if (any(is.na(parsed_dates))) {
        cat("Notification-Inconsistent date formats were found, please check the date formats.\n\n")
      } else {
        data[[date_col]] <- parsed_dates  # Update the column with parsed dates
        cat("Date column is standardized. :) \n\n")
      }
    } else {
      cat("No date column provided or all date values are missing.\n\n")
    }
  }

  # 2) Check if any coordinates are in Degrees, Minutes, Seconds (DMS) format
  check_dms <- function(lat_col, long_col) {
    lat_dms <- grep("°|d", data[[lat_col]], value = TRUE)
    long_dms <- grep("°|d", data[[long_col]], value = TRUE)

    if (length(lat_dms) > 0 || length(long_dms) > 0) {
      cat("Notification-Some coordinates are in Degrees, Minutes, Seconds (DMS) format. These should be converted to decimal degrees. \n\n")
    } else {
      cat("No DMS coordinates found. :) \n\n")
    }
  }

  # 3) Check for valid coordinates (latitude: -90 to 90, longitude: -180 to 180)
  check_valid_coordinates <- function(lat_col, long_col) {
    # Exclude NA values in latitude and longitude
    valid_lat <- !is.na(data[[lat_col]]) & (data[[lat_col]] >= -90 & data[[lat_col]] <= 90)
    valid_long <- !is.na(data[[long_col]]) & (data[[long_col]] >= -180 & data[[long_col]] <= 180)

    # Flag invalid coordinates
    invalid_lat <- !valid_lat
    invalid_long <- !valid_long

    if (any(invalid_lat) | any(invalid_long)) {
      cat("Notification-Some coordinates are outside the valid range.\n")
      cat("Latitude should be between -90 and 90, and Longitude should be between -180 and 180.\n\n")
      data[invalid_lat | invalid_long, c(lat_col, long_col)] <- NA  # Flag invalid coordinates
    } else {
      cat("All coordinates are within valid ranges. :) \n\n")
    }
  }

  # 4) Check if any text in the Elevation column and ensure consistency
  check_elevation <- function() {
    elevation_values <- data[[elevation_col]]
    m_values <- grep("m|meters", elevation_values, value = TRUE)
    ft_values <- grep("ft|feet", elevation_values, value = TRUE)

    if (length(m_values) > 0 && length(ft_values) > 0) {
      cat("Notification-Elevation contains both 'm/meters' and 'ft/feet'. This text needs to be removed and the values possibly need to be converted.\n\n")
    } else if (length(m_values) > 0) {
      cat("Notification-Elevation ends with 'm' or 'meters'. This text needs to be removed.\n\n")
    } else if (length(ft_values) > 0) {
      cat("Notification-Elevation ends with 'ft' or 'feet'. This text needs to be removed and the values possibly need to be converted.\n\n")
    } else {
      cat("Elevation values seem consistent. :) \n\n")
    }
  }

  # 5) Check for missing columns: Species, Institution, Number, Country, Region, Locality, Latitude, Longitude, Elevation
  check_missing_columns <- function() {
    required_columns <- c("Species", "Institution", "Number", "Country", "Region", "Locality",
                          "Latitude", "Longitude", "Elevation")
    missing_columns <- setdiff(required_columns, colnames(data))

    if (length(missing_columns) > 0) {
      if (length(missing_columns) > 2) {
        # Add Oxford comma for lists with more than two items
        missing_columns_text <- paste(
          paste(missing_columns[-length(missing_columns)], collapse = ", "),
          ", and ", missing_columns[length(missing_columns)], sep = ""
        )
      } else if (length(missing_columns) == 2) {
        # For exactly two items, use "and" without a preceding comma
        missing_columns_text <- paste(missing_columns, collapse = " and ")
      } else {
        # For a single item
        missing_columns_text <- missing_columns
      }
      # Add the "Notification-" prefix and ensure proper formatting
      cat(paste0("Notification-The following columns are not in the dataset: ", missing_columns_text,
                 ". They are not required, but some functions may perform better if these columns exist in your dataset, even if they are left blank.\n\n"))
    } else {
      cat("All required columns are present. :) \n\n")
    }
  }

  # 6) Check for high proportion of missing data in trait columns (more than 50%)
  check_missing_data_proportion <- function() {
    missing_proportion <- colSums(is.na(data[trait_columns])) / nrow(data)
    high_missing_columns <- names(missing_proportion[missing_proportion > 0.5])  # 50% threshold

    if (length(high_missing_columns) > 0) {
      if (length(high_missing_columns) > 2) {
        # Add Oxford comma for lists with more than two items
        high_missing_columns_text <- paste(
          paste(high_missing_columns[-length(high_missing_columns)], collapse = ", "),
          ", and ", high_missing_columns[length(high_missing_columns)], sep = ""
        )
      } else if (length(high_missing_columns) == 2) {
        # For exactly two items, use "and" without a preceding comma
        high_missing_columns_text <- paste(high_missing_columns, collapse = " and ")
      } else {
        # For a single item
        high_missing_columns_text <- high_missing_columns
      }
      cat(paste0("Notification-The following trait columns have more than 50% missing data: ",
                 high_missing_columns_text, ". Consider filling or removing these columns.\n\n"))
    } else {
      cat("No trait columns with more than 50% missing data. :) \n\n")
    }
  }

  # 7) Check for character values in trait columns
  check_character_values_in_traits <- function() {
    character_values <- sapply(data[trait_columns], function(x) any(is.character(x)))

    if (any(character_values)) {
      cat("Notification-Character values detected in trait columns. These should be numeric.\n\n")
    } else {
      cat("All trait columns contain only numeric values. :) \n\n")
    }
  }

  # Run checks
  check_date_parsing(date_col)
  check_dms(lat_col, long_col)
  check_valid_coordinates(lat_col, long_col)
  check_elevation()
  check_missing_columns()
  check_missing_data_proportion()
  check_character_values_in_traits()
}

