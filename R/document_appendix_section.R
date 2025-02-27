#' Generate an "Appendix" Section for Groups
#'
#' This function generates a formatted appendix section with specimen details grouped by a specified column.
#' Missing columns in the dataset will be automatically filled with `"N/A"`, ensuring the function runs without errors and retains proper formatting, however, the function performs optimally when all specified columns are present in the dataset.
#'
#' @param data A data frame containing specimen data with columns for grouping, country, region, locality,
#' latitude, longitude, elevation, institution, and specimen number.
#' @param grouped_by Column name to group specimens by (default: `"Species"`).
#' @param country_code_col Column name for country code (default: `"CountryCode"`).
#' @param region_col Column name for region information (default: `"Region"`).
#' @param locality_col Column name for locality information (default: `"Locality"`).
#' @param lat_col Column name for latitude values (default: `"Lat"`).
#' @param long_col Column name for longitude values (default: `"Long"`).
#' @param elevation_col Column name for elevation values (default: `"Elevation"`), assumed to be in meters above sea level.
#' @param institution_col Column name for institution information (default: `"Institution"`).
#' @param number_col Column name for specimen number (default: `"Number"`).
#'
#' @return A formatted appendix section with specimen details grouped by the specified column. Missing values or columns will be printed as `"N/A"`.
#'
#' @examples
#' data(data_morphodata)
#' document_appendix_section(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   country_code_col = "Country",
#'   lat_col = "Latitude",
#'   long_col = "Longitude")
#'
#' @import dplyr
#' @export
document_appendix_section <- function(data, grouped_by = "Species", country_code_col = "CountryCode",
                                      region_col = "Region", locality_col = "Locality", lat_col = "Lat",
                                      long_col = "Long", elevation_col = "Elevation", institution_col = "Institution",
                                      number_col = "Number") {

  # Define required columns
  required_columns <- c(grouped_by, country_code_col, region_col, locality_col, lat_col, long_col, elevation_col, institution_col, number_col)

  # Identify missing columns
  missing_columns <- required_columns[!required_columns %in% colnames(data)]

  # Fill missing columns with "N/A"
  if (length(missing_columns) > 0) {
    warning(paste("The following columns are missing from the data and will be filled with 'N/A':", paste(missing_columns, collapse = ", ")))
    for (col in missing_columns) {
      data[[col]] <- "N/A"
    }
  }

  # Ensure numeric columns are properly converted
  numeric_cols <- c(lat_col, long_col, elevation_col)
  for (col in numeric_cols) {
    if (col %in% missing_columns) {
      data[[col]] <- NA  # Fill missing numeric columns with NA
    } else {
      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))  # Convert existing columns to numeric
    }
  }

  # Create the appendix section
  appendix <- data %>%
    group_by(across(all_of(c(grouped_by, country_code_col, region_col, locality_col, lat_col, long_col, elevation_col, institution_col)))) %>%
    summarise(
      Specimen_Numbers = paste(get(number_col), collapse = ", "),
      .groups = 'drop'
    ) %>%
    mutate(
      Specimen_Count = sapply(get(grouped_by), function(sp) length(unique(data[[number_col]][data[[grouped_by]] == sp]))),
      Appendix_Text = paste0(
        ifelse(get(institution_col) == "N/A", "N/A", get(institution_col)), " ", Specimen_Numbers,
        " (", ifelse(get(country_code_col) == "N/A", "N/A", get(country_code_col)), ", ",
        ifelse(get(region_col) == "N/A", "N/A", get(region_col)), ", ",
        ifelse(get(locality_col) == "N/A", "N/A", get(locality_col)),
        "; ", ifelse(is.na(get(lat_col)), "N/A", round(get(lat_col), 6)), ", ",
        ifelse(is.na(get(long_col)), "N/A", round(get(long_col), 6)),
        ", ", ifelse(is.na(get(elevation_col)), "N/A", get(elevation_col)), "m)"
      )
    ) %>%
    arrange(across(all_of(grouped_by))) %>%
    group_by(across(all_of(grouped_by))) %>%
    summarise(
      Appendix_Text = paste0(unique(Appendix_Text), collapse = ". "),
      Specimen_Count = unique(Specimen_Count),
      .groups = 'drop'
    )

  # Print the appendix
  for (group in unique(appendix[[grouped_by]])) {
    group_name <- paste0(group, " (n=", appendix$Specimen_Count[appendix[[grouped_by]] == group], ")")
    cat(paste0(group_name, ": "))
    group_text <- gsub("\\. $", ".", appendix$Appendix_Text[appendix[[grouped_by]] == group])
    if (substring(group_text, nchar(group_text), nchar(group_text)) != ".") {
      group_text <- paste0(group_text, ".")
    }
    cat(group_text, "\n\n")
  }
}
