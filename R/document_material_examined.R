#' Generate a "Material Examined" Section for Taxonomic Descriptions
#'
#' This function processes taxonomic and specimen data to create a detailed, well-formatted "Material Examined" section for to match descriptions. It handles missing columns by automatically filling them with `"N/A"`,
#' ensuring smooth execution and consistent formatting, however, the function works best when the dataset includes all the specified columns. Specimens are grouped by the user specified group and region, and the
#' function outputs a comprehensive summary of specimens' metadata.
#'
#' @param data A data frame containing specimen metadata and taxonomic information.
#' @param grouped_by Character. The column name containing groups to summarize by (default: `"Species"`).
#' @param region_col Character. Column name for the region in which specimens were collected (default: `"Region"`).
#' @param institution_col Character. Column name for the institution or collection abbreviations (default: `"Institution"`).
#' @param number_col Character. Column name for specimen numbers (default: `"Number"`).
#' @param collector_col Character. Column name for the collector's name (default: `"Collector"`).
#' @param locality_col Character. Column name for locality names (default: `"Locality"`).
#' @param date_collected_col Character. Column name for collection dates (default: `"DateCollected"`).
#' @param type_col Character. Column name for specimen type designations (e.g., `"Holotype"`) (default: `"Type"`).
#' @param sex_col Character. Column name for sex information (default: `"Sex"`).
#' @param lat_col Character. Column name for latitude coordinates (default: `"Lat"`).
#' @param long_col Character. Column name for longitude coordinates (default: `"Long"`).
#' @param elevation_col Character. Column name for elevation information (default: `"Elevation"`).
#'
#' @details
#' The function checks for the presence of each column and automatically fills in missing columns with `"N/A"`. It groups specimens by group and region,
#' and generates a "Material Examined" section that lists details including collector, locality, geographic coordinates, and elevation. The output includes
#' a summary with the number of specimens for each group, followed by a detailed list of specimen information.
#'
#' @return Prints a "Material Examined" section for each group to the console, formatted with specimen details grouped by region.
#'
#' @examples
#' data(data_morphodata)
#' document_material_examined(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   lat_col = "Latitude",
#'   long_col = "Longitude")
#'
#' @export
document_material_examined <- function(data, grouped_by = "Species", region_col = "Region",
                                       institution_col = "Institution", number_col = "Number",
                                       collector_col = "Collector", locality_col = "Locality",
                                       date_collected_col = "DateCollected", type_col = "Type",
                                       sex_col = "Sex", lat_col = "Lat", long_col = "Long",
                                       elevation_col = "Elevation") {

  # Helper function to check if a column exists in the data
  column_exists <- function(col_name) col_name %in% names(data)

  # Define required columns
  required_columns <- c(grouped_by, region_col, institution_col, number_col,
                        collector_col, locality_col, date_collected_col, type_col,
                        sex_col, lat_col, long_col, elevation_col)

  # Identify missing columns
  missing_columns <- required_columns[!required_columns %in% colnames(data)]

  # Fill missing columns with "N/A"
  if (length(missing_columns) > 0) {
    warning(paste("The following columns are missing from the data and will be filled with 'N/A':", paste(missing_columns, collapse = ", ")))
    for (col in missing_columns) {
      data[[col]] <- "N/A"
    }
  }

  # Fill missing cells in the data with "N/A"
  data[is.na(data)] <- "N/A"
  data[data == ""] <- "N/A"  # Explicitly handle empty strings

  # Ensure numeric columns are properly converted
  numeric_cols <- c(lat_col, long_col, elevation_col)
  for (col in numeric_cols) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))  # Convert columns to numeric
  }

  # Ensure that the DateCollected column is properly converted to Date type
  if (column_exists(date_collected_col)) {
    data[[date_collected_col]] <- as.Date(data[[date_collected_col]], format = "%Y-%m-%d")
  }

  # Create the "Material Examined Text" for each row
  data <- data %>%
    mutate(
      Material_Examined_Text = paste0(
        if (column_exists(institution_col)) paste0(ifelse(is.na(!!sym(institution_col)) | !!sym(institution_col) == "", "N/A", !!sym(institution_col)), " ") else "N/A ",
        if (column_exists(number_col)) paste0(ifelse(is.na(!!sym(number_col)) | !!sym(number_col) == "", "N/A", !!sym(number_col)), " (",
                                              ifelse(column_exists(type_col) & !is.na(!!sym(type_col)), !!sym(type_col), "N/A"), "), ")
        else "N/A (N/A), ",
        if (column_exists(sex_col)) paste0(ifelse(is.na(!!sym(sex_col)) | !!sym(sex_col) == "", "N/A", !!sym(sex_col)), ", ") else "N/A, ",
        if (column_exists(collector_col)) paste0(ifelse(is.na(!!sym(collector_col)) | !!sym(collector_col) == "", "N/A", !!sym(collector_col)), ", ") else "N/A, ",
        if (column_exists(locality_col)) paste0(ifelse(is.na(!!sym(locality_col)) | !!sym(locality_col) == "", "N/A", !!sym(locality_col)), ", ") else "N/A, ",
        if (column_exists(date_collected_col)) paste0(ifelse(is.na(!!sym(date_collected_col)) | !!sym(date_collected_col) == "", "N/A", format(!!sym(date_collected_col), "%Y-%m-%d")), "; ") else "N/A; ",
        if (column_exists(lat_col) & column_exists(long_col))
          paste0(ifelse(is.na(!!sym(locality_col)) | !!sym(locality_col) == "", "", "; "),
                 ifelse(is.na(!!sym(lat_col)) | !!sym(lat_col) == "", "N/A", round(!!sym(lat_col), 6)), ", ",
                 ifelse(is.na(!!sym(long_col)) | !!sym(long_col) == "", "N/A", round(!!sym(long_col), 6)), ", ") else "; N/A, N/A, ",
        if (column_exists(elevation_col)) paste0(ifelse(is.na(!!sym(elevation_col)) | !!sym(elevation_col) == "", "N/A", !!sym(elevation_col)), "m") else "N/A m",
        "."
      )
    )

  # Group by the specified column and region to aggregate details
  material_examined_section <- data %>%
    group_by(across(all_of(grouped_by)), across(all_of(region_col))) %>%
    summarise(
      Region_Text = paste(Material_Examined_Text, collapse = " "),
      .groups = 'drop'
    )

  # Calculate the specimen count per grouping
  specimen_counts <- data %>%
    group_by(across(all_of(grouped_by))) %>%
    summarise(Specimen_Count = n_distinct(!!sym(number_col)), .groups = 'drop')

  # Merge specimen counts with the material examined data
  material_examined_section <- material_examined_section %>%
    left_join(specimen_counts, by = grouped_by)

  # Generate the output for each group
  for (group in unique(material_examined_section[[grouped_by]])) {
    group_data <- material_examined_section %>%
      filter(!!sym(grouped_by) == group)

    group_name <- paste0(group, " (n=", group_data$Specimen_Count[1], ").")

    # Start with the "Material examined" header for the current group
    output_text <- paste0("Material examined: ", group_name, " ")

    # Add details for each region
    region_details <- paste(
      group_data[[region_col]], ". ",
      group_data$Region_Text,
      sep = "",
      collapse = " "
    )

    output_text <- paste0(output_text, region_details)

    # Format the final output to match the desired format (commas, no extra semicolons, and correct placement)
    output_text <- gsub(" N/A", "N/A", output_text)  # Correct the ' N/A' format to 'N/A'
    output_text <- gsub(" ,", ",", output_text)      # Remove unwanted spaces before commas
    output_text <- gsub(" ;", ";", output_text)      # Remove unwanted spaces before semicolons
    output_text <- gsub(";;", ";", output_text)
    output_text <- gsub(",N/A", ", N/A", output_text)

    # Print the output for the group
    cat(output_text, "\n\n")
  }

  # Stop the function after printing all groups
  return(invisible(NULL))
}
