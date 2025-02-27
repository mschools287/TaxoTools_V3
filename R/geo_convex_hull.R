#' Calculate Minimum Convex Hull Area for Each Group
#'
#' This function calculates the area of the minimum convex hull for each group (e.g., "Species" or "Clades") based on
#' their geographic coordinates (latitude and longitude). The areas are returned in a data frame with group names
#' and their convex hull areas.
#'
#' @param data A data frame containing occurrence data with latitude and longitude coordinates, and a column for group identifiers.
#' @param area_unit A string specifying the unit for area calculation. Defaults to "km^2", but other options include "m^2".
#' @param grouped_by A string specifying the column name for grouping identifiers (e.g., "Species", "Clade").
#' @param lat_column A string specifying the column name for latitude values.
#' @param long_column A string specifying the column name for longitude values.
#'
#' @return A data frame with three columns:
#' \itemize{
#'   \item \code{grouped_by}: The name of the group (e.g., "Species", "Clade").
#'   \item \code{Convex_Hull_Area}: The area of the minimum convex hull for each group in the specified unit.
#'   \item \code{geometry}: The geometry of the minimum convex hull for each group.
#' }
#'
#' @details
#' Groups with fewer than three occurrences are excluded from the calculation. The \code{sf} package is used to calculate
#' convex hulls and their areas. The \code{geometry} column contains the spatial geometry of the convex hull, which can be
#' used for further spatial analysis or visualization.
#'
#' @import dplyr
#' @importFrom sf st_as_sf st_convex_hull st_area st_union
#' @export
#'
#' @examples
#' data(data_morphodata)
#' geo_convex_hull(
#'   data = data_morphodata,
#'   area_unit = "km^2",
#'   grouped_by = "Clade",
#'   lat_column = "Latitude",
#'   long_column = "Longitude")
#'
#' @export
geo_convex_hull <- function(data, grouped_by, lat_column, long_column, area_unit = "km^2") {
  # Ensure the specified 'grouped_by' column exists in the data
  if (!(grouped_by %in% colnames(data))) {
    stop(paste("The specified 'grouped_by' column", grouped_by, "does not exist in the data."))
  }

  # Ensure the specified latitude and longitude columns exist in the data
  if (!(lat_column %in% colnames(data))) {
    stop(paste("The specified 'lat_column' column", lat_column, "does not exist in the data."))
  }
  if (!(long_column %in% colnames(data))) {
    stop(paste("The specified 'long_column' column", long_column, "does not exist in the data."))
  }

  # Convert data to an sf object
  geo_data_sf <- data %>%
    filter(!is.na(!!sym(lat_column)) & !is.na(!!sym(long_column))) %>%
    st_as_sf(coords = c(long_column, lat_column), crs = 4326)  # Convert to spatial object with WGS84 projection

  # Calculate convex hull areas for each group
  hull_areas <- geo_data_sf %>%
    group_by(!!sym(grouped_by)) %>%
    filter(n() >= 3) %>%  # Only include groups with at least 3 occurrences
    summarize(
      geometry = st_convex_hull(st_union(geometry)),
      .groups = "drop"
    ) %>%
    mutate(
      Convex_Hull_Area = st_area(geometry) %>% as.numeric()
    )

  # Convert area to the specified unit
  if (area_unit == "km^2") {
    hull_areas <- hull_areas %>%
      mutate(Convex_Hull_Area = Convex_Hull_Area / 1e6)  # Convert m^2 to km^2
  }

  # Return a clean table
  result <- hull_areas %>%
    select(Group = !!sym(grouped_by), Convex_Hull_Area) %>%
    as.data.frame()

  return(result)
}

