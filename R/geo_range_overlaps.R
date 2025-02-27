#' Calculate Convex Hull Overlaps Between Groups
#'
#' This function calculates convex range overlaps between groups (e.g., "Species" or "Clades") based on geographic coordinates.
#' The function generates convex hulls for groups with at least three coordinates by generating convex hulls, while also handling groups
#' with fewer than three points.
#'
#' @param data A data frame containing geographic data, including latitude and longitude coordinates.
#' @param group_by A string specifying the column name for grouping identifiers (e.g., "Species", "Clade").
#' @param lat_col A string specifying the column name for latitude values.
#' @param long_col A string specifying the column name for longitude values.
#'
#' @return A matrix indicating whether the convex hulls of groups overlap.
#'
#' @details
#' Groups with at least three coordinates generate convex hulls. For groups with fewer than three points,
#' special handling includes checking single points for containment and treating two points as a line to check for overlaps.
#'
#' @examples
#' data(data_morphodata)
#' geo_range_overlaps(
#'   data = data_morphodata,
#'   group_by = "Species",
#'   lat_col = "Latitude",
#'   long_col = "Longitude")
#'
#' @export
geo_range_overlaps <- function(data, group_by, lat_col, long_col) {
  # Ensure the sf package is installed and loaded
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required but not installed. Please install it using install.packages('sf').")
  }
  library(sf)

  # Convert data to an sf object with WGS84 projection
  geo_data_sf <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(lat_col)) & !is.na(!!rlang::sym(long_col))) %>%
    sf::st_as_sf(coords = c(long_col, lat_col), crs = 4326)

  # Filter groups with at least three unique coordinates and calculate convex hulls
  group_hulls <- geo_data_sf %>%
    dplyr::group_by(!!rlang::sym(group_by)) %>%
    dplyr::filter(dplyr::n() >= 3) %>%
    dplyr::summarize(geometry = sf::st_convex_hull(sf::st_union(geometry)), .groups = "drop")

  # Identify groups with fewer than three points
  small_groups <- geo_data_sf %>%
    dplyr::group_by(!!rlang::sym(group_by)) %>%
    dplyr::filter(dplyr::n() < 3) %>%
    dplyr::ungroup()

  # Initialize the overlap matrix
  all_groups <- unique(geo_data_sf[[group_by]])
  overlap_matrix <- matrix("FALSE", nrow = length(all_groups), ncol = length(all_groups),
                           dimnames = list(all_groups, all_groups))
  diag(overlap_matrix) <- "N/A" # Set diagonal elements to "N/A"

  # Check for overlaps between each pair of group hulls
  for (i in 1:(nrow(group_hulls) - 1)) {
    for (j in (i + 1):nrow(group_hulls)) {
      hull1 <- group_hulls$geometry[i]
      hull2 <- group_hulls$geometry[j]
      overlap <- sf::st_intersects(hull1, hull2, sparse = FALSE)
      overlap_matrix[group_hulls[[group_by]][i], group_hulls[[group_by]][j]] <- overlap
      overlap_matrix[group_hulls[[group_by]][j], group_hulls[[group_by]][i]] <- overlap
    }
  }

  # Handle groups with one or two points
  if (nrow(small_groups) > 0) {
    for (group in unique(small_groups[[group_by]])) {
      points <- small_groups %>%
        dplyr::filter(!!rlang::sym(group_by) == group) %>%
        dplyr::pull(geometry)

      if (length(points) == 1) {
        # Single point: Check if it falls inside any convex hull
        for (j in seq_len(nrow(group_hulls))) {
          hull <- group_hulls$geometry[j]
          hull_group <- group_hulls[[group_by]][j]
          if (sf::st_contains(hull, points[[1]], sparse = FALSE)) {
            overlap_matrix[group, hull_group] <- "TRUE"
            overlap_matrix[hull_group, group] <- "TRUE"
          }
        }
      } else if (length(points) == 2) {
        # Two points: Create a line and check if it intersects any convex hull
        line <- sf::st_sfc(sf::st_linestring(do.call(rbind, lapply(points, sf::st_coordinates))), crs = 4326)
        for (j in seq_len(nrow(group_hulls))) {
          hull <- group_hulls$geometry[j]
          hull_group <- group_hulls[[group_by]][j]
          if (sf::st_intersects(hull, line, sparse = FALSE)) {
            overlap_matrix[group, hull_group] <- "TRUE"
            overlap_matrix[hull_group, group] <- "TRUE"
          }
        }

        # Check if any single-point groups fall directly on the line
        single_points <- small_groups %>%
          dplyr::filter(!!rlang::sym(group_by) != group & dplyr::n() == 1)
        for (k in seq_len(nrow(single_points))) {
          point <- single_points$geometry[k]
          point_group <- single_points[[group_by]][k]
          if (sf::st_intersects(line, point, sparse = FALSE)) {
            overlap_matrix[group, point_group] <- "TRUE"
            overlap_matrix[point_group, group] <- "TRUE"
          }
        }
      }
    }
  }

  return(overlap_matrix)
}

