#' Plot Occurrences and Convex Hulls on a World Map
#'
#' This function visualizes occurrences and their convex hulls on a world map using \code{ggplot2}.
#' Geographic coordinates (latitude and longitude) are used to generate a map with occurrences as points
#' and convex hulls for groups with at least three occurrences. The map is interactive using the \code{plotly} package.
#'
#' @param data A data frame containing occurrence data with columns for latitude and longitude, and a column for group identifiers (e.g., "Species", "Clade").
#' @param grouped_by A string specifying the column name for grouping identifiers.
#' @param lat_column A string specifying the column name for latitude values.
#' @param long_column A string specifying the column name for longitude values.
#'
#' @details
#' This function loads a world basemap from the \code{rnaturalearth} package and filters the data to remove rows with missing values.
#' It uses the \code{sf} package to create spatial objects and calculates convex hulls for groups with at least three occurrences.
#' The map is interactive via \code{plotly}, enabling zooming and hover-over features.
#'
#' @return An interactive \code{plotly} object displaying occurrences and their convex hulls on a world map.
#'
#' @examples
#' data(data_morphodata)
#' geo_plot_map(
#'   data = data_morphodata,
#'   grouped_by = "Species",
#'   lat_column = "Latitude",
#'   long_column = "Longitude")
#'
#' @import ggplot2
#' @import plotly
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf st_convex_hull st_union
#' @export
geo_plot_map <- function(data, grouped_by, lat_column, long_column) {
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

  # Load the world basemap from Natural Earth
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Convert data to spatial object with WGS84 projection
  geo_data_sf <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(lat_column)) & !is.na(!!rlang::sym(long_column))) %>%
    sf::st_as_sf(coords = c(long_column, lat_column), crs = 4326)

  # Filter groups with at least three unique coordinates and calculate convex hulls
  group_hulls <- geo_data_sf %>%
    dplyr::group_by(!!rlang::sym(grouped_by)) %>%
    dplyr::filter(dplyr::n() >= 3) %>%
    dplyr::summarize(geometry = sf::st_convex_hull(sf::st_union(geometry)), .groups = "drop")

  # Calculate bounding box of the data
  bbox <- sf::st_bbox(geo_data_sf)

  # Create the ggplot
  base_map <- ggplot2::ggplot(data = world) +
    ggplot2::geom_sf(fill = "lightgray", color = "white") +  # World map
    ggplot2::geom_sf(data = geo_data_sf, ggplot2::aes(color = !!rlang::sym(grouped_by)), size = 2) +  # Group points
    ggplot2::geom_sf(data = group_hulls, ggplot2::aes(fill = !!rlang::sym(grouped_by)), color = "black", alpha = 0.3) +  # Convex hulls
    ggplot2::scale_color_viridis_d(
      name = grouped_by,
      labels = function(x) gsub(".*,", "", x)  # Remove count part from legend labels
    ) +
    ggplot2::scale_fill_viridis_d(name = paste(grouped_by, "Hulls")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Distributions and Convex Hulls by", grouped_by),
      subtitle = paste("Occurrences and hulls for", grouped_by, "with 3+ occurrences")
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),  # Set longitude limits
      ylim = c(bbox["ymin"], bbox["ymax"])   # Set latitude limits
    )

  # Convert the ggplot to plotly
  interactive_plot <- plotly::ggplotly(base_map)

  # Clean up the legend labels to remove unwanted characters (like (Outgroup, 1))
  for (i in 1:length(interactive_plot$x$data)) {
    if (!is.null(interactive_plot$x$data[[i]]$name)) {
      interactive_plot$x$data[[i]]$name <- gsub('^\\(|,\\d+\\)$', '', interactive_plot$x$data[[i]]$name)
    }
  }

  # Configure axis ticks to ensure they always appear
  interactive_plot <- interactive_plot %>%
    plotly::layout(
      xaxis = list(
        title = "Longitude",
        showgrid = TRUE,
        tickmode = "linear",
        tick0 = 0,
        dtick = 1
      ),
      yaxis = list(
        title = "Latitude",
        showgrid = TRUE,
        tickmode = "linear",
        tick0 = 0,
        dtick = 1
      ),
      dragmode = "pan"
    )

  return(interactive_plot)
}
