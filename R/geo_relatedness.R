#' Map Geographic Distribution with Phylogenetic Relatedness
#'
#' This function generates an interactive map that visualizes the geographic distribution of species along with their phylogenetic relatedness to a reference species. Species are colored according to their relatedness to the specified reference species, with darker colors representing a higher degree of relatedness. The map is interactive, allowing for zooming and panning.
#'
#' @param data A data frame containing geographic information for species. The data must include the columns specified in `lat_column` and `long_column` for latitude and longitude.
#' @param lat_column A string specifying the name of the column in `data` that contains latitude values.
#' @param long_column A string specifying the name of the column in `data` that contains longitude values.
#' @param phylogeny A phylogenetic tree of class `phylo` representing the evolutionary relationships between the species in the dataset.
#' @param reference_species A string specifying the name of the reference species in the phylogeny. This species will be used to color other species based on their relatedness to it.
#' @param individuals_column A string specifying the name of the column in `data` that contains individual species information. Defaults to "Species_updated".
#'
#' @return An interactive `plotly` plot showing the geographic distribution of species with phylogenetic relatedness colored by distance to the reference species.
#'
#' @details The function calculates pairwise phylogenetic distances using the `cophenetic` function, assigns relatedness values to species based on their distance to the reference species, and uses the `viridis` color palette to color the species according to their relatedness. The map includes a world basemap from Natural Earth and allows zooming and panning.
#'
#' @examples
#' # Example usage:
#' geo_relatedness(
#'   data = data_morphodata,
#'   lat_column = "Latitude",
#'   long_column = "Longitude",
#'   phylogeny = data_treeML,
#'   reference_species = "clade_1_outgroup",
#'   individuals_column = "Species_updated")
#'
#' @import ggplot2
#' @import plotly
#' @import sf
#' @import scales
#' @import rnaturalearth
#' @import rlang
#' @export
geo_relatedness <- function(data, lat_column, long_column, phylogeny, reference_species, individuals_column = "Species_updated") {
  # Ensure the specified latitude and longitude columns exist in the data
  if (!(lat_column %in% colnames(data))) {
    stop(paste("The specified 'lat_column' column", lat_column, "does not exist in the data."))
  }
  if (!(long_column %in% colnames(data))) {
    stop(paste("The specified 'long_column' column", long_column, "does not exist in the data."))
  }
  # Ensure phylogeny is provided and is of class 'phylo'
  if (is.null(phylogeny) || !inherits(phylogeny, "phylo")) {
    stop("Please provide a valid phylogeny object.")
  }
  # Ensure the reference species exists in the phylogeny
  if (!(reference_species %in% phylogeny$tip.label)) {
    stop("The specified reference species does not exist in the phylogeny.")
  }
  # Calculate relatedness (pairwise phylogenetic distances)
  relatedness_matrix <- cophenetic(phylogeny)  # Pairwise distances from the phylogeny
  # Get the index of the reference species
  reference_index <- match(reference_species, phylogeny$tip.label)
  # Assign relatedness to each species based on the distance to the reference species
  data$relatedness <- sapply(data[[individuals_column]], function(species_name) {
    species_index <- match(species_name, phylogeny$tip.label)
    relatedness_matrix[species_index, reference_index]
  })
  # Mark the reference species for special coloring (Red)
  data$color <- ifelse(data[[individuals_column]] == reference_species, "red", NA)
  # For other species, color based on relatedness (exclude reference species from color scale)
  max_relatedness <- max(data$relatedness[data[[individuals_column]] != reference_species], na.rm = TRUE)
  min_relatedness <- min(data$relatedness[data[[individuals_column]] != reference_species], na.rm = TRUE)
  # Assign colors to non-reference species based on relatedness
  data$color[is.na(data$color)] <- scales::col_numeric(palette = c("black", "white"), domain = c(min_relatedness, max_relatedness))(data$relatedness[is.na(data$color)])
  # Load the world basemap from Natural Earth
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # Convert data to spatial object with WGS84 projection
  geo_data_sf <- data %>% filter(!is.na(!!rlang::sym(lat_column)) & !is.na(!!rlang::sym(long_column))) %>% st_as_sf(coords = c(long_column, lat_column), crs = 4326)
  # Calculate bounding box of the data for zoom
  bbox <- st_bbox(geo_data_sf)
  # Create the ggplot
  base_map <- ggplot(data = world) + geom_sf(fill = "lightgray", color = "white") + geom_sf(data = geo_data_sf, aes(fill = color, text = !!rlang::sym(individuals_column)), color = "black", shape = 21, stroke = 0.2, size = 3) + scale_fill_identity() + theme_minimal() + labs(title = paste("Geographic Distribution with Phylogenetic Relatedness to", reference_species), subtitle = paste("Individuals colored by relatedness to", reference_species)) + theme(legend.position = "none") + coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))
  # Convert the ggplot to an interactive plot
  interactive_plot <- ggplotly(base_map, tooltip = "text")
  # Add the caption as a text annotation to the plotly layout
  interactive_plot <- interactive_plot %>% layout(annotations = list(x = 0, y = 1.05, text = "Darker colors represent a higher degree of relatedness", showarrow = FALSE, xref = "paper", yref = "paper", font = list(size = 12)), xaxis = list(title = "Longitude", showgrid = TRUE, tickmode = "linear", tick0 = 0, dtick = 1), yaxis = list(title = "Latitude", showgrid = TRUE, tickmode = "linear", tick0 = 0, dtick = 1), dragmode = "pan")
  return(interactive_plot)
}

