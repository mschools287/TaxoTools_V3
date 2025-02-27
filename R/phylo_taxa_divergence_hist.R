#' Plot Divergence Times Histogram for Specified Taxa
#'
#' This function generates a histogram comparing the divergence times of multiple specified taxa
#' based on a phylogenetic tree. The taxa are matched using partial string matching to
#' identify nodes in the tree. Users can specify custom colors for each taxon.
#'
#' @param tree A phylogenetic tree of class \code{phylo} that includes edge lengths.
#' @param taxa_list A character vector specifying the taxa or clades to analyze. Partial string matching is used.
#' @param bin_width A numeric value specifying the width of the bins for the histogram (default is 5).
#' @param colors A character vector of colors to use for each taxon in the plot. If not provided, the function defaults to a rainbow color palette.
#'
#' @return A bar plot showing the frequency distribution of divergence times for the specified taxa.
#'
#' @details
#' The function computes node ages using edge lengths from the input tree, identifies
#' nodes corresponding to the specified taxa using partial string matching, and extracts their
#' divergence times. The divergence times are then binned, and a bar plot is created to compare
#' the distributions for the specified taxa. Each taxon is represented with a distinct color.
#'
#' If no valid divergence times are found for any of the specified taxa, an error is returned.
#'
#' @examples
#' data(data_treeTime)
#' phylo_taxa_divergence_hist(
#'   tree = data_treeTime,
#'   taxa_list = c("clade_2", "clade_3", "_a"),
#'   colors = c("darkseagreen", "deeppink", "pink"),
#'   bin_width = 2)
#'
#' @importFrom ape node.depth.edgelength
#' @importFrom graphics barplot legend grid
#' @importFrom grDevices rainbow
#' @export
phylo_taxa_divergence_hist <- function(tree, taxa_list, bin_width = 5, colors = NULL) {
  # Ensure the provided tree contains edge lengths
  if (is.null(tree$edge.length)) {
    stop("The provided tree does not contain edge lengths for divergence time calculation.")
  }

  # Compute node ages using edge lengths
  node_ages <- node.depth.edgelength(tree)
  max_age <- max(node_ages)  # Root age
  node_ages <- max_age - node_ages  # Convert to divergence times

  # Generate the node table using the provided tree
  node_table <- phylo_node_table(tree, method = "Time Tree")
  node_table$Taxa <- as.character(node_table$Taxa)

  # Initialize an empty list to store divergence times for each taxon
  divergence_list <- list()

  for (taxon in taxa_list) {
    taxon_nodes <- node_table[sapply(node_table$Taxa, function(taxa) grepl(taxon, taxa, fixed = TRUE)), ]
    divergence_times <- as.numeric(taxon_nodes$Support_or_Time)

    if (length(divergence_times) == 0) {
      warning(paste("No divergence times found for taxon:", taxon))
    } else {
      divergence_list[[taxon]] <- divergence_times
    }
  }

  # Check if there is at least one valid taxon with divergence data
  if (length(divergence_list) == 0) {
    stop("No valid divergence times found for any specified taxa. Please check your taxa names.")
  }

  # Create a combined data frame for divergence times
  divergence_data <- do.call(rbind, lapply(names(divergence_list), function(taxon) {
    data.frame(DivergenceTime = divergence_list[[taxon]], Taxa = taxon, stringsAsFactors = FALSE)
  }))

  # Define breaks for the bins
  max_time <- max(divergence_data$DivergenceTime)
  breaks <- seq(0, max_time + bin_width, by = bin_width)

  # Create counts for each bin for each taxon
  divergence_data$TimeBin <- cut(divergence_data$DivergenceTime, breaks = breaks, right = TRUE)

  # Compute frequency tables for each taxon
  counts_list <- lapply(unique(divergence_data$Taxa), function(taxon) {
    table(divergence_data$TimeBin[divergence_data$Taxa == taxon])
  })

  # Combine frequency tables into a matrix
  barplot_height <- do.call(rbind, counts_list)

  # Set colors: If no colors are provided, use rainbow by default
  if (is.null(colors)) {
    num_taxa <- length(unique(divergence_data$Taxa))
    colors <- rainbow(num_taxa)
  } else if (length(colors) != length(unique(divergence_data$Taxa))) {
    stop("The number of colors provided must match the number of taxa.")
  }

  # Set up the bar plot
  barplot_obj <- barplot(
    barplot_height, beside = TRUE, col = colors,
    names.arg = levels(divergence_data$TimeBin),
    main = "Divergence Times for Specified Taxa",
    xlab = "Divergence Time Range (in million years)",
    ylab = "Frequency",
    ylim = c(0, max(barplot_height) + 1),
    border = "black",
    cex.names = 0.7, las = 2
  )

  # Add grid behind the plot
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted", lwd = 0.5)

  # Add a legend
  legend("topright", legend = unique(divergence_data$Taxa), fill = colors, bty = "n")
}
