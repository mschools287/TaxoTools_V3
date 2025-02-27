#' Create a Heatmap of Phylogenetic Relatedness
#'
#' This function generates a heatmap visualizing the pairwise phylogenetic relatedness between species based on their phylogenetic tree. The relatedness is calculated using the cophenetic distances, which represent the phylogenetic distance between pairs of species.
#'
#' @param phylogeny A phylogenetic tree object of class \code{phylo}. The tree must include tip labels representing the species.
#'
#' @return A \code{ggplot} object containing the heatmap of pairwise phylogenetic relatedness.
#'
#' @details The function first calculates pairwise phylogenetic distances using the \code{cophenetic()} function. The distances are then visualized as a heatmap using \code{ggplot2}. The axes of the heatmap are labeled with species names, and the color scale represents the phylogenetic distance between species.
#'
#' @examples
#' # Example usage:
#' phylo_heatmap(phylogeny = data_treeML)
#'
#' @import ape
#' @import ggplot2
#' @import viridis
#' @export
phylo_heatmap <- function(phylogeny) {
  # Ensure phylogeny is provided and is of class 'phylo'
  if (is.null(phylogeny) || !inherits(phylogeny, "phylo")) {
    stop("Please provide a valid phylogeny object.")
  }

  # Calculate relatedness (pairwise phylogenetic distances)
  relatedness_matrix <- cophenetic(phylogeny)  # Pairwise distances from the phylogeny

  # Ensure row names and column names match the species names
  species_names <- phylogeny$tip.label

  # Convert the matrix to a data frame for plotting
  relatedness_df <- as.data.frame(relatedness_matrix)
  rownames(relatedness_df) <- species_names
  colnames(relatedness_df) <- species_names

  # Convert the relatedness matrix to a long format for ggplot
  relatedness_long <- as.data.frame(as.table(relatedness_matrix))

  # Convert Var1 and Var2 to factors for proper axis labeling
  relatedness_long$Var1 <- factor(relatedness_long$Var1, levels = species_names)
  relatedness_long$Var2 <- factor(relatedness_long$Var2, levels = species_names)

  # Create the heatmap using ggplot
  heatmap_plot <- ggplot(relatedness_long, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(
      title = "Heatmap of Phylogenetic Relatedness",
      subtitle = "Pairwise phylogenetic distances between species",
      x = "Species",
      y = "Species",
      fill = "Relatedness"  # Title for the key
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.y = element_text(angle = 0, hjust = 1)
    )

  # Return the heatmap plot
  return(heatmap_plot)
}
