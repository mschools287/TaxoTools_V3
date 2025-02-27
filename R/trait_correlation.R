#' Calculate and Visualize Correlations Between Traits
#'
#' This function calculates the correlation matrix between specified traits in the data and displays a heatmap of the correlations.
#'
#' @param data A data frame containing the trait data.
#' @param trait_columns A character vector specifying the names of the trait columns in the data frame for which the correlation should be calculated.
#'
#' @return A correlation matrix (data frame) showing pairwise correlations between the specified traits. The correlation matrix is also used to generate the heatmap plot.
#'
#' @details
#' The function calculates pairwise correlations between the specified trait columns using Pearson's correlation coefficient and handles missing data with the "pairwise.complete.obs" method. It then reshapes the correlation matrix into a format suitable for visualization with \code{ggplot2} and creates a heatmap of the correlation values. The heatmap colors range from blue (negative correlation) to red (positive correlation), with white representing no correlation.
#'
#' @examples
#' # Calculate and visualize correlations between traits
#' data(data_morphodata)
#' trait_correlation(
#'   data = data_morphodata,
#'   trait_columns = c("trait1", "trait2", "trait3", "trait4", "trait5"))
#'
#' @import ggplot2 reshape2
#' @export
trait_correlation <- function(data, trait_columns) {
  # Calculate the correlation matrix
  correlation_matrix <- cor(data[trait_columns], use = "pairwise.complete.obs")

  # Reshape the correlation matrix into a long format for ggplot
  cor_data <- melt(correlation_matrix)
  colnames(cor_data) <- c("Trait1", "Trait2", "Correlation")

  # Plot the heatmap
  heatmap_plot <- ggplot(cor_data, aes(x = Trait1, y = Trait2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Heatmap of Traits", fill = "Correlation") +
    coord_fixed()

  # Print the heatmap plot to the current device
  print(heatmap_plot)

  # Return the correlation matrix for further use or analysis
  return(correlation_matrix)
}
