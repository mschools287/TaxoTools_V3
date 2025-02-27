#' Compare Traits Across Groups
#'
#' This function compares specified traits across selected groups (e.g., "species", "clades") using a variety of visualizations.
#' It employs both base R scatterplot matrices and interactive boxplots using \code{ggplot2} and \code{plotly}.
#'
#' @param data A data frame containing group and trait information.
#' @param group_by_column A string specifying the column name to group by (e.g., "Species" or "Clade").
#' @param group_values A character vector specifying the groups to include in the comparison.
#' @param traits A character vector specifying the trait columns to compare.
#'
#' @details
#' The function filters the input dataset for the selected groups and traits, removes missing values,
#' and generates the following visualizations:
#' \itemize{
#'   \item A scatterplot matrix comparing all specified traits across the selected groups. This will appear in the "plots" window.
#'   \item Boxplots showing the distribution of each trait for the selected groups. These will appear in the "Viewer" window.
#' }
#'
#' The scatterplot matrix is displayed using base R graphics, while the boxplots are created using \code{ggplot2} and converted to interactive \code{plotly} objects.
#'
#' @return Two sets of graphics are produced:
#' \itemize{
#'   \item \code{pair_plot}: A base R scatterplot matrix comparing all specified traits across the selected groups. This will appear in the "plots" window.
#'   \item \code{box_plots}: A list of interactive boxplots, one for each trait. These will appear in the "Viewer" window.
#' }
#'
#' @examples
#' data(data_morphodata)
#' trait_compare(
#'   data = data_morphodata,
#'   group_by_column = "Species",
#'   group_values = c("Species A", "Species B"),
#'   traits = c("trait1", "trait2"))
#'
#' @import dplyr
#' @importFrom GGally ggpairs
#' @import plotly
#' @export
trait_compare <- function(data, group_by_column, group_values, traits) {

  # Ensure the specified 'group_by_column' exists in the data
  if (!(group_by_column %in% colnames(data))) {
    stop(paste("The specified 'group_by_column' column", group_by_column, "does not exist in the data."))
  }

  # Ensure group_values is a vector (even if a single value is passed as a string)
  group_values <- as.character(group_values)

  # Filter data based on the group values and select the relevant traits
  selected_data <- data %>%
    filter(get(group_by_column) %in% group_values) %>%
    dplyr::select(all_of(group_by_column), all_of(traits))

  # Remove NAs
  selected_data <- selected_data %>%
    na.omit()

  # Create the base R pair plot (scatterplot matrix)
  pairs(selected_data[, -1],
        col = as.factor(selected_data[[group_by_column]]),
        pch = 19,
        main = paste("Trait Comparison for", paste(group_values, collapse = ", ")))

  # Boxplot for each trait comparison (optional)
  box_plot_list <- lapply(traits, function(trait) {
    ggplot(selected_data, aes(x = .data[[group_by_column]], y = .data[[trait]], fill = .data[[group_by_column]])) +
      geom_boxplot() +
      labs(title = paste("Distribution of", trait), x = group_by_column, y = trait) +
      theme_minimal()
  })

  # Convert the ggplot boxplots to interactive plotly objects
  interactive_box_plots <- lapply(box_plot_list, ggplotly)

  # Return interactive box plots as a list
  return(list(box_plots = interactive_box_plots))
}
