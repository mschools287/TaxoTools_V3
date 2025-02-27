#' Time-Calibrated Phylogenetic Tree
#'
#' A time-calibrated phylogenetic tree derived from a Maximum Likelihood (ML) tree
#' using RelTime in MEGA11. Calibration points derived from https://timetree.org/ were used to estimate divergence times
#' for 19 individuals, including 18 ingroup samples and one outgroup.
#'
#' @format An object of class \code{phylo} (from the \code{ape} package).
#' \itemize{
#'   \item \code{edge} A matrix describing the edges of the tree.
#'   \item \code{edge.length} A numeric vector giving the lengths of the edges, representing divergence times.
#'   \item \code{Nnode} An integer giving the number of internal nodes in the tree.
#'   \item \code{tip.label} A character vector of tip labels for the 19 individuals included in the tree.
#' }
#'
#' @details
#' This time-calibrated tree was generated using the RelTime method implemented in MEGA11 with `data_treeML` as a base.
#' Calibration points derived from https://timetree.org/ were used to anchor specific nodes, enabling temporal scaling of the tree.
#' The resulting tree serves as an example dataset for Time Tree analyses and visualization.
#'
#' @examples
#' data(data_treeTime)
#' plot(
#'   data_treeTime,
#'   main = "Time-Calibrated Tree")
#'
#' @source
#' The tree was generated in MEGA11 using RelTime with `data_treeML` as input. Calibration points derived from https://timetree.org/ were applied for
#' time scaling. The input ML tree was constructed using IQ-TREE.
"data_treeTime"
