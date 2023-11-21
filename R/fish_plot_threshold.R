#' Plot FISH-QUANT Data For A Single Cell Along With Colocalisation Threshold
#'
#' @param data Output list from fish_combined, containing cell outlines and spot data
#' @param cell_of_interest The index of the cell to plot
#' @param threshold Distance threshold to plot (in nm)
#' @param spot_radius Size to plot the spots (in nm)
#'
#' @return ggplot object showing cell of interest with all of its spots, with spots proportionately sized
#' @export
#'
fish_plot_threshold <- function(data, cell_of_interest, threshold = 300, spot_radius = 75){
  
  # filter data for cell of interest
  data[["outlines"]] <- dplyr::filter(data[["outlines"]], cell == cell_of_interest)
  data[["spots"]] <- dplyr::filter(data[["spots"]], cell == cell_of_interest)
  
  # plot cell outlines
  plot <- ggplot2::ggplot(data = data[["outlines"]],
                          mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell)) +
    ggplot2::geom_polygon(alpha = 0.1) +
    ggplot2::coord_equal() +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank())
  
  # plot spots
  plot <- plot +
    ggforce::geom_circle(data = data[["spots"]],
                         mapping = ggplot2::aes(x = NULL, y = NULL, x0 = x_pos, y0 = -y_pos, r = threshold, group = cell, colour = NULL, fill = channel),
                         alpha = 0.25) +
    ggforce::geom_circle(data = data[["spots"]],
                         mapping = ggplot2::aes(x = NULL, y = NULL, x0 = x_pos, y0 = -y_pos, r = radius, group = cell, colour = NULL, fill = channel),
                         alpha = 0.75) +
    ggplot2::geom_text(data = data[["spots"]],
                       mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell, label = spot))
  
  # return plot
  plot
}
