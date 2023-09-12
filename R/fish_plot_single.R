#' Plot FISH-QUANT Data For A Single Cell
#'
#' @param data Output list from fish_combined, containing cell outlines and spot data
#' @param cell_of_interest The index of the cell to plot
#' @param pixel_size The size of each pixel (in nanometers)
#'
#' @return ggplot object showing cell of interest with all of its spots
#' @export
#'
fish_plot_single <- function(data, cell_of_interest, pixel_size){

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
    ggplot2::geom_point(data = data[["spots"]],
                        mapping = ggplot2::aes(x = Pos_X / pixel_size, y = -Pos_Y / pixel_size, group = cell, colour = channel))

  # return plot
  plot
}
