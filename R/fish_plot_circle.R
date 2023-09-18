#' Plot FISH-QUANT Data For A Single Cell, With Spots Proportionately Sized
#'
#' @param data Output list from fish_combined, containing cell outlines and spot data
#' @param cell_of_interest The index of the cell to plot
#'
#' @return ggplot object showing cell of interest with all of its spots, with spots proportionately sized
#' @export
#'
fish_plot_circle <- function(data, cell_of_interest){

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
                         mapping = ggplot2::aes(x = NULL, y = NULL, x0 = x_pos, y0 = -y_pos, r = SigmaX * sqrt(2 * log(2)), group = cell, colour = NULL, fill = channel),
                         alpha = 0.75) +
    ggplot2::geom_text(data = data[["spots"]],
                       mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell, label = spot))

  # return plot
  plot
}
