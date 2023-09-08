#' Plot FISH-QUANT Data For All Cells
#'
#' @param data Output list from fish_combined, containing cell outlines and spot data
#'
#' @return ggplot object showing all cells and spots
#' @export
#'
fish_plot_all <- function(data){

  # plot cell outlines
  plot <- ggplot2::ggplot(data = data[["outlines"]],
                          mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell)) +
    ggplot2::geom_polygon(alpha = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(cell), scales = "free") +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank())

  # plot spots
  plot <- plot +
    ggplot2::geom_point(data = data[["spots"]],
                        mapping = ggplot2::aes(x = X_det, y = -Y_det, group = cell, colour = channel))

  # return plot
  plot
}
