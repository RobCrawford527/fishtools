#' Plot FISH-QUANT Data For All Cells
#'
#' @param spots Data frame containing spot data
#' @param outlines Data frame containing cell outlines
#'
#' @return ggplot object showing all cells and spots
#' @export
#'
plot_spots_all <- function(spots, outlines){

  # plot cell outlines
  plot <- ggplot2::ggplot(data = outlines,
                          mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell)) +
    ggplot2::geom_polygon(alpha = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(cell), scales = "free") +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_blank())

  # plot spots
  plot <- plot +
    ggplot2::geom_point(data = spots,
                        mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell, colour = channel))

  # return plot
  plot
}
