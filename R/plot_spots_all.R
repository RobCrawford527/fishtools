#' Plot FISH-QUANT Data For All Cells
#'
#' @param spots_1 Data frame containing spot data
#' @param spots_2 Data frame containing spot data
#' @param outlines Data frame containing cell outlines
#'
#' @return ggplot object showing all cells and spots
#' @export
#'
plot_spots_all <- function(spots_1 = NULL, spots_2 = NULL, outlines){

  # define colours
  colours <- viridis::viridis(n = 2, begin = 0.25, end = 0.75, direction = -1, option = "inferno")

  # plot cell outlines
  plot <- ggplot2::ggplot(data = outlines,
                          mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell)) +
    ggplot2::geom_polygon(alpha = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(cell), scales = "free") +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(name = "Dataset",
                                 breaks = c("spots 1", "spots 2"),
                                 values = c("spots 1" = colours[1], "spots 2" = colours[2]))

  # plot spots as appropriate
  # spots_1
  if (!is.null(spots_1)){
    plot <- plot +
      ggplot2::geom_point(data = spots_1,
                          mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell, colour = "spots 1"))
  }
  # spots_2
  if (!is.null(spots_2)){
    plot <- plot +
      ggplot2::geom_point(data = spots_2,
                          mapping = ggplot2::aes(x = x_pos, y = -y_pos, group = cell, colour = "spots 2"))
  }

  # return plot
  plot
}
