fish_plot_single <- function(data, cell_of_interest){
  
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
                        mapping = ggplot2::aes(x = X_det, y = -Y_det, group = cell, colour = channel))
  
  # return plot
  plot
}
