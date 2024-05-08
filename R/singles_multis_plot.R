#' Plot Percentage of mRNAs in Multi-mRNA Foci
#'
#' @param data Data frame containing percentage of mRNA copies that are in multi-mRNA foci, the output from summarise_singles_multis
#'
#' @return Plot of percentage of mRNAs in multi-mRNA foci, by mRNA
#' @export
#'
singles_multis_plot <- function(data){

  # create plot
  # combined density plot, dotplot and boxplot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = mRNA,
                                             y = percentage_copies_multis,
                                             fill = mRNA)) +
    ggplot2::geom_hline(yintercept = seq(0, 100, 25), lty = 2, alpha = 0.5, colour = "grey80") +
    ggdist::stat_halfeye(colour = NA, alpha = 0.25, width = 0.5) +
    ggdist::stat_dots(side = "right", colour = NA, binwidth = 1) +
    ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "mako", begin = 0.25, end = 0.75, direction = -1) +
    ggplot2::scale_y_continuous(limits = c(-5, 105), breaks = seq(0, 100, 25), expand = c(0, 0)) +
    ggplot2::theme_classic()

  # return plot
  plot
}
