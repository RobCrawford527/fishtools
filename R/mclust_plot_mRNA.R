#' Plot Histogram And Gaussian Mixture Model From Transformed Spot Data
#'
#' @param spots Data frame containing spot data
#' @param model Transformed Gaussian mixture model parameters to use. The output of fish_mclust and transform_model.
#' @param n_bins Number of bins to use (default = 120)
#' @param multi_threshold Minimum threshold to consider a spot a multi-mRNA granule
#'
#' @return A histogram with overlaid Gaussian mixture model
#' @export
#'
mclust_plot_mRNA <- function(spots,
                             model,
                             n_bins = 120,
                             multi_threshold = 1.5){

  # determine parameters
  # binwidth derived from data and number of bins
  # number of spots, number of components and parameters extracted from model
  # colours set
  binwidth <- (max(spots[["number_of_mRNAs"]]) - min(spots[["number_of_mRNAs"]])) / n_bins
  n_spots <- model[["n_spots"]]
  n_components <- max(model[["parameters"]][["component"]])
  parameters <- model[["parameters"]]
  colours <- viridis::viridis(n = n_components, begin = 0.1, end = 0.9)

  # set up the plot
  plot <- ggplot2::ggplot(data = spots, ggplot2::aes(x = number_of_mRNAs)) +

    # plot histogram of the raw data
    ggplot2::geom_histogram(binwidth = binwidth, fill = "grey80", colour = "grey70") +
    ggplot2::geom_vline(xintercept = multi_threshold, lty = 3, colour = "grey50") +
    ggplot2::scale_x_continuous(expand = c(0,0), breaks = seq(0, 50, 1)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::theme_classic()

  # create data for plotting model
  # create empty data frame
  components <- data.frame()
  for (i in 1:n_components){

    # create evenly spaced x data
    components_i <- data.frame(component = i,
                               x = seq(min(spots[["number_of_mRNAs"]]),
                                       max(spots[["number_of_mRNAs"]]),
                                       length.out = 1000))

    # calculate value of component i at each value of x
    components_i <- dplyr::mutate(components_i,
                                  y = dnorm(x,
                                            mean = parameters[["number_of_mRNAs"]][i],
                                            sd = parameters[["variance_mRNA"]][i]) * binwidth * n_spots * parameters[["proportion"]][i])

    # add to overall data frame
    components <- rbind.data.frame(components, components_i)
  }

  # convert component column to factor
  components <- dplyr::mutate(components, component = as.factor(component))

  # calculate the combined model
  # group by x
  # add component values at each x value
  mixture_model <- dplyr::group_by(components, x)
  mixture_model <- dplyr::summarise(mixture_model, sum = sum(y))

  # calculate local minima and maxima
  # allows background spots to be filtered out
  # determine difference in each model value from the previous
  mixture_model <- dplyr::mutate(mixture_model,
                                 diff = c(NA, diff(sign(diff(sum))), NA))

  # identify local minima and maxima (diff not equal to 0)
  # print to console
  minmax <- dplyr::filter(mixture_model, diff != 0)
  print(minmax)

  # add components and mixture model to plot
  plot <- plot +
    ggplot2::geom_line(data = components, mapping = ggplot2::aes(x = x, y = y, colour = component)) +
    ggplot2::geom_line(data = mixture_model, mapping = ggplot2::aes(x = x, y = sum), lty = 2) +
    ggplot2::scale_colour_manual(values = colours)

  # return plot
  plot
}
