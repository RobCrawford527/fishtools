#' Estimate The Intersection Between Two Gaussian Mixture Model Components
#'
#' @param spots Data frame containing spot data
#' @param model Gaussian mixture model parameters, the output of fish_mclust
#' @param component_1 The first component of the model to compare. The first component must have the LOWER mean.
#' @param component_2 The second component of the model to compare.
#' @param n_bins Number of bins to use (default = 120)
#'
#' @return A single value corresponding to the estimated intersection point (raw intensity) between the two components of interest
#' @export
#'
find_intersection <- function(spots, model, component_1 = NULL, component_2 = NULL, n_bins = 120){

  # determine parameters
  # binwidth derived from data and number of bins
  # number of spots and parameters extracted from model
  binwidth <- (max(spots[["INT_raw"]]) - min(spots[["INT_raw"]])) / n_bins
  n_spots <- model[["n_spots"]]
  parameters <- model[["parameters"]]

  # create data frame for components of interest
  # create evenly spaced x data between the two component means (where the intersection of interest is located)
  # calculate value of each component at each x value
  component_data <- data.frame(x = seq(parameters[["mean"]][component_1],
                                       parameters[["mean"]][component_2],
                                       length.out = 1000))
  component_data <- dplyr::mutate(component_data,
                                  component_1 = dnorm(x,
                                                      mean = parameters[["mean"]][component_1],
                                                      sd = parameters[["variance"]][component_1]) * binwidth * n_spots * parameters[["proportion"]][component_1],
                                  component_2 = dnorm(x,
                                                      mean = parameters[["mean"]][component_2],
                                                      sd = parameters[["variance"]][component_2]) * binwidth * n_spots * parameters[["proportion"]][component_2])

  # calculate sign of difference between the y values of the two components
  # determine
  component_data <- dplyr::mutate(component_data,
                                  sign = ifelse(component_1 - component_2 > 0, TRUE, FALSE),
                                  diff = c(0, diff(sign)))

  # extract row where the crossover occurs
  # specifically this is the x value immediately AFTER the crossover as the crossing point is not exact
  intersection <- dplyr::filter(component_data,
                                diff != 0)$x

  # return value of intersection
  intersection
}
