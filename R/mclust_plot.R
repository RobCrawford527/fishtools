mclust_plot <- function(data, model, n_bins = 120){
  
  # determine parameters
  # binwidth derived from data and number of bins
  # number of spots, number of components and parameters extracted from model
  # colours set
  binwidth <- (max(data[["spots"]][["INT_raw"]]) - min(data[["spots"]][["INT_raw"]])) / n_bins
  n_spots <- model[["n_spots"]]
  n_components <- max(model[["parameters"]][["component"]])
  parameters <- model[["parameters"]]
  colours <- viridis::viridis(n = n_components, begin = 0.1, end = 0.9, option = "inferno")
  
  # set up the plot
  plot <- ggplot2::ggplot(data = data[["spots"]], ggplot2::aes(x = INT_raw)) +
    
    # plot histogram of the raw data
    ggplot2::geom_histogram(binwidth = binwidth, fill = "grey80", colour = "grey70") +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::theme_classic()
  
  # create data for plotting model
  # create empty data frame
  components <- data.frame()
  for (i in 1:n_components){
    
    # create evenly spaced x data
    components_i <- data.frame(component = i,
                               x = seq(min(data[["spots"]][["INT_raw"]]),
                                       max(data[["spots"]][["INT_raw"]]),
                                       length.out = 1000))
    
    # calculate value of component i at each value of x
    components_i <- dplyr::mutate(components_i,
                                  y = dnorm(x,
                                            mean = parameters[["mean"]][i],
                                            sd = parameters[["variance"]][i]) * binwidth * n_spots * parameters[["proportion"]][i])
    
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
