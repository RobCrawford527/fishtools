#' Calculate Colocalisation At A Range Of Distance Thresholds
#'
#' @param spots List containing cell and spot data (from which closest neighbours have been calculated)
#' @param neighbours Data frame containing closest neighbours for each spot
#' @param thresholds Numeric vector containing the distance thresholds to test
#' @param iterations The number of iterations to perform for the null distributions
#'
#' @return Data frame containing colocalisation percentage at each threshold
#' @export
#'
coloc_threshold_test <- function(spots, neighbours, thresholds = seq(0, 1000, 50), iterations = 5){

  # group neighbours by channel
  neighbours <- dplyr::group_by(neighbours, channel)

  # create empty data frame to write results into
  coloc_all <- data.frame()

  # calculate colocalisation and null distribution at each threshold
  for (i in thresholds){

    # calculate colocalisation for each pair of spots at given threshold
    neighbours_i <- fish_coloc_threshold(neighbours,
                                         method = "absolute",
                                         threshold = i)

    # calculate colocalisation percentage
    coloc_i <- dplyr::summarise(neighbours_i,
                                threshold = i,
                                coloc = (sum(colocalised) / dplyr::n()) * 100)


    if (iterations >= 1){
      # calculate null distribution at given threshold
      null_i <- fish_null_distribution(spots,
                                       iterations = iterations,
                                       method = "absolute",
                                       threshold = i)

      # write null distribution parameters into output data frame
      coloc_i[["null_mean"]] <- c(ch1 = mean(null_i[["ch1"]]), ch2 = mean(null_i[["ch2"]]))
      coloc_i[["null_sd"]] <- c(ch1 = sd(null_i[["ch1"]]), ch2 = sd(null_i[["ch2"]]))

    } else if (iterations == 0){
      # write null distribution parameters into output data frame
      coloc_i[["null_mean"]] <- c(NA, NA)
      coloc_i[["null_sd"]] <- c(NA, NA)
    }

    # combine with full results data frame
    coloc_all <- rbind.data.frame(coloc_all, coloc_i)
  }

  # calculate difference between colocalisation result and null at each threshold
  coloc_all <- dplyr::mutate(coloc_all,
                             diff = coloc - null_mean)

  # return output data frame
  coloc_all
}
