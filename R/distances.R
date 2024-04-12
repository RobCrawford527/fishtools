#' Calculate Distances Between Spots In Opposite Channels For A Single Cell
#'
#' @param ch1_spots Data frame containing spot data from first channel
#' @param ch2_spots Data frame containing spot data from second channel
#' @param cell_of_interest The index of the cell of interest
#'
#' @return Data frame containing all pairwise distances between spots in opposite channels
#'
distances <- function(ch1_spots, ch2_spots, cell_of_interest){

  # filter each data frame for cell of interest
  ch1_spots_filt <- dplyr::filter(ch1_spots, cell == cell_of_interest)
  ch2_spots_filt <- dplyr::filter(ch2_spots, cell == cell_of_interest)

  # only continue if there are spots in both channels
  if (nrow(ch1_spots_filt) >= 1 & nrow(ch2_spots_filt) >= 1){

    # create blank distance data frame
    # set starting row index
    distance <- data.frame(cell = NA,
                           ch1_index = NA,
                           ch2_index = NA,
                           distance = NA,
                           fwhm_sum = NA)[0,]
    row <- 1

    # iterate through each combination of spots from channel 1 and channel 2
    for (i in 1:nrow(ch1)){
      for (j in 1:nrow(ch2)){
        # record cell
        # record index/spot number for each channel
        distance[row, "cell"] <- cell_of_interest
        distance[row, "ch1_index"] <- i
        distance[row, "ch2_index"] <- j

        # calculate 3-dimensional distance between spots
        distance[row, "distance"] <- sqrt((ch1[["x_pos"]][i] - ch2[["x_pos"]][j])^2 +
                                          (ch1[["y_pos"]][i] - ch2[["y_pos"]][j])^2 +
                                          (ch1[["z_pos"]][i] - ch2[["z_pos"]][j])^2 )

        # calculate sum of spot radii
        # full width at half maximum (FWMH) measures width of Gaussian distribution
        # this can be interpreted as a measure of spot diameter (therefore halved to calculate radius)
        # summing the FWMH values is a metric for assessing colocalisation
        distance[row, "fwhm_sum"] <- (ch1[["SigmaX"]][i] + ch2[["SigmaX"]][j]) * sqrt(2 * log(2))

        # iterate row index
        row <- row + 1
      }
    }

    # convert index columns to factors
    # add cell_spot columns
    distance <- dplyr::mutate(distance,
                              ch1_index = as.factor(ch1_index),
                              ch2_index = as.factor(ch2_index),
                              cell_ch1 = paste(cell, ch1_index, sep = "_"),
                              cell_ch2 = paste(cell, ch2_index, sep = "_"),
                              .before = distance)

  } else {

    # create empty data frame if spots missing from one or both channels
    distance <- data.frame()
  }

  # return output data frame
  distance
}
