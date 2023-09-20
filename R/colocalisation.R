#' Calculate Distances Between Spots In Opposite Channels For A Single Cell
#'
#' @param spots Data frame containing spot data in two channels
#' @param cell_of_interest The index of the cell of interest
#' @param method Method used to determine colocalisation. Options are "radius" (uses sum of spot radii) or "absolute" (sets an absolute distance)
#' @param multiplier Multiplier for sum of radii. Set lower for more strict colocalisation. Used if method = "radius"
#' @param threshold Distance threshold (in nm) for determining colocalisation. Used if method = "absolute"
#'
#' @return Data frame containing all pairwise distances between spots in opposite channels
#'
colocalisation <- function(spots, cell_of_interest, method = "radius", multiplier = 1, threshold = 500){

  # filter spot data for cell of interest
  # define channels present in data
  spots_filt <- dplyr::filter(spots, cell == cell_of_interest)
  channels <- unique(spots_filt[["channel"]])

  # only continue if there are exactly two channels present
  if (length(channels) == 2){

    # create filtered data frames for individual channels
    ch1 <- dplyr::filter(spots_filt, channel == channels[1])
    ch2 <- dplyr::filter(spots_filt, channel == channels[2])

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

    # assess if spots are colocalised
    # two possible methods: radius uses sum of radii (can be adjusted using the multiplier)
    if (method == "radius"){

      # colocalised if distance is less than sum of radii
      # multiplier is used to make threshold more strict
      distance <- dplyr::mutate(distance,
                                colocalised = ifelse(distance < fwhm * multiplier, TRUE, FALSE))

    } else if (method == "absolute"){

      # colocalised if distance is less than absolute value
      distance <- dplyr::mutate(distance,
                                colocalised = ifelse(distance < threshold, TRUE, FALSE))
    }

  } else {

    # create empty data frame if not two channels
    distance <- data.frame()
  }

  # return output data frame
  distance
}
