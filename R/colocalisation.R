colocalisation <- function(spots, cell_of_interest){
  
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
        distance[row, "distance"] <- sqrt((ch1[["Pos_X"]][i] - ch2[["Pos_X"]][j])^2 +
                                          (ch1[["Pos_Y"]][i] - ch2[["Pos_Y"]][j])^2 +
                                          (ch1[["Pos_Z"]][i] - ch2[["Pos_Z"]][j])^2 )

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
    # assess if spots are colocalised
    # colocalised if distance is less than sum of radii
    distance <- dplyr::mutate(distance,
                              ch1_index = as.factor(ch1_index),
                              ch2_index = as.factor(ch2_index),
                              cell_ch1 = paste(cell, ch1_index, sep = "_"),
                              cell_ch2 = paste(cell, ch2_index, sep = "_"),
                              .before = distance)
    distance <- dplyr::mutate(distance,
                              colocalised = ifelse(distance < fwhm_sum, TRUE, FALSE))
    
  } else {
    
    # create empty data frame if not two channels
    distance <- data.frame()
  }
  
  # return output data frame
  distance
}
