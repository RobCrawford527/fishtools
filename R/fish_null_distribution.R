#' Estimate Null Distribution For Colocalisation Data
#'
#' @param spots List containing spot data
#' @param iterations Number of iterations to perform, default = 10
#'
#' @return Data frame containing simulated % colocalisation for each channel (one row per iteration)
#' @export
#'
fish_null_distribution <- function(spots, iterations = 10){

  # ungroup spots
  # determine number of spots present in data
  spots[["spots"]] <- dplyr::ungroup(spots[["spots"]])
  total <- nrow(spots[["spots"]])

  # create empty data frame to read results into
  null_distribution <- data.frame()

  # calculate null distribution for colocalisation
  for (i in 1:iterations){

    # create copy of spots to modify
    # shuffle X, Y and Z positions for all spots
    spots_i <- spots
    spots_i[["spots"]] <- dplyr::mutate(spots_i[["spots"]],
                                        x_pos = x_pos[sample.int(n = total, size = total)],
                                        y_pos = y_pos[sample.int(n = total, size = total)],
                                        z_pos = z_pos[sample.int(n = total, size = total)],
                                        SigmaX = SigmaX[sample.int(n = total, size = total)])

    # calculate distances between simulated spots
    distances_i <- fish_colocalisation(spots_i)

    # find closest neighbour in opposite channel for simulated spots
    closest_neighbours_i <- fish_closest_spots(distances_i)

    # calculate colocalisation % for each channel
    # write into output data frame
    coloc <- sapply(closest_neighbours_i,
                    function(x) sum(x[["colocalised"]] / length(x[["colocalised"]]) * 100))
    null_distribution <- rbind.data.frame(null_distribution, coloc)
  }

  # change column names
  colnames(null_distribution) <- c("ch1", "ch2")

  # return output data frame
  null_distribution
}
