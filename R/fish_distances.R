#' Calculate Distances Between Spots In Opposite Channels
#'
#' @param spots Data frame containing spot data in two channels
#'
#' @return Data frame containing all pairwise distances between spots in opposite channels
#' @export
#'
fish_distances <- function(spots){

  # find unique cells
  # order cells
  cells <- unique(spots[["spots"]][["cell"]])
  cells <- cells[order(cells)]

  # create empty list to record spot distances
  spot_distances <- data.frame()

  # for each cell, calculate distance between all spots in opposing channels
  # write into data frame
  for (i in cells){
    spot_distances_i <- distances(spots[["spots"]], i)
    spot_distances <- rbind.data.frame(spot_distances, spot_distances_i)
  }

  # return output list
  spot_distances
}
