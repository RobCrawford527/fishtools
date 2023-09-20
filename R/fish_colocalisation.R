#' Calculate Distances Between Spots In Opposite Channels
#'
#' @param spots Data frame containing spot data in two channels
#' @param method Method used to determine colocalisation. Options are "radius" (uses sum of spot radii) or "absolute" (sets an absolute distance)
#' @param multiplier Multiplier for sum of radii. Set lower for more strict colocalisation. Used if method = "radius"
#' @param threshold Distance threshold (in nm) for determining colocalisation. Used if method = "absolute"
#'
#' @return Data frame containing all pairwise distances between spots in opposite channels
#' @export
#'
fish_colocalisation <- function(spots, method = "radius", multiplier = 1, threshold = 500){

  # find unique cells
  # order cells
  cells <- unique(spots[["spots"]][["cell"]])
  cells <- cells[order(cells)]

  # create empty list to record spot distances
  spot_distances <- data.frame()

  # for each cell, calculate distance between all spots in opposing channels
  # write into data frame
  for (i in cells){
    spot_distances_i <- colocalisation(spots[["spots"]], i)
    spot_distances <- rbind.data.frame(spot_distances, spot_distances_i)
  }

  # return output list
  spot_distances
}
