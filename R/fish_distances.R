#' Calculate Distances Between Spots In Opposite Channels
#'
#' @param ch1_spots Data frame containing spot data from first channel
#' @param ch2_spots Data frame containing spot data from second channel
#'
#' @return Data frame containing all pairwise distances between spots in opposite channels
#' @export
#'
fish_distances <- function(ch1_spots, ch2_spots){

  # identify cells in each channel
  ch1_cells <- unique(ch1_spots$cell)
  ch2_cells <- unique(ch2_spots$cell)

  # find intersection between lists of cells
  # i.e. cells with at least one spot in each channel
  # order cells
  cells <- intersect(ch1_cells, ch2_cells)
  cells <- cells[order(cells)]

  # create empty data frame to record spot distances
  spot_distances <- data.frame()

  # for each cell, calculate distance between all spots in opposing channels
  # write into data frame
  for (i in cells){
    spot_distances_i <- distances(ch1_spots, ch2_spots, cell_of_interest = i)
    spot_distances <- rbind.data.frame(spot_distances, spot_distances_i)
  }

  # return output list
  spot_distances
}
