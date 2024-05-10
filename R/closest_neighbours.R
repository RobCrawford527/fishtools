#' Identify Closest Neighbours In Opposite Channels
#'
#' @param distances Data frame containing all pairwise distances between spots in opposite channels
#'
#' @return List of two data frames containing closest neighbours for spots in each channel
#' @export
#'
closest_neighbours <- function(distances){

  # identify closest neighbour in opposite channel for each spot

  # group by spots in ch1 and keep only minimum distance for each spot
  # ungroup
  neighbours_ch1 <- dplyr::group_by(distances, cell, channel_1, mRNA_1, ch1_index)
  neighbours_ch1 <- dplyr::filter(neighbours_ch1, distance == min(distance))
  neighbours_ch1 <- dplyr::ungroup(neighbours_ch1)
  neighbours_ch1 <- dplyr::mutate(neighbours_ch1, reference_channel = "ch1")

  # group by spots in ch2 and keep only minimum distance for each spot
  # ungroup
  neighbours_ch2 <- dplyr::group_by(distances, cell, channel_2, mRNA_2, ch2_index)
  neighbours_ch2 <- dplyr::filter(neighbours_ch2, distance == min(distance))
  neighbours_ch2 <- dplyr::ungroup(neighbours_ch2)
  neighbours_ch2 <- dplyr::mutate(neighbours_ch2, reference_channel = "ch2")

  # combine into list
  # return output list
  neighbours <- list(ch1 = neighbours_ch1,
                     ch2 = neighbours_ch2)
  neighbours
}
