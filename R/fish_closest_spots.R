fish_closest_spots <- function(distances){
  
  # identify closest neighbour in opposite channel for each spot
  
  # group by spots in ch1 and keep only minimum distance for each spot
  # ungroup
  closest_spots_ch1 <- dplyr::group_by(distances, cell_ch1)
  closest_spots_ch1 <- dplyr::filter(closest_spots_ch1, distance == min(distance))
  closest_spots_ch1 <- dplyr::ungroup(closest_spots_ch1)
  
  # group by spots in ch2 and keep only minimum distance for each spot
  # ungroup
  closest_spots_ch2 <- dplyr::group_by(distances, cell_ch2)
  closest_spots_ch2 <- dplyr::filter(closest_spots_ch2, distance == min(distance))
  closest_spots_ch2 <- dplyr::ungroup(closest_spots_ch2)
  
  # combine into list
  # return output list
  closest_spots <- list(ch1 = closest_spots_ch1,
                        ch2 = closest_spots_ch2)
  closest_spots
}
