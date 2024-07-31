#' Rotate Channel Two Spots And Calculate Colocalisation
#'
#' @param ch1_spots Data frame containing spot data from first channel
#' @param ch2_spots Data frame containing spot data from second channel
#' @param method Method used to determine colocalisation. Options are "radius" (uses sum of spot radii) or "absolute" (sets an absolute distance)
#' @param threshold Distance threshold (in nm) for determining colocalisation. Used if method = "absolute"
#' @param multiplier Multiplier for sum of radii. Set lower for more strict colocalisation. Used if method = "radius"
#'
#' @return List of two data frames indicating whether spots are colocalised
#' @export
#'
null_rotate_spots <- function(ch1_spots,
                              ch2_spots,
                              method = "absolute",
                              threshold = 500,
                              multiplier = 1){

  # create rotated version of ch2 spot data
  # x, y and z positions are mirrored
  ch2_rotated <- dplyr::mutate(ch2_spots,
                               x_pos = -x_pos,
                               y_pos = -y_pos,
                               z_pos = -z_pos)

  # calculate distances between ch1 spots and rotated ch2 spots
  rotated_distances <- fish_distances(ch1_spots = ch1_spots,
                                      ch2_spots = ch2_rotated)

  # find closest neighbour in opposite channel
  rotated_nearest_spots <-  closest_neighbours(rotated_distances)

  # determine whether nearest spots are colocalised
  rotated_nearest_spots <-  colocalisation_test(rotated_nearest_spots,
                                                method = method,
                                                threshold = threshold,
                                                multiplier = multiplier)

  # return colocalisation data frame
  rotated_nearest_spots
}
