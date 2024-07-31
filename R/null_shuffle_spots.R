#' Shuffle Channel Two Spots And Calculate Colocalisation
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
null_shuffle_spots <- function(ch1_spots,
                               ch2_spots,
                               method = "absolute",
                               threshold = 500,
                               multiplier = 1){

  # determine number of spots in channel 2
  total <- nrow(ch2_spots)

  # create new data frame for channel 2
  # ungroup
  # shuffle spots in channel 2: randomly sample the x, y and z positions for each spot, without replacement
  ch2_shuffled <- dplyr::ungroup(ch2_spots)
  ch2_shuffled <- dplyr::mutate(ch2_shuffled,
                                x_pos = x_pos[sample.int(n = total, size = total, replace = FALSE)],
                                y_pos = y_pos[sample.int(n = total, size = total, replace = FALSE)],
                                z_pos = z_pos[sample.int(n = total, size = total, replace = FALSE)],
                                SigmaX = SigmaX[sample.int(n = total, size = total, replace = FALSE)])

  # calculate distances between ch1 spots and shuffled ch2 spots
  shuffled_distances <- fish_distances(ch1_spots = ch1_spots,
                                       ch2_spots = ch2_shuffled)

  # find closest neighbour in opposite channel
  shuffled_nearest_spots <- closest_neighbours(shuffled_distances)

  # determine whether nearest spots are colocalised
  shuffled_nearest_spots <- colocalisation_test(shuffled_nearest_spots,
                                                method = method,
                                                threshold = threshold,
                                                multiplier = multiplier)

  # return colocalisation data frame
  shuffled_nearest_spots
}
