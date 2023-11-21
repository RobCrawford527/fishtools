#' Use Threshold To Decide Whether Spots Are Colocalised
#'
#' @param distances Data frame containing distances between pairs of spots
#' @param method Method used to determine colocalisation. Options are "radius" (uses sum of spot radii) or "absolute" (sets an absolute distance)
#' @param threshold Distance threshold (in nm) for determining colocalisation. Used if method = "absolute"
#' @param multiplier Multiplier for sum of radii. Set lower for more strict colocalisation. Used if method = "radius"
#'
#' @return Data frame with an added column indicating whether each pair of spots is colocalised or not (TRUE or FALSE)
#' @export
#'
fish_coloc_threshold <- function(distances,
                                 method = "absolute",
                                 threshold = 500,
                                 multiplier = 1){

  # assess if spots are colocalised
  # "absolute" uses an absolute distance threshold
  # "radius" uses the sum of the spot radii
  if (method == "absolute"){

    # colocalised if distance is less than absolute threshold value
    distances <- dplyr::mutate(distances,
                               colocalised = ifelse(distance < threshold, TRUE, FALSE))

  } else if (method == "radius"){

    # colocalised if distance is less than sum of radii
    # multiplier is used to make threshold more strict
    distances <- dplyr::mutate(distances,
                               colocalised = ifelse(distance < fwhm_sum * multiplier, TRUE, FALSE))

  }

  # return mutated data frame
  distances
}
