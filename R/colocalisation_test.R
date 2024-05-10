#' Use Threshold To Decide Whether Spots Are Colocalised
#'
#' @param distances List of two data frames containing distances between nearest pairs of spots
#' @param method Method used to determine colocalisation. Options are "radius" (uses sum of spot radii) or "absolute" (sets an absolute distance)
#' @param threshold Distance threshold (in nm) for determining colocalisation. Used if method = "absolute"
#' @param multiplier Multiplier for sum of radii. Set lower for more strict colocalisation. Used if method = "radius"
#'
#' @return List of two data frames with added columns indicating whether each pair of spots is colocalised or not (TRUE or FALSE)
#' @export
#'
colocalisation_test <- function(distances,
                                method = "absolute",
                                threshold = 500,
                                multiplier = 1){

  # assess if spots are colocalised
  # "absolute" uses an absolute distance threshold
  # "radius" uses the sum of the spot radii
  if (method == "absolute"){

    # colocalised if distance is less than absolute threshold value
    distances <- lapply(distances, function(x) dplyr::mutate(x,
                                                             colocalised = ifelse(distance < threshold, TRUE, FALSE)))

  } else if (method == "radius"){

    # colocalised if distance is less than sum of radii
    # multiplier is used to make threshold more strict
    distances <- lapply(distances, function(x) dplyr::mutate(x,
                                                             colocalised = ifelse(distance < fwhm_sum * multiplier, TRUE, FALSE)))

  }

  # calculate the percentage colocalisation for each channel
  # print to the console
  colocalisation <- lapply(distances, function(x) round(sum(x[["colocalised"]]) / nrow(x) * 100, 1))
  print(paste0(colocalisation[1],
              "% of channel 1 spots are colocalised with a channel 2 spot, using ",
              ifelse(method == "absolute",
                     paste0("absolute threshold of ", threshold, " nm"),
                     paste0("radius multiplier of ", multiplier))))
  print(paste0(colocalisation[2],
               "% of channel 2 spots are colocalised with a channel 1 spot, using ",
               ifelse(method == "absolute",
                      paste0("absolute threshold of ", threshold, " nm"),
                      paste0("radius multiplier of ", multiplier))))

  # return mutated list
  distances
}
