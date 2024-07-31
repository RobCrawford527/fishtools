#' Standardise Spot Position Data From FISH-QUANT
#'
#' @param spots Data frame containing spot data
#' @param outlines Data frame containing cell outlines
#' @param pixel_size The size of each pixel (in nanometers). If NULL (default), calculated from spot data: ratio of spot position in nm to spot position in pixels
#'
#' @return Data frame containing spot data, with standardised positions
#' @export
#'
standardise_spots <- function(spots, outlines, pixel_size = NULL){

  # calculate pixel size if not set already
  if (is.null(pixel_size)){

    # calculate ratios for x and y
    # ratio of position in nm to position in pixels
    x_ratio <- spots[["x_pos"]] / spots[["X_det"]]
    y_ratio <- spots[["y_pos"]] / spots[["Y_det"]]

    # determine pixel size
    # calculate mean ratio across x and y
    pixel_size <- median(c(x_ratio, y_ratio))

    # print pixel size
    print(pixel_size)
  }

  # multiply by pixel_size to convert outline data to nanometers
  outlines <- dplyr::mutate(outlines,
                            x_pos = x_pos * pixel_size,
                            y_pos = y_pos * pixel_size)

  # convert cell column to factor
  # group by cell
  # calculate average of maximum and minimum x and y values to find cell centres
  outlines <- dplyr::mutate(outlines,
                            cell = as.factor(cell))
  outlines <- dplyr::group_by(outlines, cell)
  outlines <- dplyr::mutate(outlines,
                            x_cen = (max(x_pos) + min(x_pos)) / 2,
                            y_cen = (max(y_pos) + min(y_pos)) / 2)

  # standardise outlines by calculating x and y distances from cell centres
  outlines <- dplyr::mutate(outlines,
                            x_pos = x_pos - x_cen,
                            y_pos = y_pos - y_cen)

  # extract set of cell xy centres
  centres_xy <- dplyr::distinct(dplyr::select(outlines,
                                              cell, x_cen, y_cen))

  # convert cell column to factor
  # group by cell
  spots <- dplyr::mutate(spots,
                         cell = as.factor(cell))
  spots <- dplyr::group_by(spots, cell)

  # calculate average of maximum and minimum z values to find cell centres
  # this method used because cell outlines do not have z data
  centres_z <- dplyr::summarise(spots,
                                z_cen = (max(z_pos) + min(z_pos)) / 2)

  # join xy centres and z centres
  centres <- dplyr::left_join(centres_xy, centres_z, by = dplyr::join_by(cell))

  # join xyz centres to spot data
  # standardise spot positions by calculating x, y and z distances from cell centres
  # ungroup
  spots <- dplyr::left_join(spots, centres, by = dplyr::join_by(cell))
  spots <- dplyr::mutate(spots,
                         x_pos = x_pos - x_cen,
                         y_pos = y_pos - y_cen,
                         z_pos = z_pos - z_cen)
  spots <- dplyr::ungroup(spots)

  # remove centre columns from data frame
  spots <- dplyr::select(spots,
                         -x_cen, -y_cen, -z_cen)

  # return output data frame
  spots
}
