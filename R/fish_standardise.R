#' Standardise Cell And Spot Position Data From FISH-QUANT
#'
#' @param data Output list from fish_combined, containing cell outlines and spot data
#' @param pixel_size The size of each pixel (in nanometers). If NULL (default), calculated from spot data: ratio of spot position in nm to spot position in pixels
#'
#' @return List containing cell centres, cell outlines and spots
#' @export
#'
fish_standardise <- function(data, pixel_size = NULL){

  # calculate pixel size if not set already
  if (is.null(pixel_size)){

    # calculate ratios for x and y
    # ratio of position in nm to position in pixels
    x_ratio <- data[["spots"]][["x_pos"]] / data[["spots"]][["X_det"]]
    y_ratio <- data[["spots"]][["y_pos"]] / data[["spots"]][["Y_det"]]

    # determine pixel size
    # calculate mean ratio across x and y
    pixel_size <- mean(c(x_ratio, y_ratio))

    # print pixel size
    print(pixel_size)
  }

  # extract outline data
  # multiply by pixel_size to convert outline data to nanometers
  outlines <- data[["outlines"]]
  outlines <- dplyr::mutate(outlines,
                            x_pos = x_pos * pixel_size,
                            y_pos = y_pos * pixel_size)

  # group by cell
  # calculate mean x and y positions for each cell to approximate xy centres
  outlines <- dplyr::group_by(outlines, cell)
  outlines <- dplyr::mutate(outlines,
                            x_cen = mean(x_pos),
                            y_cen = mean(y_pos))

  # standardise outlines by calculating x and y distances from cell centres
  outlines <- dplyr::mutate(outlines,
                            x_pos = x_pos - x_cen,
                            y_pos = y_pos - y_cen)

  # extract set of cell xy centres
  centres_xy <- dplyr::distinct(dplyr::select(outlines,
                                              cell, x_cen, y_cen))

  # extract spot data
  # group by cell
  spots <- data[["spots"]]
  spots <- dplyr::group_by(spots, cell)

  # calculate mean z positions for spots in each cell to approximate z centres
  # this method used because cell outlines do not have z data
  centres_z <- dplyr::summarise(spots,
                                z_cen = mean(z_pos))

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

  # create output list containing centres, outlines and spots
  output <- list(centres = centres,
                 outlines = dplyr::select(outlines,
                                          cell, x_pos, y_pos),
                 spots = dplyr::select(spots,
                                       -x_cen, -y_cen, -z_cen))

  # return output data frame
  output
}
