#' Extract Spot Data From FISH-QUANT File
#'
#' @param file File from FISH-QUANT (imported using readLines)
#' @param line_selection Data frame containing the line indices for each cell
#' @param channel The channel being analysed e.g. "Cy5"
#'
#' @return Data frame containing spot data in tidy format
#' @export
#'
fish_spot_extraction <- function(file, line_selection, channel){

  # create spots data frame from line_selection
  spots <- line_selection

  # create new column containing spot data
  # uses spot_extraction function to read out the appropriate lines from file
  spots[["spot_data"]] <- purrr::map2(spots[["spots_start"]],
                                      spots[["spots_end"]],
                                      ~ as.data.frame(spot_extraction(file = file,
                                                                      start = .x ,
                                                                      end = .y)))

  # add column containing number of spots for each cell
  # add column indicating channel
  spots <- dplyr::mutate(spots,
                         number_of_spots = sapply(spot_data, nrow))

  # convert to non-nested data frame
  spots <- tidyr::unnest(spots, spot_data)

  # add channel column
  # convert spot column to factor
  # add identifier column - cell/channel/spot
  # reorder columns
  # rename Pos_X to x_pos etc.
  spots <- dplyr::mutate(spots,
                         cell = cell,
                         channel = channel,
                         spot = as.factor(spot),
                         identifier = paste(cell, channel, spot, sep = "_"),
                         number_of_spots = number_of_spots,
                         .before = cell_has_spots)
  spots <- dplyr::rename(spots,
                         x_pos = Pos_X,
                         y_pos = Pos_Y,
                         z_pos = Pos_Z)

  # drop excess columns
  # remove grouping
  spots <- dplyr::select(spots,
                         -c(cell_has_spots, cell_start, cell_end, spots_start, spots_end))
  spots <- dplyr::ungroup(spots)

  # return spots data frame
  spots
}
