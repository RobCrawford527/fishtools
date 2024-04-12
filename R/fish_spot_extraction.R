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
  data <- line_selection

  # create new column containing spot data
  # uses spot_extraction function to read out the appropriate lines from file
  data[["spot_data"]] <- purrr::map2(data[["spots_start"]],
                                     data[["spots_end"]],
                                     ~ as.data.frame(spot_extraction(file = file,
                                                                     start = .x ,
                                                                     end = .y)))

  # add column containing number of spots for each cell
  # add column indicating channel
  data <- dplyr::mutate(data,
                        number_of_spots = sapply(spot_data, nrow))

  # convert to non-nested data frame
  data <- tidyr::unnest(data, spot_data)

  # add channel column
  # convert spot column to factor
  # add identifier column - cell/channel/spot
  # reorder columns
  # rename Pos_X to x_pos etc.
  data <- dplyr::mutate(data,
                        cell = type.convert(cell, as.is = TRUE),
                        channel = channel,
                        spot = as.factor(spot),
                        .before = cell_has_spots)
  data <- dplyr::rename(data,
                        x_pos = Pos_X,
                        y_pos = Pos_Y,
                        z_pos = Pos_Z)

  # drop excess columns
  # remove grouping
  data <- dplyr::select(data,
                        -c(cell_has_spots, cell_start, cell_end, spots_start, spots_end))
  data <- dplyr::ungroup(data)

  # return spots data frame
  data
}
