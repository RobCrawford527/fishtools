#' Extract Cell Outlines And Spot Data From FISH-QUANT File
#'
#' @param path The file path of the FISH-QUANT output
#' @param channel The channel being analysed e.g. "Cy5"
#'
#' @return List containing data frames of cell outlines and spot data
#' @export
#'
fish_combined <- function(path, channel){

  # read in file from path
  fish <- readLines(path)

  # select lines of interest from file
  line_selection <- fish_line_selection(fish)

  # extract cell outlines
  cell_outlines <- fish_cell_outlines(fish, line_selection)

  # extract spot data
  spots <- fish_spot_extraction(fish, line_selection, channel)

  # create list with cell outlines and spots
  # return list
  output <- list(outlines = cell_outlines,
                 spots = spots)
  output
}
