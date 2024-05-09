#' Extract Cell Outlines From FISH-QUANT File
#'
#' @param path The file path of the FISH-QUANT output
#'
#' @return Data frame containing cell outlines
#' @export
#'
import_outlines <- function(path){

  # read in file from path
  fish <- readLines(path)

  # select lines of interest from file
  lines <- line_selection(fish)

  # extract spot data
  outlines <- fish_cell_outlines(fish, lines)

  # return data frame
  outlines
}
