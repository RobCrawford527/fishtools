#' Extract Spot Data From FISH-QUANT File
#'
#' @param path The file path of the FISH-QUANT output
#' @param channel The channel being analysed e.g. "Cy5"
#' @param mRNA The name of the mRNA being analysed e.g. "PGK1"
#'
#' @return Data frame containing spot data
#' @export
#'
import_spots <- function(path, channel = NA, mRNA = NA){

  # read in file from path
  fish <- readLines(path)

  # select lines of interest from file
  lines <- line_selection(fish)

  # extract spot data
  spots <- fish_spot_extraction(fish, lines, channel, mRNA)

  # return data frame
  spots
}
