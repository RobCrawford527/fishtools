#' Determine Which Lines To Use From FISH-QUANT File
#'
#' @param file File from FISH-QUANT (imported using readLines)
#'
#' @return Data frame containing the line indices for each cell
#' @export
#'
line_selection <- function(file){

  # identify lines corresponding to cell and spot starts and ends
  lines <- tibble::tibble(line = 1:length(file),
                          cell_start = ifelse(grepl("CELL_START", file) == TRUE, line, NA),
                          cell_end = ifelse(grepl("CELL_END", file) == TRUE, line, NA),
                          spots_start = ifelse(grepl("SPOTS_START", file) == TRUE, line, NA),
                          spots_end = ifelse(grepl("SPOTS_END", file) == TRUE, line, NA),
                          cell = cumsum(!is.na(cell_start)))

  # group by cell
  # determine if each cell contains spots
  lines <- dplyr::group_by(lines, cell)
  lines <- dplyr::mutate(lines,
                         cell_has_spots = any(!is.na(spots_start) == TRUE))

  # simplify data frame so each cell is a single line
  # create selected and filtered version for all four columns of interest
  # join together using left_join
  line_selection <- dplyr::filter(dplyr::select(lines,
                                                cell, cell_has_spots, cell_start),
                                  !is.na(cell_start))
  line_selection <- dplyr::left_join(line_selection, dplyr::filter(dplyr::select(lines,
                                                                                 cell, cell_end),
                                                                   !is.na(cell_end)),
                                     by = "cell")
  line_selection <- dplyr::left_join(line_selection, dplyr::filter(dplyr::select(lines,
                                                                                 cell, spots_start),
                                                                   !is.na(spots_start)),
                                     by = "cell")
  line_selection <- dplyr::left_join(line_selection, dplyr::filter(dplyr::select(lines,
                                                                                 cell, spots_end),
                                                                   !is.na(spots_end)),
                                     by = "cell")

  # convert cell to factor
  line_selection <- dplyr::mutate(line_selection,
                                  cell = as.factor(cell))

  # return line_selection data frame
  line_selection
}
