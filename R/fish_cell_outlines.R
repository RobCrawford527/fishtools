#' Extract Cell Outlines From FISH-QUANT File
#'
#' @param file File from FISH-QUANT (imported using readLines)
#' @param line_selection Data frame containing the line indices for each cell
#'
#' @return Data frame containing the cell outlines in tidy format
#' @export
#'
fish_cell_outlines <- function(file, line_selection){

  # add new column to line_selection data frame
  # uses the cell_start column from line_selection
  # this specifies which line in the file denotes the start of each cell
  # for each cell, a data frame is created and added to the data frame as a list column
  outlines <- dplyr::mutate(line_selection,
                            outline = lapply(cell_start,
                                             function(index) data.frame(x_pos = type.convert(unlist(strsplit(file[index + 1],
                                                                                                             split = "\t"))[-1],
                                                                                             as.is = TRUE),
                                                                        y_pos = type.convert(unlist(strsplit(file[index + 2],
                                                                                                             split = "\t"))[-1],
                                                                                             as.is = TRUE))))

  # convert into non-nested data frame for ease of plotting
  # convert cell column to factor
  outlines <- tidyr::unnest(outlines, outline)
  outlines <- dplyr::mutate(outlines,
                            cell = as.factor(cell))

  # remove excess columns
  # keep only cell and x and y positions
  # remove grouping
  outlines <- dplyr::select(outlines,
                            cell, x_pos, y_pos)
  outlines <- dplyr::ungroup(outlines)

  # return cell outlines
  outlines
}
