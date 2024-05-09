#' Standardise Cell Outline Position Data From FISH-QUANT
#'
#' @param spots Data frame containing spot data
#' @param outlines Data frame containing cell outlines
#' @param pixel_size The size of each pixel (in nanometers). If NULL (default), calculated from spot data: ratio of spot position in nm to spot position in pixels
#'
#' @return Data frame containing cell outlines, with standardised positions
#' @export
#'
standardise_outlines <- function(spots, outlines, pixel_size = NULL){
  
  # calculate pixel size if not set already
  if (is.null(pixel_size)){
    
    # calculate ratios for x and y
    # ratio of position in nm to position in pixels
    x_ratio <- spots[["x_pos"]] / spots[["X_det"]]
    y_ratio <- spots[["y_pos"]] / spots[["Y_det"]]
    
    # determine pixel size
    # calculate mean ratio across x and y
    pixel_size <- mean(c(x_ratio, y_ratio))
    
    # print pixel size
    print(pixel_size)
  }
  
  # multiply by pixel_size to convert outline data to nanometers
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
  
  # keep only cell and x and y position columns
  outlines <- dplyr::select(outlines,
                            cell, x_pos, y_pos)
  
  # return output data frame
  outlines
}
