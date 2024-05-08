#' Separate Single and Multi mRNA Foci
#'
#' @param spots Data frame containing spot data, converted to mRNAs per cell
#' @param threshold Threshold above which a spot is considered a multi mRNA foci
#'
#' @return List containing data frames of single and multi mRNA foci
#' @export
#'
singles_multis_extract <- function(spots, threshold = 1.5){

  # extract single mRNA foci and multi mRNA foci
  singles <- dplyr::filter(spots, number_of_mRNAs < threshold)
  multis <- dplyr::filter(spots, number_of_mRNAs >= threshold)

  # combine into list
  output <- list(singles = singles,
                 multis = multis)

  # return output list
  output
}
