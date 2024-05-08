#' Summarise Number of Singles and Multis Per Cell in Spot Data
#'
#' @param spots Data frame containing spot data, converted to mRNAs per cell
#' @param threshold Threshold above which a spot is considered a multi mRNA foci
#'
#' @return Data frame containing summary data for sample of interest
#' @export
#'
singles_multis_summarise <- function(spots, threshold = 1.5){

  # add column specifying if single or multi
  # asks if number of mRNAs is greater than or equal to threshold
  spots_summary <- dplyr::mutate(spots,
                                 multi = ifelse(number_of_mRNAs >= threshold, TRUE, FALSE))

  # group data frame by cell and channel
  spots_summary <- dplyr::group_by(spots_summary,
                                   cell, channel, mRNA)

  # summarise data frame
  # calculate number of singles and multis
  # calculate percentage of foci that are multis
  # calculate estimated mRNA copies in singles and multis
  # calculate percentage of copies that are in multis
  spots_summary <- dplyr::summarise(spots_summary,
                                    number_total = dplyr::n(),
                                    number_singles = sum(multi == FALSE),
                                    number_multis = sum(multi == TRUE),
                                    percentage_number_multis = number_multis / number_total * 100,
                                    copies_total = sum(number_of_mRNAs),
                                    copies_singles = sum(number_of_mRNAs[multi == FALSE]),
                                    copies_multis = sum(number_of_mRNAs[multi == TRUE]),
                                    percentage_copies_multis = copies_multis / copies_total * 100)

  # return summarised data frame
  spots_summary
}
