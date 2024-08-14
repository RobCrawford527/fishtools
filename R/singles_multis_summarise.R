#' Summarise Number of Singles and Multis Per Cell in Spot Data
#'
#' @param spots Data frame containing spot data, converted to mRNAs per cell
#'
#' @return Data frame containing summary data for sample of interest
#' @export
#'
singles_multis_summarise <- function(spots){

  # group data frame by cell and channel
  spots_summary <- dplyr::group_by(spots,
                                   cell, channel, mRNA)

  # summarise data frame
  # calculate number of singles and multis
  # calculate percentage of foci that are multis
  # calculate estimated mRNA copies in singles and multis
  # calculate percentage of copies that are in multis
  spots_summary <- dplyr::summarise(spots_summary,
                                    number_total = dplyr::n(),
                                    number_singles = sum(single_or_multi == "single"),
                                    number_multis = sum(single_or_multi == "multi"),
                                    percentage_number_multis = number_multis / number_total * 100,
                                    copies_total = sum(number_of_mRNAs),
                                    copies_singles = sum(number_of_mRNAs[single_or_multi == "single"]),
                                    copies_multis = sum(number_of_mRNAs[single_or_multi == "multi"]),
                                    percentage_copies_multis = copies_multis / copies_total * 100)

  # return summarised data frame
  spots_summary
}
