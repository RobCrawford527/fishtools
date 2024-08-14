#' Transform Spot Data To Number Of mRNAs Per Spot
#'
#' @param spots Data frame containing spot data
#' @param model Gaussian mixture model parameters, the output of fish_mclust
#' @param threshold Raw intensity threshold to use for filtering spot data
#' @param single_mRNA_component The component of the model that represents single mRNA spots
#' @param multi_threshold Minimum threshold to consider a spot a multi-mRNA granule
#'
#' @return A transformed version of the data containing the columns "number_of_mRNAs" and "multi"
#' @export
#'
transform_data <- function(spots,
                           model,
                           intensity_threshold = 0,
                           single_mRNA_component,
                           multi_threshold = 1.5){

  # create filtered version of data - only keep spots with intensities above threshold
  new_spots <- dplyr::filter(spots, INT_raw > intensity_threshold)

  # extract mean for single mRNA component
  single_mRNA_mean <- model[["parameters"]][["mean"]][single_mRNA_component]

  # transform values
  # create new column with number of mRNAs per spot
  # assign spots as singles or multis, as determined by multi_threshold
  new_spots <- dplyr::mutate(new_spots,
                             number_of_mRNAs = INT_raw / single_mRNA_mean,
                             single_or_multi = ifelse(number_of_mRNAs < multi_threshold,
                                                      "single",
                                                      "multi"))

  # return transformed data
  new_spots
}
