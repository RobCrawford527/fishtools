#' Transform Spot Data To Number Of mRNAs Per Spot
#'
#' @param spots Data frame containing spot data
#' @param model Gaussian mixture model parameters, the output of fish_mclust
#' @param threshold Raw intensity threshold to use for filtering spot data
#' @param single_mRNA_component The component of the model that represents single mRNA spots
#'
#' @return A transformed version of the data containing the column number_of_mRNAs, which is a transformation of the raw intensity
#' @export
#'
transform_data <- function(spots, model, threshold = 0, single_mRNA_component){

  # create filtered version of data - only keep spots with intensities above threshold
  new_spots <- dplyr::filter(spots, INT_raw > threshold)

  # extract mean for single mRNA component
  single_mRNA_mean <- model[["parameters"]][["mean"]][single_mRNA_component]

  # transform values
  # create new column with number of mRNAs per spot
  new_spots <- dplyr::mutate(new_spots,
                             number_of_mRNAs = INT_raw / single_mRNA_mean)

  # return transformed data
  new_spots
}
