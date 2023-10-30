transform_data <- function(data, model, threshold = 0, single_mRNA_component){
  
  # create new version of data
  # filter data - only keep spots with intensities above threshold
  new_data <- data
  new_data[["spots"]] <- dplyr::filter(new_data[["spots"]], INT_raw > threshold)
  
  # extract mean for single mRNA component
  single_mRNA_mean <- model[["parameters"]][["mean"]][single_mRNA_component]
  
  # transform values
  # create new column with number of mRNAs per spot
  new_data[["spots"]] <- dplyr::mutate(new_data[["spots"]],
                                       number_of_mRNAs = INT_raw / single_mRNA_mean)
  
  # return transformed data
  new_data
}
