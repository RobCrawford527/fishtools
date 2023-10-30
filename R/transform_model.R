transform_model <- function(model, single_mRNA_component){
  
  # extract mean for single mRNA component
  single_mRNA_mean <- model[["parameters"]][["mean"]][single_mRNA_component]
  
  # create new version of model
  # calculate adjusted mean and variance
  # simply parameters divided by single mRNA mean
  new_model <- model
  new_model[["parameters"]][["number_of_mRNAs"]] <- new_model[["parameters"]][["mean"]] / single_mRNA_mean
  new_model[["parameters"]][["variance_mRNA"]] <- new_model[["parameters"]][["variance"]] / single_mRNA_mean
  
  # print and return the transformed model
  print(new_model)
  new_model
}
