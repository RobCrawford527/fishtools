#' Fit A Gaussian Mixture Model To FISH Spot Data
#'
#' @param data Output list from fish_combined, containing cell outlines and spot data
#'
#' @return Parameters for a Gaussian mixture model from mclust
#' @export
#'
fish_mclust <- function(data){

  # check if mclust ia loaded and load it if not
  if (!isNamespaceLoaded("mclust")){
    library(mclust)
  }

  # create the model
  # uses default parameters for Mclust
  model <- mclust::Mclust(data = data[["spots"]][["INT_raw"]])

  # simplify the model
  # write into table containing parameters
  model <- list(n_spots = model$n,
                parameters = data.frame(component = 1:model$G,
                                        mean = model$parameters$mean,
                                        variance = sqrt(model$parameters$variance$sigmasq),
                                        proportion = model$parameters$pro))

  # print and return the model
  print(model)
  model
}
