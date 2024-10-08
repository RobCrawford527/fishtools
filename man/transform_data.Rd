% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_data.R
\name{transform_data}
\alias{transform_data}
\title{Transform Spot Data To Number Of mRNAs Per Spot}
\usage{
transform_data(
  spots,
  model,
  intensity_threshold = 0,
  single_mRNA_component,
  multi_threshold = 1.5
)
}
\arguments{
\item{spots}{Data frame containing spot data}

\item{model}{Gaussian mixture model parameters, the output of fish_mclust}

\item{single_mRNA_component}{The component of the model that represents single mRNA spots}

\item{multi_threshold}{Minimum threshold to consider a spot a multi-mRNA granule}

\item{threshold}{Raw intensity threshold to use for filtering spot data}
}
\value{
A transformed version of the data containing the columns "number_of_mRNAs" and "multi"
}
\description{
Transform Spot Data To Number Of mRNAs Per Spot
}
