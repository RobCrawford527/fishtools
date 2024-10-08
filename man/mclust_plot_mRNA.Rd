% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mclust_plot_mRNA.R
\name{mclust_plot_mRNA}
\alias{mclust_plot_mRNA}
\title{Plot Histogram And Gaussian Mixture Model From Transformed Spot Data}
\usage{
mclust_plot_mRNA(spots, model, n_bins = 120, multi_threshold = 1.5)
}
\arguments{
\item{spots}{Data frame containing spot data}

\item{model}{Transformed Gaussian mixture model parameters to use. The output of fish_mclust and transform_model.}

\item{n_bins}{Number of bins to use (default = 120)}

\item{multi_threshold}{Minimum threshold to consider a spot a multi-mRNA granule}
}
\value{
A histogram with overlaid Gaussian mixture model
}
\description{
Plot Histogram And Gaussian Mixture Model From Transformed Spot Data
}
