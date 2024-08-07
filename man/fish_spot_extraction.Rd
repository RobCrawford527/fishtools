% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fish_spot_extraction.R
\name{fish_spot_extraction}
\alias{fish_spot_extraction}
\title{Extract Spot Data From FISH-QUANT File}
\usage{
fish_spot_extraction(file, line_selection, channel, mRNA)
}
\arguments{
\item{file}{File from FISH-QUANT (imported using readLines)}

\item{line_selection}{Data frame containing the line indices for each cell}

\item{channel}{The channel being analysed e.g. "Cy5"}

\item{mRNA}{The name of the mRNA being analysed e.g. "PGK1"}
}
\value{
Data frame containing spot data in tidy format
}
\description{
Extract Spot Data From FISH-QUANT File
}
