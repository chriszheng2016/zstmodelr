% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor-normalize.R
\name{clean_extremes_mad}
\alias{clean_extremes_mad}
\title{Clean extremes by MAD method}
\usage{
clean_extremes_mad(x, n_dmad = 3, extreme_value = c("limit", "NA"))
}
\arguments{
\item{x}{a vector of data.}

\item{n_dmad}{numbers of dmad to identify extremes, by default 3.}

\item{extreme_value}{value to replace extremes, i.e. "limit","NA",
by default "limit".}
}
\value{
\preformatted{ a vector of data without extremes
}
}
\description{
Identify extremes in data vector by MAD method, and replace extremes
by NA or max/min limitation
}
