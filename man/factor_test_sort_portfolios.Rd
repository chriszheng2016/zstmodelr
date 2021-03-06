% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor-test.R
\name{factor_test_sort_portfolios}
\alias{factor_test_sort_portfolios}
\title{Creator of factor_test_sort_portfolios class}
\usage{
factor_test_sort_portfolios(
  ds_test,
  sort_portfolios_fun,
  ...,
  output_type = c("summary", "raw"),
  factor_field = "factor_name",
  date_field = "date",
  stkcd_field = "stkcd",
  return_field = "return"
)
}
\arguments{
\item{ds_test}{A timeseries dataset with descriptors of factors for test.}

\item{sort_portfolios_fun}{A function to sort descriptors of factors to build.
portfolios for test}

\item{...}{Arguments passed to sort_portfolios_fun.}

\item{output_type}{Type of output data, i.e."summary", "raw", if "raw",
raw data will be append to output object for diagnosis.}

\item{factor_field}{Name of factor field of ds_test, by default "factor_name"}

\item{date_field}{Name of date field of ds_test, by default "date",
Column must be date-like.}

\item{stkcd_field}{Name of stkcd field of ds_test, by default "stkcd".}

\item{return_field}{Name of return field of ds_test, by default "return".}
}
\value{
\preformatted{            A object of factor_test_sort_portfolios class.
}
}
\description{
Conduct factor sort portfolios test for descriptors of factors and build object
of factor_test_sort_portfolios class as output.
}
