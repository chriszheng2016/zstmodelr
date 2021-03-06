% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-fields.R
\name{verify_fields}
\alias{verify_fields}
\title{Verify existence of fields in data}
\usage{
verify_fields(data, fields)
}
\arguments{
\item{data}{A dataframe or matrix to check.}

\item{fields}{A character vector of field names to check.}
}
\value{
Invisible NULL if succeed, otherwise raise error.
}
\description{
Verify whether some fields exist in the data or not.
}
\seealso{
Other utils_fields: 
\code{\link{expect_type_fields}()},
\code{\link{is_type_field}()}
}
\concept{utils_fields}
