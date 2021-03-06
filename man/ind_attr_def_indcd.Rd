% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator-define.R, R/indicator-define-gta.R
\name{ind_attr_def_indcd}
\alias{ind_attr_def_indcd}
\alias{ind_attr_def_indcd,gta_db-method}
\title{Get attribute definition function of industry code from stock_db}
\usage{
ind_attr_def_indcd(stock_db, ...)

\S4method{ind_attr_def_indcd}{gta_db}(stock_db, ...)
}
\arguments{
\item{stock_db}{A stock database object to operate.}

\item{...}{Extra arguments to be passed to methods.}
}
\value{
A function of attribute definition of industry code if succeed,
otherwise NULL.
}
\description{
Generic function to get attribute definition function of industry code
from stock_db.
}
\section{Functions}{
\itemize{
\item \code{ind_attr_def_indcd,gta_db-method}: get attribute definition function of
industry code from database of gta_db class.
}}

\seealso{
Other indicator define functions: 
\code{\link{create_attribute_def_fun}()},
\code{\link{create_indicator_def_fun}()},
\code{\link{get_indicator_defs}()},
\code{\link{get_indicator_vars}()},
\code{\link{ind_attr_def_trdstat}()},
\code{\link{parse_indicator_vars}()},
\code{\link{prioritize_indicator_defs}()},
\code{\link{related_indicator_defs}()}
}
\concept{indicator define functions}
