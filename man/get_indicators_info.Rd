% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stock-db.R, R/stock-db-gta.R
\name{get_indicators_info}
\alias{get_indicators_info}
\alias{get_indicators_info,gta_db-method}
\title{Get indicators Info from stock_db}
\usage{
get_indicators_info(stock_db, indicator_codes = NULL, ...)

\S4method{get_indicators_info}{gta_db}(stock_db, indicator_codes = NULL, ...)
}
\arguments{
\item{stock_db}{A stock database object to operate.}

\item{indicator_codes}{A character vector of indicator groups.
Default NULL return all indicators.}

\item{...}{Extra arguments to be passed to methods.}
}
\value{
A dataframe of matched indicators if succeed, otherwise NULL.
}
\description{
Generic function to get indicator info from stock_db.
}
\section{Functions}{
\itemize{
\item \code{get_indicators_info,gta_db-method}: get indicators info from a database of
gta_db class
}}

\seealso{
Other stock_db generics: 
\code{\link{close_stock_db}()},
\code{\link{code2name}()},
\code{\link{dir_path_db}()},
\code{\link{get_factors_info}()},
\code{\link{get_factors}()},
\code{\link{get_financial_report}()},
\code{\link{get_indicators_from_source}()},
\code{\link{get_indicators}()},
\code{\link{get_industry_info}()},
\code{\link{get_market_return}()},
\code{\link{get_profile}()},
\code{\link{get_riskfree_rate}()},
\code{\link{get_spt_stocks}()},
\code{\link{get_stock_dataset}()},
\code{\link{get_stock_industry}()},
\code{\link{get_stock_info}()},
\code{\link{get_stock_return}()},
\code{\link{get_table_dataset}()},
\code{\link{init_stock_db}()},
\code{\link{list_stock_tables}()},
\code{\link{name2code}()},
\code{\link{open_stock_db}()},
\code{\link{save_indicators_to_source}()}
}
\concept{stock_db generics}
