% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stock-db.R
\name{stocks_excess_return}
\alias{stocks_excess_return}
\title{Get stocks excess return from stocks return and riskfree_rate}
\usage{
stocks_excess_return(
  ts_stocks_return,
  ts_riskfree_rate,
  period = c("day", "month", "quarter", "year"),
  period_date = c("start", "end")
)
}
\arguments{
\item{ts_stocks_return}{A timeseries of stocks_return}

\item{ts_riskfree_rate}{A timeseries of riskfree_rate}

\item{period}{A character of period, e.g. "day", "month",
"quarter", "year". Default "day".}

\item{period_date}{A character of period_date format, e.g. "start",
"end", "start" format date as start of the period, "end" format date as end
of period. Default "start".}
}
\value{
A timeseries of excess return of stocks.
}
\description{
Get excess return timeseries by combining stocks return and riskfree_rate
}
