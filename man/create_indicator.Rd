% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator-build.R
\name{create_indicator}
\alias{create_indicator}
\title{Create indicator by definition function and variables timeseries}
\usage{
create_indicator(
  ts_def_vars,
  ind_def_fun,
  ...,
  debug = FALSE,
  date_index_field = c("date"),
  key_fields = NULL,
  parallel = getOption("zstmodelr.common.parallel", TRUE)
)
}
\arguments{
\item{ts_def_vars}{A dataframe of variable timeseries to create indicator.}

\item{ind_def_fun}{A function of defining indicator.}

\item{...}{Params to ind_def_fun.}

\item{debug}{A logic to determine whether to turn on debug in creating
indicator. Default FALSE means not to use debug.}

\item{date_index_field}{Name of date index field of ts_def_vars,
default 'date'.}

\item{key_fields}{A character vector of key fields, which identify unique
observation in each date. Default NULL means to not divide data into
groups.}

\item{parallel}{A logic to determine whether to use parallel processing.
Default TRUE means to use parallel processing.}
}
\value{
A dataframe of new indicator timeseries if succeed, otherwise NULL.
}
\description{
Use definition function and variable timeseries to create indicator.
}
\examples{
\dontrun{

# load vars dataset for generating indicators
ds_all_vars <- get_indicator_vars(stock_db,
  indicator_defs = ds_indicator_defs
)

# create ind_expr
indicator_formula <- c("stock_return <- mretwd
                          market_return <- cmretwdtl
                          model <- lm(stock_return ~ market_return)
                          beta <- coef(model)['market_return']")
indicator_expr <- create_expr(!!indicator_formula)

# create def_fun for indicator
indicator_def_fun <- create_indicator_def_fun(
  indicator_code = "m_stock_beta1",
  indicator_expr = indicator_expr,
  rolly_window = 12,
  period = "month"
)

# create a indicator from vars dataset.
ts_indicator <- create_indicator(
  ds_def_vars,
  ind_def_fun = ind_def_fun,
  debug = FALSE,
  date_index_field = "date",
  key_fields = "stkcd",
  parallel = TRUE
)
}
}
\seealso{
Other indicator build functions: 
\code{\link{compute_indicator}()},
\code{\link{modify_indicator}()}
}
\concept{indicator build functions}
