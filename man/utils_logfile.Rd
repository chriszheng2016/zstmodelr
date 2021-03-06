% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-logfile.R
\name{utils_logfile}
\alias{utils_logfile}
\alias{save_log}
\alias{read_log}
\title{Utility functions of log file}
\usage{
save_log(ds_log_info, log_file_prefix, log_dir = "./log")

read_log(log_file_name, log_dir = "./log")
}
\arguments{
\item{ds_log_info}{A dataframe of log info to save.}

\item{log_file_prefix}{A prefix of log file, which define log file as
in format of "log_file_prefix(current).csv"}

\item{log_dir}{A character of log dir where log file is located.
Default "./".}

\item{log_file_name}{A character name of log file to read.}
}
\value{
\strong{save_log()}: full path of save log file.

\strong{read_log}: A dataframe of log info if succeed, otherwise NULL.
}
\description{
Utility functions to handle log file, which contains log info of operating
in stock database.
}
\details{
The log file is saved and read as "csv" file.
}
\section{Functions}{
\itemize{
\item \code{save_log}: save log info into log file.

\item \code{read_log}: read log info from log file.
}}

