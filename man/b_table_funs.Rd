% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/b_table_funs.R
\name{b01001_}
\alias{b01001_}
\alias{b02001_}
\alias{b03002_}
\alias{b17001_}
\title{ACS B-table variable list creation}
\usage{
b01001_(var_num)

b02001_(var_num)

b03002_(var_num)

b17001_(var_num)
}
\arguments{
\item{var_num}{vector of numbers between 1 and 999, each representing a
3-digit variable number within the ACS table.}
}
\value{
A vector of ACS variable names
}
\description{
Quickly create a list of ACS variable names in a format that is
compatible with the tidycensus package.
}
\examples{
b17001_(1:2) # "B17001_001" "B17001_002"
b02001_(1:4) # "B02001_001" "B02001_002" "B02001_003" "B02001_004"
b01001_(c(1, 3:25, 27:49))
}
