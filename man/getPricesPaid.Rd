% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{getPricesPaid}
\alias{getPricesPaid}
\title{GetPricesPaid}
\usage{
getPricesPaid(abm, addvars = NULL)
}
\arguments{
\item{abm}{output of runABM}

\item{addvars}{grouping variables in addition to segment, date e.g. segment, type, qev31. If NULL all prices}
}
\value{
dataframe of prices paid
}
\description{
monthly transaction prices
}
