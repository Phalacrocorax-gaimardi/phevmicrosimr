% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/technology.R
\name{fuel_price_fun1}
\alias{fuel_price_fun1}
\title{fuel_price_fun1}
\usage{
fuel_price_fun1(type = "diesel", sD, yeartime)
}
\arguments{
\item{type}{fuel type ptrol or diesel}

\item{sD}{scenario}

\item{yeartime}{decimal time}
}
\value{
euros per litre
}
\description{
cost of motor fuels at yeartime
}
\examples{
fuel_price_fun1("gasoline",scenario_B,2025)

}
