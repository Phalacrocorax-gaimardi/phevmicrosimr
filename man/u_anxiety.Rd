% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/technology.R
\name{u_anxiety}
\alias{u_anxiety}
\title{u_anxiety}
\usage{
u_anxiety(mileage, AER, shape = 2.5, anxiety)
}
\arguments{
\item{mileage}{annual mileage in km}

\item{AER}{range in km}

\item{shape}{gamma distribution}

\item{anxiety}{range anxiety. 1 is non-anxious case, > 1 is anxious case}
}
\value{
probabilty that a daily trip exceeds AER
}
\description{
Range anxiety is assumed to reflect the probability that the daily trip exceeds the electric vehicle AER, requiring a charge.
the distribution of daily trips is gamma distributed with shape parameter 2.5. subjectivity via "anxiety" parameter anxiety
}
