\name{battery_cost_function}
\alias{battery_cost_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
battery_cost_function(sD, yeartime, nlag = 4.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sD}{
%%     ~~Describe \code{sD} here~~
}
  \item{yeartime}{
%%     ~~Describe \code{yeartime} here~~
}
  \item{nlag}{
%%     ~~Describe \code{nlag} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sD, yeartime, nlag = 4.5) 
{
    year_100 <- dplyr::filter(sD, parameter == "year_100")$value
    alpha <- dplyr::filter(sD, parameter == "learning_exponent")$value
    b1 <- dplyr::filter(sD, parameter == "cost_floor")$value/2
    b0 <- filter(bnefprices, year == 2019)$pack_ekWh - b1
    if (yeartime <= 2019.5 + nlag) 
        return(stats::approx(x = bnefprices$year + 0.5, y = bnefprices$pack_ekWh, 
            xout = yeartime - nlag)$y)
    if (yeartime >= 2019.5 + nlag) 
        return(max(b1, b0 * exp(-0.2 * (yeartime - (2019.5 + 
            nlag))) + b1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
