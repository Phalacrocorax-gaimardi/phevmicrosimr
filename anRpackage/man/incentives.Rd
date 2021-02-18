\name{incentives}
\alias{incentives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
incentives(type, p1, params, flag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{p1}{
%%     ~~Describe \code{p1} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{flag}{
%%     ~~Describe \code{flag} here~~
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
function (type, p1, params, flag) 
{
    recode(type, diesel = 0, petrol = 0, hev = 0, phev = params$phev_grantpath * 
        zev_grant(p1) + vrt_rebate("phev", 0.9 * p1, params, 
        flag), bev = params$bev_grantpath * zev_grant(p1) + vrt_rebate("bev", 
        0.9 * p1, params, flag))
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
