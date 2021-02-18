\name{current_tech_costs}
\alias{current_tech_costs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
current_tech_costs(params, yeartime)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{yeartime}{
%%     ~~Describe \code{yeartime} here~~
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
function (params, yeartime) 
{
    flag0 <- ifelse(yeartime > 2021, "new", "old")
    tech <- fleet[, 1:10] \%>\% rowwise() \%>\% mutate(p_0 = tech_cost_fun(type = type, 
        tech_cost = tech_cost_2021, kWh = kWh, 2030))
    tech <- tech \%>\% mutate(p_1 = p_0 * (1 + params$car_vat)/(1 - 
        0.9 * vrt_rate(WLTP, flag0)))
    tech <- tech \%>\% mutate(p_2 = p_1 - incentives(type, p_1, 
        params, "new"))
    return(select(tech, make, model, segment, type, WLTP, kWh, 
        AER, p_0, p_1, p_2))
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
