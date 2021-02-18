\name{seg_prob}
\alias{seg_prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
seg_prob(qc1, qk, qev26, qev25, qev28)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qc1}{
%%     ~~Describe \code{qc1} here~~
}
  \item{qk}{
%%     ~~Describe \code{qk} here~~
}
  \item{qev26}{
%%     ~~Describe \code{qev26} here~~
}
  \item{qev25}{
%%     ~~Describe \code{qev25} here~~
}
  \item{qev28}{
%%     ~~Describe \code{qev28} here~~
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
function (qc1, qk, qev26, qev25, qev28) 
{
    p <- sapply(car_segments, function(seg) filter(p0, segment == 
        seg)$prob * (filter(p1, segment == seg)[1, qc1 + 1] * 
        filter(p2, segment == seg)[1, qk + 1] * ifelse(!is.na(qev26), 
        filter(p3, segment == seg)[1, qev26 + 1], 1) * ifelse(!is.na(qev25), 
        filter(p4, segment == seg)[1, qev25 + 1], 1) * ifelse(!is.na(qev28), 
        filter(p5, segment == seg)[1, qev28 + 1], 1)))
    p <- p/(1e-06 + sum(p))
    p <- tibble(seg = car_segments, prob = p)
    return(p)
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
