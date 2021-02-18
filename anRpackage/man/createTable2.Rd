\name{createTable2}
\alias{createTable2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createTable2(survey = survey_ev, q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survey}{
%%     ~~Describe \code{survey} here~~
}
  \item{q}{
%%     ~~Describe \code{q} here~~
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
function (survey = survey_ev, q) 
{
    q1 <- q[!(q \%in\% non_multiple_choice_question_codes)]
    q2 <- q[(q \%in\% non_multiple_choice_question_codes)]
    n <- length(q)
    ev <- survey \%>\% group_by_at(vars(one_of(q))) \%>\% summarise(n = n())
    for (qq in q1) ev <- ev \%>\% inner_join(filter(qanda, code == 
        qq)[, 3:4], by = setNames("answercode", qq))
    for (qq in q2) ev$qq <- "number"
    names(ev)[1:n] <- q
    names(ev)[seq(n + 2, 2 * n + 1)] <- toupper(q)
    return(ev)
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
