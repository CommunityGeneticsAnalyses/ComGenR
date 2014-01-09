\name{calcDepend}
\alias{calcDepend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate species dependencies (i.e. conditional probabilities).
}
\description{
Calculates the species dependencies for a given set of species
abundances as the conditional probability of two species. 
}
\usage{
calcDepend(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
The abundance of species "a".
}
  \item{b}{
The abundance of species "b".
}
}
\details{

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
function (a, b) 
{
    return(length(a[(a + b) == 2])/sum(a))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
