\name{mean.g}
\alias{mean.g}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calcualtes the mean for a set of genotype.
}
\description{
Calcualtes the mean for a set of genotype.
}
\usage{
mean.g(x = "community matrix", g = "grouping")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Community matrix with species in columns.
}
  \item{g}{
    Grouping (i.e. genotype) vector.
}
}
% \details{

% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
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
function (x = "community matrix", g = "grouping") 
{
    return(apply(x, 2, function(x, g) tapply(x, g, mean), g = g))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
