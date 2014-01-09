\name{cscore}
\alias{cscore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the C-Score of Stone and Roberts 1991. 
}
\description{
Calculates the C-Score of Stone and Roberts 1991. 
}
\usage{
cscore(x, cu.mat = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Community matrix of species abundances or present absences with
    species in columns.
}
  \item{cu.mat}{
    Logical: should the checkerboard unit matrix be returned?
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
function (x, cu.mat = FALSE) 
{
    x[x != 0] <- 1
    cu <- matrix(0, nrow = ncol(x), ncol = ncol(x))
    for (i in 1:ncol(x)) {
        for (j in 1:ncol(x)) {
            ri <- sum(x[, i])
            rj <- sum(x[, j])
            S <- x[, i] * 0
            S[x[, i] == 1 & x[, j] == 1] <- 1
            S <- sum(S)
            cu[i, j] <- (ri - S) * (rj - S)
        }
    }
    if (cu.mat) {
        return(cu)
    }
    else {
        return(mean(cu))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
