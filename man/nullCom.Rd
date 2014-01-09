\name{nullCom}
\alias{nullCom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a set of null communities based on functions in the vegan
package. 
}
\description{
  This function produces a set of permuted matrices based on an input
  species abundance or presence-absence matrix.
}
\usage{
nullCom(com, method = "r1", nits = 5000, burn = 500, thin = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{com}{
    Community matrix with species abundances or
    presence-absences. Values are converted to presence-absence
    regardless of input.
}
  \item{method}{
        Permutation method. Options include: r00 = fully random, r0 = maintain
    site frequencies, r1 = maintain both site and species frequencies. For
    more options see the oecosimu function in the vegan package.
}
  \item{nits}{
        Number of iterations to conduct. 
}
  \item{burn}{
    The number permutations to conduct prior to recording matrices.    
}
  \item{thin}{
    The number of discarded matrices between iterations.
}
}
\details{
  Provides high level access to the oecosimu function in vegan.
}
\value{
  Returns a set of permuted matrices to be used for null modeling. The
  default null model is the most constrained version (i.e. both species
  and observation marginal totals are restricted).
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
function (com, method = "r1", nits = 5000, burn = 500, thin = 10) 
{
    com[com != 0] <- 1
    for (i in 1:burn) {
        post.burn <- commsimulator(x = com, method = method, 
            thin = thin)
    }
    out <- list()
    for (i in 1:nits) {
        out[[i]] <- commsimulator(x = post.burn, method = method, 
            thin = thin)
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
